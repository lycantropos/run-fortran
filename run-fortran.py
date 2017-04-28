#!/usr/local/bin/python3
import copy
import logging
import operator
import os
import re
import sys
from collections import (Counter,
                         OrderedDict,
                         namedtuple)
from functools import (partial,
                       reduce)
from typing import (Callable,
                    Iterable,
                    Iterator,
                    Dict,
                    Set,
                    Tuple,
                    List)

import click

__version__ = '0.0.5'

logger = logging.getLogger(__name__)

FORTRAN_FILES_EXTENSIONS = {'.f90', '.f95', '.f03', '.f', '.for'}

MODULE_USE_RE = re.compile(
    r'(?<=\buse\s)(?P<module>\s*\w+)',
    re.IGNORECASE)

MODULE_DEFINITION_RE = re.compile(
    r'(?<=\bmodule\s)(?P<module>\s*\w+)',
    re.IGNORECASE)

LITERAL_CONSTANTS_RE = re.compile('|'.join([
    '\'[^\']*\'',  # for single quoted literal constants
    '\"[^\"]*\"'  # for double quoted literal constants
]))

ModulesNames = namedtuple('ModulesNames', ['used', 'defined'])


@click.group()
def main() -> None:
    logging.basicConfig(level=logging.DEBUG)


@main.command()
@click.option('--path', '-p',
              default=os.getcwd(),
              type=click.Path(),
              help='Target project directory path.')
@click.option('--sep', '-s',
              default=' ',
              help='Separator between resulted modules paths.')
def run(path: str, sep: str) -> None:
    """
    Orders modules paths by inclusion.
    """
    defined_modules_names_by_modules_paths = dict(
        parse_defined_modules_names_by_modules_paths(path))
    used_modules_names_by_modules_paths = dict(
        parse_used_modules_names_by_modules_paths(path))

    modules_names_by_modules_paths = dict(
        get_modules_names_by_modules_paths(
            defined_modules_names_by_modules_paths,
            used_modules_names_by_modules_paths))

    update_chained_modules_names(modules_names_by_modules_paths)

    sorted_modules_names_by_modules_paths = (
        sort_modules_names_by_modules_paths(modules_names_by_modules_paths))

    for module_path in sorted_modules_names_by_modules_paths.keys():
        sys.stdout.write(module_path + sep)


def get_modules_names_by_modules_paths(
        defined_modules_names_by_modules_paths: Dict[str, Set[str]],
        used_modules_names_by_modules_paths: Dict[str, Set[str]]
) -> Iterable[Tuple[str, ModulesNames]]:
    modules_paths = reduce(operator.ior,
                           map(set, [defined_modules_names_by_modules_paths.keys(),
                                     used_modules_names_by_modules_paths.keys()]))
    for module_path in modules_paths:
        modules_names = ModulesNames(
            used=used_modules_names_by_modules_paths.get(module_path,
                                                         set()),
            defined=defined_modules_names_by_modules_paths.get(module_path,
                                                               set()))
        yield module_path, modules_names


def sort_modules_names_by_modules_paths(
        modules_names_by_modules_paths: Dict[str, ModulesNames]
) -> OrderedDict:
    used_modules_names_lists = map(
        list,
        (modules_names.used
         for modules_names in modules_names_by_modules_paths.values()))
    used_modules_names_list = sum(used_modules_names_lists, [])
    priorities = Counter(used_modules_names_list)

    def used_modules_names_by_modules_paths_key(
            modules_names_by_module_path: Tuple[str, ModulesNames]) -> int:
        module_path, modules_names = modules_names_by_module_path
        try:
            return max(priorities[module_name]
                       for module_name in modules_names.defined)
        except ValueError:
            return 0

    sorted_used_modules_names_by_modules_paths = OrderedDict(sorted(
        modules_names_by_modules_paths.items(),
        key=used_modules_names_by_modules_paths_key,
        reverse=True))
    return sorted_used_modules_names_by_modules_paths


def update_chained_modules_names(
        modules_names_by_modules_paths: Dict[str, ModulesNames]) -> None:
    modules_names_by_modules_paths_copy = copy.deepcopy(
        modules_names_by_modules_paths)

    def get_module_path_by_module_name(module_name: str) -> str:
        modules_paths = [
            module_path
            for (module_path,
                 modules_names) in modules_names_by_modules_paths_copy.items()
            if module_name in modules_names.defined]
        try:
            res, = modules_paths
        except ValueError as err:
            if modules_paths:
                modules_paths_str = ', '.join(modules_paths)
                err_msg = ('Requested module name is unambiguous: '
                           'found {appearances_count} appearances '
                           'of module named "{module_name}" '
                           'in modules definitions within '
                           'files located at {modules_paths}.'
                           .format(appearances_count=len(modules_paths),
                                   module_name=module_name,
                                   modules_paths=modules_paths_str))
                raise ValueError(err_msg) from err
            err_msg = ('Requested module name is not found: '
                       'no appearance '
                       'of module named "{module_name}" '
                       'in modules definitions.'
                       .format(module_name=module_name))
            raise ValueError(err_msg) from err
        return res

    for (module_path,
         used_modules_names) in modules_names_by_modules_paths_copy.items():
        unprocessed_modules_names = copy.deepcopy(used_modules_names.used)
        try:
            while True:
                used_module_name = unprocessed_modules_names.pop()
                extension = modules_names_by_modules_paths[
                    get_module_path_by_module_name(used_module_name)]
                unprocessed_modules_names |= extension.used
                used_modules_names = (
                    modules_names_by_modules_paths[module_path].used
                    | extension.used
                    | extension.defined)
                modules_names_by_modules_paths[module_path]._replace(
                    used=used_modules_names)
        except KeyError:
            continue


def parse_modules_names_by_modules_paths(
        path: str,
        modules_names_parser: Callable[[str], Set[str]]
) -> Iterable[Tuple[str, Set[str]]]:
    for module_path in get_fortran_modules_paths(path):
        yield module_path, modules_names_parser(module_path)


def get_fortran_modules_paths(path: str
                              ) -> Iterator[str]:
    path = os.path.abspath(path)
    for root_name, directories_names, files_names in os.walk(path):
        fortran_modules_names = filter(is_fortran_module, files_names)
        abs_path_getter = partial(os.path.join, root_name)
        yield from map(abs_path_getter, fortran_modules_names)


def is_fortran_module(file_name: str) -> bool:
    _, extension = os.path.splitext(file_name)
    return extension in FORTRAN_FILES_EXTENSIONS


def parse_modules_names(
        module_path: str,
        modules_names_parser: Callable[[Iterable[str]], Set[str]]) -> Set[str]:
    statements = parse_normalized_statements(module_path)
    return modules_names_parser(statements)


def parse_normalized_statements(module_path: str) -> Iterable[str]:
    with open(module_path) as module_file:
        yield from map(normalize_statement, module_file)


def normalize_statement(statement: str) -> str:
    stripped_statement = statement.strip(' ')
    statement_without_literals = LITERAL_CONSTANTS_RE.sub('',
                                                          stripped_statement)
    try:
        statement_without_literals_and_comments, comment = (
            statement_without_literals.split('!', maxsplit=1))
    except ValueError:
        # no comments found
        statement_without_literals_and_comments = statement_without_literals
    return statement_without_literals_and_comments


def parse_modules_names_from_statements(
        statements: Iterable[str],
        modules_names_getter: Callable[[str], List[str]]) -> Set[str]:
    defined_modules_names_lists = map(modules_names_getter, statements)
    defined_modules_names_list = sum(defined_modules_names_lists, [])
    return set(defined_modules_names_list)


parse_defined_modules_names_from_statements = partial(
    parse_modules_names_from_statements,
    modules_names_getter=MODULE_DEFINITION_RE.findall)
parse_used_modules_names_from_statements = partial(
    parse_modules_names_from_statements,
    modules_names_getter=MODULE_USE_RE.findall)

parse_defined_modules_names = partial(
    parse_modules_names,
    modules_names_parser=parse_defined_modules_names_from_statements)
parse_used_modules_names = partial(
    parse_modules_names,
    modules_names_parser=parse_used_modules_names_from_statements)

parse_defined_modules_names_by_modules_paths = partial(
    parse_modules_names_by_modules_paths,
    modules_names_parser=parse_defined_modules_names)
parse_used_modules_names_by_modules_paths = partial(
    parse_modules_names_by_modules_paths,
    modules_names_parser=parse_used_modules_names)

if __name__ == '__main__':
    main()
