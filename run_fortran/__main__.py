import copy
import json
import os
import re
import sys
from collections import OrderedDict
from functools import partial
from itertools import chain
from typing import (Any,
                    Callable,
                    Container,
                    Dict,
                    Iterable,
                    Iterator,
                    List,
                    NamedTuple,
                    Optional,
                    Set,
                    Tuple,
                    Union)

import click

OUTPUT_FILE_EXTENSION = '.json'

FORTRAN_FILES_EXTENSIONS = {'.f77', '.f90', '.f95', '.f03', '.f', '.for'}

MODULE_USE_RE = re.compile(r'(?<=\buse)\s*'
                           r'(?:,\s*'
                           r'(?P<intrinsic>intrinsic|non_intrinsic)'
                           r'\s*::)?'
                           r'\s(?P<module>\w+)',
                           re.IGNORECASE)
MODULE_DEFINITION_RE = re.compile(r'(?<=\bmodule\s)(?!\s*procedure)(\s*\w+)',
                                  re.IGNORECASE)

LITERAL_CONSTANTS_RE = re.compile('|'.join([
    '\'[^\']*\'',  # for single quoted literal constants
    '\"[^\"]*\"'  # for double quoted literal constants
]))

Module = NamedTuple('Module', [('name', str), ('is_intrinsic', bool)])
Namespace = NamedTuple('Namespace',
                       [('defined', Set[Module]), ('used', Set[Module])])


@click.group()
def main() -> None:
    pass


@main.command()
@click.option('--path', '-p',
              default=os.getcwd(),
              type=click.Path(),
              help='Target project directory path.')
@click.option('--intrinsic-modules-names', '-i',
              default='IEEE_Arithmetic,IEEE_Features,IEEE_Exceptions,'
                      'ISO_C_Binding,ISO_Fortran_env',
              help='Comma-separated list of intrinsic modules.')
@click.option('--sep', '-s',
              default=' ',
              help='Separator between resulted modules paths.')
@click.option('--output-file-name', '-o',
              default=None,
              type=str,
              help='File name to save modules relations to '
                   '(".json" extension will be added).')
def run(path: str,
        intrinsic_modules_names: str,
        sep: str,
        output_file_name: Optional[str]) -> None:
    """
    Orders modules paths by inclusion.
    """
    defined_modules = dict(parse_defined_modules_by_paths(path))
    used_modules = dict(parse_used_modules_by_paths(path))
    namespaces_by_paths = {
        module_path: Namespace(defined=defined_modules.get(module_path, set()),
                               used=used_modules.get(module_path, set()))
        for module_path in defined_modules.keys() | used_modules.keys()}
    intrinsic_modules_names = set(
            map(str.strip, intrinsic_modules_names.lower().split(',')))
    unfold_namespaces(namespaces_by_paths, intrinsic_modules_names)
    sorted_namespaces_by_paths = OrderedDict(sort_namespaces_with_paths(
            namespaces_by_paths.items()))
    if output_file_name is not None:
        output_file_name += OUTPUT_FILE_EXTENSION
        with open(output_file_name,
                  mode='w',
                  encoding='utf-8') as output_file:
            json.dump(namespaces_by_paths_to_json(sorted_namespaces_by_paths),
                      output_file,
                      indent=True)
    result = sep.join(sorted_namespaces_by_paths.keys())
    sys.stdout.write(result)


def namespaces_by_paths_to_json(namespaces_by_paths: Dict[str, Namespace]
                                ) -> Dict[str, Any]:
    return OrderedDict((path, namespace_to_json(namespace))
                       for path, namespace in namespaces_by_paths.items())


def namespace_to_json(namespace: Namespace) -> Dict[str, List[Any]]:
    return OrderedDict(defined=[module_to_json(module)
                                for module in namespace.defined],
                       used=[module_to_json(module)
                             for module in namespace.used])


def module_to_json(module: Module) -> Dict[str, Union[str, bool]]:
    return {module.name: module.is_intrinsic}


def sort_namespaces_with_paths(namespaces_with_paths
                               : Iterable[Tuple[str, Namespace]]
                               ) -> Iterable[Tuple[str, Namespace]]:
    result = []
    for path, namespace in namespaces_with_paths:
        index_by_defined = min(
                (index
                 for index, (_, other_namespace) in enumerate(
                        result,
                        # insertion should be before namespace in which
                        # one of current namespace's defined modules is used
                        start=0)
                 if not other_namespace.used.isdisjoint(namespace.defined)),
                default=0)
        index_by_used = max(
                (index
                 for index, (_, other_namespace) in enumerate(
                        result,
                        # insertion should be after namespace in which
                        # one of current namespace's used modules is defined
                        start=1)
                 if not other_namespace.defined.isdisjoint(namespace.used)),
                default=0)
        result.insert(max(index_by_defined, index_by_used), (path, namespace))
    return result


def unfold_namespaces(namespaces_by_paths: Dict[str, Namespace],
                      intrinsic_modules_names: Container[str]) -> None:
    namespaces_by_paths_copy = copy.deepcopy(namespaces_by_paths)
    for module_path, original_namespace in namespaces_by_paths_copy.items():
        unprocessed_modules = copy.deepcopy(original_namespace.used)
        try:
            while True:
                used_module = unprocessed_modules.pop()
                if used_module.is_intrinsic:
                    continue
                used_module_path = to_module_path(
                        used_module,
                        namespaces_by_paths=namespaces_by_paths_copy,
                        intrinsic_modules_names=intrinsic_modules_names)
                if used_module_path is None:
                    continue
                used_module_namespace = namespaces_by_paths[used_module_path]
                unprocessed_modules |= used_module_namespace.used
                namespace = namespaces_by_paths[module_path]
                unfolded_used_modules = (namespace.used
                                         | used_module_namespace.defined
                                         | used_module_namespace.used)
                namespaces_by_paths[module_path] = (
                    namespace._replace(used=unfolded_used_modules))
        except KeyError:
            continue


def to_module_path(module: Module,
                   *,
                   namespaces_by_paths: Dict[str, Namespace],
                   intrinsic_modules_names: Container[str]) -> Optional[str]:
    candidates = [path
                  for path, namespace in namespaces_by_paths.items()
                  if module in namespace.defined]
    try:
        result, = candidates
    except ValueError as error:
        if candidates:
            raise ValueError('Found {count} appearances of module "{name}" '
                             'in modules definitions at {paths}.'
                             .format(count=len(candidates),
                                     name=module,
                                     paths=', '.join(candidates))) from error
        elif module.name not in intrinsic_modules_names:
            raise ValueError('Module "{name}" is not found '
                             'in modules definitions.'
                             .format(name=module.name)) from error
        else:
            return None
    return result


def parse_modules_by_paths(path: str,
                           modules_parser: Callable[[str], Set[Module]]
                           ) -> Iterable[Tuple[str, Set[Module]]]:
    for module_path in to_fortran_modules_paths(path):
        yield module_path, modules_parser(module_path)


def to_fortran_modules_paths(directory_path: str) -> Iterator[str]:
    directory_path = os.path.abspath(directory_path)
    for root_name, directories_names, files_names in os.walk(directory_path):
        fortran_modules_names = filter(is_fortran_module, files_names)
        abs_path_getter = partial(os.path.join, root_name)
        yield from map(abs_path_getter, fortran_modules_names)


def is_fortran_module(file_name: str) -> bool:
    _, extension = os.path.splitext(file_name)
    return extension in FORTRAN_FILES_EXTENSIONS


def parse_modules(module_path: str,
                  modules_parser: Callable[[Iterable[str]], Set[Module]]
                  ) -> Set[Module]:
    return modules_parser(parse_normalized_statements(module_path))


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
    return statement_without_literals_and_comments.lower()


def statements_to_modules(statements: Iterable[str],
                          modules_parser: Callable[[str], Iterable[Module]]
                          ) -> Set[Module]:
    return set(chain.from_iterable(map(modules_parser, statements)))


def to_defined_modules(text: str) -> Iterable[Module]:
    return [Module(name=name, is_intrinsic=False)
            for name in MODULE_DEFINITION_RE.findall(text)]


def to_used_modules(text: str) -> Iterable[Module]:
    return [Module(name=name,
                   is_intrinsic=intrinsic_marker == 'intrinsic')
            for intrinsic_marker, name in MODULE_USE_RE.findall(text)]


statements_to_defined_modules = partial(statements_to_modules,
                                        modules_parser=to_defined_modules)
statements_to_used_modules = partial(statements_to_modules,
                                     modules_parser=to_used_modules)
parse_defined_modules = partial(parse_modules,
                                modules_parser=statements_to_defined_modules)
parse_used_modules = partial(parse_modules,
                             modules_parser=statements_to_used_modules)
parse_defined_modules_by_paths = partial(parse_modules_by_paths,
                                         modules_parser=parse_defined_modules)
parse_used_modules_by_paths = partial(parse_modules_by_paths,
                                      modules_parser=parse_used_modules)

if __name__ == '__main__':
    main()
