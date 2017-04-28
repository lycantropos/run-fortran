#!python3.6
import logging
import os
import re
from collections import OrderedDict
from functools import (partial,
                       cmp_to_key)
from typing import (Optional,
                    Iterable,
                    Iterator,
                    Dict,
                    Tuple,
                    List)

import click

logger = logging.getLogger(__name__)

FORTRAN_FILES_EXTENSIONS = {'.f90', '.f95', '.f03', '.f', '.for'}

USED_MODULE_RE = re.compile(
    '(?<=^use\s)(?P<module>\w*)(?=\s*,?\s*(only)?\s*:?.*?$)',
    re.IGNORECASE)


@click.group()
def main() -> None:
    logging.basicConfig(level=logging.DEBUG)


def compare_used_modules_names_by_modules_paths(
        first: Tuple[str, List[str]],
        second: Tuple[str, List[str]]) -> int:
    first_module_path, first_used_modules_names = first
    second_module_path, second_used_modules_names = second
    first_module_file_name = os.path.basename(first_module_path)
    second_module_file_name = os.path.basename(second_module_path)
    first_module_name, _ = os.path.splitext(first_module_file_name)
    second_module_name, _ = os.path.splitext(second_module_file_name)
    if first_module_name in second_used_modules_names:
        return 1
    if second_module_name in first_used_modules_names:
        return -1
    return 0


used_modules_names_by_modules_paths_key = cmp_to_key(
    compare_used_modules_names_by_modules_paths)


@main.command()
@click.option('--path', '-p',
              default=os.getcwd(),
              type=click.Path(),
              help='Target project directory path.')
def run(path: str) -> None:
    used_modules_names_by_modules_paths = dict(
        get_used_modules_names_by_modules_paths(path))
    sorted_used_modules_names_by_modules_paths = OrderedDict(
        sorted(used_modules_names_by_modules_paths.items(),
               key=used_modules_names_by_modules_paths_key))


def sort_used_modules_names_by_modules_paths(
        used_modules_names_by_modules_paths: Dict[str, List[str]]
) -> OrderedDict:
    sorted_used_modules_names_by_modules_paths_items = sorted(
        used_modules_names_by_modules_paths.items(),
        key=used_modules_names_by_modules_paths_key)
    return OrderedDict(sorted_used_modules_names_by_modules_paths_items)


def get_used_modules_names_by_modules_paths(
        path: str) -> Iterable[Tuple[str, List[str]]]:
    for module_path in get_fortran_modules_paths(path):
        names = get_used_modules_names_by_module_path(module_path)
        yield module_path, names


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


def get_used_modules_names_by_module_path(module_path: str
                                          ) -> List[str]:
    with open(module_path) as module_file:
        return get_used_modules_names_by_statements(module_file)


def get_used_modules_names_by_statements(statements: Iterable[str]
                                         ) -> List[str]:
    return list(filter(None, map(get_used_module_name, statements)))


def get_used_module_name(statement: str) -> Optional[str]:
    stripped_statement = statement.strip(' ')
    search_res = USED_MODULE_RE.search(stripped_statement)
    try:
        return search_res.group(0)
    except AttributeError:
        return None


if __name__ == '__main__':
    main()
