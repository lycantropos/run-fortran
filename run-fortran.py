#!python3.6
import logging
import os

import click

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@click.group()
def main() -> None:
    pass


@main.command()
@click.option('--path', '-p',
              default=os.getcwd(),
              type=click.Path(),
              help='Target project directory path.')
def run(path: str) -> None:
    pass


if __name__ == '__main__':
    main()
