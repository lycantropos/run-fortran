from setuptools import setup

setup(name='run-fortran',
      version='0.0.3',
      scripts=['run-fortran.py'],
      description='Script for generating list of files paths '
                  'in hierarchical order '
                  'for correct "Fortran"-files compilation.',
      author='Azat Ibrakov',
      author_email='azatibrakov@gmail.com',
      url='https://github.com/lycantropos/run-fortran',
      download_url='https://github.com/lycantropos/run-fortran/archive/'
                   'master.tar.gz',
      keywords=['fortran'],
      install_requires=[
          'click>=6.7',  # parsing command-line arguments
      ])
