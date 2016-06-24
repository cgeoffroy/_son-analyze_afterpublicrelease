"""son-analyze"""

import codecs
import os
import re
import sys
from setuptools import setup, find_packages  # type: ignore


def read(*parts):
    """Reads a file with a path relative to __file__ and
    returns its content.
    """
    path = os.path.join(os.path.dirname(__file__), *parts)
    with codecs.open(path, encoding='utf-8') as fobj:
        return fobj.read()


def find_version(*file_paths):
    """Finds __version__ in the given file and returns it."""
    version_file = read(*file_paths)
    version_match = re.search(r"^__version__ = ['\"]([^'\"]*)['\"]",
                              version_file, re.M)
    if version_match:
        return version_match.group(1)
    raise RuntimeError("Unable to find version string.")

if not (sys.version_info.major == 3 and sys.version_info.minor >= 4):
    sys.exit("Sorry, only Python 3.4 is supported")

setup(
    name='son-scikit',
    version=find_version("src/son/scikit", "__init__.py"),
    license='Apache License 2.0',
    description='A scikit extension for the SONATA platform',
    url='https://github.com/sonata-nfv/son-analyze',
    author_email='sonata-dev@sonata-nfv.eu',
    package_dir={'': 'src'},
    packages=find_packages('src'),
    install_requires=[
        'pyaml >= 15.8.0, < 16.0.0',
        'typing >= 3.5.0.1'
    ],
    zip_safe=False,
    tests_require=[
        'pytest >= 2.9.0',
        'flake8 >= 2.5.0',
        'pylint >= 1.5.0',
    ],
    include_package_data=True
)
