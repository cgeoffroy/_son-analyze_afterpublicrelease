# Copyright (c) 2015 SONATA-NFV, Thales Communications & Security
# ALL RIGHTS RESERVED.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Neither the name of the SONATA-NFV, Thales Communications & Security
# nor the names of its contributors may be used to endorse or promote
# products derived from this software without specific prior written
# permission.
#
# This work has been performed in the framework of the SONATA project,
# funded by the European Commission under Grant number 671517 through
# the Horizon 2020 and 5G-PPP programmes. The authors would like to
# acknowledge the contributions of their colleagues of the SONATA
# partner consortium (www.sonata-nfv.eu).


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
    name='son-analyze',
    version=find_version("src/son_analyze", "__init__.py"),
    license='Apache License 2.0',
    description='Analysis framework for the SONATA platform',
    url='https://github.com/sonata-nfv/son-analyze',
    author_email='sonata-dev@sonata-nfv.eu',
    package_dir={'': 'src'},
    packages=find_packages('src'),
    install_requires=[
        'pyaml >= 15.8.0, < 16.0.0',
        'typing >= 3.5.0.1',
        'docker-py >= 1.7.0',
        'requests >= 2.9.1',
    ],
    zip_safe=False,
    entry_points={
        'console_scripts': [
            'son-analyze=son_analyze.cli.main:main',
        ],
    },
    tests_require=[
        'pytest >= 2.9.0',
        'flake8 >= 2.5.0',
        'pylint >= 1.5.0',
    ],
    include_package_data=True
)
