[![Build Status](http://jenkins.sonata-nfv.eu/buildStatus/icon?job=son-analyze)](http://jenkins.sonata-nfv.eu/job/son-analyze/)

# son-analyze

SONATA's Service Platform analysis framework. `son-analyze` creates an analysis session to study a service. Inside this session, the end-user can program an analysis on a service's metrics using [pandas](https://pypi.python.org/pypi/pandas). This tools uses [Jupyter](http://jupyter.org/about.html) to offer a ready-to-use web-IDE. It also contains a set of libraries to speak with the Sonata Platform that are automatically installed in the session.


## Development

This tool is composed of multiple libraries.
* The main `son-analyze` library contains some core features and the command line entry-points.
* The `son-scikit` library use the `son-analyze` base structures and functions to bridge Sonata data with [SciPy](https://www.scipy.org/index.html) and [pandas](http://pandas.pydata.org/).


### Building

#### Adding/Removing a new dependency

* If the new library is directly used by `son-analyze`:
 * Add the new library to:
  * `setup.py`
  * `requirements.in`
 * Regenerate the dependencies: `pip-compile requirements.in`
* If the new library is only used for testing, developing:
 * Add the new library to:
  * `dev-requirements.in`
  * Regenerate the dependencies: `pip-compile dev-requirements.in`


#### Tests and checks

The code is checked and tested by [flake8](https://flake8.readthedocs.org/en/latest/), [pylint](https://www.pylint.org/), [mypy](http://mypy-lang.org/) and [py.test](http://pytest.org/latest/).

Inside a virtualenv environment:
* To Launch all the test: `scripts/all.py`
* Only launch a specific tool globally or on specific files:
 * flake8: `scripts/flake8.sh`
 * pylint: `scripts/pylint.sh setup.py`
 * mypy: `scripts/mypy.sh setup.py src/son_analyze/cli/main.py`
 * py.test: `scripts/py.test.sh`

You can also directly use these tools: `mypy src/son_analyze/cli/main.py`.


#### Continuous Integration

With Docker, the CI tests can be locally run with:
* `utils/ci/build_01_checks_and_unit_tests.sh`


### Dependencies

* `son-analyze` (at runtime)
  * [docker-py](https://pypi.python.org/pypi/docker-py) >=1.7.2 (Apache License Version 2.0)
  * [pyaml](https://pypi.python.org/pypi/pyaml) >=15.8.2 (WTFPL)
  * [typing](https://pypi.python.org/pypi/typing) >=3.5.0.1 (PSF)
  * [requests](https://pypi.python.org/pypi/requests) >=2.9.1 (Apache License Version 2.0)
* `son-scikit` (at runtime)
  * [`son-analyze`](https://github.com/sonata-nfv/son-analyze) >= 0.0.1 (Apache License Version 2.0)
  * [pyaml](https://pypi.python.org/pypi/pyaml) >=15.8.2 (WTFPL)
  * [typing](https://pypi.python.org/pypi/typing) >=3.5.0.1 (PSF)
  * [pandas](https://pypi.python.org/pypi/pandas) >=0.18.1 (BSD)
  * [Jupyter](http://jupyter.org/about.html) >=4.0 (BSD)
* During development and testing
  * [colorama](https://pypi.python.org/pypi/colorama) >=0.3.7 (BSD)
  * [pip-tools](https://pypi.python.org/pypi/pip-tools) >=1.6 (BSD)
  * [pytest](https://pypi.python.org/pypi/pytest) >=2.9.0 (MIT)
  * [flake8](https://pypi.python.org/pypi/flake8) >=2.5.4 (MIT)
  * [mypy-lang](https://pypi.python.org/pypi/mypy-lang) >=0.3.1 (MIT)
  * [pylint](https://pypi.python.org/pypi/pylint) >=1.5.4 (GPL): Pylint is used to check the source code for programming errors. It is done by calling the main `pylint` executable over this repository's files. No feature is built upon this tool. This dependency doesn't create a derivative work therefore it doesn't trigger the copyleft effect.
  * [requests_mock](https://pypi.python.org/pypi/requests-mock) >= 1.0.0 (Apache License Version 2.0)
  * [pytest-capturelog](https://pypi.python.org/pypi/pytest-capturelog) >= 0.7 (MIT)


### Contributing

You may contribute to `son-analyze` similarly to other SONATA (sub-) projects, i.e. by creating pull requests. The PR code must abide by the `flake8` (pep8) and `pylint` rules.


## Installation

Ansible & Virtualenv are used to setup the development environment on Ubuntu 14.04:
* Base packages installation
 * `sudo ansible-playbook install.yml`
 * `sudo ansible-playbook dev.install.yml`
* Virtualenv environment creation
 * `virtualenv -p python3 venv`
* Environment activation
 * `source venv/bin/activate`
 * (Optional) Environment deactivation
  * `deactivate`
* Dependencies installation
 * `pip3 install -rrequirements.txt -rdev-requirements.txt`
* Installing `son-analyze`
 * `python setup.py develop`


## Usage

`son-analyze` creates an environment based on the [SciPy](https://www.scipy.org/index.html) and [Jupyter](https://jupyter.org/).

### Commands

* Start by creating the required image on the host with:
    * `son-analyze bootstrap`
        * It will download the official Jupyter Docker image and create a new custom Docker image.
        This new image contains a SciPy Sonata library with its required dependencies.
* Enter the environment with:
    * `son-analyze run`
        * This command will start a Docker container.
    * (Optional) to stop the environment, simply hit `Ctrl-c` in the console.
    * Browse `http://localhost:8888`.
    * Some code examples are provided in the `examples` directory (`File > Open File`).


## License

This SONATA `son-analyze` software is published under Apache 2.0 license. Please see the LICENSE file for more details.


## Useful Links

* Check https://www.reddit.com/r/scipy/ for some SciPy information.
* [Sonata-nfv](http://www.sonata-nfv.eu/): the SONATA project's website


---
#### Lead Developers

The following lead developers are responsible for this repository and have admin rights. They can, for example, merge pull requests.

* Geoffroy Chollon ([cgeoffroy](https://github.com/cgeoffroy))
* Steven Van Rossem ([stevenvanrossem](https://github.com/stevenvanrossem))


#### Feedback-Chanel

* Please use the GitHub issues to report bugs.
* You may use the mailing list sonata-dev@lists.atosresearch.eu
