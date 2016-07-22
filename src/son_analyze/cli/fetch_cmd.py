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


"""son-analyze fetch command"""

# pylint: disable=unsubscriptable-object
import logging
from urllib.parse import ParseResult
from typing import Dict, Iterable, Any
import yaml  # type: ignore
import son_analyze.ops as ops
from son_analyze.core import types


_LOGGER = logging.getLogger(__name__)


def _print_yml_to_stdout(values: Iterable[Dict[str, Any]]) -> None:
    """Print the fetch result to the console."""
    print(yaml.dump_all(values))


def fetch_cmd(gatekeeper: ParseResult, kind: str,
              target: types.ResourceTargetTuple) -> None:
    """Fetch a vnfd or a nsd (with its dependencies) and display the result
     as Yaml documents on STDOUT."""
    if kind == 'vnfd':
        vnfd = ops.fetch.fetch_vnfd(gatekeeper, target.vendor,
                                    target.name, target.version)
        _print_yml_to_stdout([vnfd])
    elif kind == 'nsd':
        (nsd, vnfds) = ops.fetch.fetch_nsd(gatekeeper, target.vendor,
                                           target.name, target.version)
        _print_yml_to_stdout([nsd] + list(vnfds.values()))
    else:
        raise RuntimeError('Invalid resource type %s', kind)
    return
