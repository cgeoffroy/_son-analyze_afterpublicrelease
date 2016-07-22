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


# pylint: disable=missing-docstring
from urllib.parse import urlparse
import re
import typing  # noqa pylint: disable=unused-import
import requests_mock  # type: ignore
from son_analyze.cli import fetch_cmd
from son_analyze.core import types


def test_fetch_cmd(capsys, sonata_demo_mock) -> None:
    target = types.ResourceTargetTuple('sonata-demo',
                                       'eu.sonata-nfv.service-descriptor',
                                       '0.2.1')
    gate = urlparse('http://localhost/mock/')
    with requests_mock.Mocker() as mocker:
        for (url, value) in sonata_demo_mock:
            mocker.get(url.geturl(), json=value)
        fetch_cmd.fetch_cmd(gate, 'nsd', target)
    out, _ = capsys.readouterr()
    reg = re.compile('^author.*')
    assert reg.match(out)
    reg = re.compile('^---$', re.MULTILINE)
    assert reg.search(out)
