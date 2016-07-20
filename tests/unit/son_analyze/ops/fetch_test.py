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
import logging
import urllib.parse
import typing  # noqa pylint: disable=unused-import
import requests_mock  # type: ignore
import yaml  # type: ignore
from son_analyze.ops import fetch


def test_fetch_nsd(caplog, sonata_demo_nsd_91460c67, iperf_vnfd_d0ac3202,
                   firewall_vnfd_dce50374, tcpdump_vnfd_18741f2a) -> None:
    caplog.setLevel(logging.DEBUG)
    with requests_mock.Mocker() as mocker:
        files = [
            ('services', yaml.load(sonata_demo_nsd_91460c67)),
            ('functions', yaml.load(iperf_vnfd_d0ac3202)),
            ('functions', yaml.load(firewall_vnfd_dce50374)),
            ('functions', yaml.load(tcpdump_vnfd_18741f2a))
        ]
        for to_mock in files:
            base = ('http://localhost/mock/{:s}?name={:s}&'
                    'vendor={:s}&version={:s}').format(
                        to_mock[0], to_mock[1]['name'],
                        to_mock[1]['vendor'], to_mock[1]['version'])
            url = urllib.parse.urlparse(base)
            mocker.get(url.geturl(), json=[to_mock[1]])
        (nsd, vnfds) = fetch.fetch_nsd(url, 'eu.sonata-nfv.service-descriptor',
                                       'sonata-demo', '0.2.1')
    assert nsd['descriptor_version'] == '1.0'
    assert len(vnfds) == 3
    assert vnfds['vnf_firewall']['descriptor_version'] == 'vnfd-schema-01'
