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


"""son-analyze fetch operation"""

import logging
import collections
from urllib.parse import ParseResult, urljoin
from typing import Dict, Any, Tuple
import requests


_LOGGER = logging.getLogger(__name__)


class FetchError(Exception):
    """Base class for exceptions in this module."""
    pass


class InvalidResourceReferenceError(FetchError):
    """Raised when a resource (nfd) points to a missing vnfd."""

    # pylint: disable=unsubscriptable-object
    def __init__(self, nsd: Dict[str, Any], missing_vnf_id: str) -> None:
        super().__init__()
        self.nsd = nsd
        self.missing_vnf_id = missing_vnf_id
        self.message = ('The nsd ("{}","{}","{}") has a vnf_id field "{}" '
                        'pointing to a non-existing vnfd').format(
                            nsd['vendor'], nsd['name'], nsd['version'],
                            missing_vnf_id)


# pylint: disable=unsubscriptable-object
def _fetch_resource(gatekeeper_endpoint: ParseResult, path: str, vendor: str,
                    name: str, version: str) -> Dict[str, Any]:
    """Fetch a resource and return the Json as a dictionary. Return `None` if
     nothing is found. It raise a RuntimeError exception when a gatekeeper API
     is dectected"""
    url = urljoin(gatekeeper_endpoint.geturl(), path)
    _LOGGER.info('Fetching a resource at %s', url)
    query_params_raw = {'vendor': vendor,  # Dict[Str, Str]
                        'name': name,
                        'version': version}
    # We force the order of the query's parameters to lower the impact on tests
    # when a key is added or removed
    query_params = collections.OrderedDict(sorted(query_params_raw.items()))
    res_resp = requests.get(url, params=query_params,
                            headers={'content-type': 'application/json'})
    try:
        res_resp.raise_for_status()
    except requests.exceptions.HTTPError:
        _LOGGER.exception('Failed to retrieve a resource at %s '
                          '(status code = %d)', res_resp.url,
                          res_resp.status_code)
        raise
    tmp = res_resp.json()
    if not isinstance(tmp, list):
        exc = RuntimeError('The returned json is not boxed by a list')
        _LOGGER.error(exc)
        raise exc
    _LOGGER.info('Succeed to retrieve the resource %s (status code = %d)',
                 res_resp.url, res_resp.status_code)
    for elt in tmp:
        if all([elt['vendor'] == vendor, elt['name'] == name,
                elt['version'] == version]):
            return elt
    return None


# pylint: disable=unsubscriptable-object
def fetch_vnfd(gatekeeper_endpoint: ParseResult, vendor: str, name: str,
               version: str) -> Dict[str, Any]:
    """Fetch a Vnfd. Return `None` if nothing is found."""
    return _fetch_resource(gatekeeper_endpoint, 'functions', vendor, name,
                           version)


# pylint: disable=unsubscriptable-object
def fetch_nsd(gatekeeper_endpoint: ParseResult, vendor: str, name: str,
              version: str) -> Tuple[Dict[str, Any], Dict[str, Any]]:
    """Fetch a Nsd with its related Vnfd. Return `None` if nothing is found.
    Raise a FileNotFoundError if  """
    nsd = _fetch_resource(gatekeeper_endpoint, 'services', vendor, name,
                          version)
    _LOGGER.info('Fetching the inner vnfds of the %s nsd', nsd['name'])
    acc = {}  # Dict[str, Any]
    for fun_desc in nsd['network_functions']:
        vnfd = fetch_vnfd(gatekeeper_endpoint, fun_desc['vnf_vendor'],
                          fun_desc['vnf_name'], fun_desc['vnf_version'])
        if not vnfd:
            exc = InvalidResourceReferenceError(nsd, fun_desc['vnf_id'])
            _LOGGER.error(exc)
            raise exc
        acc[fun_desc['vnf_id']] = vnfd
    return (nsd, acc)
