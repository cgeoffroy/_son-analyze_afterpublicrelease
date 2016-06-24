"""son-analyze command line tool"""

import sys
import os
import signal
from argparse import ArgumentParser, Namespace
from pkg_resources import resource_filename  # type: ignore
import typing  # noqa pylint: disable=unused-import
from typing import List
from docker import Client  # type: ignore
from son.analyze import __version__

_IMAGE_TAG = 'son-analyze-scikit'


def bootstrap(_: Namespace) -> None:
    """Create the images used by son-analyze in the current host"""
    cli = Client(base_url='unix://var/run/docker.sock')
    path = resource_filename('son.analyze.resources', 'anaconda.Dockerfile')
    root_context = os.path.realpath(resource_filename('son.analyze', '../../..'))
    for line in cli.build(path=root_context, tag=_IMAGE_TAG,
                          dockerfile=path, rm=True, decode=True):
        print('> ', line["stream"], end="")
    sys.exit(0)


def run(args: Namespace) -> None:
    """Run an analysis framework environment"""
    cli = Client(base_url='unix://var/run/docker.sock')
    binds = {  # type: Dict[str, Dict[str, str]]
        '/dev/random': {
            'bind': '/dev/random'
        },
        '/dev/urandom': {
            'bind': '/dev/urandom'
        }
    }
    if args.dynamic_mount:
        new_entry = {
            resource_filename('son.analyze.resources.r', 'son.analyze'): {
                'bind': '/var/tmp/son.analyze',
                'mode': 'rw'
            }
        }
        binds.update(new_entry)
    host_config = cli.create_host_config(port_bindings={8787: 8787},
                                         binds=binds)
    container = cli.create_container(image=_IMAGE_TAG+':latest',
                                     labels=['com.sonata.analyze'],
                                     ports=[8787],
                                     host_config=host_config)
    container_id = container.get('Id')
    cli.start(container=container_id)

    def cleanup():
        """Remove the container"""
        cli.remove_container(container=container_id, force=True)

    def signal_term_handler(unused1, unused2):  # noqa pylint: disable=unused-argument
        """Catch signal to clean the containers"""
        print('Interruption detected, stopping environment')
        cleanup()
        sys.exit(1)

    signal.signal(signal.SIGTERM, signal_term_handler)
    signal.signal(signal.SIGINT, signal_term_handler)

    print('Browse http://localhost:8787 \n'
          'The default username/password is: rstudio/rstudio\n'
          'Type Ctrl-C to exit')
    exit_code = 0
    exit_code = cli.wait(container=container_id)
    cleanup()
    sys.exit(exit_code)


def version(args: Namespace) -> None:
    """Print the current version and exit"""
    msg = 'son-analyze version: {}'.format(__version__)
    if args.short:
        msg = __version__
    print(msg)
    sys.exit(0)


def dummy(_: Namespace) -> None:
    """Do something dummy and exit"""
    print('Dummy')
    sys.exit(1)


def dispatch(raw_args: List) -> None:
    """Parse the raw_args and dispatch the control flow"""
    parser = ArgumentParser(description=('An analysis framework '
                                         'creation tool for Sonata'))
    parser.add_argument('-v', '--verbose', type=int, default=0,
                        help='increase verbosity')

    def no_command(_: Namespace) -> None:
        """Print the help usage and exit"""
        parser.print_help()
        sys.exit(0)
    parser.set_defaults(func=no_command)
    subparsers = parser.add_subparsers()

    parser_version = subparsers.add_parser('version', help='Show the version')
    parser_version.add_argument('--short', default=False, action='store_true',
                                help='Shows only the version')
    parser_version.set_defaults(func=version)

    parser_bootstrap = subparsers.add_parser('bootstrap',
                                             help='Bootstrap son-analyze')
    parser_bootstrap.set_defaults(func=bootstrap)

    parser_run = subparsers.add_parser('run', help='Run an environment')
    parser_run.add_argument('--dynamic-mount', default=False,
                            action='store_true',
                            help=('(Dev) Dynamically mount the R code'
                                  ' inside the environment'))
    parser_run.set_defaults(func=run)

    parser_dummy = subparsers.add_parser('dummy', help='Do something dummy')
    parser_dummy.set_defaults(func=dummy)

    args = parser.parse_args(raw_args)
    args.func(args)
    assert False  # this line is impossible to reach


def main() -> None:
    """Main entry point for the son-analyze command line tool"""
    dispatch(sys.argv[1:])
