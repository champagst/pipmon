"""Main interface to the RPC server.

You should be able to just run the following to use this module:

python -m pipmon

The first line should be "pipmon-rpc ready". If it isn't, something
broke.

"""

import os
import sys

import pipmon
from pipmon.server import PipmonServer

if __name__ == '__main__':
    stdin = sys.stdin
    stdout = sys.stdout
    # sys.stdout = sys.stderr = open(os.devnull, "w")
    stdout.write('pipmon-rpc ready ({0})\n'
                 .format(pipmon.__version__))
    stdout.flush()
    PipmonServer(stdin, stdout).serve_forever()
