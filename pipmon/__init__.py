# pipmon, Gitlab pipeline monitoring

# Copyright (C) 2013-2019 Jorgen Schaefer <http://github.com/jorgenschaefer/elpy>

# Author: Steven Champagne
# URL: URL: https://github.com/champagst/pipmon

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

"""
This package provides the backend within Python to support long-running background tasks.

Emacs will start the protocol by running the module itself, like so:

  python -m pipmon

This will emit a greeting string on a single line, and then wait for
the protocol to start. Details of the protocol can be found in
pipmon.rpc.

This package is unlikely to be useful on its own.

"""

__author__ = "Steven Champagne"
__version__ = "1.35.0"
__license__ = "GPL"
