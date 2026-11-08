#!/bin/sh
# GAS, the Grassroots App Store, at peoplesweb.org: build the worlds and the site from
# cascade/ and programs/ (half a second), get the Dart packages, and serve.
# Run from any directory, in a clone of this repository, with Python 3 and
# Dart on the PATH; PORT, HOST and MAX_WORLDS as in README.md.  After a git
# pull, run it again (or restart the service, deploy/peoplesweb.service) and
# the new contracts are on the site; deploy/sweep.sh does that hourly.
set -e
cd "$(dirname "$0")"
python3 -I tools/build.py
dart pub get
exec dart run bin/server.dart
