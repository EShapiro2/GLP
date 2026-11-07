#!/bin/bash
# The Mac's run-slot wrapper, tmp/glp-run.sh, in the container: the job name dropped, the command run.
shift
exec "$@"
