#!/bin/bash
# The functions of the standard library a contract applies (README.md beside this file, item 47):
#
#   bash programs/vglp/cva/compiler/libfns.sh @<file holding the contract's reading>
#
# prints fns(Fns, Lines), as pragmas.sh prints its eighth argument, of
# programs/lib/numbers/numbers.glp where the reading, as GSC's as_read/2 prints
# it, applies one of its functions, and nothing where it applies none.
# compile.sh gives it to the compiler with the pragmas of vGLP's own contracts,
# with_fns(Pragmas, Fns) (cvac.glp, norm_pragmas).
set -u
[ $# -eq 1 ] && [ "${1#@}" != "$1" ] && [ -f "${1#@}" ] || { echo "usage: libfns.sh @<file holding the reading>" >&2; exit 2; }
PROGRAMS="$(cd "$(dirname "$0")/../../.." && pwd)"
exec perl -e '
use strict; use warnings;
my ($programs, $tf) = @ARGV;
require "$programs/vglp/cva/compiler/fns.pl";
my $term = do { open(my $th, "<", $tf) or die "libfns.sh: $tf cannot be read\n"; local $/; <$th> };
my (@fns, @lines, %seen);
lib_fns($programs, $term, \@fns, \@lines, \%seen);
print "fns([" . join(", ", @fns) . "], [" . join(", ", @lines) . "])\n" if @fns;
' "$PROGRAMS" "${1#@}"
