#!/bin/bash
# The map of a GDO simulation: the persons the census draws, printed without
# the run (programs/sglp/lbm/GDO_sglp.glp, "Configuration": the census is a function
# of the configuration alone).
#
#   bash programs/sglp/lbm/sim/census.sh <platform.glp> <printed> <census> <out>
#
#   <platform.glp>  the platform's canonical compilation, cascade/GDO/platform.glp;
#   <printed>       the directory holding profiles.glp and population.glp printed
#                   by programs/sglp/transform.sh --interface from GDO_sglp.glp;
#   <census>        the census's name, gd_town40 or gd_town100;
#   <out>           the map: a first line "# cfg <Geometry> <Metric> <Nearest> <Random> <D> <E> <MapSeed> <Step> <Movers> <Meet> <Part> <Motion>" (Motion the rule the travellers move by, trips),
#                   the configuration as profiles.glp prints it, then one line per person,
#                   "<a> <key> <x> <y> <known at the start, comma-separated> <w>", a the agent's number,
#                   the person's place in the census, x and y its location at the start, w 1 where the
#                   person walks and 0 where it stays put.
#
# It assembles a scratch directory under programs/sglp/runs/ (so that sglp.glp's types, Slot among
# them, are in its scope, as they are in a run's), as programs/sglp/cva/run.sh
# does, whose self.glp exposes the platform and calls the census of the profiles module.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
GLP_DIR="$(cd "$HERE/../../../.." && pwd)"
RT="$GLP_DIR/glp_runtime"
PLATFORM=$1; PRINTED=$2; CENSUS=$3; OUT=$4
RUNDIR="$GLP_DIR/programs/sglp/runs/gdo_census_$$"
trap 'rm -rf "$RUNDIR"' EXIT
mkdir -p "$RUNDIR" || exit 2
cp "$PLATFORM" "$RUNDIR/platform.glp" && cp "$PRINTED/profiles.glp" "$RUNDIR/profiles.glp" || exit 2
cat > "$RUNDIR/self.glp" <<GLP
-expose(platform).
GdLoc  ::= loc(Key, Integer, Integer, Integer).
GdCfg  ::= cfg(Constant, Constant, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer).
GdAttr ::= at(Integer, Integer) ; knows(List(Key)) ; table(List(GdLoc)) ; cfg(GdCfg).
Person ::= person(Key, Constant, List(GdAttr)).
imported procedure profiles#$CENSUS(Integer?, List(Person)).
exported procedure town(List(Person)).
town(Ps?) :- profiles # $CENSUS(0, Ps).
GLP
grep -m1 '^gd_config(cfg(' "$PRINTED/profiles.glp" | sed -e 's/^gd_config(cfg(//; s/)).*$//; s/,//g; s/^/# cfg /; s/$/ trips/' > "$OUT"
printf ':limit 1000000000\n%s\ntown(Ps).\n:quit\n' "$RUNDIR" | (cd "$RT" && bin/glpc 2>&1) \
  | sed -e 's/^\(GLP> \)*//' | grep -E '^Ps = ' | sed -e 's/^Ps = //' \
  | perl -ne '
      my $a = 0;
      my %w;
      while (/loc\((\w+), \d+, \d+, (\d)\)/g) { $w{$1} = $2 unless exists $w{$1} }
      while (/person\((\w+), \w+, \[at\((\d+), (\d+)\), knows\(\[([^\]]*)\]\)/g) {
          $a++; my ($k, $x, $y, $kn) = ($1, $2, $3, $4); $kn =~ s/ //g; $kn = "-" if $kn eq "";
          print "$a $k $x $y $kn ", ($w{$k} // 0), "\n";
      }' >> "$OUT"
N=$(($(wc -l < "$OUT" | tr -d ' ') - 1))
[ "$N" -ge 1 ] && echo "census $CENSUS: $N persons written to $OUT" || { echo "census.sh: $CENSUS printed no person" >&2; exit 1; }
