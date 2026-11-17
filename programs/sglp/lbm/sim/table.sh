#!/bin/bash
# The table of the records against the optimum, in Markdown, from each
# record's map (its configuration line) and measures (overlay.awk's summary
# line, the last of measures.txt):
#
#   bash programs/sglp/lbm/sim/table.sh <record> ...
#
# One row per record: its configuration (geometry, metric, d, e, the step and
# the travellers, Meet and Part), the horizon (the manifest's), the edges at the
# end, the mean edge, the optimum (the mean over every person's d nearest of
# all), the stretch (mean / optimum), the optimal edges (the pointings that are
# to one of the person's d nearest of all, as a share of the pointings), the
# ideal of friends (the mean over the d nearest friends), the transitive edges,
# the pointings and the friendships made / ended after the first day, and the
# pointings across the components of the cumulative friendship graph.
printf '| Record | Geometry, metric | d, e | Step, travellers | Meet, Part | Days | Edges | Mean edge | Optimum | Stretch | Optimal edges | Ideal of friends | Transitive | Pointings made / ended after day 1 | Friendships made / ended after day 1 | Across components |\n'
printf '|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|\n'
for R in "$@"; do
  # the configuration line: Geometry Metric Nearest Random D E MapSeed, then Step Movers Meet Part (the records of
  # 2026-11-03 have the first seven alone, their maps regenerated with three zeros; meet100_part50 has Step Meet Part)
  read -r -a c < "$R/1.census"; geo=${c[2]}; met=${c[3]}; d=${c[6]}; e=${c[7]}; step=${c[9]:-0}
  case $((${#c[@]} - 2)) in 7|10) movers=0; meet=${c[10]:-0}; part=${c[11]:-0};; *) movers=${c[10]}; meet=${c[11]}; part=${c[12]};; esac
  [ "$step" = 0 ] && movers=0
  days=$(sed -n 's/^- until: \([0-9]*\) day.*/\1/p' "$R/manifest.md" 2>/dev/null)
  tail -1 "$R/measures.txt" | awk -F'\t' -v name="$(basename "$R")" -v geo="$geo" -v met="$met" -v d="$d" -v e="$e" -v step="$step" -v movers="$movers" -v meet="$meet" -v part="$part" -v days="$days" '
    $1 == "summary" { for (i = 2; i <= NF; i++) { split($i, kv, "="); v[kv[1]] = kv[2] }
      printf "| `%s` | %s, %s | %s, %s m | %s m, %s | %s, %s | %s | %s | %s m | %s m | %s | %s%% | %s m | %s | %s / %s | %s / %s | %s |\n", name, geo, met, d, e, step, movers, meet, part, days, v["edges"], v["mean"], v["optimum"], v["stretch"], v["optedge"], v["ideal"], v["transitive"], v["made1"], v["ended1"], v["fmade1"], v["fended1"], v["across"] }'
done
