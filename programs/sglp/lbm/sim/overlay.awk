# The overlay of a GDO simulation, replayed from a run's log and its map
# (programs/sglp/lbm/GDO_sglp.glp; programs/sglp/cva/run.sh's log, one line per
# answer or output, "<time>\t<agent>\t<type>\t<term>", and census.sh's map,
# "<a> <key> <x> <y> <known at the start>").
#
#   awk -v census=<map> [-v metric=l1|l2] [-v days=<k>] [-v months=<k>] [-v d=<d>] -f overlay.awk <log>
#
# From the log: the requests by class (the Menu answers), the refusals by class
# and by refuser (the person's own machine, or a peer's), and each agent's
# last view, whose atoms give the overlay at the end: points_to(Q) at agent a
# is the edge a -> Q.  With the map: the length of each edge by the metric
# (l1, the default, or l2), the mean and the longest, against the ideal of the
# instance, each person's d nearest friends at the end (d defaults to 3) ---
# the mean length of the d shortest friendship edges at each person; and
# the bound, each person's d nearest persons of all; and
# whether every pointing is to a person reachable by friendships, the
# cumulative friendship graph being read off the views (Proposition 2 of the
# paper's Appendix C).  Against the optimum, the graph in which every person
# points to its d nearest persons of all: the stretch (the mean edge over the
# optimum's), the gap (the difference), and the optimal edges --- the pointings
# that are to one of the person's d nearest of all --- as a fraction of the
# pointings and of the optimum's edges; and the same against the ideal of
# friends.  With days=k (months=k), the overlay at the end of each of the first
# k days (months of 30 days), one row each, for the table of how it tracks the
# optimum as the persons travel.  The last line is a summary, tab-separated,
# for table.sh.

BEGIN {
    FS = "\t"
    if (metric == "") metric = "l1"
    if (d == "") d = 3
    step = 0; mapseed = 0
    if (census != "") {
        while ((getline line < census) > 0) {
            n = split(line, f, " ")
            if (f[1] == "#") { if (metric == "" || metric == "l1") metric = f[4]; mapseed = f[9]; step = f[10]; if (d == 3) d = f[7]; geo = f[3]; side = (geo == "manhattan") ? 21000 : 10000; continue }
            a = f[1]; key[a] = f[2]; agent_of[f[2]] = a; X0[a] = f[3]; Y0[a] = f[4]; W[a] = (n >= 6) ? f[6] : 1; N = a
        }
        close(census)
        for (a = 1; a <= N; a++) { X[a] = X0[a]; Y[a] = Y0[a]; S[a] = (X0[a] * 10007 + Y0[a] + mapseed + 1); day[a] = 0 }
    }
    month = 30 * 86400
}

function dist(a, b,    dx, dy) {
    dx = X[a] - X[b]; dy = Y[a] - Y[b]
    if (dx < 0) dx = -dx
    if (dy < 0) dy = -dy
    if (metric == "l2") return sqrt(dx * dx + dy * dy)
    return dx + dy
}

# The trips, as GDO_sglp.glp's gd_pos_at: a destination drawn by the minimal standard generator (in Manhattan one of
# the drivers' places, the k-th person's start, k = s mod 200 + 1; on a square a point drawn uniformly), Step a day
# towards it along the grid, x first, then the next destination; positions advanced to day k.
function lcg(a) { S[a] = (S[a] * 48271) % 2147483647; return S[a] }
function target(a,    s1, s2) {
    if (geo == "manhattan") { s1 = lcg(a); k = s1 % 200 + 1; TX[a] = X0[k]; TY[a] = Y0[k] }
    else { s1 = lcg(a); s2 = lcg(a); TX[a] = s1 % (side + 1); TY[a] = s2 % (side + 1) }
}
function towards(x, tx, bd) { if (tx - x > bd) return x + bd; if (x - tx > bd) return x - bd; return tx }
function advance(k,    a, rem) {
    if (step == 0) return
    for (a = 1; a <= N; a++) while (day[a] < k) {
        if (!W[a]) { day[a]++; continue }
        if (!(a in TX)) target(a)
        rem = step - (TX[a] > X[a] ? (TX[a] - X[a] > step ? step : TX[a] - X[a]) : (X[a] - TX[a] > step ? step : X[a] - TX[a]))
        X[a] = towards(X[a], TX[a], step); Y[a] = towards(Y[a], TY[a], rem)
        if (X[a] == TX[a] && Y[a] == TY[a]) target(a)
        day[a]++
    }
}

# The clock: positions advance with it (a day's step at each day's end).
{ if (census != "" && step > 0) advance(int($1 / 86400)) }

# The answers.
$3 == "Menu" || $3 == "Request?" {
    if (match($4, /propose\([a-z_]+/)) requests[substr($4, RSTART + 8, RLENGTH - 8)]++
    next
}
$3 == "Approval" { approvals[($4 ~ /, yes\)$/) ? "yes" : "no"]++; next }

# The outputs: a refusal, or a view, whose atoms replace the agent's last.
$3 == "output" && $4 ~ /^refused\(/ {
    # refused(<class>(<args>), <refuser>, <reason>)
    s = substr($4, 9)
    match(s, /^[a-z_]+/); cls = substr(s, 1, RLENGTH)
    s = substr(s, index(s, "), ") + 3)
    match(s, /^[a-z0-9_]+/); refuser = substr(s, 1, RLENGTH)
    who = (agent_of[refuser] == $2) ? "own" : "peer"
    refusals[cls " " who]++
    next
}
$3 == "output" && $4 ~ /^view\(/ {
    a = $2; t = $1 + 0
    # The days' and months' snapshots: before the first view past a day's (month's) end, the overlay as it stood.
    while (days != "" && di < days && t >= (di + 1) * 86400) { di++; snapshot("day", di) }
    while (months != "" && mi < months && t >= (mi + 1) * month) { mi++; snapshot("month", mi) }
    s = $4
    sub(/^view\(\[/, "", s); sub(/\].*$/, "", s)
    # Churn: pointings and friendships that appear or disappear between this view and the agent's last.
    for (i = 1; i <= npts[a]; i++) old[pts[a, i]] = 1
    for (i = 1; i <= nfr[a]; i++) oldf[fr[a, i]] = 1
    delete pts[a]; delete fr[a]
    npts[a] = 0; nfr[a] = 0
    while (match(s, /(points_to|friend)\([a-z0-9_]+\)/)) {
        atom = substr(s, RSTART, RLENGTH)
        p = index(atom, "("); pred = substr(atom, 1, p - 1); q = substr(atom, p + 1, length(atom) - p - 1)
        if (pred == "points_to") { npts[a]++; pts[a, npts[a]] = q; pointed[a, q] = 1 }
        if (pred == "friend") { nfr[a]++; fr[a, nfr[a]] = q; ever_friend[a, q] = 1; ever_friend[agent_of[q], key[a]] = 1 }
        s = substr(s, RSTART + RLENGTH)
    }
    for (i = 1; i <= npts[a]; i++) { if (!(pts[a, i] in old)) { added++; added_d[int(t / 86400)]++ } else delete old[pts[a, i]] }
    for (q in old) { removed++; removed_d[int(t / 86400)]++ }
    delete old
    for (i = 1; i <= nfr[a]; i++) { if (!(fr[a, i] in oldf)) { fadded++; if (t > 86400) fadded_late++ } else delete oldf[fr[a, i]] }
    for (q in oldf) { fremoved++; if (t > 86400) fremoved_late++ }
    delete oldf
    views[a]++
    next
}

# The measures of the overlay as it stands, at the positions as they stand: the pointing edges (E), their mean
# length (MEAN) and the longest (LONGEST), the transitive ones (TT), the pointings that are to one of the person's d
# nearest persons of all (OPT) and to one of its d nearest friends (OPTF), the optimum's mean (BOUND, over BE edges) and
# the ideal's (IDEAL, over IE edges) --- the d nearest persons of all, and the d nearest friends, at each person.
function measure(    a, b, i, j, n, v, isf, l) {
    E = 0; MEAN = 0; LONGEST = 0; TT = 0; OPT = 0; OPTF = 0; BOUND = 0; BE = 0; IDEAL = 0; IE = 0
    for (a = 1; a <= N; a++) {
        # the d nearest of all, by selection
        n = 0
        for (b = 1; b <= N; b++) if (b != a) { ls[++n] = dist(a, b); lb[n] = b }
        for (i = 2; i <= n; i++) { v = ls[i]; b = lb[i]; j = i - 1; while (j >= 1 && ls[j] > v) { ls[j + 1] = ls[j]; lb[j + 1] = lb[j]; j-- } ls[j + 1] = v; lb[j + 1] = b }
        delete nearest
        for (i = 1; i <= n && i <= d; i++) { BOUND += ls[i]; BE++; nearest[lb[i]] = 1 }
        delete ls; delete lb
        # the d nearest friends
        n = 0
        for (i = 1; i <= nfr[a]; i++) { ls[++n] = dist(a, agent_of[fr[a, i]]); lb[n] = agent_of[fr[a, i]] }
        for (i = 2; i <= n; i++) { v = ls[i]; b = lb[i]; j = i - 1; while (j >= 1 && ls[j] > v) { ls[j + 1] = ls[j]; lb[j + 1] = lb[j]; j-- } ls[j + 1] = v; lb[j + 1] = b }
        delete nearestf
        for (i = 1; i <= n && i <= d; i++) { IDEAL += ls[i]; IE++; nearestf[lb[i]] = 1 }
        delete ls; delete lb
        # the pointings
        for (i = 1; i <= npts[a]; i++) {
            b = agent_of[pts[a, i]]; E++
            if (census != "") { l = dist(a, b); MEAN += l; if (l > LONGEST) LONGEST = l; if (b in nearest) OPT++; if (b in nearestf) OPTF++ }
            isf = 0; for (j = 1; j <= nfr[a]; j++) if (fr[a, j] == pts[a, i]) isf = 1
            if (!isf) TT++
        }
    }
    if (E > 0) MEAN = MEAN / E
    if (BE > 0) BOUND = BOUND / BE
    if (IE > 0) IDEAL = IDEAL / IE
}

function snapshot(unit, k) {
    measure()
    if (census == "") { printf "%s %d: %d pointing edges, %d transitive\n", unit, k, E, TT; return }
    if (!header_done) { header_done = 1; printf "%-9s %5s %5s %5s %5s %7s %5s %7s %7s\n", "", "edges", "trans", "mean", "optim", "stretch", "ideal", "optedge", "optcovr" }
    printf "%-9s %5d %5d %5.0f %5.0f %7.2f %5.0f %6.0f%% %6.0f%%\n", unit " " k, E, TT, MEAN, BOUND, (BOUND > 0 ? MEAN / BOUND : 0), IDEAL, (E > 0 ? 100 * OPT / E : 0), (BE > 0 ? 100 * OPT / BE : 0)
}

END {
    while (days != "" && di < days) { di++; snapshot("day", di) }
    while (months != "" && mi < months) { mi++; snapshot("month", mi) }
    print "requests by class:"
    for (c in requests) printf "  %-18s %d\n", c, requests[c]
    print "refusals by class and refuser (own machine / a peer's):"
    for (c in refusals) printf "  %-24s %d\n", c, refusals[c]
    print "approvals answered:"
    for (c in approvals) printf "  %-6s %d\n", c, approvals[c]
    printf "churn: pointings made %d, ended %d; friendships made %d (%d after the first day), ended %d\n", added, removed, fadded, fadded_late, fremoved
    if (step > 0) { printf "pointings made / ended by day:"; for (k = 0; k * 86400 <= $1; k++) printf " %d:%d/%d", k, added_d[k] + 0, removed_d[k] + 0; printf "\n" }
    # The overlay at the end, at the positions at the end.
    if (census != "" && step > 0) advance(int($1 / 86400))
    measure()
    maxdeg = 0; zero = 0; over = 0
    for (a = 1; a <= N; a++) {
        deg[npts[a]]++
        if (npts[a] == 0) zero++
        if (npts[a] > d) over++
        if (npts[a] > maxdeg) maxdeg = npts[a]
    }
    printf "overlay at the end: %d edges over %d persons; out-degree", E, N
    for (k = 0; k <= maxdeg; k++) printf " %d:%d", k, deg[k] + 0
    printf "; %d with none, %d above d=%d\n", zero, over, d
    if (census == "") exit
    printf "pointing edges: mean %.0f m, longest %.0f m (%s)\n", MEAN, LONGEST, metric
    printf "ideal, the d nearest friends at the end: %d edges, mean %.0f m\n", IE, IDEAL
    printf "bound, the d nearest persons of all: %d edges, mean %.0f m\n", BE, BOUND
    printf "against the optimum (every person pointing to its d nearest of all): stretch %.2f, gap %.0f m; optimal edges %d of %d pointings (%.0f%%), covering %.0f%% of the optimum's %d edges\n", (BOUND > 0 ? MEAN / BOUND : 0), MEAN - BOUND, OPT, E, (E > 0 ? 100 * OPT / E : 0), (BE > 0 ? 100 * OPT / BE : 0), BE
    printf "against the ideal of friends (the d nearest friends): stretch %.2f, gap %.0f m; %d pointings to one of them (%.0f%%)\n", (IDEAL > 0 ? MEAN / IDEAL : 0), MEAN - IDEAL, OPTF, (E > 0 ? 100 * OPTF / E : 0)
    printf "pointings to a friend: %d; to a person who is not a friend (transitive): %d\n", E - TT, TT
    # Proposition 2: every pointing within a component of the cumulative friendship graph.
    for (a = 1; a <= N; a++) comp[a] = a
    changed = 1
    while (changed) {
        changed = 0
        for (a = 1; a <= N; a++) for (b = 1; b <= N; b++) if (ever_friend[a, key[b]] && comp[a] != comp[b]) {
            c = (comp[a] < comp[b]) ? comp[a] : comp[b]; comp[a] = c; comp[b] = c; changed = 1
        }
    }
    bad = 0
    for (a = 1; a <= N; a++) for (i = 1; i <= npts[a]; i++) if (comp[a] != comp[agent_of[pts[a, i]]]) bad++
    ncomp = 0; for (a = 1; a <= N; a++) if (comp[a] == a) ncomp++
    printf "cumulative friendship graph: %d components; pointings across components: %d\n", ncomp, bad
    # The summary, for table.sh: tab-separated.
    printf "summary\tedges=%d\tmaxdeg=%d\tover=%d\tmean=%.0f\toptimum=%.0f\tstretch=%.2f\tgap=%.0f\toptedge=%.0f\toptcover=%.0f\tideal=%.0f\ttransitive=%d\tmade=%d\tended=%d\tmade1=%d\tended1=%d\tfmade1=%d\tfended1=%d\ttrans_req=%d\ttrans_peer=%d\tacross=%d\n", E, maxdeg, over, MEAN, BOUND, (BOUND > 0 ? MEAN / BOUND : 0), MEAN - BOUND, (E > 0 ? 100 * OPT / E : 0), (BE > 0 ? 100 * OPT / BE : 0), IDEAL, TT, added, removed, added - added_d[0], removed - removed_d[0], fadded_late, fremoved_late, requests["point_transitive"] + 0, refusals["point_transitive peer"] + 0, bad
}
