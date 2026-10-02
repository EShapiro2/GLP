# The friendship graph at the end of a run of the social graph, and with
# months=<k> at the end of each of its first k months, read from the run's log
# (sGLP's paper, the repository svGLP-Stochastic-Volitional-GLP at d2f64b6:
# Section 4, "The log", and Appendix B, the log's format; Section 5,
# Definition "Friendship Graph of a Run": {p, q} is an edge at time t iff
# before t one of them answered yes to a card showing the other).
#
#   awk -v n=<agents> [-v edges=<file>] [-v months=<k>] -f friendship.awk <log>
#
# writes, with edges=<file>, the edges to <file>, one per line, "p q" with
# p < q, unsorted, and prints the counts, one per line:
#
#   answers <log lines that are answers>
#   menu_answers <answers to a Menu>      card_answers <answers to a Card>
#   yes <cards answered yes>
#   edges <edges>                         components <connected components of 1..n>
#   isolated <agents with no edge>        largest <agents in the largest component>
#   clock <the monitor's last line, the time the run ended; none if it did not>
#   unread <lines of a shape this reader does not know>
#
# These are of the whole log.  With months=<k> (sGLP's code task of 2026-10-02
# 15:23 UTC, item 3), they are followed by one line per month m of 1..k, a month
# being 2629800 s, 30.4375 days, the twelfth of a year of 365.25 days:
#
#   month <m> at <m * 2629800> edges <e> components <c> isolated <i> largest <l> within <w> across <x>
#
# the friendship graph at the month's end, simulated time m * 2629800: its
# edges are the pairs {p, q} such that p answered yes to a card showing q, or q
# to one showing p, on a log line whose time is less than the month's end;
# components, isolated and largest are as above, of agents 1..n at that time;
# within counts the edges between two agents of one sex and across the edges
# between agents of the two, agents 1..n/2 being of one sex and the rest of the
# other (harness.glp).  Months past the run's clock are printed as the rest,
# the graph standing as the log leaves it.
#
# A log line is "<time>\t<agent>\t<type>\t<term>", the term the question's with
# the answer in place: menu(Ss, Os, same(P)) or menu(Ss, Os, other(P)) for a
# Menu, card(P, yes) or card(P, no) for a Card; the monitor's last line is the
# clock alone.  A yes on a card at <agent> showing P is the edge {<agent>, P}
# from <time> on.

BEGIN { FS = "\t"; clock = "none"; MONTH = 2629800 }

NF == 1 && $1 ~ /^[0-9.e+-]+$/ { clock = $1; next }

NF != 4 { unread++; next }

$3 == "Menu" {
    if ($4 ~ /^menu\(.*(same|other)\([^()]*\)\)$/) { answers++; menu_answers++ }
    else unread++
    next
}

$3 == "Card" {
    if (match($4, /^card\([^,()]*, (yes|no)\)$/)) {
        answers++; card_answers++
        p = substr($4, 6, index($4, ", ") - 6)
        if ($4 ~ /, yes\)$/) { yes++; edge($2 + 0, p + 0, $1 + 0) }
    } else unread++
    next
}

{ unread++ }

# edge(x, y, t): the yes at time t makes {x, y} an edge; since[k] is the
# earliest time it was made, whatever the order of the log's lines.
function edge(x, y, t,    k) {
    if (x == y) return
    k = (x < y) ? x " " y : y " " x
    if (!(k in seen)) {
        seen[k] = 1; since[k] = t; nedges++
        if (edges != "") print k > edges
    } else if (t < since[k]) since[k] = t
}

function find(x) {
    while (parent[x] != x) { parent[x] = parent[parent[x]]; x = parent[x] }
    return x
}

# graph(all, t): the friendship graph of agents 1..n with every edge if all,
# else with the edges made before time t; sets g_edges, g_components,
# g_isolated, g_largest, g_within and g_across.
function graph(all, t,    x, k, e, r, r1, r2, half) {
    delete parent; delete deg; delete size
    half = int(n / 2)
    g_edges = 0; g_components = 0; g_isolated = 0; g_largest = 0
    g_within = 0; g_across = 0
    for (x = 1; x <= n; x++) { parent[x] = x; deg[x] = 0 }
    for (k in seen) {
        if (!all && !(since[k] < t)) continue
        split(k, e, " ")
        g_edges++
        if ((e[1] <= half) == (e[2] <= half)) g_within++; else g_across++
        deg[e[1]]++; deg[e[2]]++
        r1 = find(e[1]); r2 = find(e[2])
        if (r1 != r2) parent[r1] = r2
    }
    for (x = 1; x <= n; x++) {
        r = find(x); size[r]++
        if (deg[x] == 0) g_isolated++
    }
    for (r in size) { g_components++; if (size[r] > g_largest) g_largest = size[r] }
}

END {
    graph(1, 0)
    printf "answers %d\nmenu_answers %d\ncard_answers %d\nyes %d\n",
        answers, menu_answers, card_answers, yes
    printf "edges %d\ncomponents %d\nisolated %d\nlargest %d\nclock %s\nunread %d\n",
        nedges, g_components, g_isolated, g_largest, clock, unread
    for (m = 1; m <= months + 0; m++) {
        graph(0, m * MONTH)
        printf "month %d at %d edges %d components %d isolated %d largest %d within %d across %d\n",
            m, m * MONTH, g_edges, g_components, g_isolated, g_largest, g_within, g_across
    }
}
