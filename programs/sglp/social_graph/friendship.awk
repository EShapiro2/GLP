# The friendship graph at the end of a run of the social graph (graph.vglp),
# read from the run's log (sGLP 8ddff2e, Definition "Interface Variable, Log";
# Definition "Friendship Graph of a Run": {p, q} is an edge iff one of them
# answered yes to a card showing the other).
#
#   awk -v n=<agents> -v edges=<file> -f friendship.awk <log>
#
# writes the edges to <file>, one per line, "p q" with p < q, unsorted, and
# prints the counts, one per line:
#
#   entries <log entries>
#   menus <menus asked>             menu_answers <menus answered>
#   cards <cards shown>             card_answers <cards answered>
#   yes <cards answered yes>
#   edges <edges>                   components <connected components of 1..n>
#   isolated <agents with no edge>  largest <agents in the largest component>
#   unread <entries of a shape this reader does not know>
#
# A log entry is "t TAB a TAB V := s" (lib/sglp/log.dart).  The agent's
# program writes menu(Ss, Os, C) and card(P, A); the person's answer is the
# assignment to C, same(..) or other(..), and to A, yes or no.  An assignment
# of a reader V' to C or A passes the question on to V'.

BEGIN { FS = "\t" }

{
    entries++
    a = $2
    i = index($3, " := ")
    if (NF != 3 || i == 0) { unread++; next }
    v = substr($3, 1, i - 1)
    s = substr($3, i + 4)

    if (s ~ /^menu\(/) {
        menus++
        if (match(s, /V[0-9]+\)$/)) menu[substr(s, RSTART, RLENGTH - 1)] = a
        else unread++
        next
    }
    if (s ~ /^card\(/) {
        cards++
        if (match(s, /^card\([^,]+, V[0-9]+\)$/)) {
            inner = substr(s, 6, length(s) - 6)
            j = index(inner, ", ")
            p = substr(inner, 1, j - 1)
            av = substr(inner, j + 2)
            card_agent[av] = a; card_peer[av] = p
        } else unread++
        next
    }
    if (v in menu) {
        if (s ~ /^V[0-9]+\?$/) menu[substr(s, 1, length(s) - 1)] = menu[v]
        else if (s ~ /^(same|other)\(/) menu_answers++
        else unread++
        delete menu[v]
        next
    }
    if (v in card_agent) {
        if (s ~ /^V[0-9]+\?$/) {
            w = substr(s, 1, length(s) - 1)
            card_agent[w] = card_agent[v]; card_peer[w] = card_peer[v]
        } else if (s == "yes" || s == "no") {
            card_answers++
            if (s == "yes") { yes++; edge(card_agent[v] + 0, card_peer[v] + 0) }
        } else unread++
        delete card_agent[v]; delete card_peer[v]
        next
    }
}

function edge(x, y,    k) {
    if (x == y) return
    k = (x < y) ? x " " y : y " " x
    if (!(k in seen)) { seen[k] = 1; nedges++; print k > edges }
}

function find(x) {
    while (parent[x] != x) { parent[x] = parent[parent[x]]; x = parent[x] }
    return x
}

END {
    for (x = 1; x <= n; x++) { parent[x] = x; deg[x] = 0 }
    for (k in seen) {
        split(k, e, " ")
        deg[e[1]]++; deg[e[2]]++
        r1 = find(e[1]); r2 = find(e[2])
        if (r1 != r2) parent[r1] = r2
    }
    for (x = 1; x <= n; x++) {
        r = find(x); size[r]++
        if (deg[x] == 0) isolated++
    }
    for (r in size) { components++; if (size[r] > largest) largest = size[r] }
    printf "entries %d\nmenus %d\nmenu_answers %d\ncards %d\ncard_answers %d\n",
        entries, menus, menu_answers, cards, card_answers
    printf "yes %d\nedges %d\ncomponents %d\nisolated %d\nlargest %d\nunread %d\n",
        yes, nedges, components, isolated, largest, unread
}
