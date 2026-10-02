# The friendship graph at the end of a run of the social graph, read from the
# run's log (sGLP's paper, the repository svGLP-Stochastic-Volitional-GLP at
# d2f64b6: Section 4, "The log", and Appendix B, the log's format; Section 5,
# Definition "Friendship Graph of a Run": {p, q} is an edge iff one of them
# answered yes to a card showing the other).
#
#   awk -v n=<agents> -v edges=<file> -f friendship.awk <log>
#
# writes the edges to <file>, one per line, "p q" with p < q, unsorted, and
# prints the counts, one per line:
#
#   answers <log lines that are answers>
#   menu_answers <answers to a Menu>      card_answers <answers to a Card>
#   yes <cards answered yes>
#   edges <edges>                         components <connected components of 1..n>
#   isolated <agents with no edge>        largest <agents in the largest component>
#   clock <the monitor's last line, the time the run ended; none if it did not>
#   unread <lines of a shape this reader does not know>
#
# A log line is "<time>\t<agent>\t<type>\t<term>", the term the question's with
# the answer in place: menu(Ss, Os, same(P)) or menu(Ss, Os, other(P)) for a
# Menu, card(P, yes) or card(P, no) for a Card; the monitor's last line is the
# clock alone.

BEGIN { FS = "\t"; clock = "none" }

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
        if ($4 ~ /, yes\)$/) { yes++; edge($2 + 0, p + 0) }
    } else unread++
    next
}

{ unread++ }

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
    printf "answers %d\nmenu_answers %d\ncard_answers %d\nyes %d\n",
        answers, menu_answers, card_answers, yes
    printf "edges %d\ncomponents %d\nisolated %d\nlargest %d\nclock %s\nunread %d\n",
        nedges, components, isolated, largest, clock, unread
}
