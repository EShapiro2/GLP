# timeline.awk: prints the events of a GSG run's log: identities, knows and friends.
# "<time>\t<agent>\t<type>\t<term>") and prints its timeline: each fault, and each change of an
# agent's identity, friends, identity custodians, records of others' identity custodians, vouches
# and replacements, as the agent's own view shows them; then each agent's last state.
#
#   awk -f programs/sglp/spm/timeline.awk <log>
#
# An atom of a view is counted once however many copies the state holds.
BEGIN {
    FS = "\t"
    name[1] = "Alice(p1)"; name[2] = "Bob(p2)"; name[3] = "Carol(p3)"
    name[4] = "Dave(p4)"; name[5] = "Eve(p5)"; name[6] = "Frank(p6)"; name[7] = "Grace"; name[8] = "Heidi"; name[9] = "Ivan"; name[10] = "Judy"; name[11] = "Kim"; name[12] = "Leo"
    nw = split("friend knows", watch, " ")
}
# atoms(S): splits the list of atoms S at the commas outside parentheses into at[1..na].
function atoms(s,    i, c, d, cur) {
    na = 0; d = 0; cur = ""
    for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        if (c == "(") d++
        if (c == ")") d--
        if (c == "," && d == 0) { sub(/^ +/, "", cur); at[++na] = cur; cur = "" }
        else cur = cur c
    }
    sub(/^ +/, "", cur)
    if (cur != "") at[++na] = cur
}
$3 == "output" && $4 ~ /^view\(/ {
    a = $2; t = $1 / 3600
    s = $4; sub(/^view\([a-z0-9]+, \[/, "", s); sub(/\], \[.*$/, "", s)
    atoms(s)
    delete now; crashes = 0; id = 0; lost = 0
    for (i = 1; i <= na; i++) {
        x = at[i]
        if (x == "crashed") crashes++
        else if (x == "identity") id = 1
        else if (x == "lost") lost = 1
        else {
            p = x; sub(/\(.*$/, "", p)
            for (k = 1; k <= nw; k++) if (p == watch[k]) now[x] = 1
        }
    }
    who = (a in name) ? name[a] : a
    if (crashes > crashed[a]) { printf "%7.1fh  %-11s *** MINOR FAULT: state lost (fault %d)\n", t, who, crashes; crashed[a] = crashes }
    if (lost && !waslost[a]) { printf "%7.1fh  %-11s *** MAJOR FAULT: key lost\n", t, who; waslost[a] = 1 }
    if (id && !hadid[a]) printf "%7.1fh  %-11s identity established\n", t, who
    hadid[a] = id
    for (k = 1; k <= nw; k++) {
        add = ""; rem = ""
        for (x in now) { p = x; sub(/\(.*$/, "", p); if (p == watch[k] && !((a, x) in held)) add = add " +" x }
        for (ax in held) { split(ax, q, SUBSEP); if (q[1] == a) { x = q[2]; p = x; sub(/\(.*$/, "", p); if (p == watch[k] && !(x in now)) rem = rem " -" x } }
        if (add != "" || rem != "") printf "%7.1fh  %-11s%s%s\n", t, who, add, rem
    }
    for (ax in held) { split(ax, q, SUBSEP); if (q[1] == a) delete held[ax] }
    for (x in now) held[a, x] = 1
    seen[a] = 1
}
END {
    print ""; print "Last state:"
    for (a = 1; a <= 6; a++) if (a in seen) {
        line = ""
        for (k = 1; k <= nw; k++) for (ax in held) { split(ax, q, SUBSEP); if (q[1] == a) { p = q[2]; sub(/\(.*$/, "", p); if (p == watch[k]) line = line " " q[2] } }
        printf "  %-11s%s%s%s\n", name[a], (waslost[a] ? " lost" : (hadid[a] ? " identity" : " no identity")), line, ""
    }
}
