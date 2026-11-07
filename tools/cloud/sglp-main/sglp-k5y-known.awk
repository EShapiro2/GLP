# sglp-k5y-known.awk: tmp/sglp-meet/known.awk's figures at several times, for a
# 1,000-agent log (Code #12's subagent, Code #11's task of 2026-10-19 18:49 UTC,
# the five-year run's summary).  The same rules as known.awk, line for line:
# each agent knows at the start its first menu's two lists; at each yes between
# a and b, the other and the other's friends at the yes but for itself; offers
# are the menus answered, other_sex those answered other(P); yes within a sex
# and across, and those to one already a friend.  Two changes, neither in what
# is computed: (i) an approval is read as Card or Approval (Card was renamed
# Approval on 2026-10-17; the shown agent is the number after the parenthesis
# either way); (ii) each agent's friends and the count of the people it knows
# are kept per agent, where known.awk scans every friendship at each yes and
# every known pair at each count, which at 1,000 agents does not finish.
#
#   awk -v n=<agents> -v cuts="<t1> <t2> ..." -f sglp-k5y-known.awk <first> <log>
#
# <first> is the log itself, or its agents' first Menu lines in the log's order
# (sglp-k5y-cuts.awk's firstmenus), which give the same start lists.  Prints,
# at the first line whose time is at least t, for each cut t in order, and at
# the log's end (t "end"), as known.awk prints its known_at line:
#   known_at <t> mean <m> least <l> most <h> agents <a> no_menu <x>
#   at <t> offers <o> other_sex <s> share <r>
#   at <t> yes_within <w> yes_across <x> yes_to_friend_within <fw> yes_to_friend_across <fx>
# and last known.awk's own two lines, of the whole log.
function sex(x) { return x <= n / 2 ? 1 : 2 }
function know(a, x) { if (!((a, x) in K)) { K[a, x] = 1; kn[a]++ } }
function befriend(x, y) { if (!((x, y) in F)) { F[x, y] = 1; adj[x, ++deg[x]] = y } }
function addlist(a, str,   k, p, i) { k = split(str, p, ", "); for (i = 1; i <= k; i++) if (p[i] != "") know(a, p[i] + 0) }
function snap(t,   a, c, s, lo, hi, miss) {
    s = 0; lo = -1; hi = 0; miss = 0
    for (a = 1; a <= n; a++) {
        if (!(a in seen)) { miss++; continue }
        c = kn[a] + 0
        s += c; if (lo < 0 || c < lo) lo = c; if (c > hi) hi = c
    }
    printf "known_at %s mean %.2f least %d most %d agents %d no_menu %d\n", t, s / (n - miss), lo, hi, n - miss, miss
    printf "at %s offers %d other_sex %d share %.4f\n", t, menus, other, menus ? other / menus : 0
    printf "at %s yes_within %d yes_across %d yes_to_friend_within %d yes_to_friend_across %d\n", t, Y["within"], Y["across"], R["within"], R["across"]
}
BEGIN { FS = "\t"; nc = split(cuts, C, " "); ci = 1 }
FNR == NR { if (NF == 4 && $3 == "Menu" && !(($2 + 0) in seen)) {
        a = $2 + 0; seen[a] = 1
        match($4, /^menu\(\[[^]]*\]/); s1 = substr($4, 7, RLENGTH - 7)
        rest = substr($4, RLENGTH + 3); match(rest, /^\[[^]]*\]/); s2 = substr(rest, 2, RLENGTH - 2)
        addlist(a, s1); addlist(a, s2) }
    next }
NF == 4 {
    t = $1 + 0
    while (ci <= nc && t >= C[ci] + 0) { snap(C[ci]); ci++ }
}
NF == 4 && $3 == "Menu" {
    menus++
    if ($4 ~ /other\([^()]*\)\)$/) other++
}
NF == 4 && ($3 == "Card" || $3 == "Approval") && $4 ~ /, yes\)$/ {
    a = $2 + 0; o = index($4, "("); b = substr($4, o + 1, index($4, ",") - o - 1) + 0
    w = (sex(a) == sex(b)) ? "within" : "across"
    Y[w]++; if ((a, b) in F) R[w]++
    na = 0; for (i = 1; i <= deg[b]; i++) if (adj[b, i] != a) NA[++na] = adj[b, i]
    nb = 0; for (i = 1; i <= deg[a]; i++) if (adj[a, i] != b) NB[++nb] = adj[a, i]
    know(a, b); know(b, a)
    for (i = 1; i <= na; i++) know(a, NA[i])
    for (i = 1; i <= nb; i++) know(b, NB[i])
    befriend(a, b); befriend(b, a)
}
END {
    while (ci <= nc) { snap(C[ci]); ci++ }
    snap("end")
    printf "offers %d other_sex %d share %.4f\n", menus, other, menus ? other / menus : 0
    printf "yes_within %d yes_across %d yes_to_friend_within %d yes_to_friend_across %d\n", Y["within"], Y["across"], R["within"], R["across"]
}
