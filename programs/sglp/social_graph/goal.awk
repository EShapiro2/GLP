# The harness's initial goal of a run of the social graph (graph.vglp), as the
# REPL's `:at` command posts it: one conjunction, each conjunct placed.
#
#   awk -v n=<agents> -f goal.awk
#
# prints one line, `:at <placement> <goal>`:
#
#   start(a, Ss, Os, Ia?, Ua) at agent a, for a of 1..n: agents 1..n/2 are of
#   one sex and n/2+1..n of the other (sGLP 8ddff2e, Section 5: half the agents
#   of each sex), Ss the other agents of a's sex and Os the agents of the other
#   sex, each list in increasing order, Ia the agent's input stream and Ua its
#   output stream;
#
#   at no agent, mwm([merge(U1?), ..., merge(Un?)], M), the root's multiway
#   merge of the agents' output streams, and a balanced tree of n-1 goals
#   deliver(D?, Mid, L, H) over 1..n, whose leaves are I1..In, which hands each
#   message to the input stream of the agent it is addressed to.
#
# An agent is named by its number in the run, so a Peer is an integer.

function peers(lo, hi, skip,    i, sep) {
    printf "["; sep = ""
    for (i = lo; i <= hi; i++) {
        if (i == skip) continue
        printf "%s%d", sep, i; sep = ", "
    }
    printf "]"
}

# The deliver goals of the subtree over lo..hi, whose input stream is v.
function tree(lo, hi, v,    mid, l, h) {
    if (lo == hi) return
    mid = int((lo + hi) / 2)
    l = (lo == mid) ? "I" lo : "D" (++fresh)
    h = (mid + 1 == hi) ? "I" hi : "D" (++fresh)
    printf ", deliver(%s?, %d, %s, %s)", v, mid, l, h
    tree(lo, mid, l)
    tree(mid + 1, hi, h)
}

BEGIN {
    if (n !~ /^[0-9]+$/ || n < 2 || n % 2 != 0) {
        print "goal.awk: n must be an even number of agents, at least 2" > "/dev/stderr"
        exit 1
    }
    half = n / 2
    # n agents placed in order, then the mwm goal and the n-1 deliver goals at
    # no agent.
    printf ":at 1..%d", n
    for (i = 1; i <= n; i++) printf ",-"
    printf " "
    for (a = 1; a <= n; a++) {
        printf "%sstart(%d, ", (a > 1 ? ", " : ""), a
        if (a <= half) { peers(1, half, a); printf ", "; peers(half + 1, n, 0) }
        else           { peers(half + 1, n, a); printf ", "; peers(1, half, 0) }
        printf ", I%d?, U%d)", a, a
    }
    printf ", mwm(["
    for (a = 1; a <= n; a++) printf "%smerge(U%d?)", (a > 1 ? ", " : ""), a
    printf "], M)"
    fresh = 0
    tree(1, n, "M")
    printf "\n"
}
