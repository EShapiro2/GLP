# The circulation of a run of coins among friends at the end of each month,
# read from the run's log alone (sGLP's paper, the repository
# svGLP-Stochastic-Volitional-GLP at c7d0b2f: Section 4, "The log", and
# Appendix B, the log's format; Section 6, Definition "Circulation": the total
# of the holdings in the wallets of the agents at t; sGLP's code task of
# 2026-10-02 15:24 UTC, item 4, and of 2026-10-03 08:44 UTC, item 2).
#
#   awk [-v months=<k>] -f circulation.awk <log>
#
# replays the wallets from the log, in its order: an Offer answered yes at
# agent a, showing From and K, adds K to a's holding of From's coins and K to
# From's holding of a's; a Menu answered pay(F, K) at agent a takes min(K, a's
# holding of F's coins) off that holding, a holding of none being 0, and
# removes a holding that reaches zero, when a acts on it: at its answer, or,
# answered while one of a's cards waits, when that card is answered, after
# the card's own move (below); nothing else moves a wallet.  It prints one
# line per month m of 1..k, k being 12 unless months=<k> says otherwise, a
# month being 2629800 s, 30.4375 days, the twelfth of a year of 365.25 days:
#
#   month <m> at <m * 2629800> circulation <c> holdings <h> wallets <w> pays <p> proposals <s> accepted <y> declined <n>
#
# the wallets at the month's end, simulated time m * 2629800, being those the
# log's lines whose time is less than it leave: c the total of the holdings, h
# the pairs of an agent and an issuer whose holding is positive, w the agents
# with a non-empty wallet; and p, s, y and n the menus answered pay and swap
# and the offers answered yes and no on those lines, each counted at its
# answer.  Months past the run's clock are printed as the rest, the wallets
# standing as the log leaves them.  Then the run's totals, of the whole log,
# one per line:
#
#   answers <log lines that are answers>
#   menu_answers <answers to a Menu>      offer_answers <answers to an Offer>
#   pays <p>  proposals <s>  accepted <y>  declined <n>
#   circulation <c>  holdings <h>  wallets <w>, at the log's end
#   clock <the monitor's last line, the time the run ended; none if it did not>
#   unread <lines of a shape this reader does not know>
#   unordered <answers whose time is less than the time of the answer before>
#   unmatched <answers the agent's clauses do not allow where the replay
#             stands: an Offer answered at an agent at which another card, or
#             none, waits; a request answered while one answered before it
#             waits>
#
# A log line is "<time>\t<agent>\t<type>\t<term>", the term the question's
# with the answer in place: menu(Fs, W, pay(F, K)) or menu(Fs, W, swap(F, K))
# for a Menu, offer(From, K, Held, yes) or offer(From, K, Held, no) for an
# Offer; the monitor's last line is the clock alone.  The months are cut in the
# log's order, which is the order of time, so unordered is 0 on a run's log.
#
# When a card waits, and when the agent acts on a request (coins.glp, agent/7,
# settle/10).  An agent with no card waiting takes a proposal as it arrives
# and asks its person the card; while the card waits, settle/10 waits on the
# answer alone, and the agent takes neither a message nor its person's
# request (Section 6).  When the card is answered, settle/10 makes the card's
# move --- yes: add/4 puts K of the proposer's coins in the wallet; no: none
# --- and calls agent/7 again, which reduces by the first of its clauses that
# succeeds (GLP-Spec, glp.tex, Reduce: "the first clause for which the GLP
# reduction of A with C succeeds"): a request answered while the card waited
# is taken by the pay or the swap clause, which come before the proposal's
# clause and wait on the same wallet, ground(W?), so the agent acts on the
# request after the card's move and before it takes another proposal; a pay
# takes what the wallet then holds, take/5.  Then it takes the first proposal
# waiting in its input stream, if any, and that card waits.  The log shows
# when a card waits: the monitor releases one rated goal at a time, and only
# when the machine rests (monitor.glp, when_idle), so each line of the log is
# one Release, and all that its answer sets off is done before the next line.
# A swap(F, K) answered at an agent a with no card waiting sends F the proposal
# at once, and one answered while a card waits at a sends it when a acts on
# it; F, with no card waiting, takes it at once, and with one, takes it after
# the proposals that reached F before it, one proposal being sent at a line at
# most.  So the replay keeps, at each agent, the card waiting, by its From and
# K, the proposals waiting behind it, in the order they came, and the request
# answered while it waits; an Offer answered at a is of the card waiting at a,
# or it is counted unmatched and replayed at its answer, the cards waiting
# left as they stand, as is a request answered while another waits.  A request
# still waiting at the log's end moves nothing.
#
# The log holds the answers and not the messages, so the replay moves the
# proposer's holding of the other's coins at the yes.  In the run the proposer
# moves it when it takes the coins message, which it does not while a card of
# its own waits.

BEGIN {
    FS = "\t"; clock = "none"; MONTH = 2629800
    if (months == "") months = 12
    m = 1; last = ""
}

NF == 1 && $1 ~ /^[0-9.e+-]+$/ { clock = $1; next }

NF != 4 { unread++; next }

$3 == "Menu" {
    if ($4 !~ /^menu\(.*, (pay|swap)\([^(),]*, [^(),]*\)\)$/) { unread++; next }
    at($1 + 0)
    answers++; menu_answers++
    match($4, /(pay|swap)\([^(),]*, [^(),]*\)\)$/)
    req = substr($4, RSTART, RLENGTH - 1)
    i = index(req, "(")
    kind = substr(req, 1, i - 1)
    split(substr(req, i + 1, length(req) - i - 1), arg, ", ")
    if (kind == "pay") pays++; else proposals++
    a = $2 + 0
    if (!(a in card)) act(a, kind, arg[1] + 0, arg[2] + 0)
    else if (!(a in pend)) pend[a] = kind SUBSEP (arg[1] + 0) SUBSEP (arg[2] + 0)
    else { unmatched++; act(a, kind, arg[1] + 0, arg[2] + 0) }
    next
}

$3 == "Offer" {
    if ($4 !~ /^offer\([^(),]*, [^(),]*, [^(),]*, (yes|no)\)$/) { unread++; next }
    at($1 + 0)
    answers++; offer_answers++
    split(substr($4, 7, length($4) - 7), arg, ", ")
    a = $2 + 0
    ok = (a in card) && (card[a] == ((arg[1] + 0) SUBSEP (arg[2] + 0)))
    if (!ok) unmatched++
    if (arg[4] == "yes") {
        accepted++
        move(a, arg[1] + 0, arg[2] + 0)
        move(arg[1] + 0, a, arg[2] + 0)
    } else declined++
    if (ok) answered(a)
    next
}

{ unread++ }

# at(t): an answer at time t; the months that end at or before t are printed
# first, with the wallets the lines before it leave.
function at(t) {
    if (last != "" && t < last) unordered++
    last = t
    while (m <= months + 0 && t >= m * MONTH) { month(m); m++ }
}

# act(a, kind, f, k): agent a acts on its person's request kind(f, k): a pay
# moves a's wallet; a swap sends f the proposal of k.
function act(a, kind, f, k) {
    if (kind == "pay") pay(a, f, k)
    else propose(f, a, k)
}

# propose(f, a, k): a's proposal of k reaches f; f takes it at once, and its
# card waits, if none waits at f already; else it waits behind those before it.
function propose(f, a, k) {
    if (f in card) { qt[f]++; q[f, qt[f]] = a SUBSEP k }
    else card[f] = a SUBSEP k
}

# answered(a): the card waiting at a is answered, its move made; a acts on the
# request answered while it waited, if any, and then takes the first proposal
# waiting, if any, whose card waits.
function answered(a,    r) {
    delete card[a]
    if (a in pend) {
        split(pend[a], r, SUBSEP)
        delete pend[a]
        act(a, r[1], r[2] + 0, r[3] + 0)
    }
    if (qh[a] < qt[a]) { qh[a]++; card[a] = q[a, qh[a]]; delete q[a, qh[a]] }
}

# move(a, f, k): k added to a's holding of f's coins.
function move(a, f, k,    h, h1) {
    h = ((a, f) in hold) ? hold[a, f] : 0
    h1 = h + k
    set(a, f, h, h1)
}

# pay(a, f, k): a pays f; min(k, a's holding of f's coins) taken off it.
function pay(a, f, k,    h, t) {
    h = ((a, f) in hold) ? hold[a, f] : 0
    t = (k < h) ? k : h
    set(a, f, h, h - t)
}

# set(a, f, h, h1): a's holding of f's coins goes from h to h1, removed at zero.
function set(a, f, h, h1) {
    circulation += h1 - h
    if (h <= 0 && h1 > 0) { holdings++; if (npos[a]++ == 0) wallets++ }
    if (h > 0 && h1 <= 0) { holdings--; if (--npos[a] == 0) wallets-- }
    if (h1 == 0) delete hold[a, f]; else hold[a, f] = h1
}

function month(k) {
    printf "month %d at %d circulation %d holdings %d wallets %d pays %d proposals %d accepted %d declined %d\n",
        k, k * MONTH, circulation, holdings, wallets, pays, proposals, accepted, declined
}

END {
    while (m <= months + 0) { month(m); m++ }
    printf "answers %d\nmenu_answers %d\noffer_answers %d\n", answers, menu_answers, offer_answers
    printf "pays %d\nproposals %d\naccepted %d\ndeclined %d\n", pays, proposals, accepted, declined
    printf "circulation %d\nholdings %d\nwallets %d\n", circulation, holdings, wallets
    printf "clock %s\nunread %d\nunordered %d\nunmatched %d\n", clock, unread, unordered, unmatched
}
