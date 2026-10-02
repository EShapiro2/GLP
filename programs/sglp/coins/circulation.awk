# The circulation of a run of coins among friends at the end of each month,
# read from the run's log alone (sGLP's paper, the repository
# svGLP-Stochastic-Volitional-GLP at c7d0b2f: Section 4, "The log", and
# Appendix B, the log's format; Section 6, Definition "Circulation": the total
# of the holdings in the wallets of the agents at t; sGLP's code task of
# 2026-10-02 15:24 UTC, item 4).
#
#   awk [-v months=<k>] -f circulation.awk <log>
#
# replays the wallets from the log, in its order: an Offer answered yes at
# agent a, showing From and K, adds K to a's holding of From's coins and K to
# From's holding of a's; a Menu answered pay(F, K) at agent a takes min(K, a's
# holding of F's coins) off that holding, a holding of none being 0, and
# removes a holding that reaches zero; nothing else moves a wallet.  It prints
# one line per month m of 1..k, k being 12 unless months=<k> says otherwise, a
# month being 2629800 s, 30.4375 days, the twelfth of a year of 365.25 days:
#
#   month <m> at <m * 2629800> circulation <c> holdings <h> wallets <w> pays <p> proposals <s> accepted <y> declined <n>
#
# the wallets at the month's end, simulated time m * 2629800, being those the
# log's lines whose time is less than it leave: c the total of the holdings, h
# the pairs of an agent and an issuer whose holding is positive, w the agents
# with a non-empty wallet; and p, s, y and n the menus answered pay and swap
# and the offers answered yes and no on those lines.  Months past the run's
# clock are printed as the rest, the wallets standing as the log leaves them.
# Then the run's totals, of the whole log, one per line:
#
#   answers <log lines that are answers>
#   menu_answers <answers to a Menu>      offer_answers <answers to an Offer>
#   pays <p>  proposals <s>  accepted <y>  declined <n>
#   circulation <c>  holdings <h>  wallets <w>, at the log's end
#   clock <the monitor's last line, the time the run ended; none if it did not>
#   unread <lines of a shape this reader does not know>
#   unordered <answers whose time is less than the time of the answer before>
#
# A log line is "<time>\t<agent>\t<type>\t<term>", the term the question's
# with the answer in place: menu(Fs, W, pay(F, K)) or menu(Fs, W, swap(F, K))
# for a Menu, offer(From, K, Held, yes) or offer(From, K, Held, no) for an
# Offer; the monitor's last line is the clock alone.  The months are cut in the
# log's order, which is the order of time, so unordered is 0 on a run's log.
#
# The log holds the answers and not the messages, so the replay moves a holding
# at the time of the answer that moves it.  In the run, the agent that answers
# moves its own holding when it acts on the answer, and the proposer its
# holding of the other's coins when it takes the coins message, which it does
# not while its own person has a card to answer (coins.glp, agent/7).

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
    if (kind == "pay") { pays++; pay($2 + 0, arg[1] + 0, arg[2] + 0) }
    else proposals++
    next
}

$3 == "Offer" {
    if ($4 !~ /^offer\([^(),]*, [^(),]*, [^(),]*, (yes|no)\)$/) { unread++; next }
    at($1 + 0)
    answers++; offer_answers++
    split(substr($4, 7, length($4) - 7), arg, ", ")
    if (arg[4] == "yes") {
        accepted++
        move($2 + 0, arg[1] + 0, arg[2] + 0)
        move(arg[1] + 0, $2 + 0, arg[2] + 0)
    } else declined++
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
    printf "clock %s\nunread %d\nunordered %d\n", clock, unread, unordered
}
