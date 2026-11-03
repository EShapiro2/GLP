"""Write a live harness for a contract's canonical platform: each person alone, an
agent of madGLP (live_person/3), or the four persons in one heap (live_ui/1).

Usage: python3 -I gen_harness.py <platform.vglp> <outdir> <NAME>

The harness copies the platform's types it needs, runs alice, bob, carol and
dave, each an agent and a dispatcher, joins their networks, and lets the host
(the live server) be every person: in(Person, Grant) goes to that person's
channel.  A platform whose agent reads a calendar (Stream(Integer)?) gets one
per person, fed by the host's cal(Person, N).  The host's first act picks the
agent: me(Person), fed then g(Grant) and cal(N), its platform's messages sent
by send_to_net; or world, the four persons together.
"""
import re, sys, os, shutil

plat, out, name = sys.argv[1:4]
src = open(plat, encoding='utf-8').read()

decls = {}
for m in re.finditer(r'^([A-Z][A-Za-z]*(?:\([A-Z]\))?)\s*::=(.*?)\.\s*$', src, re.M | re.S):
    decls.setdefault(m.group(1), m.group(0).strip())

sig = re.search(r'^exported procedure agent\((.*)\)\.\s*$', src, re.M).group(1)
seeds = ['Net', 'Output', 'Menu', 'Approval', 'Proposal', 'Class', 'Atom', 'Request', 'YesNo']
need, todo = [], list(seeds)
while todo:
    t = todo.pop()
    key = next((k for k in decls if k == t or k.startswith(t + '(')), None)
    if not key or key in need or t == 'Peer':
        continue
    need.append(key)
    todo += [r for r in re.findall(r'\b([A-Z][A-Za-z]*)\b', decls[key].split('::=', 1)[1]) if r != t]
types = '\n'.join(decls[k] for k in decls if k in need)

cal = 'Stream(Integer)?' in sig
P, L = ['alice', 'bob', 'carol', 'dave'], 'ABCD'

ks = ', KA, KB, KC, KD' if cal else ''
body = f"""
Question ::= menu_w(Menu) ; approval_w(Approval).
Drawn    ::= menu_w(_) ; approval_w(_) ; output(List(Output)) ; Request ; YesNo.
Ask(Q)   ::= ask(Constant, Q).
HostIn   ::= [_ | HostIn].

imported procedure platform#agent({sig}, Stream(Ask(Question))).
imported procedure platform#dispatch(Stream(Ask(Question))?,
    PersonChannel(Drawn)?, Channel(Stream(Input(Drawn)), Stream(Output))).

%% The agent's initial goal, every agent's, agent(ch(_?,_),ch(_?,_)) (IGLP,
%% Implementation Notes, "Boot"): the host's acts are the first channel's
%% input, and the harness writes to the person itself, by send_to_user/1.
%% The host's first act says which agent this is: me(Person), one person
%% alone, whose network is the agent's (live_person/3); or world, the four
%% persons, their networks joined inside it (live_ui/1).
exported procedure agent(Channel(HostIn, Stream(Constant))?, Channel(Stream(Net), Stream(Net))?).
agent(ch(Ins, []), ch(N, [])) :- start(Ins?, N?).

procedure start(HostIn?, Stream(Net)?).
start([me(P) | Ins], N) :- ground(P?) | live_person(P?, Ins?, N?).
start([world | Ins], _) :- live_ui(Ins?).

procedure live_ui(HostIn?).
live_ui(Ins) :-
    split4(Ins?, GA, GB, GC, GD{ks}),
"""
for p, x in zip(P, L):
    k = f'K{x}?, ' if cal else ''
    body += f"""    platform # agent({p}, In{x}?, Out{x}, T{x}, {k}As{x}),
    platform # dispatch(As{x}?, ch(G{x}?, D{x}), M{x}),
    to_person(M{x}?, T{x}?),
    tag({p}, D{x}?, S{x}),
"""
body += """    merge(OutA?, OutB?, Net1),
    merge(OutC?, OutD?, Net2),
    merge(Net1?, Net2?, Net),
    route4(Net?, alice, InA, bob, InB, carol, InC, dave, InD, N),
    tag_net(N?, SN),
    merge(SA?, SB?, S1),
    merge(SC?, SD?, S2),
    merge(S1?, S2?, S3),
    merge(S3?, SN?, S),
    send_to_user(S?).

"""
if not cal:
    body += """procedure split4(HostIn?, PersonIn(Drawn), PersonIn(Drawn), PersonIn(Drawn), PersonIn(Drawn)).
split4([in(alice, G)|Ins], [G?|A?], B?, C?, D?) :- split4(Ins?, A, B, C, D).
split4([in(bob, G)|Ins], A?, [G?|B?], C?, D?) :- split4(Ins?, A, B, C, D).
split4([in(carol, G)|Ins], A?, B?, [G?|C?], D?) :- split4(Ins?, A, B, C, D).
split4([in(dave, G)|Ins], A?, B?, C?, [G?|D?]) :- split4(Ins?, A, B, C, D).
split4([_|Ins], A?, B?, C?, D?) :- otherwise | split4(Ins?, A, B, C, D).
"""
else:
    R = 'KA, KB, KC, KD'
    Rq = 'KA?, KB?, KC?, KD?'
    body += f"""procedure split4(HostIn?, PersonIn(Drawn), PersonIn(Drawn), PersonIn(Drawn), PersonIn(Drawn),
    Stream(Integer), Stream(Integer), Stream(Integer), Stream(Integer)).
split4([in(alice, G)|Ins], [G?|A?], B?, C?, D?, {Rq}) :- split4(Ins?, A, B, C, D, {R}).
split4([in(bob, G)|Ins], A?, [G?|B?], C?, D?, {Rq}) :- split4(Ins?, A, B, C, D, {R}).
split4([in(carol, G)|Ins], A?, B?, [G?|C?], D?, {Rq}) :- split4(Ins?, A, B, C, D, {R}).
split4([in(dave, G)|Ins], A?, B?, C?, [G?|D?], {Rq}) :- split4(Ins?, A, B, C, D, {R}).
split4([cal(alice, N)|Ins], A?, B?, C?, D?, [N?|KA?], KB?, KC?, KD?) :- split4(Ins?, A, B, C, D, {R}).
split4([cal(bob, N)|Ins], A?, B?, C?, D?, KA?, [N?|KB?], KC?, KD?) :- split4(Ins?, A, B, C, D, {R}).
split4([cal(carol, N)|Ins], A?, B?, C?, D?, KA?, KB?, [N?|KC?], KD?) :- split4(Ins?, A, B, C, D, {R}).
split4([cal(dave, N)|Ins], A?, B?, C?, D?, KA?, KB?, KC?, [N?|KD?]) :- split4(Ins?, A, B, C, D, {R}).
split4([_|Ins], A?, B?, C?, D?, {Rq}) :- otherwise | split4(Ins?, A, B, C, D, {R}).
"""
body += """
procedure tag(Peer?, Stream(_)?, Stream(_)).
tag(N, [X|Xs], [d(N?, X?)|Ts?]) :- ground(N?) | tag(N?, Xs?, Ts).
tag(_, [], []).

procedure tag_net(Stream(_)?, Stream(_)).
tag_net([X|Xs], [net(X?)|Ts?]) :- tag_net(Xs?, Ts).
tag_net([], []).
"""
# One person alone: live_person(Person, HostIn, NetIn) runs the person's
# platform agent and dispatcher, its network input the agent's own; the
# platform's messages go out by send_to_net, a cold call each to the person
# it names, and arrive on that person's network input.  The host feeds the
# person g(Grant) and, with a calendar, cal(N).
body += """
procedure live_person(Peer?, HostIn?, Stream(Net)?).
"""
for p in P:
    k = 'K?, ' if cal else ''
    split = 'split_person(Ins?, G, K)' if cal else 'split_person(Ins?, G)'
    body += f"""live_person({p}, Ins, NetIn) :-
    {split},
    platform # agent({p}, NetIn?, Out, T, {k}As),
    platform # dispatch(As?, ch(G?, D), M),
    to_person(M?, T?),
    tee_net(Out?, Net, L),
    send_to_net(Net?),
    tag({p}, D?, S),
    tag_net(L?, SN),
    merge(S?, SN?, S1),
    send_to_user(S1?).
"""
if cal:
    body += """
procedure split_person(HostIn?, PersonIn(Drawn), Stream(Integer)).
split_person([g(G)|Ins], [G?|Gs?], Ks?) :- split_person(Ins?, Gs, Ks).
split_person([cal(N)|Ins], Gs?, [N?|Ks?]) :- split_person(Ins?, Gs, Ks).
split_person([_|Ins], Gs?, Ks?) :- otherwise | split_person(Ins?, Gs, Ks).
"""
else:
    body += """
procedure split_person(HostIn?, PersonIn(Drawn)).
split_person([g(G)|Ins], [G?|Gs?]) :- split_person(Ins?, Gs).
split_person([_|Ins], Gs?) :- otherwise | split_person(Ins?, Gs).
"""
body += """
%% The agent's network output, passed on whole, each message noted as route4 notes it.
procedure tee_net(Stream(Net)?, Stream(Net), Stream(Sent(Cargo))).
tee_net([msg(To, M)|Ms], [msg(To?, M?)|Os?], Log?) :-
    ground(To?), ground(M?) |
    note(M?, To?, Log, Log1?),
    tee_net(Ms?, Os, Log1).
tee_net([], [], []).
"""
hdr = f"""%% The live harness of the canonical CVA platform of {name} (platform.vglp,
%% as cascade.sh prints it): four persons, alice, bob, carol and dave, in one
%% heap, their networks joined; the host (the live server) is every person{', and feeds each agent the day by cal(Person, N)' if cal else ''}.
%% Generated by gen_harness.py; the types are the platform's, copied.

"""
os.makedirs(out, exist_ok=True)
shutil.copy(plat, os.path.join(out, 'platform.vglp'))
open(os.path.join(out, 'self.glp'), 'w', encoding='utf-8', newline='\n').write(hdr + types + '\n' + body)
print(name, 'calendar' if cal else '', len(need), 'types')
