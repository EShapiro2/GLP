"""A contract's mini-app of the Grassroots Super-App, made mechanically from its
canonical platform, as the apps of programs/apps are made by hand:

  platform.vglp   the canonical CVA platform vGLP's compiler prints
  miniapp.glp     the entry miniapp/4, the same file in every app (programs/apps/gsg's;
                  GB's where the platform requires the day and reads the calendar)
  self.glp        the platform's types written again, the person's and the router's,
                  and the exports, as programs/apps/gsg/self.glp has them
  person.glp      a scripted person, programs/apps/gsg's engine with a short script:
                  establish an identity and acquaint the other person, where the
                  contract has those acts

and, for a run on two smartphones, a phone module in programs/social/graph/core and
a boot file, as programs/apps/gsg_boot.glp and core/gsg_phone.glp are written, with
the three lines core/self.glp needs to start the phone.  Nothing in these files is
GAS2's own GLP beyond the names and the script: each is GLP's text with them put in."""
import os
import re

# The platform types programs/apps/gsg/self.glp writes again, in its order.
PLATFORM_TYPES = ['Peer', 'Class', 'Proposal', 'Request', 'YesNo', 'Reason', 'Vote', 'Decision', 'Rho',
                  'Names', 'Attempt', 'Cargo', 'Content', 'Net', 'Atom', 'Output', 'Tab', 'Asking',
                  'Approved', 'Approval', 'Word']
OWN_TYPES = {'Question', 'Drawn', 'Ask', 'Asked', 'Seen', 'NetEvent', 'Route', 'Routes', 'Sending'}
BUILTIN = {'Key', 'String', 'Integer', 'Constant', 'List', 'Stream', 'Channel', 'Opaque', 'Number',
           'RootChannel', 'PersonChannel', 'PersonIn', 'PersonOut', 'SgChannel', 'Input', 'Draw', 'Path',
           'Output_'}


def type_defs(vglp):
    """The type definitions of a vGLP file, name -> its text, continuation lines included."""
    defs, cur, name = {}, [], None
    for line in vglp.split('\n'):
        m = re.match(r'^([A-Z][A-Za-z0-9]*)(\([A-Z, ]+\))?\s*::=', line)
        if m and '=::=' not in line:
            if name:
                defs[name] = '\n'.join(cur)
            name, cur = m.group(1), [line]
            if line.rstrip().endswith('.'):
                defs[name] = '\n'.join(cur)
                name, cur = None, []
            continue
        if name:
            if line.strip() == '' or line.startswith('%'):
                defs[name] = '\n'.join(cur)
                name, cur = None, []
            else:
                cur.append(line)
                if line.rstrip().endswith('.'):
                    defs[name] = '\n'.join(cur)
                    name, cur = None, []
    if name:
        defs[name] = '\n'.join(cur)
    return defs


def platform_types(vglp):
    """The platform types gsg's self.glp restates, with every platform type they name, in order."""
    defs = type_defs(vglp)
    want, seen = [], set()

    def add(n):
        if n in seen or n in OWN_TYPES or n not in defs:
            return
        seen.add(n)
        body = defs[n].split('::=', 1)[1]
        for r in re.findall(r'\b([A-Z][A-Za-z0-9]*)\b', re.sub(r'"[^"]*"', '', body)):
            if r not in BUILTIN:
                add(r)
        want.append(n)

    for n in PLATFORM_TYPES:
        add(n)
    # gsg's order first, then the further platform types they name
    order = [n for n in PLATFORM_TYPES if n in seen] + [n for n in want if n not in PLATFORM_TYPES]
    return [defs[n] for n in order]


def proposals(vglp):
    """The contract's acts, in its schemas' order, as the platform's Proposal type lists them
    (the creator's certification names them in that order)."""
    d = type_defs(vglp).get('Proposal', '')
    return list(dict.fromkeys(re.findall(r'\b([a-z_][a-z0-9_]*)\(', d.split('::=', 1)[-1])))


SELF_TAIL = '''Question ::= request_r(Request?) ; approval_w(Approval).
Drawn    ::= request_r(_) ; approval_w(_) ; output(List(Output)) ; Request ; YesNo.
Ask(Q)   ::= ask(Constant, Q).

%% What the mini-app's scripted person reports, its screen (person.glp).
Asked    ::= Class ; Attempt.
Seen     ::= Output ; menu(Key, List(Key)) ; gave(Asked, YesNo).

%% The network of the platform over the conversations (miniapp.glp).
NetEvent ::= opened(Key, RootChannel(Net)) ; sent(Net).
Route    ::= route(Key, Stream(Net)?).
Routes   ::= [] ; [Route | Routes].
Sending  ::= went ; held(Net).

%% The mini-app's entry, the instance of the root's entry template miniapp/4 (programs/self.glp).
imported procedure miniapp#miniapp(Key?, PersonChannel(Drawn)?, SgChannel(Net)?, Stream(Integer)?).
exported procedure miniapp(Key?, PersonChannel(Drawn)?, SgChannel(Net)?, Stream(Integer)?).
miniapp(Id, Ui, Sg, Cal) :- miniapp # miniapp(Id?, Ui?, Sg?, Cal?).

%% The scripted person, the instance of the root's second template miniapp_person/5.
imported procedure person#miniapp_person(Key?, Constant?, List(Key)?, Channel(PersonOut(Drawn), PersonIn(Drawn))?, Stream(Seen)).
exported procedure miniapp_person(Key?, Constant?, List(Key)?, Channel(PersonOut(Drawn), PersonIn(Drawn))?, Stream(Seen)).
miniapp_person(Id, Role, Ps, PEnd, Screen?) :- person # miniapp_person(Id?, Role?, Ps?, PEnd?, Screen).
'''


def self_glp(cid, title, vglp):
    head = (f'%% ---------------------------------------------------------------------------\n'
            f'%% {cid}, {title}, as a mini-app of the Grassroots Super-App, made by GAS2 from the\n'
            f'%% contract\'s canonical platform, platform.vglp, as the apps of programs/apps are made:\n'
            f'%% the platform\'s types written again, then the person\'s and the router\'s, and the\n'
            f'%% exports.  It calls nothing that reaches the network or the person, so the\n'
            f'%% REPL\'s :artefact certifies it.\n'
            f'%% ---------------------------------------------------------------------------\n\n')
    return head + '\n'.join(platform_types(vglp)) + '\n\n' + SELF_TAIL


def script_block(acts):
    """The script of the two persons of GAS2's run: each establishes an identity, and
    alice acquaints bob, where the contract has those acts."""
    if 'establish' in acts and 'acquaint' in acts:
        return '''procedure script(Constant?, Key?, List(Key)?, Steps).
script(alice, _, [Bob], [look, own([A1], propose(establish(A1?))), wait(holds(identity)),
                         own([M1], propose(acquaint(M1?, Bob?))), wait(holds(knows(Bob?))), look]) :-
    ground(Bob?) | true.
script(bob, Me, [Alice], [own([B1], propose(establish(B1?))), wait(holds(identity)),
                          approval(acquaint(Alice?, Me?), yes), wait(holds(knows(Alice?)))]) :-
    ground(Me?), ground(Alice?) | true.
script(_, _, _, []) :- otherwise | true.'''
    if 'establish' in acts:
        return '''procedure script(Constant?, Key?, List(Key)?, Steps).
script(_, _, _, [look, own([A1], propose(establish(A1?))), wait(holds(identity)), look]) :- true.'''
    return '''procedure script(Constant?, Key?, List(Key)?, Steps).
script(_, _, _, [look]) :- true.'''


def person_glp(engine_src, acts):
    """programs/apps/gsg/person.glp with GAS2's script in place of its own."""
    s = re.sub(r'^procedure script\(.*?^script\(_, _, _, \[\]\) :- otherwise \| true\.\n',
               script_block(acts) + '\n', engine_src, flags=re.S | re.M)
    if s == engine_src:
        raise ValueError('the person engine has no script block to replace')
    return s


def write_app(glp, app, cid, title, vglp):
    """Write the mini-app's directory programs/apps/<app>; its path."""
    src = glp.path('programs', 'apps', 'gsg')
    d = glp.path('programs', 'apps', app)
    os.makedirs(d, exist_ok=True)
    rd = lambda *p: open(os.path.join(*p), encoding='utf-8').read()
    w = lambda n, t: open(os.path.join(d, n), 'w', encoding='utf-8', newline='\n').write(t)
    w('platform.vglp', vglp)
    # a platform that requires the day reads the calendar the super-app grants: GB's file
    dated = re.search(r'^exported procedure agent\(Peer\?, Stream\(Net\)\?, Stream\(Net\), Stream\(Output\), Stream\(Integer\)\?\)', vglp, re.M)
    w('miniapp.glp', rd(glp.path('programs', 'apps', 'gb') if dated else src, 'miniapp.glp'))
    w('self.glp', self_glp(cid, title, vglp))
    w('person.glp', person_glp(rd(src, 'person.glp'), proposals(vglp)))
    return d


# --- two smartphones ---------------------------------------------------------------

PHONE = '''%% {app}_phone.glp --- one smartphone of GAS2's run of {cid}: the super-app's agent under the
%% person's own key, the mini-app {app}.glpw read from its certified artefact and installed,
%% and the person's acts on their super-app, written as programs/social/graph/core/gsg_phone.glp
%% writes GSG's, with {cid}'s name and acts in place of GSG's.  Made by GAS2.

imported procedure agent#agent(Key?, UserInStream?, NetInStream?, OutputsList?).

exported procedure {app}_phone(Constant?, Key?, List(Key)?, NetInStream?).
{app}_phone(alice, Me, [Q], NetIn) :-
    agent # agent(Me?, In?, NetIn?, [user_output(Out), net_output(NetOut)]),
    send_to_net(NetOut?),
    load_file("{app}.glpw", C),
    certified(C?, CK, CSig),
    new_channel(AppEnd, PersonEnd),
    alice_run0(Me?, Q?, C?, CK?, CSig?, AppEnd?, Out?, In),
    miniapp_person_run(C?, Me?, alice, [Q?], PersonEnd?, Screen),
    send_to_user(Screen?).
{app}_phone(bob, Me, [P], NetIn) :-
    agent # agent(Me?, In?, NetIn?, [user_output(Out), net_output(NetOut)]),
    send_to_net(NetOut?),
    load_file("{app}.glpw", C),
    certified(C?, CK, CSig),
    new_channel(AppEnd, PersonEnd),
    bob_run0(Me?, P?, C?, CK?, CSig?, AppEnd?, Out?, In),
    miniapp_person_run(C?, Me?, bob, [P?], PersonEnd?, Screen),
    send_to_user(Screen?).
{app}_phone(_, _, _, _) :- otherwise | true.

%% alice: trust the compiler's key, install the mini-app with her end of its person channel,
%% connect to bob, and invite him once he is her friend.
procedure alice_run0(Key?, Key?, Content?, String?, String?, PersonChannel(Opaque)?, AgentToUserStream?, UserInStream).
alice_run0(Me, Q, C, CK, CSig, AppEnd, Outs,
          [msg(person, Me?, trust(CK?)),
           msg(person, Me?, install({app}, C?, CSig?, AppEnd?)),
           msg(person, Me?, connect(Q?)) | Cmds?]) :-
    ground(Me?), ground(Q?) |
    alice_run(Me?, Q?, Outs?, Cmds).

procedure alice_run(Key?, Key?, AgentToUserStream?, UserInStream).
alice_run(Me, Q, [msg(agent, person, connected(Q1)) | Outs],
              [msg(person, Me?, invite({app}, Q?)) | Cmds?]) :-
    Q1? =?= Q? |
    send_to_user([sg(connected(Q1?))]),
    alice_run(Me?, Q?, Outs?, Cmds).
alice_run(Me, Q, [_ | Outs], Cmds?) :- otherwise | alice_run(Me?, Q?, Outs?, Cmds).
alice_run(_, _, [], []).

%% bob: trust, install, consent in advance to alice's invitation, and say yes to her
%% request to connect.
procedure bob_run0(Key?, Key?, Content?, String?, String?, PersonChannel(Opaque)?, AgentToUserStream?, UserInStream).
bob_run0(Me, P, C, CK, CSig, AppEnd, Outs,
        [msg(person, Me?, trust(CK?)),
         msg(person, Me?, install({app}, C?, CSig?, AppEnd?)),
         msg(person, Me?, accept_invite({app}, P?)) | Cmds?]) :-
    ground(Me?), ground(P?) |
    bob_run(Me?, P?, Outs?, Cmds).

procedure bob_run(Key?, Key?, AgentToUserStream?, UserInStream).
bob_run(Me, P, [msg(agent, person, befriend(P1, X)) | Outs],
            [msg(person, Me?, decision(yes, P?, X?)) | Cmds?]) :-
    P1? =?= P? |
    send_to_user([sg(befriend(P1?))]),
    bob_run(Me?, P?, Outs?, Cmds).
bob_run(Me, P, [msg(agent, person, connected(P1)) | Outs], Cmds?) :-
    P1? =?= P? |
    send_to_user([sg(connected(P1?))]),
    bob_run(Me?, P?, Outs?, Cmds).
bob_run(Me, P, [_ | Outs], Cmds?) :- otherwise | bob_run(Me?, P?, Outs?, Cmds).
bob_run(_, _, [], []).

%% The creator's certification, signed here over the mini-app's source identity, the
%% contract's act schemas and the checker's verdict, as gsg_phone.glp signs GSG's.
procedure certified(Content?, String, String).
certified(M, CK?, CSig?) :-
    module(M?) |
    decompose_module(M?, CK, Src, _),
    self_key(K),
    sign(creator_cert(Src?, [{acts}], {verdict}), K?, CSig).

procedure miniapp_person_run(Content?, Key?, Constant?, List(Key)?, Channel(PersonOut(Opaque), PersonIn(Opaque))?, Stream(Opaque)).
miniapp_person_run(M, Id, Role, Ps, PEnd, Screen?) :-
    module(M?) |
    opaque_type(miniapp/4, M?, H0),
    opaque_type(miniapp_person/5, M?, H),
    person_at(H0?, H?, M?, Id?, Role?, Ps?, PEnd?, Screen).

procedure person_at(Stream(Constant)?, Stream(Constant)?, Module?, Key?, Constant?, List(Key)?, Channel(PersonOut(Opaque), PersonIn(Opaque))?, Stream(Opaque)).
person_at([D0, _], [D, S], M, Id, Role, Ps, PEnd, Screen?) :-
    D0? =?= D? |
    find_type(miniapp_person/5, [D?, S?], T),
    run(miniapp_person(Id?, Role?, Ps?, PEnd?, Screen), T?, M?).
person_at(_, _, _, _, _, _, PEnd, []) :- otherwise | hold_person(PEnd?).
'''

BOOT = '''%% {app}_boot.glp --- GAS2's run of {cid} on two smartphones, as programs/apps/gsg_boot.glp
%% runs GSG's: two isolates, each the super-app of programs/social/graph/core with its own
%% key pair.  Made by GAS2.

procedure boot.
boot :-
    agent(ch(_?, _), ch(_?, _))@alice,
    say(alice, alice, [bob], ch(_?, _))@alice,
    agent(ch(_?, _), ch(_?, _))@bob,
    say(bob, bob, [alice], ch(_?, _))@bob.

Role     ::= alice ; bob.
SayAct ::= {app}_phone(Role, Key, List(Key)).
SayIn  ::= [SayAct | SayEnd].
SayEnd ::= [].

exported procedure say(Role?, Key?, List(Key)?, Channel(Stream(Constant), SayIn)?).
say(R, Me, Ps, ch(_, [{app}_phone(R?, Me?, Ps?)])).
'''


def write_phone(glp, app, cid, acts, verdict):
    core = glp.path('programs', 'social', 'graph', 'core')
    with open(os.path.join(core, f'{app}_phone.glp'), 'w', encoding='utf-8', newline='\n') as f:
        # the super-app installs a mini-app whose creator certifies it grassroots (core/agent.glp, cert_check/4)
        f.write(PHONE.format(app=app, cid=cid, acts=', '.join(acts),
                             verdict='grassroots' if verdict == 'syntactically_grassroots' else verdict))
    boot = glp.path('programs', 'apps', f'{app}_boot.glp')
    with open(boot, 'w', encoding='utf-8', newline='\n') as f:
        f.write(BOOT.format(app=app, cid=cid))
    return boot


def patch_core(glp, apps):
    """The three lines core/self.glp needs for each of GAS2's phones: the person's first
    act in CoreIn, the phone's import, and core_start's clause; written after core's own
    gsg_phone lines.  Idempotent."""
    p = glp.path('programs', 'social', 'graph', 'core', 'self.glp')
    s = open(p, encoding='utf-8').read()
    for app in apps:
        if f'{app}_phone#' in s:
            continue
        s, n1 = re.subn(r'(\n(\s*); gsg_phone\(Constant, Key, List\(Key\)\))',
                        rf'\1\n\2; {app}_phone(Constant, Key, List(Key))', s, count=1)
        s, n2 = re.subn(r'(\nimported procedure gsg_phone#gsg_phone\(Constant\?, Key\?, List\(Key\)\?, NetInStream\?\)\.)',
                        rf'\1\nimported procedure {app}_phone#{app}_phone(Constant?, Key?, List(Key)?, NetInStream?).', s, count=1)
        s, n3 = re.subn(r'(\ncore_start\(\[gsg_phone\(R, P, Qs\) \| _\], N\) :- gsg_phone # gsg_phone\(R\?, P\?, Qs\?, N\?\)\.)',
                        rf'\1\ncore_start([{app}_phone(R, P, Qs) | _], N) :- {app}_phone # {app}_phone(R?, P?, Qs?, N?).', s, count=1)
        if not (n1 and n2 and n3):
            raise ValueError(f'core/self.glp has not the gsg_phone lines GAS2 writes beside ({n1}{n2}{n3})')
    with open(p, 'w', encoding='utf-8', newline='\n') as f:
        f.write(s)
