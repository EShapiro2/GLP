/// GSG (social graph) UI manifest.
///
/// The one app-specific file for the GSG instance. Every GSG constructor name
/// lives here and nowhere in `lib/ui_runtime/`. Verified against
/// `programs/book/social_graph/typed_ui_mediator.glp`:
///
///   UserCmd    = decision(Decision, Constant, ReqId) | accept_intro(Constant, ReqId)
///              | reject_intro(Constant) | connect(Constant)
///              | send(Constant, Constant) | introduce(Constant, Constant)
///   UserNotify = befriend(Constant, ReqId) | befriend_intro(Constant, Constant, ReqId)
///              | connected(Constant) | rejected | rejected(Constant)
///              | received(Constant, Constant)
///
/// `send`/`received` are messaging (GrassApp, not GSG) and are omitted here.
///
/// DEFERRED (v1): unfriend. This mediator has no `unfriend`/`unfriended`. Adding
/// it later is a manifest line plus ported mediator clauses (a separately
/// approved change). The schema already leaves room: a `removeFrom('friends')`
/// activity rule on an eventual `unfriended(Who)` notify, and an `unfriend`
/// outbox/per-friend command. Do not add it without touching the .glp first.
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

final Manifest gsgManifest = Manifest(
  title: 'Social Graph',

  // Outbox: the free UserCmds (no ReqId).
  commands: const [
    CommandDesc(
      ctor: 'connect',
      label: 'Connect',
      args: [FieldDesc('target', FieldType.person, 'Person to connect')],
    ),
    CommandDesc(
      ctor: 'introduce',
      label: 'Introduce',
      args: [
        FieldDesc('p', FieldType.person, 'Person'),
        FieldDesc('q', FieldType.person, 'Introduce to'),
      ],
    ),
  ],

  // Inbox: the ReqId-bearing UserNotifies.
  inbox: [
    InboxDesc(
      notifyCtor: 'befriend',
      args: const ['from', 'req'],
      title: '{from} wants to connect',
      answers: const [
        AnswerDesc(
          label: 'Accept',
          cmdCtor: 'decision',
          fill: [ConstFill(GAtom('yes')), FromField('from'), FromField('req')],
        ),
        AnswerDesc(
          label: 'Decline',
          cmdCtor: 'decision',
          fill: [ConstFill(GAtom('no')), FromField('from'), FromField('req')],
        ),
      ],
    ),
    InboxDesc(
      notifyCtor: 'befriend_intro',
      args: const ['from', 'other', 'req'],
      title: '{from} introduces {other}',
      answers: const [
        AnswerDesc(
          label: 'Accept',
          cmdCtor: 'accept_intro',
          fill: [FromField('other'), FromField('req')],
        ),
        // reject_intro/1 carries no ReqId — the answer simply omits that field.
        AnswerDesc(
          label: 'Decline',
          cmdCtor: 'reject_intro',
          fill: [FromField('other')],
        ),
      ],
    ),
  ],

  // Activity: the all-ground UserNotifies.
  activity: const [
    ActivityDesc(
      notifyCtor: 'connected',
      args: ['who'],
      effect: AppendTo('friends', 'who'),
      feed: 'Connected to {who}',
    ),
    ActivityDesc(
      notifyCtor: 'rejected',
      args: ['who'],
      feed: '{who} declined your request',
    ),
    ActivityDesc(
      notifyCtor: 'rejected',
      args: [],
      feed: 'Your request was declined',
    ),
  ],

  state: const [
    StateView('friends', 'Friends', StateKind.list),
  ],
);
