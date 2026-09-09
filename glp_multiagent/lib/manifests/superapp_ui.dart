/// The Grassroots Super-App's own screen, the image of the display
/// declarations of `programs/social/graph/core/home.vglp`.
///
/// Nothing here is invented. Every element is derived from one declaration
/// under vGLP's Definitions "Display Declaration", "Manifest" and "Canonical
/// Compilation", exactly as the currency mini-app's manifest is
/// ([coinsManifest]). The declarations, carried through into the compiled
/// `home.glp`, are:
///
/// ```
/// display home *(Key) : panel(apps), label("Trust a compiler"),
///     field(Key, text), persistent.
/// display home *(Name, App) : panel(apps), label("Install a mini-app"),
///     field(Name, text), field(App, text), persistent.
/// display home *(App, Friend) : panel(apps), label("Invite a friend"),
///     field(App, peer), field(Friend, peer), persistent.
/// display home *(Friend) : panel(apps), label("Connect"),
///     field(Friend, peer), persistent.
/// display respond_friend *(yes, From?) : panel(apps), label("Accept"), transient.
/// display respond_friend *(no, From?) : panel(apps), label("Decline"), transient.
/// display msg(agent, person, connected(Friend)) : panel(apps), view(friends).
/// ```
///
/// The four persistent clauses are the person's acts on their own super-app —
/// trusting a compiler's key, installing a certified mini-app, inviting a
/// friend to it, asking a person to connect — and the friend's own request is
/// answered on the transient card of `respond_friend`, whose two clauses share
/// the context `ctx_respond_friend_j(From)` and are therefore one card with a
/// button each.
///
/// Two of the `text` fields are GLP `String`s and one is a `Constant`: the
/// compiled answer types are `xs_home_1(String)` and `xs_home_2(String,
/// Constant)`, and `home_2`'s first argument is the file name `load_file/2`
/// reads. The widget does not settle that, so the transcription reads `Xs_C`
/// as well as the `field` items — see [FieldType.string].
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

/// The four persistent clauses of `home`, each a compose form bound to the
/// standing card of its clause.
const _trust = CommandDesc(
  ctor: '',
  clause: 'home_1',
  answerCtor: 'xs_home_1',
  label: 'Trust a compiler',
  args: [FieldDesc('Key', FieldType.string, 'Key')],
);

const _install = CommandDesc(
  ctor: '',
  clause: 'home_2',
  answerCtor: 'xs_home_2',
  label: 'Install a mini-app',
  args: [
    FieldDesc('Name', FieldType.string, 'Name'),
    FieldDesc('App', FieldType.text, 'App'),
  ],
);

const _invite = CommandDesc(
  ctor: '',
  clause: 'home_3',
  answerCtor: 'xs_home_3',
  label: 'Invite a friend',
  args: [
    FieldDesc('App', FieldType.person, 'App'),
    FieldDesc('Friend', FieldType.person, 'Friend'),
  ],
);

const _connect = CommandDesc(
  ctor: '',
  clause: 'home_4',
  answerCtor: 'xs_home_4',
  label: 'Connect',
  args: [FieldDesc('Friend', FieldType.person, 'Friend')],
);

/// The two clauses of the friend responder, transient and sharing the context
/// `ctx_respond_friend_j(From)`: one card, a button each, neither carrying a
/// deadline — a request to connect stands until the person decides.
const _respondFriend = InboxDesc(
  clauses: ['respond_friend_1', 'respond_friend_2'],
  args: ['From'],
  itemKey: 'From',
  title: '',
  answers: [
    AnswerDesc(
      label: 'Accept',
      cmdCtor: '',
      clause: 'respond_friend_2',
      answerCtor: 'xs_respond_friend_2',
      fill: [ConstFill(GAtom('yes'))],
    ),
    AnswerDesc(
      label: 'Decline',
      cmdCtor: '',
      clause: 'respond_friend_1',
      answerCtor: 'xs_respond_friend_1',
      fill: [ConstFill(GAtom('no'))],
    ),
  ],
);

/// Panel `apps`: the four forms, the connect card, the friends view, and the
/// default display — a list in the program's panel — which takes every other
/// message the super-app shows its person.
const _appsPanel = Panel(
  id: 'apps',
  name: 'apps',
  commands: [_trust, _install, _invite, _connect],
  inbox: [_respondFriend],
  views: [
    ScreenView(
      pattern: 'msg(agent, person, connected(Friend))',
      content: 'Friend',
      kind: ViewKind.friends,
      label: 'Friends',
      store: 'friends',
    ),
    ScreenView(
      pattern: 'msg(agent, person, S)',
      content: 'S',
      kind: ViewKind.list,
      label: 'Screen',
      store: 'screen',
    ),
  ],
);

/// The Grassroots Super-App's manifest.
const Manifest superappManifest = Manifest(
  title: 'Apps',
  panels: [_appsPanel],
  activity: [],
);
