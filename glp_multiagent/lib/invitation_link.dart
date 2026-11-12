/// An invitation link the person tapped, handed to the app by the phone's
/// operating system, sent on the person channel as the person's act that
/// opens it.
///
/// IGLP, Implementation Notes, "Boot" (b7bfcfe): "An address the smartphone
/// hands the application, an invitation link the person tapped, is the
/// person's act: the application sends on the person channel the act that
/// opens the link, as if the person had entered its text."  The address is
/// GNA's, Section 6, "Invitation links": "a web address,
/// https://peoplesweb.org/i# followed by the link's text.  Tapped on a
/// smartphone, the address opens the Grassroots App where it is installed,
/// the app having registered the address with the smartphone's operating
/// system".  The app registers it by the associated domain
/// `applinks:peoplesweb.org` on iOS (ios/Runner/Runner.entitlements) and by
/// an intent filter for https://peoplesweb.org/i on Android
/// (android/app/src/main/AndroidManifest.xml).
///
/// The phone hands the address to Flutter's deep linking, which both
/// platforms have on (`FlutterDeepLinkingEnabled` in ios/Runner/Info.plist,
/// `flutter_deeplinking_enabled` in the manifest): an address that starts the
/// app is the platform's initial route on Android, and every other is pushed
/// as route information, on iOS the one that starts the app too.  [listen]
/// reads both, and takes every pushed route, the app having no routes of its
/// own: an address of GNA's form is held until the person's request form is
/// on the screen, and anything else is ignored.
///
/// The act is the grant the request form of home makes when the person
/// chooses Open link, enters the link's text and submits it: `input(Id, P,
/// open_link(Link))`, `Id` the construct of the request stream, `P` the
/// position of its question, and `Link` the text, a `String`
/// (construct_family.dart, `_submitting` and `_value`;
/// programs/social/graph/core/home.glp, the request `open_link(Link)` and
/// its form `form(open_link, [field('Link', text)])`).
library;

import 'package:flutter/widgets.dart';

import 'ui_runtime/bridge.dart';
import 'ui_runtime/term.dart';

/// The host of an invitation link's address (GNA, Section 6).
const String invitationHost = 'peoplesweb.org';

/// The path of an invitation link's address (GNA, Section 6).
const String invitationPath = '/i';

/// The request of home that opens a link, `open_link(Link)`
/// (programs/social/graph/core/home.glp).
const String openLinkCtor = 'open_link';

/// The link's text in [address], where it is of GNA's form,
/// `https://peoplesweb.org/i#<link>`: the text after the `#`, percent-decoded.
/// Null for any other address: another scheme, host or path, a port, a user,
/// a query, no text after the `#`, or text that does not percent-decode.
String? invitationLinkText(Uri address) {
  if (address.scheme != 'https' ||
      address.host != invitationHost ||
      address.hasPort ||
      address.userInfo.isNotEmpty ||
      address.path != invitationPath ||
      address.hasQuery ||
      !address.hasFragment ||
      address.fragment.isEmpty) {
    return null;
  }
  try {
    final link = Uri.decodeComponent(address.fragment);
    return link.isEmpty ? null : link;
  } on ArgumentError {
    return null;
  } on FormatException {
    return null;
  }
}

/// The grant that opens [link] on the open construct of [bridge] whose
/// question is the person's stream of requests with the form Open link:
/// `(Id, P, open_link(Link))`, as the form grants it when the person enters
/// [link] and submits it.  Null where no such construct is open.
(int, GList, GTerm)? openLinkGrant(Bridge bridge, String link) {
  for (final c in bridge.constructs) {
    final path = _questionOf(c.view);
    if (path == null) continue;
    final form = _openLinkForm(c.widget);
    if (form == null) continue;
    return (c.id, path, GStruct(form, [GString(link)]));
  }
  return null;
}

/// The position of the question of a stream the person writes, in its view:
/// `input(P)` where no element is answered or none holds a position the
/// program writes, and otherwise the tail after the elements drawn
/// (construct_family.dart, `_node` and `_shown`).
GList? _questionOf(GTerm view) {
  var v = view;
  while (v is GStruct && v.functor == '.' && v.args.length == 2) {
    v = v.args[1];
  }
  return questionAt(v);
}

/// The functor of the form Open link, where [widget] is the widget of a
/// stream the person writes, `input_box(E)`, and `E` that form or a menu
/// with it among its alternatives; null otherwise.  The form is
/// `form(open_link, [field(L, text)])`, its one field the link's text.
String? _openLinkForm(GTerm widget) {
  final box = _plain(widget);
  if (box == null) return null;
  final (k, ws) = ctorArgs(box);
  if (k != 'input_box' || ws.length != 1) return null;
  final element = _plain(ws[0]);
  if (element == null) return null;
  final (ek, es) = ctorArgs(element);
  final alternatives = ek == 'menu' && es.length == 1 && es[0] is GList
      ? (es[0] as GList).items
      : [element];
  for (final a in alternatives) {
    final form = _plain(a);
    if (form == null) continue;
    final (fk, fs) = ctorArgs(form);
    if (fk != 'form' || fs.length != 2 || fs[1] is! GList) continue;
    if (ctorArgs(fs[0]).$1 != openLinkCtor) continue;
    final fields = (fs[1] as GList).items;
    if (fields.length != 1) continue;
    final (dk, ds) = ctorArgs(fields.single);
    if (dk != 'field' || ds.length != 2) continue;
    final text = _plain(ds[1]);
    if (text == null || formatTerm(text) != 'text') continue;
    return ctorArgs(fs[0]).$1;
  }
  return null;
}

/// [w] with its label, its declared widget's name and the binding of a
/// recursive type taken off its top, as the construct family draws it: a
/// label is drawn in place of the name, a declared widget's default widget
/// is the editor of a form, and `rec(T, W)` at the root of a question is
/// drawn at once as `W` (construct_family.dart, `_editor` and `_value`;
/// bridge.dart, [unlabelled], [namedWidget], [unfoldWidget]).
GTerm? _plain(GTerm w) {
  var cur = w;
  for (var step = 0; step < 16; step++) {
    final u = unfoldWidget(cur, const {});
    if (u == null) return null;
    cur = unlabelled(u.$1);
    final named = namedWidget(cur);
    if (named == null) return cur;
    cur = named.$2;
  }
  return null;
}

/// The invitation links the phone has handed the app and the app has not
/// yet sent, in the order the phone handed them, and the bridge they are
/// sent on once it is given.
///
/// A person answers a question of the request stream once: once a link is
/// granted at a position, that position is answered, and the next question
/// is at the stream's tail, which the program draws.  So a further link
/// waits until the screen offers a question at another position (GLP #15,
/// on IGLP b7bfcfe, Boot: "as if the person had entered its text").
class InvitationLinks with WidgetsBindingObserver {
  final List<String> _waiting = [];
  Bridge? _bridge;

  /// The construct and the position of the question the last link was
  /// granted at, as `Id:P`; null before any.
  String? _granted;

  /// The links waiting for the request form, in order.
  List<String> get waiting => List.unmodifiable(_waiting);

  /// The links of the address that started the app, where it is one, and of
  /// every address the phone hands the app from now on.  Called before
  /// runApp, so that it reads the pushed addresses before the app's
  /// navigator, which has no route for them.
  static InvitationLinks listen() {
    final binding = WidgetsFlutterBinding.ensureInitialized();
    final links = InvitationLinks();
    final initial = Uri.tryParse(binding.platformDispatcher.defaultRouteName);
    if (initial != null) links.handOver(initial);
    binding.addObserver(links);
    return links;
  }

  /// The address the phone handed the app: its link waits for the request
  /// form, and is sent at once where the form is on the screen at a question
  /// no link was granted at; any other address is ignored.
  void handOver(Uri address) {
    final link = invitationLinkText(address);
    if (link == null) return;
    _waiting.add(link);
    final bridge = _bridge;
    if (bridge != null) deliver(bridge);
  }

  /// Send the first waiting link on [bridge], as the person's act that
  /// opens it, where the request form is open at a question other than the
  /// one the last link was granted at; the links wait otherwise.  The app
  /// calls it whenever the screen changes, and the next link is sent once
  /// the program has drawn the question after it.
  void deliver(Bridge bridge) {
    _bridge = bridge;
    if (_waiting.isEmpty) return;
    final grant = openLinkGrant(bridge, _waiting.first);
    if (grant == null) return;
    final (id, path, raw) = grant;
    final at = '$id:${formatTerm(path)}';
    if (at == _granted) return;
    _waiting.removeAt(0);
    _granted = at;
    bridge.grant(id, path, raw);
  }

  @override
  Future<bool> didPushRouteInformation(RouteInformation routeInformation) {
    handOver(routeInformation.uri);
    return Future.value(true);
  }
}
