// An invitation link the person tapped is the person's act (IGLP,
// Implementation Notes, "Boot": "An address the smartphone hands the
// application, an invitation link the person tapped, is the person's act: the
// application sends on the person channel the act that opens the link, as if
// the person had entered its text").  The address is GNA's, Section 6,
// "Invitation links", https://peoplesweb.org/i# followed by the link's text.
//
// The tests: the link's text is the text after the #, percent-decoded, of an
// address of that form and of no other; and the act the app sends for it is
// the grant home's request form makes when the person chooses Open link,
// enters the same text and submits it (lib/invitation_link.dart).
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/invitation_link.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

/// The request form of home, as the super-app draws it
/// (programs/social/graph/core/home.glp; test/superapp_screen_test.dart).
const _requestForm = "input_box(menu([form(trust, [field('CompilerKey', peer)]), "
    "form(install, [field('AppName', text), field('Artefact', text)]), "
    "form(invite, [field('AppName', text), field('Friend', peer)]), "
    "button(open_place), "
    "button(close_place), "
    "button(invite_link), "
    "form(open_link, [field('Link', text)]), "
    "form(list, [field('Email', text), field('Name', text)]), "
    "form(list_unlisted, [field('Email', text), field('Name', text)]), "
    "button(unlist)]))";

/// A link's text with the characters a term's text carries: spaces, quotes,
/// parentheses, commas, a colon and a dot.
const _link = "signed(invitation('ab12', 'cd34', '10.0.0.1:4433', 7, 1762905600), 'ef56')";

/// The address of [link] in GNA's form, its text percent-encoded as the
/// phone hands it.
Uri _address(String link) =>
    Uri.parse('https://peoplesweb.org/i#${Uri.encodeComponent(link)}');

/// A bridge with construct 1 open, home's request form at its question
/// `[]`, and every grant it returns.
(Bridge, List<String>) _bridge() {
  final grants = <String>[];
  final bridge = Bridge(onGrant: (g) => grants.add(formatTerm(g)));
  final draw = tryParseTerm('draw(1, $_requestForm, stream_request_r(input([])))');
  expect(draw, isNotNull);
  expect(bridge.read(draw!), isTrue);
  return (bridge, grants);
}

void main() {
  group('the link of an address', () {
    test('GNA\'s form gives the text after the #, percent-decoded', () {
      expect(invitationLinkText(_address(_link)), _link);
      expect(invitationLinkText(Uri.parse('https://peoplesweb.org/i#abc')), 'abc');
      expect(invitationLinkText(Uri.parse('https://peoplesweb.org/i#a%20b%28c%29%27d%27')),
          "a b(c)'d'");
      // A + is itself: the fragment is not a query.
      expect(invitationLinkText(Uri.parse('https://peoplesweb.org/i#a+b')), 'a+b');
      expect(invitationLinkText(Uri.parse('https://peoplesweb.org/i#%C3%A9')), 'é');
    });

    test('any other address gives none', () {
      for (final a in [
        'https://example.org/i#abc', // another host
        'https://www.peoplesweb.org/i#abc',
        'https://peoplesweb.org/j#abc', // another path
        'https://peoplesweb.org/i/#abc',
        'https://peoplesweb.org/#abc',
        'https://peoplesweb.org/i/x#abc',
        'https://peoplesweb.org/i', // no fragment
        'https://peoplesweb.org/i#', // an empty one
        'http://peoplesweb.org/i#abc', // another scheme
        'grassroots://peoplesweb.org/i#abc',
        'https://peoplesweb.org:8443/i#abc', // a port
        'https://u@peoplesweb.org/i#abc', // a user
        'https://peoplesweb.org/i?x=1#abc', // a query
        '/', // the platform's default route
      ]) {
        expect(invitationLinkText(Uri.parse(a)), isNull, reason: a);
      }
    });
  });

  testWidgets('the act for the address is the act the form sends for its text',
      (tester) async {
    // The person chooses Open link on the request form, enters the link's
    // text and submits it.
    final (formBridge, formGrants) = _bridge();
    await tester.pumpWidget(MaterialApp(
        home: Scaffold(
            body: SingleChildScrollView(
                child: ConstructView(
                    construct: formBridge.construct(1)!,
                    grant: (p, r) => formBridge.grant(1, p, r))))));
    await tester.tap(find.widgetWithText(ChoiceChip, 'open_link'));
    await tester.pumpAndSettle();
    await tester.enterText(
        find.descendant(
            of: find.byKey(nodeKey(1, 'text', const [1])),
            matching: find.byType(TextField)),
        _link);
    await tester.pumpAndSettle();
    await tester.ensureVisible(find.byKey(submitKey(1, const [])));
    await tester.tap(find.byKey(submitKey(1, const [])));
    await tester.pumpAndSettle();
    expect(formGrants, hasLength(1));

    // The phone hands the app the address of the same text.
    final (linkBridge, linkGrants) = _bridge();
    final links = InvitationLinks()..handOver(_address(_link));
    links.deliver(linkBridge);
    expect(links.waiting, isEmpty);
    expect(linkGrants, formGrants);
    expect(
        linkGrants.single,
        formatTerm(const GStruct('input', [
          GInt(1),
          GList([]),
          GStruct('open_link', [GString(_link)]),
        ])));
  });

  test('a link waits for the request form, and is sent once it is open', () {
    final grants = <String>[];
    final bridge = Bridge(onGrant: (g) => grants.add(formatTerm(g)));
    final links = InvitationLinks()..handOver(_address('abc'));
    links.deliver(bridge);
    expect(grants, isEmpty);
    expect(links.waiting, ['abc']);
    bridge.read(tryParseTerm('draw(1, $_requestForm, stream_request_r(input([])))')!);
    links.deliver(bridge);
    expect(links.waiting, isEmpty);
    expect(grants, ["input(1, [], open_link(abc))"]);
    // A link handed over once the form is on the screen is sent at once.
    links.handOver(_address('def'));
    expect(grants.last, "input(1, [], open_link(def))");
  });

  test('an address of another form is ignored, and none is sent', () {
    final (bridge, grants) = _bridge();
    final links = InvitationLinks();
    links.deliver(bridge);
    links.handOver(Uri.parse('https://peoplesweb.org/x#abc'));
    links.handOver(Uri.parse('https://peoplesweb.org/i'));
    expect(links.waiting, isEmpty);
    expect(grants, isEmpty);
  });

  test('a pushed route is taken whatever it is, and only a link is sent',
      () async {
    final (bridge, grants) = _bridge();
    final links = InvitationLinks()..deliver(bridge);
    expect(
        await links.didPushRouteInformation(
            RouteInformation(uri: Uri.parse('/somewhere'))),
        isTrue);
    expect(grants, isEmpty);
    expect(
        await links.didPushRouteInformation(
            RouteInformation(uri: _address('abc'))),
        isTrue);
    expect(grants, ["input(1, [], open_link(abc))"]);
  });
}
