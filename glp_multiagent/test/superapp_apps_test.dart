/// The Grassroots App on the real entry, two phones in one process, each a [SuperAppHost] over a
/// network in this process, as superapp_nearby_test.dart has them: the persons meet and befriend in
/// Nearby, then each installs a mini-app the app carries, GSG's Friendship (programs/apps/gsg), and
/// its screen is shown to the person: what the mini-app draws on its person channel reaches the
/// phone under its name, app(gsg, Draw), and the person's grants on it go back under that name
/// (programs/social/graph/core/self.glp, shown_superapp/7, app_screens/1).  Bob consents in advance
/// to Alice's invitation, Alice invites him, and each phone draws the mini-app, its view naming the
/// person's own key.
library;

import 'dart:async';
import 'dart:io';

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/superapp_apps.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/manifests/superapp_ui.dart';
import 'package:glp_multiagent/superapp_host.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart'
    show generateKeyPair;

import 'helpers/air.dart';
import 'programs_dir.dart';

/// One phone: the app's host, its network, and what its person sees.
class _Phone {
  final String name;
  final SuperAppHost host;
  final AirNetwork net;
  final List<String> errors = [];
  final List<(bool Function(), Completer<void>)> _waits = [];

  /// The agent's transcript, and, where GLP_TRACE_DIR names a directory, the
  /// file it is written to as it comes.
  final List<String> lines = [];

  _Phone(this.name, this.host, this.net) {
    host.ui.onChange = _changed;
    final dir = Platform.environment['GLP_TRACE_DIR'];
    final sink = dir == null ? null : File('$dir/$name.log').openWrite();
    host.onOutput = (line) {
      lines.add(line);
      sink?.writeln(line);
      if (line.startsWith('[ERROR]')) {
        errors.add(line);
        for (final (_, c) in _waits) {
          c.completeError(StateError('$name: $line'));
        }
        _waits.clear();
      }
    };
  }

  UiRuntime get ui => host.ui;
  String get key => host.key;

  void _changed() => _waits.removeWhere((w) {
        if (!w.$1()) return false;
        w.$2.complete();
        return true;
      });

  /// Once [test] holds of what the person sees: on the event of the screen's
  /// change, with no clock.
  Future<void> until(bool Function() test) async {
    // On the screen's change, and, a mini-app's screen changing on a message from the other
    // phone, looked at again every tenth of a second.
    while (!test()) {
      final c = Completer<void>();
      _waits.add((test, c));
      await Future.any([c.future, Future<void>.delayed(const Duration(milliseconds: 100))]);
      _waits.removeWhere((w) => w.$2 == c);
    }
  }

  /// What the agent told the person, each notice's content, an atom by its
  /// name: a key, 64 hex characters, as it is.
  List<String> notices() {
    final v = ui.bridge.construct(0)?.view;
    return [
      if (v is GList)
        for (final m in v.items) _plain(ctorArgs(m).$2[2])
    ];
  }

  /// Home's request form: the stream of the person's requests.
  OpenConstruct? form() {
    for (final c in ui.bridge.constructs) {
      if (c.id != 0 && formatTerm(c.widget).startsWith('input_box(menu(')) {
        return c;
      }
    }
    return null;
  }

  /// One request on home's form, as the person sends it.
  void request(GTerm r) {
    final f = form()!;
    ui.bridge.grant(f.id, questionAt(f.view)!, r);
  }

  /// A friend's request to connect, the card home draws, where one is open:
  /// `form(from, ...)`, the friend and the buttons yes and no.
  OpenConstruct? card() {
    for (final c in ui.bridge.constructs) {
      if (c.id != 0 &&
          formatTerm(c.widget).startsWith('form(from,') &&
          _questionIn(c.view) != null) {
        return c;
      }
    }
    return null;
  }

  /// Yes on the card.
  void yes() {
    final c = card()!;
    ui.bridge.grant(c.id, _questionIn(c.view)!, const GAtom('yes'));
  }

  /// The card home draws of a friend's invitation to a mini-app installed here, where one is open:
  /// `form(invited, ...)`, the friend, the mini-app and the answer (home.vglp, respond_invite/4).
  OpenConstruct? inviteCard() {
    for (final c in ui.bridge.constructs) {
      if (c.id != 0 && formatTerm(c.widget).startsWith('form(invited,') && _questionIn(c.view) != null) return c;
    }
    return null;
  }

  /// The card home draws of a friend and a mini-app installed here, `form(invitation, ...)`, with
  /// the button invite (home.vglp, invite_card/4).
  OpenConstruct? invitationCard(String friend, String app) {
    for (final c in ui.bridge.constructs) {
      final v = formatTerm(c.view);
      if (c.id != 0 && formatTerm(c.widget).startsWith('form(invitation,') && _questionIn(c.view) != null &&
          v.contains(friend) && v.contains(app)) {
        return c;
      }
    }
    return null;
  }

  /// Invite on that card.
  void pickInvite(String friend, String app) {
    final c = invitationCard(friend, app)!;
    ui.bridge.grant(c.id, _questionIn(c.view)!, const GAtom('invite'));
  }

  /// Yes on the invitation's card.
  void acceptInvite() {
    final c = inviteCard()!;
    ui.bridge.grant(c.id, _questionIn(c.view)!, const GAtom('yes'));
  }

  /// The card home draws of a person nearby whom the person knows, the
  /// person of key [key], where one is open: `form(nearby, ...)`, the person
  /// and the button befriend (home.vglp, befriend_card/3).
  OpenConstruct? nearbyCard(String key) {
    for (final c in ui.bridge.constructs) {
      if (c.id != 0 &&
          formatTerm(c.widget).startsWith('form(nearby,') &&
          _plain(c.view).contains(key) &&
          _questionIn(c.view) != null) {
        return c;
      }
    }
    return null;
  }

  /// Befriend on the card of the person of key [key]: the person picks them
  /// in Nearby, and types no key.
  void befriend(String key) {
    final c = nearbyCard(key)!;
    ui.bridge.grant(c.id, _questionIn(c.view)!, const GAtom('befriend'));
  }
}

/// [t] as text, an atom by its name, unquoted.
String _plain(GTerm t) => switch (t) {
      GAtom(:final name) => name,
      GList(:final items) => '[${items.map(_plain).join(', ')}]',
      GStruct(:final functor, :final args) =>
        '$functor(${args.map(_plain).join(', ')})',
      _ => formatTerm(t),
    };

/// The position of the question a view marks, anywhere inside it.
GList? _questionIn(GTerm v) {
  final p = questionAt(v);
  if (p != null) return p;
  if (v is GStruct) {
    for (final a in v.args) {
      final q = _questionIn(a);
      if (q != null) return q;
    }
  } else if (v is GList) {
    for (final a in v.items) {
      final q = _questionIn(a);
      if (q != null) return q;
    }
  }
  return null;
}

void main() {
  final programs = programsDir();
  final air = Air();
  final phones = <_Phone>[];

  setUpAll(() async {
    // The certified mini-app the phones carry: an artefact (gitignored) written into the
    // super-app's directory, where load_file/2 reads it, as the app's bundle has it
    // (tool/sync_glp_assets.sh).
    final out = await Process.run('bash', ['-c', "printf ':artefact ../programs/apps/gsg ../programs/social/graph/core\\n:quit\\n' | bin/glpc"],
        workingDirectory: Directory('$programs/../glp_runtime').absolute.path);
    expect('${out.stdout}${out.stderr}', contains('gsg.glpw'));
  });

  Future<_Phone> phone(String name) async {
    final kp = generateKeyPair();
    final net = AirNetwork(kp, air);
    final host = SuperAppHost(
      agentId: name,
      program: '$programs/social/graph/core',
      rootSelfGlpPath: '$programs/self.glp',
      network: net,
      manifest: superappManifest,
    );
    final p = _Phone(name, host, net);
    host.onAppsChange = p._changed;
    phones.add(p);
    await host.start();
    await p.until(() => p.form() != null);
    return p;
  }

  tearDownAll(() {
    for (final p in phones) {
      p.host.dispose();
    }
  });

  late _Phone alice, bob;

  test('two phones meet and befriend in Nearby', () async {
    alice = await phone('alice');
    bob = await phone('bob');
    alice.request(const GAtom('open_place'));
    bob.request(const GAtom('open_place'));
    await alice.until(() => alice.nearbyCard(bob.key) != null);
    alice.befriend(bob.key);
    await bob.until(() => bob.card() != null);
    bob.yes();
    await alice.until(() => alice.notices().contains('connected(${bob.key})'));
    await bob.until(() => bob.notices().contains('connected(${alice.key})'));
  }, timeout: const Timeout(Duration(minutes: 3)));

  test('each installs Friendship and Alice invites Bob: his home asks him on a card of its own, he says '
      'yes, and each phone shows the mini-app\'s screen, under its name, its view its person\'s own', () async {
    // Each installs it by home's request, picking it among the apps the phone carries: no key typed
    // and no file named (home.vglp, install_carried).
    for (final p in [alice, bob]) {
      p.request(const GStruct('install_carried', [GAtom('friendship')]));
    }
    await alice.until(() => alice.host.apps.containsKey('friendship'));
    await bob.until(() => bob.host.apps.containsKey('friendship'));
    // Home draws Alice a card of Bob, her friend, and the mini-app she installed: she picks invite.
    await alice.until(() => alice.invitationCard(bob.key, 'friendship') != null);
    alice.pickInvite(bob.key, 'friendship');
    // The invitation waits for Bob's consent: the agent tells him, invited(Alice, gsg), and home asks him.
    await bob.until(() => bob.inviteCard() != null);
    expect(formatTerm(bob.inviteCard()!.view), allOf(contains('friendship'), contains(alice.key)));
    bob.acceptInvite();
    String? viewOf(_Phone p) {
      final v = p.host.apps['friendship']?.bridge.construct(0)?.view;
      return v == null ? null : formatTerm(v);
    }
    await alice.until(() => (viewOf(alice) ?? '').contains('view('));
    await bob.until(() => (viewOf(bob) ?? '').contains('view('));
    expect(viewOf(alice), contains(alice.key));
    expect(viewOf(bob), contains(bob.key));
    // The mini-app's own request form is drawn on its screen, apart from home's.
    OpenConstruct? request(_Phone p) => p.host.apps['friendship']!.bridge.constructs
        .where((c) => c.id != 0 && formatTerm(c.widget).contains('social_menu'))
        .firstOrNull;
    await alice.until(() => request(alice) != null);
    expect(alice.form(), isNotNull, reason: 'home is drawn as before');
    expect(alice.errors, isEmpty);
    expect(bob.errors, isEmpty);
  }, timeout: const Timeout(Duration(minutes: 5)));

  test('on the mini-app\'s screens, over the network between the phones: each sets up an identity, '
      'Alice asks to know Bob, Bob says yes on his phone, and each knows the other', () async {
    String viewOf(_Phone p) => formatTerm(p.host.apps['friendship']!.bridge.construct(0)!.view);
    // The person's last view on the mini-app's screen holds knows(K): read off the terms, a key
    // being an atom whatever its first character.
    bool knows(_Phone p, String k) {
      final v = p.host.apps['friendship']!.bridge.construct(0)!.view;
      final views = v is GList ? v.items.where((x) => x is GStruct && x.functor == 'view').toList() : const <GTerm>[];
      if (views.isEmpty) return false;
      final state = (views.last as GStruct).args[1];
      return state is GList &&
          state.items.any((a) => a is GStruct && a.functor == 'knows' && a.args.single is GAtom && (a.args.single as GAtom).name == k);
    }
    OpenConstruct request(_Phone p) => p.host.apps['friendship']!.bridge.constructs
        .firstWhere((c) => c.id != 0 && formatTerm(c.widget).contains('social_menu'));
    void propose(_Phone p, GTerm act) {
      final r = request(p);
      p.host.apps['friendship']!.bridge.grant(r.id, questionAt(r.view) ?? const GList([]), GStruct('propose', [act]));
    }
    for (final p in [alice, bob]) {
      propose(p, GStruct('establish', [GAtom(p.key)]));
    }
    await alice.until(() => viewOf(alice).contains('identity'));
    await bob.until(() => viewOf(bob).contains('identity'));
    propose(alice, GStruct('acquaint', [GAtom(alice.key), GAtom(bob.key)]));
    // Bob's phone asks him, on the mini-app's screen: its approval question.
    OpenConstruct? asked() => bob.host.apps['friendship']!.bridge.constructs
        .where((c) => c.id != 0 && _questionIn(c.view) != null && !formatTerm(c.widget).contains('social_menu'))
        .firstOrNull;
    await bob.until(() => asked() != null);
    final q = asked()!;
    bob.host.apps['friendship']!.bridge.grant(q.id, _questionIn(q.view)!, const GAtom('yes'));
    await alice.until(() => knows(alice, bob.key));
    await bob.until(() => knows(bob, alice.key));
    expect(alice.errors, isEmpty);
    expect(bob.errors, isEmpty);
  }, timeout: const Timeout(Duration(minutes: 5)));

  testWidgets('the phone\'s screens: home, and a tab for each installed mini-app, opening its own '
      'screen; installing and inviting are home\'s', (tester) async {
    await tester.pumpWidget(MaterialApp(home: Scaffold(body: SuperAppScreens(host: alice.host, home: const Text('home')))));
    expect(find.text('home'), findsOneWidget);
    expect(find.byKey(const ValueKey('superapp:tab:friendship')), findsOneWidget);
    expect(find.text('Friendship'), findsOneWidget, reason: 'the tab reads the mini-app\'s name');
    await tester.tap(find.byKey(const ValueKey('superapp:tab:friendship')));
    await tester.pumpAndSettle();
    expect(find.text('home'), findsNothing);
    expect(find.byType(AgentSurface), findsOneWidget, reason: 'the mini-app\'s own screen');
    await tester.tap(find.byKey(const ValueKey('superapp:tab:home')));
    await tester.pumpAndSettle();
    expect(find.text('home'), findsOneWidget);
  });
}
