/// The play `coins_ui/3` run in an isolate of its own, over
/// `programs/currencies/coins` resolved from the repo on disc, with the
/// person's grants injected and her screen read back across the isolate
/// boundary.
///
/// `coins_ui(alice, Grants, _)` is the mini-app's bridge entry
/// (`programs/currencies/coins/play_ui.glp`): it gives alice's mini-app the
/// person channel `ch(Grants?, Draws)` of vGLP's Definition "Person Channel,
/// Person Writer, GLP with Persons, Grant", and sends her the draws by
/// `send_to_user/1`.  The test is the person at the bridge (vGLP,
/// sections/elicitation.tex, the paragraph "The implementation"): it reads
/// the constructs the dispatcher draws, `draw(Id, W, V)` and `withdraw(Id)`,
/// and grants `input(Id, P, R)` at the position `P` a draw marks
/// `input(P)`, `R` the ground term of her act.
///
/// The play is run directly, as test code, with no host: the host posts
/// `superapp/3` and nothing of Currencies' (GSG Section 5.1; Currencies #7
/// Cowork, 2026-10-04 09:15 UTC, item 2).  In the play's isolate the engine
/// loads the coins program and posts `coins_ui(alice, Grants?, [])` through
/// its posting call, which checks the goal as every posted goal is checked
/// (TGLP modules.tex, "the initial goal posted to the runtime ... is
/// type-checked before execution as a body goal"; GlpEngine.postGoal); each
/// grant the test sends is injected into `Grants` through the injector the
/// call returned, and every term the play sends alice comes back whole.
///
/// The widget test beside this one drives the surface; this one holds the
/// wiring the surface never sees — the resolved program directory, the goal
/// and its three arguments, and the isolate boundary.
library;

import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart'
    show runtimeTermOf, screenTermOf;
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;

/// What the play's isolate reports that is not a term of alice's screen.
class _PlayFault {
  final String error;
  _PlayFault(this.error);
}

/// The play's isolate: [init] is the test's port and the repo's programs/.
/// It sends back its own port, for alice's grants, once the play is posted;
/// then each term the play sends alice, whole, and a [_PlayFault] where the load,
/// the posting or a run fails.
void _playIsolate((SendPort, String) init) {
  final (reply, repo) = init;
  final grants = ReceivePort();
  final engine = GlpEngine(rootSelfGlpPath: '$repo/self.glp');
  engine.runtime.outputTermCallback = (t) => reply.send(screenTermOf(t));
  try {
    engine.loadProgram('$repo/currencies/coins');
    final play = engine.postGoal('coins_ui(alice, Grants?, [])',
        inputs: ['Grants']);
    final grantsIn = play.inputs['Grants']!;

    /// The play run until quiescent; a run stopped at the net is reported.
    void drain() {
      final result = play.scheduler.drainToQuiescence(maxCycles: 200000);
      if (result.status == ExecutionStatus.capped) {
        reply.send(_PlayFault('the play did not quiesce'));
      }
    }

    reply.send(grants.sendPort);
    drain();
    grants.listen((grant) {
      grantsIn
          .inject(runtimeTermOf(grant as GTerm))
          .forEach(engine.runtime.gq.enqueue);
      drain();
    });
  } catch (e) {
    reply.send(_PlayFault('$e'));
    grants.close();
  }
}

void main() {
  test('alice mints and swaps across the isolate boundary', () async {
    final repo = Directory('../programs').existsSync()
        ? Directory('../programs').absolute.path
        : '/Users/udi/Grassroots/GLP/programs';

    final reply = ReceivePort();
    SendPort? commands;
    final sends = <GTerm>[];
    final r = UiRuntime(manifest: coinsManifest, onSend: sends.add);

    reply.listen((m) {
      if (m is SendPort) {
        commands = m;
      } else if (m is GTerm) {
        r.handle(m);
      } else if (m is _PlayFault) {
        fail('the play: ${m.error}');
      }
    });

    final isolate = await Isolate.spawn(_playIsolate, (reply.sendPort, repo));
    addTearDown(() {
      isolate.kill(priority: Isolate.immediate);
      reply.close();
    });

    Future<bool> until(bool Function() done,
        {Duration t = const Duration(seconds: 20)}) async {
      final end = DateTime.now().add(t);
      while (DateTime.now().isBefore(end)) {
        if (done()) return true;
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      return done();
    }

    /// The balances panel as owner-to-amount, sorted — the state the
    /// assertions below are on. A tally message replaces the panel whole, so
    /// during a swap the panel is empty between her coins leaving and bob's
    /// arriving: a wait on "alice is named" sees the panel mid-swap.
    String balances() {
      final b = r.store.balances['balances'] ?? const <String, GTerm>{};
      final keys = b.keys.toList()..sort();
      return [for (final k in keys) '$k=${formatTerm(b[k]!)}'].join(' ');
    }

    /// The person's grants, each `input(Id, P, R)` as the bridge returns it,
    /// sent across the isolate boundary in order.
    Future<void> send() async {
      while (sends.isNotEmpty) {
        commands!.send(sends.removeAt(0));
        await Future<void>.delayed(const Duration(milliseconds: 200));
      }
    }

    /// The open constructs whose widget is [w].
    List<OpenConstruct> drawn(String w) => [
          for (final c in r.bridge.constructs)
            if (formatTerm(c.widget) == w) c,
        ];

    /// The position of the question [view] marks, where it marks one.
    GList? at(GTerm view) => questionAt(view);

    // The agent asks its question once, as soon as it runs: the stream of
    // her requests, drawn as the default widget of Stream(Request)? --- an
    // input box of a menu of the four forms of Request --- over the view
    // input([]), the whole variable hers to write; each field labelled by
    // the name of its declared type, a Key a peer field.
    const requestForm = "input_box(menu([form(mint, [field('Amount', number)]), "
        "form(swap, [field('Friend', peer), field('GiveCoin', peer), field('GiveAmount', number), "
        "field('WantCoin', peer), field('WantAmount', number)]), "
        "form(pay, [field('Friend', peer), field('Amount', number)]), "
        "form(redeem, [field('Friend', peer), field('WantCoin', peer)])]))";
    expect(await until(() => drawn(requestForm).length == 1), isTrue,
        reason: 'the request form, drawn once');
    final form = drawn(requestForm).single;
    expect(formatTerm(form.view), 'input([])');
    final p = at(form.view)!;

    // Mint 2: one request on the form.
    r.bridge.grant(form.id, p, tryParseTerm('mint(2)')!);
    expect(formatTerm(sends.single), 'input(${form.id}, [], mint(2))');
    await send();
    await until(() => balances() == 'alice=2');
    expect(balances(), 'alice=2');

    // Swap her 2 for bob's 2, the next request on the same form, which a
    // submission does not withdraw; bob's script accepts.
    expect(r.bridge.construct(form.id), isNotNull,
        reason: 'the request form stays open, one request per submission');
    r.bridge.grant(
        form.id, p, tryParseTerm('swap(bob, alice, 2, bob, 2)')!);
    await send();
    expect(
        await until(() => r.store.lists['screen']!
            .any((t) => formatTerm(t) == 'swap_done(bob)')),
        isTrue);
    await until(() => balances() == 'bob=2');
    expect(balances(), 'bob=2');

    // Bob proposes the reverse swap: one card, the swap responder's question,
    // Card ::= card(Offer, YesNo?) in writer mode --- the offer shown and her
    // answer at position [2].
    const card =
        "form(card, [field('Offer', shown), field('YesNo', buttons([yes, no]))])";
    await until(() => drawn(card).length == 1);
    expect(drawn(card), hasLength(1));
    final offer = drawn(card).single;
    // The offer, for her to see: bob wants 2 of his own coins back and offers
    // the 2 alice-coins he holds.
    final (cardCtor, cardArgs) = ctorArgs(offer.view);
    expect(cardCtor, 'card');
    final (offerCtor, offerArgs) = ctorArgs(cardArgs[0]);
    expect(offerCtor, 'offer');
    expect(formatTerm(offerArgs[0]), 'bob');
    expect(formatTerm(offerArgs[1]), 'lot(bob, 2)');
    expect((offerArgs[2] as GList).items.map(formatTerm),
        everyElement(startsWith('coin(alice, ')));
    expect((offerArgs[2] as GList).items, hasLength(2));
    expect(formatTerm(cardArgs[1]), 'input([2])');
    r.bridge.grant(offer.id, at(cardArgs[1])!, const GAtom('yes'));
    expect(formatTerm(sends.single), 'input(${offer.id}, [2], yes)');
    await send();
    await until(() => balances() == 'alice=2');
    expect(balances(), 'alice=2');
    // Answered, the card is withdrawn; the request form stands.
    expect(await until(() => r.bridge.construct(offer.id) == null), isTrue,
        reason: 'the answered card is withdrawn');
    expect(r.bridge.constructs.map((c) => c.id), [form.id]);
  }, timeout: const Timeout(Duration(minutes: 2)));
}
