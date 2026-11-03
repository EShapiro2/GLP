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
@Timeout.none
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

/// The play's isolate has run to quiescence on its start, or on a grant: the
/// terms that run sent alice are ahead of this on the port, which delivers in
/// order.
class _PlayIdle {
  const _PlayIdle();
}

/// The play's isolate: [init] is the test's port and the repo's programs/.
/// It sends back its own port, for alice's grants, once the play is posted;
/// then each term the play sends alice, whole, a [_PlayIdle] when a run is done,
/// and a [_PlayFault] where the load, the posting or a run fails.
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
      reply.send(const _PlayIdle());
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

    // The waits are on events alone and on no clock (IGLP-coop, 2026-11-02;
    // until then each polled every 50 ms until a deadline of twenty seconds,
    // and a grant waited 200 ms): the play's runs are owed, its start and one
    // for each grant sent, and each [_PlayIdle] is one done; a wait ends when
    // its condition holds, and fails on a fault, or on quiescence with its
    // condition not holding, which nothing not sent will change.
    var owed = 1;
    var done = 0;
    final faults = <String>[];
    final waits = <(String, bool Function(), Completer<void>)>[];
    void check() {
      waits.removeWhere((w) {
        final (what, holds, c) = w;
        if (faults.isNotEmpty) {
          c.completeError(StateError('waiting for $what: ${faults.first}'));
        } else if (holds()) {
          c.complete();
        } else if (done >= owed) {
          c.completeError(
              StateError('the play is quiescent and $what does not hold'));
        } else {
          return false;
        }
        return true;
      });
    }

    Future<void> until(String what, bool Function() holds) {
      final c = Completer<void>();
      waits.add((what, holds, c));
      check();
      return c.future;
    }

    reply.listen((m) {
      if (m is SendPort) {
        commands = m;
      } else if (m is GTerm) {
        r.handle(m);
      } else if (m is _PlayIdle) {
        done++;
      } else if (m is _PlayFault) {
        faults.add('the play: ${m.error}');
      }
      check();
    });

    final events = ReceivePort();
    events.listen((e) {
      faults.add(e is List
          ? 'the play\'s isolate threw: ${e.first}'
          : 'the play\'s isolate exited');
      check();
    });
    final isolate = await Isolate.spawn(_playIsolate, (reply.sendPort, repo),
        onError: events.sendPort, onExit: events.sendPort);
    addTearDown(() {
      events.close();
      isolate.kill(priority: Isolate.immediate);
      reply.close();
    });

    /// What the agent has written to her, its screen's content: the
    /// elements of the output thread, construct 0, which is how her screen
    /// reaches her (vGLP 169b008, Definition "Canonical Compilation"; vGLP,
    /// 2026-10-13 19:25 UTC, items 2 and 3).
    List<GTerm> screen() {
      final v = r.bridge.construct(0)?.view;
      return [if (v is GList) ...v.items];
    }

    /// Her latest holdings as issuer-to-amount, sorted --- the state the
    /// assertions below are on.  A tally message replaces the last whole, so
    /// during a swap it is empty between her coins leaving and bob's
    /// arriving: a wait on "alice is named" sees it mid-swap.
    String balances() {
      final hs = [
        for (final m in screen())
          if (ctorArgs(m).$1 == 'holdings') m
      ];
      if (hs.isEmpty) return '';
      final lots = ctorArgs(hs.last).$2.single as GList;
      final pairs = [
        for (final l in lots.items)
          '${formatTerm(ctorArgs(l).$2[0])}=${formatTerm(ctorArgs(l).$2[1])}'
      ]..sort();
      return pairs.join(' ');
    }

    /// The person's grants, each `input(Id, P, R)` as the bridge returns it,
    /// sent across the isolate boundary in order.
    Future<void> send() async {
      while (sends.isNotEmpty) {
        owed++;
        commands!.send(sends.removeAt(0));
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
    await until('the request form, drawn once',
        () => drawn(requestForm).length == 1);
    final form = drawn(requestForm).single;
    expect(formatTerm(form.view), 'input([])');
    final p = at(form.view)!;

    // Mint 2: one request on the form.
    r.bridge.grant(form.id, p, tryParseTerm('mint(2)')!);
    expect(formatTerm(sends.single), 'input(${form.id}, [], mint(2))');
    await send();
    await until('alice=2', () => balances() == 'alice=2');
    expect(balances(), 'alice=2');

    // Swap her 2 for bob's 2, the next request on the same form, which a
    // submission does not withdraw; bob's script accepts.
    expect(r.bridge.construct(form.id), isNotNull,
        reason: 'the request form stays open, one request per submission');
    r.bridge.grant(
        form.id, p, tryParseTerm('swap(bob, alice, 2, bob, 2)')!);
    await send();
    await until('swap_done(bob)',
        () => screen().any((t) => formatTerm(t) == 'swap_done(bob)'));
    await until('bob=2', () => balances() == 'bob=2');
    expect(balances(), 'bob=2');

    // Bob proposes the reverse swap: one card, the swap responder's question,
    // Approval ::= approval(Offer, YesNo?) in writer mode --- the offer shown and her
    // answer at position [2].  The offer, a tuple the program writes, is a
    // form of shown fields, each labelled by its declared type's name, its
    // lot and its coins forms of their own (Definition "Widget Declaration,
    // Default Widget"; vGLP #8 Cowork, 2026-10-13 08:15 UTC, item b).
    const card = "form(approval, [field('Offer', form(offer, "
        "[field('From', shown), field('Want', form(lot, [field('Issuer', shown), "
        "field('Amount', shown)])), field('Offered', thread(form(coin, "
        "[field('Issuer', shown), field('Serial', shown)])))])), "
        "field('YesNo', buttons([yes, no]))])";
    await until('the card, drawn once', () => drawn(card).length == 1);
    expect(drawn(card), hasLength(1));
    final offer = drawn(card).single;
    // The offer, for her to see: bob wants 2 of his own coins back and offers
    // the 2 alice-coins he holds.
    final (cardCtor, cardArgs) = ctorArgs(offer.view);
    expect(cardCtor, 'approval');
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
    await until('alice=2', () => balances() == 'alice=2');
    expect(balances(), 'alice=2');
    // Answered, the card is withdrawn; the request form stands.
    await until('the answered card withdrawn',
        () => r.bridge.construct(offer.id) == null);
    expect(r.bridge.constructs.map((c) => c.id), [0, form.id]);
  }, timeout: Timeout.none);
}
