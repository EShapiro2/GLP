/// GrassApp village market: the six-agent scenario of Grassroots-Bonds §8.2.
///
/// The headless gate for the phone-UI reproduction of the village market. It
/// runs the GrassApp port of programs/currencies/play12 — Alice (baker), Bob
/// (farmer), Charlie (carpenter), Diana (doctor), Eve (teacher), Frank
/// (fisherman) — and checks that all seven operations of §8.2 actually occur
/// and that the economy lands on the paper's closing balances.
///
/// It loads exactly the way the app does — GlpEngine with strictTypes = false
/// and loadSource per file, no programDir — so a pass here also exercises the
/// app's load path, not just the GLP logic.

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  test('GrassApp village market: the seven operations of §8.2', () async {
    const repo = '/Users/udi/Grassroots/GLP';
    const dir = '$repo/programs/book/grassapp';

    const files = <String>[
      'self.glp',
      'currency_txn.glp',
      'grassapp_agent.glp',
      'grassapp_mediator.glp',
      'play_village_headless.glp',
    ];

    // Six agents publishing balances to each other is far more reduction-heavy
    // than the small plays: the scheduler's default budget (1000 cycles) cuts
    // the run off mid-economy. The currencies REPL harness uses the same
    // headroom for this scenario (:limit 5000000).
    final engine = GlpEngine(rootSelfGlpPath: '$repo/programs/self.glp')
      ..strictTypes = false
      ..maxCycles = 5000000;

    for (final f in files) {
      final file = File('$dir/$f');
      expect(file.existsSync(), isTrue,
          reason: 'missing GLP source: ${file.path}');
      engine.loadSource(file.readAsStringSync(), filename: f);
    }

    final lines = <String>[];
    engine.runtime.outputCallback = lines.add;

    await engine.runGoal('play_village.');
    final all = lines.join('\n');

    // The village formed: all eight trust edges.
    for (final edge in [
      'tagged(bob, connected(alice))',
      'tagged(charlie, connected(alice))',
      'tagged(eve, connected(alice))',
      'tagged(diana, connected(bob))',
      'tagged(eve, connected(charlie))',
      'tagged(frank, connected(charlie))',
      'tagged(frank, connected(diana))',
      'tagged(frank, connected(eve))',
    ]) {
      expect(all, contains(edge), reason: 'missing trust edge $edge in:\n$all');
    }

    // Op 1 — symmetric mutual credit: four 1-for-1 credit lines settled.
    expect(all, contains('tagged(alice, trade_completed(bob))'),
        reason: 'no Alice-Bob credit line in:\n$all');
    expect(all, contains('tagged(alice, trade_completed(charlie))'),
        reason: 'no Alice-Charlie credit line in:\n$all');
    expect(all, contains('tagged(charlie, trade_completed(eve))'),
        reason: 'no Charlie-Eve credit line in:\n$all');
    expect(all, contains('tagged(eve, trade_completed(frank))'),
        reason: 'no Eve-Frank credit line in:\n$all');

    // Op 2 — asymmetric credit: Diana's two loans, priced in dated bonds.
    expect(all, contains('tagged(bob, trade_proposed(alice, bob, 0, 15,'),
        reason: 'Alice never offered Bob the 15-for-15 in:\n$all');
    expect(all, contains('tagged(diana, trade_proposed(bob, diana, 0, 20,'),
        reason: "Bob never proposed Diana's loan terms in:\n$all");
    expect(all, contains('tagged(frank, trade_proposed(diana, frank, 28, 18,'),
        reason: 'Diana never offered Frank the loan in:\n$all');
    // The interest is real: Bob's 24 bonds mature at day 25, Frank's 18 at 28.
    expect(all, contains('balance_report(diana, bob, 25, 24)'),
        reason: 'Diana does not hold the 24 bob-bonds(25) in:\n$all');

    // Op 4 — portfolio swap: Eve buys the alice-coins she needs from Charlie.
    expect(all, contains('tagged(charlie, trade_proposed(eve, alice, 0, 5,'),
        reason: 'no portfolio swap proposal from Eve in:\n$all');

    // Op 5 — escrow: deposited, seen by the beneficiary, and time-released
    // (never cancelled, because the dock was built).
    expect(all, contains('tagged(charlie, escrow_deposited(frank,'),
        reason: 'Charlie never escrowed the dock payment in:\n$all');
    expect(all, contains('tagged(frank, escrow_received(charlie,'),
        reason: 'Frank was never told of the escrow in:\n$all');
    expect(all, contains('tagged(frank, escrow_released(charlie))'),
        reason: 'the escrow never released to Frank in:\n$all');
    expect(all, contains('tagged(charlie, escrow_expired(frank))'),
        reason: 'Charlie never saw the escrow expire in:\n$all');
    expect(all, isNot(contains('escrow_cancelled')),
        reason: 'nobody cancelled, so no cancellation should appear in:\n$all');

    // Op 7 — sale of debt: 10 bob-coins for 4 frank-coins, at a discount.
    expect(all, contains('tagged(eve, trade_proposed(alice, frank, 0, 4,'),
        reason: 'Alice never offered Eve the bob-debt in:\n$all');
    expect(all, contains('tagged(alice, trade_completed(eve))'),
        reason: 'the debt sale never settled in:\n$all');

    // Ops 3 and 6 land in the closing balances, which are the paper's:
    //   Alice  5 bob, 15 charlie, 8 alice, 4 frank   (paid twice, sold debt)
    //   Bob    10 alice, 20 diana                    (paid Alice 5)
    //   Eve    10 bob, 2 alice, 4 charlie, 1 frank   (bought the debt)
    //   Frank  7 diana, 10 eve                       (redeemed 5 diana-coins)
    for (final bal in [
      'balance_report(alice, bob, 0, 5)',
      'balance_report(alice, charlie, 0, 15)',
      'balance_report(alice, alice, 0, 8)',
      'balance_report(alice, frank, 0, 4)',
      'balance_report(bob, alice, 0, 10)',
      'balance_report(bob, diana, 0, 20)',
      'balance_report(eve, bob, 0, 10)',
      'balance_report(eve, alice, 0, 2)',
      'balance_report(frank, diana, 0, 7)',
      'balance_report(frank, eve, 0, 10)',
    ]) {
      expect(all, contains(bal), reason: 'closing balance $bal missing in:\n$all');
    }

    expect(lines.where((l) => l.contains('[ERROR]')), isEmpty);
  });
}
