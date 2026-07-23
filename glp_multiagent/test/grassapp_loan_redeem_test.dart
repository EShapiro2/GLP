/// GrassApp currencies: loan -> redemption on per-agent local clocks.
///
/// The GrassApp analogue of currencies fplay13, and the standing regression for
/// the Milestone 1b rebuild (agent/7 + the currencies mediator + the lifted
/// transaction core).
///
/// It loads exactly the way the app does — GlpEngine with strictTypes = false
/// and loadSource per file, no programDir (see glp_runtime
/// lib/multiagent/agent_runtime.dart, the mode main_grassapp_duo.dart uses) — so
/// a pass here also exercises the app's load path, not just the GLP logic.
///
/// What it pins down:
///   * a loan is a maturity-carrying swap (lot specs with a future maturity);
///   * redeeming BEFORE the holder's own clock reaches maturity is refused —
///     and refused *before* the date advances, which is what makes it a gate
///     rather than a coincidence;
///   * after Advance-date the same redemption is honoured by the issuer;
///   * set-off returns the lender's own coins, and money is conserved.

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  test('GrassApp: loan then maturity-gated redemption on local clocks',
      () async {
    const repo = '/Users/udi/Grassroots/GLP';
    const dir = '$repo/programs/book/grassapp';

    // Same list the app loads, plus the headless scenario.
    const files = <String>[
      'self.glp',
      'currency_txn.glp',
      'grassapp_agent.glp',
      'grassapp_mediator.glp',
      'play_loan_headless.glp',
    ];

    final engine = GlpEngine(rootSelfGlpPath: '$repo/programs/self.glp')
      ..strictTypes = false;

    for (final f in files) {
      final file = File('$dir/$f');
      expect(file.existsSync(), isTrue, reason: 'missing GLP source: ${file.path}');
      engine.loadSource(file.readAsStringSync(), filename: f);
    }

    final lines = <String>[];
    engine.runtime.outputCallback = lines.add;

    await engine.runGoal('play_loan.');
    final all = lines.join('\n');

    // The loan: alice offers her maturity-0 coins, wants bob-bonds dated 10.
    expect(all, contains('trade_proposed(alice, bob, 10, 5'),
        reason: 'no maturity-carrying loan offer in:\n$all');

    // The gate: the pre-maturity redemption is refused, and it is refused
    // BEFORE the date advances. Order is the point — presence alone would also
    // hold if the refusal came from something else later in the run.
    final refused = all.indexOf('tagged(alice, trade_failed(bob))');
    final advanced = all.indexOf('tagged(alice, date_advanced(11))');
    expect(refused, greaterThanOrEqualTo(0),
        reason: 'pre-maturity redeem was not refused in:\n$all');
    expect(advanced, greaterThanOrEqualTo(0),
        reason: 'local clock never advanced in:\n$all');
    expect(refused, lessThan(advanced),
        reason: 'the refusal must precede the advance — otherwise the maturity '
            'gate is not what refused it');

    // After advancing, the same redemption is honoured.
    expect(all.indexOf('tagged(alice, trade_completed(bob))', advanced),
        greaterThan(advanced),
        reason: 'redemption not honoured after the date advanced in:\n$all');

    // Set-off and conservation: alice has her own 3 coins back plus the 2
    // unredeemed bob-bonds (the interest); bob holds the 3 of his own bonds
    // that came home. 3 alice-coins and 5 bob-bonds, all accounted for.
    expect(all, contains('balance_report(alice, alice, 0, 3)'),
        reason: 'set-off did not return the lender its own coins');
    expect(all, contains('balance_report(alice, bob, 10, 2)'),
        reason: 'the 2 unredeemed bob-bonds (the interest) are missing');
    expect(all, contains('balance_report(bob, bob, 10, 3)'),
        reason: "the issuer's own redeemed bonds did not come home");

    expect(lines.where((l) => l.contains('[ERROR]')), isEmpty);
  });
}
