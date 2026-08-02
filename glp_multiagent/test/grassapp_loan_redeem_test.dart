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
///   * presenting bob-bonds for redemption BEFORE the ISSUER's own clock reaches
///     maturity is not a redemption (Def 3.2): bob reclassifies it as a normal
///     offer and returns it — and returns it *before* bob advances its date,
///     which is what makes it a gate rather than a coincidence;
///   * after bob advances its own date the re-presented redemption is honoured;
///   * set-off returns the lender's own coins, and money is conserved.

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  test('GrassApp: loan then maturity-gated redemption on local clocks',
      () async {
    const repo = '/Users/udi/Grassroots/GLP';
    const dir = '$repo/programs/grassapp';

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

    // The gate is the ISSUER's date (Def 3.2), not the holder's.  At bob's date
    // 0 the bob-bonds are not coins, so bob reclassifies the early presentation
    // as a normal offer and returns it — it is NOT honoured as a redemption.
    // (bob advances its own date and declines in one step, so the advance and
    // the return are adjacent; the point is that the honour comes only after.)
    final returned = all.indexOf('tagged(alice, trade_returned(bob))');
    final advanced = all.indexOf('tagged(bob, date_advanced(11))');
    expect(returned, greaterThanOrEqualTo(0),
        reason: 'the early presentation was not reclassified and returned in:\n$all');
    expect(advanced, greaterThanOrEqualTo(0),
        reason: "the issuer's clock never advanced in:\n$all");

    // The redemption is honoured only after bob's own date reaches maturity, and
    // the early presentation is returned before that honour — so the gate is the
    // issuer's date, not a coincidence.
    final honoured = all.indexOf('tagged(alice, trade_completed(bob))', advanced);
    expect(honoured, greaterThan(advanced),
        reason: 'redemption not honoured after the issuer advanced its date in:\n$all');
    expect(returned, lessThan(honoured),
        reason: 'the early presentation must be returned before the redemption is honoured');

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
