/// GrassApp escrow: the time-release settles to the beneficiary.
///
/// The smoke test for the escrow mechanism the village market needs (paper §5;
/// §8.2 operation 5, Charlie hiring Frank to build a dock). Charlie locks 5 of
/// his own coins for Frank with a real-time release; nobody cancels, so the
/// timer wins the race and Frank is paid.
///
/// It loads exactly the way the app does — GlpEngine with strictTypes = false
/// and loadSource per file, no programDir — so a pass here also exercises the
/// app's load path, not just the GLP logic.
///
/// What it pins down:
///   * the escrow/5 race commits on the timer, not the cancel;
///   * the depositor gets a cancellable req id, and the beneficiary is told
///     of the escrow BEFORE it settles;
///   * on release the bonds actually move into the beneficiary's holdings.

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  test('GrassApp: escrow time-release pays the beneficiary', () async {
    const repo = '/Users/udi/Grassroots/GLP';
    const dir = '$repo/programs/grassapp';

    const files = <String>[
      'self.glp',
      'currency_txn.glp',
      'grassapp_agent.glp',
      'grassapp_mediator.glp',
      'play_escrow_headless.glp',
    ];

    final engine = GlpEngine(rootSelfGlpPath: '$repo/programs/self.glp')
      ..strictTypes = false;

    for (final f in files) {
      final file = File('$dir/$f');
      expect(file.existsSync(), isTrue,
          reason: 'missing GLP source: ${file.path}');
      engine.loadSource(file.readAsStringSync(), filename: f);
    }

    final lines = <String>[];
    engine.runtime.outputCallback = lines.add;

    await engine.runGoal('play_escrow.');
    final all = lines.join('\n');

    // The escrow opened: the depositor holds a cancellable req id, and the
    // beneficiary is told before anything settles.
    // Both sides carry the lot scalars, so the wallet can show what is locked
    // (escrowed bonds leave Holdings, so no balance_report describes them).
    expect(all, contains('escrow_deposited(frank, '),
        reason: 'no escrow_deposited for charlie in:\n$all');
    expect(all, contains(', charlie, 0, 5, req('),
        reason: 'escrow_deposited does not carry the 5 charlie-coins in:\n$all');
    expect(all, contains('tagged(frank, escrow_received(charlie, '),
        reason: 'frank was not told of the escrow in:\n$all');
    expect(all, contains('charlie, 0, 5))'),
        reason: 'escrow_received does not carry the 5 charlie-coins in:\n$all');

    // The timer won the race, not the cancel.
    expect(all, contains('tagged(frank, escrow_released(charlie))'),
        reason: 'the escrow never released to frank in:\n$all');
    expect(all, contains('tagged(charlie, escrow_expired(frank))'),
        reason: 'charlie was not told the escrow expired in:\n$all');
    expect(all, isNot(contains('escrow_cancelled')),
        reason: 'nobody cancelled, so no cancellation should appear in:\n$all');

    // The coins actually moved — the point of the whole mechanism.
    expect(all, contains('balance_report(frank, charlie, 0, 5)'),
        reason: 'frank did not receive the escrowed coins in:\n$all');

    expect(lines.where((l) => l.contains('[ERROR]')), isEmpty);
  });
}
