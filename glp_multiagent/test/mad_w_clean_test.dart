/// Two isolates, `_w` carried through a channel whose Out position is closed
/// (no UI, no mediator).
///
/// Alice MAD-sends a bare writer wrapped in w/1 to bob. Two things are pinned:
/// bob_produced, which is the `_w`-backed reader matching the nested clause
/// head w(W?) and is the admissibility the fixture exists to show; and
/// bob_ch_otherwise, which is bob_consumer's first clause not committing
/// because this fixture supplies ch(S?, closed) — a constant where its head,
/// ch([wrapped(Y?)|In], Out?), the form receive/3's unit clause unfolds to, has
/// the reader Out?, and a term against a head reader is "fail" (GLP-Spec
/// appendix-term-matching.tex, Definition Term Matching, column Reader X2?).
/// mad_w_probe_test.dart is the same file with a writer at Out, which is
/// assigned the head's reader, and matches.
///
/// Each outcome is asserted singly. The old disjunction matched||otherwise was
/// vacuous: one of bob_consumer's two clauses always fires.
@Timeout.none
library;

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'isolate_agents.dart';
import 'programs_dir.dart';

void main() {
  test('_w matches the nested head; the channel head fails on a constant at Out',
      () async {
    final programs = programsDir();
    // The one program each agent runs, a module file (TGLP, def:program).
    final probe = '$programs/tests/mad_w_clean.glp';
    final rootSelf = '$programs/self.glp';

    final agents = IsolateAgents();
    addTearDown(agents.dispose);
    await agents.spawn('alice', probe, rootSelf);
    await agents.spawn('bob', probe, rootSelf);
    await agents.ready();
    agents.start('alice');
    agents.start('bob');
    // Each person's first act tells the agent its role and the keys of the
    // peers it knows, the agent started from agent(ch(_?,_),ch(_?,_))
    // (GLP-Spec maglp.tex, Definition "Multiagent GLP"; IGLP 670d36a,
    // Implementation Notes, "Boot", and Remark "Agent Names").
    agents.input('alice', const GStruct(
        'init', [GAtom('alice'), GList([GAtom('bob')])]));
    agents.input('bob', const GStruct('init', [GAtom('bob'), GList([])]));

    // Both reports, and then quiescence: bob_producer and bob_consumer are
    // concurrent and the order they reach the output is not fixed, and the
    // clause that must not fire is judged once nothing more will run.
    await agents.until('bob_produced and bob_ch_otherwise',
        () => agents.has('bob', 'bob_produced') && agents.has('bob', 'bob_ch_otherwise'));
    await agents.settle();
    final out = agents.out;
    final produced = out['bob']!.any((l) => l.contains('bob_produced'));
    final matched = out['bob']!.any((l) => l.contains('bob_ch_matched'));
    final otherwise = out['bob']!.any((l) => l.contains('bob_ch_otherwise'));

    // ignore: avoid_print
    print('BOB OUT: ${out['bob']}');
    // ignore: avoid_print
    print('ALICE OUT: ${out['alice']}');

    expect(produced, isTrue,
        reason: 'the `_w`-backed reader matched the nested head w(W?)');
    expect(otherwise, isTrue,
        reason: 'a constant at the head reader Out? fails, so '
            'bob_consumer\'s otherwise clause fires');
    expect(matched, isFalse,
        reason: 'ch(S?, closed) is the clean file; ch(S?, _) is mad_w_probe');
  });
}
