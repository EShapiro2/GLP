/// Two isolates, `_w` carried through a channel whose Out position is an
/// unassigned writer (no UI, no mediator).
///
/// Alice MAD-sends a bare writer wrapped in w/1 to bob. Two things are pinned:
/// bob_produced, which is the `_w`-backed reader matching the nested clause
/// head w(W?) and is the admissibility the fixture exists to show; and
/// bob_ch_matched, which is bob_consumer's first clause committing once S is
/// bound: its head is ch([wrapped(Y?)|In], Out?), the form receive/3's unit
/// clause unfolds to, this fixture supplies ch(S?, _), and by GLP-Spec "Writer
/// MGU" the writer at Out is assigned the head's reader. `otherwise` does not
/// fire behind it, since it succeeds only if all previous clauses fail
/// (appendix-guards.tex), and the first clause suspends until S is bound and
/// then commits.
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
  test('_w matches the nested head; the channel head commits with a writer at Out',
      () async {
    final programs = programsDir();
    // The one program each agent runs, a module file (TGLP, def:program).
    final probe = '$programs/tests/mad_w_probe.glp';
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
    await agents.until('bob_produced and bob_ch_matched',
        () => agents.has('bob', 'bob_produced') && agents.has('bob', 'bob_ch_matched'));
    await agents.settle();
    final out = agents.out;
    final produced = out['bob']!.any((l) => l.contains('bob_produced'));
    final otherwise = out['bob']!.any((l) => l.contains('bob_ch_otherwise'));
    final matched = out['bob']!.any((l) => l.contains('bob_ch_matched'));

    // ignore: avoid_print
    print('BOB OUT: ${out['bob']}');
    // ignore: avoid_print
    print('ALICE OUT: ${out['alice']}');

    expect(produced, isTrue,
        reason: 'the `_w`-backed reader matched the nested head w(W?)');
    expect(matched, isTrue,
        reason: 'the writer at Out is assigned the head reader Out? (Writer '
            'MGU), so bob_consumer commits once S is bound');
    expect(otherwise, isFalse,
        reason: 'otherwise waits while the first clause suspends, and that '
            'clause then commits');
  });
}
