/// Tests for the body kernel '_set_clock'/1 and the machine's clock (the GLP paper, appendix-guards, "Clock access (now/1, set_clock/1)"): set_clock(T) sets the clock to T and the clock runs on from T; the host that starts a machine says whether its clock may be set, and where it may not set_clock aborts.
import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

BodyKernelResult _call(GlpRuntime rt, String name, List<Object?> args) =>
    rt.bodyKernels.lookup(name, args.length)!(rt, args);

int _now(GlpRuntime rt) {
  final (w, _) = rt.heap.allocateVariable();
  expect(_call(rt, '_now', [VarRef(w)]), BodyKernelResult.success);
  return (rt.heap.getValue(w) as ConstTerm).value as int;
}

void main() {
  group('the machine\'s clock', () {
    test('now/1 reads the system clock until the clock is set', () {
      final rt = GlpRuntime();
      final before = DateTime.now().millisecondsSinceEpoch;
      final t = _now(rt);
      expect(t, greaterThanOrEqualTo(before));
      expect(t, lessThanOrEqualTo(DateTime.now().millisecondsSinceEpoch));
    });

    test('set_clock(T): now/1 gives T, and the clock runs on from it', () {
      final rt = GlpRuntime();
      const t = 946684800000;
      expect(_call(rt, '_set_clock', [ConstTerm(t)]), BodyKernelResult.success);
      final read = _now(rt);
      expect(read, greaterThanOrEqualTo(t));
      expect(read, lessThan(t + 60000));
    });

    test('set_clock aborts where the host says the clock may not be set', () {
      final rt = GlpRuntime()..clockSettable = false;
      expect(_call(rt, '_set_clock', [ConstTerm(946684800000)]),
          BodyKernelResult.abort);
      expect(_now(rt), greaterThan(1700000000000));
    });

    test('the agent of a smartphone is started with a clock that may not be set',
        () {
      final agent = AgentRuntime(
          agentId: 'alice', program: 'p', rootSelfGlpPath: 'self.glp');
      expect(agent.clockSettable, isFalse);
    });
  });
}
