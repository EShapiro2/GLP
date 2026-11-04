/// Tests for the body kernels '_start_clock'/1, '_rated'/2 and '_step_clock'/2,
/// behind start_clock/1, rated/2 and step_clock/2.
///
/// Spec: GLP-ICLP appendix-guards, "Simulated time (start_clock/1, rated/2,
/// step_clock/2)" (GLP-ICLP-2026 5fd9a15).  The pinned times were computed
/// from the paper's sentences in Python, independently of this code.
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;

BodyKernelResult _call(GlpRuntime rt, String name, List<Object?> args) =>
    rt.bodyKernels.lookup(name, args.length)!(rt, args);

HeapCell _rated(GlpRuntime rt, num rate) {
  final (w, _) = rt.heap.allocateVariable();
  expect(_call(rt, '_rated', [ConstTerm(rate), VarRef(w)]),
      BodyKernelResult.success);
  return w;
}

/// One step with [horizon]: the Step term assigned.
StructTerm _step(GlpRuntime rt, num horizon) {
  final (w, _) = rt.heap.allocateVariable();
  expect(_call(rt, '_step_clock', [ConstTerm(horizon), VarRef(w)]),
      BodyKernelResult.success);
  return rt.heap.getValue(w) as StructTerm;
}

String _show(Object? v) => v is StructTerm
    ? '${v.functor}(${(v.args.single as ConstTerm).value})'
    : (v as ConstTerm).value.toString();

void main() {
  group('simulated time kernels', () {
    test('rated and step_clock abort before the first start_clock', () {
      final rt = GlpRuntime();
      final (w, _) = rt.heap.allocateVariable();
      expect(_call(rt, '_rated', [ConstTerm(1), VarRef(w)]),
          BodyKernelResult.abort);
      final (s, _) = rt.heap.allocateVariable();
      expect(_call(rt, '_step_clock', [ConstTerm(5), VarRef(s)]),
          BodyKernelResult.abort);
    });

    test('seed 42, rates 1, 2 and 0.5: the reference releases, then stopped',
        () {
      final rt = GlpRuntime();
      expect(_call(rt, '_start_clock', [ConstTerm(42)]),
          BodyKernelResult.success);
      final a = _rated(rt, 1), b = _rated(rt, 2), c = _rated(rt, 0.5);
      expect(_show(_step(rt, 1000)), 'released(1.522941750012381)');
      expect(_show(rt.heap.getValue(c)), 'go(1.522941750012381)');
      expect(_show(_step(rt, 1000)), 'released(1.7360415557214341)');
      expect(_show(rt.heap.getValue(a)), 'go(1.7360415557214341)');
      expect(_show(_step(rt, 1000)), 'released(1.7652128941799705)');
      expect(_show(rt.heap.getValue(b)), 'go(1.7652128941799705)');
      expect(_show(_step(rt, 1000)), 'stopped(1.7652128941799705)');
    });

    test('the horizon: tokens past it are bound to stop, Step stopped(C)', () {
      final rt = GlpRuntime();
      _call(rt, '_start_clock', [ConstTerm(7)]);
      final a = _rated(rt, 1), b = _rated(rt, 1), c = _rated(rt, 1);
      expect(_show(_step(rt, 0.5)), 'released(0.32910293578974764)');
      expect(_show(_step(rt, 0.5)), 'stopped(0.32910293578974764)');
      expect(_show(rt.heap.getValue(a)), 'stop');
      expect(_show(rt.heap.getValue(b)), 'stop');
      expect(_show(rt.heap.getValue(c)), 'go(0.32910293578974764)');
      expect(rt.simClock.pendingCount, 0);
    });

    test('a tie is released in the order made pending', () {
      final rt = GlpRuntime();
      _call(rt, '_start_clock', [ConstTerm(3)]);
      _rated(rt, 0.001);
      final t0 = (_step(rt, 1e9).args.single as ConstTerm).value as double;
      final x = _rated(rt, 1000000000000000000);
      final y = _rated(rt, 1000000000000000000);
      expect(_show(_step(rt, 1e9)), 'released($t0)');
      expect(_show(rt.heap.getValue(x)), 'go($t0)');
      expect(rt.heap.getValue(y), isNull);
      expect(_show(_step(rt, 1e9)), 'released($t0)');
      expect(_show(rt.heap.getValue(y)), 'go($t0)');
    });

    test('start_clock binds every pending token to stop and resets the clock',
        () {
      final rt = GlpRuntime();
      _call(rt, '_start_clock', [ConstTerm(5)]);
      final a = _rated(rt, 1);
      _call(rt, '_start_clock', [ConstTerm(42)]);
      expect(_show(rt.heap.getValue(a)), 'stop');
      expect(rt.simClock.clock, 0.0);
      _rated(rt, 1);
      _rated(rt, 2);
      _rated(rt, 0.5);
      expect(_show(_step(rt, 1000)), 'released(1.522941750012381)');
    });
  });
}
