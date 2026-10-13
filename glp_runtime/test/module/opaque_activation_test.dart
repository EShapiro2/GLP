/// Opaque activation: a host activates a guest it was not compiled with,
/// holding no declaration of the guest's types, by opaque_type(P/N, M, H),
/// find_type(P/N, H, Type) and run(Goal, Type, M) (GLP-Spec appendix-guards,
/// "Dynamic activation"; TGLP modules.tex, Dynamic Activation, "Opaque types"
/// and Proposition "Opaque Activation").
///
/// programs/tests/opaque/self.glp declares the host's entry template entry/2;
/// host/ is the host, and guest_a, guest_c and guest_not are guests at Move,
/// at Bid, and at no type.  A guest's module reaches the host as a file of its
/// directory, read by load_file/2, written here from the guest's compiled
/// artefact and removed after.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/module_kernels.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/artefact.dart';

const _root = '../programs/self.glp';
const _dir = '../programs/tests/opaque';

void main() {
  final modules = <String, ModuleTerm>{};
  final written = <File>[];
  late GlpEngine host;

  setUpAll(() {
    for (final g in ['guest_a', 'guest_c', 'guest_not']) {
      final engine = GlpEngine(rootSelfGlpPath: _root)
        ..loadProgram(File('$_dir/$g').absolute.path);
      final m = engine.appModule!;
      modules[g] = m;
      final f = File('$_dir/host/$g.glpm')
        ..writeAsBytesSync((m.artefact as Artefact).toBytes());
      written.add(f);
    }
    host = GlpEngine(rootSelfGlpPath: _root)
      ..loadProgram(File('$_dir/host').absolute.path);
  });

  tearDownAll(() {
    for (final f in written) {
      if (f.existsSync()) f.deleteSync();
    }
  });

  String? shown(GlpEngine engine, Term? t) {
    if (t == null) return null;
    final d = engine.runtime.heap.dereference(t);
    return d is ConstTerm ? '${d.value}' : '$d';
  }

  test('the host activates a guest at its own type, holding no declaration '
      'of it', () async {
    for (final g in ['guest_a', 'guest_c']) {
      final r = await host.runGoal("activate('$g.glpm', D)");
      expect(r.status, ExecutionStatus.succeeded, reason: '$g: ${r.error}');
      expect(shown(host, r.bindings['D']), 'done', reason: g);
    }
  });

  test('the host refuses a guest whose entry is not an instance of its '
      'template', () async {
    final r = await host.runGoal("activate('guest_not.glpm', D)");
    expect(r.status, isNot(ExecutionStatus.succeeded));
    expect(shown(host, r.bindings['D']), isNot('done'));
  });

  /// A runtime whose calling goal carries the host's module value.
  GlpRuntime inHost() {
    final rt = GlpRuntime();
    registerModuleKernels(rt);
    const goalId = 9191;
    rt.setGoalModule(goalId, host.appModule!);
    rt.currentGoalId = goalId;
    return rt;
  }

  Term ref(String name, int arity) =>
      StructTerm('/', [ConstTerm(name), ConstTerm(arity)]);

  (BodyKernelResult, Object?) call(
      GlpRuntime rt, String kernel, List<Object?> inputs) {
    final (w, _) = rt.heap.allocateVariable();
    final result =
        rt.bodyKernels.lookup(kernel, inputs.length + 1)!(rt, [...inputs, VarRef(w)]);
    final v = rt.heap.getValue(w);
    return (result, v is ConstTerm ? v.value : v);
  }

  test('opaque_type/3 gives the type the exports record; find_type/3 the '
      'identity of the opaque instance at it, which run/3 activates under',
      () {
    final rt = inHost();
    final a = modules['guest_a']!;
    final (r1, h) = call(rt, '_opaque_type', [ref('entry', 2), a]);
    expect(r1, BodyKernelResult.success);
    final record = interfaceTypeIdentityTables(
      typeDefsText: (a.artefact as Artefact).typeDefsText,
      exportDeclarationTexts:
          (a.artefact as Artefact).exports.map((e) => e.declarationText),
    ).opaque['entry/2']!;
    expect(h, record.type);

    // The host's declaration of entry/2 is its ancestor's, the key the
    // compiled module carries it under.
    final (r2, t) =
        call(rt, '_find_type', [ref('tests/opaque:entry', 2), ConstTerm(h)]);
    expect(r2, BodyKernelResult.success);
    expect(t, record.entry);

    final (w1, _) = rt.heap.allocateVariable();
    final (w2, _) = rt.heap.allocateVariable();
    final goal = StructTerm('entry', [VarRef(w1), VarRef(w2)]);
    expect(rt.bodyKernels.lookup('_run', 3)!(rt, [goal, ConstTerm(t), a]),
        BodyKernelResult.success);
    // The same identity is not guest_c's, at another type.
    expect(
        rt.bodyKernels.lookup('_run', 3)!(
            rt, [goal, ConstTerm(t), modules['guest_c']!]),
        BodyKernelResult.abort);
  });

  test('opaque_type/3 errs where the exports record no instance', () {
    final rt = inHost();
    expect(call(rt, '_opaque_type', [ref('entry', 2), modules['guest_not']!]).$1,
        BodyKernelResult.abort);
    expect(call(rt, '_opaque_type', [ref('nothing', 2), modules['guest_a']!]).$1,
        BodyKernelResult.abort);
  });

  test('find_type/3 errs where P/N is not declared in the caller\'s scope or '
      'has not exactly one type parameter', () {
    final rt = inHost();
    final h = ConstTerm('a' * 64);
    // Not declared.
    expect(call(rt, '_find_type', [ref('nothing', 2), h]).$1,
        BodyKernelResult.abort);
    // Declared, with no type parameter: the host's own activate/2.
    expect(call(rt, '_find_type', [ref('activate', 2), h]).$1,
        BodyKernelResult.abort);
    // closed/2 is the host's own parameterised declaration, with one.
    expect(call(rt, '_find_type', [ref('tests/opaque/host:closed', 2), h]).$1,
        BodyKernelResult.success);
  });
}
