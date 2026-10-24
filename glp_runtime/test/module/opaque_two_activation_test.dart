/// Opaque activation at a template of two type parameters.
///
/// opaque_type(P/N, M, Hs) gives the list of type identities, one per type
/// parameter of the template in declaration order, and find_type(P/N, Hs,
/// Type) takes it, erring where the length of Hs is not the number of the
/// declaration's type parameters; a guest's exports record
/// the identity of D⟨ι(T1), ..., ι(Tk)⟩ and the list ι(T1), ..., ι(Tk) (GLP-Spec
/// appendix-guards, "Dynamic activation"; TGLP modules.tex, Dynamic
/// Activation, "Opaque types" and Proposition "Opaque Activation").
///
/// programs/tests/opaque_two/self.glp declares the host's template
/// procedure(D, T) t(PersonChannel(D)?, SgChannel(T)?); guest/ is its
/// instance at Colour and Note; host/ activates it, holding no declaration of
/// either, the channels it hands over at Opaque.  The guest's module reaches
/// the host as a file of its directory, read by load_file/2, written here from
/// the guest's compiled artefact and removed after.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart'
    show TypeRef;
import 'package:glp_runtime/analysis/type_checker/type_checker.dart'
    show buildModuleTypeEnvironment;
import 'package:glp_runtime/analysis/type_checker/type_identity.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/module_kernels.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart' show rootScope;
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/artefact.dart';

const _root = '../programs/self.glp';
const _dir = '../programs/tests/opaque_two';

void main() {
  late ModuleTerm guest;
  late GlpEngine host;
  late File written;

  setUpAll(() {
    final engine = GlpEngine(rootSelfGlpPath: _root)
      ..loadProgram(File('$_dir/guest').absolute.path);
    guest = engine.appModule!;
    written = File('$_dir/host/guest.glpm')
      ..writeAsBytesSync((guest.artefact as Artefact).toBytes());
    host = GlpEngine(rootSelfGlpPath: _root)
      ..loadProgram(File('$_dir/host').absolute.path);
  });

  tearDownAll(() {
    if (written.existsSync()) written.deleteSync();
  });

  GlpRuntime inHost() {
    final rt = GlpRuntime();
    registerModuleKernels(rt);
    const goalId = 9292;
    rt.setGoalModule(goalId, host.appModule!);
    rt.currentGoalId = goalId;
    return rt;
  }

  Term ref(String name, int arity) =>
      StructTerm('/', [ConstTerm(name), ConstTerm(arity)]);

  (BodyKernelResult, Object?) call(
      GlpRuntime rt, String kernel, List<Object?> inputs) {
    final (w, _) = rt.heap.allocateVariable();
    final result = rt.bodyKernels.lookup(kernel, inputs.length + 1)!(
        rt, [...inputs, VarRef(w)]);
    final v = rt.heap.getValue(w);
    return (result, v is ConstTerm ? v.value : v);
  }

  Term list(List<String> ids) {
    Term t = ConstTerm(nil);
    for (final id in ids.reversed) {
      t = StructTerm('.', [ConstTerm(id), t]);
    }
    return t;
  }

  List<Object?> values(GlpRuntime rt, Object? t) {
    final out = <Object?>[];
    var c = t;
    while (c is StructTerm && c.functor == '.') {
      final h = rt.heap.dereference(c.args[0]);
      out.add(h is ConstTerm ? h.value : h);
      c = rt.heap.dereference(c.args[1]);
    }
    return out;
  }

  test('the guest\'s exports record two identities, one per parameter, and '
      'the identity of the instance at both', () {
    final record = interfaceTypeIdentityTables(
      typeDefsText: (guest.artefact as Artefact).typeDefsText,
      exportDeclarationTexts:
          (guest.artefact as Artefact).exports.map((e) => e.declarationText),
    ).opaque['t/2'];
    expect(record, isNotNull);
    expect(record!.types, hasLength(2));
    expect(record.types[0], isNot(record.types[1]));
  });

  test('the list is in the order of the template\'s parameters: iota(Colour) '
      'at D, the first, and iota(Note) at T, the second', () {
    final record = interfaceTypeIdentityTables(
      typeDefsText: (guest.artefact as Artefact).typeDefsText,
      exportDeclarationTexts:
          (guest.artefact as Artefact).exports.map((e) => e.declarationText),
    ).opaque['t/2']!;
    final env = buildModuleTypeEnvironment(
        Parser(Lexer(File('$_dir/guest/self.glp').readAsStringSync())
                .tokenize())
            .parseModule(),
        ancestorScope: rootScope(File(_root).absolute.path));
    String iota(String t) =>
        typeIdentityOfTypeHex(TypeRef(t, 0, 0), env.types);
    expect(record.types, [iota('Colour'), iota('Note')]);
  });

  test('the host activates the guest at its two types by opaque_type/3, '
      'find_type/3 and run/3, holding no declaration of either', () async {
    // P and S are the ends the host holds at Opaque; the guest's t/2 reads
    // neither, so they stay unbound.
    final r = await host.runGoal("activate('guest.glpm', P?, S?, D)");
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    final d = r.bindings['D'];
    expect(d == null ? null : host.runtime.heap.dereference(d),
        isA<ConstTerm>().having((c) => c.value, 'value', 'done'));
  });

  test('the host, holding the channels at Opaque, loads: the step back admits '
      'each at the position of its own parameter', () {
    expect(host.appModule, isNotNull);
  });

  test('opaque_type/3 gives the list of two; find_type/3 the identity of the '
      'opaque instance at them, which run/3 activates the guest under', () {
    final rt = inHost();
    final (r1, hs) = call(rt, '_opaque_type', [ref('t', 2), guest]);
    expect(r1, BodyKernelResult.success);
    final ids = values(rt, hs).cast<String>();
    expect(ids, hasLength(2));

    final (r2, t) =
        call(rt, '_find_type', [ref('tests/opaque_two:t', 2), list(ids)]);
    expect(r2, BodyKernelResult.success);

    final (w1, _) = rt.heap.allocateVariable();
    final (w2, _) = rt.heap.allocateVariable();
    final goal = StructTerm('t', [VarRef(w1), VarRef(w2)]);
    expect(rt.bodyKernels.lookup('_run', 3)!(rt, [goal, ConstTerm(t), guest]),
        BodyKernelResult.success);

    // The two identities in the other order name another instance, which
    // the guest's exports do not record.
    final (r3, swapped) = call(rt, '_find_type',
        [ref('tests/opaque_two:t', 2), list(ids.reversed.toList())]);
    expect(r3, BodyKernelResult.success);
    expect(swapped, isNot(t));
    expect(
        rt.bodyKernels.lookup('_run', 3)!(
            rt, [goal, ConstTerm(swapped), guest]),
        BodyKernelResult.abort);

    // One identity for a template of two is refused.
    expect(
        call(rt, '_find_type', [ref('tests/opaque_two:t', 2), list([ids[0]])])
            .$1,
        BodyKernelResult.abort);
  });

  test('find_type/3 errs where the length of Hs is not the number of the '
      'declaration\'s type parameters, two', () {
    final rt = inHost();
    final t = ref('tests/opaque_two:t', 2);
    final h = 'a' * 64, g = 'b' * 64;
    expect(call(rt, '_find_type', [t, list([h, g])]).$1,
        BodyKernelResult.success);
    for (final hs in [
      <String>[],
      [h],
      [h, g, h],
    ]) {
      final (r, v) = call(rt, '_find_type', [t, list(hs)]);
      expect(r, BodyKernelResult.abort, reason: '${hs.length} identities');
      expect(v, isNot(isA<String>()), reason: 'Type is left unassigned');
    }
  });
}
