/// The single-file path checks the object it compiles.
///
/// TGLP modules.tex, Compilation: "The flat program is the linked program of
/// def:program, and it is the object checked"; claude.md, "Code": the same
/// object is type-checked and then compiled.  A file loaded alone
/// (GlpEngine.loadSource on a real file) is linked as a one-module program,
/// and that linked program is checked as a directory program's is
/// (checkedLinkedProgram): each module of it against its scope, then the flat
/// program.  Until 2026-10-03 the single-file path checked the file's module
/// alone and compiled the linked program unchecked (GLP #3 Cowork, 2026-10-03
/// 21:18 UTC, "16:01. 4": "faults, fix them").
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

GlpEngine _engine() => GlpEngine(
    rootSelfGlpPath: File('../programs/self.glp').absolute.path);

String _abs(String p) => File(p).absolute.path;

void main() {
  test('a file whose linked program does not check is refused', () {
    // main.glp's go/3 calls the root-exposed send_user/3 at an entry union
    // with no user_output: its module checks alone, the call's instantiation
    // does not (the directory program is refused alike, harness X7).
    expect(
        () => _engine()
            .loadFile(_abs('../programs/tests/a5_routing_neg/main.glp')),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('Type checking failed for linked program') &&
              s.contains('user_output');
        }, 'the linked program refused at send_user/3')));
  });

  test("each module of the file's program is checked, its self.glp among them",
      () {
    expect(
        () => _engine().loadFile(
            _abs('../programs/tests/module_self_type_error/worker.glp')),
        throwsA(predicate(
            (e) => e
                .toString()
                .contains('module_self_type_error/self.glp:3: Head of bad_proc'),
            'self.glp:3 refused as a module of the program')));
  });

  test('a file whose linked program checks loads and runs', () async {
    final engine = _engine();
    expect(
        engine.loadFile(_abs('../programs/tests/typed/operator_names.glp')),
        isTrue);
    final r = await engine.runGoal('arith(A, B, C).');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(r.bindings['C'].toString(), 'Const(14)');
  });
}
