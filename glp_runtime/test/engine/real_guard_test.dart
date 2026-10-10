/// The guard real/1.
///
/// GLP-Spec appendix-guards.tex (12be29b), the guard table beside number:
/// `procedure real(Real?).`, Ground yes --- it succeeds if its argument is a
/// Real, suspends on an unbound reader, and fails otherwise (GLP #3 Cowork,
/// 2026-10-02 22:19 UTC).  glp.tex, Guards: "A guard suspends if
/// it does not succeed but some instance of it under a readers substitution
/// would succeed.  A guard fails if no such instance exists."
///
/// A Real is the runtime's floating-point number, a Dart double: the lexer
/// reads a literal with a decimal point as one, and `/` and '_real' give one
/// ("Convert to float", appendix-guards.tex); TGLP types an integer literal
/// Integer or Number and a real literal Real or Number (well-typing.tex, rows 4
/// and 5).  So 2.0 is a Real and 2 is not, as integer(2.0) fails where
/// integer(2) succeeds.
///
/// It has no instruction of its own, as integer/1 has none (IGLP
/// code-format-fragment.tex, the opcode table): codegen gives it the generic
/// guard call (0x40), which the engine runs from the encoded code image.  Its
/// success grounds its argument, so X? may occur more than once after it
/// (glp.tex, Remark "Guards and SRSW"; the table's Ground column), and its
/// declaration narrows the occurrence it tests to Real? (TGLP typed-glp.tex,
/// "Type checking of guards").  The runtime cases load the fixture the suite
/// loads, programs/tests/typed/real_guard.glp (Section A12b).
library;

import 'dart:io';

import 'package:glp_runtime/analysis/type_checker/root_scope.dart'
    show isBuiltinProcedure;
import 'package:glp_runtime/bytecode/opcodes.dart' as op;
import 'package:glp_runtime/bytecode/runner.dart' show runtimeGuards;
import 'package:glp_runtime/compiler/ast.dart' as ast;
import 'package:glp_runtime/compiler/glp_printer.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/interp.dart' show codeImageFromProgram;
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

const _rootSelf = '../programs/self.glp';
const _fixture = '../programs/tests/typed/real_guard.glp';

const _ok = ExecutionStatus.succeeded;
const _fails = ExecutionStatus.failed;
const _waits = ExecutionStatus.suspended;

GlpEngine _bare() => GlpEngine(rootSelfGlpPath: File(_rootSelf).absolute.path);

GlpEngine _engine() {
  final engine = _bare();
  expect(engine.loadFile(File(_fixture).absolute.path), isTrue);
  return engine;
}

/// [t] as the REPL shows it, dereferenced throughout: `p(1.5, 1.5)`.
String _show(GlpEngine engine, Term? t) {
  if (t == null) return '<unbound>';
  final d = engine.runtime.heap.dereference(t);
  if (d is ConstTerm) return '${d.value}';
  if (d is StructTerm) {
    return '${d.functor}(${d.args.map((a) => _show(engine, a)).join(', ')})';
  }
  return '$d';
}

/// [goal] ends [status], and each of [bound] is shown as given.
void _runs(String goal, ExecutionStatus status,
    {Map<String, String> bound = const {}}) {
  test('$goal ${status.name}${bound.isEmpty ? '' : ', $bound'}', () async {
    final engine = _engine();
    final result = await engine.runGoal(goal);
    expect(result.status, status, reason: '${result.error}');
    for (final MapEntry(:key, :value) in bound.entries) {
      expect(_show(engine, result.bindings[key]), value, reason: key);
    }
  });
}

void main() {
  group('it is declared where integer/1 is', () {
    test('the root self.glp declares procedure real(Real?)', () {
      final root = File(_rootSelf).readAsStringSync();
      expect(root, contains('\nprocedure real(Real?).\n'));
    });
    test('a builtin procedure, so the root may declare it without clauses', () {
      expect(isBuiltinProcedure('real/1'), isTrue);
    });
    test('a guard the runtime evaluates', () {
      expect(runtimeGuards, contains('real/1'));
    });
  });

  group('a Real succeeds', () {
    _runs('real_or_other(2.5, R)', _ok, bound: {'R': 'real'});
    _runs('only_real(2.5, R)', _ok, bound: {'R': 'yes'});
    _runs('only_real(0.5, R)', _ok, bound: {'R': 'yes'});
  });

  group('an Integer, an atom, a string and a structure fail', () {
    for (final term in const ['2', 'hello', '"text"', 'f(1.5)', '[]']) {
      _runs('only_real($term, R)', _fails);
      _runs('real_or_other($term, R)', _ok, bound: {'R': 'other'});
    }
  });

  group('2.0 is a Real and 2 is not', () {
    _runs('only_real(2.0, R)', _ok, bound: {'R': 'yes'});
    _runs('kind_of(2, R)', _ok, bound: {'R': 'integer'});
    _runs('kind_of(2.0, R)', _ok, bound: {'R': 'real'});
    // 4 / 2 is 2.0, and '_real' converts to float.
    _runs('half(4, R)', _ok, bound: {'R': 'real'});
    _runs('half(5, R)', _ok, bound: {'R': 'real'});
    _runs('as_real(3, R)', _ok, bound: {'R': 'real'});
  });

  group('an unbound reader suspends, and resumes when it is bound', () {
    _runs('only_real(X?, R)', _waits);
    _runs('real_or_other(X?, R)', _waits);
    _runs('only_real(X?, R), X = 2.5', _ok, bound: {'R': 'yes'});
    _runs('real_or_other(X?, R), X = 2.5', _ok, bound: {'R': 'real'});
    _runs('real_or_other(X?, R), X = 2', _ok, bound: {'R': 'other'});
    _runs('in_f(f(Z?), R)', _waits);
    _runs('in_f(f(Z?), R), Z = 1.5', _ok, bound: {'R': 'yes'});
  });

  group('an unbound writer fails', () {
    // The goal's writer W: no readers substitution assigns it.
    _runs('in_f(f(W), R)', _ok, bound: {'R': 'no'});
    // A variable the clause alone holds.
    _runs('held(W, R)', _fails);
  });

  group('its success grounds its argument', () {
    // X? twice after it, run.  Under the root self.glp the guard's own
    // occurrence is a Real?, a constant type, which licenses the repeat as
    // well; the analyzer's mark alone is tested where no engine has set the
    // root scope, which is process-wide, in test/srsw_test.dart, beside
    // integer/1's.
    _runs('twice(1.5, Y)', _ok, bound: {'Y': 'p(1.5, 1.5)'});
  });

  group('its declaration types the occurrence it tests', () {
    // Number? narrowed to Real?, which takes_real/2 accepts.
    _runs('narrow(2.5, R)', _ok, bound: {'R': 'ok'});
    _runs('narrow(3, R)', _ok, bound: {'R': 'no'});

    test('an Integer? occurrence is refused at load: the meet is empty', () {
      expect(
          () => _bare().loadSource('''
procedure never(Integer?, Constant).
never(X, yes) :- real(X?) | true.
''', filename: 'real_guard_meet.glp'),
          throwsA(predicate(
              (e) =>
                  '$e'.contains('Guard real tests X?') &&
                  '$e'.contains('the meet is empty'),
              'the empty meet refused')));
    });
  });

  group('the generic guard call', () {
    test('codegen emits guard real/1, and the code image binds it by name',
        () {
      final engine = _engine();
      final programs = engine.loadedPrograms.values.toList();
      final guards = [
        for (final p in programs)
          ...p.ops
              .whereType<op.Guard>()
              .where((g) => g.procedureLabel == 'real' && g.arity == 1)
      ];
      expect(guards, isNotEmpty);
      final codeless = [
        for (final p in programs)
          if (p.ops.any((o) => o is op.Guard && o.procedureLabel == 'real'))
            ...codeImageFromProgram(p)
                .symbols
                .where((s) => !s.compiled)
                .map((s) => s.signature)
      ];
      expect(codeless, contains('real/1'));
    });

    test('the printer prints it as written, and a Real as a Real', () {
      const source = 'procedure r(_?, Constant).\n'
          'r(X, yes) :- real(X?) | true.\n';
      final module = Parser(Lexer(source).tokenize()).parseModule();
      final clause = module.procedures.single.clauses.single;
      expect(GlpPrinter().printClause(clause), 'r(X, yes) :- real(X?) | true.');
      expect(GlpPrinter().printTerm(ast.ConstTerm(2.0, 1, 1)), '2.0');
      expect(GlpPrinter().printTerm(ast.ConstTerm(2, 1, 1)), '2');
    });
  });
}
