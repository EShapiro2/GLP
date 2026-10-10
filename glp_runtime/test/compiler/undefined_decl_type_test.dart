/// An undefined type name in a declaration and not in its parameter list is
/// an error, whether or not the declaration has a list.
///
/// TGLP parameterized-types.tex, paragraph "Declaration parameters": "The
/// parameters of a procedure declaration are exactly those its parameter list
/// names.  An undefined type name occurring in a declaration and not in its
/// parameter list is an error, so a misspelt type name is rejected rather than
/// read as a parameter."  Until 2026-10-03 a declaration naming no parameters
/// fell back to reading such a name as a parameter ($abstract_<Name>),
/// transitional until the tree's declarations carried their lists; it went
/// once sGLP's Peers was defined (GLP #3 Cowork, 2026-10-03 21:18 UTC, "15:15.
/// 5": "compliance; remove the fallback once sGLP's `Peers` lands").  The
/// refusal names the declaration, the file, the line and the type name.
library;

import 'dart:io';

import 'package:glp_runtime/analysis/type_checker/param_expansion.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _rootSelf = '../programs/self.glp';

GlpEngine _engine() =>
    GlpEngine(rootSelfGlpPath: File(_rootSelf).absolute.path);

/// [body] throws, and its message carries every one of [parts].
void _refusedNaming(void Function() body, List<String> parts) {
  expect(
      body,
      throwsA(predicate((e) {
        final s = e.toString();
        return parts.every(s.contains);
      }, 'a refusal naming ${parts.join(', ')}')));
}

/// A program directory under the root, programs/tests/ (TGLP modules.tex,
/// "Scope construction": "A program lies at or below the root"), written for
/// one test and removed after it.
Directory _programDir(Map<String, String> files) {
  final dir = Directory('../programs/tests').createTempSync('decl_undefined_');
  for (final e in files.entries) {
    File('${dir.path}/${e.key}').writeAsStringSync(e.value);
  }
  return dir;
}

void main() {
  group('the expansion', () {
    test('a declaration with no list and an undefined name is refused', () {
      final m = Parser(Lexer('procedure keep(M?, M).\nkeep(X, X?).\n')
              .tokenize())
          .parseModule();
      expect(
          () => expandParameterizedTypes(m),
          throwsA(isA<UndefinedDeclarationTypeError>()
              .having((e) => e.typeName, 'typeName', 'M')
              .having((e) => e.procedure, 'procedure', 'keep')
              .having((e) => e.arity, 'arity', 2)
              .having((e) => e.line, 'line', 1)
              .having((e) => e.typeParams, 'typeParams', isEmpty)));
    });

    test('with the parameter named, the same declaration is accepted', () {
      final m = Parser(Lexer('procedure(M) keep(M?, M).\nkeep(X, X?).\n')
              .tokenize())
          .parseModule();
      expect(() => expandParameterizedTypes(m), returnsNormally);
    });
  });

  group('the loaders name the file', () {
    test('a single file: the file, the line, the declaration and the name', () {
      final path =
          File('../programs/tests/decl_undefined_neg.glp').absolute.path;
      _refusedNaming(() => _engine().loadFile(path),
          [path, 'line 16', 'undefined type "M"', 'keep/2']);
    });

    test('a module of a directory program: its file and line', () {
      final dir =
          Directory('../programs/tests/decl_undefined_dir_neg').absolute.path;
      _refusedNaming(() => _engine().loadProgram(dir),
          ['$dir/worker.glp:4', 'undefined type "Mesage"', 'relay/2']);
    });

    test('a self.glp of a directory program: its file and line', () {
      final dir = _programDir({
        'self.glp': 'exported procedure go(Widget).\ngo(_).\n',
      });
      try {
        _refusedNaming(() => _engine().loadProgram(dir.absolute.path),
            ['self.glp:1', 'undefined type "Widget"', 'go/1']);
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('the directory program spelt right loads and runs', () async {
      final dir = _programDir({
        'self.glp': File('../programs/tests/decl_undefined_dir_neg/self.glp')
            .readAsStringSync(),
        'worker.glp': File('../programs/tests/decl_undefined_dir_neg/worker.glp')
            .readAsStringSync()
            .replaceAll('Mesage', 'Message'),
      });
      try {
        final engine = _engine();
        expect(engine.loadProgram(dir.absolute.path), isTrue);
        final r = await engine.runGoal('go([hello, bye], Out).');
        expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      } finally {
        dir.deleteSync(recursive: true);
      }
    });
  });
}
