/// S5 — deterministic flattening + source identity h(M) (§6).
///
/// h(M) is the SHA-256 of the canonical print of the linked, *pruned* program.
/// Determinism (two builds equal), stability (editing unreachable code does not
/// change h(M) — it is pruned by DCE before printing), sensitivity (editing a
/// reachable clause does).
library;

import 'dart:io';
import 'package:glp_runtime/wire/flattening.dart';
import 'package:test/test.dart';

const _rootSelf = '../programs/self.glp';

/// A project whose exported `go/2` reaches `helper/2`; `dead/2` is unreachable.
String _src({required String helperBody, required String deadBody}) => '''
exported procedure go(Integer?, Integer).
go(X, Y?) :- helper(X?, Y).

procedure helper(Integer?, Integer).
helper(X, Y?) :- $helperBody.

procedure dead(Integer?, Integer).
dead(X, Y?) :- $deadBody.
''';

Directory _project(String source) {
  final dir = Directory.systemTemp.createTempSync('glp_flatten_');
  File('${dir.path}/main.glp').writeAsStringSync(source);
  return dir;
}

void main() {
  group('deterministic flattening + h(M)', () {
    test('determinism: two flattens of the same project give equal h(M)', () {
      final dir = _project(_src(helperBody: 'Y := X? + 1', deadBody: 'Y := X? + 99'));
      try {
        final a = flattenProject(dir.path, rootSelfGlpPath: _rootSelf);
        final b = flattenProject(dir.path, rootSelfGlpPath: _rootSelf);
        expect(a.hM, b.hM);
        expect(a.hM.length, 32);
        // The reachable procedures are printed; the unreachable one is not.
        expect(a.print, contains('helper'));
        expect(a.print, isNot(contains('dead')));
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('stability: editing unreachable code does not change h(M)', () {
      final d1 = _project(_src(helperBody: 'Y := X? + 1', deadBody: 'Y := X? + 99'));
      final d2 = _project(_src(helperBody: 'Y := X? + 1', deadBody: 'Y := X? + 12345'));
      try {
        final h1 = flattenProject(d1.path, rootSelfGlpPath: _rootSelf).hM;
        final h2 = flattenProject(d2.path, rootSelfGlpPath: _rootSelf).hM;
        expect(h2, h1, reason: 'dead/2 is pruned, so its edit is invisible to h(M)');
      } finally {
        d1.deleteSync(recursive: true);
        d2.deleteSync(recursive: true);
      }
    });

    test('sensitivity: editing a reachable clause changes h(M)', () {
      final d1 = _project(_src(helperBody: 'Y := X? + 1', deadBody: 'Y := X? + 99'));
      final d2 = _project(_src(helperBody: 'Y := X? + 2', deadBody: 'Y := X? + 99'));
      try {
        final h1 = flattenProject(d1.path, rootSelfGlpPath: _rootSelf).hM;
        final h2 = flattenProject(d2.path, rootSelfGlpPath: _rootSelf).hM;
        expect(h2, isNot(h1), reason: 'helper/2 is reachable, so its edit moves h(M)');
      } finally {
        d1.deleteSync(recursive: true);
        d2.deleteSync(recursive: true);
      }
    });
  });
}
