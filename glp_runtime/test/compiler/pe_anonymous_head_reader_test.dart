// glp_runtime/test/compiler/pe_anonymous_head_reader_test.dart
//
// A unit clause's ANONYMOUS head reader `_?` met by the partial evaluator.
// GLP-Spec glp.tex, Remark "Anonymous Variables" (297db75), and TGLP
// "Anonymous variables" (2424f4c): "Anonymous readers (`_?`, `_Name?`) are
// not permitted in a clause, in the head or the body".  From 2026-10-02 until
// 2026-10-14 both copies of the partial evaluator's term matching ---
// partial_evaluator.dart's, which the engine and the linker run, and
// analyzer.dart's, which GlpCompiler runs --- matched a unit clause's `_?` as
// a head reader (GLP's task of 2026-10-02 08:40 UTC, B), and this file held
// that matching.  The branch is gone with the rule (GLP, 2026-10-14 09:35
// UTC, item 3), and this file holds that a program whose unit clause has
// `_?` is refused on both paths whatever calls it, the unit clause reached by
// a defined guard only among them: by the type checker on the engine's load
// path, and by codegen in GlpCompiler, which compiles without the checker.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

/// Unit clauses with `_?` at an argument, inside a structure, as a list's
/// tail, and written with a name, `_X?`.
const _units = <String, String>{
  'at an argument': 'pick2(1, _?).\n',
  'inside a structure': 'pick3(ch(_, _?)).\n',
  'as a list tail': 'pick4([1 | _?]).\n',
  'named, `_X?`': 'pick5(1, _X?).\n',
};

/// Each unit clause's declaration, for the engine's typed load path, where a
/// declaration is followed by its clauses.
const _decls = <String, String>{
  'at an argument': 'procedure pick2(Integer?, Integer).\n',
  'inside a structure': 'Ch ::= ch(Integer, Integer?).\nprocedure pick3(Ch?).\n',
  'as a list tail': 'procedure pick4(Stream(Integer)).\n',
  'named, `_X?`': 'procedure pick5(Integer?, Integer).\n',
};

/// Each unit clause's guard call, the unit clause reached by it alone.
const _guardCalls = <String, String>{
  'at an argument':
      'procedure t(Integer?).\nt(A) :- pick2(A?, B) | use(B?).\n',
  'inside a structure': 'procedure t(Integer?).\nt(A) :- pick3(ch(A?, B)) | use(B?).\n',
  'as a list tail':
      'procedure t(Integer?).\nt(_) :- pick4([1 | B]) | use(B?).\n',
  'named, `_X?`':
      'procedure t(Integer?).\nt(A) :- pick5(A?, B) | use(B?).\n',
};

const _use = 'procedure use(_?).\nuse(_).\n';

Matcher get _refused => throwsA(predicate((Object e) =>
    '$e'.contains('(anonymous reader) is not permitted in program clauses')));

void main() {
  for (final MapEntry(key: where, value: unit) in _units.entries) {
    test('a unit clause with `_?` $where is refused: GlpCompiler', () {
      expect(() => GlpCompiler().compile(unit), _refused);
    });

    test('a unit clause with `_?` $where, reached by a defined guard only, is '
        'refused: the engine', () {
      final engine = GlpEngine(
          rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      expect(
          () => engine.loadSource(
              '${_decls[where]}$unit$_use${_guardCalls[where]}'),
          _refused);
    });
  }
}
