// glp_runtime/test/vglp/root_self_vglp_test.dart
//
// The self.vglp of the root.  Spec: vGLP, Section 6.3, sections/parts/
// canonical-compilation.tex: "the self.vglp of the root holds widget
// declarations only."  vGLP Cowork, 2026-10-16 11:25 UTC, item 2, approved by
// Udi: "The root programs/self.vglp holds widget declarations only: yes ...
// refuse a procedure there, naming the rule."

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/program_linker.dart';

void main() {
  group('the self.vglp of the root holds widget declarations only', () {
    late Directory tmp;
    void put(String rel, String text) =>
        (File('${tmp.path}/$rel')..createSync(recursive: true))
            .writeAsStringSync(text);
    setUp(() {
      // A tree of its own, so that the root's self.vglp is not programs/'s.
      tmp = Directory.systemTemp.createTempSync('vglp_root_self_vglp_');
      put('programs/self.glp', '');
      put('programs/app/self.glp',
          'exported procedure ping(Constant).\nping(a).\n');
    });
    tearDown(() => tmp.deleteSync(recursive: true));
    List<DiscoveredModule> discover() => discoverProgram(
        '${tmp.path}/programs/app',
        rootSelfGlpPath: '${tmp.path}/programs/self.glp');
    final refused = throwsA(isA<CompileError>().having(
        (e) => e.message,
        'message',
        allOf(
            contains('programs/self.vglp'),
            contains('"the self.vglp of the root holds widget declarations '
                'only" (vGLP, Section 6.3'))));

    test('widget declarations alone are accepted', () {
      put('programs/self.vglp',
          '%% the root\'s widgets\nYesNo? =::= toggle.\nCard =::= card.\n');
      expect(discover, returnsNormally);
    });

    test('a procedure is refused, naming the rule', () {
      put('programs/self.vglp', '''
YesNo? =::= toggle.
YesNo ::= yes ; no.
procedure (YesNo?)*ok(Integer?).
(yes)*ok(_).
''');
      expect(discover, refused);
    });

    test('an ordinary procedure is refused, naming the rule', () {
      put('programs/self.vglp', 'procedure p(Constant).\np(a).\n');
      expect(discover, refused);
    });

    test('a type alone is refused, naming the rule', () {
      put('programs/self.vglp', 'YesNo ::= yes ; no.\n');
      expect(discover, refused);
    });

    test('text the vGLP front end does not read is refused, naming the rule',
        () {
      put('programs/self.vglp', 'p(X) :- *(yes) | q(X?).\n');
      expect(discover, refused);
    });
  });
}
