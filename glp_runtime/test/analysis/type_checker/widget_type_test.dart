/// The root's Widget, the type of the widget in a draw.
///
/// vGLP, Section 6.3: "Widget is the type of the terms that draw widgets: the
/// terms of the default widgets, built from the constructors of Definition
/// Default Widget, and, for a declared widget, the term named(W, D) of the
/// name W its declaration gives and the default widget D of its type"; the
/// root types the person channel's draws by it, Draw(T) ::= draw(Integer,
/// Widget, T) ; withdraw(Integer) (vGLP, Definition "Person Channel, Person
/// Writer, GLP with Persons, Grant").  GLP #9 Cowork, 2026-10-20 09:22 UTC,
/// item 4.1: "the root's Widget has the alternative named(Constant, Widget)".
/// Until 2026-10-20 the root's Draw left the widget untyped, draw(Integer, _,
/// T).
///
/// Each program is written to a directory under the root, programs/ (TGLP
/// modules.tex, "Scope construction"), and loaded as the REPL loads a file,
/// which type-checks it and refuses it if it does not check.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:test/test.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;

void main() {
  late Directory dir;

  setUp(() {
    dir = Directory('../programs/tests').createTempSync('glp_widget_type_');
  });

  tearDown(() => dir.deleteSync(recursive: true));

  /// Load [source] as the file [name] in [dir].
  bool load(String name, String source) {
    final file = File('${dir.path}/$name')..writeAsStringSync(source);
    return GlpEngine(rootSelfGlpPath: _rootSelf).loadFile(file.absolute.path);
  }

  /// The load of [source] as [name] is refused by the type check.
  void refused(String name, String source) {
    expect(
        () => load(name, source),
        throwsA(predicate((e) => e.toString().contains('Type checking failed'),
            'refused by the type check')));
  }

  /// A program drawing [widget] for its person: one draw of construct 1, of
  /// the constant `dishes`, at the root's Draw(Constant).
  String drawing(String widget) =>
      'exported procedure d(Draw(Constant)).\n'
      'd(draw(1, $widget, dishes)).\n';

  group('a draw whose widget is', () {
    test('a declared widget, named(W, D), D the default widget of its type, '
        'typechecks', () {
      expect(load('named.glp', drawing('named(card, buttons([yes, no]))')),
          isTrue);
    });

    test('a declared widget of a form, its fields drawn by declared and '
        'default widgets, typechecks', () {
      expect(
          load(
              'named_form.glp',
              drawing("named(chore_card, form(chore, [field('String', shown), "
                  "field('YesNo', named(toggle, buttons([yes, no])))]))")),
          isTrue);
    });

    test('each default widget of Definition Default Widget typechecks', () {
      final widgets = [
        'button(yes)',
        'button([])',
        'button(3)',
        'buttons([yes, no])',
        'text',
        'number',
        'peer',
        "form(post, [field('String', text), question('Stream(String)', "
            "input_box(text))])",
        "menu([form(pay, [field('Integer', number)]), button(done)])",
        'shown',
        'picker',
        'input_box(number)',
        'input_list(text)',
        'thread',
        "thread(form(item, [field('String', shown)]))",
        "rec('Speech', menu([form(say, [field('String', text)]), "
            "form(sign, [field('Speech', ref('Speech'))])]))",
      ];
      for (var i = 0; i < widgets.length; i++) {
        expect(load('default_$i.glp', drawing(widgets[i])), isTrue,
            reason: widgets[i]);
      }
    });

    test('a bare name, with no default widget beside it, is refused', () {
      refused('bare.glp', drawing('card'));
    });

    test('a term that draws no widget is refused', () {
      refused('foreign.glp', drawing('named(card, slider(1, 10))'));
    });

    test('named(W, D) with W not a constant is refused', () {
      refused('name_term.glp', drawing('named(card(1), shown)'));
    });
  });
}
