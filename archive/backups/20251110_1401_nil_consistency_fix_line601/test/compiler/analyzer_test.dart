import 'package:test/test.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/analyzer.dart';
import 'package:glp_runtime/compiler/error.dart';

void main() {
  group('Analyzer', () {
    test('analyzes simple clause with variables', () {
      final source = 'p(X, Y) :- q(X?, Y?).';  // Use readers in body
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();
      final analyzer = Analyzer();

      final annotated = analyzer.analyze(program);

      expect(annotated.procedures.length, 1);
      final proc = annotated.procedures[0];
      expect(proc.clauses.length, 1);

      final clause = proc.clauses[0];
      expect(clause.varTable.getAllVars().length, 2);

      // Check X
      final varX = clause.varTable.getVar('X');
      expect(varX, isNotNull);
      expect(varX!.writerOccurrences, 1);  // X in head
      expect(varX.readerOccurrences, 1);   // X? in body
      expect(varX.registerIndex, isNotNull);

      // Check Y
      final varY = clause.varTable.getVar('Y');
      expect(varY, isNotNull);
    });

    test('detects SRSW violation - multiple writer occurrences', () {
      final source = 'p(X, X).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();
      final analyzer = Analyzer();

      expect(
        () => analyzer.analyze(program),
        throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          contains('SRSW violation'),
        )),
      );
    });

    test('detects SRSW violation - multiple reader occurrences without ground', () {
      final source = 'p(X) :- q(X?), r(X?).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();
      final analyzer = Analyzer();

      expect(
        () => analyzer.analyze(program),
        throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          contains('SRSW violation'),
        )),
      );
    });

    test('allows multiple reader occurrences with ground guard', () {
      final source = 'p(X) :- ground(X?) | q(X?), r(X?).';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();
      final analyzer = Analyzer();

      // Should not throw
      final annotated = analyzer.analyze(program);

      final clause = annotated.procedures[0].clauses[0];
      final varX = clause.varTable.getVar('X');
      expect(varX!.readerOccurrences, 3);  // ground(X?), q(X?), r(X?)
      expect(clause.varTable.isGrounded('X'), isTrue);
    });

    test('analyzes merge/3 example', () {
      final source = '''
        merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
        merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
        merge([], [], []).
      ''';
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();
      final analyzer = Analyzer();

      final annotated = analyzer.analyze(program);

      expect(annotated.procedures.length, 1);
      expect(annotated.procedures[0].name, 'merge');
      expect(annotated.procedures[0].arity, 3);
      expect(annotated.procedures[0].clauses.length, 3);

      // Clause 1: X, Xs, Ys, Zs
      final clause1 = annotated.procedures[0].clauses[0];
      expect(clause1.varTable.getAllVars().length, 4);

      // Clause 2: Xs, Y, Ys, Zs
      final clause2 = annotated.procedures[0].clauses[1];
      expect(clause2.varTable.getAllVars().length, 4);

      // Clause 3: no variables
      final clause3 = annotated.procedures[0].clauses[2];
      expect(clause3.varTable.getAllVars().length, 0);
    });

    test('assigns register indices', () {
      final source = 'p(X, Y, Z) :- q(X?, Y?, Z?).';  // Use readers in body
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();
      final analyzer = Analyzer();

      final annotated = analyzer.analyze(program);
      final clause = annotated.procedures[0].clauses[0];

      // All variables should have unique register indices
      final vars = clause.varTable.getAllVars();
      expect(vars.length, 3);

      final indices = vars.map((v) => v.registerIndex).toSet();
      expect(indices.length, 3);  // All unique
      expect(indices.contains(0), isTrue);
      expect(indices.contains(1), isTrue);
      expect(indices.contains(2), isTrue);
    });

    test('handles nested structures', () {
      final source = 'p(f(X, g(Y))) :- q(X?, Y?).';  // Use readers in body
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();
      final analyzer = Analyzer();

      final annotated = analyzer.analyze(program);
      final clause = annotated.procedures[0].clauses[0];

      expect(clause.varTable.getAllVars().length, 2);
      expect(clause.varTable.getVar('X'), isNotNull);
      expect(clause.varTable.getVar('Y'), isNotNull);
    });

    test('handles underscore (anonymous variable)', () {
      final source = 'p(_, X) :- q(X?).';  // Use reader in body
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();
      final parser = Parser(tokens);
      final program = parser.parse();
      final analyzer = Analyzer();

      final annotated = analyzer.analyze(program);
      final clause = annotated.procedures[0].clauses[0];

      // Underscore doesn't create a variable entry
      expect(clause.varTable.getAllVars().length, 1);
      expect(clause.varTable.getVar('X'), isNotNull);
    });

    test('marks clauses with guards and body', () {
      final source1 = 'p(X).';  // No guards, no body
      final source2 = 'p(X) :- q(X?).';  // No guards, has body
      final source3 = 'p(X) :- ground(X?) | q(X?).';  // Has guards and body

      for (final (source, expectedGuards, expectedBody) in [
        (source1, false, false),
        (source2, false, true),
        (source3, true, true),
      ]) {
        final lexer = Lexer(source);
        final tokens = lexer.tokenize();
        final parser = Parser(tokens);
        final program = parser.parse();
        final analyzer = Analyzer();

        final annotated = analyzer.analyze(program);
        final clause = annotated.procedures[0].clauses[0];

        expect(clause.hasGuards, expectedGuards, reason: 'source: $source');
        expect(clause.hasBody, expectedBody, reason: 'source: $source');
      }
    });
  });
}
