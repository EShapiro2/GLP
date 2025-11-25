import 'lexer.dart';
import 'parser.dart';
import 'analyzer.dart';
import 'codegen.dart';
import 'error.dart';
import 'token.dart';
import 'result.dart';
import '../bytecode/runner.dart' show BytecodeProgram;

// Re-export for users of this module
export '../bytecode/runner.dart' show BytecodeProgram;
export 'result.dart' show CompilationResult;

/// Main GLP compiler
class GlpCompiler {
  final Lexer Function(String) _createLexer;
  final Parser Function(List<Token>) _createParser;
  final Analyzer Function({bool skipSRSW}) _createAnalyzer;
  final CodeGenerator Function() _createCodegen;
  final bool skipSRSW;

  GlpCompiler({
    Lexer Function(String)? createLexer,
    Parser Function(List<Token>)? createParser,
    Analyzer Function({bool skipSRSW})? createAnalyzer,
    CodeGenerator Function()? createCodegen,
    this.skipSRSW = false,
  })  : _createLexer = createLexer ?? ((source) => Lexer(source)),
        _createParser = createParser ?? ((tokens) => Parser(tokens)),
        _createAnalyzer = createAnalyzer ?? (({bool skipSRSW = false}) => Analyzer(skipSRSW: skipSRSW)),
        _createCodegen = createCodegen ?? (() => CodeGenerator());

  /// Compile GLP source to bytecode program
  BytecodeProgram compile(String source) {
    final result = compileWithMetadata(source);
    return result.program;
  }

  /// Compile GLP source to bytecode program with variable metadata
  CompilationResult compileWithMetadata(String source) {
    try {
      // Phase 1: Lexical analysis
      final lexer = _createLexer(source);
      final tokens = lexer.tokenize();

      // Phase 2: Syntax analysis
      final parser = _createParser(tokens);
      final ast = parser.parse();

      // Phase 3: Semantic analysis
      final analyzer = _createAnalyzer(skipSRSW: skipSRSW);
      final annotatedAst = analyzer.analyze(ast);

      // Phase 4: Code generation
      final codegen = _createCodegen();
      final result = codegen.generateWithMetadata(annotatedAst);

      return result;
    } on CompileError catch (e) {
      // Rethrow with source context
      throw CompileError(e.message, e.line, e.column, source: source, phase: e.category?.toString().split('.').last);
    }
  }
}
