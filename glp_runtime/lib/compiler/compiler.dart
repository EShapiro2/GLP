import 'lexer.dart';
import 'parser.dart';
import 'analyzer.dart';
import 'codegen.dart';
import 'error.dart';
import 'token.dart';
import 'result.dart';
import '../bytecode/runner.dart' show BytecodeProgram;
import '../runtime/loaded_module.dart';
import '../runtime/service_registry.dart';

// Re-export for users of this module
export '../bytecode/runner.dart' show BytecodeProgram;
export '../runtime/loaded_module.dart' show LoadedModule;
export '../runtime/service_registry.dart' show ServiceRegistry;
export 'result.dart' show CompilationResult;
export 'ast.dart' show ModuleInfo, RemoteGoal;

/// Main GLP compiler
class GlpCompiler {
  final Lexer Function(String) _createLexer;
  final Parser Function(List<Token>) _createParser;
  final Analyzer Function() _createAnalyzer;
  final CodeGenerator Function() _createCodegen;

  GlpCompiler({
    Lexer Function(String)? createLexer,
    Parser Function(List<Token>)? createParser,
    Analyzer Function()? createAnalyzer,
    CodeGenerator Function()? createCodegen,
  })  : _createLexer = createLexer ?? ((source) => Lexer(source)),
        _createParser = createParser ?? ((tokens) => Parser(tokens)),
        _createAnalyzer = createAnalyzer ?? (() => Analyzer()),
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
      final analyzer = _createAnalyzer();
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

  /// Compile GLP source to a LoadedModule for the module system.
  ///
  /// Unlike compile() which produces a merged BytecodeProgram,
  /// this produces a LoadedModule with its own namespace.
  ///
  /// Module metadata can come from:
  /// 1. Declarations in source (-module, -export, -import)
  /// 2. Parameters (override declarations)
  LoadedModule compileModule(
    String source, {
    String? moduleName,
    Set<String>? exports,
    List<String>? imports,
  }) {
    try {
      // Phase 1: Lexical analysis
      final lexer = _createLexer(source);
      final tokens = lexer.tokenize();

      // Phase 2a: Parse module declarations
      final parser1 = _createParser(tokens);
      final moduleInfo = parser1.parseModuleInfo();

      // Phase 2b: Parse procedures (with fresh parser at start)
      final parser2 = _createParser(tokens);
      final ast = parser2.parse();

      // Phase 3: Semantic analysis
      final analyzer = _createAnalyzer();
      final annotatedAst = analyzer.analyze(ast);

      // Phase 4: Code generation to LoadedModule
      // Parameters override parsed declarations
      final codegen = _createCodegen();
      return codegen.generateModule(
        annotatedAst,
        moduleName: moduleName ?? moduleInfo.name,
        exports: exports ?? (moduleInfo.exports.isNotEmpty ? moduleInfo.exports : null),
        imports: imports ?? (moduleInfo.imports.isNotEmpty ? moduleInfo.imports : null),
      );
    } on CompileError catch (e) {
      // Rethrow with source context
      throw CompileError(e.message, e.line, e.column, source: source, phase: e.category?.toString().split('.').last);
    }
  }
}
