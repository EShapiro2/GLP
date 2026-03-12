import 'lexer.dart';
import 'parser.dart';
import 'analyzer.dart';
import 'codegen.dart';
import 'error.dart';
import 'token.dart';
import 'result.dart';
import 'ast.dart' show Program, Procedure, Clause, Atom, Goal, Guard, Term, VarTerm, StructTerm, UnderscoreTerm, CompileMode;
import '../analysis/type_checker/type_ast.dart' show ProcDecl;
import 'package:glp_runtime/bytecode/runner.dart' show BytecodeProgram;
import '../analysis/type_checker/type_checker.dart' show checkModule;

// Re-export for users of this module
export 'package:glp_runtime/bytecode/runner.dart' show BytecodeProgram;
export 'result.dart' show CompilationResult;
export 'compiler.dart' show CompileOptions;

/// Compilation options
class CompileOptions {
  /// Enable type checking
  final bool typeCheck;

  /// Abort compilation on type errors (only applies if typeCheck is true)
  final bool strictTypes;

  const CompileOptions({
    this.typeCheck = false,
    this.strictTypes = false,
  });
}

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
  BytecodeProgram compile(String source, [CompileOptions? options]) {
    final result = compileWithMetadata(source, options);
    return result.program;
  }

  /// Compile GLP source to bytecode program with variable metadata
  CompilationResult compileWithMetadata(String source, [CompileOptions? options]) {
    final opts = options ?? const CompileOptions();
    try {
      // Phase 1: Lexical analysis
      // Note: Main lexer now handles type declarations (::= and procedure)
      final lexer = _createLexer(source);
      final tokens = lexer.tokenize();

      // Phase 2: Syntax analysis (use parseModule to get module info)
      final parser = _createParser(tokens);
      final module = parser.parseModule();

      // Convert Module to Program for analyzer
      final ast = Program(module.procedures, module.line, module.column);

      // Phase 2.4: Apply partial evaluation (defined guard expansion) BEFORE type checking
      // This transforms clauses to unfold unit clause guards, which affects coverage checking
      final partialEvaluator = PartialEvaluator();
      final transformedAst = partialEvaluator.transformDefinedGuards(ast);

      // Phase 2.5: Type checking (optional)
      if (opts.typeCheck) {
        try {
          // Use checkModule with transformed procedures
          // This ensures type checking sees the expanded guards
          final typeResult = checkModule(module, transformedProcedures: transformedAst.procedures);

          // Report type errors and warnings
          if (typeResult.errors.isNotEmpty) {
            for (final error in typeResult.errors) {
              print('[TYPE ERROR] ${error.message} at line ${error.line}');
            }
            if (opts.strictTypes) {
              throw CompileError(
                'Type checking failed with ${typeResult.errors.length} error(s)',
                typeResult.errors.first.line,
                typeResult.errors.first.column,
              );
            }
          }

          if (typeResult.warnings.isNotEmpty) {
            for (final warning in typeResult.warnings) {
              print('[TYPE WARNING] ${warning.message} at line ${warning.line}');
            }
          }
        } catch (e) {
          if (opts.strictTypes) {
            rethrow;
          }
          // In non-strict mode, just print the error and continue
          print('[TYPE CHECK] Failed: $e');
        }
      }

      // Generate reduce/2 for all files except system-mode code (stdlib)
      final generateReduce = module.compileMode != CompileMode.system;

      // Phase 3: Semantic analysis (with reduce generation flag and proc declarations)
      // Pass proc declarations for type-based SRSW relaxation
      final analyzer = _createAnalyzer();
      final annotatedAst = analyzer.analyze(
        ast,
        generateReduce: generateReduce,
        procDeclarations: module.procDeclarations,
        compileMode: module.compileMode,
      );

      // Phase 4: Code generation
      final codegen = _createCodegen();
      final result = codegen.generateWithMetadata(annotatedAst);

      return result;
    } on CompileError catch (e) {
      // Rethrow with source context
      throw CompileError(e.message, e.line, e.column, source: source, phase: e.category?.toString().split('.').last);
    }
  }

  /// Compile a Program AST directly to bytecode.
  ///
  /// Used by the project linker for statically linked programs.
  /// Skips lexing, parsing, type checking, and _select generation.
  ///
  /// [procDeclarations] should contain renamed declarations (e.g., from
  /// [linkProject]) for SRSW type-based relaxation.
  BytecodeProgram compileProgram(Program ast, {List<ProcDecl>? procDeclarations}) {
    final analyzer = _createAnalyzer();
    final annotated = analyzer.analyze(
      ast,
      generateReduce: true,
      compileMode: CompileMode.system,
      procDeclarations: procDeclarations ?? [],
      skipGlobalSRSW: true,  // Linked programs: modules already type-checked individually
    );

    final codegen = _createCodegen();
    return codegen.generateWithMetadata(annotated).program;
  }

  /// Generate _select/1 dispatch table from exported procedure declarations.
  ///
}
