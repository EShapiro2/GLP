import 'lexer.dart';
import 'parser.dart';
import 'analyzer.dart';
import 'codegen.dart';
import 'error.dart';
import 'ast.dart' show Program;
import '../analysis/type_checker/type_ast.dart' show ProcDecl, TypeEnvironment;
import '../analysis/type_checker/type_checker.dart' show buildModuleTypeEnvironment;
import 'package:glp_runtime/bytecode/runner.dart' show BytecodeProgram;

// Re-export for users of this module
export 'package:glp_runtime/bytecode/runner.dart' show BytecodeProgram;

/// Main GLP compiler
///
/// The compiler takes no options.  Until 2026-09-20 a `CompileOptions` carried
/// `typeCheck` and `strictTypes`, and the block they guarded ran the module
/// check here, printed `[TYPE ERROR]` per error and carried on whenever
/// `strictTypes` was off; nothing anywhere constructed the options with
/// `typeCheck` true, so the block never ran either.  The same object is
/// typechecked and then compiled, and no diagnostic on a load path is a
/// warning: a program that does not check does not run (Coordination #1,
/// 2026-09-18).  The block is therefore deleted rather than repaired, and the
/// options with it; the loaders check the module and refuse it, and a caller
/// of this compiler gets no switch that would let a compiled object go
/// unchecked.
class GlpCompiler {
  /// Compile GLP source to bytecode program.
  ///
  /// [typeEnv] is the scope the source was type-checked in.  The SRSW
  /// relaxations of a typed program are decided on the type each occurrence has
  /// (TGLP typed-glp.tex, "Readers of ground types"), so the analyzer is given
  /// the same scope the checker used, which is also the scope whose defined
  /// guards it unfolds.  With none, the source's own declarations are built
  /// here over [ancestorScope], the scope passed in, and over the language
  /// primitives alone where none is (TGLP Definition "Root, Scope", Π): the
  /// root self.glp, compiled on its own, is checked in that.  Until 2026-10-04
  /// the default was a root scope set once for the whole process.
  BytecodeProgram compile(String source,
      {TypeEnvironment? typeEnv, TypeEnvironment? ancestorScope}) {
    try {
      // Phase 1: Lexical analysis
      final lexer = Lexer(source);
      final tokens = lexer.tokenize();

      // Phase 2: Syntax analysis (use parseModule to get module info)
      final parser = Parser(tokens);
      final module = parser.parseModule();

      // Convert Module to Program for analyzer
      final ast = Program(module.procedures, module.line, module.column);

      // Phase 3: Semantic analysis.  Pass proc declarations for type-based SRSW
      // relaxation.
      final analyzer = Analyzer();
      final annotatedAst = analyzer.analyze(
        ast,
        procDeclarations: module.procDeclarations,
        typeEnv: typeEnv ??
            buildModuleTypeEnvironment(module, ancestorScope: ancestorScope),
      );

      // Phase 4: Code generation
      final codegen = CodeGenerator();
      return codegen.generate(annotatedAst);
    } on CompileError catch (e) {
      // Rethrow with source context, and with the error's category.  The
      // category's name was passed as the phase until 2026-10-02 ('lexical',
      // 'syntax', 'semantic'), which CompileError does not map, so a lexical,
      // syntax or semantic error from the compiler came back with none.
      throw CompileError(e.message, e.line, e.column, source: source, category: e.category);
    }
  }

  /// Compile a Program AST directly to bytecode.
  ///
  /// Used by the program linker for statically linked programs.
  /// Skips lexing, parsing, type checking, and _select generation.
  ///
  /// [procDeclarations] should contain renamed declarations (e.g., from
  /// [linkProgram]).  [typeEnv] is the scope the linked program was checked in
  /// (the flat module's, [linkedProgramEnvironment]): the SRSW relaxations of a
  /// typed program are decided on the type each occurrence has, so the analyzer
  /// is given the same scope the checker used.
  ///
  /// The flat program is the object checked, and every clause in it satisfies
  /// SRSW, the linker's alias clauses included (TGLP modules.tex §Compilation).
  /// The analyzer's SRSW pass runs here as it does for a single-module program;
  /// until 2026-09-18 a `skipGlobalSRSW` flag defaulted to skipping it for a
  /// linked program, and nothing else performed the check, so a directory
  /// program was compiled and run with no SRSW check at all.
  BytecodeProgram compileProgram(Program ast,
      {List<ProcDecl>? procDeclarations, TypeEnvironment? typeEnv}) {
    final analyzer = Analyzer();
    final annotated = analyzer.analyze(
      ast,
      procDeclarations: procDeclarations ?? [],
      typeEnv: typeEnv,
    );

    final codegen = CodeGenerator();
    return codegen.generate(annotated);
  }
}
