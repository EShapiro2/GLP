/// Boot Loader for maGLP Isolate Spawning
///
/// Reads a GLP file's `boot/0` procedure, whose body goals `G@p` are its spawn
/// directives (IGLP app:in-execution, Boot), with the GLP lexer and parser.
library;

import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/token.dart';

/// A single spawn directive extracted from the boot clause.
///
/// Represents `goalFunctor(agentId, ...)@agentId`
///
/// For `parent_init(alice, carol, 4, _)@alice`:
///   agentId = 'alice', goalFunctor = 'parent_init', goalArity = 4,
///   constantArgs = ['carol', '4']
///
/// The isolate entry point always passes: arg0 = agentId (constant),
/// args 1..n-2 = constantArgs, arg n-1 = netInReader.
class SpawnDirective {
  /// The agent identifier (e.g., 'alice', 'bob')
  final String agentId;

  /// The goal functor to spawn (e.g., 'agent_init', 'parent_init')
  final String goalFunctor;

  /// The arity of the goal (e.g., 2 for agent_init/2, 4 for parent_init/4)
  final int goalArity;

  /// Constant arguments between agentId and the final netIn variable.
  /// For `parent_init(alice, carol, 4, _)@alice`: ['carol', '4']
  /// For `agent_init(alice, _)@alice`: [] (empty)
  final List<String> constantArgs;

  SpawnDirective({
    required this.agentId,
    required this.goalFunctor,
    required this.goalArity,
    this.constantArgs = const [],
  });

  @override
  String toString() => 'SpawnDirective($goalFunctor/$goalArity($agentId, ...)@$agentId)';
}

/// Configuration extracted from a GLP boot file.
class BootConfig {
  /// The spawn directives from the boot clause
  final List<SpawnDirective> directives;

  /// The full source code (original, including boot clause)
  final String fullSource;

  /// Source code with boot clause stripped (for GLP compilation)
  /// The boot clause contains @ which the GLP parser doesn't understand.
  final String source;

  /// Optional program directory for static linking.
  /// When set, each isolate loads the program via loadProgram() instead of
  /// loading individual shared sources. The boot source is loaded on top.
  String? programDir;

  /// Absolute path to programs/self.glp
  String rootSelfGlpPath;

  /// The boot file's path, where it was loaded from one ([BootLoader.loadFile])
  /// or the caller knows it. The boot source is checked in the linked
  /// program's entry points and the boot file's ancestor chain of self.glp
  /// declarations (IGLP, Implementation Notes, "The scope a boot source is
  /// checked in", 8aafd09), and the chain is discovered from this path;
  /// without it the boot source is checked as a module at the root, its chain
  /// the root self.glp alone.
  String? bootPath;

  BootConfig({
    required this.directives,
    required this.fullSource,
    required this.source,
    this.programDir,
    this.rootSelfGlpPath = '',
    this.bootPath,
  });
}

/// Loader for GLP files with isolate boot clauses.
///
/// The GLP lexer and parser read the boot procedure --- the declaration
/// `procedure boot.` and the clause `boot :- G1@p1, ..., Gn@pn.` --- and the
/// spawn directives are taken from the clause's spawn goals; the declaration
/// and the clause are then blanked out of the source, their lines kept, so the
/// rest of the file compiles as an ordinary module with the line numbers it
/// has (IGLP app:in-execution, Boot).
class BootLoader {
  /// Load a GLP file and extract boot configuration.
  ///
  /// Throws [BootLoaderException] if:
  /// - File doesn't declare `procedure boot.`
  /// - Boot clause is missing or malformed
  /// - Agent IDs don't match between goal and @target
  /// - Duplicate agent IDs
  BootConfig load(String source) {
    final List<Token> tokens;
    try {
      tokens = Lexer(source).tokenize();
    } on CompileError catch (e) {
      throw BootLoaderException(
          'Boot file: ${e.message} at line ${e.line}, column ${e.column}');
    }

    final declaration = _bootDeclaration(tokens);
    if (declaration == null) {
      throw BootLoaderException('First procedure must be boot/0. '
          'Expected "procedure boot." declaration.');
    }
    final clause = _bootClause(tokens);
    if (clause == null) {
      throw BootLoaderException('Could not find boot clause. '
          'Expected "boot :- ... ."');
    }

    final directives = _spawnDirectives(tokens, declaration, clause);
    return BootConfig(
      directives: directives,
      fullSource: source,
      source: _blankOut(source, tokens, [declaration, clause]),
    );
  }

  /// Load from file path (convenience method)
  BootConfig loadFile(String filePath) {
    final file = _readFile(filePath);
    return load(file)..bootPath = filePath;
  }

  /// The tokens `procedure boot .`, as (first, last) indices.
  (int, int)? _bootDeclaration(List<Token> tokens) {
    for (var i = 0; i + 2 < tokens.length; i++) {
      if (tokens[i].type == TokenType.PROCEDURE &&
          tokens[i + 1].type == TokenType.ATOM &&
          tokens[i + 1].lexeme == 'boot' &&
          tokens[i + 2].type == TokenType.DOT) {
        return (i, i + 2);
      }
    }
    return null;
  }

  /// The clause `boot :- ... .`, as (first, last) indices: an item beginning
  /// `boot :-` and ending at the next full stop outside brackets.
  (int, int)? _bootClause(List<Token> tokens) {
    var depth = 0;
    var atItemStart = true;
    int? start;
    for (var i = 0; i < tokens.length; i++) {
      final t = tokens[i];
      if (t.type == TokenType.EOF) break;
      if (atItemStart && depth == 0 && start == null &&
          t.type == TokenType.ATOM &&
          t.lexeme == 'boot' &&
          i + 1 < tokens.length &&
          tokens[i + 1].type == TokenType.IMPLIES) {
        start = i;
      }
      atItemStart = false;
      if (t.type == TokenType.LPAREN || t.type == TokenType.LBRACKET) {
        depth++;
      } else if (t.type == TokenType.RPAREN || t.type == TokenType.RBRACKET) {
        if (depth > 0) depth--;
      } else if (t.type == TokenType.DOT && depth == 0) {
        if (start != null) return (start, i);
        atItemStart = true;
      }
    }
    return null;
  }

  /// The spawn directives of the boot clause: each spawn goal `G@p` whose goal
  /// takes the agent ID `p` first and the network input last, the arguments
  /// between them constants.
  List<SpawnDirective> _spawnDirectives(
      List<Token> tokens, (int, int) declaration, (int, int) clause) {
    final Module module;
    try {
      module = Parser([
        ...tokens.sublist(declaration.$1, declaration.$2 + 1),
        ...tokens.sublist(clause.$1, clause.$2 + 1),
        Token(TokenType.EOF, '', tokens[clause.$2].line,
            tokens[clause.$2].column + 1),
      ]).parseModule();
    } on CompileError catch (e) {
      throw BootLoaderException(
          'Boot clause: ${e.message} at line ${e.line}, column ${e.column}');
    }
    final body = module.procedures.single.clauses.single.body ?? const <Goal>[];

    final directives = <SpawnDirective>[];
    final agentIds = <String>{};
    for (final goal in body) {
      if (goal is! SpawnGoal) continue;
      final inner = goal.innerGoal;
      if (inner.args.isEmpty) continue;

      final goalAgentId = _agentIdText(inner.args.first);
      if (goalAgentId == null) {
        throw BootLoaderException(
            'First argument of spawn goal must be an agent ID (atom), '
            'got "${inner.args.first}"');
      }
      if (goalAgentId != goal.agentId) {
        throw BootLoaderException(
            'Agent ID mismatch: goal has "$goalAgentId" but @target is '
            '"${goal.agentId}". They must match.');
      }
      if (!agentIds.add(goalAgentId)) {
        throw BootLoaderException('Duplicate agent ID: $goalAgentId');
      }

      // Middle args (between agentId and last arg which is netIn) are
      // constants. For parent_init(alice, carol, 4, _)@alice: ['carol', '4'].
      final constantArgs = <String>[];
      for (var i = 1; i < inner.args.length - 1; i++) {
        final arg = inner.args[i];
        if (arg is! ConstTerm) {
          throw BootLoaderException(
              'Argument ${i + 1} of spawn goal ${inner.functor} must be a '
              'constant, got "$arg"');
        }
        constantArgs.add('${arg.value}');
      }

      directives.add(SpawnDirective(
        agentId: goalAgentId,
        goalFunctor: inner.functor,
        goalArity: inner.args.length,
        constantArgs: constantArgs,
      ));
    }

    if (directives.isEmpty) {
      throw BootLoaderException('Boot clause contains no spawn directives. '
          'Expected "goal(agent, _)@agent"');
    }
    return directives;
  }

  /// An agent ID, the name of an atom or a numeral; null for anything else.
  String? _agentIdText(Term term) {
    if (term is! ConstTerm) return null;
    final value = term.value;
    if (value is! String && value is! int) return null;
    final text = '$value';
    return RegExp(r'^\w+$').hasMatch(text) ? text : null;
  }

  /// [source] with the token spans [spans] blanked out: each replaced by the
  /// line breaks it held, so every other line keeps its number.
  String _blankOut(
      String source, List<Token> tokens, List<(int, int)> spans) {
    final lineStarts = <int>[0];
    for (var i = 0; i < source.length; i++) {
      if (source[i] == '\n') lineStarts.add(i + 1);
    }
    int offsetOf(Token t) => lineStarts[t.line - 1] + t.column - 1;

    final cuts = [
      for (final (first, last) in spans)
        (offsetOf(tokens[first]), offsetOf(tokens[last]) + tokens[last].lexeme.length),
    ]..sort((a, b) => a.$1.compareTo(b.$1));

    final out = StringBuffer();
    var at = 0;
    for (final (from, to) in cuts) {
      out.write(source.substring(at, from));
      out.write('\n' * '\n'.allMatches(source.substring(from, to)).length);
      at = to;
    }
    out.write(source.substring(at));
    return out.toString();
  }

  /// Read file contents (platform-specific)
  String _readFile(String filePath) {
    // This will be implemented differently for different platforms
    // For now, we assume dart:io is available
    throw UnimplementedError('Use load(source) directly or implement file reading');
  }
}

/// Exception thrown by BootLoader for parse errors.
class BootLoaderException implements Exception {
  final String message;
  BootLoaderException(this.message);

  @override
  String toString() => 'BootLoaderException: $message';
}
