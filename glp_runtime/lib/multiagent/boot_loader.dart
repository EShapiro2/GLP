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

/// The agent a boot clause declares: its name p, and the person scripted in
/// GLP at p, if the clause names one (IGLP, Implementation Notes, "Boot").
///
/// Every agent's initial goal is the same, `agent(ch(_?,_),ch(_?,_))` (GLP-Spec
/// maglp.tex, Definition "Multiagent GLP"), so a directive carries only the
/// agent's name and its scripted person.
class SpawnDirective {
  /// The agent's name p in the boot clause, which is the persona's name its
  /// key is made from (IGLP, Implementation Notes, "Boot").
  final String agentId;

  /// The person scripted in GLP at p, the second goal `P@p`, or null where
  /// the clause names none.
  final PersonGoal? person;

  SpawnDirective({required this.agentId, this.person});

  @override
  String toString() => 'SpawnDirective(agent@$agentId'
      '${person == null ? '' : ', ${person!.functor}@$agentId'})';
}

/// The person scripted in GLP at an agent: a goal `P@p` of the boot clause,
/// one of whose arguments is written `ch(_?,_)`, the person's channel, and
/// whose other arguments are ground terms.  The runtime runs P on p's person
/// channel in the person's place, its writes being the person's input (IGLP,
/// Implementation Notes, "Boot"): the channel's reader is the agent's output
/// to the person, and its writer the person's input to the agent.
class PersonGoal {
  /// The goal's predicate.
  final String functor;

  /// The goal's arguments as GLP text, the person's channel among them at
  /// [channelIndex].
  final List<String> args;

  /// The position of the argument written `ch(_?,_)`.
  final int channelIndex;

  PersonGoal(this.functor, this.args, this.channelIndex);

  /// The goal's text with its channel `ch(<toPerson>?, <fromPerson>)`: the
  /// reader of the agent's output to the person, and the writer of the
  /// person's input.
  String text(String toPerson, String fromPerson) {
    final a = [...args];
    a[channelIndex] = 'ch($toPerson?, $fromPerson)';
    return '$functor(${a.join(', ')})';
  }
}

/// Configuration extracted from a GLP boot file.
class BootConfig {
  /// The spawn directives from the boot clause
  final List<SpawnDirective> directives;

  /// The full source code (original, including boot clause)
  final String fullSource;

  /// Source code with the boot declaration and clause blanked out (for GLP
  /// compilation).  The parser reads the clause's `G@p` goals as [SpawnGoal]s,
  /// from which [BootLoader] takes the [directives]; the rest of the file
  /// compiles as an ordinary module.
  final String source;

  /// Optional program directory for static linking.
  /// When set, each isolate loads the program via loadProgram() instead of
  /// loading individual shared sources. The boot source is loaded on top.
  String? programDir;

  /// Absolute path to programs/self.glp
  String rootSelfGlpPath;

  /// The boot file's path, where the caller knows it. The boot source is
  /// checked in the linked
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
  /// - A goal of the clause is no `G@p`, an agent's goal is not
  ///   `agent(ch(_?,_),ch(_?,_))`, or a scripted person's is not one goal
  ///   with one argument `ch(_?,_)` and the others ground, at an agent the
  ///   clause declares
  /// - Duplicate agent IDs, or two scripted persons at one agent
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

  /// The spawn directives of the boot clause: for each agent p, its goal
  /// `agent(ch(_?,_),ch(_?,_))@p`, every agent's initial goal (GLP-Spec
  /// maglp.tex, Definition "Multiagent GLP"; IGLP, Implementation Notes,
  /// "Boot"), and at most one second goal `P@p`, the person scripted in GLP
  /// at p.  Any other goal of the clause is refused.
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

    final agents = <String>[];
    final persons = <String, PersonGoal>{};
    for (final goal in body) {
      if (goal is! SpawnGoal) {
        throw BootLoaderException('Boot clause: $goal is no goal G@p: '
            'a boot clause declares its agents and their scripted persons');
      }
      final p = goal.agentId;
      if (!RegExp(r'^\w+$').hasMatch(p)) {
        throw BootLoaderException('Boot clause: $p is no agent name');
      }
      final inner = goal.innerGoal;
      if (inner.functor == 'agent' && inner.args.length == 2) {
        if (!inner.args.every(_isAnonymousChannel)) {
          throw BootLoaderException('Boot clause: $goal: every agent\'s '
              'initial goal is agent(ch(_?,_),ch(_?,_))');
        }
        if (agents.contains(p)) {
          throw BootLoaderException('Duplicate agent ID: $p');
        }
        agents.add(p);
        continue;
      }
      if (persons.containsKey(p)) {
        throw BootLoaderException('Boot clause: $goal: agent $p has a '
            'scripted person already, ${persons[p]!.functor}');
      }
      persons[p] = _personGoal(goal);
    }

    for (final p in persons.keys) {
      if (!agents.contains(p)) {
        throw BootLoaderException('Boot clause: ${persons[p]!.functor}@$p '
            'scripts the person of $p, and the clause declares no agent $p '
            'by agent(ch(_?,_),ch(_?,_))@$p');
      }
    }
    if (agents.isEmpty) {
      throw BootLoaderException('Boot clause contains no spawn directives. '
          'Expected "agent(ch(_?,_),ch(_?,_))@p"');
    }
    return [
      for (final p in agents) SpawnDirective(agentId: p, person: persons[p]),
    ];
  }

  /// Whether [t] is written `ch(_?,_)`.
  static bool _isAnonymousChannel(Term t) =>
      t is StructTerm &&
      t.functor == 'ch' &&
      t.args.length == 2 &&
      t.args[0] is UnderscoreTerm &&
      (t.args[0] as UnderscoreTerm).isReader &&
      t.args[1] is UnderscoreTerm &&
      !(t.args[1] as UnderscoreTerm).isReader;

  /// The scripted person of [goal]: exactly one argument written `ch(_?,_)`,
  /// the others ground.
  PersonGoal _personGoal(SpawnGoal goal) {
    final inner = goal.innerGoal;
    final channels = [
      for (var i = 0; i < inner.args.length; i++)
        if (_isAnonymousChannel(inner.args[i])) i
    ];
    if (channels.length != 1) {
      throw BootLoaderException('Boot clause: $goal: the person scripted at '
          '${goal.agentId} takes its channel as one argument written '
          'ch(_?,_), and this goal has ${channels.length}');
    }
    for (var i = 0; i < inner.args.length; i++) {
      if (i != channels.single && !_ground(inner.args[i])) {
        throw BootLoaderException('Boot clause: $goal: argument ${i + 1} of '
            'the scripted person is not ground');
      }
    }
    return PersonGoal(inner.functor,
        [for (final a in inner.args) '$a'], channels.single);
  }

  /// Whether [t] holds no variable.
  static bool _ground(Term t) {
    final stack = <Term?>[t];
    while (stack.isNotEmpty) {
      final x = stack.removeLast();
      if (x == null || x is ConstTerm) continue;
      if (x is VarTerm || x is UnderscoreTerm) return false;
      if (x is StructTerm) {
        stack.addAll(x.args);
      } else if (x is ListTerm) {
        stack..add(x.head)..add(x.tail);
      } else {
        return false;
      }
    }
    return true;
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
}

/// Exception thrown by BootLoader for parse errors.
class BootLoaderException implements Exception {
  final String message;
  BootLoaderException(this.message);

  @override
  String toString() => 'BootLoaderException: $message';
}
