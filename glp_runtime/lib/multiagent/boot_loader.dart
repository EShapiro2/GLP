/// Boot Loader for maGLP Isolate Spawning
///
/// Parses GLP files with `boot/0` clauses containing `@` spawn directives.
/// Extracts spawn configuration without modifying the GLP parser.
///
/// See: docs/ma/isolate-boot-spec.md (v0.4)

/// A single spawn directive extracted from the boot clause.
///
/// Represents `goalFunctor(agentId, ...)@agentId`
class SpawnDirective {
  /// The agent identifier (e.g., 'alice', 'bob')
  final String agentId;

  /// The goal functor to spawn (e.g., 'agent_init', 'alice_agent')
  final String goalFunctor;

  /// The arity of the goal (number of arguments)
  final int arity;

  SpawnDirective({
    required this.agentId,
    required this.goalFunctor,
    required this.arity,
  });

  @override
  String toString() => 'SpawnDirective($goalFunctor/$arity($agentId, ...)@$agentId)';
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

  /// Optional shared source code to load before the boot file
  /// (e.g., social_agent.glp containing agent/4, actors, helpers)
  String? sharedSource;

  BootConfig({
    required this.directives,
    required this.fullSource,
    required this.source,
    this.sharedSource,
  });
}

/// Loader for GLP files with isolate boot clauses.
///
/// Parses the boot clause to extract spawn directives, then provides
/// the source for compilation.
class BootLoader {
  /// Load a GLP file and extract boot configuration.
  ///
  /// Throws [BootLoaderException] if:
  /// - File doesn't start with `procedure boot.`
  /// - Boot clause is malformed
  /// - Agent IDs don't match between goal and @target
  /// - Duplicate agent IDs
  BootConfig load(String source) {
    final directives = _parseBootClause(source);
    final compilableSource = _stripBootClause(source);
    return BootConfig(
      directives: directives,
      fullSource: source,
      source: compilableSource,
    );
  }

  /// Load from file path (convenience method)
  BootConfig loadFile(String filePath) {
    final file = _readFile(filePath);
    return load(file);
  }

  /// Parse the boot clause and extract spawn directives.
  List<SpawnDirective> _parseBootClause(String source) {
    // Remove comments for parsing
    final noComments = _removeComments(source);

    // Check for procedure boot declaration
    if (!_hasProcedureBoot(noComments)) {
      throw BootLoaderException('First procedure must be boot/0. '
          'Expected "procedure boot." declaration.');
    }

    // Find boot clause
    final bootClause = _extractBootClause(noComments);
    if (bootClause == null) {
      throw BootLoaderException('Could not find boot clause. '
          'Expected "boot :- ... ."');
    }

    // Parse spawn directives from boot clause body
    final directives = _parseSpawnDirectives(bootClause);
    if (directives.isEmpty) {
      throw BootLoaderException('Boot clause contains no spawn directives. '
          'Expected "goal(agent, _)@agent"');
    }

    // Validate no duplicate agent IDs
    final agentIds = <String>{};
    for (final d in directives) {
      if (agentIds.contains(d.agentId)) {
        throw BootLoaderException('Duplicate agent ID: ${d.agentId}');
      }
      agentIds.add(d.agentId);
    }

    return directives;
  }

  /// Remove GLP comments (lines starting with %%)
  String _removeComments(String source) {
    return source
        .split('\n')
        .where((line) => !line.trimLeft().startsWith('%'))
        .join('\n');
  }

  /// Check if source has "procedure boot." declaration
  bool _hasProcedureBoot(String source) {
    // Match "procedure boot." with flexible whitespace
    final pattern = RegExp(r'procedure\s+boot\s*\.', multiLine: true);
    return pattern.hasMatch(source);
  }

  /// Extract the boot clause body (between "boot :-" and ".")
  String? _extractBootClause(String source) {
    // Match "boot :- ... ." capturing the body
    // This handles multi-line boot clauses
    final pattern = RegExp(
      r'boot\s*:-\s*(.*?)\.\s*(?=\n|procedure|$)',
      multiLine: true,
      dotAll: true,
    );
    final match = pattern.firstMatch(source);
    return match?.group(1)?.trim();
  }

  /// Parse spawn directives from boot clause body.
  ///
  /// Looks for patterns like:
  /// `functor(agentId, ...)@agentId`
  ///
  /// The first argument must be the agent ID (an atom).
  /// Remaining arguments can be arbitrary terms (e.g., ch(_?,_)).
  /// The @target must match the first argument.
  List<SpawnDirective> _parseSpawnDirectives(String clauseBody) {
    final directives = <SpawnDirective>[];

    // Strategy: find each @target, then work backwards to find the matching
    // goal(agentId, ...) by balancing parentheses.
    final atPattern = RegExp(r'@\s*(\w+)');

    for (final atMatch in atPattern.allMatches(clauseBody)) {
      final targetAgentId = atMatch.group(1)!;
      final beforeAt = clauseBody.substring(0, atMatch.start).trimRight();

      // The character before @ should be ')' (end of goal arguments)
      if (beforeAt.isEmpty || beforeAt[beforeAt.length - 1] != ')') {
        continue; // Not a goal@target pattern
      }

      // Walk backwards from ')' to find matching '('
      var depth = 0;
      var parenStart = -1;
      for (var i = beforeAt.length - 1; i >= 0; i--) {
        if (beforeAt[i] == ')') depth++;
        if (beforeAt[i] == '(') depth--;
        if (depth == 0) {
          parenStart = i;
          break;
        }
      }

      if (parenStart < 0) continue;

      // Extract functor name before '('
      final beforeParen = beforeAt.substring(0, parenStart).trimRight();
      final functorMatch = RegExp(r'(\w+)$').firstMatch(beforeParen);
      if (functorMatch == null) continue;
      final functor = functorMatch.group(1)!;

      // Extract first argument (agent ID) from inside the parentheses
      final argsStr = beforeAt.substring(parenStart + 1, beforeAt.length - 1);
      // First arg is everything up to the first comma at depth 0
      var argDepth = 0;
      var firstArgEnd = argsStr.length;
      for (var i = 0; i < argsStr.length; i++) {
        if (argsStr[i] == '(' || argsStr[i] == '[') argDepth++;
        if (argsStr[i] == ')' || argsStr[i] == ']') argDepth--;
        if (argsStr[i] == ',' && argDepth == 0) {
          firstArgEnd = i;
          break;
        }
      }
      final goalAgentId = argsStr.substring(0, firstArgEnd).trim();

      // Agent ID must be a simple atom (word characters only)
      if (!RegExp(r'^\w+$').hasMatch(goalAgentId)) {
        throw BootLoaderException(
            'First argument of spawn goal must be an agent ID (atom), '
            'got "$goalAgentId"');
      }

      // Count arity: number of commas at depth 0, plus 1
      var arity = 1;
      var aDepth = 0;
      for (var i = 0; i < argsStr.length; i++) {
        if (argsStr[i] == '(' || argsStr[i] == '[') aDepth++;
        if (argsStr[i] == ')' || argsStr[i] == ']') aDepth--;
        if (argsStr[i] == ',' && aDepth == 0) arity++;
      }

      // Validate agent IDs match
      if (goalAgentId != targetAgentId) {
        throw BootLoaderException(
            'Agent ID mismatch: goal has "$goalAgentId" but @target is "$targetAgentId". '
            'They must match.');
      }

      directives.add(SpawnDirective(
        agentId: goalAgentId,
        goalFunctor: functor,
        arity: arity,
      ));
    }

    return directives;
  }

  /// Strip the boot clause and procedure declaration from source.
  /// Returns source that can be compiled by the GLP compiler.
  String _stripBootClause(String source) {
    // Remove "procedure boot." line
    var result = source.replaceFirst(
      RegExp(r'procedure\s+boot\s*\.\s*\n?', multiLine: true),
      '',
    );

    // Remove "boot :- ... ." clause (possibly multi-line)
    // Match from "boot :-" to the closing "." before next procedure or end
    result = result.replaceFirst(
      RegExp(r'boot\s*:-\s*.*?\.\s*\n?', multiLine: true, dotAll: true),
      '',
    );

    return result.trim() + '\n';
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
