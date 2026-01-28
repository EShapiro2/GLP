/// Boot Loader for maGLP Isolate Spawning
///
/// Parses GLP files with `boot/0` clauses containing `@` spawn directives.
/// Extracts spawn configuration without modifying the GLP parser.
///
/// See: docs/ma/isolate-boot-spec.md (v0.4)

/// A single spawn directive extracted from the boot clause.
///
/// Represents `goalFunctor(agentId, ch(_?,_), ch(_?,_))@agentId`
class SpawnDirective {
  /// The agent identifier (e.g., 'alice', 'bob')
  final String agentId;

  /// The goal functor to spawn (e.g., 'agent_init', 'alice_agent')
  final String goalFunctor;

  SpawnDirective({
    required this.agentId,
    required this.goalFunctor,
  });

  @override
  String toString() => 'SpawnDirective($goalFunctor($agentId, ...)@$agentId)';
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

  BootConfig({
    required this.directives,
    required this.fullSource,
    required this.source,
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
          'Expected "goal(agent, ch(_?,_), ch(_?,_))@agent"');
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
  /// `functor(agentId, ch(_?,_), ch(_?,_))@agentId`
  List<SpawnDirective> _parseSpawnDirectives(String clauseBody) {
    final directives = <SpawnDirective>[];

    // Pattern for spawn directive:
    // functor(agentId, ch(_?,_), ch(_?,_))@agentId
    //
    // Breakdown:
    // - (\w+) : goal functor (e.g., agent_init)
    // - \( : opening paren
    // - (\w+) : first arg = agent ID
    // - \s*,\s* : comma separator
    // - ch\s*\(\s*_\?\s*,\s*_\s*\) : ch(_?,_) pattern for UICh
    // - \s*,\s* : comma separator
    // - ch\s*\(\s*_\?\s*,\s*_\s*\) : ch(_?,_) pattern for NetCh
    // - \) : closing paren
    // - \s*@\s* : @ operator
    // - (\w+) : target agent ID
    final pattern = RegExp(
      r'(\w+)\s*\(\s*(\w+)\s*,\s*ch\s*\(\s*_\?\s*,\s*_\s*\)\s*,\s*ch\s*\(\s*_\?\s*,\s*_\s*\)\s*\)\s*@\s*(\w+)',
    );

    for (final match in pattern.allMatches(clauseBody)) {
      final functor = match.group(1)!;
      final goalAgentId = match.group(2)!;
      final targetAgentId = match.group(3)!;

      // Validate agent IDs match
      if (goalAgentId != targetAgentId) {
        throw BootLoaderException(
            'Agent ID mismatch: goal has "$goalAgentId" but @target is "$targetAgentId". '
            'They must match.');
      }

      directives.add(SpawnDirective(
        agentId: goalAgentId,
        goalFunctor: functor,
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
