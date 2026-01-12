// test/golden/golden_parser.dart
//
// Parser for .glp test file expectations
// Extracts %% comments and parses them into structured expectations

/// Parsed expectations from a .glp test file
class GoldenExpectations {
  final String testName;
  final String? paperRef;
  final bool expectWellTyped;
  final List<String> expectedErrors;
  final Map<String, AutomatonExpectation> automata;
  final List<ClauseExpectation> clauses;

  GoldenExpectations({
    required this.testName,
    this.paperRef,
    required this.expectWellTyped,
    required this.expectedErrors,
    required this.automata,
    required this.clauses,
  });

  bool get hasAutomatonExpectations => automata.isNotEmpty;
  bool get hasClauseExpectations => clauses.isNotEmpty;

  @override
  String toString() {
    final buf = StringBuffer();
    buf.writeln('GoldenExpectations(');
    buf.writeln('  testName: $testName,');
    buf.writeln('  paperRef: $paperRef,');
    buf.writeln('  expectWellTyped: $expectWellTyped,');
    buf.writeln('  expectedErrors: $expectedErrors,');
    buf.writeln('  automata: ${automata.keys.toList()},');
    buf.writeln('  clauses: ${clauses.length},');
    buf.writeln(')');
    return buf.toString();
  }
}

/// Expected transitions for a type or procedure automaton
class AutomatonExpectation {
  final String stateName;
  final List<TransitionExpectation> transitions;

  AutomatonExpectation(this.stateName, this.transitions);

  @override
  String toString() => 'AutomatonExpectation($stateName, ${transitions.length} transitions)';
}

/// Expected transition: label -> target
class TransitionExpectation {
  final String symbol;       // e.g., "[]", "[|]", "merge"
  final int? argIndex;       // e.g., 1, 2, 3 (null for constants)
  final String? mode;        // "↑" or "↓" (null for some)
  final String targetState;  // e.g., "_", "Stream?", "✓"

  TransitionExpectation({
    required this.symbol,
    this.argIndex,
    this.mode,
    required this.targetState,
  });

  @override
  String toString() {
    final parts = [symbol];
    if (argIndex != null) parts.add('$argIndex');
    if (mode != null) parts.add(mode!);
    parts.add('->');
    parts.add(targetState);
    return parts.join(' ');
  }
}

/// Expected moded clause
class ClauseExpectation {
  final int clauseIndex;  // 1-based
  final String? head;
  final String? body;
  final Map<String, String> variableTypes;  // e.g., {"X": "_", "X?": "_?"}

  ClauseExpectation({
    required this.clauseIndex,
    this.head,
    this.body,
    required this.variableTypes,
  });

  @override
  String toString() => 'ClauseExpectation($clauseIndex, head: $head, vars: ${variableTypes.length})';
}

/// Parse a .glp file and extract expectations from %% comments
GoldenExpectations parseGoldenExpectations(String source) {
  final lines = source.split('\n');
  
  String? testName;
  String? paperRef;
  bool? expectWellTyped;
  final expectedErrors = <String>[];
  final automata = <String, AutomatonExpectation>{};
  final clauses = <ClauseExpectation>[];
  
  String? currentAutomaton;
  List<TransitionExpectation>? currentTransitions;
  
  int? currentClauseIndex;
  String? currentHead;
  String? currentBody;
  Map<String, String>? currentVars;
  
  void flushAutomaton() {
    if (currentAutomaton != null && currentTransitions != null) {
      automata[currentAutomaton!] = AutomatonExpectation(
        currentAutomaton!,
        currentTransitions!,
      );
    }
    currentAutomaton = null;
    currentTransitions = null;
  }
  
  void flushClause() {
    if (currentClauseIndex != null) {
      clauses.add(ClauseExpectation(
        clauseIndex: currentClauseIndex!,
        head: currentHead,
        body: currentBody,
        variableTypes: currentVars ?? {},
      ));
    }
    currentClauseIndex = null;
    currentHead = null;
    currentBody = null;
    currentVars = null;
  }
  
  for (final line in lines) {
    final trimmed = line.trim();
    
    // Only process %% lines
    if (!trimmed.startsWith('%%')) continue;
    
    final content = trimmed.substring(2).trim();
    
    // Header fields
    if (content.startsWith('TEST:')) {
      testName = content.substring(5).trim();
    } else if (content.startsWith('PAPER:')) {
      paperRef = content.substring(6).trim();
    } else if (content.startsWith('EXPECT:')) {
      final value = content.substring(7).trim().toLowerCase();
      expectWellTyped = value == 'well-typed';
    } else if (content.startsWith('ERROR:')) {
      expectedErrors.add(content.substring(6).trim());
    }
    // Automaton section
    else if (content.startsWith('AUTOMATON ')) {
      flushAutomaton();
      flushClause();
      currentAutomaton = content.substring(10).trim();
      currentTransitions = [];
    }
    // Clause section
    else if (content.startsWith('CLAUSE ')) {
      flushAutomaton();
      flushClause();
      currentClauseIndex = int.tryParse(content.substring(7).trim());
    } else if (content.startsWith('HEAD:')) {
      currentHead = content.substring(5).trim();
    } else if (content.startsWith('BODY:')) {
      currentBody = content.substring(5).trim();
    } else if (content.startsWith('VARS:')) {
      currentVars = _parseVars(content.substring(5).trim());
    }
    // Transition line (inside AUTOMATON section)
    else if (currentAutomaton != null && content.contains('->')) {
      final transition = _parseTransition(content);
      if (transition != null) {
        currentTransitions!.add(transition);
      }
    }
  }
  
  // Flush any remaining sections
  flushAutomaton();
  flushClause();
  
  return GoldenExpectations(
    testName: testName ?? 'Unnamed Test',
    paperRef: paperRef,
    expectWellTyped: expectWellTyped ?? true,
    expectedErrors: expectedErrors,
    automata: automata,
    clauses: clauses,
  );
}

/// Parse a transition line like "[|] 1 ↑ -> _"
TransitionExpectation? _parseTransition(String line) {
  final arrowIndex = line.indexOf('->');
  if (arrowIndex < 0) return null;
  
  final lhs = line.substring(0, arrowIndex).trim();
  final rhs = line.substring(arrowIndex + 2).trim();
  
  // Parse LHS: symbol [argIndex] [mode]
  final parts = lhs.split(RegExp(r'\s+'));
  if (parts.isEmpty) return null;
  
  final symbol = parts[0];
  int? argIndex;
  String? mode;
  
  for (var i = 1; i < parts.length; i++) {
    final part = parts[i];
    if (part == '↑' || part == '↓') {
      mode = part;
    } else {
      final num = int.tryParse(part);
      if (num != null) {
        argIndex = num;
      }
    }
  }
  
  return TransitionExpectation(
    symbol: symbol,
    argIndex: argIndex,
    mode: mode,
    targetState: rhs,
  );
}

/// Parse variable types like "X:_, X?:_?, Xs:Stream"
Map<String, String> _parseVars(String varsStr) {
  final result = <String, String>{};
  
  final pairs = varsStr.split(',');
  for (final pair in pairs) {
    final colonIndex = pair.indexOf(':');
    if (colonIndex > 0) {
      final varName = pair.substring(0, colonIndex).trim();
      final typeName = pair.substring(colonIndex + 1).trim();
      result[varName] = typeName;
    }
  }
  
  return result;
}
