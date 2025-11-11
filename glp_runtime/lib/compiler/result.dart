import '../bytecode/runner.dart' show BytecodeProgram;

/// Result of compilation including bytecode and metadata
class CompilationResult {
  final BytecodeProgram program;
  final Map<String, int> variableMap;  // Variable name -> register index

  CompilationResult(this.program, this.variableMap);
}
