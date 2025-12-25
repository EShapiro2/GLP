// lib/analysis/type_checker/type_compiler.dart
//
// Compiler from type definitions to DFAs.
// Uses NFA→DFA pipeline per Yardeni-Shapiro and spec v1.9.

import 'mode.dart';
import 'type_ast.dart';
import 'type_dfa.dart';
import 'type_nfa.dart';
import 'nfa_compiler.dart';
import 'nfa_to_dfa.dart';

/// Compiles TypeDef AST to TypeDFA via NFA→DFA pipeline
class TypeCompiler {
  final TypeEnvironment env;
  final Map<String, TypeDFA> _cache = {};

  TypeCompiler(this.env);

  /// Compile a type by name to DFA
  TypeDFA compile(String typeName) {
    // Check cache
    if (_cache.containsKey(typeName)) {
      return _cache[typeName]!;
    }

    // Handle built-in types
    if (typeName == 'Number') {
      final dfa = NumberTypeDFA();
      _cache[typeName] = dfa;
      return dfa;
    }

    if (typeName == 'String') {
      final dfa = StringTypeDFA();
      _cache[typeName] = dfa;
      return dfa;
    }

    // Look up type definition
    final typeDef = env.getType(typeName);
    if (typeDef == null) {
      throw TypeCompileError('Undefined type: $typeName');
    }

    // Compile via NFA→DFA pipeline
    final nfaCompiler = TypeNFACompiler(env);
    final nfa = nfaCompiler.compile(typeName);
    final dfaConverter = NFAToDFAConverter(nfa);
    final dfa = dfaConverter.convert();

    _cache[typeName] = dfa;
    return dfa;
  }

  /// Clear the compilation cache
  void clearCache() {
    _cache.clear();
  }
}

/// Error during type compilation
class TypeCompileError implements Exception {
  final String message;

  TypeCompileError(this.message);

  @override
  String toString() => 'Type compile error: $message';
}
