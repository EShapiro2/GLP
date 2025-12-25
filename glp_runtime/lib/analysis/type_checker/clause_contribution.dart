// lib/analysis/type_checker/clause_contribution.dart
//
// Computes clause contributions T_{C}^{α,m}(S) for fixpoint checking.
// Follows spec v1.10 Section 5.4: patterns are converted to type expressions,
// then compiled via the NFA→DFA pipeline.

import '../../compiler/ast.dart' as ast;
import 'type_dfa.dart';
import 'type_ast.dart';
import 'type_nfa.dart';
import 'nfa_compiler.dart';
import 'nfa_to_dfa.dart';
import 'mode.dart';

/// Computes clause contributions for fixpoint checking
class ClauseContributionComputer {
  final TypeEnvironment typeEnv;

  ClauseContributionComputer(this.typeEnv);

  /// Compute DFA contribution for a clause head argument pattern.
  ///
  /// Per spec 5.4: Convert pattern to type expression, then use NFA→DFA pipeline.
  TypeDFA computeArgContribution(
    ast.Term pattern,
    Map<String, TypeDFA> varTypes,
    Map<String, String> varTypeNames,
    TypeDFA declaredDFA,
  ) {
    // Step 1: Convert pattern to type expression (spec 5.4.2-5.4.3)
    final typeExpr = patternToTypeExpr(pattern, varTypeNames);

    // Step 2: Compile via NFA→DFA pipeline (spec 5.4.3)
    final nfaCompiler = TypeNFACompiler(typeEnv);
    final nfa = nfaCompiler.compileExpr(typeExpr);
    final dfaConverter = NFAToDFAConverter(nfa);
    final dfa = dfaConverter.convert();

    return dfa;
  }

  /// Convert a pattern term to an equivalent type expression.
  ///
  /// Spec 5.4.2 Pattern-to-Type Correspondence:
  /// | Pattern              | Equivalent Type Expression                    |
  /// |----------------------|-----------------------------------------------|
  /// | Constant c           | ConstantAlt(c)                                |
  /// | Variable X (writer)  | TypeRef(varTypes[X], isInput: false)          |
  /// | Variable X? (reader) | TypeRef(varTypes[X], isInput: true)           |
  /// | Structure f(t₁,...,tₙ) | StructAlt(f, [T₁,...,Tₙ])                   |
  /// | List [H|T]           | ListConsAlt(patternToType(H), patternToType(T))|
  /// | List []              | ListNilAlt                                    |
  /// | Underscore _         | TypeRef("Every")                              |
  TypeExpr patternToTypeExpr(ast.Term term, Map<String, String> varTypeNames) {
    if (term is ast.VarTerm) {
      // Variable: reference to inferred type with variable's mode
      // Writer X → TypeRef(T, isInput: false) (output mode)
      // Reader X? → TypeRef(T, isInput: true) (input mode)
      final varTypeName = varTypeNames[term.name] ?? 'Any';
      final isInput = term.isReader;
      return TypeRef(varTypeName, term.line, term.column, isInput: isInput);
    }

    if (term is ast.ConstTerm) {
      // Constant: create constant alternative
      final value = term.value;
      if (value != null) {
        return ConstantAlt(value, term.line, term.column);
      }
      // Null value - treat as atom with empty string
      return ConstantAlt('', term.line, term.column);
    }

    if (term is ast.StructTerm) {
      // Structure: recursively convert arguments
      final argExprs = <TypeExpr>[];
      for (final arg in term.args) {
        argExprs.add(patternToTypeExpr(arg, varTypeNames));
      }
      return StructAlt(term.functor, argExprs, term.line, term.column);
    }

    if (term is ast.ListTerm) {
      if (term.isNil) {
        // Empty list
        return ListNilAlt(term.line, term.column);
      }
      // List cons: [H | T]
      final headExpr = term.head != null
          ? patternToTypeExpr(term.head!, varTypeNames)
          : TypeRef('Any', term.line, term.column);
      final tailExpr = term.tail != null
          ? patternToTypeExpr(term.tail!, varTypeNames)
          : TypeRef('List', term.line, term.column);
      return ListConsAlt(headExpr, tailExpr, term.line, term.column);
    }

    if (term is ast.UnderscoreTerm) {
      // Anonymous variable: accepts any value at any mode
      return TypeRef('Every', term.line, term.column);
    }

    // Fallback: treat as Any type
    return TypeRef('Any', term.line, term.column);
  }
}
