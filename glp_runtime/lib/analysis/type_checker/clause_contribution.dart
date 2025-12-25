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
  ///
  /// IMPORTANT: declaredIsInput determines the mode used in TypeRefs.
  /// This ensures inferred DFA has same modes as declared DFA for fixpoint check.
  TypeDFA computeArgContribution(
    ast.Term pattern,
    Map<String, TypeDFA> varTypes,
    Map<String, String> varTypeNames,
    TypeDFA declaredDFA,
    bool declaredIsInput,
  ) {
    // Step 1: Convert pattern to type expression with declared mode
    final typeExpr = patternToTypeExpr(pattern, varTypeNames, declaredIsInput);

    // Step 2: Compile via NFA→DFA pipeline (spec 5.4.3)
    final nfaCompiler = TypeNFACompiler(typeEnv);
    final nfa = nfaCompiler.compileExpr(typeExpr);
    final dfaConverter = NFAToDFAConverter(nfa);
    var dfa = dfaConverter.convert();

    // Step 3: Apply same mode transformation as declaredDFA
    // For input arguments, declaredDFA has applyModeComplement() applied,
    // so we must apply the same to inferredDFA for subset comparison to work.
    // Without this, the alphabets differ (e.g., [.|output] vs [.|input]).
    if (declaredIsInput) {
      dfa = dfa.applyModeComplement();
    }

    return dfa;
  }

  /// Convert a pattern term to an equivalent type expression.
  ///
  /// Per spec Section 5.4.4 and paper Section 6 (Example 6.22):
  /// - Writer variable X → TypeRef with output mode (isInput: false)
  /// - Reader variable X? → TypeRef with input mode (isInput: true)
  ///
  /// The variable's reader/writer status determines the mode annotation,
  /// which affects how the clause contribution DFA is built.
  TypeExpr patternToTypeExpr(ast.Term term, Map<String, String> varTypeNames, bool declaredIsInput) {
    if (term is ast.VarTerm) {
      // Variable: reference to inferred type with VARIABLE's mode (reader/writer)
      // Per spec 5.4.4: Writer X → output mode, Reader X? → input mode
      final varTypeName = varTypeNames[term.name] ?? 'Any';
      final varIsInput = term.isReader;  // reader X? → input, writer X → output
      return TypeRef(varTypeName, term.line, term.column, isInput: varIsInput);
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
      // Structure: recursively convert arguments with same declared mode
      final argExprs = <TypeExpr>[];
      for (final arg in term.args) {
        argExprs.add(patternToTypeExpr(arg, varTypeNames, declaredIsInput));
      }
      return StructAlt(term.functor, argExprs, term.line, term.column);
    }

    if (term is ast.ListTerm) {
      if (term.isNil) {
        return ListNilAlt(term.line, term.column);
      }
      final headExpr = term.head != null
          ? patternToTypeExpr(term.head!, varTypeNames, declaredIsInput)
          : TypeRef('Any', term.line, term.column, isInput: declaredIsInput);
      final tailExpr = term.tail != null
          ? patternToTypeExpr(term.tail!, varTypeNames, declaredIsInput)
          : TypeRef('List', term.line, term.column, isInput: declaredIsInput);
      return ListConsAlt(headExpr, tailExpr, term.line, term.column);
    }

    if (term is ast.UnderscoreTerm) {
      return TypeRef('Every', term.line, term.column, isInput: declaredIsInput);
    }

    return TypeRef('Any', term.line, term.column, isInput: declaredIsInput);
  }
}
