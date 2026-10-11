// lib/analysis/type_checker/type_conversion.dart
//
// Converts Term AST (from unified parser) to TypeExpr AST for type definitions.
// Specification: TGLP (Moded-Types), sections/well-typing.tex

import '../../compiler/ast.dart';
import 'type_ast.dart';

/// Converts a Term AST node to a TypeExpr AST node.
/// 
/// This is a pure structural conversion with no semantic validation.
/// Semantic checks (determinism, alias prohibition) happen in type_environment.
TypeExpr termToTypeExpr(Term term) {
  if (term is VarTerm) {
    // Variable → TypeRef
    return TypeRef(term.name, term.line, term.column, isInput: term.isReader);
  }
  
  if (term is UnderscoreTerm) {
    // Anonymous _ or _? → PrimitiveModeAlt
    return PrimitiveModeAlt(term.isReader, term.line, term.column);
  }
  
  if (term is ConstTerm) {
    // Constant → ConstantAlt
    // ConstTerm.value is Object? but ConstantAlt expects Object
    final value = term.value ?? '';
    return ConstantAlt(value, term.line, term.column);
  }
  
  if (term is ListTerm) {
    // Empty list
    if (term.head == null && term.tail == null) {
      return ListNilAlt(term.line, term.column);
    }
    // List cons
    return ListConsAlt(
      termToTypeExpr(term.head!),
      termToTypeExpr(term.tail!),
      term.line,
      term.column,
    );
  }
  
  if (term is StructTerm) {
    // Difference list: A \ B
    if (term.functor == '\\' && term.args.length == 2) {
      return DiffListAlt(
        termToTypeExpr(term.args[0]),
        termToTypeExpr(term.args[1]),
        term.line,
        term.column,
      );
    }

    // Parameterized type reference — a name written as a type name is, a
    // variable-shaped token, before "(" ([TypeRefTerm]), uppercase-initial,
    // e.g., Stream(X), Channel(In, Out), Stream(X)? (encoded as "Stream?").
    // A structure whose functor is a name written as a constant is, quoted
    // or bare, is a structure alternative whatever its first letter.
    if (term is TypeRefTerm) {
      var functor = term.functor;
      bool isInput = false;
      if (functor.endsWith('?')) {
        functor = functor.substring(0, functor.length - 1);
        isInput = true;
      }
      if (functor.isNotEmpty && _isUppercaseLetter(functor[0])) {
        return TypeRef(functor, term.line, term.column,
            isInput: isInput,
            typeArgs: term.args.map(termToTypeExpr).toList());
      }
    }

    // A structure alternative: its functor a name, quoted or bare, an
    // operator's among them, and the conjunction ','
    return StructAlt(
      term.functor,
      term.args.map(termToTypeExpr).toList(),
      term.line,
      term.column,
    );
  }
  
  // Should not reach here for valid terms
  throw ArgumentError('Cannot convert term to type expression: $term');
}

/// A parameterised type reference in a type alternative, `Name(T1, ..., Tn)`,
/// as the parser reads it: `Name` a variable-shaped token, as a type name is
/// written, its functor carrying a trailing `?` where it is marked as a dual.
/// Only this is read as a type reference ([termToTypeExpr]).  A quoted name is
/// a string, never a type name (GLP-Spec appendix-lp.tex, Definition "Logic
/// Programs Syntax"; GLP #3 Cowork, 2026-10-11 12:59 UTC, "21:56"), so
/// `'Odd one'(Integer)` is the structure of that functor, its first letter a
/// capital or not.  Until 2026-10-11 a structure was read as a type reference
/// by its functor's first letter, quoted or not.
class TypeRefTerm extends StructTerm {
  TypeRefTerm(super.functor, super.args, super.line, super.column);
}

/// Check if a character is an uppercase letter (A-Z).
/// Excludes operators (+, -, etc.) and underscore which satisfy toUpperCase() == self.
bool _isUppercaseLetter(String ch) {
  return ch.codeUnitAt(0) >= 65 && ch.codeUnitAt(0) <= 90; // A-Z
}
