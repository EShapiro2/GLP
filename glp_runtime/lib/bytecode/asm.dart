// ignore_for_file: non_constant_identifier_names
import 'opcodes.dart';
import 'opcodes_v2.dart' as opv2;
import 'runner.dart';

class BC {
  // lowerCamelCase helpers
  static Label l(String name) => Label(name);
  static ClauseTry try_() => ClauseTry();
  static GuardNeedReader r(int readerId) => GuardNeedReader(readerId);
  static HeadBindWriter w(int writerId) => HeadBindWriter(writerId);
  static Commit commit() => Commit();

  // New spec-compliant control flow instructions
  static ClauseNext clauseNext(String label) => ClauseNext(label);
  static TryNextClause tryNextClause() => TryNextClause();
  static NoMoreClauses noMoreClauses() => NoMoreClauses();

  // Legacy (deprecated)
  @deprecated
  static UnionSiAndGoto u(String label) => UnionSiAndGoto(label);
  @deprecated
  static ResetAndGoto next(String label) => ResetAndGoto(label);

  static SuspendEnd susp() => SuspendEnd();
  static Proceed proceed() => Proceed();
  static BodySetConst bconst(int writerId, Object? v) => BodySetConst(writerId, v);
  static BodySetStructConstArgs bstructC(int writerId, String f, List<Object?> constArgs)
    => BodySetStructConstArgs(writerId, f, constArgs);
  static HeadStructure headStruct(String functor, int arity, int argSlot)
    => HeadStructure(functor, arity, argSlot);
  static opv2.HeadVariable headWriter(int varIndex) => opv2.HeadVariable(varIndex, isReader: false);
  static opv2.HeadVariable headReader(int varIndex) => opv2.HeadVariable(varIndex, isReader: true);
  static UnifyConstant unifyConst(Object? value) => UnifyConstant(value);
  static UnifyVoid unifyVoid({int count = 1}) => UnifyVoid(count: count);
  static HeadConstant headConst(Object? value, int argSlot) => HeadConstant(value, argSlot);
  static GetVariable getVar(int varIndex, int argSlot) => GetVariable(varIndex, argSlot);
  static GetValue getVal(int varIndex, int argSlot) => GetValue(varIndex, argSlot);
  static PutConstant putConst(Object? value, int argSlot) => PutConstant(value, argSlot);
  static PutStructure putStructure(String functor, int arity, int argSlot) => PutStructure(functor, arity, argSlot);
  static SetConstant setConst(Object? value) => SetConstant(value);
  static Otherwise otherwise() => Otherwise();
  static opv2.IfVariable ifWriter(int varIndex) => opv2.IfVariable(varIndex, isReader: false);
  static opv2.IfVariable ifReader(int varIndex) => opv2.IfVariable(varIndex, isReader: true);
  static Spawn spawn(String label, int arity) => Spawn(label, arity);
  static Requeue requeue(String label, int arity) => Requeue(label, arity);

  // Guard instructions
  static Guard guard(String label, int arity) => Guard(label, arity);
  static Ground ground(int varIndex) => Ground(varIndex);
  static Known known(int varIndex) => Known(varIndex);

  // List-specific instructions
  static HeadNil headNil(int argSlot) => HeadNil(argSlot);
  static HeadList headList(int argSlot) => HeadList(argSlot);
  static PutNil putNil(int argSlot) => PutNil(argSlot);
  static PutList putList(int argSlot) => PutList(argSlot);

  // Environment frame instructions
  static Allocate allocate(int slots) => Allocate(slots);
  static Deallocate deallocate() => Deallocate();

  // Utility instructions
  static Nop nop() => Nop();
  static Halt halt() => Halt();

  // UPPERCASE aliases
  static Label L(String name) => l(name);
  static ClauseTry TRY() => try_();
  static GuardNeedReader R(int readerId) => r(readerId);
  static HeadBindWriter W(int writerId) => w(writerId);
  static Commit COMMIT() => commit();

  // New spec-compliant control flow (UPPERCASE)
  static ClauseNext CLAUSE_NEXT(String label) => clauseNext(label);
  static TryNextClause TRY_NEXT_CLAUSE() => tryNextClause();
  static NoMoreClauses NO_MORE_CLAUSES() => noMoreClauses();

  // Legacy (deprecated)
  @deprecated
  static UnionSiAndGoto U(String label) => u(label);
  @deprecated
  static ResetAndGoto NEXT(String label) => next(label);

  static SuspendEnd SUSP() => susp();
  static Proceed PROCEED() => proceed();
  static Otherwise OTHERWISE() => otherwise();
  static opv2.IfVariable IF_WRITER(int varIndex) => ifWriter(varIndex);
  static opv2.IfVariable IF_READER(int varIndex) => ifReader(varIndex);
  static BodySetConst BCONST(int writerId, Object? v) => bconst(writerId, v);
  static BodySetStructConstArgs BSTRUCTC(int writerId, String f, List<Object?> constArgs)
    => bstructC(writerId, f, constArgs);

  static BytecodeProgram prog(List<Op> ops) => BytecodeProgram(ops);
}
