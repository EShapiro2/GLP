// ignore_for_file: non_constant_identifier_names
import 'opcodes.dart';
import 'runner.dart';

class BC {
  // lowerCamelCase helpers
  static Label l(String name) => Label(name);
  static ClauseTry try_() => ClauseTry();
  static GuardNeedReader r(int readerId) => GuardNeedReader(readerId);
  static HeadBindWriter w(int writerId) => HeadBindWriter(writerId);
  static Commit commit() => Commit();
  static UnionSiAndGoto u(String label) => UnionSiAndGoto(label);
  static ResetAndGoto next(String label) => ResetAndGoto(label);
  static SuspendEnd susp() => SuspendEnd();
  static Proceed proceed() => Proceed();
  static BodySetConst bconst(int writerId, Object? v) => BodySetConst(writerId, v);
  static BodySetStructConstArgs bstructC(int writerId, String f, List<Object?> constArgs)
    => BodySetStructConstArgs(writerId, f, constArgs);
  static HeadStructure headStruct(String functor, int arity, int argSlot)
    => HeadStructure(functor, arity, argSlot);
  static HeadWriter headWriter(int varIndex) => HeadWriter(varIndex);
  static HeadReader headReader(int varIndex) => HeadReader(varIndex);
  static UnifyConstant unifyConst(Object? value) => UnifyConstant(value);
  static UnifyVoid unifyVoid({int count = 1}) => UnifyVoid(count: count);
  static HeadConstant headConst(Object? value, int argSlot) => HeadConstant(value, argSlot);
  static GetVariable getVar(int varIndex, int argSlot) => GetVariable(varIndex, argSlot);
  static GetValue getVal(int varIndex, int argSlot) => GetValue(varIndex, argSlot);
  static PutWriter putWriter(int varIndex, int argSlot) => PutWriter(varIndex, argSlot);
  static PutReader putReader(int varIndex, int argSlot) => PutReader(varIndex, argSlot);
  static PutConstant putConst(Object? value, int argSlot) => PutConstant(value, argSlot);
  static PutStructure putStructure(String functor, int arity, int argSlot) => PutStructure(functor, arity, argSlot);
  static SetWriter setWriter(int varIndex) => SetWriter(varIndex);
  static SetReader setReader(int varIndex) => SetReader(varIndex);
  static SetConstant setConst(Object? value) => SetConstant(value);
  static Otherwise otherwise() => Otherwise();
  static Spawn spawn(String label, int arity) => Spawn(label, arity);
  static Requeue requeue(String label, int arity) => Requeue(label, arity);

  // UPPERCASE aliases
  static Label L(String name) => l(name);
  static ClauseTry TRY() => try_();
  static GuardNeedReader R(int readerId) => r(readerId);
  static HeadBindWriter W(int writerId) => w(writerId);
  static Commit COMMIT() => commit();
  static UnionSiAndGoto U(String label) => u(label);
  static ResetAndGoto NEXT(String label) => next(label);
  static SuspendEnd SUSP() => susp();
  static Proceed PROCEED() => proceed();
  static Otherwise OTHERWISE() => otherwise();
  static BodySetConst BCONST(int writerId, Object? v) => bconst(writerId, v);
  static BodySetStructConstArgs BSTRUCTC(int writerId, String f, List<Object?> constArgs)
    => bstructC(writerId, f, constArgs);

  static BytecodeProgram prog(List<Op> ops) => BytecodeProgram(ops);
}
