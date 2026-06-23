/// GLP instruction encoding (D3 wire format, §4).
///
/// Normative source: the IGLP paper appendix `app:wire-format`,
/// §wf-instructions. An encoded instruction is a u8 opcode followed by its
/// operands in the order the opcode table lists. Instruction *semantics* are the
/// companion GLP paper's; this assigns bytes.
///
/// Operand kinds (§4.1):
///  - polarity: u8 (0 writer, 1 reader)
///  - negated:  u8 (0 plain, 1 negated guard)
///  - varIndex/argSlot/arity/count/slots/regIndex/importIndex: clen
///  - constant: the §3.1 constant payload (u8 tag + payload)
///  - functor:  string
///  - proc:     clen index into the artefact's procedure table
///  - ctarget:  clen instruction index within the current procedure
///
/// Procedure references on the wire are table indices, never names; clause
/// targets are procedure-relative instruction indices. Assembly `Label`s do not
/// exist on the wire — [encodeCode] strips them and resolves the indices. The
/// caller supplies the name<->index maps (the artefact's procedure table in S4;
/// bijective stubs in tests).
library;

import 'dart:typed_data';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/opcodes_v2.dart' as opv2;
import 'package:glp_runtime/wire/codec.dart';

/// §4.2 opcode bytes.
class Opcode {
  static const int clauseTry = 0x01;
  static const int clauseNext = 0x02;
  static const int noMoreClauses = 0x03;
  static const int commit = 0x04;
  static const int proceed = 0x05;
  static const int halt = 0x06;
  static const int nop = 0x07;
  static const int headConstant = 0x10;
  static const int headNil = 0x11;
  static const int headStructure = 0x12;
  static const int headList = 0x13;
  static const int headVariable = 0x14;
  static const int getVariable = 0x15;
  static const int getValue = 0x16;
  static const int unifyVariable = 0x20;
  static const int unifyConstant = 0x21;
  static const int unifyVoid = 0x22;
  static const int unifyStructure = 0x23;
  static const int push = 0x24;
  static const int pop = 0x25;
  static const int putVariable = 0x30;
  static const int putConstant = 0x31;
  static const int putNil = 0x32;
  static const int putList = 0x33;
  static const int putStructure = 0x34;
  static const int setVariable = 0x35;
  static const int setConstant = 0x36;
  static const int allocate = 0x37;
  static const int deallocate = 0x38;
  static const int putBoundConst = 0x39;
  static const int putBoundNil = 0x3A;
  static const int guard = 0x40;
  static const int ground = 0x41;
  static const int known = 0x42;
  static const int unknown = 0x43;
  static const int noReaders = 0x44;
  static const int groundEqual = 0x45;
  static const int otherwise = 0x46;
  static const int spawn = 0x50;
  static const int requeue = 0x51;
  static const int distribute = 0x52;
  static const int transmit = 0x53;
}

/// Resolvers an encoder needs: a procedure label -> proc-table index, and a
/// clause-target label -> procedure-relative instruction index.
typedef ProcIndexOf = int Function(String label);
typedef CTargetOf = int Function(String label);

/// Resolvers a decoder needs: the inverses.
typedef ProcNameOf = String Function(int index);
typedef CTargetLabelOf = String Function(int index);

/// Encode one instruction object to bytes. `Label` is rejected — labels are
/// assembly-time and erased on the wire; encode a procedure with [encodeCode].
void encodeInstruction(
  WireWriter w,
  Object op, {
  required ProcIndexOf procIndexOf,
  required CTargetOf ctargetOf,
}) {
  void constant(Object? v) => encodeConstantPayload(w, wireConstFromValue(v));
  void pol(bool isReader) => w.u8(isReader ? 1 : 0);
  void neg(bool negated) => w.u8(negated ? 1 : 0);

  if (op is Label) {
    throw WireFormatException(
        'Label is assembly-time and not encodable; strip labels first');
  } else if (op is ClauseTry) {
    w.u8(Opcode.clauseTry);
  } else if (op is ClauseNext) {
    w.u8(Opcode.clauseNext);
    w.clen(ctargetOf(op.label));
  } else if (op is NoMoreClauses) {
    w.u8(Opcode.noMoreClauses);
  } else if (op is Commit) {
    w.u8(Opcode.commit);
  } else if (op is Proceed) {
    w.u8(Opcode.proceed);
  } else if (op is Halt) {
    w.u8(Opcode.halt);
  } else if (op is Nop) {
    w.u8(Opcode.nop);
  } else if (op is HeadConstant) {
    w.u8(Opcode.headConstant);
    constant(op.value);
    w.clen(op.argSlot);
  } else if (op is HeadNil) {
    w.u8(Opcode.headNil);
    w.clen(op.argSlot);
  } else if (op is HeadStructure) {
    w.u8(Opcode.headStructure);
    w.string(op.functor);
    w.clen(op.arity);
    w.clen(op.argSlot);
  } else if (op is HeadList) {
    w.u8(Opcode.headList);
    w.clen(op.argSlot);
  } else if (op is opv2.HeadVariable) {
    w.u8(Opcode.headVariable);
    pol(op.isReader);
    w.clen(op.varIndex);
  } else if (op is opv2.GetVariable) {
    w.u8(Opcode.getVariable);
    pol(op.isReader);
    w.clen(op.varIndex);
    w.clen(op.argSlot);
  } else if (op is opv2.GetValue) {
    w.u8(Opcode.getValue);
    pol(op.isReader);
    w.clen(op.varIndex);
    w.clen(op.argSlot);
  } else if (op is opv2.UnifyVariable) {
    w.u8(Opcode.unifyVariable);
    pol(op.isReader);
    w.clen(op.varIndex);
  } else if (op is UnifyConstant) {
    w.u8(Opcode.unifyConstant);
    constant(op.value);
  } else if (op is UnifyVoid) {
    w.u8(Opcode.unifyVoid);
    w.clen(op.count);
  } else if (op is UnifyStructure) {
    w.u8(Opcode.unifyStructure);
    w.string(op.functor);
    w.clen(op.arity);
  } else if (op is Push) {
    w.u8(Opcode.push);
    w.clen(op.regIndex);
  } else if (op is Pop) {
    w.u8(Opcode.pop);
    w.clen(op.regIndex);
  } else if (op is opv2.PutVariable) {
    w.u8(Opcode.putVariable);
    pol(op.isReader);
    w.clen(op.varIndex);
    w.clen(op.argSlot);
  } else if (op is PutConstant) {
    w.u8(Opcode.putConstant);
    constant(op.value);
    w.clen(op.argSlot);
  } else if (op is PutNil) {
    w.u8(Opcode.putNil);
    w.clen(op.argSlot);
  } else if (op is PutList) {
    w.u8(Opcode.putList);
    w.clen(op.argSlot);
  } else if (op is PutStructure) {
    w.u8(Opcode.putStructure);
    w.string(op.functor);
    w.clen(op.arity);
    w.clen(op.argSlot);
  } else if (op is opv2.SetVariable) {
    w.u8(Opcode.setVariable);
    pol(op.isReader);
    w.clen(op.varIndex);
  } else if (op is SetConstant) {
    w.u8(Opcode.setConstant);
    constant(op.value);
  } else if (op is Allocate) {
    w.u8(Opcode.allocate);
    w.clen(op.slots);
  } else if (op is Deallocate) {
    w.u8(Opcode.deallocate);
  } else if (op is PutBoundConst) {
    w.u8(Opcode.putBoundConst);
    constant(op.value);
    w.clen(op.argSlot);
  } else if (op is PutBoundNil) {
    w.u8(Opcode.putBoundNil);
    w.clen(op.argSlot);
  } else if (op is Guard) {
    w.u8(Opcode.guard);
    w.clen(procIndexOf(op.procedureLabel));
    w.clen(op.arity);
    neg(op.negated);
  } else if (op is Ground) {
    w.u8(Opcode.ground);
    w.clen(op.varIndex);
    neg(op.negated);
  } else if (op is Known) {
    w.u8(Opcode.known);
    w.clen(op.varIndex);
    neg(op.negated);
  } else if (op is opv2.Unknown) {
    w.u8(Opcode.unknown);
    w.clen(op.varIndex);
  } else if (op is NoReaders) {
    w.u8(Opcode.noReaders);
    w.clen(op.varIndex);
    neg(op.negated);
  } else if (op is GroundEqual) {
    w.u8(Opcode.groundEqual);
    w.clen(op.leftVarIndex);
    w.clen(op.rightVarIndex);
    neg(op.negated);
  } else if (op is Otherwise) {
    w.u8(Opcode.otherwise);
  } else if (op is Spawn) {
    w.u8(Opcode.spawn);
    w.clen(procIndexOf(op.procedureLabel));
    w.clen(op.arity);
  } else if (op is Requeue) {
    w.u8(Opcode.requeue);
    w.clen(procIndexOf(op.procedureLabel));
    w.clen(op.arity);
  } else if (op is Distribute) {
    w.u8(Opcode.distribute);
    w.clen(op.importIndex);
    w.string(op.functor);
    w.clen(op.arity);
  } else if (op is Transmit) {
    w.u8(Opcode.transmit);
    w.clen(op.moduleVarIndex);
    w.string(op.functor);
    w.clen(op.arity);
  } else {
    throw WireFormatException(
        'instruction not in the §4.2 wire ISA: ${op.runtimeType}');
  }
}

/// Decode one instruction. Polarity opcodes decode to the v2 classes; `proc`
/// and `ctarget` indices are resolved back to labels via the supplied maps.
Object decodeInstruction(
  WireReader r, {
  required ProcNameOf procNameOf,
  required CTargetLabelOf ctargetLabelOf,
}) {
  final opcode = r.u8();
  bool pol() {
    final p = r.u8();
    if (p != 0 && p != 1) throw WireFormatException('polarity not 0/1: $p');
    return p == 1;
  }

  bool neg() {
    final n = r.u8();
    if (n != 0 && n != 1) throw WireFormatException('negated not 0/1: $n');
    return n == 1;
  }

  Object? constant() => valueOfWireConst(decodeConstantPayload(r));

  switch (opcode) {
    case Opcode.clauseTry:
      return ClauseTry();
    case Opcode.clauseNext:
      return ClauseNext(ctargetLabelOf(r.clen()));
    case Opcode.noMoreClauses:
      return NoMoreClauses();
    case Opcode.commit:
      return Commit();
    case Opcode.proceed:
      return Proceed();
    case Opcode.halt:
      return Halt();
    case Opcode.nop:
      return Nop();
    case Opcode.headConstant:
      final v = constant();
      return HeadConstant(v, r.clen());
    case Opcode.headNil:
      return HeadNil(r.clen());
    case Opcode.headStructure:
      final f = r.string();
      return HeadStructure(f, r.clen(), r.clen());
    case Opcode.headList:
      return HeadList(r.clen());
    case Opcode.headVariable:
      final isReader = pol();
      return opv2.HeadVariable(r.clen(), isReader: isReader);
    case Opcode.getVariable:
      final isReader = pol();
      final varIndex = r.clen();
      return opv2.GetVariable(varIndex, r.clen(), isReader: isReader);
    case Opcode.getValue:
      final isReader = pol();
      final varIndex = r.clen();
      return opv2.GetValue(varIndex, r.clen(), isReader: isReader);
    case Opcode.unifyVariable:
      final isReader = pol();
      return opv2.UnifyVariable(r.clen(), isReader: isReader);
    case Opcode.unifyConstant:
      return UnifyConstant(constant());
    case Opcode.unifyVoid:
      return UnifyVoid(count: r.clen());
    case Opcode.unifyStructure:
      final f = r.string();
      return UnifyStructure(f, r.clen());
    case Opcode.push:
      return Push(r.clen());
    case Opcode.pop:
      return Pop(r.clen());
    case Opcode.putVariable:
      final isReader = pol();
      final varIndex = r.clen();
      return opv2.PutVariable(varIndex, r.clen(), isReader: isReader);
    case Opcode.putConstant:
      final v = constant();
      return PutConstant(v, r.clen());
    case Opcode.putNil:
      return PutNil(r.clen());
    case Opcode.putList:
      return PutList(r.clen());
    case Opcode.putStructure:
      final f = r.string();
      return PutStructure(f, r.clen(), r.clen());
    case Opcode.setVariable:
      final isReader = pol();
      return opv2.SetVariable(r.clen(), isReader: isReader);
    case Opcode.setConstant:
      return SetConstant(constant());
    case Opcode.allocate:
      return Allocate(r.clen());
    case Opcode.deallocate:
      return Deallocate();
    case Opcode.putBoundConst:
      final v = constant();
      return PutBoundConst(v, r.clen());
    case Opcode.putBoundNil:
      return PutBoundNil(r.clen());
    case Opcode.guard:
      final label = procNameOf(r.clen());
      final arity = r.clen();
      return Guard(label, arity, negated: neg());
    case Opcode.ground:
      final varIndex = r.clen();
      return Ground(varIndex, negated: neg());
    case Opcode.known:
      final varIndex = r.clen();
      return Known(varIndex, negated: neg());
    case Opcode.unknown:
      return opv2.Unknown(r.clen());
    case Opcode.noReaders:
      final varIndex = r.clen();
      return NoReaders(varIndex, negated: neg());
    case Opcode.groundEqual:
      final l = r.clen();
      final rr = r.clen();
      return GroundEqual(l, rr, negated: neg());
    case Opcode.otherwise:
      return Otherwise();
    case Opcode.spawn:
      final label = procNameOf(r.clen());
      return Spawn(label, r.clen());
    case Opcode.requeue:
      final label = procNameOf(r.clen());
      return Requeue(label, r.clen());
    case Opcode.distribute:
      final importIndex = r.clen();
      final f = r.string();
      return Distribute(importIndex, f, r.clen());
    case Opcode.transmit:
      final varIndex = r.clen();
      final f = r.string();
      return Transmit(varIndex, f, r.clen());
    default:
      throw WireFormatException(
          'unknown opcode: 0x${opcode.toRadixString(16)}');
  }
}

/// Encode a procedure's instruction list to a code-section byte string. `Label`
/// ops are stripped (erased on the wire); a `ClauseNext` target resolves to the
/// procedure-relative index of its label in the stripped sequence. Proc
/// references resolve via [procIndexOf] (the artefact's procedure table).
Uint8List encodeCode(
  List<Object> ops, {
  required ProcIndexOf procIndexOf,
}) {
  // Map each label to the index of the instruction it precedes in the
  // label-stripped sequence (for ClauseNext ctargets).
  final labelToIndex = <String, int>{};
  var idx = 0;
  for (final op in ops) {
    if (op is Label) {
      labelToIndex[op.name] = idx;
    } else {
      idx++;
    }
  }
  final w = WireWriter();
  for (final op in ops) {
    if (op is Label) continue;
    encodeInstruction(w, op,
        procIndexOf: procIndexOf,
        ctargetOf: (label) => labelToIndex[label]!);
  }
  return w.toBytes();
}

/// Decode a code-section byte string back to instruction objects. `proc`
/// indices resolve via [procNameOf]; `ctarget` indices are returned as synthetic
/// labels `#<index>` (Labels were erased, so the original name is unrecoverable;
/// current codegen emits no ClauseNext, so this path is for completeness).
List<Object> decodeCode(
  Uint8List bytes, {
  required ProcNameOf procNameOf,
}) {
  final r = WireReader(bytes);
  final ops = <Object>[];
  while (!r.atEnd) {
    ops.add(decodeInstruction(r,
        procNameOf: procNameOf, ctargetLabelOf: (i) => '#$i'));
  }
  return ops;
}
