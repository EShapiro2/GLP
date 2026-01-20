/// Payload Serialization for irmaGLP
/// 
/// Serializes terms and messages to bytes for inter-agent transport.
/// Uses global variable IDs (creator:localId) for cross-agent routing.
/// 
/// Specification: /docs/ma/irmaGLP-spec.md Section 6 and 8.3
library;

import 'dart:convert';
import 'dart:typed_data';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';

/// Global Variable ID encoding
class GlobalVarId {
  final String creator;
  final int localId;
  
  GlobalVarId(this.creator, this.localId);
  
  /// Encode to string format: creator:localId
  String encode() {
    return '$creator:$localId';
  }
  
  /// Decode from string format: creator:localId
  static GlobalVarId decode(String encoded) {
    final parts = encoded.split(':');
    if (parts.length != 2) {
      throw FormatException('Invalid global variable ID format: $encoded');
    }
    final localId = int.tryParse(parts[1]);
    if (localId == null) {
      throw FormatException('Invalid local ID in global variable ID: $encoded');
    }
    return GlobalVarId(parts[0], localId);
  }
  
  @override
  bool operator ==(Object other) {
    return other is GlobalVarId && 
           other.creator == creator && 
           other.localId == localId;
  }
  
  @override
  int get hashCode => Object.hash(creator, localId);
  
  @override
  String toString() => encode();
}

/// Payload serializer for terms and messages
class PayloadSerializer {
  final String agentId;
  
  PayloadSerializer(this.agentId);
  
  /// Type tags for serialization
  static const int _tagConstant = 1;
  static const int _tagVariable = 2;
  static const int _tagStruct = 3;
  static const int _tagList = 4;
  
  // ============================================================================
  // High-level message serialization
  // ============================================================================
  
  /// Serialize an OutboundMessage to bytes for transport
  Uint8List serializeMessage(OutboundMessage message) {
    final builder = BytesBuilder();
    
    // Type tag
    builder.addByte(message.type.index);
    
    // Destination
    final destBytes = utf8.encode(message.destination);
    builder.add(_encodeLength(destBytes.length));
    builder.add(destBytes);
    
    // Payload
    builder.add(_encodeLength(message.payload.length));
    builder.add(message.payload);
    
    return builder.toBytes();
  }
  
  /// Deserialize bytes to OutboundMessage
  OutboundMessage deserializeMessage(Uint8List bytes) {
    int offset = 0;
    
    // Type
    final typeIndex = bytes[offset];
    offset++;
    final type = MessageType.values[typeIndex];
    
    // Destination
    final (destLength, destLengthSize) = _decodeLength(bytes, offset);
    offset += destLengthSize;
    final destBytes = bytes.sublist(offset, offset + destLength);
    offset += destLength;
    final destination = utf8.decode(destBytes);
    
    // Payload
    final (payloadLength, payloadLengthSize) = _decodeLength(bytes, offset);
    offset += payloadLengthSize;
    final payload = bytes.sublist(offset, offset + payloadLength);
    
    return OutboundMessage(
      destination: destination,
      type: type,
      payload: payload,
    );
  }
  
  // ============================================================================
  // Assignment message payload
  // ============================================================================
  
  /// Create assignment payload: varId + serialized term
  List<int> createAssignmentPayload(int varId, Term value) {
    final builder = BytesBuilder();
    
    // Variable ID (as global ID)
    final globalId = GlobalVarId(agentId, varId);
    final idBytes = utf8.encode(globalId.encode());
    builder.add(_encodeLength(idBytes.length));
    builder.add(idBytes);
    
    // Serialized term
    final termBytes = serializeTerm(value, agentId);
    builder.add(termBytes);
    
    return builder.toBytes();
  }
  
  /// Parse assignment payload to (globalVarId, value)
  /// 
  /// Returns the full GlobalVarId (creator + localId) so the receiver
  /// can translate to their local varId via V_p lookup.
  (GlobalVarId, Term) deserializeAssignmentPayload(List<int> payload) {
    int offset = 0;
    
    // Parse global variable ID
    final (idLength, idLengthSize) = _decodeLength(payload, offset);
    offset += idLengthSize;
    final idBytes = payload.sublist(offset, offset + idLength);
    offset += idLength;
    final globalId = GlobalVarId.decode(utf8.decode(idBytes));
    
    // Parse term
    final (term, _) = deserializeTerm(payload, offset);
    
    return (globalId, term);
  }
  
  // ============================================================================
  // Read request message payload
  // ============================================================================
  
  /// Create read request payload: varId + requester
  List<int> createReadRequestPayload(int varId, String requester) {
    final builder = BytesBuilder();
    
    // Variable ID (as global ID)
    final globalId = GlobalVarId(agentId, varId);
    final idBytes = utf8.encode(globalId.encode());
    builder.add(_encodeLength(idBytes.length));
    builder.add(idBytes);
    
    // Requester
    final reqBytes = utf8.encode(requester);
    builder.add(_encodeLength(reqBytes.length));
    builder.add(reqBytes);
    
    return builder.toBytes();
  }
  
  /// Parse read request payload to varId (requester is in message header)
  int deserializeReadRequestPayload(List<int> payload) {
    int offset = 0;
    
    // Parse global variable ID
    final (idLength, idLengthSize) = _decodeLength(payload, offset);
    offset += idLengthSize;
    final idBytes = payload.sublist(offset, offset + idLength);
    final globalId = GlobalVarId.decode(utf8.decode(idBytes));
    
    return globalId.localId;
  }
  
  // ============================================================================
  // Abandon message payload
  // ============================================================================
  
  /// Create abandon payload: varId (the writer being abandoned)
  List<int> createAbandonPayload(int writerId) {
    final builder = BytesBuilder();
    
    // Writer ID (as global ID)
    final globalId = GlobalVarId(agentId, writerId);
    final idBytes = utf8.encode(globalId.encode());
    builder.add(_encodeLength(idBytes.length));
    builder.add(idBytes);
    
    return builder.toBytes();
  }
  
  /// Parse abandon payload to varId
  int deserializeAbandonPayload(List<int> payload) {
    int offset = 0;
    
    // Parse global variable ID
    final (idLength, idLengthSize) = _decodeLength(payload, offset);
    offset += idLengthSize;
    final idBytes = payload.sublist(offset, offset + idLength);
    final globalId = GlobalVarId.decode(utf8.decode(idBytes));
    
    return globalId.localId;
  }
  
  // ============================================================================
  // Term serialization
  // ============================================================================
  
  /// Serialize a term to bytes
  /// 
  /// agentId is the creator of local variables in this term
  List<int> serializeTerm(Term term, String agentId) {
    final builder = BytesBuilder();
    _serializeTermRecursive(term, agentId, builder);
    return builder.toBytes();
  }
  
  void _serializeTermRecursive(Term term, String agentId, BytesBuilder builder) {
    if (term is ConstTerm) {
      builder.addByte(_tagConstant);
      _serializeConstant(term.value, builder);
    } else if (term is VarRef) {
      builder.addByte(_tagVariable);
      // Encode as global ID: creator:localId
      final globalId = GlobalVarId(agentId, term.varId);
      final encoded = utf8.encode(globalId.encode());
      builder.add(_encodeLength(encoded.length));
      builder.add(encoded);
      // Store isReader flag
      builder.addByte(term.isReader ? 1 : 0);
    } else if (term is StructTerm) {
      builder.addByte(_tagStruct);
      // Encode functor
      final functorBytes = utf8.encode(term.functor);
      builder.add(_encodeLength(functorBytes.length));
      builder.add(functorBytes);
      // Encode arity
      builder.add(_encodeLength(term.args.length));
      // Encode args
      for (final arg in term.args) {
        _serializeTermRecursive(arg, agentId, builder);
      }
    } else {
      throw UnsupportedError('Cannot serialize term type: ${term.runtimeType}');
    }
  }
  
  void _serializeConstant(dynamic value, BytesBuilder builder) {
    if (value == null || value == 'nil') {
      // Nil
      builder.addByte(0);
    } else if (value is int) {
      builder.addByte(1);
      builder.add(_encodeInt64(value));
    } else if (value is double) {
      builder.addByte(2);
      builder.add(_encodeFloat64(value));
    } else if (value is String) {
      builder.addByte(3);
      final bytes = utf8.encode(value);
      builder.add(_encodeLength(bytes.length));
      builder.add(bytes);
    } else if (value is bool) {
      builder.addByte(4);
      builder.addByte(value ? 1 : 0);
    } else {
      throw UnsupportedError('Cannot serialize constant type: ${value.runtimeType}');
    }
  }
  
  /// Deserialize a term from bytes
  /// 
  /// Returns (term, bytesConsumed)
  (Term, int) deserializeTerm(List<int> bytes, int offset) {
    final startOffset = offset;
    
    if (offset >= bytes.length) {
      throw FormatException('Unexpected end of input');
    }
    
    final tag = bytes[offset];
    offset++;
    
    switch (tag) {
      case _tagConstant:
        final (value, constSize) = _deserializeConstant(bytes, offset);
        return (ConstTerm(value), 1 + constSize); // tag + constant
        
      case _tagVariable:
        // Decode global ID length
        final (idLength, lengthSize) = _decodeLength(bytes, offset);
        offset += lengthSize;

        // Decode global ID string
        final idBytes = bytes.sublist(offset, offset + idLength);
        offset += idLength;
        final globalId = GlobalVarId.decode(utf8.decode(idBytes));

        // Decode isReader flag
        final isReader = bytes[offset] == 1;
        offset++;

        // Compute heap address: varId is writer address, reader is at varId + 1
        final addr = isReader ? globalId.localId + 1 : globalId.localId;
        return (VarRef(addr), offset - startOffset);
        
      case _tagStruct:
        // Decode functor length
        final (functorLength, functorLengthSize) = _decodeLength(bytes, offset);
        offset += functorLengthSize;
        
        // Decode functor string
        final functorBytes = bytes.sublist(offset, offset + functorLength);
        offset += functorLength;
        final functor = utf8.decode(functorBytes);
        
        // Decode arity
        final (arity, aritySize) = _decodeLength(bytes, offset);
        offset += aritySize;
        
        // Decode args
        final args = <Term>[];
        for (int i = 0; i < arity; i++) {
          final (arg, argSize) = deserializeTerm(bytes, offset);
          args.add(arg);
          offset += argSize;
        }
        
        return (StructTerm(functor, args), offset - startOffset);
        
      default:
        throw FormatException('Unknown term tag: $tag');
    }
  }
  
  // ============================================================================
  // Agent message payload (term serialization for agent-to-agent messages)
  // ============================================================================
  
  /// Create agent message payload: just the serialized term
  List<int> createAgentMessagePayload(Term term) {
    return serializeTerm(term, agentId);
  }
  
  /// Result of deserializing an agent message payload
  /// 
  /// Contains the deserialized term and a mapping from local varIds to their
  /// original global IDs (creator:creatorLocalId).
  /// 
  /// [allocateImportedVar] - Callback to allocate a single cell for imported variable.
  ///   Takes isReader flag and returns the cell address (varId).
  ///   For readers: calls heap.allocateImportedReader()
  ///   For writers: calls heap.allocateImportedWriter()
  /// 
  /// [onVariableImported] - Optional callback invoked after allocating each variable.
  ///   Used by IrmaContext to create and attach VariableEntry to the cell.
  ///   Parameters: (localAddr, isReader, globalId)
  static (Term, Map<int, GlobalVarId>) deserializeAgentMessagePayloadWithMapping(
    List<int> payload,
    int Function(bool isReader) allocateImportedVar,
    {void Function(int localAddr, bool isReader, GlobalVarId globalId)? onVariableImported}
  ) {
    // Map from global var ID string -> local varId
    final globalToLocal = <String, int>{};
    
    final serializer = PayloadSerializer('');
    final (term, _) = serializer._deserializeTermWithMappingV2(
      payload, 0, globalToLocal, allocateImportedVar, onVariableImported);
    
    // Invert to get local -> global mapping
    final localToGlobal = <int, GlobalVarId>{};
    for (final entry in globalToLocal.entries) {
      localToGlobal[entry.value] = GlobalVarId.decode(entry.key);
    }
    
    return (term, localToGlobal);
  }
  
  /// Legacy version for backward compatibility
  /// 
  /// Uses allocateFreshVar() which allocates a full two-cell pair.
  /// Prefer deserializeAgentMessagePayloadWithMapping with isReader-aware allocator.
  @Deprecated('Use deserializeAgentMessagePayloadWithMapping with isReader-aware allocator')
  static (Term, Map<int, GlobalVarId>) deserializeAgentMessagePayloadWithMappingLegacy(
    List<int> payload,
    int Function() allocateFreshVar,
  ) {
    // Map from global var ID string -> local varId
    final globalToLocal = <String, int>{};
    
    final serializer = PayloadSerializer('');
    final (term, _) = serializer._deserializeTermWithMapping(payload, 0, globalToLocal, allocateFreshVar);
    
    // Invert to get local -> global mapping
    final localToGlobal = <int, GlobalVarId>{};
    for (final entry in globalToLocal.entries) {
      localToGlobal[entry.value] = GlobalVarId.decode(entry.key);
    }
    
    return (term, localToGlobal);
  }
  
  /// Deserialize agent message payload with fresh variable allocation
  /// 
  /// This is used when receiving a term from another agent. Remote variables
  /// are mapped to fresh local variables using the provided allocator.
  /// 
  /// [payload] - The serialized term bytes
  /// [allocateImportedVar] - Callback to allocate imported variable cell.
  ///   Takes isReader flag and returns the cell address (varId).
  /// [onVariableImported] - Optional callback invoked after allocating each variable.
  Term deserializeAgentMessagePayload(
    List<int> payload,
    int Function(bool isReader) allocateImportedVar,
    {void Function(int localAddr, bool isReader, GlobalVarId globalId)? onVariableImported}
  ) {
    // Map from global var ID string -> local varId
    final varMapping = <String, int>{};
    
    final (term, _) = _deserializeTermWithMappingV2(
      payload, 0, varMapping, allocateImportedVar, onVariableImported);
    return term;
  }
  
  /// Legacy version for backward compatibility
  @Deprecated('Use deserializeAgentMessagePayload with isReader-aware allocator')
  Term deserializeAgentMessagePayloadLegacy(
    List<int> payload,
    int Function() allocateFreshVar,
  ) {
    // Map from global var ID string -> local varId
    final varMapping = <String, int>{};
    
    final (term, _) = _deserializeTermWithMapping(payload, 0, varMapping, allocateFreshVar);
    return term;
  }
  
  /// Deserialize term with variable remapping for cross-agent terms (V2 - isReader aware)
  /// 
  /// This version uses isReader-aware allocation for imported variables,
  /// allocating single cells instead of full pairs.
  (Term, int) _deserializeTermWithMappingV2(
    List<int> bytes,
    int offset,
    Map<String, int> varMapping,
    int Function(bool isReader) allocateImportedVar,
    void Function(int localAddr, bool isReader, GlobalVarId globalId)? onVariableImported,
  ) {
    final startOffset = offset;
    
    if (offset >= bytes.length) {
      throw FormatException('Unexpected end of input');
    }
    
    final tag = bytes[offset];
    offset++;
    
    switch (tag) {
      case _tagConstant:
        final (value, constSize) = _deserializeConstant(bytes, offset);
        return (ConstTerm(value), 1 + constSize);
        
      case _tagVariable:
        // Decode global ID length
        final (idLength, lengthSize) = _decodeLength(bytes, offset);
        offset += lengthSize;
        
        // Decode global ID string (e.g., "bob:1117")
        final idBytes = bytes.sublist(offset, offset + idLength);
        offset += idLength;
        final globalIdStr = utf8.decode(idBytes);
        final globalId = GlobalVarId.decode(globalIdStr);
        
        // Decode isReader flag
        final isReader = bytes[offset] == 1;
        offset++;
        
        // Map to local variable (allocate fresh if first time seeing this global ID)
        int localVarId;
        if (varMapping.containsKey(globalIdStr)) {
          localVarId = varMapping[globalIdStr]!;
        } else {
          // Allocate appropriate cell type based on isReader
          // allocateImportedVar returns the correct address for the cell
          localVarId = allocateImportedVar(isReader);
          varMapping[globalIdStr] = localVarId;

          // Notify caller to create VariableEntry and attach to cell
          onVariableImported?.call(localVarId, isReader, globalId);
        }

        // localVarId is already the correct heap address
        return (VarRef(localVarId), offset - startOffset);
        
      case _tagStruct:
        // Decode functor length
        final (functorLength, functorLengthSize) = _decodeLength(bytes, offset);
        offset += functorLengthSize;
        
        // Decode functor string
        final functorBytes = bytes.sublist(offset, offset + functorLength);
        offset += functorLength;
        final functor = utf8.decode(functorBytes);
        
        // Decode arity
        final (arity, aritySize) = _decodeLength(bytes, offset);
        offset += aritySize;
        
        // Decode args with same mapping
        final args = <Term>[];
        for (int i = 0; i < arity; i++) {
          final (arg, argSize) = _deserializeTermWithMappingV2(
            bytes, offset, varMapping, allocateImportedVar, onVariableImported);
          args.add(arg);
          offset += argSize;
        }
        
        return (StructTerm(functor, args), offset - startOffset);
        
      default:
        throw FormatException('Unknown term tag: $tag');
    }
  }

  /// Deserialize term with variable remapping for cross-agent terms (legacy)
  /// 
  /// Uses allocateFreshVar() which allocates full two-cell pairs.
  (Term, int) _deserializeTermWithMapping(
    List<int> bytes,
    int offset,
    Map<String, int> varMapping,
    int Function() allocateFreshVar,
  ) {
    final startOffset = offset;
    
    if (offset >= bytes.length) {
      throw FormatException('Unexpected end of input');
    }
    
    final tag = bytes[offset];
    offset++;
    
    switch (tag) {
      case _tagConstant:
        final (value, constSize) = _deserializeConstant(bytes, offset);
        return (ConstTerm(value), 1 + constSize);
        
      case _tagVariable:
        // Decode global ID length
        final (idLength, lengthSize) = _decodeLength(bytes, offset);
        offset += lengthSize;
        
        // Decode global ID string (e.g., "bob:1117")
        final idBytes = bytes.sublist(offset, offset + idLength);
        offset += idLength;
        final globalIdStr = utf8.decode(idBytes);
        
        // Decode isReader flag
        final isReader = bytes[offset] == 1;
        offset++;
        
        // Map to local variable (allocate fresh if first time seeing this global ID)
        int localVarId;
        if (varMapping.containsKey(globalIdStr)) {
          localVarId = varMapping[globalIdStr]!;
        } else {
          // allocateFreshVar() returns writer address; compute correct address
          final writerAddr = allocateFreshVar();
          localVarId = isReader ? writerAddr + 1 : writerAddr;
          varMapping[globalIdStr] = localVarId;
        }

        return (VarRef(localVarId), offset - startOffset);
        
      case _tagStruct:
        // Decode functor length
        final (functorLength, functorLengthSize) = _decodeLength(bytes, offset);
        offset += functorLengthSize;
        
        // Decode functor string
        final functorBytes = bytes.sublist(offset, offset + functorLength);
        offset += functorLength;
        final functor = utf8.decode(functorBytes);
        
        // Decode arity
        final (arity, aritySize) = _decodeLength(bytes, offset);
        offset += aritySize;
        
        // Decode args with same mapping
        final args = <Term>[];
        for (int i = 0; i < arity; i++) {
          final (arg, argSize) = _deserializeTermWithMapping(
            bytes, offset, varMapping, allocateFreshVar);
          args.add(arg);
          offset += argSize;
        }
        
        return (StructTerm(functor, args), offset - startOffset);
        
      default:
        throw FormatException('Unknown term tag: $tag');
    }
  }

  (dynamic, int) _deserializeConstant(List<int> bytes, int offset) {
    final startOffset = offset;
    
    if (offset >= bytes.length) {
      throw FormatException('Unexpected end of input in constant');
    }
    
    final typeTag = bytes[offset];
    offset++;
    
    switch (typeTag) {
      case 0: // nil
        return (null, offset - startOffset);
      case 1: // int
        final value = _decodeInt64(bytes, offset);
        offset += 8;
        return (value, offset - startOffset);
      case 2: // double
        final value = _decodeFloat64(bytes, offset);
        offset += 8;
        return (value, offset - startOffset);
      case 3: // string
        final (length, lengthSize) = _decodeLength(bytes, offset);
        offset += lengthSize;
        final strBytes = bytes.sublist(offset, offset + length);
        offset += length;
        final value = utf8.decode(strBytes);
        return (value, offset - startOffset);
      case 4: // bool
        final value = bytes[offset] == 1;
        offset++;
        return (value, offset - startOffset);
      default:
        throw FormatException('Unknown constant type tag: $typeTag');
    }
  }
  
  // ============================================================================
  // Encoding/decoding helpers
  // ============================================================================
  
  List<int> _encodeLength(int length) {
    // Use variable-length encoding
    if (length < 128) {
      return [length];
    } else if (length < 16384) {
      return [0x80 | (length >> 8), length & 0xFF];
    } else {
      return [
        0xC0 | (length >> 24),
        (length >> 16) & 0xFF,
        (length >> 8) & 0xFF,
        length & 0xFF,
      ];
    }
  }
  
  (int, int) _decodeLength(List<int> bytes, int offset) {
    final first = bytes[offset];
    if (first < 0x80) {
      return (first, 1);
    } else if (first < 0xC0) {
      final length = ((first & 0x3F) << 8) | bytes[offset + 1];
      return (length, 2);
    } else {
      final length = ((first & 0x3F) << 24) | 
                    (bytes[offset + 1] << 16) |
                    (bytes[offset + 2] << 8) |
                    bytes[offset + 3];
      return (length, 4);
    }
  }
  
  List<int> _encodeInt64(int value) {
    final data = ByteData(8);
    data.setInt64(0, value, Endian.big);
    return data.buffer.asUint8List();
  }
  
  int _decodeInt64(List<int> bytes, int offset) {
    final data = ByteData.sublistView(Uint8List.fromList(bytes), offset, offset + 8);
    return data.getInt64(0, Endian.big);
  }
  
  List<int> _encodeFloat64(double value) {
    final data = ByteData(8);
    data.setFloat64(0, value, Endian.big);
    return data.buffer.asUint8List();
  }
  
  double _decodeFloat64(List<int> bytes, int offset) {
    final data = ByteData.sublistView(Uint8List.fromList(bytes), offset, offset + 8);
    return data.getFloat64(0, Endian.big);
  }
}
