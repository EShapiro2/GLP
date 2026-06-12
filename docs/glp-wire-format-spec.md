# GLP Wire Format Specification

**Status: Version 1 — APPROVED (Udi, 2026-06-12). Normative for D3.**

## 1. Scope and Authorities

This document is the normative byte-level specification of GLP on the wire: the canonical encoding of globalized terms and assignment messages, the encoding of the v2.16 instruction set, the shipped-module artefact, deterministic flattening, the loader, and the offer and handshake messages.

Paper authorities, per the Grassroots authority map:

- **IGLP** (arXiv): the canonical encoding — Definition (Canonical Encoding) and its four properties — with the mandate "the byte-level layout of e is normative in the madGLP specification that accompanies the implementation"; modules as shippable values; public keys as agent names. This document is that normative layout.
- **Secure GLP**: the two module identities (source identity h(M), artefact identity), adoption, the conversation handshake, signature kernels. §§5 and 8 transcribe; they do not redesign.
- **TGLP** (modules section): projects, exports, the project interface, reachability ("the linker passes to the compiler the code reachable from the root's exported procedures"), and type automata travelling with compiled modules for load-time checks.
- **glp-bytecode-v216-complete.md**: the instruction set this document encodes. The ISA document remains the authority on instruction semantics; §4 assigns bytes.

Deviations from the current implementation, to be aligned in D3 (code follows spec):

1. Embedded variables are encoded by their global names per Definition Globalize (the globalizing agent's names); the implementation's original-creator identifiers and paired-reader field are irmaGLP residue (master plan, open item 7).
2. The serializer message tail is an encoded variable `_w(q,0)`, not a string marker.
3. The agent identifier is the person's public key; symbolic names are the simulation realisation's choice (IGLP, Remark on agent names).

## 2. Primitive Encodings

All multi-byte integers are big-endian. A term yields the same byte string on every machine.

- **u8** — one byte.
- **i64** — 8 bytes, two's complement.
- **f64** — 8 bytes, IEEE 754 binary64.
- **clen (compact length)** — an unsigned integer below 2^30 in 1, 2, or 4 bytes: values below 128 in one byte; below 16384 in two bytes, the first with high bits `10`; otherwise four bytes, the first with high bits `11`. The remaining bits, big-endian, carry the value. Encoders emit the shortest form; decoders reject longer-than-necessary forms (one value, one encoding).
- **string** — clen byte count, then UTF-8 bytes.
- **bytes** — clen byte count, then raw bytes.
- **hash** — 32 raw bytes: SHA-256 of the hashed content. h(M) is the hash of the flattened source bytes (§6); the artefact identity is the hash of the artefact bytes (§5).
- **agent** — bytes; its content is the public key of the agent's person. The current realisation uses Ed25519 public keys (32 bytes); the simulation realisation uses UTF-8 symbolic names. The encoding does not interpret the content.

## 3. Term and Message Encoding

This section is the byte-level layout of the canonical encoding e of IGLP: an injective function from globalized terms and assignment messages to byte strings.

### 3.1 Terms

A globalized term is encoded by a tagged recursion. Each node opens with a u8 tag:

- **1 constant** — followed by a u8 constant tag and its payload:
  - **0 nil** — no payload (the empty list).
  - **1 integer** — i64.
  - **2 float** — f64.
  - **3 string** — string.
  - **4 boolean** — u8: 0 false, 1 true.
  - **5 blob** — bytes; opaque byte strings, the form in which compiled modules ship (§5).
- **2 variable** — a global name per Definition Globalize: u8 polarity (0 writer `_w(p,i)`, 1 reader `_r(p,i)`), agent p, clen index i. Names are the globalizing agent's; no other variable representation exists on the wire.
- **3 structure** — string functor, clen arity n, then the n argument encodings in order.

Lists are structures: a cell is the structure `.`/2; the empty list is the constant nil.

The four properties of the canonical encoding hold by this layout: hardware-independent (§2 pins widths and byte order); address-free (a variable appears only as its global name with polarity); globalizer's names (tag 2 admits nothing else); canonical on ground terms (a ground term contains tags 1 and 3 only, so its bytes are agent-independent — these are the bytes over which terms are signed and hashed). Injectivity holds by unique decoding: every tag determines its payload form, every length is explicit, and clen is one-value-one-encoding.

### 3.2 Messages

An assignment message `G := T↑` is encoded as: u8 polarity of G, agent of G, clen index of G, then the encoding of T↑.

A serializer (cold-call) message to agent q is the assignment `_w(q,0) := [T↑ | _w(q,0)]`: polarity 0, agent q, index 0, then the encoding of the list cell whose head is T↑ and whose tail is the variable `_w(q,0)` (tag 2, polarity 0, agent q, index 0). The receiver treats index 0 by serializer semantics: append, update the entry, do not remove it.

### 3.3 Signed and Hashed Content

The canonical bytes of a ground term T are e(T). Protocols that sign or hash compound content encode it as a ground term and apply e; the signature kernels of Secure GLP sign e of the pair of the module identity and the term, in the message grammar of §8.

## 4. Instruction Encoding

Authority: `glp-bytecode-v216-complete.md` (instruction semantics; its §15 anticipates this encoding). This section assigns bytes to the v2.16 instruction set. An encoded instruction is a u8 opcode followed by its operands, in the order listed.

### 4.1 Operand kinds

- **polarity** — u8: 0 writer, 1 reader (the `isReader` flag; same convention as §3.1 tag 2).
- **negated** — u8: 0 plain, 1 negated guard (`~G`).
- **varIndex, argSlot, arity, count, slots, regIndex, importIndex** — clen.
- **constant** — the constant payload of §3.1 (u8 constant tag, then its payload); one constant representation serves terms and code.
- **functor** — string.
- **proc** — clen index into the artefact's procedure table (§5). Procedure references on the wire are table indices, never names and never raw program counters.
- **ctarget** — clen instruction index within the current procedure's code (clause targets are procedure-relative). Assembly labels do not exist on the wire.

### 4.2 Opcode table

| Opcode | Mnemonic | Operands |
|---|---|---|
| 0x01 | clause_try | — |
| 0x02 | clause_next | ctarget |
| 0x03 | no_more_clauses | — |
| 0x04 | commit | — |
| 0x05 | proceed | — |
| 0x06 | halt | — |
| 0x07 | nop | — |
| 0x10 | head_constant | constant, argSlot |
| 0x11 | head_nil | argSlot |
| 0x12 | head_structure | functor, arity, argSlot |
| 0x13 | head_list | argSlot |
| 0x14 | head_variable | polarity, varIndex |
| 0x15 | get_variable | polarity, varIndex, argSlot |
| 0x16 | get_value | polarity, varIndex, argSlot |
| 0x20 | unify_variable | polarity, varIndex |
| 0x21 | unify_constant | constant |
| 0x22 | unify_void | count |
| 0x23 | unify_structure | functor, arity |
| 0x24 | push | regIndex |
| 0x25 | pop | regIndex |
| 0x30 | put_variable | polarity, varIndex, argSlot |
| 0x31 | put_constant | constant, argSlot |
| 0x32 | put_nil | argSlot |
| 0x33 | put_list | argSlot |
| 0x34 | put_structure | functor, arity, argSlot |
| 0x35 | set_variable | polarity, varIndex |
| 0x36 | set_constant | constant |
| 0x37 | allocate | slots |
| 0x38 | deallocate | — |
| 0x39 | put_bound_const | constant, argSlot |
| 0x3A | put_bound_nil | argSlot |
| 0x40 | guard | proc, arity, negated |
| 0x41 | ground | varIndex, negated |
| 0x42 | known | varIndex, negated |
| 0x43 | unknown | varIndex |
| 0x44 | no_readers | varIndex, negated |
| 0x45 | ground_equal | varIndex, varIndex, negated |
| 0x46 | otherwise | — |
| 0x50 | spawn | proc, arity |
| 0x51 | requeue | proc, arity |
| 0x52 | distribute | importIndex, functor, arity |
| 0x53 | transmit | varIndex, functor, arity |

### 4.3 Reserved and excluded

Opcodes 0x47–0x4F are reserved for the comparison guards of ISA §19.3 (guard_less and its five siblings), assigned when they are implemented; their arrival is an ISA version change carried in the artefact header (§5). The following implementation classes are not part of the wire format: Label (assembly-time, erased by §4.1); TryNextClause (unused per ISA §2.3); GuardFail, SuspendEnd, TailStep, BodySetConst, BodySetStructConstArgs, HeadBindWriter, GuardNeedReader, and the *Arg test variants (legacy). D3 verifies that the set of instructions codegen emits equals the table above; a gap in either direction is reported upstream before code changes, per the consistency rule.

## 5. Module Artefact

Authority: IGLP (a module ships as opaque bytes inside an ordinary assignment message), Secure GLP (the two identities; the receiver verifies the artefact hash against the bytes and takes the link to h(M) on the sender's attestation), TGLP (a compiled module carries what the load-time interface check needs).

The artefact is the byte string a compiler produces from a flattened module; it travels as a blob constant (§3.1 tag 5). Its **artefact identity** is the SHA-256 of the entire artefact byte string. Its sections, in order:

1. **Header** — magic `GLPW` (4 bytes); u8 wire-format version (this document: 1); string ISA version (`2.16.3`); hash h(M), the claimed source identity; string module name.
2. **Interface table** — the project interface, carried as declaration source text, from which the loader derives the type automata: (\ia) string: the reachable type definitions, in the canonical print of §6; (\ib) clen export count, then per export: string name, clen arity, string declaration text. Carrying text rather than compiled automata keeps one source of truth: type identity is source-based throughout (Secure GLP), and equal sources define equal automata.
3. **Procedure table** — clen count; per procedure: string name, clen arity, clen byte offset into the code section, clen byte length. The `proc` operands of §4 index this table. Exported procedures are those named in the interface table; the loader aliases exactly them.
4. **Code section** — clen byte count, then the concatenated procedure bodies in the instruction encoding of §4. There is no constant pool: constants are inline in instructions (§4.1).

The artefact is code only. Source distribution is an ordinary value exchange: the flattened source travels as a blob in a term, and the receiver verifies it by hashing against h(M).

## 6. Deterministic Flattening

Authority: Secure GLP ("the project flattened at the source level by a deterministic flattening"); TGLP (project compilation; the linked program is the code reachable from the root's exported procedures); the project-compilation spec (the pipeline).

The **flattened source** of a project — the preimage of h(M) — is the canonical print of the linked, pruned program: the project is discovered, type-checked, renamed, resolved, and pruned to the procedures reachable from the root's exported entry points, per the project-compilation spec; the result is printed canonically and hashed.

Canonical print: UTF-8, LF line ends, no comments, tokens separated by single spaces, one declaration or clause per line. Order: (\ia) the reachable type definitions, lexicographically by type name; (\ib) the exported procedure declarations, lexicographically by name then arity; (\ic) the procedures, lexicographically by renamed name then arity — within a procedure, clauses keep their source order, which is semantic (first-applicable-clause selection). Procedure order is not semantic and is fixed by sorting.

The same byte string is what a digital social contract's participants read and agree to, what the handshake compares by hash, and what versioning names.

## 7. Loader

Authority: IGLP (the receiver loads at runtime; the loader adapts the fixed shipped form to the local engine), Secure GLP (A2, adoption), TGLP (load-time interface checks; dynamic linking).

Given a received artefact and the adoption context (the offered h(M), the sender's attested certification of the artefact-to-source link), the loader:

1. Computes SHA-256 of the artefact bytes and verifies it equals the certified artefact identity; verifies the header's h(M) equals the offered h(M); refuses unsupported wire-format or ISA versions (§9).
2. Derives the type automata from the interface table's declaration text and runs the load-time interface checks of the module-system spec §7 when linking the module against local callers: the callee's exported type must accept the caller's imported type.
3. Decodes the code section into the engine's instruction objects per §4, resolving proc indices through the procedure table; generates unqualified aliases for the exported procedures only.
4. Registers the module under (h(M), artefact identity); artefacts are cached and deduplicated by artefact identity.

Admission of conversation traffic is not the loader's: the runtime admits application traffic on a conversation only after its handshake succeeds (Secure GLP A2), and aborts failsafe otherwise.

## 8. Offer and Handshake Messages

Authority: Secure GLP §Mechanisms; transcribed, not redesigned. These are ground terms between runtimes on the attested channel, encoded per §3; hash values inside terms are blob constants of 32 bytes.

- **offer(HSrc, tau(HSrcDef, TypeName))** — the adoption offer: the contract's module identity, and the root channel's type identity — the pair of the module identity of the type's defining source and the type's name (a string).
- **accept** / **decline** — the consent outcome of the offeree's person.
- **ship(Artefact, HBin, HSrc)** — the module bytecode as a blob, its artefact identity, and the source identity it is certified to compile from; the certification is the sender's attested runtime speaking on the attested channel (A1, A2).
- **handshake(HSrc, tau(HSrcDef, TypeName))** — exchanged by the two runtimes on a conversation before any application traffic; both ends must present equal values, or the runtime aborts the conversation failsafe. Derived variables undergo no handshake.
- **Signed content** — the kernel `sign(Term?, Sig)` signs the bytes e(sig(HSrc, Term)), the canonical encoding of the 2-ary structure `sig` whose arguments are the calling instance's module identity and the ground term; `verify` checks the same bytes. The functor `sig` is fixed by this specification.

## 9. Format Versioning

The header carries two versions: the wire-format version (u8) — this document, version 1 — and the ISA version (string), maintained by the ISA document. A loader refuses an artefact whose wire-format version or ISA version it does not support; refusal at adoption time means the offer fails before any conversation exists. New instructions enter by ISA version (reserved ranges, §4.3); changes to the encodings of §§2–3 or the artefact layout of §5 are wire-format version changes. Module versioning — a version is h(M); a new version is a new contract — is Secure GLP's and is not restated here.
