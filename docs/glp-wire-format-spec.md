# GLP Wire Format Specification

**Status: DRAFT — §§1–3 for Udi's review; §§4–9 to follow.**

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

## 4. Instruction Encoding — TO FOLLOW (authority: glp-bytecode-v216-complete.md)

## 5. Module Artefact — TO FOLLOW (authority: IGLP, Secure GLP, TGLP)

## 6. Deterministic Flattening — TO FOLLOW (authority: TGLP; DCE per the compilation spec)

## 7. Loader — TO FOLLOW (authority: IGLP, Secure GLP, TGLP §7 checks)

## 8. Offer and Handshake Messages — TO FOLLOW (authority: Secure GLP)

## 9. Format Versioning — TO FOLLOW
