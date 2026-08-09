# Declaration parameters: bug report and proposals

**Date:** 2026-07-22 (updated same day with the paper-bug finding and proposals)
**Reporter:** GLP Code session (goal-check scope-chain work)
**Status:** open — checker enforcement implemented and parked; two paper decisions pending (Udi)
**Blocking:** yes, for the checker enforcement (Part C); the runtime and both test suites remain green meanwhile

## The rule

TGLP now specifies (`sections/parameterized-types.tex` §Declaration parameters, commit `b0cb42a`):

> The parameters of a procedure declaration are exactly the undefined names occurring within the arguments of template instantiations in it: `X` is a parameter of `merge` through its occurrence in `Stream(X)`, and may also occur bare elsewhere in the declaration.  Any other undefined name in a declaration is an error, so a misspelt type name is rejected rather than read as a parameter.

Motivation: previously an undefined type name in a declaration was silently classified as a type parameter (definedness was the sole discriminator), so a misspelt or out-of-scope type name turned the procedure parametric instead of being reported.  This is how the p99 goal-check bug hid: with the ancestor scope missing, `IntList`/`Req`/`Maybe` read as parameters.

## Bug 1 (code, GLP-ICLP): `lib/routing` relies on bare-only parameters

Enforcing the rule in the checker (`_detectProcTypeParams` in `glp_runtime/lib/analysis/type_checker/param_expansion.dart`) and sweeping all 921 `.glp` files under `programs/` with the real load-time classification finds exactly four violating files — all GLP-ICLP-owned, none TGLP-owned:

| File | Bare-only parameter |
|---|---|
| `lib/routing/output.glp` | `M` in `send_user` (l.12), `send_net` (l.19), `send_friend` (l.26), `send_child` (l.34), `send_parent` (l.42) |
| `lib/routing/befriend.glp` | `M` in `imported procedure output#send_user(M?, …)` (l.4) |
| `lib/routing/inject.glp` | `R` in `inject_msg(R?, …)` (l.11) |
| `lib/routing/intro.glp` | `IR` in `intro_await_peer(…, IR)` (l.13) |

In each, the parameter occurs only bare — never inside a template-instantiation argument — so under the rule it is an unknown type, and the declaration is rejected with a located diagnostic (e.g. `Unresolved type: M at line 12`).

**Cascade:** root `programs/self.glp` contains `-expose(lib#routing#befriend).`, so the violation reaches every program load.  With enforcement on, both test suites go red wholesale.  This is why the enforcement cannot land first.

**Not accidental.**  The old classifier's own documentation named this style as a deliberate case: "a carried-message type that has no constructor wrapper and no common supertype — e.g. a `lib/` router's message argument — can only appear here."  The routers are parametric in a message type that genuinely never occurs inside an instantiation; the natural-instantiation route was judged unavailable when the library was written.

**Rejected workaround.**  A parameterised alias `Msg(M) ::= M.` with routers re-declared as `send_user(Msg(M)?, …)` would satisfy the rule's letter, but it is a vacuous identity type existing only to smuggle a parameter binder — a workaround, withdrawn.  Its very availability exposed Bug 2.

## Bug 2 (paper, TGLP): a parameter may constitute an alternative

Definition def:parameterized-type says the parameters "may occur in place of type names within $A_1, \ldots, A_n$" — and an alternative may itself be a bare type name (root `self.glp`: `Exp ::= Number ; +(Exp, Exp) ; …`).  A parameter standing in that position makes the whole alternative a parameter, licensing two degeneracies:

- the pure alias `Msg(M) ::= M.` (the rejected workaround above);
- a union alternative that swallows the parameter's entire language, defeating structural clause selection — the implementation carries a special routing case (`paramUsedAsTypeAlternative`) for exactly this.

A corpus scan finds **no** `.glp` file using a parameter as a bare alternative, so restricting it costs nothing.

**Proposal (paper edit).**  In Definition def:parameterized-type (`parameterized-types.tex`, line 36), after "may occur in place of type names within $A_1, \ldots, A_n$", insert:

> `, but no parameter may itself constitute an alternative: a parameter occurs only properly within an alternative`

**Checker consequence:** `paramUsedAsTypeAlternative` stops being a routing case and becomes a located rejection, with a negative fixture.  Folded into the parked Part C.

## Proposal for Bug 1: explicit parameter form (paper edit, then code)

Restructuring the routers so each parameter occurs inside a real instantiation would force a constructor wrapper on every routed message across all platforms — invasive, and the wrapper would be as artificial as `Msg(M)`.  The right fix is to extend the rule:

> A declaration's parameters are the undefined names occurring within template-instantiation arguments, **plus those the declaration explicitly declares**; any other undefined name is an error.

This keeps the rule's whole point — a misspelt name is neither applied in an instantiation nor declared, so it is rejected — while legalizing the four `lib/routing` declarations with a one-line change each and no restructuring.  Implicit binding through `Stream(X)` stays as is, so `merge` and the rest of the corpus need no annotation.

The concrete syntax of the explicit parameter form is language design — Udi's choice (e.g. `procedure send_user(M?, Stream(Ent)?, Stream(Ent)) parameters M.`, or a marker on the name).  Sequence once chosen:

1. Udi/Chat amends the §Declaration parameters paragraph (and the syntax).
2. Code re-derives the checker rule from the amended text.
3. GLP-ICLP's four files gain the explicit parameter declarations (one line each; cross-project request below, or waived by Udi).
4. The parked Part C lands: enforcement, negative/positive fixtures, `typed-glp-manual.md` and `glp-cheat-sheet.md` parameter sections updated from the revised paper, both suites green sequentially, commit by explicit filename.

## Cross-project request (drafted for relay)

> **To:** GLP-ICLP.  **What:** Add explicit parameter declarations to the four `lib/routing` files above, per TGLP's amended Declaration-parameters rule (syntax per the amendment).  **Why:** the rule makes misspelt type names rejectable instead of silently read as parameters; the checker enforcement is implemented and ready to land; root `self.glp`'s expose of `lib#routing#befriend` makes these four files block every program load once enforcement is on.  **Blocking:** yes — for the checker enforcement only; the runtime remains green meanwhile.

## State of the parked work

- The checker change is ~60 lines replacing `_detectProcTypeParams` and its collector in `param_expansion.dart`; it throws the existing located `UnknownTypeError`, which `checkModule`'s Issue-19 boundary surfaces as `Unresolved type: <name> at line N`.  Re-derivable from the paper paragraph; a copy is parked in the session scratchpad.
- Verified while in place: negative — `procedure q(FooBar?, FooBar).` rejected with `Unresolved type: FooBar at line 2`; positive — root `self.glp` loads (`receive`'s bare `X` is legal via `OpenStream(X)`), p99 and `scope_chain` unaffected.
- Pending Udi: (a) the Bug 2 definition restriction (edit text above, ready to apply on disk for `git diff` review); (b) the explicit-parameter-form syntax for Bug 1.
