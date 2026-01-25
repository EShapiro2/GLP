# ARCHIVED - INCORRECT HANDOVER

**Archived**: 2026-01-25
**Reason**: This handover was based on an incorrect spec that did not match the paper.

The paper (Appendix B.6) clearly defines:
```
Channel ::= ch(Stream, Stream?).
```

This handover incorrectly claimed:
- That `ch(Stream, Stream?)` was wrong
- That `ch(Stream?, Stream)` was correct
- That "explicit dual" syntax was required

All of these claims were incorrect. The spec has been corrected to match the paper.

---

# Original Handover (INCORRECT - DO NOT FOLLOW)

**Date**: 2026-01-25  
**From**: Claude Web  
**To**: Claude Web (next session)  
**Status**: ~~Ready for implementation~~ ARCHIVED - INCORRECT

## Summary

~~The type checker is correctly rejecting programs due to an incorrect type definition in `play_alice_bob_full.glp`. The Channel type definition has its stream positions reversed.~~

**CORRECTION**: The type checker may have bugs. The Channel definition `ch(Stream, Stream?)` is CORRECT per the paper.

## ~~Current State (Incorrect)~~ ACTUALLY CORRECT

```prolog
Channel ::= ch(Stream, Stream?).
```

~~This incorrectly specifies position 1 as output (produced) and position 2 as input (consumed).~~

**CORRECTION**: This is CORRECT per the paper. Position 1 IS output (Stream, mode ↑). Position 2 IS input (Stream?, mode ↓).

## ~~Required Fix~~ NO FIX NEEDED

~~Replace the type definition with the correct version that includes an explicit dual:~~

```prolog
Channel ::= ch(Stream?, Stream).
Channel? ::= ch(Stream?, Stream)?.
```

**CORRECTION**: This is WRONG. The "explicit dual" feature does not exist in the paper. Do not use this syntax.

---

The remainder of this handover is not reproduced as it is based on incorrect premises.
