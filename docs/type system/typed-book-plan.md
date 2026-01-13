# Typed Book Programs - Type Annotation Plan

**Version**: 1.3  
**Date**: 2026-01-13  
**Status**: In Progress

## Overview

This document tracks adding correct type declarations to all programs in `/programs/typed_book/`. The type checker reports ALL errors (does not stop at first error).

## Goals

1. Add correct type declarations to ALL typed book programs
2. Use predefined types where possible
3. Document type checker bugs encountered
4. Do NOT skip programs - fix all regardless of whether they pass

## Type Declaration Fixes Required

| Issue | Fix |
|-------|-----|
| Undefined `Any` type | Replace with `_` (wildcard) |
| Missing mode annotations | Add `?` for input arguments |
| Predefined type redefinitions | Remove `List ::= ...`, use predefined |
| Missing procedure declarations | Add `procedure name(Type?, Type)` |

## Predefined Types (from prelude)

Available without definition:
- `List ::= [_ | List] ; []`
- `Stream ::= [_ | Stream] ; []`
- `DiffList ::= List \ List?`
- `Channel ::= ch(Stream?, Stream)`
- Primitives: `Integer`, `Real`, `Number`, `String`, `_`

## Type Checker Bugs Found

| Bug ID | Description | Test File |
|--------|-------------|-----------|
| BUG-001 | Universal type `_` doesn't accept structured terms. DFA has no transitions for compound terms from `_?` state. Error: "No transition for foo(n,m):↓ from state _?" | `positive/book/universal_accepts_structured.glp` |
| BUG-002 | Guard `tuple/1` undefined in prelude | (annotation issue, needs prelude update) |

## Batches

### Batch 1: Simple Arithmetic (13 files)
**Directory:** `recursive/arithmetic_trees/`
**Status:** ✅ Annotations Fixed

| File | Annotations Fixed | Known Issues |
|------|-------------------|--------------|
| `ackermann.glp` | ✅ | BUG-001, BUG-002 |
| `exp.glp` | ✅ | BUG-001, BUG-002 |
| `factorial.glp` | ✅ | BUG-001 |
| `fibonacci.glp` | ✅ | BUG-001 |
| `gcd_integer.glp` | ✅ | BUG-001 |
| `hanoi.glp` | ✅ | BUG-001 |
| `lesseq.glp` | ✅ | BUG-001 |
| `min.glp` | ✅ | BUG-001, BUG-002 |
| `natural_numbers.glp` | ✅ | BUG-001 |
| `plus.glp` | ✅ | BUG-001 |
| `primes.glp` | ✅ | BUG-001 |
| `sum_list.glp` | ✅ | BUG-001 |
| `times.glp` | ✅ | BUG-001, BUG-002 |

### Batch 2: Basic List Processing (15 files)
**Directory:** `recursive/list_processing/`
**Status:** ✅ Annotations Fixed

| File | Annotations Fixed | Known Issues |
|------|-------------------|--------------|
| `append.glp` | ✅ | BUG-001 |
| `copy.glp` | ✅ | BUG-001 |
| `delete.glp` | ✅ | BUG-001 |
| `dl_append.glp` | ✅ | None (passes) |
| `filter_even.glp` | ✅ | BUG-001 |
| `is_list.glp` | ✅ | BUG-001 |
| `length.glp` | ✅ | BUG-001 |
| `map_inc.glp` | ✅ | BUG-001 |
| `maxlist.glp` | ✅ | BUG-001 |
| `member.glp` | ✅ | BUG-001 |
| `nth.glp` | ✅ | BUG-001 |
| `prefix.glp` | ✅ | BUG-001 |
| `reverse.glp` | ✅ | None (passes) |
| `reverse_naive.glp` | ✅ | None (passes) |
| `translate.glp` | ✅ | BUG-001 |

### Batch 3: Complex List Processing (11 files)
**Directory:** `recursive/list_processing/`
**Status:** ⬜ Not Started

| File | Annotations Fixed | Known Issues |
|------|-------------------|--------------|
| `bubble_sort.glp` | ⬜ | |
| `flatten.glp` | ⬜ | |
| `inner_product.glp` | ⬜ | |
| `inner_product_iter.glp` | ⬜ | |
| `insertion_sort.glp` | ⬜ | |
| `merge_ordered.glp` | ⬜ | |
| `merge_sort.glp` | ⬜ | |
| `polygon_area.glp` | ⬜ | |
| `quicksort.glp` | ⬜ | |
| `variants/flatten_original.glp` | ⬜ | |
| `variants/quicksort_original.glp` | ⬜ | |

### Batch 4: Structure Processing (11 files)
**Directory:** `recursive/structure_processing/`
**Status:** ⬜ Not Started

| File | Annotations Fixed | Known Issues |
|------|-------------------|--------------|
| `ancestor.glp` | ⬜ | |
| `binary_tree.glp` | ⬜ | |
| `distribute_nonground.glp` | ⬜ | |
| `heapify.glp` | ⬜ | |
| `list_to_bst.glp` | ⬜ | |
| `observe.glp` | ⬜ | |
| `observe_minimal.glp` | ⬜ | |
| `observe_play.glp` | ⬜ | |
| `substitute.glp` | ⬜ | |
| `traversals.glp` | ⬜ | |
| `tree_sum.glp` | ⬜ | |

### Batch 5: Stream Basics (22 files)
**Directory:** `streams/producers_consumers/`
**Status:** ⬜ Not Started

| File | Annotations Fixed | Known Issues |
|------|-------------------|--------------|
| `biased_merge.glp` | ⬜ | |
| `channels.glp` | ⬜ | |
| `cooperative.glp` | ⬜ | |
| `cooperative_producers.glp` | ⬜ | |
| `distribute.glp` | ⬜ | |
| `distribute_binary.glp` | ⬜ | |
| `distribute_ground.glp` | ⬜ | |
| `distribute_indexed.glp` | ⬜ | |
| `dynamic_merger.glp` | ⬜ | |
| `fair_merge.glp` | 🔧 | |
| `merge_dynamic.glp` | ⬜ | |
| `merge_simple.glp` | ⬜ | |
| `merge_tree.glp` | ⬜ | |
| `mwm.glp` | ⬜ | |
| `observer.glp` | ⬜ | |
| `observers.glp` | ⬜ | |
| `parallel_table.glp` | ⬜ | |
| `producer_consumer.glp` | ⬜ | |
| `producer_consumer_countdown.glp` | ⬜ | |
| `buffered/bounded_buffer.glp` | ⬜ | |
| `buffered/bounded_buffer_original.glp` | ⬜ | |
| `buffered/switch2x2.glp` | ⬜ | |

### Batch 6: Monitors/Objects (10 files)
**Directory:** `streams/objects_monitors/`
**Status:** ⬜ Not Started

### Batch 7: Constants/Circuits (3 files)
**Directory:** `constants/`
**Status:** ⬜ Not Started

### Batch 8: Meta Interpreters (12 files)
**Directory:** `meta/`
**Status:** ⬜ Not Started

### Batch 9: Modules (2 files)
**Directory:** `modules/`
**Status:** ⬜ Not Started

### Batch 10: Social Graph (24 files)
**Directory:** `social_graph/`
**Status:** ⬜ Not Started

### Batch 11: Social Networks (17 files)
**Directory:** `social_networks/`
**Status:** ⬜ Not Started

### Batch 12: Constitutional Consensus (6 files)
**Directory:** `constitutional_consensus/`
**Status:** ⬜ Not Started

### Batch 13: Cryptocurrencies (6 files)
**Directory:** `cryptocurrencies/`
**Status:** ⬜ Not Started

## Progress Summary

| Batch | Files | ✅ Fixed | ⬜ Pending |
|-------|-------|----------|-----------|
| 1. Arithmetic | 13 | 13 | 0 |
| 2. Basic List | 15 | 15 | 0 |
| 3. Complex List | 11 | 0 | 11 |
| 4. Structure | 11 | 0 | 11 |
| 5. Stream Basics | 22 | 1 | 21 |
| 6. Monitors | 10 | 0 | 10 |
| 7. Constants | 3 | 0 | 3 |
| 8. Meta | 12 | 0 | 12 |
| 9. Modules | 2 | 0 | 2 |
| 10. Social Graph | 24 | 0 | 24 |
| 11. Social Networks | 17 | 0 | 17 |
| 12. Consensus | 6 | 0 | 6 |
| 13. Crypto | 6 | 0 | 6 |
| **Total** | **152** | **29** | **123** |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-01-12 | Initial plan |
| 1.1 | 2026-01-12 | Added workflow details |
| 1.2 | 2026-01-13 | BUG-001 identified |
| 1.3 | 2026-01-13 | Clarified: fix ALL annotations, don't skip any. Type checker reports all errors. |
