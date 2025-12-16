# Claude Code Instructions for AofGLP Programs

## Overview

This directory contains GLP programs for "The Art of Grassroots Logic Programming" book. Programs are organised by chapter topic. Each subdirectory may contain:
- `.glp` source files
- `README.md` with chapter-specific instructions
- Test files (`test_*.glp`)
- Play files (`play_*.glp`)

## Running Programs

### Setup

1. Ensure Dart SDK is installed and in PATH:
   ```bash
   export PATH="/home/user/dart-sdk/bin:$PATH"
   dart --version
   ```

2. Navigate to the GLP runtime directory:
   ```bash
   cd /home/user/GLP/glp_runtime
   ```

3. Compile the REPL (if not already compiled):
   ```bash
   dart compile exe bin/glp_repl.dart -o glp_repl
   ```

### Running a Program

From the `glp_runtime` directory:

```bash
# Interactive mode
./glp_repl
> 23_cryptocurrencies/gc.glp
> play_mutual_credit(Result).

# Non-interactive mode
echo -e '23_cryptocurrencies/play_payment.glp\nplay_payment(R).' | ./glp_repl
```

### Program Locations

Programs should be placed in or symlinked from `/home/user/GLP/glp_runtime/glp/` since the REPL prepends `glp/` to file paths.

Alternatively, copy or symlink the AofGLP directory:
```bash
ln -s /path/to/AofGLP /home/user/GLP/glp_runtime/glp/AofGLP
```

Then run:
```bash
./glp_repl
> AofGLP/23_cryptocurrencies/gc.glp
> test_balance(Result).
```

## Testing Protocol

### Before Making Changes

1. Run baseline tests to ensure everything passes
2. Note the number of passing tests
3. Commit current state

### Running Tests

Each chapter directory may have test files. Run them individually:

```bash
# Load the main module and test file
> 23_cryptocurrencies/gc.glp
> 23_cryptocurrencies/test_balance.glp
> test_balance(Result).
```

Or run plays:
```bash
> 23_cryptocurrencies/play_payment.glp
> play_payment(Result).
```

### After Making Changes

1. Re-run all tests in the affected directory
2. Ensure no regressions
3. Report results

## GLP Key Rules

### No I/O Predicates
GLP has no `write/1`, `nl/0`, or other I/O predicates. All output is through procedure arguments. Tests return `passed` or `failed(...)` through an output argument.

### Empty Body
An empty body is written as `| true.` not just `| .`:
```prolog
is_leader(Agent, Wave, Participants) :-
    leader_of_wave(Wave?, Participants?, Leader),
    Agent? =?= Leader? | true.
```

### Guard Negation
Use `~G` for negatable guards (type guards and `=?=`), not `\+`:
```prolog
filter_nonempty([block(R, Payload, P)|Bs], [block(R?, Payload?, P?)|Rest?]) :-
    ~(Payload? =?= empty) |
    filter_nonempty(Bs?, Rest).
```

### Arithmetic Guards
- `=:=` numeric equality
- `=\=` numeric inequality  
- `<`, `>`, `=<`, `>=` comparisons

These cannot be negated with `~`.

### Otherwise
Use `otherwise` for default cases (when all prior clauses failed or suspended):
```prolog
determine_mode(_, high) :- otherwise | true.
```

### wait/1 is a Guard
`wait(Duration)` suspends for Duration milliseconds. It's a guard, not a body predicate:
```prolog
wait_for_leader(Timeout, Block, timeout) :-
    wait(Timeout?),
    unknown(Block?) | true.
```

## Chapter 23: Grassroots Cryptocurrencies

### Files

| File | Description |
|------|-------------|
| `gc.glp` | Core implementation: agent, handlers, balance management, redemption |
| `play_mutual_credit.glp` | Alice and Bob exchange 100 personal coins |
| `play_payment.glp` | Three-agent payment: Alice pays Carol in Bob-coins |
| `play_redemption.glp` | Bob redeems Alice-coins with preference list |
| `test_balance.glp` | Unit tests for get_balance/set_balance |
| `test_repayments.glp` | Unit tests for compute_repayments |

### Expected Results

All plays should return `passed`. Tests should return lists of `passed` results.

### Key Things to Watch For

1. **SRSW Compliance**: Every variable must have exactly one writer occurrence per clause.

2. **Reader/Writer Modes**: In clause heads:
   - Writers receive values from goal arguments
   - Readers are bound to goal argument readers

3. **Guard Placement**: Guards must come before the commit (`|`).

4. **Stream Threading**: The agent pattern threads streams through `Stream, Stream1`:
   ```prolog
   Stream = [block(...) | Stream1?]
   ```

### Debugging

Enable tracing in the REPL:
```
> :trace
> play_mutual_credit(R).
```

## Chapter 24: Constitutional Consensus

### Files

| File | Description |
|------|-------------|
| `consensus.glp` | Core implementation: waves, rounds, blocks, finality, agent, τ ordering |
| `play_low_throughput.glp` | Single transaction, no conflict, finalized in one wave |
| `play_high_throughput.glp` | Two transactions conflict, leader wins, retry next wave |
| `play_agents.glp` | Multi-agent simulation using the agent process |
| `test_waves.glp` | Unit tests for wave/round utilities, leader selection |
| `test_blocklace.glp` | Unit tests for blocklace operations, conflict detection |

### Key Concepts

1. **Constitution**: `(P, σ, Δ)` — participants, majority threshold (0.5 with attestation), timeout

2. **Wave Structure**: 3 rounds per wave
   - Round 1: Candidates (propose transaction or empty)
   - Round 2: Endorsements (point to candidate)
   - Round 3: Ratifications (point to endorsements)

3. **Finality**: Block finalized when majority endorses AND majority ratifies

4. **Modes**:
   - Low-throughput: spontaneous leader (any agent can propose)
   - High-throughput: formal leader (round-robin by wave)

5. **τ Function**: Orders finalized blocks into sequence; all correct agents compute same sequence

### Running Tests

```bash
# Wave and round utilities
> AofGLP/24_consensus/consensus.glp
> AofGLP/24_consensus/test_waves.glp
> test_waves(Results).

# Blocklace operations
> AofGLP/24_consensus/test_blocklace.glp
> test_blocklace(Results).

# Low-throughput scenario
> AofGLP/24_consensus/play_low_throughput.glp
> play_low_throughput(Result).
> test_finality(Result).

# High-throughput scenario (conflict)
> AofGLP/24_consensus/play_high_throughput.glp
> play_high_throughput(Result).
> test_conflict(Result).
> test_no_conflict(Result).

# Agent process
> AofGLP/24_consensus/play_agents.glp
> test_agent_propose(Result).
> test_agent_endorse(Result).
```

### Expected Results

- `test_waves(R)` → R contains all `passed`
- `test_blocklace(R)` → R contains all `passed`
- `play_low_throughput(R)` → R = passed
- `play_high_throughput(R)` → R = passed
- `test_finality(R)` → R = passed
- `test_conflict(R)` → R = passed

### Key Things to Watch For

1. **Round Arithmetic**: Rounds are 1-indexed. Wave k has rounds 3k-2, 3k-1, 3k.
   - `wave_of_round(R, W)` — W := (R-1) // 3 + 1
   - `round_in_wave(R, P)` — P := ((R-1) mod 3) + 1

2. **Leader Selection**: Round-robin by wave number
   - `leader_of_wave(Wave, Participants, Leader)` — Leader := Participants[(Wave-1) mod |P|]

3. **Majority**: Strictly greater than half (>n/2, not ≥)
   - `is_majority(Count, Total)` — Count > Total // 2

4. **Block References**: Endorsements point to candidates; ratifications point to endorsements

5. **Mode Detection**: Based on whether previous wave was quiescent

## General Guidelines

1. **Never assume programs work** - always test after any change
2. **Read error messages carefully** - they often indicate the exact issue
3. **Check the spec** - if behaviour seems wrong, verify against the chapter text
4. **Test incrementally** - run unit tests before plays
5. **Report all results** - include both successes and failures

## References

### Chapter 23
- Book chapter: `chapters/cryptocurrencies.tex`
- Flash paper: Lewis-Pye, Naor, Shapiro. "Grassroots Flash" (arXiv:2309.13191)
- GC paper: Shapiro. "Grassroots Currencies" (arXiv:2202.05619)

### Chapter 24
- Book chapter: `chapters/constitutional_consensus.tex`
- CC paper: Keidar, Lewis-Pye, Shapiro. "Constitutional Consensus" (arXiv:2505.19216)

### GLP Specs
- GLP spec: `/home/user/GLP/docs/glp-bytecode-v216-complete.md`
- Runtime spec: `/home/user/GLP/docs/glp-runtime-spec.txt`
