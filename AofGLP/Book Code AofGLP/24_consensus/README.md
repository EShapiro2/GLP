# 24_consensus - Constitutional Consensus

Implementation of single-epoch Constitutional Consensus in GLP.
Assumes attestation (no Byzantine faults), so simple majority (>n/2) suffices.

## Files

### Core Module
- `consensus.glp` - Main implementation: waves, rounds, blocks, finality, agent process, τ ordering

### Plays
- `play_low_throughput.glp` - Single transaction, no conflict, finalized in one wave
- `play_high_throughput.glp` - Two transactions conflict, leader wins, retry in next wave
- `play_agents.glp` - Multi-agent simulation using the agent process

### Tests
- `test_waves.glp` - Unit tests for wave/round utilities and leader selection
- `test_blocklace.glp` - Unit tests for blocklace operations and conflict detection

## Concepts

### Constitution
- `(P, σ, Δ)` where P = participants, σ = majority threshold (0.5), Δ = timeout
- With attestation, simple majority suffices (no need for 2/3)

### Wave Structure
- Wave = 3 rounds: Candidate → Endorse → Ratify
- Round 1: Propose transaction (or empty)
- Round 2: Endorse (point to candidate)
- Round 3: Ratify (point to endorsements)

### Finality
- Block is finalized when:
  1. Majority of round 2 blocks endorse it
  2. Majority of round 3 blocks ratify those endorsements

### Modes
- **Low-throughput**: Any agent can propose (spontaneous leader)
- **High-throughput**: Formal leader (round-robin) proposes, others wait or timeout

### τ Function
- Computes ordered sequence of finalized payloads
- All correct agents produce same sequence

## Running

All output is through procedure arguments. Results are bound to variables.

```
%% Load and test wave utilities
> test_waves.glp
> test_waves(Results).
%% Results = [wave_of_round(passed), round_in_wave(passed), ...]

%% Load and test blocklace operations
> test_blocklace.glp
> test_blocklace(Results).
%% Results = [blocks_at_round(passed), nonempty(passed), ...]

%% Run low-throughput play
> play_low_throughput.glp
> play_low_throughput(Result).
%% Result = passed

> test_finality(Result).
%% Result = passed

%% Run high-throughput play
> play_high_throughput.glp
> play_high_throughput(Result).
%% Result = passed

> test_conflict(Result).
%% Result = passed

> test_no_conflict(Result).
%% Result = passed

%% Test agent process
> play_agents.glp
> test_agent_propose(Result).
%% Result = passed

> test_agent_endorse(Result).
%% Result = passed
```

## Expected Results

- `test_waves(R)` → R contains all `passed`
- `test_blocklace(R)` → R contains all `passed`
- `play_low_throughput(R)` → R = passed
- `play_high_throughput(R)` → R = passed
- `test_finality(R)` → R = passed
- `test_conflict(R)` → R = passed

## References

- Constitutional Consensus paper (arXiv:2505.19216)
- Book chapter: `chapters/constitutional_consensus.tex`



Please test the Constitutional Consensus programs in AofGLP/24_consensus/

1. First, symlink if needed:
   ln -sf /path/to/AofGLP /home/user/GLP/glp_runtime/glp/AofGLP

2. Run tests in order:
   ./glp_repl
   > AofGLP/24_consensus/consensus.glp
   > AofGLP/24_consensus/test_waves.glp
   > test_waves(R).
   
   > AofGLP/24_consensus/test_blocklace.glp
   > test_blocklace(R).
   
   > AofGLP/24_consensus/play_low_throughput.glp
   > play_low_throughput(R).
   > test_finality(R).
   
   > AofGLP/24_consensus/play_high_throughput.glp
   > play_high_throughput(R).
   > test_conflict(R).
   
   > AofGLP/24_consensus/play_agents.glp
   > test_agent_propose(R).
   > test_agent_endorse(R).

3. Report all results, including any errors or suspensions.
