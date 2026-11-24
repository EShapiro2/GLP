# Second-Score Auction under sGLP: ten persons, forty minutes (SCRATCH, 2026-11-23)

A real sGLP run of the Second-Score Auction, seed 1, 40 simulated minutes, at GLP
235bab588 (main) with this branch: the
canonical CVA platform the cascade compiles from programs/lbm/Auction.txt and
Second-Score-Auction.txt (scratch branch gra-numbers), driven by GRA_sglp.glp
(written by programs/sglp/gra/gen.py). Six drivers d01..d06 and four riders
r01..r04 at real Manhattan pickup points.

    bash programs/sglp/gra/run.sh cascade/Second-Score-Auction/platform.glp <printed> census "40 minutes" 1 1.log
    python3 programs/sglp/gra/rides.py 1.log > acts.txt

## The rides

Terms of every call: A 50, λ drawn in 1..3, T 10 minutes, deposit 5; bidding 3
minutes. Score = 50 − λ·delay − fare. Price = 50 − λ·(winner's delay) − second
score. Refund at arrival = min(price, λ·minutes late). Ties go to the bid ranked first. Drivers bid on every call they receive; a winner who already has a ride is skipped, and his bid counts neither for the winner nor for the second score.

| Call | Rider | λ | Bids: driver (fare, delay → score) | Winner | Price | Pickup | Arrival |
|---|---|---|---|---|---|---|---|
| 2119 | r01 | 2 | d05 (12, 2 → 34), d03 (15, 1 → 33), d04 (15, 3 → 29), d02 (13, 4 → 29), d06 (16, 4 → 26) | d05 | 13 | min 9, due 9 | on time, no refund |
| 2726 | r02 | 3 | d06 (10, 2 → 34), d04 (11, 3 → 30), d05 (13, 3 → 28), d03 (18, 4 → 20), d02 (16, 5 → 19) | d06 | 14 | min 9, due 9 | on time, no refund |
| 3552 | r04 | 1 | d05 (13, 2 → 35), d03 (15, 3 → 32), d06 (13, 5 → 32), d04 (17, 6 → 27), d02 (17, 8 → 25) | d03 (skipped busy d05, d06) | 20 | min 14, due 14 | on time, no refund |
| 4090 | r03 | 2 | d03 (16, 4 → 26), d01 (14, 5 → 26), d02 (15, 5 → 25), d04 (15, 6 → 23), d05 (18, 5 → 22), d06 (18, 8 → 16) | d01 | 14 | min 14, due 14 | on time, no refund |
| 14865 | r02 | 1 | d06 (11, 0 → 39), d04 (13, 7 → 30), d05 (15, 10 → 25), d02 (20, 9 → 21) | d06 | 20 | min 19, due 19 | on time, no refund |
| 18510 | r01 | 1 | d05 (11, 0 → 39), d04 (12, 6 → 32), d02 (16, 5 → 29), d06 (14, 10 → 26) | d05 | 18 | min 22, due 22 | driving at the horizon |
| 27338 | r03 | 3 | d01 (13, 0 → 37), d02 (13, 8 → 13), d04 (18, 9 → 5), d03 (27, 24 → -49) | d01 | 37 | min 31, due 31 | driving at the horizon |
| 34736 | r04 | 3 | d03 (10, 0 → 40), d04 (17, 16 → -15), d02 (23, 17 → -24), d06 (27, 24 → -49) | d03 | 50 | min 39, due 39 | driving at the horizon |
| 36415 | r02 | 3 | d06 (13, 0 → 37), d02 (14, 8 → 12), d04 (13, 9 → 10), d03 (22, 24 → -44) | — | — | open at the horizon | — |

Every price, refund and pickup above was checked by the app's rules: the rider
proposes the act with its numbers and the app refuses wrong ones.

## How people behave

- A rider with nothing to do calls, about every two minutes, from where she is to
  a destination drawn from the table. The call carries its pickup point.
- A driver without a ride bids on every call he received:
  the delay is his street distance to the pickup point at 500 m a minute, the fare
  8 + half the delay + a draw of 1..8.
- After the bidding, the rider ranks the bids one by one and awards by the second
  score, or closes the call if no bid is admissible.
- The driver drives to the pickup point; the rider waits there and records the
  pickup when he arrives. From the pickup, both phones report the car's place,
  500 m a minute along the street path, by the clock. At the destination the
  rider releases the fare and the refund.
- A rider not picked up within T cancels. A rider stays where her ride ended.

## What is still scratch

- The rules (Logic pragmas only; the contract texts are the paper's) keep the
  numbers the paper's terms need: the call's end, terms, route and pickup point;
  each bid's fare and delay; the ranking (leader, second score); the award time
  and the pickup time; and each person's location (an act Move).
- Auction extends Overlay here, not Local Overlay: each person points once to
  their three nearest drivers at the start. Keeping the pointing nearest as
  drivers move (Local Overlay's "closest", "furthest") is not in the rules yet.
- The scratch harness (programs/sglp/gra/harness.glp.in) tells each phone if it
  is a rider or a driver, sets the app's clock from sGLP's clock (one app day =
  one simulated minute), and shows the clock on the screen. A person with
  nothing to do proposes a refused act to see the clock again.
- The rider always awards the leading bid. If the driver's phone refuses because he
  already has a ride, she skips him (the app checks he has one), ranks the other bids
  again and awards the next; and so on. A skipped bid counts neither for the winner
  nor for the second score: the terms apply to the bidders who have no ride.
- Cancel's "Alice was at the pickup point from t0 to t0 + T" is checked as "Alice
  is at the pickup point when she cancels"; she never leaves it before the pickup.
