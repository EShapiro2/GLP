# A simulation of CSSN under sGLP, 2026-10-25

CSSN's Section 5: the four mixes of policy and response, seed 1, one village of ten families (cs_village1: 17 parents, 20 children), one simulated year, at Section 5's rates as CSSN #12 revised them on 2026-10-25 (45 in 67 an offer of friendship, 6 an introduction of a friend to another, both adults, 6 child_meet_2, 6 child_befriend, 1 child_meet of a child and a friend, 1 child_adult_befriend, 1 child_unfriend, 1 unfriend), over the cascade record cascade/CSSN/2026-10-24-2 (c08f3ede), by programs/sglp/cva/ (record.sh) at programs/sglp 38f7b4d0 and repository 7f3f05db.  Run by a subagent of Code #16 from 07:00 to 08:10 UTC, each mix 64 to 71 minutes and at most 433 MB, the four at once; each mix is a record of its own, in the directory of its name, with its manifest.

The source of each mix is programs/cssn/CSSN_sglp.glp at 7f3f05db with the shares exchanged: strict_choosy as it stands; strict_keen with keen 1.0; permissive_choosy with permissive 1.0; permissive_keen with both.  measures.json in each is programs/cssn/sim/measure.py (eeca5e9d) on its log, --months 12 --horizon 31557600, the contract git show cascade/CSSN/2026-10-24-2:cascade/CSSN/CSSN.glp; edges.png in each is programs/cssn/sim/fig_edges.py on its measures.json.

| Mix | Child friendships at the end of each month | Parents' requests for a child friendship committed | Days from request to commit, mean and most | Approvals a parent answers a month, months 2 to 12 | Child components at the end |
|---|---|---|---|---|---|
| strict_choosy | 0 every month | 0 of 105 | none | 2.11 | 20 |
| strict_keen | 0 every month | 0 of 105 | none | 2.09 | 20 |
| permissive_choosy | 0 to month 11, then 1 | 2 of 104 | 0.51, 0.71 | 2.24 | 19 |
| permissive_keen | 0,0,0,1,1,2,2,4,4,6,6,8 | 9 of 92 | 2.61, 4.13 | 2.04 | 12 |

The parents are in one component from month 1 in every mix; the children are in one component by the end of no month in any mix.  A parent answers 17.0 to 17.2 approvals in month 1, the meetings and friendships of the start.  The monitor finds no exception in any mix, and no change of state is unexplained.
