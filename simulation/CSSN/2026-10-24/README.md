# A simulation of CSSN under sGLP, 2026-10-24

CSSN's Section 5: the four mixes of policy and response, seed 1, one village of ten families (cs_village1: 17 parents, 20 children), one simulated year, at Section 5's rates, over the cascade record cascade/CSSN/2026-10-24-2 (c08f3ede), by programs/sglp/cva/ (record.sh) at programs/sglp 38f7b4d0 and repository ff9ed5ba.  Run by Code #15 from 22:23 to 23:22 UTC, each mix 56 to 59 minutes and at most 430 MB; each mix is a record of its own, in the directory of its name, with its manifest.

The source of each mix is programs/cssn/CSSN_sglp.glp at GLP main a00f6517 (sGLP #9) with the shares exchanged: strict_choosy as it stands; strict_keen with keen 1.0; permissive_choosy with permissive 1.0; permissive_keen with both.  measures.json in each is programs/cssn/sim/measure.py (CSSN-sim e9b1ea33) on its log, --months 12 --horizon 31557600.

| Mix | Child friendships at the end of each month | Children's requests committed | Child components at the end |
|---|---|---|---|
| strict_choosy | 0 every month | 0 of 107 | 20 |
| strict_keen | 0 to month 8, then 1 | 1 of 117 | 19 |
| permissive_choosy | 0,0,0,0,0,1,1,1,2,2,2,2 | 2 of 115 | 18 |
| permissive_keen | 0,0,0,2,3,3,6,7,9,9,13,15 | 15 of 109 | 7 |

The parents are in one component from month 1 in every mix.  The monitor's exceptions, 0, 0, 1 and 3, are of the measure: each is a child meeting or friendship committed by the other parent's attempt of the same part, which measure.py credits to the first parent's request.
