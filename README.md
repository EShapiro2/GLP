# The contracts of the village

Villagers is a grassroots contract that we designed to address the needs of villagers.
This branch has its 13 contracts: Villagers, the six groups that it unites, and the six contracts that the village adds to the master world.

They are printed here from the repository of the paper, EShapiro2/Villagers, at commit 8947c4c97ce6532a6bf10eed8b3e1ffcbbf1881e.
This branch is not merged into main, and the 13 are not contracts of the master world, which is `calculus/contracts` of main.

For each contract NAME, the directory `contracts/NAME/` has:

- `contract.txt`, the contract.
- `logic-pragmas.txt`, its Logic pragmas, for the six contracts that have clauses.
- `full-contract.txt`, the contract with every clause that it inherits, as the Calculus's `expand.py` prints it.

`contracts/MASTER.txt` names the commit of main whose `calculus/contracts` the 13 inherit from, and the commit of the Calculus that printed the full contracts.
