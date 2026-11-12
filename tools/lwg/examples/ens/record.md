# Record: DAO under the ENS Constitution

| # | Provision | Kind | Became | Question | Answer | Clause | Status |
|---|---|---|---|---|---|---|---|
| 0 | ENS DAO Constitution The ENS constitution is a set of binding rules that determine what governance actions are legitimate for the DAO to ... | none | no clause | The document does not say who is a member or how ordinary decisions are made. Used: the grassroots DAO as it is (a steward admits; more than half of the members decide). Agreed? |  |  | no clause |
| I | Name ownership shall not be infringed: ENS governance will not enact any change that infringes on the rights of ENS users to retain names... | scope | undertaking of the organization: a limit on what it may decide | Who judges that a decision breaches this clause? Default: nobody; it is a promise of the organization, and the members weigh it when they vote. |  | LWGE | built |
| I ex | Examples: Permissible: ENS governance may enact a change affecting the registration or extension costs of all names based on transparent ... | none | kept as a test of the reading; not in the contract |  |  |  | no clause |
| II | Fees are primarily an incentive mechanism: The primary purpose of registration fees is as an incentive mechanism to prevent the namespace... | scope | undertaking of the organization |  |  | LWGE | built |
| II ex | Examples: Permissible: ENS governance may increase the price of name registrations in order to address excessive speculative registration... | none | kept as a test of the reading |  |  |  | no clause |
| III | Income funds ENS and other public goods: Any income generated to the ENS treasury is to be used first of all to ensure the long-term viab... | scope | undertaking of the organization | "Reasonably required" is left as it is. Define it, or leave it to the members? |  | LWGE | built |
| III ex | Examples: Permissible: ENS governance may offer grant funding for a public good unrelated to ENS or Ethereum, so long as doing so does no... | none | kept as a test of the reading |  |  |  | no clause |
| IV | ENS Integrates with the global namespace: In order to facilitate making the most widely usable naming system, ENS aims to integrate with ... | scope | undertaking of the organization |  |  | LWGE | built |
| IV ex | Examples: Permissible: ENS governance should grant control of a top-level domain to its owner in the DNS system on request. Not permissib... | none | kept as a test of the reading |  |  |  | no clause |
| V | Amendments to this constitution by majority vote: Any change may be made to this constitution only by two-thirds majority and at least 1%... | aggregation | act: amend; decided by the amendment rule | The document counts tokens. Here each member has one vote, and "1% of all tokens taking part" becomes 1% of the members voting. Agreed, or should votes go by coins of the DAO (not built)? |  | LWGE | built |

## Bylaws

| Act | Decided by | Rule |
|---|---|---|
| every other act | members | more than half of the members vote for |
| admit a member | stewards | one steward decides |
| amend the clauses | members | at least two thirds of the votes cast are for, and at least 1/100 of the members voted |

## Contract

    LWGE: DAO under the ENS Constitution
    LWGE instantiates GDAO and restricts it.
    Orga is a DAO; its steward admits members, its members decide its acts when more than half of the members vote for, and they amend its bylaws when at least two thirds of the votes cast are for, and at least 1/100 of the members voted.
    Alice and Bob are persons, Orga a DAO.
    1. ○ Names.  Orga enacts no change that takes a name from its owner, or that treats the owners of names unequally in extending, transferring or using their names.
    2. ○ Fees.  Orga sets a fee only to deter the speculative registration of names or to fund its own development.
    3. ○ Income.  Orga uses its income first for its own long-term upkeep, and gives funds only to those who undertake these clauses.
    4. ○ Integration.  Orga enacts no change that weakens its integration with the outside system of names.
    5. ■ Amendment.  The members may amend the bylaws of Orga.
    6. ■ Voting against.  A member may vote against a proposed act of Orga, once, in place of voting for it.

Check: `lwge: verdict: syntactically_grassroots_with_wards(acquaint, [org], [guardianship(org, created, creator)], [], [identity, person])`
