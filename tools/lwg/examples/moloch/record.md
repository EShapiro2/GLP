# Record: Guild under the Moloch rules

| # | Provision | Kind | Became | Question | Answer | Clause | Status |
|---|---|---|---|---|---|---|---|
| 1 | Moloch has a native asset called shares. Shares are minted and assigned when a new member is accepted into the Guild and provide voting r... | position | shares of the organization; a vote counts the shares its voter holds |  |  |  | absorbed |
| 2 | They are non-transferrable ... | choice | the acts that pass a coin on exclude shares; the organization still pays out the shares it issues |  |  | LWGM | built |
| 3 | ... but can be irreversibly redeemed at any time to collect a proportional share of all ETH held by the Guild in the Guild Bank. | payoff | act: exit, by the shareholder alone: give up shares and take the same part of a coin the organization holds; the shares are cancelled | Exit takes a part of one kind of coin at a time; with several kinds in the treasury, the shareholder exits in several steps. Agreed? |  | LWGM | built |
| 4 | To combat spam, new membership proposals can only be submitted by existing members and require a 10 ETH deposit. | boundary | a shareholder moves a motion to issue shares; no deposit | The deposit against spam is not built. Needed? |  | LWGM | built |
| 5 | The membership proposal includes the number of shares the applicant is requesting, and either the amount of ETH the applicant is offering... | boundary | act: issue shares; the motion names the number of shares | The motion does not name the applicant or the tribute: the organization issues the shares to itself and then pays them out. Default kept. Tie the tribute to the grant? |  | LWGM | built |
| 6 | All ETH offered as tribute is held in escrow by the Moloch.sol contract until the proposal vote is completed and processed. If a proposal... | payoff | escrow of the tribute |  |  |  | not built |
| 7 | Proposals are voted on in the order they are submitted. The voting period for each proposal is 7 days. During the voting period, members ... | aggregation | a shareholder votes once on a motion; the motion can be decided only after its period | Order of proposals and the limit of five a day are not built. Needed? |  | LWGM | built |
| 8 | Proposal votes are determined by simple majority of votes cast on the proposal, with no quorum requirement. | aggregation | the shares rule: more than half of the shares voted are for; no quorum |  |  | LWGM | built |
| 9 | At the end of the voting period, proposals enter into a 7 day grace period before the proposal is processed. The grace period gives membe... | aggregation | the motion can be decided only 14 days or more after it was moved | Voting does not close after 7 days here: votes stay open for the 14 days. A shareholder who voted for a motion may still exit before it is decided, which the document forbids. Default kept; neither is built. |  | LWGM | built |
| 10 | At the end of the grace period, proposals are processed when anyone calls processProposal. A 0.1 ETH reward is deducted from the proposal... | choice | a director takes the act once the rule holds; no reward | Here a director, not anyone, carries out a decided motion. Agreed? |  | LWGM | built |

## Bylaws

| Act | Decided by | Rule |
|---|---|---|
| every other act | shareholders | more than half of the shares voted are for, and the motion was moved at least 14 days before |
| issue shares | shareholders | more than half of the shares voted are for, and the motion was moved at least 14 days before |

## Contract

    LWGM: Guild under the Moloch rules
    LWGM instantiates SC and restricts it.
    Orga is a shareholding corporation; the shareholders decide to issue shares when more than half of the shares voted are for, and the motion was moved at least 14 days before; the shareholders decide every other act of the organization when more than half of the shares voted are for, and the motion was moved at least 14 days before.
    Alice and Bob are persons, Orga a shareholding corporation.
    1. ■ Voting against by shares.  A shareholder may vote against a proposed act of Orga, once, the vote counting the shares held, in place of voting for it.
    2. ■ Shares stay.  A shareholder does not pass a share on to another person.
    3. ■ Exit.  A shareholder may at any time give up shares and take the same part of a coin that Orga holds.
    4. ■ Motions.  A shareholder may move as a motion: to issue shares.

Check: `lwgm: verdict: syntactically_grassroots_with_wards(acquaint, [org], [guardianship(org, created, creator)], [], [identity, person])`
