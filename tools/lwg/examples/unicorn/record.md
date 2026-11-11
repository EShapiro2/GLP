# Record: Worker cooperative under the Unicorn Grocery rules (excerpt)

| # | Provision | Kind | Became | Question | Answer | Clause | Status |
|---|---|---|---|---|---|---|---|
| 14 | All Employees of the Co-operative may be admitted to membership of the Co-operative, except that the Co-operative in a general meeting ma... | boundary | admission is an act of the applicant and the directors; that the applicant is an employee is not checked | The app does not know who is an employee. Default: the directors judge it when they admit. Should someone attest employment? |  |  | not built |
| 15 | In accordance with the Co-operative Principle of voluntary and open membership, whilst the Co-operative shall undertake to encourage its ... | scope | undertaking of the organization; joining is always the applicant's own act |  |  | LWGU | built |
| 16 | No person shall be admitted into membership of the Co-operative unless that person has attained the age of 18, supports the aims of the C... | boundary | act: admit a member; decided by the directors rule | How the directors decide is in a part of the rules not given here. Used: more than half of all directors. Age and the one share are not checked. Agreed? |  | LWGU | built |
| 17 | All Members agree to attend general meetings and take an active interest in the operation and development of the Co-operative and its bus... | choice | a member's promise | Should joining record these duties as the member's own promise? Default: not recorded. |  |  | not built |
| 18 | In accordance with the Co-operative Principle of education, training and information, the Co-operative shall provide potential Members wi... | information | undertaking of the organization |  |  | LWGU | built |
| 19 | The Co-operative shall provide ongoing education and training in co-operative values and principles and associated topics. The Co-operati... | information | carried by the same undertaking as rule 18 |  |  | LWGU | built |
| 20 | A Member shall cease to be a Member of the Co-operative immediately that s/he: (a) Ceases to be in the employment of the Co-operative; or... | boundary | resigning is the member's own act (leave); expulsion is the act expel; the other three are not recorded | Who records that employment ended, that the share is no longer held, or a death? Default: nobody; the members may expel. |  |  | absorbed |
| 21 | A Member may be expelled for conduct prejudicial to the Co-operative by an Extraordinary Resolution, provided that the grounds for expuls... | aggregation | act: expel; decided by the extraordinary rule | The member must be heard before the vote. Default: the 14 days between the motion and the decision serve for it, and nothing checks that the member answered. Should the app require a recorded response? |  | LWGU | built |
| 22 | If on due notice having been served the Member fails to attend the meeting the meeting may proceed in the Member's absence. (a) An expell... | payoff | payment for shares; re-admission by the members | Shares and their repayment are not built. Re-admission of an expelled member would need the app to remember the expulsion. Default: re-admission goes as any admission. |  |  | not built |
| 32 | The Directors shall call the annual general meeting giving 14 Clear Days' notice to all Members. All other general meetings shall be conv... | information | a motion can be decided only 14 days or more after it was moved | Shorter notice with the written agreement of 90% of the members is not built. Needed? |  | LWGU | built |
| 37 | No business shall be transacted at a general meeting unless a quorum of Members is present which shall include those Members not present ... | aggregation | quorum on the votes: at least 33/50 of the members voted | The floor of 3 members is not built; it matters only below five members. Agreed to leave it out? |  | LWGU | built |
| 47 | In accordance with the Co-operative Principle of democratic member control, each Member shall have one vote on any question to be decided... | aggregation | one vote each |  |  | LWGU | built |
| 49 | In the case of an equality of votes, whether on a show of hands or a poll, the chairperson shall not have a second or casting vote and th... | aggregation | a tie does not decide: the threshold is more than half |  |  | LWGU | built |
| 53 | Decisions at general meetings shall be made by passing resolutions: (a) The following decisions must be made by Extraordinary Resolution:... | aggregation | the table of acts: expel and amend by the extraordinary rule, every other act by the ordinary rule | Disposing of a third of the assets and winding up have no act of their own here, so they go by the ordinary rule. Default kept; should they wait until such acts exist? |  | LWGU | built |
| 54 | An Extraordinary Resolution is one passed by a majority of not less than 75% of votes cast at a general meeting and an ordinary resolutio... | aggregation | the extraordinary rule (at least three quarters of the votes cast) and the ordinary rule (more than half of the votes cast) | "Simple majority (51%)" is read as more than half. Agreed? |  | LWGU | built |
| 55 | Resolutions may be passed at general meetings or by written resolution. A written resolution may consist of several identical Documents s... | aggregation | every vote in the app is a signed vote on a written motion |  |  |  | absorbed |

## Bylaws

| Act | Decided by | Rule |
|---|---|---|
| every other act | members | more than half of the votes cast are for, and at least 33/50 of the members voted, and the motion was moved at least 14 days before |
| admit a member | directors | more than half of the directors are for |
| expel a member | members | at least three quarters of the votes cast are for, and at least 33/50 of the members voted, and the motion was moved at least 14 days before |
| amend the bylaws | members | at least three quarters of the votes cast are for, and at least 33/50 of the members voted, and the motion was moved at least 14 days before |

## Contract

    LWGU: Worker cooperative under the Unicorn Grocery rules (excerpt)
    LWGU instantiates GCoop and restricts it.
    Orga is a cooperative; the directors decide to admit a member when more than half of the directors are for; the members decide to expel a member when at least three quarters of the votes cast are for, and at least 33/50 of the members voted, and the motion was moved at least 14 days before; the members decide to amend the bylaws when at least three quarters of the votes cast are for, and at least 33/50 of the members voted, and the motion was moved at least 14 days before; the members decide every other act of the organization when more than half of the votes cast are for, and at least 33/50 of the members voted, and the motion was moved at least 14 days before.
    Alice and Bob are persons, Orga a cooperative.
    1. ○ Voluntary membership.  Orga does not make membership a condition of employment.
    2. ○ Education.  Orga tells those who may join what a member's role is, and trains its members in it.
    3. ■ Voting against.  A holder of a role may vote against a proposed act of Orga, once, in place of voting for it.

Check: `lwgu: verdict: syntactically_grassroots_with_wards(acquaint, [org], [guardianship(org, created, creator)], [], [identity, person])`
