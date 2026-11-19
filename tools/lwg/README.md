# LWG: from laws and whitepapers to GrassApps (prototype)

Goal: given the governing document of an organization, produce a GrassApp that follows it, with a record that shows what became of every provision.

Paper: "LWG: From Laws and Whitepapers to GrassApps" (Nimrod Talmon, in preparation).

## Place in the cascade

LWG stands before the cascade. Its output is an ordinary contract, `programs/lwg/NAME.txt` and `NAME.glp`, that takes an organisation of `programs/go` (GCoop, GDAO or SC) and fixes its bylaws. It adds nothing to the language and changes no file outside `tools/lwg` and `programs/lwg`. `cascade.sh` finds a contract by a row of its table, which is not ours to edit, so Build runs a temporary copy of the script with one row added and removes it.

## Four modules

Each fills its own columns of the record (one row for each provision).

| Module | File | Job | Basis |
|---|---|---|---|
| Read | `lwg/read.py` | split the document into provisions; attach the kind of each | Ostrom's seven types of rules, plus none / outside / judgment |
| Map | `lwg/map_.py`, `lwg/rules.py` | the bylaws: roles, and for each act who decides and by which decision rule | the bylaws of the organisations paper; the six-part decision rule |
| Review | `lwg/review.py`, the web page | open choices become questions; a person answers | |
| Build | `lwg/build.py` | write the contract (clauses and schemas) and run the cascade's check | the generator of the organisations paper, called as a library; `cascade.sh` |

A decision rule (`lwg/rules.py`): body, weight (one each, or by shares), base (all, or votes cast), threshold, quorum, and the days that must pass after a motion is moved. A rule that the organisations paper already has is passed to its generator unchanged; any other adds a vote against and a constraint over the votes.

Shares (`lwg/shares.py`), for a contract on SC, each switched on in the bylaws: exit (a shareholder gives up shares and takes the same part of a coin the organisation holds), shares that are not passed on, and motions moved by a shareholder. These three are additions to SC made here and are for the organisations paper to adopt or replace.

## Run

    python3 run.py examples/amutot --check        # a prepared example
    python3 run.py --bundle bundle.json --check   # a bundle from the web page; builds the trial contract LWGX
    python3 site.py                               # writes site/index.html

The check needs Dart and takes about a minute. Build needs `vendor/` (below).

## Examples

| Folder | Document | Base | Contract |
|---|---|---|---|
| `examples/amutot` | Israeli Amutot Law, model bylaws | GCoop | LWGA |
| `examples/ens` | ENS DAO constitution | GDAO | LWGE |
| `examples/unicorn` | Unicorn Grocery rules (excerpt) | GCoop | LWGU |
| `examples/moloch` | Moloch v1 README, overview | SC | LWGM |

All four pass the cascade's check (syntactically grassroots, with wards). No play has been run on them. None is compiled into an app yet.

## The web page

`site/index.html` is one self-contained page: the four records, the table of who decides what, a calculator for trying a decision rule, and "Your document". There a person pastes a document, copies a prompt (`prompt.md`) to a language model, pastes the reply back, and answers the questions; the page then gives a bundle for `run.py --bundle`. The prompt offers GCoop and GDAO only.

## What a language model does

The kind of each provision (`kinds.json`) and the bylaws with their questions (`map.json`) are written by a language model and saved as files. The code checks them, assembles the record, writes the contract and runs the check.

## Not built

Abstention; a day on which an undecided motion fails; a decision by two bodies; dues and payments tied to an act; escrow; and every provision a record marks "not built".

## vendor/

`vendor/` is not in the repository. Build calls the generator of the organisations paper, which is not in this repository yet. To run Build, put that project's `docs/tools/*.py` in `vendor/go_tools/` and its main `.tex` at `vendor/go_paper.tex`.
