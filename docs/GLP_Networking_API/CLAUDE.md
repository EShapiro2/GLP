# Instructions for Claude

## Project Overview

**Repository:** GLP-Networking-API
**Main file:** `main.tex`
**Purpose:** "GLP Networking API Specification" — the network layer (API + transports: BLE, IP, rendezvous) beneath GLP programs.

## Code ownership

This project is **GLP-Networking-API**.  Owns under `/GLP/`: `examples/rendezvous` and the transport specification — its **paper → code authority**.  Does **not** own the social-graph or rendezvous *protocol* (pure GLP above the network line), which belongs to the owning platform paper (e.g. GSG).  Implementation decisions go in this paper's arXiv "Implementation Notes" appendix, not a separate spec doc.  **At session start, before any work, read `/Grassroots/docs/glp-paper-code-map.md` in full and state that you have done so** — it is the authoritative map, ownership policy, project roster, and the procedure for requesting changes to code you do not own.

## Session start

Read `/Grassroots/claude.md`, `/Grassroots/docs/writing-style-guide.md`, and the ownership doc above before working.  LaTeX is edited in Overleaf only.
