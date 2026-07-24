---
description: "Use when making non-trivial design or architecture decisions. Points to the local Well-Architected Framework reference material in docs/waf/ (pillar checklists, trade-off prompts) that the agent should consult and cite."
---
# Well-Architected reference

For any non-trivial design choice, consult the local WAF reference in
[docs/waf/](../../docs/waf/) and **cite the pillar(s)** you are optimizing for
and what you trade away (see the base operating model in
`copilot-instructions.md`).

- Pillar checklists and trade-off prompts live in `docs/waf/pillars/`.
- These are version-controlled, reviewable references — prefer them (and live
  doc MCP servers) over model memory.
- Record rationale inline in the relevant code comment, commit message, or
  `CHANGELOG.md` entry rather than a separate spec file.
