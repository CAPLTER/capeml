# Engineering Operating Model — Intentional Coding

These instructions apply to all work in this repository. They establish an
**intentional, well-architected** workflow: think briefly before coding,
consult authoritative sources, and stay aware of design trade-offs — without
the overhead of formal spec files or gated planning phases.

## The approach

Before implementing anything non-trivial:

1. **State the intent** — one sentence: what does this change do and why?
2. **Note relevant WAF pillars** — which of the five pillars apply, and is
   there an obvious trade-off to flag? (See below; consult `docs/waf/` for
   checklists.)
3. **Implement** — keep changes scoped; if scope expands materially, pause and
   re-state intent.
4. **Verify** — run tests/linters; confirm the change behaves as stated.

For large or ambiguous requests, ask one clarifying question rather than
guessing. For genuinely complex multi-team or long-lived work, consider the
companion spec-driven template instead.

## Well-Architected pillars (decision lens)

For non-trivial choices, note which pillar(s) you are optimizing for and what
you trade away. Full checklists are in `docs/waf/pillars/`.

- **Reliability** — reproducibility, error handling, idempotency, recovery.
- **Security** — least privilege, secret handling, dependency provenance,
  OWASP Top 10. Never commit credentials or PII.
- **Operational excellence** — automation, observability, CI, documentation.
- **Performance efficiency** — appropriate data structures, vectorization,
  avoiding needless recomputation.
- **Cost / sustainability** — compute and storage footprint, caching, batch
  vs. interactive workloads.

Inline format (keep it brief):
> _Optimizing for **reliability** (pinned deps, seeded RNG). Trading some
> setup time._

## Authoritative-source discipline

- Prefer official documentation over model memory. Use connected MCP doc
  servers and the project's own docs before asserting an API.
- When stating an API signature, function name, or config key, verify it from
  a source confirmed this session — not a guess.
- If a fact is uncertain, say so and point to where to verify.

## Project conventions

- **Languages**: R and Python. Language-specific standards load automatically
  from `.github/instructions/` based on file type.
- **Reproducibility first**: suggest `renv` for R and use Astral `uv` for
  Python dependency locking/execution, set seeds, and make scripts runnable
  end-to-end from a clean checkout.
- **Domain knowledge** (e.g. ecological metadata / EML / EDI) lives in
  `.github/skills/` and loads on demand.
- **Record operating-model changes** in `.github/CHANGELOG.md` — capture
  the *why*, not just the *what*.
