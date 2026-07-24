# Customizations Changelog

Tracks intentional changes to the engineering operating model: instructions,
skills, WAF docs, and MCP config. Git history captures *what*; this file
captures *why*. Newest first.

## 2026-06-29
- Update R guidance: treat `renv` as suggested (not required), prefer
  explicit non-base namespacing, and favor `purrr` iteration patterns over
  `for` loops by default.
- Standardize Python guidance on the Astral `uv` workflow (`uv lock`,
  `uv sync`, `uv run`) across base, analysis, and reliability references.
- Add PostgreSQL instruction set (`.github/instructions/postgresql.instructions.md`)
  with MCP-first workflow guidance (context -> query -> modify), safety checks,
  and performance review expectations.
- Update README with a dedicated PostgreSQL section so users discover MCP-first
  DB workflow guidance from the template landing page.
- Add a PostgreSQL MCP quick-start checklist in README for faster onboarding to
  safe DB workflows.

## 2026-06-29
- Initial scaffold: lightweight `copilot-instructions.md` (think-then-code
  with inline WAF awareness, no spec/plan gates), R/Python/analysis style
  instructions, `well-architected` instruction, `docs/waf/` pillar checklists.
  Forked from the spec-driven template — omits `/spec`, `/plan` prompts and
  `Architect`/`Planner` agents by design. Use when formal spec overhead is not
  warranted (exploratory work, solo projects, known domains).
