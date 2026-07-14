# legacy/

This directory exists for one reason: to make the audit trail explicit.

> Maintainers scan the tracked tree for files that look like they belong
> to local development (e.g. `agent_backend.py`, `upload_to_huggingface.py`,
> `server_backup_*.R`, `ui_backup_*.R`, ad-hoc Python/FastAPI helper
> scripts, debugging notebooks) and move them here before peer review.

On the current commit (`b4f9453` plus its predecessors), the scan
returned zero matches:

| Pattern | Tracked hits |
| --- | --- |
| `*.py`, `*.pyc` | 0 |
| `agent_*`, `*_agent*`, `*backend*` | 0 |
| `upload_to_*`, `huggingface*` (excluding `config.R` reference text) | 0 |
| `*backup*` | 0 |
| `*.bak`, `*.orig`, `*.rej`, `*~`, `*.tmp` | 0 |
| `test_*` regression scripts at the repo root (excluding `examples/test/`) | 0 |
| Random `*.py` and FastAPI / `uvicorn` / `pydantic` mentions in source code | 0 |

The directory is therefore intentionally empty, kept in the repository
so that:

1. Reviewers can see the maintenance intent immediately.
2. A future PR that introduces a temporary helper script has an obvious
   home until the dependency is either promoted to the pipeline or
   deleted.

If you need to retire a script during the review cycle:

```bash
git mv <file>.py legacy/<file>.py
git commit -m "legacy: retire <file>.py (out of paper scope)"
```

Files in `legacy/` are excluded from:

- The Snakemake pipeline (rule globs under `scripts/`, `workflow/`,
  `R/`).
- The Docker Hub image (`rocker/shiny` base + the bind-mounted app
  directory; `legacy/` is not bind-mounted).
- The reviewer smoke test (`examples/test/`).
