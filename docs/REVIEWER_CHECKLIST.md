# Reviewer checklist — what to test in 5 minutes

These are the order of steps a Nature referee typically takes to verify
a code submission. Each step has an expected outcome so you can pass
or fail the submission objectively.

## 1. Public visibility (10 s)

- Visit `https://github.com/mcr-mCherry/TomatoCD` (or the PR branch
  `nature-review`).
- **Pass:** the repository is public.

## 2. LICENSE (10 s)

- Click `LICENSE` in the file list.
- **Pass:** an OSI-approved license is shipped (MIT in this case).

## 3. README / install instructions (30 s)

- Open `README.md`.
- **Pass:** a one-line install command is documented.

## 4. Smoke test (≤ 5 min)

```bash
git clone https://github.com/mcr-mCherry/TomatoCD.git
cd TomatoCD
conda env create -f environment.yml
conda activate tomATOCD
bash examples/test/run.sh
```

- **Pass:** `Smoke test OK → results/figures/Fig1B.pdf` is printed and
  the file exists.

## 5. Reference output comparison (5 s)

```bash
bash examples/test/check_output.sh
```

- **Pass:** `OK: smoke test produced ...` is printed; the rendered PDF
  matches the reference under `examples/test/expected/Fig1B.pdf`.

## 6. Data accessibility (30 s)

- Open `docs/data_sources.md`.
- **Pass:** newly-generated data are listed at GEO Series
  GSE173212 / GSE173213; reused datasets list TEA / SRR3110654.

## 7. Code availability paragraph (30 s)

- Open `docs/Code_Availability_Statement.md`.
- **Pass:** a Methods-section drop-in paragraph is available (DOI
  placeholder is acceptable during peer review).

## 8. Citation metadata (10 s)

- View the GitHub sidebar "About" → "Cite this repository".
- **Pass:** a BibTeX entry generated from `CITATION.cff` is visible.

## Optional: Docker reproduction (10 min if rebuilding from scratch)

```bash
docker build -t tomatocd .
docker run --rm -v "$PWD":/work -w /work tomatocd bash examples/test/run.sh
```

- **Pass:** the container builds without error and the smoke test
  produces `results/figures/Fig1B.pdf`.
