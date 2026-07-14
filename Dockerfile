# TomatoCD analysis pipeline — single-image build
# Reviewers can run the smoke test with:
#   docker build -t tomatocd .
#   docker run --rm -v "$PWD":/work -w /work tomatocd bash examples/test/run.sh
# Smoke test completes in ~30 s and writes /work/results/figures/Fig1B.pdf.

FROM mambaorg/micromamba:1.5.10

USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
        chromium chromium-driver \
    && rm -rf /var/lib/apt/lists/*

USER mambauser
WORKDIR /work

# Conda env ships inside the image so reviewers don't resolve conda themselves.
COPY --chown=mambauser:mambauser environment.yml /work/environment.yml
RUN micromamba env create -f /work/environment.yml -y && \
    micromamba clean -a -y

ENV PATH="/opt/conda/envs/tomATOCD/bin:/usr/lib/chromium:${PATH}"
ENV CONDA_DEFAULT_ENV=tomATOCD
ENV MAMBA_ROOT_PREFIX=/opt/conda
SHELL ["micromamba", "-n", "tomATOCD", "/bin/bash", "-c"]

# Snakemake/webdriver both need a sensible sandbox, and webshot2 needs Chromium.
ENV CHROMIUM_BIN=/usr/bin/chromium

COPY --chown=mambauser:mambauser . /work

# Default: dry-run to verify the pipeline is wired up.
ENTRYPOINT ["micromamba", "-n", "tomATOCD", "/bin/bash", "-lc"]
CMD ["snakemake -n"]
