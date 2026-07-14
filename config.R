# -----------------------------------------------------------------------------
# TomatoCD — runtime configuration constants
# Released under the MIT License. See LICENSE in the repository root.
#
# Manuscript: NCOMMS-26-056367-T (Nature Communications, under review)
# Role:       declares Hugging Face mirror coordinates, proxy flags,
#             and external-URL routing used by server.R.
# Maps to:
#   - docs/hardware.md (External data mirror)
#   - docs/deployment.md (EXTERNAL_URLS routing)
# -----------------------------------------------------------------------------

HUGGINGFACE_CONFIG <- list(
  repo_id = "TomatoCisRegDB/TomatoCisRegDB",
  base_url = "",     
  use_huggingface = FALSE,
  timeout = 300
)

USE_EXTERNAL_URL <- TRUE

EXTERNAL_URLS <- list(
  data        = "data",
  jbrowse2    = "jbrowse2",
  www         = "",
  gene_search = "gene_search"
)

USE_PROXY <- FALSE
PROXY_HOST <- "127.0.0.1"
PROXY_PORT <- 7897
