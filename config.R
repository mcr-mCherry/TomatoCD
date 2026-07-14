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
  base_url = "",           # 保持为空，让 Shiny 自己根据当前域名处理
  use_huggingface = FALSE,
  timeout = 300
)

# 开启外部 URL 模式，这样 JBrowse2 和前端搜索才能通过 HTTP 访问数据
USE_EXTERNAL_URL <- TRUE

EXTERNAL_URLS <- list(
  data        = "data",
  jbrowse2    = "jbrowse2",
  www         = "",
  gene_search = "gene_search"
)

# 除非你需要连接外部数据库下载数据，否则保持 FALSE 最稳
USE_PROXY <- FALSE
PROXY_HOST <- "127.0.0.1"
PROXY_PORT <- 7897