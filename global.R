# 全局设置和库加载
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(data.table)

# 设置全局选项
options(shiny.maxRequestSize = 50*1024^2)  # 设置最大上传文件大小为50MB
options(stringsAsFactors = FALSE)  # 不再自动将字符串转换为因子

# 定义目录路径
BASE_DIR <- getwd()
INPUT_DIR <- file.path(BASE_DIR, "input")
OUTPUT_DIR <- file.path(BASE_DIR, "output")
OUTPUT_DATA_DIR <- file.path(OUTPUT_DIR, "data")
OUTPUT_PLOTS_DIR <- file.path(OUTPUT_DIR, "plots")
OUTPUT_REPORTS_DIR <- file.path(OUTPUT_DIR, "reports")
OUTPUT_LOGS_DIR <- file.path(OUTPUT_DIR, "logs")
WWW_DIR <- file.path(BASE_DIR, "www")

# 确保目录存在
create_directories <- function() {
  dirs <- c(INPUT_DIR, OUTPUT_DIR, OUTPUT_DATA_DIR, OUTPUT_PLOTS_DIR, 
            OUTPUT_REPORTS_DIR, OUTPUT_LOGS_DIR, WWW_DIR)
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
  }
}

# 创建必要的目录
create_directories()

# 全站共享缓存：避免每个 Shiny 会话重复读取同一批大文件
APP_CACHE <- new.env(parent = emptyenv())

get_cached_data <- function(key, loader, force_refresh = FALSE) {
  if (!force_refresh && exists(key, envir = APP_CACHE, inherits = FALSE)) {
    return(get(key, envir = APP_CACHE, inherits = FALSE))
  }

  value <- loader()
  assign(key, value, envir = APP_CACHE)
  value
}

clear_cached_data <- function(key = NULL) {
  if (is.null(key)) {
    rm(list = ls(envir = APP_CACHE, all.names = TRUE), envir = APP_CACHE)
  } else if (exists(key, envir = APP_CACHE, inherits = FALSE)) {
    rm(list = key, envir = APP_CACHE)
  }
}

# 注意：不要在 global.R 里注册 jbrowse2 资源路径。
# Shiny 会在 global.R 之前尚未加载 config.R，此时 USE_EXTERNAL_URL 还不存在，
# 会导致 /jbrowse2 被过早注册，并与 www/jbrowse2 发生前缀冲突。
# 统一在 server.R 里、读取完 config.R 后再按配置决定是否 addResourcePath。

# 定义全局变量和函数
welcome_message <- "Welcome to the Tomato Functional Genomics Database!"

# 示例数据加载函数（可以根据需要扩展）
load_example_data <- function() {
  # 这里可以添加数据加载逻辑
  return(mtcars)  # 使用内置数据集作为示例
}

# 番茄物种列表
tomato_species <- c(
  "Solanum lycopersicum",
  "Solanum pennellii",
  "Solanum pimpinellifolium",
  "Solanum habrochaites",
  "Solanum chilense"
)

# 功能模块描述
module_descriptions <- list(
  network = "Analyze gene-gene interaction networks",
  protein_binding = "Explore protein-DNA interaction sites",
  motif = "Discover regulatory DNA motifs",
  mc_preference = "Study DNA methylation patterns",
  gene_annotation = "Access comprehensive gene annotations",
  expression = "Visualize gene expression profiles"
)


