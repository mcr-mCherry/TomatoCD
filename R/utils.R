# -----------------------------------------------------------------------------
# TomatoCD — application utility helpers
# Released under the MIT License. See LICENSE in the repository root.
#
# Manuscript: NCOMMS-26-056367-T (Nature Communications, under review)
# Role:       helper functions used by server.R / ui.R for file upload,
#             session bookkeeping, plotting, and other interactive UI glue.
# Maps to:
#   - Methods § Software / Shiny application utilities
#   - Code availability § docs/Code_Availability_Statement.md
#
# Production runtime:
#   rocker/shiny:latest (R 4.4.x, Shiny 1.14.0)
# Reproducibility:
#   - This file is mounted from /srv/shiny-server/cistrome_web/ at runtime.
#   - It is exercised by the smoke test in examples/test/run.sh only via
#     its load-time dependencies; the figure pipeline in
#     scripts/fig1b_family_distribution.R has its own copy of voronoiTreemap.
# -----------------------------------------------------------------------------

# 实用工具函数

# 生成唯一文件名
generate_unique_filename <- function(original_name, prefix = "") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  ext <- tools::file_ext(original_name)
  base_name <- tools::file_path_sans_ext(basename(original_name))
  
  if (prefix != "") {
    prefix <- paste0(prefix, "_")
  }
  
  return(paste0(prefix, base_name, "_", timestamp, ".", ext))
}

# 保存上传的文件
save_uploaded_file <- function(file_info, prefix = "") {
  if (is.null(file_info)) return(NULL)
  
  # 生成唯一文件名
  unique_name <- generate_unique_filename(file_info$name, prefix)
  
  # 保存文件到输入目录
  dest_path <- file.path(INPUT_DIR, unique_name)
  file.copy(file_info$datapath, dest_path)
  
  return(list(original_name = file_info$name, 
              saved_name = unique_name, 
              saved_path = dest_path))
}

# 读取数据文件
read_data_file <- function(file_path, file_type = NULL) {
  if (is.null(file_type)) {
    file_type <- tolower(tools::file_ext(basename(file_path)))
  }
  
  data <- NULL
  
  tryCatch({
    switch(file_type,
           "csv" = data <- read.csv(file_path, header = TRUE),
           "tsv" = data <- read.delim(file_path, header = TRUE),
           "txt" = data <- read.table(file_path, header = TRUE),
           "rds" = data <- readRDS(file_path),
           "rdata" = {
             load(file_path)
             data <- get(ls()[1])
           },
           stop(paste("不支持的文件类型:", file_type))
    )
  }, error = function(e) {
    stop(paste("读取文件失败:", e$message))
  })
  
  return(data)
}

# 数据预处理函数
preprocess_data <- function(data) {
  # 示例数据预处理逻辑
  cleaned_data <- data %>%
    na.omit() %>%
    mutate(across(where(is.numeric), scale))
  
  return(cleaned_data)
}

# 结果可视化函数
plot_results <- function(data, x_var, y_var) {
  # 示例可视化逻辑
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_minimal()
  
  return(p)
}

# 保存可视化结果
save_plot <- function(plot_object, filename, width = 10, height = 8, dpi = 300) {
  # 生成唯一文件名
  unique_name <- generate_unique_filename(filename, prefix = "plot")
  
  # 保存图片到输出目录
  dest_path <- file.path(OUTPUT_PLOTS_DIR, unique_name)
  ggsave(dest_path, plot_object, width = width, height = height, dpi = dpi)
  
  return(list(saved_name = unique_name, saved_path = dest_path))
}

# 结果导出函数
export_results <- function(data, filename, file_type = "csv") {
  # 生成唯一文件名
  unique_name <- generate_unique_filename(filename, prefix = "result")
  
  # 保存结果到输出目录
  dest_path <- file.path(OUTPUT_DATA_DIR, unique_name)
  
  tryCatch({
    switch(tolower(file_type),
           "csv" = write.csv(data, file = dest_path, row.names = FALSE),
           "tsv" = write.table(data, file = dest_path, sep = "\t", row.names = FALSE),
           "rds" = saveRDS(data, file = dest_path),
           stop(paste("不支持的输出文件类型:", file_type))
    )
  }, error = function(e) {
    stop(paste("导出结果失败:", e$message))
  })
  
  return(list(saved_name = unique_name, saved_path = dest_path))
}