#!/usr/bin/env Rscript
# Fig1B: Voronoi treemap of TF family distribution among Pass-QC TFs.
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(voronoiTreemap)
  library(htmlwidgets)
  library(webshot2)
})

parser <- argparse::ArgumentParser(description = "Fig1B TF family treemap")
parser$add_argument("--input",  required = TRUE, help = "Tab-delimited TF summary table")
parser$add_argument("--output", required = TRUE, help = "Output PDF path")
parser$add_argument("--width",  default = "1000", help = "widget width in px")
parser$add_argument("--height", default = "800",  help = "widget height in px")
parser$add_argument("--seed",   default = "42",   help = "RNG seed")
args <- parser$parse_args()

set.seed(as.integer(args$seed))
width  <- as.integer(args$width)
height <- as.integer(args$height)

dir.create(dirname(args$output), recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

df <- readr::read_delim(
  args$input,
  delim = "\t",
  show_col_types = FALSE,
  name_repair = "minimal"
) |>
  dplyr::filter(QC_Status == "Pass") |>
  dplyr::count(Family, name = "Freq") |>
  dplyr::arrange(dplyr::desc(Freq), Family)

vor <- data.frame(
  h1 = "Pass QC TFs",
  h2 = df$Family,
  h3 = df$Family,
  color = grDevices::colorRampPalette(
    rev(c("#D8B70A", "#81A88D", "#3B9AB2", "#02401B",
          "#972D15", "#A2A475", "grey70"))
  )(nrow(df)),
  weight = df$Freq,
  codes = paste0(df$Family, "\n(", df$Freq, ")"),
  stringsAsFactors = FALSE
)

vt <- voronoiTreemap::vt_input_from_df(vor, scaleToPerc = TRUE)
v  <- voronoiTreemap::vt_d3(voronoiTreemap::vt_export_json(vt))

tmp_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(v, tmp_html, selfcontained = TRUE)
webshot2::webshot(
  tmp_html, args$output,
  vwidth = width, vheight = height, zoom = 2
)
unlink(tmp_html)

cat("OK: wrote ", args$output, "\n", sep = "")
