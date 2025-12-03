# Configuration for the Package
# file.edit("~/GitHub/Packages/CodeAndRoll2/Development/config.R")

DESCRIPTION <- list(
  package.name = "CodeAndRoll2",
  version = "2.7.6",
  title = "CodeAndRoll2 for vector, matrix and list manipulations",
  description = "CodeAndRoll2 is a set of more than 130 productivity functions for vector, matrix
  and list manipulations and math. Used by MarkdownReports, ggExpress, SeuratUtils, etc.",

  depends = "Stringendo",
  imports = "dplyr, tibble, stringr, purrr, plyr, gplots, gtools, colorRamps, RColorBrewer, sessioninfo, rstudioapi", # graphics,
  suggests = "sm, grDevices",

  author.given = "Abel",
  author.family = "Vertesy",
  author.email = "av@imba.oeaw.ac.at",
  github.user = "vertesy",
  license = "GPL-3 + file LICENSE"
)

