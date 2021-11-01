######################################################################################################
# 01.Format.the.CodeAndRoll2.package.R
######################################################################################################
# source("~/MarkdownReports/Workflow_to_Create_an_R_Package.R")
rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)
# install.packages("devtools")

# Functions ------------------------
if (F) {
  devtools::install_github(repo = "vertesy/RoxygenReady/RoxygenReady")
  require("RoxygenReady")
}

RoxygenReady("~/GitHub/Packages/CodeAndRoll2/CodeAndRoll2.R")



# Setup ------------------------
remotes::install_github('yonicd/sinew')
require('sinew')
makeOxyFile("~/GitHub/Packages/CodeAndRoll2/CodeAndRoll2.R", overwrite = FALSE)

makeOxyFile("~/GitHub/Packages/StringParser/R/StringParser.R", overwrite = FALSE)

makeOxyFile("~/GitHub/Packages/ReadWriter/R/ReadWriter.R", overwrite = FALSE)


require('ggExpress')
