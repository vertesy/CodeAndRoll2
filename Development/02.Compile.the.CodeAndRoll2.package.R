######################################################################################################
# 02.Compile.the.CodeAndRoll2.package.R
######################################################################################################
# source("~/CodeAndRoll2/Workflow_to_Create_an_R_Package.R")
rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)
# install.packages("devtools")
# Functions ------------------------
# devtools::install_github(repo = "vertesy/CodeAndRoll2/CodeAndRoll2")
try (source('~/GitHub/Packages/CodeAndRoll/CodeAndRoll.R'),silent= FALSE)

# irequire("devtools")
# install_version("devtools", version = "2.0.2", repos = "http://cran.at.r-project.org")
irequire("devtools")
irequire("roxygen2")
irequire("stringr")

kollapse <-function(..., print = TRUE) {
if (print == TRUE) {
    print(paste0(c(...), collapse = ""))
  }
  paste0(c(...), collapse = "")
}

# Setup ------------------------
PackageName = 	"CodeAndRoll2"
setwd("~/GitHub/")

RepositoryDir = kollapse("~/GitHub/Packages/", PackageName, "/")
fname = 	kollapse(PackageName, ".R")
Package_FnP = 	kollapse(RepositoryDir, "R/", fname)

BackupDir = "~/GitHub/Packages/CodeAndRoll2/Development/"
dir.create(BackupDir)

# devtools::use_package("vioplot")
DESCRIPTION <- list("Title" = "CodeAndRoll2 for vector, matrix and list manipulations"
    , "Author" = person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )
    , "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )'
    , "Description" = "CodeAndRoll2 is a set of more than >130 productivity functions. Used by MarkdownReports, ggExpress, SeuratUtils.
    Packaged version of the core functionalities  (vector, matrix and list manipulations; math) of the formerly used CodeAndRoll (v1)."
    , "License" = "GPL-3 + file LICENSE"
    , "Version"= "2.0.0"
    , "Packaged" =  Sys.time()
    , "Repository" =  "CRAN"
    , "Imports" = "dplyr, gtools, stringr, stats, methods, sm, graphics, grDevices, gplots, RColorBrewer, colorRamps, clipr,  sessioninfo"
    # , "Suggests" = ""
    , "BugReports"= "https://github.com/vertesy/CodeAndRoll2/issues"
)


setwd(RepositoryDir)
if ( !dir.exists(RepositoryDir) ) { create(path = RepositoryDir, description = DESCRIPTION, rstudio = TRUE)
} else {
    getwd()
    try(file.remove(c("DESCRIPTION","NAMESPACE", "CodeAndRoll2.Rproj")))
    create_package(path = RepositoryDir, fields = DESCRIPTION, open = F)
}


# go and write fun's ------------------------------------------------------------------------
# file.edit(Package_FnP)

# Create Roxygen Skeletons ------------------------
# RoxygenReady(Package_FnP)

# replace output files ------------------------------------------------
BackupOldFile = 	kollapse(BackupDir, "Development", ".bac", print = FALSE)
AnnotatedFile = 	kollapse(BackupDir, "Development", ".annot.R", print = FALSE)
file.copy(from = Package_FnP, to = BackupOldFile, overwrite = TRUE)
# file.copy(from = AnnotatedFile, to = Package_FnP, overwrite = TRUE)

# Manual editing of descriptors ------------------------------------------------
# file.edit(Package_FnP)

# Compile a package ------------------------------------------------
setwd(RepositoryDir)
getwd()
document()


# Install your package ------------------------------------------------
# # setwd(RepositoryDir)
install(RepositoryDir)
# require("CodeAndRoll2")
# # remove.packages("CodeAndRoll2")
# # Test your package ------------------------------------------------
# help("wplot")
# cat("\014")
# devtools::run_examples()


# Test if you can install from github ------------------------------------------------
# devtools::install_github(repo = "vertesy/CodeAndRoll2")
# devtools::install_github(repo = "vertesy/CodeAndRoll2/CodeAndRoll2")
# require("CodeAndRoll2")

# Clean up if not needed anymore ------------------------------------------------
# View(installed.packages())
# remove.packages("CodeAndRoll2")

check(RepositoryDir, cran = TRUE)
# as.package(RepositoryDir)
#
#
# # source("https://install-github.me/r-lib/desc")
# # library(desc)
# # desc$set("CodeAndRoll2", "foo")
# # desc$get(CodeAndRoll2)
#
#
# system("cd ~/GitHub/CodeAndRoll2/; ls -a; open .Rbuildignore")
#
