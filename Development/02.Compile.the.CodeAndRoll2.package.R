######################################################################################################
# 02.Compile.the.CodeAndRoll2.package.R
######################################################################################################
# source("~/CodeAndRoll2/Workflow_to_Create_an_R_Package.R")
rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)
# install.packages("devtools")

# Functions ------------------------
# install_version("devtools", version = "2.0.2", repos = "http://cran.at.r-project.org") # install.packages("devtools")
require("devtools")
require("roxygen2")
require("stringr")

# devtools::install_github(repo = "vertesy/CodeAndRoll2")
require('CodeAndRoll2')
require('Stringendo')

# Setup ------------------------
PackageName = 	"CodeAndRoll2"
package.version = "2.4.4"

setwd("~/GitHub/")

RepositoryDir = kollapse("~/GitHub/Packages/", PackageName, "/")
fname = 	kollapse(PackageName, ".R")
Package_FnP = kollapse(RepositoryDir, "R/", fname)

BackupDir = "~/GitHub/Packages/CodeAndRoll2/Development/"
dir.create(BackupDir)

# devtools::use_package("vioplot")
DESCRIPTION <- list("Title" = "CodeAndRoll2 for vector, matrix and list manipulations"
    , "Author" = person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )
    , "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )'
    , "Description" = "CodeAndRoll2 is a set of more than >130 productivity functions. Used by MarkdownReports, ggExpress, SeuratUtils.
    Packaged version of the core functionalities  (vector, matrix and list manipulations; math) of the formerly used CodeAndRoll (v1)."
    , "License" = "GPL-3 + file LICENSE"
    , "Version"= package.version
    , "Packaged" =  Sys.time()
    # , "Repository" =  "CRAN"
    , "Depends" =  "Stringendo"
    , "Imports" = "base, colorRamps, dplyr, gplots, graphics, grDevices, gtools, methods, plyr, RColorBrewer, sessioninfo, sm, stats, Stringendo, stringr, utils"
    # , "Imports" = "base, colorRamps, dplyr, gplots, graphics, grDevices, gtools, MarkdownReports, methods, plyr, RColorBrewer, sessioninfo, sm, stats, Stringendo, stringr, utils"
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
warnings()

{
  "update cff version"
  citpath <- paste0(RepositoryDir, 'CITATION.cff')
  xfun::gsub_file(file = citpath, perl = T
                  , "^version: v.+", paste0("version: v", package.version))
}

# Install your package ------------------------------------------------
# # setwd(RepositoryDir)
install(RepositoryDir, upgrade = F)




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

# Check package dependencies ------------------------------------------------
depFile = paste0(RepositoryDir, 'Development/Dependencies.R')

(f.deps <- NCmisc::list.functions.in.file(filename = Package_FnP))
# clipr::write_clip(f.deps)

sink(file = depFile); print(f.deps); sink()
p.deps <- gsub(x = names(f.deps), pattern = 'package:', replacement = '')
write(x = p.deps, file = depFile, append = T)
p.dep.declared <- trimws(unlist(strsplit(DESCRIPTION$Imports, ",")))
p.dep.new <- sort(union( p.deps, p.dep.declared))
# clipr::write_clip(p.dep.new)


