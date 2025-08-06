######################################################################################################
# 02.Compile.the.CodeAndRoll2.package.R
######################################################################################################
# source("~/GitHub/Packages/CodeAndRoll2/Development/02.Compile.the.CodeAndRoll2.package.R")
rm(list = ls(all.names = TRUE))
try(dev.off(), silent = TRUE)
# install.packages("devtools")

# Functions ------------------------
# require("devtools")

# Setup ------------------------
package.name <- "CodeAndRoll2"
package.version <- "2.5.6"

RepositoryDir <- paste0("~/GitHub/Packages/", package.name, "/")
fname <- paste0(package.name, ".R")
package.FnP <- paste0(RepositoryDir, "R/", fname)

BackupDir <- "~/GitHub/Packages/CodeAndRoll2/Development/"
dir.create(BackupDir)

# devtools::use_package("vioplot")
DESCRIPTION <- list(
  "Title" = "CodeAndRoll2 for vector, matrix and list manipulations",
  "Author" = person(given = "Abel", family = "Vertesy", email = "av@imba.oeaw.ac.at", role = c("aut", "cre")),
  "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "av@imba.oeaw.ac.at", role =  c("aut", "cre") )',
  "Description" = "CodeAndRoll2 is a set of more than 130 productivity functions. Used by MarkdownReports, ggExpress, SeuratUtils.
    Packaged version of the core functionalities  (vector, matrix and list manipulations; math) of the formerly used CodeAndRoll (v1).",
  "License" = "GPL-3 + file LICENSE",
  "Version" = package.version,
  "Packaged" = Sys.time()
  # , "Repository" =  "CRAN"
  , "Depends" = "Stringendo",
  "Imports" = "colorRamps, dplyr, gplots, graphics, grDevices, gtools, plyr, RColorBrewer, sessioninfo, sm, stringr"
  # , "Suggests" = ""
  , "BugReports" = "https://github.com/vertesy/CodeAndRoll2/issues"
)


setwd(RepositoryDir)
if (!dir.exists(RepositoryDir)) {
  create(path = RepositoryDir, description = DESCRIPTION, rstudio = TRUE)
} else {
  getwd()
  try(file.remove(c("DESCRIPTION", "NAMESPACE", "CodeAndRoll2.Rproj")))
  usethis::create_package(path = RepositoryDir, fields = DESCRIPTION, open = F)
}


# go and write fun's ------------------------------------------------------------------------
# file.edit(package.FnP)

# Create Roxygen Skeletons ------------------------
# RoxygenReady(package.FnP)

# replace output files ------------------------------------------------
BackupOldFile <- paste0(BackupDir, "Development", ".bac", print = FALSE)
AnnotatedFile <- paste0(BackupDir, "Development", ".annot.R", print = FALSE)
file.copy(from = package.FnP, to = BackupOldFile, overwrite = TRUE)
# file.copy(from = AnnotatedFile, to = package.FnP, overwrite = TRUE)

# Manual editing of descriptors ------------------------------------------------
# file.edit(package.FnP)

# Compile a package ------------------------------------------------
setwd(RepositoryDir)
getwd()
devtools::document()
warnings()

{
  "update cff version"
  citpath <- paste0(RepositoryDir, "CITATION.cff")
  xfun::gsub_file(
    file = citpath, perl = T,
    "^version: v.+", paste0("version: v", package.version)
  )
}

# Install your package ------------------------------------------------
install(RepositoryDir, upgrade = F)

# Test if you can install from github ------------------------------------------------
pak::pkg_install("vertesy/CodeAndRoll2")
# unload("CodeAndRoll2")
# require("CodeAndRoll2")
# # remove.packages("CodeAndRoll2")


# Check CRAN ------------------------------------------------
check(RepositoryDir, cran = TRUE)
# as.package(RepositoryDir)
# # source("https://install-github.me/r-lib/desc")
# # library(desc)
# # desc$set("CodeAndRoll2", "foo")
# # desc$get(CodeAndRoll2)
# system("cd ~/GitHub/CodeAndRoll2/; ls -a; open .Rbuildignore")


# Check package dependencies ------------------------------------------------
{
  depFile <- paste0(RepositoryDir, "Development/Dependencies.R")

  (f.deps <- NCmisc::list.functions.in.file(filename = package.FnP))
  # clipr::write_clip(f.deps)

  sink(file = depFile)
  print(f.deps)
  sink()
  p.deps <- gsub(x = names(f.deps), pattern = "package:", replacement = "")
  write(x = p.deps, file = depFile, append = T)
  p.dep.declared <- trimws(unlist(strsplit(DESCRIPTION$Imports, ",")))
  p.dep.new <- sort(union(p.deps, p.dep.declared))
  # clipr::write_clip(p.dep.new)
}

# Package styling, and visualization ------------------------------------------------
{
  styler::style_pkg(RepositoryDir)
  # styler::style_file("~/GitHub/Packages/CodeAndRoll2/Development/02.Compile.the.CodeAndRoll2.package.R")

  {
    # Exploring the Structure and Dependencies of my R Package:
    "works on an installed package!"
    pkgnet_result <- pkgnet::CreatePackageReport(package.name)
    fun_graph <- pkgnet_result$FunctionReporter$pkg_graph$"igraph"

    # devtools::load_all('~/GitHub/Packages/PackageTools/R/DependencyTools.R')
    convert_igraph_to_mermaid(graph = fun_graph, openMermaid = T, copy_to_clipboard = T)
  }

  if (F) {
    # Add @importFrom statements
    (FNP <- package.FnP)
    PackageTools::add_importFrom_statements(FNP, exclude_packages = "")
    add_importFrom_statements(FNP, exclude_packages = "")
  }
}
