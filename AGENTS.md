# AGENTS

## Overview
This repository contains the **CodeAndRoll2** R package, a collection of utility functions for vector, matrix, and list manipulations.

### Structure
- `R/` – core package functions.
  - `CodeAndRoll2.R` exposes the main utilities.
  - `deprecated.R` lists legacy helpers that should not be used in new code.
- `man/` – roxygen2-generated documentation.
- `DESCRIPTION` & `NAMESPACE` – package metadata.
- `Development/` – auxiliary scripts used for building and maintenance; not required for package use.
- `README.md` – package description, installation instructions, and an extensive function catalogue.

## Development Workflow
1. Use R (>= 4.0).
2. After editing code in `R/`, keep roxygen2-style comments and regenerate documentation with:
   ```r
   devtools::document()
   ```
3. Run checks before committing:
   ```sh
   R CMD build .
   R CMD check CodeAndRoll2_*.tar.gz
   ```
   (or in R: `devtools::check()`).
4. Delete the generated `CodeAndRoll2_*.tar.gz` archive and `CodeAndRoll2.Rcheck` directory after the check.
5. Commit only when checks complete without errors or warnings.

## Getting Started for New Contributors
- Read `README.md` for a package overview and list of available functions.
- Explore `R/CodeAndRoll2.R` to see implementation style and naming conventions.
- Review `DESCRIPTION` to understand package dependencies (e.g., Stringendo, ReadWriter).
- Check `deprecated.R` to avoid relying on obsolete functions.
- Suggested next steps: learn roxygen2 for documentation, devtools for package development, and review dependent packages (MarkdownReports, ggExpress, Seurat.utils) to see CodeAndRoll2 in action.

