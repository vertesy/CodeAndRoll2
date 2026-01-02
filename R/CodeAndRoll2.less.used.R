##################################################################### _
# CodeAndRoll2 Less used functions ----
##################################################################### _
# source('~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.less.used.R')



# _________________________________________________________________________________________________
# Deprecated ----
# _________________________________________________________________________________________________


# DON'T DELETE: FOR BACKTRACKING
# _________________________________________________________________________________________________
#' @title as.numeric.wNames.deprecated
#'
#' @description Converts any vector into a numeric vector, and puts the original character values
#' into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
#' @param vec input vector
#'
# #' @export as.numeric.wNames.deprecated
as.numeric.wNames.deprecated <- function(vec) {
  .Deprecated("as.numeric.wNames.factor")
  numerified_vec <- as.numeric(as.factor(vec)) - 1 # as factor gives numbers [1:n] instead [0:n]
  if (!is.null(names(vec))) {
    names(numerified_vec) <- names(vec)
  }
  return(numerified_vec)
}



# _________________________________________________________________________________________________
#' @title as.named.vector.deprecated
#'
#' @description Convert a dataframe column or row into a vector, keeping the corresponding dimension name.
#' @param df_col data frame column
#' @param WhichDimNames Shall we extract rows (2) or columns (1, default)?, Default: 1
#'
# #' @export as.named.vector.deprecated
as.named.vector.deprecated <- function(df_col, WhichDimNames = 1) {
  .Deprecated("as.named.vector.df")
  stopifnot(
    "Input must be one-dimensional (vector or single column)" =
      is.null(dim(df_col)) || ncol(df_col) == 1
  )
  namez <- dimnames(df_col)[[WhichDimNames]]

  # use RowNames: WhichDimNames = 1 , 2: use ColNames
  # !!! might require drop = FALSE in subsetting!!! eg: df_col[, 3, drop = FALSE]
  # df_col[which(unlist(lapply(df_col, is.null)))] = "NULL" # replace NULLs - they would fall out of vectors - DOES not work yet
  if (is.list(df_col) && !is.data.frame(df_col)) namez <- names(df_col)

  if (inherits(df_col, "tbl_df")) warning("Tibbles have now rownames. Synthetic row indices detected.", immediate. = TRUE)

  vecc <- as.vector(unlist(df_col))
  names(vecc) <- namez
  return(vecc)
}



# _________________________________________________________________________________________________
#' @title as.factor.numeric.deprecated
#'
#' @description  Turn any vector into numeric categories as.numeric(as.factor(vec))
#' @param vec vector of factors or strings
#' @param rename Rename the vector?
#' @param ... Pass any other argument. to as.factor()
#' @examples as.factor.numeric(LETTERS[1:4])
#'
#' @export as.factor.numeric

as.factor.numeric <- function(vec, rename = FALSE, ...) {
  .Deprecated("as.numeric.wNames.factor")

  vec2 <- as.numeric(as.factor(vec, ...))
  names(vec2) <- if (!rename && !is.null(names(vec))) {
    names(vec)
  } else {
    vec
  }
  return(vec2)
}


# _________________________________________________________________________________________________
#' @title as.named.vector.df
#'
#' @description Convert any column or row of a dataframe into a vector, keeping the
#' corresponding dimension name.
#' @param df A dataframe.
#' @param col.or.row.name.or.index Which column or row to extract (numeric index).
#' @param verbose Print the columnname or rowname that is being used
#' @param WhichDimNames Shall we extract rows (2) or columns (1, default)?, Default: 1
#' @param ... Additional arguments passed to `as.vector()`.
#'
#' @export
as.named.vector.df <- function(
    df, col.or.row.name.or.index = 1, verbose = TRUE,
    WhichDimNames = 1,
    ...) {
  .Deprecated(old = "as.named.vector.df", new = "df.col.2.named.vector or df.row.2.named.vector")

  if (verbose) message("input df dimensions: ", kppc(idim(df)))

  name.selection <- dimnames(df)[[(3 - WhichDimNames)]][col.or.row.name.or.index]
  if (verbose) iprint("Variable used:", name.selection)

  vecc <- if (WhichDimNames == 1) {
    as.vector(unlist(df[, col.or.row.name.or.index]), ...)
  } else if (WhichDimNames == 2) {
    as.vector(unlist(df[col.or.row.name.or.index, ]), ...)
  }

  names(vecc) <- dimnames(df)[[WhichDimNames]]
  return(vecc)
}


# # _________________________________________________________________________________________________
# #' @title as.named.vector.df
# #'
# #' @description Convert any column or row of a dataframe into a vector, keeping the
# #' corresponding dimension name.
# #'
# #' @param df A dataframe.
# #' @param col.or.row.name.or.index Which column or row to extract (numeric index).
# #' @param verbose Print the columnname or rowname that is being used
# #' @param WhichDimNames Shall we extract rows (1) or columns (2, default)?, Default: 1
# #' @param ... Additional arguments passed to `as.vector()`.
# #'
# #' @export
# as.named.vector.df <- function(
    #     df, col.or.row.name.or.index = 1, verbose = TRUE,
#     WhichDimNames = 2,
#     ...) {
#
#   # name.selection <- dimnames(df)[[(3 - WhichDimNames)]][col.or.row.name.or.index] # Original not working properlu
#
#   if(verbose) {
#     tag <- if(WhichDimNames == 1) "row" else "column"
#     message("input df dimensions: ", kppc(idim(df)))
#     if(is.numeric(col.or.row.name.or.index)) name.of.selection <- dimnames(df)[[WhichDimNames]][col.or.row.name.or.index]
#     message("Selecting: ", tag, " ", col.or.row.name.or.index, ", called: ", name.selection)
#   }
#
#   # Extract the column or row
#   vecc <- if (WhichDimNames == 1) {
#     as.vector(unlist(df[, col.or.row.name.or.index]), ...)
#   } else if (WhichDimNames == 2) {
#     as.vector(unlist(df[col.or.row.name.or.index, ]), ...)
#   }
#
#   names(vecc) <- dimnames(df)[[WhichDimNames]]
#   return(vecc)
# }



# # _________________________________________________________________________________________________
# #' @title topN.dfCol
# #' @description Find the n highest values in a named vector.
# #' @param df_col data frame column, Default: `as.named.vector(df[, 1, drop = FALSE])`
# #' @param n top N values, Default: 5
# #' @export
# topN.dfCol <- function(df_col = as.named.vector(df[, 1, drop = FALSE]), n = 5) {
#   .Deprecated()
#   head(sort(df_col, decreasing = TRUE), n = n)
# } # Find the n highest values in a named vector


# # _________________________________________________________________________________________________
# #' @title bottomN.dfCol
# #' @description Find the n lowest values in a named vector.
# #' @param df_col data frame column, Default: `as.named.vector(df[, 1, drop = FALSE])`
# #' @param n lowest N values, Default: 5
# #' @export
# bottomN.dfCol <- function(df_col = as.named.vector(df[, 1, drop = FALSE]), n = 5) {
#   .Deprecated()
#   head(sort(df_col, decreasing = FALSE), n = n)
# } # Find the n lowest values in a named vector



# #' @title sort.mat
# #' @export sort.mat
# sort.mat <- function() .Deprecated("sort_matrix_rows()")


# #' @title is.list2
# #' @export is.list2
# is.list2 <- function() .Deprecated("is.list.simple()")
