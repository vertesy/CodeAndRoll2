##################################################################### _
# CodeAndRoll2 - A collection of custom R functions ----
##################################################################### _
# source('~/GitHub/Packages/CodeAndRoll2/R/CodeAndRoll2.R')
# source('https://raw.githubusercontent.com/vertesy/CodeAndRoll2/master/CodeAndRoll2.R')
# source('~/.pack.R')

# devtools::check_man("~/GitHub/Packages/CodeAndRoll2")
# devtools::load_all("~/GitHub/Packages/CodeAndRoll2")
# devtools::document("~/GitHub/Packages/CodeAndRoll2")
# file.edit("~/GitHub/Packages/CodeAndRoll2/Development/Create_the_CodeAndRoll2_Package.R")



### CHAPTERS:

# _________________________________________________________________________________________________
## Rstudio based context retrieval ____________________________________________________________ ----

#' @title Get Current Script Name or Basename of Output Directory
#'
#' @description This function attempts to retrieve the name of the currently opened script in
#' the RStudio editor. If the script name cannot be obtained or if the `rstudioapi` package is
#' not available, it returns the basename of the directory specified by `OutDir`.
#'
#' @return A string containing the basename of the current script or the basename of `OutDir`
#' if the script name is unavailable.
#' @importFrom rstudioapi getSourceEditorContext
#'
#' @export
getScriptName <- function() {
  # Check if rstudioapi is available
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    message("rstudioapi package is not available. Please install it using install.packages('rstudioapi').")
  } else {
    scriptName <- basename(rstudioapi::getSourceEditorContext()$path)
  }
  # If scriptName is empty, return basename of OutDir
  # Can happen at an unsaved file, etc.
  if (scriptName == "") scriptName <- basename(OutDir)

  return(scriptName)
}

# _________________________________________________________________________________________________
#' @title getProject
#'
#' @description Try to get the project name you are wokring on in Rstudio.
#' @returns The final subfolder of your project, or NULL, if you are not running one
#' @importFrom rstudioapi getActiveProject
#' @examples getProject()
#'
#' @export
getProject <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    warning("rstudioapi package is not available. Please install it using install.packages('rstudioapi').",
            immediate. = TRUE
    )
  } else {
    tryCatch(basename(rstudioapi::getActiveProject()), error = function(e) {})
  }
}

# _________________________________________________________________________________________________
#' @title Save Command History to "command_history.date.scriptname.txt"
#'
#' @description
#' This function saves the command history of the current R session to a text file. The file name
#' includes the current date and, if available, the name of the current R script (when running in
#' RStudio). The file is saved in the current working directory.
#'
#' @return Nothing is returned, but the file path is printed to the console.
#'
#' @importFrom rstudioapi getSourceEditorContext
#' @examples
#' \dontrun{
#' savehistory_2()
#' }
savehistory_2 <- function() {
  # Get the current working directory
  current_dir <- getwd()

  # Construct the file name using the current date and optionally the file name from RStudio
  script_name <- try(basename(rstudioapi::getSourceEditorContext()$path), silent = TRUE)
  if ("try-error" %in% is(script_name)) script_name <- ""

  file_name <- ppp(
    "command_history",
    format(Sys.time(), format = "%Y.%m.%d"),
    script_name, "txt"
  )

  # Save the command history
  savehistory(file = file_name)

  # Print and return the file path
  print(file.path(current_dir, file_name))
}


# _________________________________________________________________________________________________
## Pipetools ____________________________________________________________ ----

#' @title Print and Return an Object in a Pipe
#'
#' @description Prints the input object and returns it, enabling you to inspect values inside a pipe.
#'
#' @param x The object to print and return. Default: None.
#'
#' @return The input object `x`, unchanged.
#'
#' @examples
#' results <- c(1, 2, 3) %>% pSee() %>% sqrt() %>% tail(2) ; results
pSee <- function(x, max_elements = 100) {
  if(max_elements) y <- head(x)
  message(kppc(y))
  return(x)
}


#' @title Print Length and Return an Object in a Pipe
#'
#' @description Prints the length of the input object and returns it, allowing you to verify
#' length inside a pipe operation.
#'
#' @param x The object whose length to print and return. Default: None.
#'
#' @return The input object `x`, unchanged.
#'
#' @examples
#' results <- c(9:1) %>% tail(4) %>% pLength() %>% sqrt() ; results
pLength <- function(x) {
  stopifnot(!missing(x), is.vector(x) || is.list(x))
  message("length: ",length(x))
  return(x)
}


# _________________________________________________________________________________________________
## Create and check variables ____________________________________________________________ ----

#' @title vec.fromNames
#'
#' @description Create a vector from a vector of names.
#' @param name_vec A vector of names, Default: `LETTERS[1:5]`
#' @param fill The value to fill the new vector, Default: `NA`
#' @export
vec.fromNames <- function(name_vec = LETTERS[1:5], fill = NA) {
  v <- numeric(length(name_vec))
  if (length(fill) == 1) {
    v <- rep(fill, length(name_vec))
  } else if (length(fill == length(name_vec))) {
    v <- fill
  }
  names(v) <- name_vec
  return(v)
}


# _________________________________________________________________________________________________
#' @title Create a Named List from a Character Vector of Names, or a names of an object
#'
#' @description This function takes a character vector of names and creates a list where each
#' element is named according to the character vector and filled with a specified value.
#' @param x A character vector of names, with a default of the first five letters of the alphabet.
#' @param fill The value used to fill the elements of the list, with a default of `NaN`.
#' @param use.names Logical. If `TRUE`, the names of the input vector are used as names for the list.
#'
#' @return A named list with elements filled with the specified value.
#' @examples
#' list.fromNames() # Default behavior with `LETTERS[1:5]` and `NaN`
#' @export
list.fromNames <- function(x = LETTERS[1:5], fill = NaN, use.names = FALSE) {
  liszt <- as.list(rep(fill, length(x)))

  # names(liszt) <-
  #   if (is.character(x) | ) {
  #     x
  #   } else {
  #     if (!is.null(names(x))) {
  #       names(x)
  #     } else {
  #       stop("Input is nor a chacter vector, nor an object with names")
  #     }
  #   }

  names(liszt) <-
    if (!is.null(names(x)) & use.names) {
      names(x)
    } else {
      x
    }

  kollapse("List of", length(liszt), "| names:", head(names(liszt)), "...", collapseby = " ")
  return(liszt)
}


# _________________________________________________________________________________________________
#' @title vec.from.template
#'
#' @description Create a vector from a names of another vector / list / etc.
#' @param x A vector of names, Default: `LETTERS[1:5]`
#' @param fill The value to fill the new vector, Default: `NA`
#' @export
vec.from.template <- function(x = table(LETTERS[1:5]), fill = NA) {
  stopifnot(is.list(x) | is.vector(x) | is.table(x))
  v <- rep(fill, length(x))
  names(v) <- names(x)
  return(v)
}


# _________________________________________________________________________________________________
#' @title list.from.template
#'
#' @description Create an empty list from a template list, copying names and filling values with NA.
#' @param x A template list with names,.
#' @param fill The value to fill the new list, Default: `NA`
#'
#' @export
list.from.template <- function(x, fill = NA) {
  stopifnot(is(x)[1] == "list" | is.vector(x) | is.table(x))
  liszt <- as.list(rep(fill, length(x)))
  names(liszt) <- names(x)
  return(liszt)
}


# _________________________________________________________________________________________________
#' @title matrix.fromNames
#' @description Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.
#' @param rowname_vec A vector of names for rows, Default: `1:10`
#' @param colname_vec A vector of names for columns, Default: `LETTERS[1:5]`
#' @param fill The value to fill the new vector, Default: `NA`
#' @export
matrix.fromNames <- function(rowname_vec = 1:10, colname_vec = LETTERS[1:5], fill = NA) {
  mx <- matrix(
    data = fill, nrow = length(rowname_vec), ncol = length(colname_vec),
    dimnames = list(rowname_vec, colname_vec)
  )
  iprint("Dimensions:", dim(mx))
  return(mx)
}

# _________________________________________________________________________________________________
#' @title data.frame.fromNames
#' @description Create a data frame from 2 vectors defining the row- and column names of the
#' data frame Default fill value: NA.
#' @param rowname_vec A vector of names for rows, Default: `1:10`
#' @param colname_vec A vector of names for columns, Default: `LETTERS[1:5]`
#' @param fill The value to fill the new vector, Default: `NA`
#' @export
data.frame.fromNames <- function(rowname_vec = 1:10, colname_vec = LETTERS[1:5], fill = NA) {
  df <- matrix(
    data = fill, nrow = length(rowname_vec), ncol = length(colname_vec),
    dimnames = list(rowname_vec, colname_vec)
  ) |> as.data.frame()
  iprint("Dimensions:", dim(df))
  return(df)
}



# _________________________________________________________________________________________________
#' @title matrix.fromVector
#' @description Create a matrix from values in a vector repeated for each column / each row.
#' Similar to rowNameMatrix and colNameMatrix.
#' @param vector Input vector for rows (number of rows = length), Default: `1:5`.
#' @param HowManyTimes Number of columns, Default: `3`.
#' @param IsItARow Transpose? Swap rows an columns. Default: `TRUE`
#' @export
matrix.fromVector <- function(vector = 1:5, HowManyTimes = 3, IsItARow = TRUE) {
  matt <- matrix(vector, nrow = length(vector), ncol = HowManyTimes)
  if (!IsItARow) {
    matt <- t(matt)
  }
  return(matt)
}



# _________________________________________________________________________________________________
#' @title array.fromNames
#' @description Create an N-dimensional array from N vectors defining the row-, column, etc names of the array.
#' @param rowname_vec A vector of names for rows, Default: `1:3`
#' @param colname_vec A vector of names for columns, Default: `letters[1:2]`
#' @param z_name_vec A vector of names for Z dimension, Default: `LETTERS[4:6]`
#' @param fill The value to fill the new vector, Default: NA
#' @export
array.fromNames <- function(
    rowname_vec = 1:3, colname_vec = letters[1:2],
    z_name_vec = LETTERS[4:6], fill = NA) {
  DimNames <- list(rowname_vec, colname_vec, z_name_vec)
  Dimensions_ <- lapply(DimNames, length)
  mx <- array(data = fill, dim = Dimensions_, dimnames = DimNames)
  iprint("Dimensions:", dim(mx))
  return(mx)
}



# _________________________________________________________________________________________________
#' @title what
#' @description A better version of is(). It can print the first "printme" elements.
#' @param x An object to identify.
#' @param printme print the first "printme" elements, Default: 0
#' @export
what <- function(x, printme = 0) {
  iprint(is(x), "; nr. of elements:", length(x))
  if (is.numeric(x)) {
    iprint("min&max:", range(x))
  } else {
    print("Not numeric")
  }
  if (length(dim(x)) > 0) {
    iprint("Dim:", dim(x))
  }
  if (printme > 0) {
    iprint("Elements:", x[0:printme])
  }
  head(x)
}



# _________________________________________________________________________________________________
#' @title idim
#' @description A dim() function that can handle if you pass on a vector: then, it gives the length.
#' @param any_object An object to identify.
#' @export
idim <- function(any_object) {
  if (is.null(dim(any_object))) {
    if (is.list(any_object)) {
      print("list")
    } # if
    print(length(any_object))
  } else {
    print(dim(any_object))
  }
}


# _________________________________________________________________________________________________
#' @title Test if object is a list
#' @description The 'is.list()' function fails on tibbles: it returns TRUE, as if it were a list. This distiguishes. Thaat's why we need this function.
#' @param object Object to test.
#' @export
#' @examples is.list2(list())
#' is.list2(dplyr::tibble())
is.list2 <- function(object) {
  "list" %in% class(object)
}



# _________________________________________________________________________________________________
#' @title idimnames
#' @description A dimnames() function that can handle if you pass on a vector: it gives back the names.
#' @param any_object Any object with N dimensions (with or w/o names).
#' @param print_max Max number of names to print per dimension. Default 25.
#' @export
idimnames <- function(any_object, print_max = 25) {
  iprint("print max:", print_max, "names.")
  if (!is.null(dimnames(any_object))) {
    dimNamesShort <- lapply(dimnames(any_object), head, n = print_max)
    idim(any_object)
    print(dimNamesShort)
  } else if (!is.null(colnames(any_object))) {
    iprint("colnames:", colnames(any_object))
  } else if (!is.null(rownames(any_object))) {
    iprint("rownames:", rownames(any_object))
  } else if (!is.null(names(any_object))) {
    iprint("names:", names(any_object))
  }
}


# _________________________________________________________________________________________________
#' @title printEveryN
#'
#' @description Report iterator value at every e.g. 1000
#' @param i Current iteration of the loop. Default: i.
#' @param N  print Every N
#' @param prefix Optional prefix to display alongside the progress.
#'
#' @export
printEveryN <- function(i = i, N = 1000, prefix = NULL) {
  if ((i %% N) == 0) message(prefix, i)
}

# _________________________________________________________________________________________________
#' @title Print Loop Progress
#'
#' @description Prints the progress of a loop as a number and percentage.
#'
#' @param i Current iteration of the loop. Default: i.
#' @param total Total number of iterations in the loop.
#' @param digits Digits to display
#' @param message Optional message to display alongside the progress.
#'
#' @export
printProgress <- function(i = i, total, message = "Progress", digits = 0) {
  percentage <- formatC(100 * i / total, format = "f", digits = digits)
  cat(paste0(message, ": ", i, "/", total, " (", percentage, "%)\n"))
}


# _________________________________________________________________________________________________
#' @title table_fixed_categories
#'
#' @description Generate a table() with a fixed set of categories. It fills up the table with
#' missing categories, that are relevant when comparing to other vectors.
#'
#' @param vec Input vector to be counted.
#' @param categories_vec Fixed list of categories to be counted in your input vector.
#' @param strict Stop or warn if not all values are covered in the categories vector?
#' @param v Verbose. Default: TRUE.
#'
#' @return A table with the fixed set of categories.
#'
#' @export

table_fixed_categories <- function(vec, categories_vec, strict = TRUE,
                                   v = TRUE) {
  if (!is.vector(vec)) {
    iprint("vec is not a vector -  it is a:", is(vec)[1])
  }

  missing_from_category <- unique(vec) %!in% categories_vec
  if (any(missing_from_category)) {
    txt1 <- pc_TRUE(
      logical_vector = missing_from_category, NumberAndPC = TRUE,
      suffix = "values are NOT found in the categories vector!"
    )
    if (strict) stop(txt1) else warning(txt1)
  }

  txt2 <- pc_TRUE(
    logical_vector = categories_vec %in% vec, NumberAndPC = TRUE,
    suffix = "categories are found in the vector"
  )
  if (v) print(txt2)

  table(factor(unlist(vec), levels = categories_vec))
}



# _________________________________________________________________________________________________
#' @title Frequency Table with Sorting Option
#'
#' @description
#' This function generates a frequency table of the input vector `vec` and allows the option
#' to sort the table in decreasing or increasing order. It handles NA values.
#'
#' @param vec A vector. The vector for which the frequency table is to be generated.
#' @param decreasing Logical. Should the table be sorted in decreasing order? Default: `TRUE`.
#' @param useNA A string. Specifies how to handle NA values. Can be "ifany", "always", or "no".
#' Default: `"ifany"`.
#'
#' @return A frequency table sorted based on the `decreasing` argument.
#' @examples
#' vec <- c(1, 2, 2, NA, 3, 3, 3)
#' table_decreasing(vec)
#' table_decreasing(vec, decreasing = FALSE)
#'
#' @export
table_decreasing <- function(vec, decreasing = TRUE, useNA = "ifany") {
  stopifnot(useNA %in% c("ifany", "always", "no"))
  tbl <- table(vec, useNA = useNA)
  if (decreasing) sort.decreasing(tbl) else sort(tbl)
}


# _________________________________________________________________________________________________
#' @title Frequency Table with Hyrid Sorting: you can sort by frequency and by specified value
#'
#' @description
#' This function generates a frequency table of the input vector `vec` and displays the table
#' sorted by frequency and by a set of specified values. It handles NA values.
#'
#' @param vec A vector. The vector for which the frequency table is to be generated.
#' @param first_elements Logical. Should the table be sorted in decreasing order? Default: `TRUE`.
#' @param useNA A string. Specifies how to handle NA values. Can be "ifany", "always", or "no".
#' Default: `"ifany"`.
#'
#' @return A frequency table sorted.
#' @examples
#' vec <- c(1, 2, 2, NA, 3, 3, 3)
#' table_decreasing_hybrid(vec)
#' table_decreasing_hybrid(vec, first_elements = "1")
#'
#' @export
table_decreasing_hybrid <- function(vec, first_elements = FALSE, useNA = "ifany") {
  stopifnot(useNA %in% c("ifany", "always", "no"))
  tbl <- table(vec, useNA = useNA)

  tbl_decr <- sort.decreasing(tbl)
  if (isFALSE(first_elements)) {
    return(tbl_decr)
  } else {
    if (!all(first_elements %in% names(tbl_decr))) warning("Not all elements are found in the table!", immediate. = TRUE)
    first_elements <- intersect(first_elements, names(tbl_decr))
    tbl_decr <- c(tbl_decr[first_elements], tbl_decr[!names(tbl_decr) %in% first_elements])
    return(tbl_decr)
  }
}

# _________________________________________________________________________________________________
#' @title getCategories
#' @description Extract unique entries with a corresponding name.
#' @param named_categ_vec A vector of categories with names.
#' "Uniqueness" in the vector and its name should be the same!!!
#' @export
#' @examples getCategories(c("A" = 1, "B" = 1, "C" = 2, 3))
getCategories <- function(named_categ_vec) {
  named_categ_vec[names(unique(named_categ_vec))]
}



# _________________________________________________________________________________________________
## Vector operations ____________________________________________________________ ----


#' @title Count the number of unique values
#'
#' @description Count the number of unique values
#' @param x vector
#' @export
nr.unique <- function(x) {
  if (is.data.frame(x)) x <- x[[1]]
  length(unique(x))
}



#' @title grep that returns the value of the match.
#'
#' @description grep returning the value. A character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning. Missing values are allowed except for regexpr, gregexpr and regexec.
#' @param pattern pattern to look for
#' @param x The haystack to search through. a character vector where matches are sought, or an object which can be coerced by as.character to a character vector. Long vectors are supported.
#' @param ignore.case Ignore letter case, Default: FALSE
#' @param perl logical. Should Perl-compatible regexps be used? Default: FALSE
#' @param value if FALSE, a vector containing the (integer) indices of the matches determined by grep is returned, and if TRUE, a vector containing the matching elements themselves is returned. Default: FALSE
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments. Default: FALSE
#' @param useBytes logical. If TRUE the matching is done byte-by-byte rather than character-by-character. See ‘Details’., Default: FALSE
#' @param invert logical. If TRUE return indices or values for elements that do not match. Default: FALSE
#' @param ... Pass any other argument.
#' @export
grepv <- function(
    pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
    fixed = FALSE, useBytes = FALSE, invert = FALSE, ...) {
  grep(pattern, x,
       ignore.case = ignore.case, perl = perl, fixed = fixed,
       useBytes = useBytes, invert = invert, ..., value = TRUE
  )
}



# _________________________________________________________________________________________________
#' @title most_frequent_elements
#' @description Show the most frequent elements of a table.
#' @param vec input vector
#' @param topN How many pof the most frequent elements should be returned? Default: 10
#' @export
most_frequent_elements <- function(vec, topN = 10) {
  tail(sort(table(vec, useNA = "ifany")), topN)
}


# _________________________________________________________________________________________________
#' @title count_occurrence_each_element
#' @description Count the number of times each element occurs in the full vector, AND give it back as a vector, that is the same length as the input vector, each element corresponding one-by-one.
#' @param vec input vector
#' @export
count_occurrence_each_element <- function(vec) {
  tableX <- table(vec)
  rep(x = tableX, tableX)
}




# _________________________________________________________________________________________________
#' @title top_indices
#'
#' @description Returns the positions of the `n` highest values in `x`. For equal values, it maintains the original order.
#' @param x A vector.
#' @param n The number of highest values to return.
#' @param top Whether to return the `n` highest (or `n` lowest) values.
#' @return A vector of indices.
#' @examples top_indices(rnorm(10), n = 3)
#'
#' @export
top_indices <- function(x, n = 3, top = TRUE) {
  head(order(x, decreasing = top), n)
}



# _________________________________________________________________________________________________
#' @title trail
#'
#' @description A combination of `head()` and `tail()`: Returns a vector containing the first
#' `N` and last `N` elements of vector.
#'
#' @param vec A vector.
#' @param N The number of elements to include from each end.
#' @return A vector.
#' @examples trail(rnorm(10), N = 3)
#'
#' @export
trail <- function(vec, N = 10) unique(c(head(vec, n = N), tail(vec, n = N)))



# _________________________________________________________________________________________________
#' @title sort.decreasing
#'
#' @description Sorts `vec` in decreasing order.
#' @param vec A vector.
#' @return A sorted vector.
#' @examples sort.decreasing(rnorm(10))
#'
#' @export sort.decreasing
sort.decreasing <- function(vec) sort(vec, decreasing = TRUE) # Sort in decreasing order.


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

# _________________________________________________________________________________________________
#' @title as.named.vector.2colDF
#'
#' @description Convert a 2-column dataframe (value, name) into a named vector. Use for simple tibbles.
#' @param df data frame
#' @param values Index of column with values, Default: 1
#' @param names Index of column with names, Default: 2
#' @param make.names make.names, Default: F
#'
#' @export
as.named.vector.2colDF <- function(df, values = 1, names = 2, make.names = FALSE) {
  vec <- df[[values]]
  names(vec) <- df[[names]]
  if (make.names) names(vec) <- make.names(names(vec))
  return(vec)
}


# _________________________________________________________________________________________________
#' @title df.col.2.named.vector
#'
#' @description Convert a dataframe column into a vector, keeping the corresponding dimension name.
#' @param df data frame
#' @param col column index
#' @export
df.col.2.named.vector <- function(df, col) {
  vec <- df[, col]
  names(vec) <- rownames(df)
  return(vec)
}

# df.col.2.named.vector <- function(df_col) {
#   namez <- rownames(df_col)
#   vecc <- as.vector(unlist(df_col))
#   names(vecc) <- namez
#   return(vecc)
# }


# _________________________________________________________________________________________________
#' @title df.row.2.named.vector
#'
#' @description Convert a dataframe row into a vector, keeping the corresponding dimension name.
#' @param df_row data frame row
#' @export
df.row.2.named.vector <- function(df_row) {
  "Needs update see above"
  namez <- colnames(df_row)
  vecc <- as.vector(unlist(df_row))
  names(vecc) <- namez
  return(vecc)
}



# _________________________________________________________________________________________________
#' @title tibble_summary_to_namedVec
#' @description Convert a key-value tibble into a named vector (as opposed to using rownames).
#' @param tbl A tibble, Default: dplyr::tibble(key = sample(x = 1:5, size = 20, replace = TRUE), value = rnorm(20))
#' @param idx PARAM_DESCRIPTION, Default: c(key = 1, value = 2)
#' @seealso
#'  \code{\link[dplyr]{reexports}}
#' @examples tibble_summary_to_namedVec()
#' @importFrom dplyr tibble
#'
#' @export
tibble_summary_to_namedVec <- function(
    tbl = dplyr::tibble("key" = sample(x = 1:5, size = 20, replace = TRUE), "value" = rnorm(20)),
    idx = c(key = 1, value = 2)) {
  iprint("The following name and value columns are taken:", colnames(tbl[idx]), "; with indices:", idx)
  tbl_2_col <- tbl[, idx]
  named.vec <- tbl_2_col[[2]]
  names(named.vec) <- tbl_2_col[[1]]
  return(named.vec)
}



# _________________________________________________________________________________________________
#' @title as_tibble_from_namedVec
#' @description Convert a vector with names into a tibble, keeping the names as rownames.
#' @param vec.w.names A vector with names, Default: c(a = 1, b = 2)
#' @param transpose Transpose? Default: T
#' @examples as_tibble_from_namedVec()
#' @importFrom dplyr bind_rows
#'
#' @export
as_tibble_from_namedVec <- function(vec.w.names = c("a" = 1, "b" = 2), transpose = TRUE) {
  stopifnot(!is.null(names(vec.w.names)))
  tbl <- dplyr::bind_rows(vec.w.names)
  if (transpose) t(tbl) else tbl
}


# _________________________________________________________________________________________________
#' @title Unique elements
#' @description Get the unique elements of a vector, keep their names
#' @param x A vector with names
#' @export unique.wNames
unique.wNames <- function(x) {
  x[!duplicated(x)]
}




# _________________________________________________________________________________________________
#' @title as.numeric.wNames.character
#'
#' @description Converts (1) a 'character' v. into a numeric v., or
#' a 'factor' v. as as.numeric(as.character(vec)) and preserves the original names.
#' The old 'as.numeric.wNames()' is deprecated as it was not clearly documented that it converts via facotr in any case. Code saved at the end.
#' @param vec input vector
#' @param verbose Print troubleshooting messages
#' @param factor.to.character convert Input vector to first to 'character', then numeric.
#' @param ... Pass any other argument to as.numeric()
#' @examples vec <- as.character(c(1, 2, 8, 9))
#' names(vec) <- LETTERS[1:4]
#' vec
#' as.numeric.wNames.character(vec)
#' vec2 <- as.factor(c(1, 2, 8, 9))
#' names(vec2) <- LETTERS[1:4]
#' vec2
#' as.numeric.wNames.character(vec2, factor.to.character = FALSE)
#'
#' @export as.numeric.wNames.character
as.numeric.wNames.character <- function(
    vec, verbose = TRUE,
    factor.to.character = TRUE, ...) {
  if (is.character(vec) | is.logical(vec)) {
    numerified_vec <- as.numeric(vec, ...)
  } else {
    if (verbose) print("Input vector is not 'character' or 'logical'.")
    if (is.factor(vec)) {
      if (verbose) print("Input vector is factor.")
      if (factor.to.character) {
        if (verbose) {
          print("Input vector converted to 'character', then numeric.")
        }
        numerified_vec <- as.numeric(as.character(vec), ...)
      } else {
        warning("Input is factor, now converted as is")
        numerified_vec <- as.numeric(vec, ...)
      } # else / factor.to.character
    } else {
      warning("Input vector is not character/logical/factor. Simple conversion attempted.")
      numerified_vec <- as.numeric(vec, ...)
    } # else / is.factor
  } # else / is.character or is.logical

  if (is.null(names(vec))) {
    if (verbose) warning("Input vector has no names!")
  } else {
    names(numerified_vec) <- names(vec)
  } # else: has names

  return(numerified_vec)
}



# _________________________________________________________________________________________________
#' @title as.numeric.wNames.factor
#'
#' @description  Turn any vector into numeric categories as.numeric(as.factor(vec))
#' Forerly as.factor.numeric
#' @param vec vector of factors, strings, (or even logical)
#' @param ... Pass any other argument to as.factor()
#' @examples as.numeric.wNames.factor(LETTERS[1:4])
#'
#' @export as.numeric.wNames.factor

as.numeric.wNames.factor <- function(vec, ...) {
  if (is.character(vec)) warning("Input is character, now converted via as.factor()")
  if (is.logical(vec)) warning("Input is logical, now converted via as.factor()")

  numerified_vec <- as.numeric(as.factor(vec, ...))

  if (is.null(names(vec))) {
    warning("Input vector has no names!")
  } else {
    names(numerified_vec) <- names(vec)
  } # else: has names

  return(numerified_vec)
}



# _________________________________________________________________________________________________
#' @title as.character.wNames
#' @description Converts your input vector into a character vector, and puts the original
#' character values into the names of the new vector, unless it already has names.
#' @param vec input vector
#'
#' @export as.character.wNames
as.character.wNames <- function(vec) {
  char_vec <- as.character(vec)
  if (!is.null(names(vec))) {
    names(char_vec) <- names(vec)
  }
  return(char_vec)
}


# _________________________________________________________________________________________________
#' @title Translate a set of values to a new set using a dictionary
#'
#' @description Replaces a set of values in a vector with another set of values, so
#' it translates your vector. Oldvalues and newvalues have to be 1-to-1
#' corresponding vectors.  'chartr("a-cX", "D-Fw", x) does the same as above
#' in theory, but it did not seem very robust regarding your input...'
#'
#' @param vec set of values where you want to replace
#' @param old oldvalues, a vector of values that occur in `vec` to be replaced.
#' @param new newvalues, a vector of equal length, to be copied from, corresponding 1-by-1 to old.
#' @examples A <- 1:3
#' translate(vec = A, old = 2:3, new = letters[1:2])
#'
#' @export
translate <- function(vec, old, new) {
  stopifnot(length(old) == length(new) | length(new) == 1)
  # "PROVIDE ONE NEW VALUE, OR THE SAME NUMBER OF NEW VALUES AS OLD VALUES!"

  if (length(old) > length(new) & length(new) == 1) {
    new <- rep(new, length(old))
  }

  vec_replaced <- vec
  for (i in 1:length(old)) {
    oldval <- old[i]
    vec_replaced[vec == oldval] <- new[i]
    printEveryN(i = i, N = 1000)
  }
  return(vec_replaced)
}


# _________________________________________________________________________________________________
#' @title rescale
#' @description Linear transformation to a given range of values.
#' @param vec input vector
#' @param from min, Default: 0
#' @param upto max, Default: 100
#' @export
rescale <- function(vec, from = 0, upto = 100) {
  vec <- vec - min(vec, na.rm = TRUE)
  vec <- vec * ((upto - from) / max(vec, na.rm = TRUE))
  vec <- vec + from
  return(vec)
} # fun

# _________________________________________________________________________________________________
#' @title fractions
#' @description x/sum(x)
#' @param vec input vector
#' @param na_rm remove NAs
#' @export
fractions <- function(vec, na_rm = TRUE) vec / sum(vec, na.rm = na_rm)


# _________________________________________________________________________________________________

#' @title flip_value2name
#' @description Flip the values and the names of a vector with names.
#' @param namedVector named vector
#' @param NumericNames PARAM_DESCRIPTION, Default: FALSE
#' @param silent Suppress printing info? Default: FALSE
#' @export
flip_value2name <- function(namedVector, NumericNames = FALSE, silent = FALSE) {
  if (!is.null(names(namedVector))) {
    newvec <- names(namedVector)
    if (NumericNames) {
      newvec <- as.numeric(names(namedVector))
    }
    names(newvec) <- namedVector
  } else {
    iprint("Vector without names!", head(namedVector))
  }
  if (!silent) {
    if (any(duplicated(namedVector))) {
      iprint("New names contain duplicated elements", head(namedVector[which(duplicated(namedVector))]))
    }
    if (any(duplicated(newvec))) {
      iprint("Old names contained duplicated elements", head(newvec[which(duplicated(newvec))]))
    }
  }
  return(newvec)
}


# _________________________________________________________________________________________________
#' @title sortbyitsnames
#'
#' @description Sort a vector or list by the alphanumeric order of its names (instead of its values).
#' @param vec_or_list A vector or list.
#' @param decreasing Logical. Whether to sort in decreasing order.
#' @param ... Additional arguments passed to `mixedsort()`.
#' @return A sorted vector or list.
#' @importFrom gtools mixedsort
#' @export
sortbyitsnames <- function(vec_or_list, decreasing = FALSE, ...) {
  xx <- names(vec_or_list)
  names(xx) <- 1:length(vec_or_list)
  order <- as.numeric(names(gtools::mixedsort(xx, decreasing = decreasing, ...)))
  vec_or_list[order]
}



# _________________________________________________________________________________________________
#' @title any.duplicated
#' @description How many entries are duplicated?.
#' @param vec input vector
#' @param summarize Print summary? Default: TRUE
#' @param max.shown Print first X elements. Default: 25
#' @export any.duplicated
any.duplicated <- function(vec, summarize = TRUE, max.shown = 25) {
  y <- sum(duplicated(vec))
  if (summarize & y) {
    x <- table(vec)
    x <- sort.decreasing(x[x > 1])

    print(paste("The following", y, "elements have  > 1 extra copies: (max", max.shown, "shown.)"))
    print(head(x, n = max.shown))
  }
  invisible(y > 0)
}



# _________________________________________________________________________________________________
#' @title which.duplicated
#' @description Which values are duplicated?.
#' @param vec input vector
#' @param verbose print statistics to terminal. Default: TRUE
#' @export
which.duplicated <- function(vec, verbose = TRUE) {
  DPL <- vec[which(duplicated(vec))]
  if (verbose) iprint(length(DPL), "Duplicated entries (1-5): ", head(DPL), "...")
  return(DPL)
}



# _________________________________________________________________________________________________
#' @title which.NA
#' @description Which values are NA?.
#' @param vec input vector. Default: TRUE
#' @param verbose print statistics to terminal. Default: TRUE
#' @export
which.NA <- function(vec, verbose = TRUE) {
  NANs <- vec[which(is.na(vec))]
  if (verbose) iprint(length(NANs), "NaN entries: ", NANs)
  NAs <- vec[which(is.na(vec))]
  if (verbose) iprint(length(NAs), "NA entries: ", NAs, "(only NA-s are returned)")
  return(NAs)
}



# _________________________________________________________________________________________________
#' @title clip.at.fixed.value
#' @description Signal clipping. Cut values in a distribution, above or below a threshold.
#' @param x A vector of numeric values (distribution).
#' @param high Clip above threshold? Default: TRUE
#' @param thr threshold values, Default: 3
#'
#' @export
clip.at.fixed.value <- function(x, high = TRUE, thr = 3) {
  if (high) {
    x[x > thr] <- thr
  } else {
    x[x < thr] <- thr
  }
  x
}


# _________________________________________________________________________________________________
#' @title clip.outliers.at.percentile
#' @description Signal clipping based on the input data's distribution. It clips values
#' in a distribution above or below the extreme N% of the distribution.
#'
#' @param x A vector of numeric values.
#' @param high Clip above threshold? Default: TRUE
#' @param percentiles At which percentiles to cut off?, Default: c(0.01, 0.99)
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @param showhist PARAM_DESCRIPTION, Default: FALSE
#' @param ... Pass any other argument.
#' @export
# #' @importFrom MarkdownReports whist

clip.outliers.at.percentile <- function(x, high = TRUE,
                                        percentiles = c(.01, .99),
                                        na.rm = TRUE, showhist = FALSE,
                                        ...) {
  qnt <- quantile(x, probs = percentiles, na.rm = na.rm)
  if (showhist) {
    hist(unlist(x),
         breaks = 50, main = "Distribution and cutoffs histogram",
         sub = paste("Percentile cutoffs at: ", paste(percentiles, collapse = " and ")),
         xlab = "Values"
    )
    abline(v = qnt, col = 2)
  }
  # if (showhist) { MarkdownReports::whist(unlist(x), breaks = 50 ,vline = qnt, filtercol = -1)} #if
  y <- x
  y[x < qnt[1]] <- qnt[1]
  y[x > qnt[2]] <- qnt[2]
  y
}



# _________________________________________________________________________________________________
#' @title as.logical.wNames
#' @description Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.
#' @param x input vector
#' @param ... Pass any other argument.
#' @export as.logical.wNames
as.logical.wNames <- function(x, ...) {
  numerified_vec <- as.logical(x, ...)
  if (!is.null(names(x))) {
    names(numerified_vec) <- names(x)
  }
  return(numerified_vec)
}


# _________________________________________________________________________________________________
#' @title col2named.vec.tbl
#' @description Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.
#' @param tbl.2col 2-column tibble
#' @export
col2named.vec.tbl <- function(tbl.2col) {
  nvec <- tbl.2col[[2]]
  names(nvec) <- tbl.2col[[1]]
  nvec
}



# _________________________________________________________________________________________________
#' @title topN.dfCol
#' @description Find the n highest values in a named vector.
#' @param df_col data frame column, Default: `as.named.vector(df[, 1, drop = FALSE])`
#' @param n top N values, Default: 5
#' @export
topN.dfCol <- function(df_col = as.named.vector(df[, 1, drop = FALSE]), n = 5) {
  head(sort(df_col, decreasing = TRUE), n = n)
} # Find the n highest values in a named vector


# _________________________________________________________________________________________________
#' @title bottomN.dfCol
#' @description Find the n lowest values in a named vector.
#' @param df_col data frame column, Default: `as.named.vector(df[, 1, drop = FALSE])`
#' @param n lowest N values, Default: 5
#' @export
bottomN.dfCol <- function(df_col = as.named.vector(df[, 1, drop = FALSE]), n = 5) {
  head(sort(df_col, decreasing = FALSE), n = n)
} # Find the n lowest values in a named vector



# _________________________________________________________________________________________________
#' @title Split a Vector into a List by Every N-th Element
#' @description This function divides a given vector into chunks of size `by` (default is 9).
#' The resulting list contains vectors of the specified chunk size or smaller.
#' @param vec A numeric or character vector to be split.
#' @param by Integer value specifying the chunk size. Default is 9.
#' @return A list where each element is a vector containing up to `by` elements from `vec`.
#' @export
split_vec_to_list_by_N <- function(vec = 1:27, by = 9) {
  n_groups <- ceiling(length(vec) / by)
  assignment <- gl(n_groups, by, length = length(vec))
  lsX <- split(x = vec, f = assignment)
  names(lsX) <- paste0("v", 1:length(lsX))
  lsX
}
# FORMERLY / aka: iterBy.over()
# lsX = split(vec, sort(rank(vec) %% steps))

# _________________________________________________________________________________________________
#' @title zigzagger
#' @description Mix entries so that they differ.
#' @param vec input vector, Default: 1:9
#' @export
zigzagger <- function(vec = 1:9) {
  intermingle2vec(vec, rev(vec))[1:length(vec)]
}


# _________________________________________________________________________________________________
#' @title Formats a Sequence of Numbers with Zero Padding
#'
#' @description This function generates a sequence of numbers between two specified values,
#' optionally padding them with leading zeros to a specified length. It is useful
#' for creating numeric sequences with consistent character lengths.
#' @param x The starting number of the sequence. Default: 1.
#' @param y The ending number of the sequence. Default: 100.
#' @param zeropadding Logical; whether to pad numbers with zeros. Default: TRUE.
#' @param pad_length The length of padding for the numbers. Default: Calculated as
#' floor(log10(max(abs(x), abs(y)))) + 1.
#' @return A vector of formatted numbers, with or without zero padding.
#' @seealso \code{\link[stringr]{str_pad}}
#' @export
#' @examples numerate(1, 122)
#' @importFrom stringr str_pad
numerate <- function(x = 1, y = 100, zeropadding = TRUE,
                     pad_length = floor(log10(max(abs(x), abs(y)))) + 1) {
  # Input argument assertions
  stopifnot(
    is.numeric(x), is.numeric(y),
    x <= y,
    is.logical(zeropadding), is.numeric(pad_length),
    pad_length > 0
  )

  # Create the sequence of numbers
  z <- x:y

  # Pad the numbers with zeros if required
  if (zeropadding) {
    z <- stringr::str_pad(z, pad = "0", width = pad_length)
  }

  return(z)
}




# _________________________________________________________________________________________________
#' @title MaxN
#' @description Find second (third…) highest/lowest value in vector.
#' @param vec input vector, Default: rpois(4, lambda = 3)
#' @param topN PARAM_DESCRIPTION, Default: 2
#' @export
MaxN <- function(vec = rpois(4, lambda = 3), topN = 2) {
  topN <- topN - 1
  n <- length(vec)
  sort(vec, partial = n - topN)[n - topN]
}
# https://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column



# _________________________________________________________________________________________________
#' @title cumsubtract
#' @description Cumulative subtraction, opposite of cumsum().
#' @param numericVec PARAM_DESCRIPTION, Default: blanks
#' @export
cumsubtract <- function(numericVec = blanks) {
  DiffZ <- numericVec[-1] - numericVec[-length(numericVec)]
  print(table(DiffZ))
  DiffZ
}



# _________________________________________________________________________________________________
#' @title sumBySameName
#' @description Sum up vector elements with the same name.
#' @param namedVec PARAM_DESCRIPTION
#' @export
sumBySameName <- function(namedVec) {
  # unlapply(splitbyitsnames(namedVec), sum)
  tapply(X = namedVec, INDEX = names(namedVec), sum)
}


# _________________________________________________________________________________________________
#' @title Check Minimum Overlap Between Two Vectors
#'
#' @description Checks if the overlap between two character vectors is at least a specified
#' percentage of the shorter vector. Stops execution with an error if the condition is not met.
#'
#' @param x First character vector.
#' @param y Second character vector.
#' @param min_overlap Minimum required overlap as a fraction of the shorter vector's length.
#' @param stop_it Use stop(), else warning()
#' @param verbose verbose
#' @return Invisible TRUE if the overlap is sufficient, otherwise stops with an error.
#' @export
checkMinOverlap <- function(x, y, min_overlap = 0.2, stop_it = TRUE, verbose = TRUE) {
  stopifnot(is.character(x), is.character(y), is.numeric(min_overlap), min_overlap >= 0)

  namez <- c(substitute(x), substitute(y))
  lengths <- c(length(x), length(y))

  min_len <- min(lengths)
  max_len <- max(lengths)

  overlap_len <- length(intersect(x, y))

  required_overlap <- min_len * min_overlap

  if (verbose) {
    iprint("Overlap is", overlap_len)
    iprint(percentage_formatter(overlap_len / min_len), "or", min_len, "of", namez[which.min(lengths)])
    iprint(percentage_formatter(overlap_len / max_len), "or", max_len, "of", namez[which.max(lengths)])
  }

  pass <- overlap_len > required_overlap
  if (!pass) {
    iprint(substitute(x), "-", head(x))
    iprint(substitute(y), "-", head(y))

    msg <- "Minimum overlap condition not met."
    if (stop_it) stop(msg) else warning(msg, immediate. = TRUE)
  }
  invisible(pass)
}



# _________________________________________________________________________________________________
### Vector filtering ____________________________________________________________ ----

#' @title which_names
#' @description Return the names where the input vector is TRUE. The input vector is converted to logical.
#' @param namedVec PARAM_DESCRIPTION
#' @export
which_names <- function(namedVec) {
  return(names(which(as.logical.wNames(namedVec))))
}



# _________________________________________________________________________________________________
#' @title which_names_grep
#'
#' @description Return the vector elements whose names partially match a pattern.
#' @param namedVec A vector of named elements.
#' @param pattern A regular expression pattern.
#' @param ... Pass any other argument to grepv()
#' @export
which_names_grep <- function(namedVec, pattern, ...) {
  idx <- grepv(x = names(namedVec), pattern = pattern, ...)
  return(namedVec[idx])
}



# _________________________________________________________________________________________________
#' @title na.omit.strip
#'
#' @description Calls na.omit() and returns a clean vector.
#' Omit NA values from a vector and return a clean vector without any spam.
#' @param object Values to filter for NA
#' @param silent Silence the data structure coversion warning: anything ->vector
#' @param ... Pass any other argument to na.omit()
#' @importFrom stats na.omit
#'
#' @export na.omit.strip
#'
#' @examples # CodeAndRoll2::na.omit.strip(c(1, 2, 3, NA, NaN, 2))
na.omit.strip <- function(object, silent = FALSE, ...) {
  if (is.data.frame(object)) {
    if (min(dim(object)) > 1 & silent == FALSE) {
      iprint(dim(object), "dimensional array is converted to a vector.")
    }
    object <- unlist(object)
  }
  clean <- stats::na.omit(object, ...)
  attributes(clean)$na.action <- NULL
  return(clean)
}



# _________________________________________________________________________________________________
#' @title inf.omit
#' @description Omit infinite values from a vector.
#' @param vec input vector
#'
#' @export
inf.omit <- function(vec) {
  if (is.data.frame(vec)) {
    if (min(dim(vec)) > 1) {
      iprint(dim(vec), "dimensional array is converted to a vector.")
    }
    vec <- unlist(vec)
  }
  clean <- vec[is.finite(vec)]
  # attributes(clean)$na.action <- NULL
  return(clean)
}



# _________________________________________________________________________________________________
#' @title zero.omit
#' @description Omit zero values from a vector.
#' @param vec input vector
#' @param verbose Whether to print the range of the cleaned vector.
#' @export
zero.omit <- function(vec, verbose = TRUE) {
  v2 <- vec[vec != 0]
  if (verbose) iprint("range: ", range(v2))
  if (!is.null(names(vec))) {
    names(v2) <- names(vec)[vec != 0]
  }
  return(v2)
}


# _________________________________________________________________________________________________
#' @title pc_TRUE
#'
#' @description  Calculates the percentage of true values in a logical vector, parsed as text.
#' @param logical_vector A logical vector.
#' @param percentify Whether to return the percentage as a formatted string (default: TRUE).
#' @param NumberAndPC Whether to return the percentage and the number of true values (default: FALSE).
#' @param NArm Whether to ignore NA values (default: TRUE).
#' @param prefix A prefix to add to the output string (default: NULL).
#' @param suffix A suffix to add to the output string (default: NULL).
#' @param digitz The number of decimal places to use when formatting the percentage (default: 3).
#' @param ... Additional arguments to pass to `percentage_formatter()`.
#'
#' @return A string representing the percentage of true values in the logical vector.
#'
#' @export
pc_TRUE <- function(
    logical_vector, percentify = TRUE, NumberAndPC = FALSE,
    NArm = TRUE, prefix = NULL, suffix = NULL, digitz = 3, ...) {
  # Calculate the percentage of true values
  SUM <- sum(logical_vector, na.rm = NArm)
  LEN <- length(logical_vector)
  out <- SUM / LEN

  # Format the percentage as a string
  if (percentify) out <- percentage_formatter(out, digitz = digitz, ...)

  # Add the number of true values if requested
  if (NumberAndPC) out <- paste0(out, " or ", SUM, " of ", LEN)

  # Add the prefix and suffix
  if (!is.null(prefix)) out <- paste(prefix, out)
  if (!is.null(suffix)) out <- paste(out, suffix)

  return(out)
}



# _________________________________________________________________________________________________
#' @title Calculate Percentage Overlap Between Two Vectors
#'
#' @description Computes the percentage of overlap between two vectors based on the specified basis of calculation.
#'
#' @param x The first vector for overlap calculation.
#' @param y The second vector for overlap calculation.
#' @param basis A character string specifying the basis for calculating the percentage overlap.
#' Can be "x" for the length of `x`, "y" for the length of `y`, or "sum" for the sum
#' of lengths of both `x` and `y`. Default is "x".
#' @param prefix A prefix to add to the output string (default: NULL).
#' @param suffix A suffix to add to the output string (default: NULL).
#' @param ... Additional arguments to pass to `percentage_formatter()`.
#'
#' @return The percentage of overlap between `x` and `y` based on the specified basis.
#' @examples x <- 1:5
#' y <- 3:8
#' pc_overlap(x, y, basis = "x")
#'
#' @export

pc_overlap <- function(x, y, basis = "x", prefix = NULL, suffix = NULL, ...) {
  # Assertions to ensure input validity
  stopifnot(
    is.character(basis),
    basis %in% c("x", "y", "sum"),
    is.vector(x),
    is.vector(y)
  )

  # Calculate intersection
  overlap <- length(intersect(x, y))

  # Calculate denominator based on the basis argument
  denominator <- switch(basis,
                        "x" = length(x),
                        "y" = length(y),
                        "sum" = length(x) + length(y)
  )

  # Calculate and return percent overlap
  percent_overlap <- percentage_formatter(x = overlap / denominator, ...)
  if (is.null(suffix)) suffix <- kppws("of", substitute(basis), "is found in both vectors.")
  text <- kppws(prefix, percent_overlap, suffix)
  message(text)

  return(overlap / denominator)
}




# _________________________________________________________________________________________________
#' @title pc_in_total_of_match
#'
#' @description Calculates the percentage of a certain value within a vector or table.
#' @param vec_or_table A vector or table.
#' @param category The value to calculate the percentage for.
#' @param NA_omit Logical. Whether to omit missing values from the calculation.
#' @return The percentage of `category` in `vec_or_table`.
#'
#' @export
#' @importFrom stats na.omit
pc_in_total_of_match <- function(vec_or_table, category, NA_omit = TRUE) {
  if (is.table(vec_or_table)) {
    vec_or_table[category] / sum(vec_or_table, na.rm = NA_omit)
  } else {
    # Check if `NA_omit` is TRUE and there are missing values
    if (NA_omit) {
      if (sum(is.na(vec_or_table))) {
        vec_or_table <- stats::na.omit(vec_or_table)
        iprint(sum(is.na(vec_or_table)), "NA are omitted from the vec_or_table of:", length(vec_or_table))
      }
      "Not working complelety : if NaN is stored as string, it does not detect it"
    }

    # Calculate the percentage
    sum(vec_or_table == category) / length(vec_or_table)
  } # else: is vector
} # fun


# _________________________________________________________________________________________________


# _________________________________________________________________________________________________
#' @title remove_outliers
#'
#' @description Remove values that fall outside the trailing `probs` percentiles of the distribution.
#' @param x A numeric vector.
#' @param na.rm Remove NA values for calculation? Default: TRUE.
#' @param probs A vector of two probabilities, specifying the trailing percentiles to use.
#' @param ... Additional arguments passed to `quantile()`.
#' @return A vector with the outliers removed.
#'
#' @export
remove_outliers <- function(x, na.rm = TRUE, probs = c(.05, .95), ...) {
  print("Deprecated. Use clip.outliers.at.percentile()")
  qnt <- quantile(x, probs = probs, na.rm = na.rm, ...)
  y <- x
  y[x < qnt[1]] <- NA ## Add IQR dependence
  y[x > qnt[2]] <- NA
  y
}


# _________________________________________________________________________________________________
#' @title simplify_categories
#'
#' @description Replace all occurrences of `replaceit` in `category_vec` with `to`.
#' @param category_vec A vector of categories.
#' @param replaceit The value to replace.
#' @param to The replacement value.
#' @return A vector with the simplified categories.
#'
#' @export
simplify_categories <- function(category_vec, replaceit, to) {
  matches <- which(category_vec %in% replaceit)
  iprint(length(matches), "instances of", replaceit, "are replaced by", to)
  category_vec[matches] <- to
  return(category_vec)
}


# _________________________________________________________________________________________________
## Matrix operations ____________________________________________________________ ----


# _________________________________________________________________________________________________
### Matrix calculations ____________________________________________________________ ----



# _________________________________________________________________________________________________
#' @title colSubtract
#' @description Subtract a vector (length = nr. columns) column by column from each value of the matrix.
#' @param mat Numeric input matrix.
#' @param vec Vector to subtract. Length = nr. columns.
#' @export
colSubtract <- function(mat = xx, vec = 5:1) {
  stopifnot(NCOL(mat) == length(vec))
  t(apply(mat, 1, function(x) x - vec))
}


# _________________________________________________________________________________________________
#' @title rowSubtract
#' @description Subtract a vector (length = nr. rows) row by row from each value of the matrix
#' @param mat Numeric input matrix.
#' @param vec Vector to subtract. Length = nr. rows.
#' @export
rowSubtract <- function(mat = yy, vec = 5:1) {
  stopifnot(NROW(mat) == length(vec))
  apply(mat, 2, function(x) x - vec)
}



# _________________________________________________________________________________________________
#' @title Row-wise division of a matrix by a column vector
#'
#' @description Each element of the matrix is divided by the corresponding element of the vector
#' that matches the column of the matrix element. This is typically used to normalize data,
#' for example, to scale values in each row by certain factors like totals or means. Soruce
#' \url{https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r}.
#'
#' @param mat A numeric matrix where each row represents a distribution to be divided.
#' @param vec A numeric vector whose elements are the divisors for each column of the matrix.
#' The length of the vector must match the number of columns in the matrix. If not supplied,
#' the default is to use the column sums of the matrix as divisors.
#'
#' @return A matrix with the same dimensions as the input where each element in the original matrix
#' has been divided by the corresponding element in the vector.
#'
#' @export
colDivide <- function(mat, vec = colSums(mat)) {
  stopifnot(ncol(mat) == length(vec), is.numeric(vec))
  mat / vec[col(mat)] # This operation divides each column element-wise by the vector
}


# _________________________________________________________________________________________________
#' @title colMutliply
#' @description Multiply by column. See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r.
#' @param mat Numeric input matrix with the distribution.
#' @param vec Vector to multiply by.
#' @export
colMutliply <- function(mat, vec) {
  stopifnot(NCOL(mat) == length(vec))
  mat * vec[col(mat)] # fastest
}



# _________________________________________________________________________________________________
#' @title rowDivide
#' @description Divide by row.
#' @param mat Numeric input matrix with the distribution.
#' @param vec Vector to divide by.
#' @export
rowDivide <- function(mat, vec) {
  stopifnot(NROW(mat) == length(vec))
  mat / vec[row(mat)] # fastest
}



# _________________________________________________________________________________________________
#' @title rowMutliply
#' @description Mutliply by row.
#' @param mat Numeric input matrix with the distribution.
#' @param vec Vector to multiply by.
#' @export
rowMutliply <- function(mat, vec) {
  stopifnot(NROW(mat) == length(vec))
  mat * vec[row(mat)] # fastest
}



# _________________________________________________________________________________________________
#' @title row.Zscore
#' @description Calculate Z-score over rows of data frame.
#' @param x Numeric input matrix with the distribution.
#' @export
row.Zscore <- function(x) t(scale(t(x)))


# _________________________________________________________________________________________________
#' @title TPM_normalize
#' @description Normalize each column to 1 million.
#' @param mat Numeric input matrix with the distribution.
#' @param SUM PARAM_DESCRIPTION, Default: 1e+06
#' @export
TPM_normalize <- function(mat, SUM = 1e6) {
  cs <- colSums(mat, na.rm = TRUE)
  norm_mat <- (t(t(mat) / cs)) * SUM
  return(norm_mat)
}



# _________________________________________________________________________________________________
#' @title median_normalize
#' @description Normalize each column to the median of all the column-sums.
#' @param mat Numeric input matrix with the distribution.
#' @export
median_normalize <- function(mat) {
  cs <- colSums(mat, na.rm = TRUE)
  norm_mat <- (t(t(mat) / cs)) * median(cs)
  iprint("colMedians: ", head(signif(colMedians(norm_mat), digits = 3)))
  return(norm_mat)
}



# _________________________________________________________________________________________________
#' @title mean_normalize
#' @description Normalize each column to the median of the columns.
#' @param mat Numeric input matrix.
#' @export
mean_normalize <- function(mat) {
  cs <- colSums(mat, na.rm = TRUE)
  norm_mat <- (t(t(mat) / cs)) * mean(cs)
  iprint("colMeans: ", head(signif(colMeans(norm_mat))))
  return(norm_mat)
}



# _________________________________________________________________________________________________
### Matrix stats basic ____________________________________________________________ ----


# _________________________________________________________________________________________________
#' @title rowMin
#' @description Calculates the minimum of each row of a numeric matrix / data frame.
#' @param x Input matrix, or all-numeric dataframe.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
rowMin <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 1, min, na.rm = na.rm)
}


# _________________________________________________________________________________________________
#' @title colMin
#' @description Calculates the minimum of each column of a numeric matrix / data frame.
#' @param x Input matrix, or all-numeric dataframe.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
colMin <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 2, min, na.rm = na.rm)
}



# _________________________________________________________________________________________________
#' @title rowMax
#' @description Calculates the maximum of each row of a numeric matrix / data frame.
#' @param x Numeric input matrix with the distribution.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
rowMax <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 1, max, na.rm = na.rm)
}


# _________________________________________________________________________________________________
#' @title colMax
#' @description Calculates the maximum of each column of a numeric matrix / data frame.
#' @param x Numeric input matrix with the distribution.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
colMax <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 2, max, na.rm = na.rm)
}



# _________________________________________________________________________________________________
### Matrix stats ____________________________________________________________ ----


#' @title rowMedians
#' @description Calculates the median of each row of a numeric matrix / data frame.
#' @param x Input matrix, or all-numeric dataframe.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
rowMedians <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 1, median, na.rm = na.rm)
}


# _________________________________________________________________________________________________
#' @title colMedians
#' @description Calculates the median of each column of a numeric matrix / data frame.
#' @param x Input matrix, or all-numeric dataframe.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
colMedians <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 2, median, na.rm = na.rm)
}



# _________________________________________________________________________________________________
#' @title rowGeoMeans
#' @description Calculates the median of each row of a numeric matrix / data frame.
#' @param x Input matrix, or all-numeric dataframe.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
rowGeoMeans <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 1, geomean, na.rm = na.rm)
}


# _________________________________________________________________________________________________
#' @title colGeoMeans
#' @description Calculates the median of each column of a numeric matrix / data frame.
#' @param x Input matrix, or all-numeric dataframe.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
colGeoMeans <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 2, geomean, na.rm = na.rm)
}



# _________________________________________________________________________________________________
#' @title rowCV
#' @description Calculates the CV of each ROW of a numeric matrix / data frame.
#' @param x Input matrix, or all-numeric dataframe.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
rowCV <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 1, cv, na.rm = na.rm)
}


# _________________________________________________________________________________________________
#' @title colCV
#' @description Calculates the CV of each column of a numeric matrix / data frame.
#' @param x Input matrix, or all-numeric dataframe.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
colCV <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 2, cv, na.rm = na.rm)
}



# _________________________________________________________________________________________________
#' @title rowVariance
#' @description Calculates the CV of each ROW of a numeric matrix / data frame.
#' @param x Input matrix, or all-numeric dataframe.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
rowVariance <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 1, var, na.rm = na.rm)
}


# _________________________________________________________________________________________________
#' @title colVariance
#' @description Calculates the CV of each column of a numeric matrix / data frame.
#' @param x Input matrix, or all-numeric dataframe.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
colVariance <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 2, var, na.rm = na.rm)
}




# _________________________________________________________________________________________________
#' @title rowSEM
#' @description Calculates the SEM of each row of a numeric matrix / data frame.
#' @param x Numeric input matrix with the distribution.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
rowSEM <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 1, sem, na.rm = na.rm)
}


# _________________________________________________________________________________________________
#' @title colSEM
#' @description Calculates the SEM of each column of a numeric matrix / data frame.
#' @param x Numeric input matrix with the distribution.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
colSEM <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 2, sem, na.rm = na.rm)
}



# _________________________________________________________________________________________________
#' @title rowSD
#' @description Calculates the SEM of each row of a numeric matrix / data frame.
#' @param x Numeric input matrix with the distribution.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
rowSD <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 1, sd, na.rm = na.rm)
}


# _________________________________________________________________________________________________
#' @title colSD
#' @description Calculates the SD of each column of a numeric matrix / data frame.
#' @param x Numeric input matrix with the distribution.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
colSD <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 2, sd, na.rm = na.rm)
}



# _________________________________________________________________________________________________
#' @title rowIQR
#' @description Calculates the IQR of each row of a numeric matrix / data frame.
#' @param x Numeric input matrix with the distribution.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
rowIQR <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 1, IQR, na.rm = na.rm)
}


# _________________________________________________________________________________________________
#' @title colIQR
#' @description Calculates the IQR of each column of a numeric matrix / data frame.
#' @param x Numeric input matrix with the distribution.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @export
colIQR <- function(x, na.rm = TRUE) {
  apply(data.matrix(x), 2, IQR, na.rm = na.rm)
}



# _________________________________________________________________________________________________
#' @title rowQuantile
#' @description Calculates the quantile of each row of a numeric matrix / data frame.
#' @param x Numeric input matrix with the distribution.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @param ... Pass any other argument.
#' @export
rowQuantile <- function(x, na.rm = TRUE, ...) {
  apply(data.matrix(x), 1, quantile, ..., na.rm = na.rm)
}


# _________________________________________________________________________________________________
#' @title colQuantile
#' @description Calculates the quantile of each column of a numeric matrix / data frame.
#' @param x Numeric input matrix with the distribution.
#' @param na.rm Remove NA values for calculation? Default: TRUE
#' @param ... Pass any other argument.
#' @export
colQuantile <- function(x, na.rm = TRUE, ...) {
  apply(data.matrix(x), 2, quantile, ..., na.rm = na.rm)
}

# _________________________________________________________________________________________________
#' @title Bind two named vectors by matching names
#' @description Combines two named vectors into a data frame by matching their names.
#' Missing values are filled with NA.
#' @param vec1 First named vector.
#' @param vec2 Second named vector.
#' @return A data frame with columns for both vectors, matched by name.
#' @export

cbind_vectors_by_names <- function(vec1, vec2) {
  all_names <- union(names(vec1), names(vec2))
  df <- data.frame(vec1 = vec1[all_names], vec2 = vec2[all_names])
  colnames(df) <- c(substitute(vec1), substitute(vec2))
  df
}


# _________________________________________________________________________________________________
## Matrix manipulations ____________________________________________________________ ----



# _________________________________________________________________________________________________
#' @title sortEachColumn
#' @description Sort each column of a numeric matrix / data frame.
#' @param data Numeric input matrix.
#' @param ... Pass any other argument.
#' @export
sortEachColumn <- function(data, ...) sapply(data, sort, ...) # Sort each column of a numeric matrix / data frame.



# _________________________________________________________________________________________________
#' @title Sort matrix or data frame by a column or row names
#'
#' @description
#' Sorts a numeric matrix or data frame by a specified column or by row names. The function can only
#' handle sorting by a single column. It offers options to sort in increasing or decreasing order,
#' and to control the placement of `NA` values.
#'
#' @param df A numeric matrix or data frame to be sorted. Default: none.
#' @param column A column name or index by which to sort. The function can handle only single-column
#'   sorting. Default: none.
#' @param rownames Logical. If `TRUE`, row names will be used for sorting instead of a column.
#'   Default: `FALSE`.
#' @param decrease Logical. If `TRUE`, the data will be sorted in decreasing order. Default: `FALSE`.
#' @param na_last Logical. If `TRUE`, `NA` values will be placed at the end. Default: `TRUE`.
#'
#' @examples
#' df <- data.frame(AA = c(1, 2, 3), BB = c(3, 2, 1), row.names = letters[7:5])
#' sort_matrix_rows(df, "AA")
#' sort_matrix_rows(df, "AA", decrease = TRUE)
#' sort_matrix_rows(df, "A", rownames = TRUE)
#'
#' @return A sorted version of the input matrix or data frame.
#'
#' @export
sort_matrix_rows <- function(df, column = NULL, rownames = FALSE, decrease = FALSE, na_last = TRUE) {
  stopifnot(
    is.data.frame(df) | is.matrix(df),
    is.character(column) | is.numeric(column) | if (rownames) is.null(column),
    "cannot handle multi column sort" = length(column) == 1 | if (rownames) is.null(column),
    is.logical(rownames), is.logical(decrease), is.logical(na_last),
    (if (isFALSE(rownames) & is.character(column)) column %in% colnames(df) else TRUE)
  )

  message("Sorting by ", if (rownames) "rownames" else paste(column, "column"), " in ", if (decrease) "Decreasing" else "Increasing", " order.")

  ordering_vakues <- if (rownames) rownames(df) else df[, column]
  sorted_order <- order(rownames(df), decreasing = decrease, na.last = na_last)

  df[sorted_order, ]
}





# _________________________________________________________________________________________________
#' @title rownames.trimws
#' @description Trim whitespaces from the rownames.
#' @param matrix1 Input matrix.
#' @export
rownames.trimws <- function(matrix1) {
  rownames(matrix1) <- trimws(rownames(matrix1))
  return(matrix1)
}



# _________________________________________________________________________________________________
#' @title colsplit
#' @description Split a data frame by a factor corresponding to columns.
#' @param df Input data frame.
#' @param f Splitting factor, Default: colnames(df)
#' @export
colsplit <- function(df, f = colnames(df)) {
  ListOfDFs <- NULL
  levelz <- unique(f)
  for (i in 1:length(levelz)) {
    ListOfDFs[[i]] <- df[, which(f == levelz[i])]
  }
  names(ListOfDFs) <- levelz
  return(ListOfDFs)
}



# _________________________________________________________________________________________________
#' @title rowsplit
#' @description Split a data frame by a factor corresponding to columns.
#' @param df Input data frame.
#' @param f Splitting factor, Default: rownames(df)
#' @export
rowsplit <- function(df, f = rownames(df)) {
  ListOfDFs <- NULL
  levelz <- unique(f)
  for (i in 1:length(levelz)) {
    ListOfDFs[[i]] <- df[which(f == levelz[i]), ]
  }
  names(ListOfDFs) <- levelz
  return(ListOfDFs)
}

# _________________________________________________________________________________________________
#' @title Get the Column Name corresponding to the Maximum Value in each Row (handles ambiguous matches)
#'
#' @description
#' This function takes a numeric matrix as input and returns a named vector where each element
#' corresponds to a row of the matrix. The names of the vector are the row names of the matrix,
#' and the values are the column names where the maximum value of each row is found. If there are
#' multiple columns with the maximum value in a row, the value for that row will be set to
#' `multi_max_str`. If `na.remove` is set to `TRUE`, NA values will be removed before finding the
#' maximum value.
#'
#' @param mat A numeric matrix
#' @param na.remove Logical. Should NA values be removed before finding the maximum value?
#' Default: TRUE
#' @param collapse Character. The character to use to collapse multiple column names into a single
#' string. Default: "-"
#' @param verbose Logical. Should messages be printed to the console? Default: TRUE
#' @param multi_max_str Character. The string to use when multiple columns have the maximum value.
#' Default: "multiple.maxima"
#' @param suffix Character. The suffix to add to the `multi_max_str` string. Default: "rows have
#'
#'
#' @examples
#' mat <- matrix(data = c(1, 2, 3, 9, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
#' colnames(mat) <- c("UVI1", "UVI2", "UVI3")
#' rownames(mat) <- c("Cell1", "Cell2")
#' mat
#' get_max_colname_per_row(mat)
#' mat[5] <- NA
#' mat[2] <- NaN
#' mat[1] <- 2
#' mat
#' get_max_colname_per_row(mat)
#'
#' @export

get_max_colname_per_row <- function(
    mat, na.remove = TRUE, collapse = "-", verbose = TRUE,
    multi_max_str = "multiple.maxima",
    suffix = "rows have multiple maxima.") {
  # Remove NA values if specified
  if (na.remove) mat[is.na(mat)] <- -Inf

  # Function to find the maximum indices (1 or more ) of values in a vector
  which.max.multi <- function(x) which(x == max(x, na.rm = TRUE))

  # Apply function to find the maximum indices to each row and return appropriate result
  max_colname_per_row <- apply(mat, 1, function(row) {
    # One or more maximum values
    max_indices <- which.max.multi(row)

    # If there are multiple maximum values, return the "multi_max_str"
    if (length(max_indices) > 1) {
      return(multi_max_str)
    }

    return(colnames(mat)[max_indices])
  })

  # Name the result with row names (cell names)
  names(max_colname_per_row) <- rownames(mat)

  # stats
  if (verbose) {
    message(paste(sum(max_colname_per_row == multi_max_str), "of", length(max_colname_per_row), suffix))
  }

  return(max_colname_per_row)
}




# _________________________________________________________________________________________________
#' @title select_rows_and_columns
#' @description Subset rows and columns. It checks if the selected dimension names exist and reports if any of those they aren't found.
#' @param df Input data frame.
#' @param RowIDs RowIDs, Default: NULL
#' @param ColIDs ColIDs, Default: NULL
#' @export
select_rows_and_columns <- function(df, RowIDs = NULL, ColIDs = NULL) {
  if (length(RowIDs)) {
    true_rownames <- intersect(rownames(df), RowIDs)
    NotFound <- setdiff(RowIDs, rownames(df))
    if (length(NotFound)) {
      iprint(length(NotFound), "Row IDs Not Found:", head(NotFound), "...     Rows found:", length(true_rownames))
    } else {
      iprint("All row IDs found")
    } # if
    df <- df[true_rownames, ]
  } # if
  if (length(ColIDs)) {
    true_colnames <- intersect(colnames(df), ColIDs)
    NotFound <- setdiff(ColIDs, colnames(df))
    if (length(NotFound)) {
      iprint(length(NotFound), "Column IDs Not Found:", head(NotFound), "...     Rows found:", length(true_colnames))
    } else {
      iprint("All column IDs found")
    }
    df <- df[, true_colnames]
  } # if
  iprint(dim(df))
  return(df)
}



# _________________________________________________________________________________________________
#' @title getRows
#'
#' @description Returns a subset of rows based on their names and optionally removes rows with only NA or zero values. Reports the number of missing rows.
#' @param mat Input matrix.
#' @param rownamez Vector of row names to search for in the matrix.
#' @param silent Logical indicating whether to suppress printing of missing rows. Default: FALSE
#' @param removeNAonly Logical indicating whether to remove rows with only NA values. Default: FALSE
#' @param remove0only Logical indicating whether to remove rows with only zero values. Default: FALSE
#' @return A matrix that is a subset of the input matrix.
#' @export
getRows <- function(mat, rownamez, silent = FALSE, removeNAonly = FALSE, remove0only = FALSE) {
  idx <- intersect(rownamez, row.names(mat))
  if (removeNAonly) {
    idx <- which_names(rowSums(!is.na(mat[idx, ]), na.rm = TRUE) > 0)
  }
  if (remove0only) {
    idx <- which_names(rowSums(mx != 0, na.rm = TRUE) > 0)
  }
  if (!silent) {
    iprint(length(idx), "/", length(rownamez), "are found. Missing: ", length(setdiff(row.names(mat), rownamez)))
  }
  mat[idx, ]
}



# _________________________________________________________________________________________________
#' @title getCols
#'
#' @description Returns a subset of columns based on their names and optionally removes columns with only NA or zero values. Reports the number of missing columns.
#' @param mat Input matrix.
#' @param colnamez Vector of column names to search for in the matrix.
#' @param silent Logical indicating whether to suppress printing of missing columns. Default: FALSE
#' @param removeNAonly Logical indicating whether to remove columns with only NA values. Default: FALSE
#' @param remove0only Logical indicating whether to remove columns with only zero values. Default: FALSE
#' @return A matrix that is a subset of the input matrix.
#' @export
getCols <- function(mat, colnamez, silent = FALSE, removeNAonly = FALSE, remove0only = FALSE) {
  idx <- intersect(colnamez, colnames(mat))
  print(symdiff(colnamez, colnames(mat)))
  if (removeNAonly) {
    idx <- which_names(colSums(!is.na(mat[, idx]), na.rm = TRUE) > 0)
  }
  if (remove0only) {
    idx <- which_names(colSums(mx != 0, na.rm = TRUE) > 0)
  }
  if (!silent) {
    iprint(length(idx), "/", length(colnamez), "are found. Missing: ", length(setdiff(colnames(mat), colnamez)))
  }
  mat[, idx]
}



# _________________________________________________________________________________________________
#' @title get.oddoreven
#'
#' @description Returns either odd or even indexed rows or columns from a data frame.
#' @param df_ A data frame. Default: NULL
#' @param rows Logical indicating whether to return rows (if TRUE) or columns (if FALSE). Default: FALSE
#' @param odd Logical indicating whether to return odd indexed rows/columns (if TRUE) or even indexed rows/columns (if FALSE). Default: TRUE
#' @return A subset of the input data frame.
#' @export
get.oddoreven <- function(df_ = NULL, rows = FALSE, odd = TRUE) {
  counter <- if (rows) NROW(df_) else NCOL(df_)
  IDX <- if (odd) seq(1, to = counter, by = 2) else seq(2, to = counter, by = 2)
  df_out <- if (rows) df_[IDX, ] else df_[, IDX]
  return(df_out)
}



# _________________________________________________________________________________________________
#' @title merge_dfs_by_rn
#' @description Merge any data frames by rownames. Required plyr package.
#' @param list_of_dfs PARAM_DESCRIPTION
#' @seealso
#'  \code{\link[plyr]{join_all}}
#' @export
#' @importFrom plyr join_all
merge_dfs_by_rn <- function(list_of_dfs) {
  if (length(names(list_of_dfs)) != length(list_of_dfs)) {
    names(list_of_dfs) <- 1:length(list_of_dfs)
  }

  for (i in names(list_of_dfs)) {
    colnames(list_of_dfs[[i]]) <- paste0(i, ".", colnames(list_of_dfs[[i]]))
  } # make unique column names
  for (i in names(list_of_dfs)) {
    list_of_dfs[[i]]$rn <- rownames(list_of_dfs[[i]])
  } # for
  COMBINED <- plyr::join_all(list_of_dfs, by = "rn", type = "full")
  idim(COMBINED)
  rownames(COMBINED) <- COMBINED$rn
  COMBINED$rn <- NULL
  return(COMBINED)
}

# _________________________________________________________________________________________________
#' @title merge_1col_dfs_by_rn
#' @description A function that merges single-column data frames based on rownames.
#' @param list_of_dfs  list of 1col dfs
#' @param FILLwith 0 by def
#' @param columnUSE column index in both. 1 by default.
#' @export
#' @examples merge_1col_dfs_by_rn()
merge_1col_dfs_by_rn <- function(list_of_dfs, FILLwith = 0, columnUSE = 1) {
  all.rn <- sort(union.ls(lapply(list_of_dfs, rownames)))
  iprint("n rownames:", length(all.rn))
  df_new <- data.frame(matrix(data = FILLwith, nrow = length(all.rn), ncol = length(list_of_dfs)), row.names = all.rn)
  colnames(df_new) <- names(list_of_dfs)
  for (i in 1:length(list_of_dfs)) {
    print(i)
    indf <- list_of_dfs[[i]]
    df_new[rownames(indf), i] <- indf[, columnUSE]
  }
  df_new
}



# _________________________________________________________________________________________________
#' @title merge_numeric_df_by_rn
#' @description Merge 2 numeric data frames by rownames.
#' @param x Input matrix, or all-numeric dataframe.
#' @param y Input matrix, or all-numeric dataframe.
#' @export
merge_numeric_df_by_rn <- function(x, y) {
  rn1 <- rownames(x)
  rn2 <- rownames(y)
  diffz <- symdiff(rn1, rn2)
  merged <- merge(x, y, by = "row.names", all = TRUE) # merge by row names(by = 0 or by = "row.names")
  rownames(merged) <- merged$Row.names
  merged <- merged[, -1] # remove row names
  merged[is.na(merged)] <- 0

  print("Uniq Rows (top 10 by sum)")
  x1 <- rowSums(x[diffz[[1]], ])
  x2 <- rowSums(y[diffz[[2]], ])
  print("")
  iprint("Values specific to 1: ", round(sum(x1)), "or", percentage_formatter(sum(x1) / sum(merged)))
  print(tail(sort(x1), n = 10))
  print("")
  iprint("Values specific to 2: ", round(sum(x2)), "or", percentage_formatter(sum(x2) / sum(merged)))
  print(tail(sort(x2), n = 10))
  iprint("Dimensions of merged DF:", dim(merged))

  return(merged)
}


# _________________________________________________________________________________________________
#' @title merge_2_named_vec_as_df
#' @description Merge two named vectors by names, into a dataframe with 2 columns.
#' @param x A vector with named elements.
#' @param y Another vector with named elements.
#' @examples # a <- 1:5; names(a) <- letters[a]; b <- 9:3; names(b) <- letters[b]; merge_2_named_vec_as_df(a,b)
#' @export

merge_2_named_vec_as_df <- function(x, y) {
  COMBINED <-
    full_join(x = stack(x), y = stack(y), by = "ind")[, c(2, 1, 3)] %>%
    FirstCol2RowNames.as.df()

  colnames(COMBINED) <- c(substitute(x), substitute(y))
  return(COMBINED)
}


# _________________________________________________________________________________________________
#' @title merge_ls_of_named_vec_as_df_cols
#' @description Merge any number of named vectors (presented as a list) by names, into a dataframe
#' @param named_list A named list of named vectors.
#' @param missing_values How to fill missing values
#' @examples # merge_ls_of_named_vec_as_df_cols()
#' @export

merge_ls_of_named_vec_as_df_cols <- function(
    named_list = list(
      vec1 = c(A = 1, B = 2, C = 3),
      vec2 = c(B = 4, D = 5),
      vec3 = c(A = 6, C = 7, D = 8),
      vec4 = c(B = 9, C = 10, D = 11, E = 12)
    ),
    missing_values = NaN) {
  stopifnot(length(names(named_list)) == length(named_list)) # stop if names are missing
  stopifnot(all(unlapply(lapply(named_list, names), length) > 0)) # stop if there are empty vectors

  # Merge any data frames by rownames. Required plyr package
  ls.indexed.dfs <- lapply(named_list, stack)
  suppressWarnings(COMBINED <- Reduce(function(x, y) merge(x, y, by = "ind", all = TRUE, ), ls.indexed.dfs))

  colnames(COMBINED)[-1] <- names(named_list)
  COMBINED[is.na(COMBINED)] <- missing_values

  return(FirstCol2RowNames(COMBINED))
}


# _________________________________________________________________________________________________
#' @title Extract and Display Column Types of a Data Frame or Tibble
#'
#' @description This function returns the primary class/type of each column in a data frame or tibble.
#' Additionally, it can print a summary of the column types.
#'
#' @param df A data frame or tibble whose column types are to be extracted.
#' @param print_it Logical; if `TRUE` (default), prints a table of column types and a summary.
#' @return A named character vector where names are column names and the values are their respective primary types.
#' @examples
#' df <- data.frame(a = 1:3, b = c("A", "B", "C"), c = factor(c("X", "Y", "X")))
#' get_col_types(df)
#'
#' @importFrom purrr map_chr
#' @export

get_col_types <- function(df, print_it = TRUE) {
  x <- purrr::map_chr(df, ~ class(.x)[1])
  if (print_it) {
    typetable <- t(t(x))
    colnames(typetable) <- "Type"
    print(typetable)
  }
  print("Summary")
  print(table(x))
  return(x)
}





# _________________________________________________________________________________________________
#' @title Convert List Columns of a Tibble to String Vectors
#'
#' @description Converts columns of type `list` in a tibble or data frame to string vectors.
#' It combines list elements into a single string per cell, using a specified separator.
#' @param df A tibble or data frame where list columns are to be converted.
#'           Default: None, must be supplied by the user.
#' @param verbose Logical; whether to print progress messages. Default: TRUE.
#' @param print_full Logical; whether to print full details. Default: FALSE.
#' @param collapse_by The character used to collapse list elements. Default: ",".
#' @return A tibble or data frame with list columns converted to string vectors.
#'
#' @examples
#' df <- tibble::tibble(a = list(1:2, 3:4, 5:6), b = c("A", "B", "C"))
#' fix_tibble_lists(df)
#'
#' @importFrom purrr map_chr
#' @importFrom tibble as_tibble
#'
#' @export
fix_tibble_lists <- function(df, verbose = TRUE, print_full = FALSE, collapse_by = ",") {
  stopifnot(
    is.data.frame(df), is.logical(verbose),
    is.logical(print_full), is.character(collapse_by)
  )

  if (verbose) {
    cat("Before conversion:\n")
    coltypes <- get_col_types(df, print_it = print_full)
  }

  list_cols <- which(coltypes %in% "list") # Identify list columns

  # Convert list columns to string vectors
  df[, list_cols] <- purrr::map(
    df[, list_cols],
    ~ sapply(.x, paste, collapse = collapse_by)
  )

  if (verbose) {
    cat("\nAfter conversion:\n")
    get_col_types(df, print_it = print_full)
  }

  # Output assertion
  stopifnot(is.data.frame(df))

  return(df)
}

# _________________________________________________________________________________________________
#' @title Rotate a Matrix by 90 Degrees
#'
#' @description Rotates a given numeric matrix 90 degrees in a specified direction. The rotation
#' can be either clockwise or counterclockwise, determined by the `clockwise` parameter.
#'
#' @param x A numeric matrix that is to be rotated.
#' @param clockwise Logical; if TRUE (default), rotates the matrix 90 degrees clockwise.
#'
#' @return A numeric matrix rotated 90 degrees in the specified direction.
#'
#' @examples
#' # Define a 3x3 matrix
#' matrix_original <- matrix(1:9, nrow = 3)
#'
#' # Rotate the matrix clockwise
#' rotated_clockwise <- rotate(matrix_original, TRUE)
#'
#' # Rotate the matrix counterclockwise
#' rotated_counterclockwise <- rotate(matrix_original, FALSE)
#'
#' @export
rotate_matrix <- function(x, clockwise = TRUE) {
  if (clockwise) {
    t(apply(x, 2, rev)) # first reverse, then transpose, it's the same as rotate 90 degrees
  } else {
    apply(t(x), 2, rev) # first transpose, then reverse, it's the same as rotate -90 degrees
  }
}


# _________________________________________________________________________________________________
## Matrix filtering ____________________________________________________________ ----


#' @title Omit Rows with NA Values from a Matrix
#'
#' @description Removes rows from a matrix based on the presence of NA values. Can remove rows with any NA values or only those completely filled with NAs.
#'
#' @param mat Input matrix from which rows with NAs are to be omitted.
#' @param any Logical; if TRUE (default), removes rows containing any NA values.
#' If FALSE, removes only rows completely filled with NA values.
#'
#' @return A matrix with rows containing NA values omitted according to the specified criteria.
#'
#' @examples
#' mat <- matrix(c(1, NA, 3, 4, 5, NA, NA, NA, 9), ncol = 3)
#' na.omit.mat(mat) # Default, any = TRUE
#' na.omit.mat(mat, any = FALSE)
#'
#' @export
na.omit.mat <- function(mat, any = TRUE) {
  mat <- as.matrix(mat)
  stopifnot(length(dim(mat)) == 2)
  if (any) {
    outMat <- mat[!is.na(rowSums(mat)), ]
  } else {
    outMat <- mat[(rowSums(is.na(mat)) < ncol(mat)), ]
  }
  outMat
}

# _________________________________________________________________________________________________
#' @title remove.na.rows
#' @description Cols have to be a vector of numbers corresponding to columns.
#' @param mat In put matrix.
#' @param cols PARAM_DESCRIPTION, Default: 1:NCOL(mat)
#' @export
remove.na.rows <- function(mat, cols = 1:NCOL(mat)) {
  mat2 <- mat[, cols]
  idxOK <- which(rowSums(!apply(mat2, 2, is.na)) == NCOL(mat))
  mat[idxOK, ]
}



# _________________________________________________________________________________________________
#' @title remove.na.cols
#' @description Cols have to be a vector of numbers corresponding to columns.
#' @param mat In put matrix.
#' @export
remove.na.cols <- function(mat) {
  idxOK <- !is.na(colSums(mat))
  return(mat[, idxOK])
}




# _________________________________________________________________________________________________
#' @title Remove empty rows and columns from a data frame.
#'
#' @description This function takes a data frame and a threshold value, and removes all rows and columns that contain only zeros or the threshold value.
#'
#' @param df A data frame.
#' @param suffix A suffix to add to the plot titles.
#' @param rows The name of the variable that will store the fraction of rows that were removed.
#' @param cols The name of the variable that will store the fraction of columns that were removed.
#' @param thr.cell.empty The threshold value below a cell is considered "empty".
#' @param plot_stats Whether to plot the fraction of rows and columns that were removed.
#' @param ... Additional arguments to pass to `qbarplot`.
#'
#' @return A data frame with the empty rows and columns removed.
#' @export

df.remove.empty.rows.and.columns <- function(
    df = UVI.assignment.filtered.3.HF,
    suffix =  substitute_deparse(df),
    rows = "rows",
    cols = "cols",
    thr.cell.empty = 0,
    plot_stats = TRUE,
    ...) {
  # Create a boolean vector that indicates whether each cell is non-empty
  df.boolean <- (df != thr.cell.empty)
  # view.head(df.boolean)

  # Calculate the number of non-empty rows and columns
  rsx <- rowSums(df.boolean)
  csx <- colSums(df.boolean)

  # Calculate the fraction of rows and columns that were removed
  s1 <- pc_TRUE(csx == 0, suffix = paste0(cols, " are empty/removed."), NumberAndPC = TRUE)
  s2 <- pc_TRUE(rsx == 0, suffix = paste0(rows, " are empty/removed."), NumberAndPC = TRUE)
  print(s1)
  print(s2)

  # Plot the fraction of rows and columns that were removed, if requested
  if (plot_stats) {
    Removal.Dimensions <- c(
      "rows" = pc_TRUE(rsx == 0, percentify = FALSE),
      "cols" = pc_TRUE(csx == 0, percentify = FALSE)
    )
    names(Removal.Dimensions) <- c(rows, cols)
    qbarplot(Removal.Dimensions,
             label = percentage_formatter(Removal.Dimensions),
             suffix = suffix,
             xlab.angle = 45, xlab = "",
             ylim = 0:1, ylab = "Fractions removed",
             ...
    )
  }

  # Remove the empty rows and columns
  df.filt <- df[rsx > 0, csx > 0, drop = FALSE]
  idim(df.filt)
  return(df.filt)
}



# _________________________________________________________________________________________________
## Create Special Matrices ____________________________________________________________ ----

# _________________________________________________________________________________________________
#' @title rowNameMatrix
#' @description Create a copy of your matrix, where every entry is replaced by the corresponding
#' row name. Useful if you want to color by row name in a plot (where you have different number of
#'  NA-values in each row).
#' @param mat_w_dimnames A named matrix to copy from.
#' @export
rowNameMatrix <- function(mat_w_dimnames) {
  matrix(rep(rownames(mat_w_dimnames), ncol(mat_w_dimnames)), nrow = nrow(mat_w_dimnames), ncol = ncol(mat_w_dimnames))
}



# _________________________________________________________________________________________________
#' @title colNameMatrix
#' @description Create a copy of your matrix, where every entry is replaced by the corresponding
#' column name. Useful if you want to color by column name in a plot (where you have different
#' number of NA-values in each column).
#' @param mat_w_dimnames A named matrix to copy from.
#' @export
colNameMatrix <- function(mat_w_dimnames) {
  x <- rep(colnames(mat_w_dimnames), nrow(mat_w_dimnames))
  t(matrix(x, nrow = ncol(mat_w_dimnames), ncol = nrow(mat_w_dimnames)))
}




# _________________________________________________________________________________________________
# Multi-dimensional lists ____________________________________________________________ ----


#' @title copy.dimension.and.dimnames
#'
#' @description Copy the dimension and dimnames of a 1D vector to a 2D array.
#' @param list.1D A 1D vector.
#' @param obj.2D A 2D array.
#' @return A 2D array with the same dimension and dimnames as `obj.2D`.
#' @export
copy.dimension.and.dimnames <- function(list.1D, obj.2D) {
  dim(list.1D) <- dim(obj.2D)
  dimnames(list.1D) <- dimnames(obj.2D)
  list.1D
}


# _________________________________________________________________________________________________
#' @title mdlapply
#'
#' @description A wrapper for `lapply()` that works on multidimensional arrays.
#' @param list_2D A multidimensional array.
#' @param ... Function and arguments to pass to `lapply()`.
#' @return A multidimensional array with the same dimensions as `list_2D`.
#' @export

mdlapply <- function(list_2D, ...) {
  x <- lapply(list_2D, ...)
  copy.dimension.and.dimnames(x, list_2D)
}


# _________________________________________________________________________________________________
#' @title arr.of.lists.2.df
#' @description Simplify 2D-list-array to a DF.
#' @param two.dim.arr.of.lists PARAM_DESCRIPTION
#' @export
arr.of.lists.2.df <- function(two.dim.arr.of.lists) {
  list.1D <- unlist(two.dim.arr.of.lists)
  dim(list.1D) <- dim(two.dim.arr.of.lists)
  dimnames(list.1D) <- dimnames(two.dim.arr.of.lists)
  list.1D
}



# _________________________________________________________________________________________________
#' @title mdlapply2df
#' @description Multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF).
#' @param list_2D PARAM_DESCRIPTION
#' @param ... Pass any other argument.
#' @export
mdlapply2df <- function(list_2D, ...) {
  x <- lapply(list_2D, ...)
  z <- copy.dimension.and.dimnames(x, list_2D)
  arr.of.lists.2.df(z)
}



# _________________________________________________________________________________________________
# List operations ____________________________________________________________ ----


#' @title any.duplicated.rownames.ls.of.df
#' @description Check if there are any duplocated rownames in a list of dataframes.
#' @param ls List of 2 or more vectors (sets) with categorical variables.
#'
#' @export any.duplicated.rownames.ls.of.df
any.duplicated.rownames.ls.of.df <- function(ls) any.duplicated(rownames(ls)) # Check if there are any duplocated rownames in a list of dataframes.



# _________________________________________________________________________________________________
#' @title intersect.ls
#' @description Intersect any number of lists.
#' @param ls List of 2 or more vectors (sets) with categorical variables.
#' @param ... Pass any other argument.
#'
#' @export
intersect.ls <- function(ls, ...) {
  Reduce(intersect, ls)
} # Intersect any number of lists.



# _________________________________________________________________________________________________
#' @title union.ls
#' @description Intersect any number of list elements. Faster than reduce.
#' @param ls List of 2 or more vectors (sets) with categorical variables.
#' @param ... Pass any other argument.
#' @export
union.ls <- function(ls, ...) {
  sort(unique(do.call(c, ls)))
} # Intersect any number of list elements. Faster than reduce.

# _________________________________________________________________________________________________
#' @title symdiff.ls
#' @description Calculate symmetric difference on a list (of 2 vectors).
#' @param ls List of 2 or more vectors (sets) with categorical variables.
#' @param ... Pass any other argument.
#' @export
symdiff.ls <- function(ls, ...) {
  res <- Reduce(symdiff, ls)
  if (length(names(ls)) == length(ls)) {
    names(res) <- names(ls)
  } else {
    message("No names in list / some names missing. Numeric names will be used.")
    names(res) <- 1:length(res)
  }

  return(res)
} # Intersect any number of list elements. Faster than reduce.

# _________________________________________________________________________________________________
#' @title setdiff.ls
#' @description Calculate set difference on a list (of 2 vectors).
#' @param ls List of 2 or more vectors (sets) with categorical variables.
#' @param ... Pass any other argument.
#' @export
setdiff.ls <- function(ls, ...) {
  print("Difference to first element:")
  Reduce(setdiff, ls)
} # Intersect any number of list elements. Faster than reduce.


# _________________________________________________________________________________________________
#' @title Like sapply, but with names preserved. Help in some other cases too.
#'
#' @description Do an `lapply()`, then `unlist()`, with preserving the list element names.
#' @param list A list to apply the function to.
#' @param FUN PARAM_DESCRIPTION
#' @param ... Pass any other argument.
#'
#' @export
unlapply <- function(list, FUN, ...) {
  x <- unlist(lapply(X = list, FUN = FUN, ...))
  # names(x) <- names(list) # not needed
  return(x)
}



# _________________________________________________________________________________________________
#' @title list.wNames
#' @description Create a list with names from ALL variables you pass on to the function.
#' @param ... Pass any other argument.
#' @export
list.wNames <- function(...) {
  lst <- list(...)
  names(lst) <- as.character(match.call()[-1])
  return(lst)
}



# _________________________________________________________________________________________________
#' @title as.list.df.by.row
#'
#' @description Split a dataframe into a list by its columns.
#' @param dtf A dataframe.
#' @param na.omit Whether to omit rows with missing values.
#' @param zero.omit Whether to omit rows with all-zero values.
#' @param omit.empty Whether to omit rows with zero length.
#' @param verbose Whether to print messages.
#' @return A list.
#' @examples
#' dtf <- data.frame(x = c(1, 2, NA), y = c(3, 4, 0), z = c(5, 6, 7))
#' as.list.df.by.row(dtf)
#'
#' @export as.list.df.by.row
as.list.df.by.row <- function(dtf, na.omit = TRUE, zero.omit = FALSE, omit.empty = FALSE,
                              verbose = TRUE) {
  outList <- as.list(as.data.frame(t(dtf)))

  # Omit rows with missing values, all-zero values, or zero length
  if (na.omit) {
    outList <- lapply(outList, na.omit.strip)
  }
  if (zero.omit) {
    outList <- lapply(outList, zero.omit)
  }
  if (omit.empty) {
    outList <- outList[(lapply(outList, length)) > 0]
  }
  if (verbose) print(str(outList, vec.len = 2))
  return(outList)
}



# _________________________________________________________________________________________________
#' @title as.list.df.by.col
#'
#' @description Split a dataframe into a list by its rows.
#' @param dtf A dataframe.
#' @param na.omit Whether to omit rows with missing values.
#' @param zero.omit Whether to omit rows with all-zero values.
#' @param omit.empty Whether to omit rows with zero length.
#' @return A list.
#' @examples
#' dtf <- data.frame(x = c(1, 2, NA), y = c(3, 4, 0), z = c(5, 6, 7))
#' as.list.df.by.col(dtf)
#'
#' @export as.list.df.by.col
as.list.df.by.col <- function(dtf, na.omit = TRUE, zero.omit = FALSE, omit.empty = FALSE) {
  outList <- as.list(dtf)
  if (na.omit) {
    outList <- lapply(outList, na.omit.strip)
  }
  if (zero.omit) {
    outList <- lapply(outList, zero.omit)
  }
  if (omit.empty) {
    outList <- outList[(lapply(outList, length)) > 0]
  }
  print(str(outList, vec.len = 2))
  return(outList)
}



# _________________________________________________________________________________________________
#' @title reorder.list
#'
#' @description Reorder elements of lists in your custom order of names / indices.
#' @param L A list.
#' @param namesOrdered A vector of names or indices in the desired order.
#' @return A list.
#' @examples
#' L <- list(a = 1, b = 2, c = 3)
#' namesOrdered <- c("c", "a", "b")
#' reorder.list(L, namesOrdered)
#' @export reorder.list
#' @seealso
#'  \code{\link[gtools]{mixedsort}}
#' @importFrom gtools mixedsort
reorder.list <- function(L, namesOrdered = gtools::mixedsort(names(L))) {
  Lout <- list(NA)
  for (x in 1:length(namesOrdered)) {
    Lout[[x]] <- L[[namesOrdered[x]]]
  }
  if (length(names(L))) {
    names(Lout) <- namesOrdered
  }
  return(Lout)
}



# _________________________________________________________________________________________________
#' @title range.list
#'
#' @description Calculates the range of values in the entire a list.
#' @param L A list.
#' @return A vector of length 2, containing the minimum and maximum values in `L`.
#'
#' @export range.list
range.list <- function(L) {
  return(range(unlist(L), na.rm = TRUE))
}



# _________________________________________________________________________________________________
#' @title intermingle2lists
#'
#' @description Combine 2 lists (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().
#' @param L1 A list.
#' @param L2 A list.
#' @return A list.
#' @export
intermingle2lists <- function(L1, L2) {
  stopifnot(length(L1) == length(L2))
  Lout <- list(NA)

  # Create a new list with the combined elements of `L1` and `L2`
  for (x in 1:(2 * length(L1))) {
    if (x %% 2) {
      Lout[[x]] <- L1[[((x + 1) / 2)]]
      names(Lout)[x] <- names(L1)[((x + 1) / 2)]
    } else {
      Lout[[x]] <- L2[[x / 2]]
      names(Lout)[x] <- names(L2)[x / 2]
    }
  } # for
  return(Lout)
}



# _________________________________________________________________________________________________
#' @title as.listalike
#'
#' @description Converts a vector to a list with the same dimensions as a given list.
#' @param vec A vector.
#' @param list_wannabe A list with the desired dimensions.
#' @return A list with the same dimensions as `list_wannabe`, and the same elements as `vec` in the same order.
#'
#' @examples
#' vec <- 1:10
#' list_wannabe <- list(a = 1:3, b = 4:6, c = 7:10)
#' as.listalike(vec, list_wannabe)
#'
#' @export
as.listalike <- function(vec, list_wannabe) {
  stopifnot(length(vec) == length(unlist(list_wannabe)))
  list_return <- list_wannabe
  past <- 0

  # Iterate over the list, and fill in the elements with the corresponding elements from the vectorfor (v in 1:length(list_wannabe)) {
  for (v in 1:length(list_wannabe)) {
    lv <- length(list_wannabe[[v]])
    list_return[[v]] <- vec[(past + 1):(past + lv)]
    past <- past + lv
  } # for
  return(list_return)
}



# _________________________________________________________________________________________________
#' @title Reverse the Hierarchy of a List
#'
#' @description This function reverses the hierarchy of a given, 2 level, nested list. The
#' function will ensure that all lists at the same level have the same names,
#' and then transpose the structure, creating a new list for each unique name.
#' Any missing elements in the original lists are not included in the final
#' output. The result is a list where the top-level names are derived from
#' the unique names found at the lower levels of the input list.
#'
#' @param list_of_lists A list where some or all elements are themselves lists.
#' The hierarchy of this list will be reversed.
#'
#' @return A list with the elements of the original list in reversed order.
#' @source https://stackoverflow.com/a/15263737
#' @examples
#' list_of_lists <- list("z1" = list(a = 1, b = 2), "z2" = list(b = 4, a = 1, c = 0))
#' reverse.list.hierarchy(list_of_lists)
#'
#' @export
reverse.list.hierarchy <- function(list_of_lists) {
  # Find unique names in all sublists
  names_level2 <- unique(unlist(lapply(list_of_lists, function(X) names(X))))
  iprint("Level-1 names:", names(list_of_lists))
  iprint("Level-2 names:", names_level2)

  # Ensure all lists have the same names, in the same order
  list_of_lists <- lapply(list_of_lists, function(X) setNames(X[names_level2], names_level2))

  # Transpose the structure, creating a new list for each unique name
  list_of_lists <- apply(do.call(rbind, list_of_lists), 2, as.list)

  # Remove null entries
  lapply(list_of_lists, function(X) X[!sapply(X, is.null)])
}




# _________________________________________________________________________________________________
#' @title list2fullDF.byNames
#'
#' @description Converts a list to a full matrix, with rows and columns named by the elements of the list.
#' @param your.list A list.
#' @param as.df Logical. Whether to return a data frame (default) or a matrix.
#' @param byRow Logical. Whether the resulting matrix should be arranged by row (default) or by column.
#' @param FILL A value to fill in missing entries.
#' @return A matrix with the same elements as `your.list`, but with rows and columns named by the elements of the list.
#' @examples
#' your.list <- list(set.1 = LETTERS[1:5], set.2 = LETTERS[3:9])
#' list2fullDF.byNames(your.list)
#'
#' @export
list2fullDF.byNames <- function(your.list = list(
  "set.1" = vec.fromNames(LETTERS[1:5], fill = 1), # Convert a list to a full matrix. Rows = names(union.ls(your_list)) or all names of within list elements, columns = names(your_list).
  "set.2" = vec.fromNames(LETTERS[3:9], fill = 2)
),
as.df = TRUE,
byRow = TRUE,
FILL = NA) {
  # Get the lengths of the list elements
  length.list <- length(your.list)
  list.names <- names(your.list)
  list.element.names <- sort(unique(unlist(lapply(your.list, names))))

  # Create a matrix with the correct dimensions
  mat <- matrix.fromNames(rowname_vec = list.element.names, colname_vec = list.names, fill = FILL)

  # Fill in the matrix with the elements of the list
  for (i in 1:length.list) {
    element <- list.names[i]
    mat[names(your.list[[element]]), element] <- your.list[[element]]
  }
  if (!byRow) {
    mat <- t(mat)
  }
  if (as.df) mat <- as.data.frame(mat)
  return(mat)
}



# _________________________________________________________________________________________________
#' @title list2fullDF.presence
#'
#' @description Converts a list to a full matrix, with rows and columns named by the elements of the list.
#' The matrix will contain a 1 in each cell where the corresponding element of the list is present, and a 0 otherwise.
#' @param your.list A list.
#' @param byRow Logical. Whether the resulting matrix should be arranged by row (default) or by column.
#' @param FILL A value to fill in missing entries.
#' @return A matrix with the same elements as `your.list`, but with rows and columns named by the elements of the list.
#' @examples
#' your.list <- list("set.1" = LETTERS[1:5], "set.2" = LETTERS[3:9])
#' list2fullDF.presence(your.list)
#'
#' @export
list2fullDF.presence <- function(your.list = list(
  "set.1" = LETTERS[1:5] # Convert a list to a full matrix.  Designed for occurence counting, think tof table(). Rows = all ENTRIES of within your list, columns = names(your_list).
  , "set.2" = LETTERS[3:9]
), byRow = TRUE, FILL = 0) {
  length.list <- length(your.list)
  list.names <- names(your.list)
  list.elements <- sort(Reduce(f = union, your.list))

  # Create a matrix with the correct dimensions
  mat <- matrix.fromNames(rowname_vec = list.elements, colname_vec = list.names, fill = FILL)

  # Fill in the matrix with the elements of the list
  for (i in 1:length.list) {
    element <- list.names[i]
    mat[your.list[[element]], element] <- 1
  }
  if (!byRow) {
    mat <- t(mat)
  }
  return(mat)
}



# _________________________________________________________________________________________________
#' @title splitbyitsnames
#' @description Split a list by its names.
#' @param namedVec Vector with names
#' @export
splitbyitsnames <- function(namedVec) {
  stopifnot(!is.null(names(namedVec)))
  split(namedVec, f = names(namedVec))
}



# _________________________________________________________________________________________________
#' @title Split the names of list by its values.
#' @description Split the names of a list by its its values.
#' @param namedVec A vector with names.
#'
#' @return A list of vectors, each of which contains the elements of `namedVec` that have the corresponding value.
#'
#' @examples
#' namedVec <- c("A", "B", "C", "A", "B", "D")
#' splititsnames_byValues(namedVec)
#'
#' @export
splititsnames_byValues <- function(namedVec) {
  stopifnot(!is.null(names(namedVec)))
  split(names(namedVec), f = namedVec)
}



# _________________________________________________________________________________________________
#' @title intermingle2vec
#'
#' @description Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.
#' @param V1 A vector.
#' @param V2 A vector.
#' @param wNames Logical. Whether to include the names of the vectors in the output vector.
#' @param name_prefix A character vector of length 2. If provided, the names of the vectors will
#' be prefixed with the corresponding element of `name_prefix`.
#' @return A vector that combines `V1` and `V2`, with the elements of `V1` alternating with the elements of `V2`.
#' @examples
#' V1 <- c(1, 3, 5)
#' V2 <- c(2, 4, 6)
#' intermingle2vec(V1, V2)
#'
#' @export
intermingle2vec <- function(V1, V2, wNames = TRUE, name_prefix = NULL) {
  stopifnot(
    length(V1) == length(V2),
    is.null(name_prefix) | length(name_prefix) == 2
  )

  if (!is.null(name_prefix)) {
    names(V1) <- paste0(names(V1), name_prefix[1])
    names(V2) <- paste0(names(V2), name_prefix[2])
  }

  Vout <- c(rbind(V1, V2))

  if (wNames) {
    names(Vout) <- c(rbind(names(V1), names(V2)))
  }
  return(Vout)
}



# _________________________________________________________________________________________________
#' @title intermingle.cbind
#'
#' @description Combine 2 data frames (of the same length) so that form every odd and every even
#' element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().
#' @param df1 A data frame.
#' @param df2 A data frame.
#' @return A data frame that combines `df1` and `df2`, with the columns of `df1` alternating with the columns of `
#'
#' @export

intermingle.cbind <- function(df1, df2) {
  stopifnot(ncol(df1) == ncol(df2))
  if (nrow(df1) != nrow(df2)) { # not equal rows: subset
    print(symdiff(rownames(df2), rownames(df1)))
    CommonGenes <- intersect(rownames(df2), rownames(df1))
    print(length(CommonGenes))
    df1 <- df1[CommonGenes, ]
    df2 <- df2[CommonGenes, ]
  } else {
    CommonGenes <- rownames(df1)
  }

  # Create New column names
  if (length(colnames(df1)) == ncol(df1) & length(colnames(df2)) == ncol(df2)) {
    NewColNames <- intermingle2vec(paste0("df1.", colnames(df1)), paste0("df2.", colnames(df2)))
  } else {
    NewColNames <- intermingle2vec(paste0("df1.", 1:ncol(df1)), paste0("df2.", 1:ncol(df2)))
  }
  NewMatr <- matrix.fromNames(rowname_vec = CommonGenes, colname_vec = NewColNames)
  for (x in 1:(2 * length(df1))) {
    if (x %% 2) {
      NewMatr[, x] <- df1[, (x + 1) / 2]
    } else {
      NewMatr[, x] <- df2[, (x) / 2]
    }
  } # for
  print(idim(NewMatr))
  return(NewMatr)
}



# _________________________________________________________________________________________________
#' @title ls2categvec
#'
#' @description Converts a list to a vector repeating list-element names, while vector names are the list elements.
#' @param your_list A list.
#' @return A vector with the same elements as `your_list`, but with the names of the list elements repeated as many times as the number of elements in each list element.
#' @examples ls2categvec(list(a = 1, b = 2, c = 3))
#' @export
ls2categvec <- function(your_list) {
  VEC <- rep(names(your_list), unlapply(your_list, length))
  names(VEC) <- unlist(your_list, use.names = TRUE)
  return(VEC)
}



# _________________________________________________________________________________________________
#' @title list.2.replicated.name.vec
#'
#' @description Converts a list to a vector, with list elements names replicated as many times as many elements each element had.
#' @param ListWithNames A list.
#' @return A vector with the same elements as `ListWithNames`, but with the names of the list elements repeated as many times as the number of elements in each list element.
#' @examples
#' ListWithNames <- list(a = 1, b = 2, c = 3)
#' list.2.replicated.name.vec(ListWithNames)
#' @export
list.2.replicated.name.vec <- function(ListWithNames = Sections.ls.Final) {
  NZ <- names(ListWithNames)
  LZ <- unlapply(ListWithNames, length)
  replicated.name.vec <- rep(NZ, LZ)
  names(replicated.name.vec) <- unlist(ListWithNames)
  return(replicated.name.vec)
}

# _________________________________________________________________________________________________
## Set operations ____________________________________________________________ ----


#' @title Symmetric difference
#'
#' @description Quasi symmetric difference of any number of vectors.
#' @param x A vector.
#' @param y A vector.
#' @param z A vector.
#' @return A list of vectors, each of which contains the elements that are only present in that
#' vector and not in any of the other vectors.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 4, 6, 8, 10)
#' z <- c(3, 5, 7, 9, 11)
#' symdiff(x, y, z)
#'
#' @export
symdiff <- function(x, y, z = NULL) {
  big.vec <- c(unique(x), unique(y), unique(z))
  # ls <- list(x, y, z)
  ls <- Filter(function(l) !is.null(l), list(x, y, z))
  if (length(ls) > 2) {
    print("# Not Mathematically correct for more than 2 vectors,
          but logical: https://en.wikipedia.org/wiki/Symmetric_difference#Properties")
  }
  names(ls) <- paste("Only in", as.character(match.call()[-1]))
  duplicates <- big.vec[duplicated(big.vec)]
  lapply(ls, function(x) setdiff(x, duplicates))
}


#' @title Intersect with Name Preservation
#'
#' @description Intersects two character vectors while preserving names from the specified vector.
#'
#' @param x A character vector.
#' @param y A character vector.
#' @param names Character. Specifies which vector's names to preserve in the output.
#'   "x" preserves `x`'s names, "y" preserves `y`'s names. Default: "x".
#'
#' @return A character vector with names preserved from the specified vector (`x` or `y`).
#'
#' @examples
#' intersect.wNames(c(a = "gene1", b = "gene2"), c("gene2", "gene3"), names = "x")
#'
#' @export intersect.wNames
intersect.wNames <- function(x, y, names = "x") {
  # browser()
  stopifnot(
    is.vector(x), is.vector(y), names %in% c("x", "y")
  )
  warnif(
    "x argument has no names!" =  (names == "x" & !Stringendo::HasNames(x) )
    , "y argument has no names!" = (names == "y" & !Stringendo::HasNames(y) )
  )


  # Perform intersection with name preservation based on `names` argument
  result <-
    if (names == "x") {
      x[x %in% intersect(x, y)]
    } else {
      y[y %in% intersect(x, y)]
    }

  return(result)
}


#' @title Union with Name Preservation
#'
#' @description Unites two character vectors while preserving names from the specified vector.
#'   Gives a warning if there are conflicts in names between `x` and `y`.
#'
#' @param x A character vector.
#' @param y A character vector.
#' @param names Character. Specifies which vector's names to preserve in the output.
#'   "x" preserves `x`'s names, "y" preserves `y`'s names. Default: "x".
#'
#' @return A character vector with names preserved from the specified vector (`x` or `y`).
#'
#' @examples
#' union.wNames(x =c(a = "gene1", b = "gene2", c = "gene3")
#' , y =c( c = "gene3", dada = "gene2", "gene4")
#' , names = "x")
#'
#' @export union.wNames
union.wNames <- function(x, y, names = "x") {
  stopifnot(
    is.vector(x) || is.null(x),
    is.vector(y) || is.null(y),
    is.vector(x) || is.vector(y),
    names %in% c("x", "y")
  )
  warnifnot(HasNames(x), HasNames(y) )

  if(is.null(x)) {
    message("x is NULL, returning y.")
    return(y)
  }

  if(is.null(y)) {
    message("y is NULL, returning x.")
    return(x)
  }

  # Perform union
  all_elements <- union(x, y)
  common_elements <- intersect(x, y)

  # Check if names agree
  names_x <- names(sort(x[x %in% common_elements]))
  names_y <- names(sort(y[y %in% common_elements]))

  # Check for name conflicts: if names of common elements are different, issue a warning.
  if ( !identical(names_x, names_y) ) {
    warning("Names of intersecting elements is not the same in x & y!", immediate. = T)
    iprint("names_x: ", head(names_x))
    iprint("names_y: ", head(names_y))

    message("Names, for intersecting elements, inherited from: ", names)
  }


  result <-
    if (names == "x") {
      c(x, setdiff(y, x))
    } else if (names == "y") {
      c(y, setdiff(x, y))
    }

    message("Beware that: union(x, y) != union(y, x) - only if you sort the values.")
    return(result)
}



# _________________________________________________________________________________________________
## Math & stats ____________________________________________________________ ----



#' @title iround
#' @description Rounds a value to the significant amount of digits. Its a wrapper for signif().
#' @param x Unrounded number.
#' @param digitz Number of digits to keep. 3 by default.
#' @examples iround(x = 2.3232, digitz = 3)
#'
#' @export
iround <- function(x, digitz = 3) {
  signif(x, digits = digitz)
}



# _________________________________________________________________________________________________
#' @title modus
#'
#' @description Calculates the mode (modus) of a numeric vector (it excludes NA-s by default). https://en.wikipedia.org/wiki/Mode_(statistics)
#' @param x A numeric vector
#' @import stats
#' @examples modus(c(1, 1, 2, 3, 3, 3, 4, 5))
#' modus(1:4)
#'
#' @export
modus <- function(x) {
  x <- unlist(na.exclude(x))
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}



# _________________________________________________________________________________________________
#' @title cv
#'
#' @description Calculates the coefficient of variation (CV) for a numeric vector (it excludes NA-s by default).
#' @param x A numeric vector.
#' @param na.rm Should NA values be removed before calculation? Defaults to TRUE.
#' @return The coefficient of variation of the input vector.
#' @examples cv(1:5)
#'
#' @export
cv <- function(x, na.rm = TRUE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}



# _________________________________________________________________________________________________
#' @title sem
#'
#' @description Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default).
#' @param x A numeric vector.
#' @param na.rm Should NA values be removed before calculation? Defaults to TRUE.
#' @return The standard error of the mean of the input vector.
#' @examples sem(1:5)
#'
#' @export
sem <- function(x, na.rm = TRUE) {
  sd(unlist(x), na.rm = na.rm) / sqrt(length(na.omit.strip(as.numeric(x)))) # Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default)
}




# _________________________________________________________________________________________________
#' @title fano
#'
#' @description Calculates the fano factor on a numeric vector (it excludes NA-s by default).
#' @param x A numeric vector.
#' @param na.rm Should NA values be removed before calculation? Defaults to TRUE.
#' @param USE The method used to calculate the variance. Defaults to `"na.or.complete"`.
#' @return The fano factor of the input vector.
#' @examples fano(1:5)
#'
#' @export
fano <- function(x, na.rm = TRUE, USE = "na.or.complete") {
  var(x, na.rm = na.rm, use = USE) / mean(x, na.rm = na.rm) # Calculates the fano factor on a numeric vector (it excludes NA-s by default)
}

# _________________________________________________________________________________________________
#' @title geomean
#'
#' @description Calculates the geometric mean of a numeric vector (it excludes NA-s by default).
#' @param x A numeric vector.
#' @param na.rm Should NA values be removed before calculation? Defaults to TRUE.
#' @return The geometric mean of the input vector.
#' @examples geomean(1:5)
#'
#' @export
geomean <- function(x, na.rm = TRUE) {
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}



# _________________________________________________________________________________________________
#' @title mean_of_log
#'
#' @description Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default).
#' @param x A numeric vector.
#' @param k The base of the logarithm. Defaults to 2.
#' @param na.rm Should NA values be removed before calculation? Defaults to TRUE.
#' @return The mean of the log_k of the input vector.
#' @examples mean_of_log(1:5)
#'
#' @export
mean_of_log <- function(x, k = 2, na.rm = TRUE) {
  stopifnot(is.numeric(x), length(k) == 1, is.finite(k), is.logical(na.rm))

  negs <- sum(x < 0)
  zeros <- sum(x == 0)
  if (negs | zeros) {
    iprint("The input vector has", negs, "negative values and", zeros, "zeros.")
  }
  mean(log(x, base = k), na.rm = na.rm)
}



# _________________________________________________________________________________________________
#' @title Moving / rolling average
#'
#' @description Calculates the moving / rolling average of a numeric vector.
#' @param x A numeric vector.
#' @param oneSide The size of the moving window. Defaults to 5.
#' @return A vector of the moving averages.
#' @examples movingAve(1:5)
#'
#' @export
movingAve <- function(x, oneSide = 5) {
  y <- NULL
  for (i in oneSide:length(x)) {
    y[i] <- mean(x[(i - oneSide):(i + oneSide)])
  }
  return(y)
}



# _________________________________________________________________________________________________
#' @title Moving / rolling average (v2, filter)
#' @description Calculates the moving / rolling average of a numeric vector, using `filter()`.
#' @param x A numeric vector.
#' @param n The size of the moving window. Defaults to 5.
#' @return A vector of the moving averages.
#' @examples movingAve2(1:5)
#'
#' @export
movingAve2 <- function(x, n = 5) {
  filter(x, rep(1 / n, n), sides = 2)
} # Calculates the moving / rolling average of a numeric vector, using filter().



# _________________________________________________________________________________________________
#' @title movingSEM
#' @description Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.
#' @param x A numeric vector.
#' @param oneSide The size of the moving window, in terms of the number of elements on either side of the current element.
#' @return A vector of the same length as `x`, containing the SEMs for each element.
#' @export
movingSEM <- function(x, oneSide = 5) {
  # Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.
  y <- NULL
  for (i in oneSide:length(x)) {
    y[i] <- sem(x[(i - oneSide):(i + oneSide)])
  }
  return(y)
}



# _________________________________________________________________________________________________
#' @title imovingSEM
#'
#' @description Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller window-size.
#' @param x A numeric vector.
#' @param oneSide The size of the moving window, in terms of the number of elements on either side of the current element.
#' @return A vector of the same length as `x`, containing the SEMs for each element.
#' @export
imovingSEM <- function(x, oneSide = 5) {
  # Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller window-size.
  y <- NULL
  for (i in 1:length(x)) {
    oneSideDynamic <- min(i - 1, oneSide, length(x) - i)
    oneSideDynamic
    indexx <- (i - oneSideDynamic):(i + oneSideDynamic)
    y[i] <- sem(x[indexx])
  }
  return(y)
}


################################################################################################
# NOT YET ORGANIZED


# _________________________________________________________________________________________________
#' @title Pretty Printing of R Objects (pretty_dput)
#'
#' @description This function modifies the output of the traditional dput() function
#' so that each key-value pair in a vector appears on a new line. It's useful for
#' creating more readable output.
#' @param vec A named vector to be printed. The vector should be in the form `c('name' = 'value')`.
#' @return No return value. Outputs the vector with each element on a new line.
#' @examples
#' vec <- c(`0` = "ACyte", `1` = "Misp.1.DCN")
#' pretty_dput(vec)
#'
#' @export
dput_pretty <- pretty_dput <- function(vec) {
  if (is.null(names(vec))) names(vec) <- 1:length(vec)
  cat("c(", sep = "")
  for (i in 1:length(vec)) {
    cat("\n`", vec[i], "` = \"", names(vec)[i], "\"",
        ifelse(i != length(vec), ",", ""),
        sep = ""
    )
  }
  cat("\n)\n")
}




################################################################################################

# DON'T DELETE: FOR BACKTRACKING

# _________________________________________________________________________________________________
#' @title as.numeric.wNames.deprecated
#'
#' @description Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
#' @param vec input vector
#'
#' @export as.numeric.wNames.deprecated
as.numeric.wNames.deprecated <- function(vec) {
  numerified_vec <- as.numeric(as.factor(vec)) - 1 # as factor gives numbers [1:n] instead [0:n]
  if (!is.null(names(vec))) {
    names(numerified_vec) <- names(vec)
  }
  return(numerified_vec)
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
  names(vec2) <- if (!rename & !is.null(names(vec))) {
    names(vec)
  } else {
    vec
  }
  return(vec2)
}


# _________________________________________________________________________________________________
#' @title as.named.vector.deprecated
#'
#' @description Convert a dataframe column or row into a vector, keeping the corresponding dimension name.
#' @param df_col data frame column
#' @param WhichDimNames Shall we extract rows (2) or columns (1, default)?, Default: 1
#'
#' @export as.named.vector.deprecated
as.named.vector.deprecated <- function(df_col, WhichDimNames = 1) {
  namez <- dimnames(df_col)[[WhichDimNames]]
  # use RowNames: WhichDimNames = 1 , 2: use ColNames
  # !!! might require drop = FALSE in subsetting!!! eg: df_col[, 3, drop = FALSE]
  # df_col[which(unlist(lapply(df_col, is.null)))] = "NULL" # replace NULLs - they would fall out of vectors - DOES not work yet
  if (is.list(df_col) & !is.data.frame(df_col)) {
    namez <- names(df_col)
  }
  vecc <- as.vector(unlist(df_col))
  names(vecc) <- namez
  return(vecc)
}


# _________________________________________________________________________________________________
# Deprecated ----
# _________________________________________________________________________________________________
#' @title sort.mat
#' @export sort.mat
sort.mat <- function() .Deprecated("sort_matrix_rows()")



#  ______________________________________________________________________________________
