######################################################################
# A collection of custom R functions
######################################################################
# source('~/GitHub/Packages/CodeAndRoll2/CodeAndRoll2.R')
# source('https://raw.githubusercontent.com/vertesy/CodeAndRoll2/master/CodeAndRoll2.R')

## If something is not found:
# try(source("https://raw.githubusercontent.com/vertesy/ggExpressDev/main/ggExpress.functions.R"), silent = T)

# try(source("~/Github/TheCorvinas/R/RNA_seq_specific_functions.r"), silent = T)
## For Plotting From Clipboard or Files
# source("~/Github/TheCorvinas/R/Plotting.From.Clipboard.And.Files.r")
# # Load sequence length and base distribution check
# source("~/Github/TheCorvinas/R/Gene.Stats.mm10.R")
suppressMessages(try(require(clipr), silent = T))
try(require(ggplot2),silent = T)


### CHAPTERS:

## Create and check variables -------------------------------------------------------------------------------------------------

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname vec.fromNames
#' @export 
vec.fromNames <- function(name_vec = LETTERS[1:5], fill = NA) { # create a vector from a vector of names
  v = numeric(length(name_vec))
  if (length(fill) == 1) {v = rep(fill, length(name_vec))}
  else if (length(fill == length(name_vec))) {v = fill}
  names(v) = name_vec
  return(v)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NaN
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list.fromNames
#' @export 
list.fromNames <- function(name_vec = LETTERS[1:5], fill = NaN) { # create list from a vector with the names of the elements
  liszt = as.list(rep(fill, length(name_vec)))
  names(liszt) = name_vec
  return(liszt)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rowname_vec PARAM_DESCRIPTION, Default: 1:10
#' @param colname_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname matrix.fromNames
#' @export 
matrix.fromNames <- function(rowname_vec = 1:10, colname_vec = LETTERS[1:5], fill = NA) { # Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.
  mx = matrix(data = fill, nrow = length(rowname_vec), ncol = length(colname_vec), dimnames = list(rowname_vec, colname_vec))
  iprint("Dimensions:", dim(mx))
  return(mx)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vector PARAM_DESCRIPTION, Default: 1:5
#' @param HowManyTimes PARAM_DESCRIPTION, Default: 3
#' @param IsItARow PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname matrix.fromVector
#' @export 
matrix.fromVector <- function(vector = 1:5, HowManyTimes = 3, IsItARow = TRUE) { # Create a matrix from values in a vector repeated for each column / each row. Similar to rowNameMatrix and colNameMatrix.
  matt = matrix(vector, nrow = length(vector), ncol = HowManyTimes)
  if ( !IsItARow ) {matt = t(matt)}
  return(matt)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param rowname_vec PARAM_DESCRIPTION, Default: 1:3
#' @param colname_vec PARAM_DESCRIPTION, Default: letters[1:2]
#' @param z_name_vec PARAM_DESCRIPTION, Default: LETTERS[4:6]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname array.fromNames
#' @export 
array.fromNames <- function(rowname_vec = 1:3, colname_vec = letters[1:2], z_name_vec = LETTERS[4:6], fill = NA) { # create an N-dimensional array from N vectors defining the row-, column, etc names of the array
  DimNames = list(rowname_vec, colname_vec, z_name_vec)
  Dimensions_ = lapply(DimNames, length)
  mx = array(data = fill, dim = Dimensions_, dimnames = DimNames)
  iprint("Dimensions:", dim(mx))
  return(mx)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param printme PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname what
#' @export 
what <- function(x, printme = 0) { # A better version of is(). It can print the first "printme" elements.
  iprint(is(x), "; nr. of elements:", length(x))
  if ( is.numeric(x) )    { iprint("min&max:", range(x) ) } else {print("Not numeric")}
  if ( length(dim(x) ) > 0 )  { iprint("Dim:", dim(x) ) }
  if ( printme > 0)       { iprint("Elements:", x[0:printme] ) }
  head(x)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param any_object PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname idim
#' @export 
idim <- function(any_object) { # A dim() function that can handle if you pass on a vector: then, it gives the length.
  if (is.null(dim(any_object))) {
    if (is.list(any_object)) { print("list") } #if
    print(length(any_object))
  }
  else { print(dim(any_object))  }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param any_object PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname idimnames
#' @export 
idimnames <- function(any_object) { # A dimnames() function that can handle if you pass on a vector: it gives back the names.
  if (!is.null(dimnames(any_object)))   { print(dimnames(any_object)) }
  else if (!is.null(colnames(any_object))) { iprint("colnames:", colnames(any_object))  }
  else if (!is.null(rownames(any_object))) { iprint("rownames:", rownames(any_object))  }
  else if (!is.null(names(any_object))) { iprint("names:", names(any_object)) }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vector PARAM_DESCRIPTION
#' @param categories_vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname table_fixed_categories
#' @export 
table_fixed_categories <- function(vector, categories_vec) { # generate a table() with a fixed set of categories. It fills up the table with missing categories, that are relevant when comparing to other vectors.
  if ( !is.vector(vector)) {print(is(vector[]))}
  table(factor(unlist(vector), levels = categories_vec))
}

## Vector operations -------------------------------------------------------------------------------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param ignore.case PARAM_DESCRIPTION, Default: FALSE
#' @param perl PARAM_DESCRIPTION, Default: FALSE
#' @param value PARAM_DESCRIPTION, Default: FALSE
#' @param fixed PARAM_DESCRIPTION, Default: FALSE
#' @param useBytes PARAM_DESCRIPTION, Default: FALSE
#' @param invert PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname grepv
#' @export 
grepv <- function(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE  # grep returning the value
                  , invert = FALSE, ...) grep(pattern, x, ignore.case = ignore.case, perl = perl, fixed = fixed
                                              , useBytes = useBytes, invert = invert, ..., value = TRUE)


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @param topN PARAM_DESCRIPTION, Default: 10
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname most_frequent_elements
#' @export 
most_frequent_elements <- function(vec, topN = 10) { # Show the most frequent elements of a table
  tail(sort(table(vec, useNA = "ifany")), topN)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION, Default: 3
#' @param top PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname top_indices
#' @export 
top_indices <- function(x, n = 3, top = TRUE) { # Returns the position / index of the n highest values. For equal values, it maintains the original order
  head( order(x, decreasing = top), n )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @param N PARAM_DESCRIPTION, Default: 10
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname trail
#' @export 
trail <- function(vec, N = 10) c(head(vec, n = N), tail(vec, n = N) ) # A combination of head() and tail() to see both ends.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sort.decreasing
#' @export 
sort.decreasing <- function(vec) sort(vec, decreasing = TRUE) # Sort in decreasing order.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param string PARAM_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION, Default: '_'
#' @param n PARAM_DESCRIPTION, Default: 2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_split}}
#' @rdname sstrsplit
#' @export 
#' @importFrom stringr str_split_fixed
sstrsplit <- function(string, pattern = "_", n = 2) { stringr::str_split_fixed(string, pattern = pattern, n = n) } # Alias for str_split_fixed in the stringr package

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df_Col PARAM_DESCRIPTION, Default: as.named.vector(df[, 1, drop = FALSE])
#' @param n PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname topN.dfCol
#' @export 
topN.dfCol <- function(df_Col = as.named.vector(df[ , 1, drop = FALSE]), n = 5)   { head(sort(df_Col, decreasing = TRUE), n = n) } # Find the n highest values in a named vector
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df_Col PARAM_DESCRIPTION, Default: as.named.vector(df[, 1, drop = FALSE])
#' @param n PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname bottomN.dfCol
#' @export 
bottomN.dfCol <- function(df_Col = as.named.vector(df[ , 1, drop = FALSE]), n = 5) { head(sort(df_Col, decreasing = FALSE), n = n) } # Find the n lowest values in a named vector


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df_col PARAM_DESCRIPTION
#' @param WhichDimNames PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.named.vector
#' @export 
as.named.vector <- function(df_col, WhichDimNames = 1) { # Convert a dataframe column or row into a vector, keeping the corresponding dimension name.
  # use RowNames: WhichDimNames = 1 , 2: use ColNames
  # !!! might require drop = FALSE in subsetting!!! eg: df_col[, 3, drop = FALSE]
  # df_col[which(unlist(lapply(df_col, is.null)))] = "NULL" # replace NULLs - they would fall out of vectors - DOES not work yet
  namez = dimnames(df_col)[[WhichDimNames]]
  if (is.list(df_col) & !is.data.frame(df_col)) {namez = names(df_col)}
  vecc = as.vector(unlist(df_col))
  names(vecc) = namez
  return(vecc)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname col2named.vector
#' @export 
col2named.vector <- function(df_col) { # Convert a dataframe column into a vector, keeping the corresponding dimension name.
  namez = rownames(df_col)
  vecc = as.vector(unlist(df_col))
  names(vecc) = namez
  return(vecc)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df_row PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname row2named.vector
#' @export 
row2named.vector <- function(df_row) { # Convert a dataframe row into a vector, keeping the corresponding dimension name.
  namez = colnames(df_row)
  vecc = as.vector(unlist(df_row))
  names(vecc) = namez
  return(vecc)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param tbl PARAM_DESCRIPTION, Default: dplyr::tibble(key = sample(x = 1:5, size = 20, replace = T), 
#'    value = rnorm(20))
#' @param idx PARAM_DESCRIPTION, Default: c(key = 1, value = 2)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{reexports}}
#' @rdname tibble_summary_to_named_vec
#' @export 
#' @importFrom dplyr tibble
tibble_summary_to_named_vec <- function(tbl =  dplyr::tibble('key' = sample(x = 1:5, size = 20, replace = T), 'value' = rnorm(20) )
                                        ,  idx = c(key =1, value = 2)) { # Convert a key-value tibble into a named vector (as opposed to using rownames).
  iprint("The following name and value columns are taken:",colnames(tbl[idx]), "; with indices:", idx)
  tbl_2_col <- tbl[,idx]
  named.vec <- tbl_2_col[[2]]
  names(named.vec) <- tbl_2_col[[1]]
  return(named.vec)
}
# tibble_summary_to_named_vec()


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec.w.names PARAM_DESCRIPTION, Default: c(a = 1, b = 2)
#' @param transpose PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as_tibble_from_named_vec
#' @export 
as_tibble_from_named_vec <- function(vec.w.names =  c("a" = 1, "b" = 2), transpose = T) { # Convert a vector with names into a tibble, keeping the names as rownames.
  stopif(is_null(names(vec.w.names)))
  tbl <- bind_rows(vec.w.names)
  if (transpose) t(tbl) else tbl
}
# as_tibble_from_named_vec()


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.numeric.wNames
#' @export 
as.numeric.wNames <- function(vec) { # Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
  numerified_vec = as.numeric(as.factor(vec)) - 1 # as factor gives numbers [1:n] instead [0:n]
  if (!is.null(names(vec))) {names(numerified_vec) = names(vec)}
  return(numerified_vec)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.numeric.wNames.old
#' @export 
as.numeric.wNames.old <- function(vec) { # Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
  numerified_vec = as.numeric(as.factor(vec))
  if (!is.null(names(vec))) {names(numerified_vec) = names(vec)}
  return(numerified_vec)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.character.wNames
#' @export 
as.character.wNames <- function(vec) { # Converts your input vector into a character vector, and puts the original character values into the names of the new vector, unless it already has names.
  char_vec = as.character(vec)
  if (!is.null(names(vec))) {names(char_vec) = names(vec)}
  return(char_vec)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @param from PARAM_DESCRIPTION, Default: 0
#' @param upto PARAM_DESCRIPTION, Default: 100
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rescale
#' @export 
rescale <- function(vec, from = 0, upto = 100) { # linear transformation to a given range of values
  vec = vec - min(vec, na.rm = TRUE)
  vec = vec * ((upto - from)/max(vec, na.rm = TRUE))
  vec = vec + from
  return(vec)
} # fun

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param named_vector PARAM_DESCRIPTION
#' @param NumericNames PARAM_DESCRIPTION, Default: FALSE
#' @param silent PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname flip_value2name
#' @export 
flip_value2name <- function(named_vector, NumericNames = FALSE, silent = F) { # Flip the values and the names of a vector with names
  if (!is.null(names(named_vector))) {
    newvec = names(named_vector)
    if (NumericNames) { newvec = as.numeric(names(named_vector))     }
    names(newvec) = named_vector
  } else {llprint("Vector without names!", head(named_vector))}
  if (!silent) {
    if (any(duplicated(named_vector))) {iprint("New names contain duplicated elements", head(named_vector[which(duplicated(named_vector))])) }
    if (any(duplicated(newvec))) {iprint("Old names contained duplicated elements", head(newvec[which(duplicated(newvec))])) }
  }
  return(newvec)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param named_vector PARAM_DESCRIPTION
#' @param NumericNames PARAM_DESCRIPTION, Default: FALSE
#' @param silent PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname value2name_flip
#' @export 
value2name_flip = flip_value2name

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec_or_list PARAM_DESCRIPTION
#' @param decreasing PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[gtools]{mixedsort}}
#' @rdname sortbyitsnames
#' @export 
#' @importFrom gtools mixedsort
sortbyitsnames <- function(vec_or_list, decreasing = FALSE, ...) { # Sort a vector by the alphanumeric order of its names(instead of its values).
  xx = names(vec_or_list)
  names(xx) = 1:length(vec_or_list)
  order = as.numeric(names(gtools::mixedsort(xx, decreasing = decreasing, ...)))
  vec_or_list[order]
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @param summarize PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname any.duplicated
#' @export 
any.duplicated <- function(vec, summarize = TRUE) { # How many entries are duplicated
  y = sum(duplicated(vec))
  if (summarize & y) {
    x = table(vec); x = x[x > 1] - 1;
    print("The following elements have  > 1 extra copies:")
    print(x) # table formatting requires a separate entry
  }
  return(y)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @param orig PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname which.duplicated
#' @export 
which.duplicated <- function(vec, orig = F) { # which values are duplicated?
  DPL = vec[which(duplicated(vec))]; iprint(length(DPL), "Duplicated entries: ", DPL)
  # for (i in DPL ) {   print(grepv(i,orig)) } #for
  return(DPL)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @param orig PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname which.NA
#' @export 
which.NA <- function(vec, orig = F) { # which values are NA?
  NANs = vec[which(is.na(vec))]; iprint(length(NANs), "NaN entries: ", NANs)
  NAs = vec[which(is.na(vec))]; iprint(length(NAs), "NA entries: ", NAs, "(only NA-s are returned)")
  # for (i in DPL ) {   print(grepv(i,orig)) } #for
  return(NAs)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param len PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pad.na
#' @export 
pad.na <- function(x, len) { c(x, rep(NA, len - length(x))) } # Fill up with a vector to a given length with NA-values at the end.


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param valz PARAM_DESCRIPTION
#' @param high PARAM_DESCRIPTION, Default: TRUE
#' @param thr PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname clip.values
#' @export 
clip.values <- function(valz, high = TRUE, thr = 3) { # Signal clipping. Cut values above or below a threshold.
  if (high) { valz[valz > thr] = thr
  } else {    valz[valz < thr] = thr }
  valz
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param valz PARAM_DESCRIPTION
#' @param high PARAM_DESCRIPTION, Default: TRUE
#' @param probs PARAM_DESCRIPTION, Default: c(0.01, 0.99)
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param showhist PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname clip.outliers
#' @export 
clip.outliers <- function(valz, high = TRUE, probs = c(.01, .99), na.rm = TRUE, showhist = FALSE, ...) { # Signal clipping based on the input data's distribution. It clips values above or below the extreme N% of the distribution.
  qnt <- quantile(valz, probs = probs, na.rm = na.rm)
  if (showhist) { whist(unlist(valz), breaks = 50 ,vline = qnt, filtercol = -1)} #if
  y <- valz
  y[valz < qnt[1]] <- qnt[1]
  y[valz > qnt[2]] <- qnt[2]
  y
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.logical.wNames
#' @export 
as.logical.wNames <- function(x, ...) { # Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.
  numerified_vec = as.logical(x, ...)
  if (!is.null(names(x))) {names(numerified_vec) = names(x)}
  return(numerified_vec)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param tbl.2col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname col2named.vec.tbl
#' @export 
col2named.vec.tbl <- function(tbl.2col) { # Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.
  nvec = tbl.2col[[2]]
  names(nvec) = tbl.2col[[1]]
  nvec
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param yourvec PARAM_DESCRIPTION
#' @param by PARAM_DESCRIPTION, Default: 9
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname iterBy.over
#' @export 
iterBy.over <- function(yourvec, by = 9) { # Iterate over a vector by every N-th element.
  steps = ceiling(length(yourvec)/by)
  lsX = split(yourvec, sort(rank(yourvec) %% steps))
  names(lsX) = 1:length(lsX)
  lsX
} # for (i in iterBy.over(yourvec = x)) { print(i) }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION, Default: 1:9
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname zigzagger
#' @export 
zigzagger <- function(vec = 1:9) { # mix entries so that they differ
  intermingle2vec(vec, rev(vec))[1:length(vec)]
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION, Default: 1
#' @param y PARAM_DESCRIPTION, Default: 100
#' @param zeropadding PARAM_DESCRIPTION, Default: TRUE
#' @param pad_length PARAM_DESCRIPTION, Default: floor(log10(max(abs(x), abs(y)))) + 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_pad}}
#' @rdname numerate
#' @export 
#' @importFrom stringr str_pad
numerate <- function(x = 1, y = 100, zeropadding = TRUE, pad_length = floor( log10( max(abs(x), abs(y)) ) ) + 1) { # numerate from x to y with additonal zeropadding
  z = x:y
  if (zeropadding) { z = stringr::str_pad(z, pad = 0, width = pad_length)   }
  return(z)
}
# (numerate(1, 122))


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION, Default: rpois(4, lambda = 3)
#' @param topN PARAM_DESCRIPTION, Default: 2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname MaxN
#' @export 
MaxN <- function(vec = rpois(4, lambda = 3), topN = 2) { # find second (third…) highest/lowest value in vector
  topN = topN - 1
  n <- length(vec)
  sort(vec, partial = n - topN)[n - topN]
}
# https://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param numericV PARAM_DESCRIPTION, Default: blanks
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname cumsubtract
#' @export 
cumsubtract <- function(numericV = blanks) { # Cumulative subtraction, opposite of cumsum()
  DiffZ = numericV[-1] - numericV[-length(numericV)]
  print(table(DiffZ))
  DiffZ
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param namedVec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sumBySameName
#' @export 
sumBySameName <- function(namedVec) { # Sum up vector elements with the same name
  # unlapply(splitbyitsnames(namedVec), sum)
  tapply(X = namedVec, INDEX = names(namedVec), sum)
}


### Vector filtering  -------------------------------------------------------------------------------------------------

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param named_Vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname which_names
#' @export 
which_names <- function(named_Vec) { # Return the names where the input vector is TRUE. The input vector is converted to logical.
  return(names(which(as.logical.wNames(named_Vec)))) }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param named_Vec PARAM_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname which_names_grep
#' @export 
which_names_grep <- function(named_Vec, pattern) { # Return the vector elements whose names are partially matched
  idx = grepv(x = names(named_Vec),pattern = pattern)
  return(named_Vec[idx])
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @param silent PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname na.omit.strip
#' @export 
na.omit.strip <- function(vec, silent = FALSE) { # Calls na.omit() and returns a clean vector
  if (is.data.frame(vec)) {
    if (min(dim(vec)) > 1 & silent == FALSE) { iprint(dim(vec), "dimensional array is converted to a vector.") }
    vec = unlist(vec) }
  clean = na.omit(vec)
  attributes(clean)$na.action <- NULL
  return(clean)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname inf.omit
#' @export 
inf.omit <- function(vec) { # Omit infinite values from a vector.
  if (is.data.frame(vec)) {
    if ( min(dim(vec)) > 1 ) { iprint(dim(vec), "dimensional array is converted to a vector.") }
    vec = unlist(vec) }
  clean = vec[is.finite(vec)]
  # attributes(clean)$na.action <- NULL
  return(clean)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname zero.omit
#' @export 
zero.omit <- function(vec) { # Omit zero values from a vector.
  v2 = vec[vec != 0]
  iprint("range: ", range(v2))
  if ( !is.null(names(vec)) ) {names(v2) = names(vec)[vec != 0]}
  return(v2)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param logical_vector PARAM_DESCRIPTION
#' @param percentify PARAM_DESCRIPTION, Default: TRUE
#' @param NumberAndPC PARAM_DESCRIPTION, Default: FALSE
#' @param NArm PARAM_DESCRIPTION, Default: TRUE
#' @param prefix PARAM_DESCRIPTION, Default: NULL
#' @param suffix PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pc_TRUE
#' @export 
pc_TRUE <- function(logical_vector, percentify = TRUE, NumberAndPC = FALSE, NArm = TRUE, prefix = NULL, suffix = NULL) { # Percentage of true values in a logical vector, parsed as text (useful for reports.)
  SUM = sum(logical_vector, na.rm = NArm)
  LEN = length(logical_vector)
  out = SUM / LEN
  if (percentify) {out = percentage_formatter(out) }
  if (NumberAndPC) { out = paste0(out, " or " , SUM, " of ", LEN) }
  if (!is.null(prefix)) {out = paste(prefix, out) }
  if (!is.null(suffix)) {out = paste(out, suffix) }
  return(out)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec_or_table PARAM_DESCRIPTION
#' @param category PARAM_DESCRIPTION
#' @param NA_omit PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pc_in_total_of_match
#' @export 
pc_in_total_of_match <- function(vec_or_table, category, NA_omit = TRUE) { # Percentage of a certain value within a vector or table.
  if (is.table(vec_or_table)) { vec_or_table[category]/sum(vec_or_table, na.rm = NA_omit) }
  else {# if (is.vector(vec_or_table))
    if (NA_omit) {
      if (sum(is.na(vec_or_table))) { vec_or_table = na.omit(vec_or_table); iprint(sum(is.na(vec_or_table)), 'NA are omitted from the vec_or_table of:', length(vec_or_table))}
      "Not wokring complelety : if NaN is stored as string, it does not detect it"
      }
    sum(vec_or_table == category) /  length(vec_or_table)
  } # else: is vector
} # fun

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param length_new PARAM_DESCRIPTION
#' @param length_old PARAM_DESCRIPTION
#' @param prepend PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname filter_survival_length
#' @export 
filter_survival_length <- function(length_new, length_old, prepend = "") { # Parse a sentence reporting the % of filter survival.
  pc = percentage_formatter(length_new/length_old)
  llprint(prepend, pc, " of ", length_old, " entries make through the filter")
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @param probs PARAM_DESCRIPTION, Default: c(0.05, 0.95)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname remove_outliers
#' @export 
remove_outliers <- function(x, na.rm = TRUE, ..., probs = c(.05, .95)) { # Remove values that fall outside the trailing N % of the distribution.
  print("Deprecated. Use clip.outliers()")
  qnt <- quantile(x, probs = probs, na.rm = na.rm, ...)
  # H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  # y[x < (qnt[1] - H)] <- NA ## Add IQR dependence
  # y[x > (qnt[2] + H)] <- NA
  y[x < qnt[1]] <- NA ## Add IQR dependence
  y[x > qnt[2]] <- NA
  y
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param category_vec PARAM_DESCRIPTION
#' @param replaceit PARAM_DESCRIPTION
#' @param to PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname simplify_categories
#' @export 
simplify_categories <- function(category_vec, replaceit , to ) { # Replace every entry that is found in "replaceit", by a single value provided by "to"
  matches = which(category_vec %in% replaceit); iprint(length(matches), "instances of", replaceit, "are replaced by", to)
  category_vec[matches] = to
  return(category_vec)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param needle PARAM_DESCRIPTION
#' @param haystack PARAM_DESCRIPTION
#' @param exact PARAM_DESCRIPTION, Default: TRUE
#' @param report PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lookup
#' @export 
lookup <- function(needle, haystack, exact = TRUE, report = FALSE) { # Awesome pattern matching for a set of values in another set of values. Returns a list with all kinds of results.
  ls_out = as.list( c(ln_needle = length(needle), ln_haystack = length(haystack), ln_hits = "",  hit_poz = "", hits = "") )
  Findings = numeric(0)
  ln_needle = length(needle)
  if (exact) {
    for (i in 1:ln_needle) {      Findings = c(Findings, which(haystack == needle[i]) )    } # for
  } else {
    for (i in 1:ln_needle) {      Findings = c(Findings, grep(needle[i], haystack,  ignore.case = TRUE, perl = FALSE))    } # for
  } # exact or partial match
  ls_out$'hit_poz' = Findings
  ls_out$'ln_hits' = length(Findings)
  ls_out$'hits' = haystack[Findings]
  if (length(Findings)) { ls_out$'nonhits' = haystack[-Findings]
  } else {      ls_out$'nonhits' = haystack }
  if (report) {
    llprint(length(Findings), "/", ln_needle, '(', percentage_formatter(length(Findings)/ln_needle)
            , ") of", substitute(needle), "were found among", length(haystack), substitute(haystack), "." )
    if (length(Findings)) { llprint( substitute(needle), "findings: ", paste( haystack[Findings], sep = " " ) ) }
  } else { iprint(length(Findings), "Hits:", haystack[Findings]) } # if (report)
  return(ls_out)
}




## Matrix operations -------------------------------------------------------------------------------------------------

### Matrix calculations  -------------------------------------------------------------------------------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowMedians
#' @export 
rowMedians <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, median, na.rm = na.rm) # Calculates the median of each row of a numeric matrix / data frame.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colMedians
#' @export 
colMedians <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, median, na.rm = na.rm) # Calculates the median of each column of a numeric matrix / data frame.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowGeoMeans
#' @export 
rowGeoMeans <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, geomean, na.rm = na.rm) # Calculates the median of each row of a numeric matrix / data frame.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colGeoMeans
#' @export 
colGeoMeans <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, geomean, na.rm = na.rm) # Calculates the median of each column of a numeric matrix / data frame.


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowCV
#' @export 
rowCV <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, cv, na.rm = na.rm ) # Calculates the CV of each ROW of a numeric matrix / data frame.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colCV
#' @export 
colCV <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, cv, na.rm = na.rm ) # Calculates the CV of each column of a numeric matrix / data frame.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowVariance
#' @export 
rowVariance <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, var, na.rm = na.rm ) # Calculates the CV of each ROW of a numeric matrix / data frame.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colVariance
#' @export 
colVariance <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, var, na.rm = na.rm ) # Calculates the CV of each column of a numeric matrix / data frame.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowMin
#' @export 
rowMin <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, min, na.rm = na.rm) # Calculates the minimum of each row of a numeric matrix / data frame.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colMin
#' @export 
colMin <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, min, na.rm = na.rm) # Calculates the minimum of each column of a numeric matrix / data frame.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowMax
#' @export 
rowMax <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, max, na.rm = na.rm) # Calculates the maximum of each row of a numeric matrix / data frame.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colMax
#' @export 
colMax <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, max, na.rm = na.rm) # Calculates the maximum of each column of a numeric matrix / data frame.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowSEM
#' @export 
rowSEM <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, sem, na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colSEM
#' @export 
colSEM <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, sem, na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowSD
#' @export 
rowSD <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, sd, na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colSD
#' @export 
colSD <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, sd, na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowIQR
#' @export 
rowIQR <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, IQR, na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colIQR
#' @export 
colIQR <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, IQR, na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowquantile
#' @export 
rowquantile <- function(x, na.rm = TRUE, ...) apply(data.matrix(x), 1, quantile, ..., na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colquantile
#' @export 
colquantile <- function(x, na.rm = TRUE, ...) apply(data.matrix(x), 2, quantile, ..., na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colDivide
#' @export 
colDivide <- function(mat, vec) { # divide by column # See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
  stopifnot(NCOL(mat) == length(vec))
  mat / vec[col(mat)] # fastest
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colMutliply
#' @export 
colMutliply <- function(mat, vec) { # Mutliply by column # See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
  stopifnot(NCOL(mat) == length(vec))
  mat * vec[col(mat)] # fastest
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowDivide
#' @export 
rowDivide <- function(mat, vec) { # divide by row
  stopifnot(NROW(mat) == length(vec))
  mat / vec[row(mat)] # fastest
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowMutliply
#' @export 
rowMutliply <- function(mat, vec) { # Mutliply by row
  stopifnot(NROW(mat) == length(vec))
  mat * vec[row(mat)] # fastest
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param DF PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname row.Zscore
#' @export 
row.Zscore <- function(DF) t(scale(t(DF))) # Calculate Z-score over rows of data frame.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @param SUM PARAM_DESCRIPTION, Default: 1e+06
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname TPM_normalize
#' @export 
TPM_normalize <- function(mat, SUM = 1e6) { # normalize each column to 1 million
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * SUM
  return(norm_mat)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname median_normalize
#' @export 
median_normalize <- function(mat) { # normalize each column to the median of all the column-sums
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * median(cs)
  iprint("colMedians: ", head(signif(colMedians(norm_mat), digits = 3)))
  return(norm_mat)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mean_normalize
#' @export 
mean_normalize <- function(mat) { # normalize each column to the median of the columns
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * mean(cs)
  iprint("colMeans: ", head(signif(colMeans(norm_mat))))
  return(norm_mat)
}


## Matrix manipulations -------------------------------------------------------------------------------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param clockwise PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rotate
#' @export 
rotate <- function(x, clockwise = TRUE) { # rotate a matrix 90 degrees.
  if (clockwise) { t( apply(x, 2, rev))  #first reverse, then transpose, it's the same as rotate 90 degrees
  } else {apply( t(x), 2, rev)}  #first transpose, then reverse, it's the same as rotate -90 degrees:
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sortEachColumn
#' @export 
sortEachColumn <- function(data, ...) sapply(data, sort, ...) # Sort each column of a numeric matrix / data frame.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param colname_in_df PARAM_DESCRIPTION, Default: 1
#' @param decrease PARAM_DESCRIPTION, Default: FALSE
#' @param na_last PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sort.mat
#' @export 
sort.mat <- function(df, colname_in_df = 1, decrease = FALSE, na_last = TRUE) { # Sort a matrix. ALTERNATIVE: dd[with(dd, order(-z, b)), ]. Source: https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r
  if (length(colname_in_df) > 1) { print("cannot handle multi column sort") }
  else {df[ order(df[, colname_in_df], decreasing = decrease, na.last = na_last), ]}
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat_w_dimnames PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowNameMatrix
#' @export 
rowNameMatrix <- function(mat_w_dimnames) { # Create a copy of your matrix, where every entry is replaced by the corresponding row name. Useful if you want to color by row name in a plot (where you have different number of NA-values in each row).
  matrix(rep(rownames(mat_w_dimnames), ncol(mat_w_dimnames) ), nrow = nrow(mat_w_dimnames), ncol = ncol(mat_w_dimnames))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat_w_dimnames PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colNameMatrix
#' @export 
colNameMatrix <- function(mat_w_dimnames) { # Create a copy of your matrix, where every entry is replaced by the corresponding column name. Useful if you want to color by column name in a plot (where you have different number of NA-values in each column).
  x = rep(colnames(mat_w_dimnames), nrow(mat_w_dimnames) )
  t(matrix(x, nrow = ncol(mat_w_dimnames), ncol = nrow(mat_w_dimnames)))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param matrix1 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rownames.trimws
#' @export 
rownames.trimws <- function(matrix1) { # trim whitespaces from the rownames
  rownames(matrix1) = trimws(rownames(matrix1))
  return(matrix1)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION, Default: colnames(df)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colsplit
#' @export 
colsplit <- function(df, f = colnames(df)) { # split a data frame by a factor corresponding to columns.
  ListOfDFs = NULL
  levelz = unique(f)
  for (i in 1:length(levelz)) {   ListOfDFs[[i]] = df[ , which(f == levelz[i]) ]  }
  names(ListOfDFs) = levelz
  return(ListOfDFs)
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION, Default: colnames(df)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname splitByCol
#' @export 
splitByCol = colsplit

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION, Default: rownames(df)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowsplit
#' @export 
rowsplit <- function(df, f = rownames(df)) { # split a data frame by a factor corresponding to columns.
  ListOfDFs = NULL
  levelz = unique(f)
  for (i in 1:length(levelz)) {   ListOfDFs[[i]] = df[ which(f == levelz[i]), ]  }
  names(ListOfDFs) = levelz
  return(ListOfDFs)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param RowIDs PARAM_DESCRIPTION, Default: NULL
#' @param ColIDs PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname select.rows.and.columns
#' @export 
select.rows.and.columns <- function(df, RowIDs = NULL, ColIDs = NULL ) { # Subset rows and columns. It checks if the selected dimension names exist and reports if any of those they aren't found.
  if (length(RowIDs)) {
    true_rownames = intersect(rownames(df), RowIDs)
    NotFound = setdiff(RowIDs, rownames(df))
    if (length(NotFound)) { iprint(length(NotFound), "Row IDs Not Found:", head(NotFound), "...     Rows found:", length(true_rownames)) } else {iprint("All row IDs found")} #if
    df = df[ true_rownames, ]
  } #if
  if (length(ColIDs)) {
    true_colnames = intersect(colnames(df), ColIDs)
    NotFound = setdiff(ColIDs, colnames(df))
    if (length(NotFound)) { iprint(length(NotFound), "Column IDs Not Found:", head(NotFound), "...     Rows found:", length(true_colnames)) } else {iprint("All column IDs found")}
    df = df[ , true_colnames ]
  } #if
  iprint(dim(df))
  return(df)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @param rownamez PARAM_DESCRIPTION
#' @param silent PARAM_DESCRIPTION, Default: FALSE
#' @param removeNAonly PARAM_DESCRIPTION, Default: FALSE
#' @param remove0only PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname getRows
#' @export 
getRows <- function(mat, rownamez, silent = FALSE, removeNAonly = FALSE, remove0only = FALSE ) { # Get the subset of rows with existing rownames, report how much it could not find.
  idx = intersect(rownamez, row.names(mat))
  if (removeNAonly) { idx = which_names(rowSums(!is.na(mat[ idx, ]), na.rm = TRUE) > 0) }
  if (remove0only) { idx = which_names(rowSums(mx != 0, na.rm = TRUE) > 0) }
  if (!silent) { iprint(length(idx), "/", length(rownamez), "are found. Missing: ", length(setdiff(row.names(mat), rownamez))  ) }
  mat[ idx, ]
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @param colnamez PARAM_DESCRIPTION
#' @param silent PARAM_DESCRIPTION, Default: FALSE
#' @param removeNAonly PARAM_DESCRIPTION, Default: FALSE
#' @param remove0only PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname getCols
#' @export 
getCols <- function(mat, colnamez, silent = FALSE, removeNAonly = FALSE, remove0only = FALSE ) { # Get the subset of cols with existing colnames, report how much it could not find.
  idx = intersect(colnamez, colnames(mat))
  print(symdiff(colnamez, colnames(mat)))
  if (removeNAonly) {   idx = which_names(colSums(!is.na(mat[ ,idx ]), na.rm = TRUE) > 0) }
  if (remove0only) { idx = which_names(colSums(mx != 0, na.rm = TRUE) > 0) }
  if (!silent) { iprint(length(idx), "/", length(colnamez), "are found. Missing: ", length(setdiff(colnames(mat), colnamez))  ) }
  mat[ ,idx ]
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df_ PARAM_DESCRIPTION, Default: NULL
#' @param rows PARAM_DESCRIPTION, Default: FALSE
#' @param odd PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get.oddoreven
#' @export 
get.oddoreven <- function(df_ = NULL, rows = FALSE, odd = TRUE) { # Get odd or even columns or rows of a data frame
  counter = if (rows) NROW(df_) else NCOL(df_)
  IDX = if (odd) seq(1, to = counter, by = 2) else seq(2, to = counter, by = 2)
  df_out = if (rows) df_[IDX, ] else df_[, IDX]
  return(df_out)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param matrix1 PARAM_DESCRIPTION
#' @param matrix2 PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname combine.matrices.intersect
#' @export 
combine.matrices.intersect <- function(matrix1, matrix2, k = 2) { # combine matrices by rownames intersect
  rn1 = rownames(matrix1); rn2 = rownames(matrix2);
  idx = intersect(rn1, rn2)
  llprint(length(idx), "out of", substitute(matrix1), length(rn1), "and", length(rn2), substitute(matrix2), "rownames are merged")
  merged = cbind(matrix1[idx, ], matrix2[idx, ])
  diffz = symdiff(rn1, rn2)
  print("Missing Rows 1, 2")
  x1 = rowSums( matrix1[diffz[[1]], ] )
  x2 = rowSums( matrix2[diffz[[2]], ] ); print("")
  iprint("Values lost 1: ", round(sum(x1)), "or", percentage_formatter(sum(x1)/sum(merged)))
  print(tail(sort(x1), n = 10));print("")
  iprint("Values lost 2: ", round(sum(x2)), "or", percentage_formatter(sum(x2)/sum(merged)))
  print(tail(sort(x2), n = 10))
  iprint("dim:", dim(merged)); return(merged)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param list_of_dfs PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[plyr]{join_all}}
#' @rdname merge_dfs_by_rn
#' @export 
#' @importFrom plyr join_all
merge_dfs_by_rn <- function(list_of_dfs) { # Merge any data frames by rownames. Required plyr package
  for (i in names(list_of_dfs) ) { colnames(list_of_dfs[[i]]) <- paste0(i,'.',colnames(list_of_dfs[[i]])) } # make unique column names
  for (i in names(list_of_dfs) ) { list_of_dfs[[i]]$rn <- rownames(list_of_dfs[[i]]) } #for
  COMBINED <- plyr::join_all(list_of_dfs, by = 'rn', type = 'full');   idim(COMBINED)
  rownames(COMBINED) = COMBINED$rn
  COMBINED$rn = NULL
  return(COMBINED)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname merge_numeric_df_by_rn
#' @export 
merge_numeric_df_by_rn <- function(x, y) { # Merge 2 numeric data frames by rownames
  rn1 = rownames(x); rn2 = rownames(y);
  diffz = symdiff(rn1, rn2)
  merged = merge(x , y, by = "row.names", all = TRUE)  # merge by row names(by = 0 or by = "row.names")
  rownames(merged) = merged$Row.names
  merged = merged[ , -1] # remove row names
  merged[is.na(merged)] <- 0

  print("Uniq Rows (top 10 by sum)")
  x1 = rowSums( x[diffz[[1]], ] )
  x2 = rowSums( y[diffz[[2]], ] ); print("")
  iprint("Values specific to 1: ", round(sum(x1)), "or", percentage_formatter(sum(x1)/sum(merged)))
  print(tail(sort(x1), n = 10));print("")
  iprint("Values specific to 2: ", round(sum(x2)), "or", percentage_formatter(sum(x2)/sum(merged)))
  print(tail(sort(x2), n = 10))
  iprint("Dimensions of merged DF:", dim(merged))

  return(merged)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @param cols PARAM_DESCRIPTION, Default: 1:NCOL(mat)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname remove.na.rows
#' @export 
remove.na.rows <- function(mat, cols = 1:NCOL(mat)) { # cols have to be a vector of numbers corresponding to columns
  mat2 = mat[ , cols]
  idxOK = which(rowSums(!apply(mat2, 2, is.na)) == NCOL(mat)  )
  mat[idxOK, ]
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname remove.na.cols
#' @export 
remove.na.cols <- function(mat) { # cols have to be a vector of numbers corresponding to columns
  idxOK = !is.na(colSums(mat))
  return(mat[, idxOK])
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mat PARAM_DESCRIPTION
#' @param any PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname na.omit.mat
#' @export 
na.omit.mat <- function(mat, any = TRUE) { # Omit rows with NA values from a matrix. Rows with any, or full of NA-s
  mat = as.matrix(mat)
  stopifnot(length(dim(mat)) == 2)
  if (any) outMat = mat[ !is.na(rowSums(mat)), ]
  else outMat = mat[ (rowSums(is.na(mat)) <= ncol(mat)), ] # keep rows not full with NA
  outMat
}


# Multi-dimensional lists ----------------------------------------------------------------

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param list.1D PARAM_DESCRIPTION
#' @param obj.2D PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname copy.dimension.and.dimnames
#' @export 
copy.dimension.and.dimnames <- function(list.1D, obj.2D) { # copy dimension and dimnames
  dim(list.1D) <- dim(obj.2D)
  dimnames(list.1D) <- dimnames(obj.2D)
  list.1D
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param list_2D PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mdlapply
#' @export 
mdlapply <- function(list_2D, ...) { #  lapply for multidimensional arrays
  x = lapply(list_2D, ...)
  copy.dimension.and.dimnames(x,list_2D)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param two.dim.arr.of.lists PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname arr.of.lists.2.df
#' @export 
arr.of.lists.2.df <- function(two.dim.arr.of.lists) { # simplify 2D-list-array to a DF
  list.1D = unlist(two.dim.arr.of.lists)
  dim(list.1D) <- dim(two.dim.arr.of.lists)
  dimnames(list.1D) <- dimnames(two.dim.arr.of.lists)
  list.1D
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param list_2D PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mdlapply2df
#' @export 
mdlapply2df <- function(list_2D, ...) { # multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF)
  x = lapply(list_2D, ...)
  z = copy.dimension.and.dimnames(x,list_2D)
  arr.of.lists.2.df(z)
}



# List operations -------------------------------------------------------------------------------------------------
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ls PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname any.duplicated.rownames.ls.of.df
#' @export 
any.duplicated.rownames.ls.of.df <- function(ls) any.duplicated(rownames(ls)) # Check if there are any duplocated rownames in a list of dataframes.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ls PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname intersect.ls
#' @export 
intersect.ls <- function(ls, ...) { Reduce(intersect, ls) } # Intersect any number of lists.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ls PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname union.ls
#' @export 
union.ls <- function(ls, ...) { sort(unique(do.call(c,ls))) } # Intersect any number of list elements. Faster than reduce.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname unlapply
#' @export 
unlapply <- function(...) { unlist(lapply(...)) } # lapply, then unlist

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list.wNames
#' @export 
list.wNames <- function(...) { # create a list with names from ALL variables you pass on to the function
  lst = list(...)
  names(lst) = as.character(match.call()[-1])
  return(lst)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dtf PARAM_DESCRIPTION
#' @param na.omit PARAM_DESCRIPTION, Default: TRUE
#' @param zero.omit PARAM_DESCRIPTION, Default: FALSE
#' @param omit.empty PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.list.df.by.row
#' @export 
as.list.df.by.row <- function(dtf, na.omit = TRUE, zero.omit = FALSE, omit.empty = FALSE) { # Split a dataframe into a list by its columns. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.
  outList = as.list(as.data.frame(t( dtf ) ) )
  if (na.omit) {   outList = lapply(outList, na.omit.strip) }
  if (zero.omit) {   outList = lapply(outList, zero.omit) }
  if (omit.empty) {   outList = outList[(lapply(outList, length)) > 0] }
  print(str(outList, vec.len = 2))
  return(outList)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dtf PARAM_DESCRIPTION
#' @param na.omit PARAM_DESCRIPTION, Default: TRUE
#' @param zero.omit PARAM_DESCRIPTION, Default: FALSE
#' @param omit.empty PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.list.df.by.col
#' @export 
as.list.df.by.col <- function(dtf, na.omit = TRUE, zero.omit = FALSE, omit.empty = FALSE) { # oSplit a dataframe into a list by its rows. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.
  outList = as.list(dtf)
  if (na.omit) {   outList = lapply(outList, na.omit.strip) }
  if (zero.omit) {   outList = lapply(outList, zero.omit) }
  if (omit.empty) {   outList = outList[(lapply(outList, length)) > 0] }
  print(str(outList, vec.len = 2))
  return(outList)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param L PARAM_DESCRIPTION
#' @param namesOrdered PARAM_DESCRIPTION, Default: mixedsort(names(L))
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname reorder.list
#' @export 
reorder.list <- function(L, namesOrdered = mixedsort(names(L))) { # reorder elements of lists in your custom order of names / indices.
  Lout = list(NA)
  for (x in 1:length(namesOrdered)) { Lout[[x]] = L[[namesOrdered[x] ]]  }
  if (length(names(L))) { names(Lout) = namesOrdered }
  return(Lout)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param L PARAM_DESCRIPTION
#' @param namesOrdered PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname range.list
#' @export 
range.list <- function(L, namesOrdered) { # range of values in whole list
  return(range(unlist(L), na.rm = TRUE))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param L1 PARAM_DESCRIPTION
#' @param L2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname intermingle2lists
#' @export 
intermingle2lists <- function(L1, L2) { # Combine 2 lists (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().
  stopifnot(length(L1) == length(L2) )
  Lout = list(NA)
  for (x in 1:(2*length(L1)) ) {
    if (x  %% 2) {  Lout[[x]] = L1[[((x + 1) / 2)]]; names(Lout)[x] = names(L1)[((x + 1) / 2)]
    } else {    Lout[[x]] = L2[[x / 2]]; names(Lout)[x] = names(L2)[x / 2]      }
  } # for
  return(Lout)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @param list_wannabe PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.listalike
#' @export 
as.listalike <- function(vec, list_wannabe) { # convert a vector to a list with certain dimensions, taken from the list it wanna resemble
  stopifnot(length(vec) == length(unlist(list_wannabe)))
  list_return = list_wannabe
  past = 0
  for (v in 1:length(list_wannabe)) {
    lv = length(list_wannabe[[v]])
    list_return[[v]] = vec[(past + 1):(past + lv)]
    past = past + lv
  } # for
  return(list_return)
}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ll PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname reverse.list.hierarchy
#' @export 
reverse.list.hierarchy <- function(ll) { # reverse list hierarchy
  ## https://stackoverflow.com/a/15263737
  nms <- unique(unlist(lapply(ll, function(X) names(X))))
  ll <- lapply(ll, function(X) setNames(X[nms], nms))
  ll <- apply(do.call(rbind, ll), 2, as.list)
  lapply(ll, function(X) X[!sapply(X, is.null)])
}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param your.list PARAM_DESCRIPTION, Default: list(set.1 = vec.fromNames(LETTERS[1:5], fill = 1), set.2 = vec.fromNames(LETTERS[3:9], 
#'    fill = 2))
#' @param byRow PARAM_DESCRIPTION, Default: TRUE
#' @param FILL PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list2fullDF.byNames
#' @export 
list2fullDF.byNames <- function(your.list = list(
  "set.1" = vec.fromNames(LETTERS[1:5], fill = 1),  # Convert a list to a full matrix. Rows = names(union.ls(your_list)) or all names of within list elements, columns = names(your_list).
  "set.2" = vec.fromNames(LETTERS[3:9], fill = 2)
  ), byRow = TRUE, FILL = NA) {
  length.list = length(your.list)
  list.names = names(your.list)
  list.element.names = sort(unique(unlist(lapply(your.list, names))))

  mat = matrix.fromNames(rowname_vec = list.element.names, colname_vec = list.names, fill = FILL)
  for (i in 1:length.list) {
    element = list.names[i]
    mat[ names(your.list[[element]]), element] = your.list[[element]]
  }
  if (!byRow) {mat = t(mat)}
  return(mat)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param your.list PARAM_DESCRIPTION, Default: list(set.1 = LETTERS[1:5], set.2 = LETTERS[3:9])
#' @param byRow PARAM_DESCRIPTION, Default: TRUE
#' @param FILL PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list2fullDF.presence
#' @export 
list2fullDF.presence <- function(your.list = list("set.1" = LETTERS[1:5]  # Convert a list to a full matrix.  Designed for occurence counting, think tof table(). Rows = all ENTRIES of within your list, columns = names(your_list).
                                                , "set.2" = LETTERS[3:9]), byRow = TRUE, FILL = 0) {
  length.list = length(your.list)
  list.names = names(your.list)
  list.elements = sort(Reduce(f = union, your.list))

  mat = matrix.fromNames(rowname_vec = list.elements, colname_vec = list.names, fill = FILL)
  for (i in 1:length.list) {
    element = list.names[i]
    mat[ your.list[[element]], element] = 1
  }
  if (!byRow) {mat = t(mat)}
  return(mat)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param namedVec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname splitbyitsnames
#' @export 
splitbyitsnames <- function(namedVec) { # split a list by its names
stopif(is.null(names(namedVec)), message = "NO NAMES")
split(namedVec, f = names(namedVec))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param namedVec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname splititsnames_byValues
#' @export 
splititsnames_byValues <- function(namedVec) { # split a list by its names
  stopif(is.null(names(namedVec)), message = "NO NAMES")
  split(names(namedVec), f = namedVec)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param V1 PARAM_DESCRIPTION
#' @param V2 PARAM_DESCRIPTION
#' @param wNames PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname intermingle2vec
#' @export 
intermingle2vec <- function(V1, V2, wNames = TRUE) { # Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.
  stopifnot(length(V1) == length(V2) )
  Vout = c(rbind(V1, V2))
  if (wNames) {names(Vout) = c(rbind(names(V1), names(V2)))}
  return(Vout)
}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df1 PARAM_DESCRIPTION
#' @param df2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname intermingle.cbind
#' @export 
intermingle.cbind <- function(df1, df2) { # Combine 2 data frames (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().
  stopifnot(ncol(df1) == ncol(df2) )
  if (nrow(df1) != nrow(df2) ) { # not equal rows: subset
    print(symdiff(rownames(df2), rownames(df1)))
    CommonGenes = intersect(rownames(df2), rownames(df1)); print(length(CommonGenes))
    df1 = df1[CommonGenes, ]
    df2 = df2[CommonGenes, ]
  } else { CommonGenes = rownames(df1) }

  # Create New column names
  if (length(colnames(df1)) == ncol(df1) & length(colnames(df2)) == ncol(df2) ) {
    NewColNames = intermingle2vec(paste0("df1.", colnames(df1) ), paste0("df2.", colnames(df2) ))
  } else {
    NewColNames = intermingle2vec(paste0("df1.", 1:ncol(df1) ), paste0("df2.", 1:ncol(df2) ))
  }
  NewMatr = matrix.fromNames(rowname_vec = CommonGenes, colname_vec = NewColNames)
  for (x in 1:(2*length(df1)) ) {
    if (x  %% 2) {  NewMatr[ , x ] = df1[ , (x + 1)/2 ]
    } else {        NewMatr[ , x ] = df2[ , (x)/2 ]      }
  } # for
  print(idim(NewMatr))
  return(NewMatr)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param your_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ls2categvec
#' @export 
ls2categvec <- function(your_list ) { # Convert a list to a vector repeating list-element names, while vector names are the list elements
  VEC = rep(names(your_list),unlapply(your_list, length))
  names(VEC) = unlist(your_list, use.names = TRUE)
  return(VEC)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ListWithNames PARAM_DESCRIPTION, Default: Sections.ls.Final
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list.2.replicated.name.vec
#' @export 
list.2.replicated.name.vec <- function(ListWithNames = Sections.ls.Final) { # Convert a list to a vector, with list elements names replicated as many times, as many elements each element had.
  NZ = names(ListWithNames)
  LZ = unlapply(ListWithNames, length)
  replicated.name.vec = rep(NZ, LZ)
  names(replicated.name.vec) = unlist(ListWithNames)
  return(replicated.name.vec)
}

## Set operations -------------------------------------------------------------------------------------------------

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname symdiff
#' @export 
symdiff <- function(x, y, ...) { # Quasy symmetric difference of any number of vectors
  big.vec <- c(x, y, ...)
  ls = list(x, y, ...); if ( length(ls) > 2) {print("# Not Mathematically correct, but logical for n>2 vectors: https://en.wikipedia.org/wiki/Symmetric_difference#Properties")}
  names(ls) = paste("Only in", as.character(match.call()[-1]))
  duplicates <- big.vec[duplicated(big.vec)]
  lapply(ls, function(x) setdiff(x, duplicates))
}



## Math & stats -------------------------------------------------------------------------------------------------


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname cv
#' @export 
cv <- function(x, na.rm = TRUE) {
  sd( x, na.rm = na.rm)/mean(x, na.rm = na.rm)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sem
#' @export 
sem <- function(x, na.rm = TRUE) sd(unlist(x), na.rm = na.rm)/sqrt(length(na.omit.strip(as.numeric(x))))  # Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default)

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param USE PARAM_DESCRIPTION, Default: 'na.or.complete'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fano
#' @export 
fano <- function(x, na.rm = TRUE, USE = "na.or.complete") var(x, na.rm = na.rm, use = USE )/mean(x, na.rm = na.rm) # Calculates the fano factor on a numeric vector (it excludes NA-s by default)

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname geomean
#' @export 
geomean <- function(x, na.rm = TRUE) { # Calculates the geometric mean of a numeric vector (it excludes NA-s by default)
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x)) }
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname gm_mean
#' @export 
gm_mean = geomean

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 2
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mean_of_log
#' @export 
mean_of_log <- function(x, k = 2, na.rm = TRUE) { # Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default)
  negs = sum(x < 0);  zeros = sum(x == 0)
  if (negs | zeros) { iprint("The input vector has", negs, "negative values and", zeros, "zeros." ) }
  mean(log(x, base = k), na.rm = na.rm) }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param oneSide PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname movingAve
#' @export 
movingAve <- function(x, oneSide = 5) { # Calculates the moving / rolling average of a numeric vector.
  y = NULL
  for (i in oneSide:length(x)) {
    y[i] = mean( x[ (i - oneSide):(i + oneSide) ] )
  };  return(y)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname movingAve2
#' @export 
movingAve2 <- function(x,n = 5) {filter(x,rep(1/n,n), sides = 2)} # Calculates the moving / rolling average of a numeric vector, using filter().

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param oneSide PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname movingSEM
#' @export 
movingSEM <- function(x, oneSide = 5) { # Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.
  y = NULL
  for (i in oneSide:length(x)) {
    y[i] = sem( x[ (i - oneSide):(i + oneSide) ] )
  };  return(y)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param oneSide PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname imovingSEM
#' @export 
imovingSEM <- function(x, oneSide = 5) { # Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller window-size.
  y = NULL
  for (i in 1:length(x)) {
    oneSideDynamic = min(i - 1, oneSide, length(x) - i); oneSideDynamic
    indexx = (i - oneSideDynamic):(i + oneSideDynamic);indexx
    y[i] = sem( x[ indexx ] )
  };  return(y)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param p PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname shannon.entropy
#' @export 
shannon.entropy <- function(p) { # Calculate shannon entropy
  if (min(p) < 0 || sum(p) <= 0) return(NA)
  p.norm <- p[p > 0]/sum(p) - sum(log2(p.norm)*p.norm)
}



