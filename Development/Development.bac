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



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title vec.fromNames
#' @description Create a vector from a vector of names.
#' @param name_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # vec.fromNames()
#'  }
#' }
#' @export
vec.fromNames <- function(name_vec = LETTERS[1:5], fill = NA) { # Create a vector from a vector of names.
  v = numeric(length(name_vec))
  if (length(fill) == 1) {v = rep(fill, length(name_vec))}
  else if (length(fill == length(name_vec))) {v = fill}
  names(v) = name_vec
  return(v)
}




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title list.fromNames
#' @description create list from a vector with the names of the elements.
#' @param name_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NaN
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # list.fromNames()
#'  }
#' }
#' @export
list.fromNames <- function(name_vec = LETTERS[1:5], fill = NaN) { # create list from a vector with the names of the elements.
  liszt = as.list(rep(fill, length(name_vec)))
  names(liszt) = name_vec
  return(liszt)
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title matrix.fromNames
#' @description Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.
#' @param rowname_vec PARAM_DESCRIPTION, Default: 1:10
#' @param colname_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # matrix.fromNames()
#'  }
#' }
#' @export
matrix.fromNames <- function(rowname_vec = 1:10, colname_vec = LETTERS[1:5], fill = NA) { # Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.
  mx = matrix(data = fill, nrow = length(rowname_vec), ncol = length(colname_vec), dimnames = list(rowname_vec, colname_vec))
  iprint("Dimensions:", dim(mx))
  return(mx)
}



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title matrix.fromVector
#' @description Create a matrix from values in a vector repeated for each column / each row. Similar to rowNameMatrix and colNameMatrix.
#' @param vector Input vector, Default: 1:5
#' @param HowManyTimes PARAM_DESCRIPTION, Default: 3
#' @param IsItARow PARAM_DESCRIPTION, Default: TRUE
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # matrix.fromVector()
#'  }
#' }
#' @export
matrix.fromVector <- function(vector = 1:5, HowManyTimes = 3, IsItARow = TRUE) { # Create a matrix from values in a vector repeated for each column / each row. Similar to rowNameMatrix and colNameMatrix.
  matt = matrix(vector, nrow = length(vector), ncol = HowManyTimes)
  if ( !IsItARow ) {matt = t(matt)}
  return(matt)
}



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title array.fromNames
#' @description Create an N-dimensional array from N vectors defining the row-, column, etc names of the array.
#' @param rowname_vec PARAM_DESCRIPTION, Default: 1:3
#' @param colname_vec PARAM_DESCRIPTION, Default: letters[1:2]
#' @param z_name_vec PARAM_DESCRIPTION, Default: LETTERS[4:6]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # array.fromNames()
#'  }
#' }
#' @export
array.fromNames <- function(rowname_vec = 1:3, colname_vec = letters[1:2], z_name_vec = LETTERS[4:6], fill = NA) { # Create an N-dimensional array from N vectors defining the row-, column, etc names of the array.
  DimNames = list(rowname_vec, colname_vec, z_name_vec)
  Dimensions_ = lapply(DimNames, length)
  mx = array(data = fill, dim = Dimensions_, dimnames = DimNames)
  iprint("Dimensions:", dim(mx))
  return(mx)
}





# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title what
#' @description A better version of is(). It can print the first "printme" elements.
#' @param x PARAM_DESCRIPTION
#' @param printme PARAM_DESCRIPTION, Default: 0
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # a <- 1:2; what(a)
#'  }
#' }
#' @export
what <- function(x, printme = 0) { # A better version of is(). It can print the first "printme" elements.
  iprint(is(x), "; nr. of elements:", length(x))
  if ( is.numeric(x) )    { iprint("min&max:", range(x) ) } else {print("Not numeric")}
  if ( length(dim(x) ) > 0 )  { iprint("Dim:", dim(x) ) }
  if ( printme > 0)       { iprint("Elements:", x[0:printme] ) }
  head(x)
}




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title idim
#' @description A dim() function that can handle if you pass on a vector: then, it gives the length.
#' @param any_object PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # a <- 1:2; idim(a)
#'  }
#' }
#' @export
idim <- function(any_object) { # A dim() function that can handle if you pass on a vector: then, it gives the length.
  if (is.null(dim(any_object))) {
    if (is.list(any_object)) { print("list") } #if
    print(length(any_object))
  }
  else { print(dim(any_object))  }
}




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title idimnames
#' @description A dimnames() function that can handle if you pass on a vector: it gives back the names.
#' @param any_object PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #a <- 1:2; idimnames(a)
#'  }
#' }
#' @export
idimnames <- function(any_object) { # A dimnames() function that can handle if you pass on a vector: it gives back the names.
  if (!is.null(dimnames(any_object)))   { print(dimnames(any_object)) }
  else if (!is.null(colnames(any_object))) { iprint("colnames:", colnames(any_object))  }
  else if (!is.null(rownames(any_object))) { iprint("rownames:", rownames(any_object))  }
  else if (!is.null(names(any_object))) { iprint("names:", names(any_object)) }
}




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title table_fixed_categories
#' @description Generate a table() with a fixed set of categories. It fills up the table with missing categories, that are relevant when comparing to other vectors.
#' @param vec Input vector
#' @param categories_vec PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # table_fixed_categories(vec = 1:3, categories_vec = 1:4)
#'  }
#' }
#' @export
table_fixed_categories <- function(vec, categories_vec) { # generate a table() with a fixed set of categories. It fills up the table with missing categories, that are relevant when comparing to other vectors.
  if ( !is.vector(vec)) {print(is(vec[]))}
  table(factor(unlist(vec), levels = categories_vec))
}

## Vector operations -------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title grepv
#' @description grep returning the value.
#' @param pattern PARAM_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param ignore.case PARAM_DESCRIPTION, Default: FALSE
#' @param perl PARAM_DESCRIPTION, Default: FALSE
#' @param value PARAM_DESCRIPTION, Default: FALSE
#' @param fixed PARAM_DESCRIPTION, Default: FALSE
#' @param useBytes PARAM_DESCRIPTION, Default: FALSE
#' @param invert PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
grepv <- function(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE  # grep returning the value
                  , invert = FALSE, ...) grep(pattern, x, ignore.case = ignore.case, perl = perl, fixed = fixed
                                              , useBytes = useBytes, invert = invert, ..., value = TRUE)





# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title most_frequent_elements
#' @description Show the most frequent elements of a table.
#' @param vec input vector
#' @param topN PARAM_DESCRIPTION, Default: 10
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
most_frequent_elements <- function(vec, topN = 10) { # Show the most frequent elements of a table.
  tail(sort(table(vec, useNA = "ifany")), topN)
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title top_indices
#' @description Returns the position / index of the n highest values. For equal values, it maintains the original order.
#' @param x PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION, Default: 3
#' @param top PARAM_DESCRIPTION, Default: TRUE
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
top_indices <- function(x, n = 3, top = TRUE) { # Returns the position / index of the n highest values. For equal values, it maintains the original order.
  head( order(x, decreasing = top), n )
}



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title trail
#' @description A combination of head() and tail() to see both ends.
#' @param vec input vector
#' @param N PARAM_DESCRIPTION, Default: 10
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
trail <- function(vec, N = 10) c(head(vec, n = N), tail(vec, n = N) ) # A combination of head() and tail() to see both ends.



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title sort.decreasing
#' @description Sort in decreasing order.
#' @param vec input vector
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
sort.decreasing <- function(vec) sort(vec, decreasing = TRUE) # Sort in decreasing order.


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title sstrsplit
#' @description Alias for str_split_fixed in the stringr package.
#' @param string PARAM_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION, Default: '_'
#' @param n PARAM_DESCRIPTION, Default: 2
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_split}}
#' @export
#' @importFrom stringr str_split_fixed
sstrsplit <- function(string, pattern = "_", n = 2) { stringr::str_split_fixed(string, pattern = pattern, n = n) } # Alias for str_split_fixed in the stringr package



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title topN.dfCol
#' @description Find the n highest values in a named vector.
#' @param df_col data frame column, Default: as.named.vector(df[, 1, drop = FALSE])
#' @param n PARAM_DESCRIPTION, Default: 5
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
topN.dfCol <- function(df_col = as.named.vector(df[ , 1, drop = FALSE]), n = 5)   { head(sort(df_col, decreasing = TRUE), n = n) } # Find the n highest values in a named vector


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title bottomN.dfCol
#' @description Find the n lowest values in a named vector.
#' @param df_col data frame column, Default: as.named.vector(df[, 1, drop = FALSE])
#' @param n PARAM_DESCRIPTION, Default: 5
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
bottomN.dfCol <- function(df_col = as.named.vector(df[ , 1, drop = FALSE]), n = 5) { head(sort(df_col, decreasing = FALSE), n = n) } # Find the n lowest values in a named vector





# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title as.named.vector
#' @description Convert a dataframe column or row into a vector, keeping the corresponding dimension name.
#' @param df_col data frame column
#' @param WhichDimNames PARAM_DESCRIPTION, Default: 1
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title col2named.vector
#' @description Convert a dataframe column into a vector, keeping the corresponding dimension name.
#' @param df_col data frame column
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
col2named.vector <- function(df_col) { # Convert a dataframe column into a vector, keeping the corresponding dimension name.
  namez = rownames(df_col)
  vecc = as.vector(unlist(df_col))
  names(vecc) = namez
  return(vecc)
}




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title row2named.vector
#' @description Convert a dataframe row into a vector, keeping the corresponding dimension name.
#' @param df_row data frame row
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
row2named.vector <- function(df_row) { # Convert a dataframe row into a vector, keeping the corresponding dimension name.
  namez = colnames(df_row)
  vecc = as.vector(unlist(df_row))
  names(vecc) = namez
  return(vecc)
}



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title tibble_summary_to_namedVec
#' @description Convert a key-value tibble into a named vector (as opposed to using rownames).
#' @param tbl PARAM_DESCRIPTION, Default: dplyr::tibble(key = sample(x = 1:5, size = 20, replace = T), value = rnorm(20))
#' @param idx PARAM_DESCRIPTION, Default: c(key = 1, value = 2)
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{reexports}}
#' @export
#' @importFrom dplyr tibble
tibble_summary_to_namedVec <- function(tbl =  dplyr::tibble('key' = sample(x = 1:5, size = 20, replace = T), 'value' = rnorm(20) )
                                       ,  idx = c(key =1, value = 2)) { # Convert a key-value tibble into a named vector (as opposed to using rownames).
  iprint("The following name and value columns are taken:",colnames(tbl[idx]), "; with indices:", idx)
  tbl_2_col <- tbl[,idx]
  named.vec <- tbl_2_col[[2]]
  names(named.vec) <- tbl_2_col[[1]]
  return(named.vec)
}
# tibble_summary_to_namedVec()





# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title as_tibble_from_namedVec
#' @description Convert a vector with names into a tibble, keeping the names as rownames.
#' @param vec.w.names PARAM_DESCRIPTION, Default: c(a = 1, b = 2)
#' @param transpose PARAM_DESCRIPTION, Default: T
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
as_tibble_from_namedVec <- function(vec.w.names =  c("a" = 1, "b" = 2), transpose = T) { # Convert a vector with names into a tibble, keeping the names as rownames.
  stopif(is_null(names(vec.w.names)))
  tbl <- bind_rows(vec.w.names)
  if (transpose) t(tbl) else tbl
}
# as_tibble_from_namedVec()





# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
#' @description FUNCTION_DESCRIPTION.
#' @param vec input vector
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
as.numeric.wNames <- function(vec) { # Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
  numerified_vec = as.numeric(as.factor(vec)) - 1 # as factor gives numbers [1:n] instead [0:n]
  if (!is.null(names(vec))) {names(numerified_vec) = names(vec)}
  return(numerified_vec)
}






# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title as.character.wNames
#' @description Converts your input vector into a character vector, and puts the original character values into the names of the new vector, unless it already has names.
#' @param vec input vector
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
as.character.wNames <- function(vec) { # Converts your input vector into a character vector, and puts the original character values into the names of the new vector, unless it already has names.
  char_vec = as.character(vec)
  if (!is.null(names(vec))) {names(char_vec) = names(vec)}
  return(char_vec)
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title rescale
#' @description Linear transformation to a given range of values.
#' @param vec input vector
#' @param from PARAM_DESCRIPTION, Default: 0
#' @param upto PARAM_DESCRIPTION, Default: 100
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
rescale <- function(vec, from = 0, upto = 100) { # Linear transformation to a given range of values.
  vec = vec - min(vec, na.rm = TRUE)
  vec = vec * ((upto - from)/max(vec, na.rm = TRUE))
  vec = vec + from
  return(vec)
} # fun


# ------------------------------------------------------------------------------------------------------------------------------------------------

#' @title flip_value2name
#' @description Flip the values and the names of a vector with names.
#' @param namedVector PARAM_DESCRIPTION
#' @param NumericNames PARAM_DESCRIPTION, Default: FALSE
#' @param silent PARAM_DESCRIPTION, Default: F
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
flip_value2name <- function(namedVector, NumericNames = FALSE, silent = F) { # Flip the values and the names of a vector with names.
  if (!is.null(names(namedVector))) {
    newvec = names(namedVector)
    if (NumericNames) { newvec = as.numeric(names(namedVector))     }
    names(newvec) = namedVector
  } else {llprint("Vector without names!", head(namedVector))}
  if (!silent) {
    if (any(duplicated(namedVector))) {iprint("New names contain duplicated elements", head(namedVector[which(duplicated(namedVector))])) }
    if (any(duplicated(newvec))) {iprint("Old names contained duplicated elements", head(newvec[which(duplicated(newvec))])) }
  }
  return(newvec)
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title sortbyitsnames
#' @description Sort a vector by the alphanumeric order of its names(instead of its values).
#' @param vec_or_list PARAM_DESCRIPTION
#' @param decreasing PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[gtools]{mixedsort}}
#' @export
#' @importFrom gtools mixedsort
sortbyitsnames <- function(vec_or_list, decreasing = FALSE, ...) { # Sort a vector by the alphanumeric order of its names(instead of its values).
  xx = names(vec_or_list)
  names(xx) = 1:length(vec_or_list)
  order = as.numeric(names(gtools::mixedsort(xx, decreasing = decreasing, ...)))
  vec_or_list[order]
}




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title any.duplicated
#' @description How many entries are duplicated?.
#' @param vec input vector
#' @param summarize PARAM_DESCRIPTION, Default: TRUE
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
any.duplicated <- function(vec, summarize = TRUE) { # How many entries are duplicated?.
  y = sum(duplicated(vec))
  if (summarize & y) {
    x = table(vec); x = x[x > 1] - 1;
    print("The following elements have  > 1 extra copies:")
    print(x) # table formatting requires a separate entry
  }
  return(y)
}




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title which.duplicated
#' @description Which values are duplicated?.
#' @param vec input vector
#' @param orig PARAM_DESCRIPTION, Default: F
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
which.duplicated <- function(vec, orig = F) { # Which values are duplicated?.
  DPL = vec[which(duplicated(vec))]; iprint(length(DPL), "Duplicated entries: ", DPL)
  # for (i in DPL ) {   print(grepv(i,orig)) } #for
  return(DPL)
}




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title which.NA
#' @description Which values are NA?.
#' @param vec input vector
#' @param orig PARAM_DESCRIPTION, Default: F
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
which.NA <- function(vec, orig = F) { # Which values are NA?.
  NANs = vec[which(is.na(vec))]; iprint(length(NANs), "NaN entries: ", NANs)
  NAs = vec[which(is.na(vec))]; iprint(length(NAs), "NA entries: ", NAs, "(only NA-s are returned)")
  # for (i in DPL ) {   print(grepv(i,orig)) } #for
  return(NAs)
}



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title pad.na
#' @description Fill up with a vector to a given length with NA-values at the end.
#' @param x PARAM_DESCRIPTION
#' @param len PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
pad.na <- function(x, len) { c(x, rep(NA, len - length(x))) } # Fill up with a vector to a given length with NA-values at the end.



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title clip.values
#' @description Signal clipping. Cut values above or below a threshold.
#' @param valz PARAM_DESCRIPTION
#' @param high PARAM_DESCRIPTION, Default: TRUE
#' @param thr PARAM_DESCRIPTION, Default: 3
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
clip.values <- function(valz, high = TRUE, thr = 3) { # Signal clipping. Cut values above or below a threshold.
  if (high) { valz[valz > thr] = thr
  } else {    valz[valz < thr] = thr }
  valz
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title clip.outliers
#' @description Signal clipping based on the input data's distribution. It clips values above or below the extreme N% of the distribution.
#' @param valz PARAM_DESCRIPTION
#' @param high PARAM_DESCRIPTION, Default: TRUE
#' @param probs PARAM_DESCRIPTION, Default: c(0.01, 0.99)
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param showhist PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
clip.outliers <- function(valz, high = TRUE, probs = c(.01, .99), na.rm = TRUE, showhist = FALSE, ...) { # Signal clipping based on the input data's distribution. It clips values above or below the extreme N% of the distribution.
  qnt <- quantile(valz, probs = probs, na.rm = na.rm)
  if (showhist) { whist(unlist(valz), breaks = 50 ,vline = qnt, filtercol = -1)} #if
  y <- valz
  y[valz < qnt[1]] <- qnt[1]
  y[valz > qnt[2]] <- qnt[2]
  y
}




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title as.logical.wNames
#' @description Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
as.logical.wNames <- function(x, ...) { # Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.
  numerified_vec = as.logical(x, ...)
  if (!is.null(names(x))) {names(numerified_vec) = names(x)}
  return(numerified_vec)
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title col2named.vec.tbl
#' @description Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.
#' @param tbl.2col PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
col2named.vec.tbl <- function(tbl.2col) { # Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.
  nvec = tbl.2col[[2]]
  names(nvec) = tbl.2col[[1]]
  nvec
}





# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title iterBy.over
#' @description Iterate over a vector by every N-th element.
#' @param vec PARAM_DESCRIPTION
#' @param by PARAM_DESCRIPTION, Default: 9
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
iterBy.over <- function(vec, by = 9) { # Iterate over a vec by every N-th element.
  steps = ceiling(length(vec)/by)
  lsX = split(vec, sort(rank(vec) %% steps))
  names(lsX) = 1:length(lsX)
  lsX
} # for (i in iterBy.over(vec = x)) { print(i) }




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title zigzagger
#' @description Mix entries so that they differ.
#' @param vec input vector, Default: 1:9
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
zigzagger <- function(vec = 1:9) { # mix entries so that they differ.
  intermingle2vec(vec, rev(vec))[1:length(vec)]
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title numerate
#' @description Numerate from x to y with additonal zeropadding.
#' @param x PARAM_DESCRIPTION, Default: 1
#' @param y PARAM_DESCRIPTION, Default: 100
#' @param zeropadding PARAM_DESCRIPTION, Default: TRUE
#' @param pad_length PARAM_DESCRIPTION, Default: floor(log10(max(abs(x), abs(y)))) + 1
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_pad}}
#' @export
#' @importFrom stringr str_pad
numerate <- function(x = 1, y = 100, zeropadding = TRUE, pad_length = floor( log10( max(abs(x), abs(y)) ) ) + 1) { # numerate from x to y with additonal zeropadding.
  z = x:y
  if (zeropadding) { z = stringr::str_pad(z, pad = 0, width = pad_length)   }
  return(z)
}
# (numerate(1, 122))





# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title MaxN
#' @description Find second (third…) highest/lowest value in vector.
#' @param vec input vector, Default: rpois(4, lambda = 3)
#' @param topN PARAM_DESCRIPTION, Default: 2
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
MaxN <- function(vec = rpois(4, lambda = 3), topN = 2) { # find second (third…) highest/lowest value in vector.
  topN = topN - 1
  n <- length(vec)
  sort(vec, partial = n - topN)[n - topN]
}
# https://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column





# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title cumsubtract
#' @description Cumulative subtraction, opposite of cumsum().
#' @param numericVec PARAM_DESCRIPTION, Default: blanks
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
cumsubtract <- function(numericVec = blanks) { # Cumulative subtraction, opposite of cumsum().
  DiffZ = numericVec[-1] - numericVec[-length(numericVec)]
  print(table(DiffZ))
  DiffZ
}





# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title sumBySameName
#' @description Sum up vector elements with the same name.
#' @param namedVec PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
sumBySameName <- function(namedVec) { # Sum up vector elements with the same name.
  # unlapply(splitbyitsnames(namedVec), sum)
  tapply(X = namedVec, INDEX = names(namedVec), sum)
}


### Vector filtering  -------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title which_names
#' @description Return the names where the input vector is TRUE. The input vector is converted to logical.
#' @param namedVec PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
which_names <- function(namedVec) { # Return the names where the input vector is TRUE. The input vector is converted to logical.
  return(names(which(as.logical.wNames(namedVec)))) }




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title which_names_grep
#' @description Return the vector elements whose names are partially matched.
#' @param namedVec PARAM_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
which_names_grep <- function(namedVec, pattern) { # Return the vector elements whose names are partially matched.
  idx = grepv(x = names(namedVec),pattern = pattern)
  return(namedVec[idx])
}




# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title na.omit.strip
#' @description Calls na.omit() and returns a clean vector.
#' Omit NA values from a vector and return a clean vector without any spam.
#' @param object Values to filter for NA
#' @param silent Silence the data structure coversion warning: anything ->vector
#' @param ... Pass any other argument to na.omit()
#' @importFrom stats na.omit
#' @export
#'
#' @examples # CodeAndRoll2::na.omit.strip(c(1, 2, 3, NA, NaN, 2))

na.omit.strip <- function(object, silent = FALSE, ...) {
  if (is.data.frame(object)) {
    if (min(dim(object)) > 1 & silent == FALSE) {
      iprint(dim(object), "dimensional array is converted to a vector.")
    }
    object = unlist(object)
  }
  clean = stats::na.omit(object, ...)
  attributes(clean)$na.action <- NULL
  return(clean)
}





# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title inf.omit
#' @description Omit infinite values from a vector.
#' @param vec input vector
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
inf.omit <- function(vec) { # Omit infinite values from a vector.
  if (is.data.frame(vec)) {
    if ( min(dim(vec)) > 1 ) { iprint(dim(vec), "dimensional array is converted to a vector.") }
    vec = unlist(vec) }
  clean = vec[is.finite(vec)]
  # attributes(clean)$na.action <- NULL
  return(clean)
}



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title zero.omit
#' @description Omit zero values from a vector.
#' @param vec input vector
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
zero.omit <- function(vec) { # Omit zero values from a vector.
  v2 = vec[vec != 0]
  iprint("range: ", range(v2))
  if ( !is.null(names(vec)) ) {names(v2) = names(vec)[vec != 0]}
  return(v2)
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title pc_TRUE
#' @description FUNCTION_DESCRIPTION.
#' @param logical_vector Percentage of true values in a logical vector, parsed as text (useful for reports).
#' @param percentify PARAM_DESCRIPTION, Default: TRUE
#' @param NumberAndPC PARAM_DESCRIPTION, Default: FALSE
#' @param NArm PARAM_DESCRIPTION, Default: TRUE
#' @param prefix PARAM_DESCRIPTION, Default: NULL
#' @param suffix PARAM_DESCRIPTION, Default: NULL
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
pc_TRUE <- function(logical_vector, percentify = TRUE, NumberAndPC = FALSE, NArm = TRUE, prefix = NULL, suffix = NULL) { # Percentage of true values in a logical vector, parsed as text (useful for reports.).
  SUM = sum(logical_vector, na.rm = NArm)
  LEN = length(logical_vector)
  out = SUM / LEN
  if (percentify) {out = percentage_formatter(out) }
  if (NumberAndPC) { out = paste0(out, " or " , SUM, " of ", LEN) }
  if (!is.null(prefix)) {out = paste(prefix, out) }
  if (!is.null(suffix)) {out = paste(out, suffix) }
  return(out)
}



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title pc_in_total_of_match
#' @description Percentage of a certain value within a vector or table.
#' @param vec_or_table PARAM_DESCRIPTION
#' @param category PARAM_DESCRIPTION
#' @param NA_omit PARAM_DESCRIPTION, Default: TRUE
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stats]{na.omit}}
#' @export
#' @importFrom stats na.omit
pc_in_total_of_match <- function(vec_or_table, category, NA_omit = TRUE) { # Percentage of a certain value within a vector or table.
  if (is.table(vec_or_table)) { vec_or_table[category]/sum(vec_or_table, na.rm = NA_omit) }
  else {# if (is.vector(vec_or_table))
    if (NA_omit) {
      if (sum(is.na(vec_or_table))) { vec_or_table = stats::na.omit(vec_or_table); iprint(sum(is.na(vec_or_table)), 'NA are omitted from the vec_or_table of:', length(vec_or_table))}
      "Not wokring complelety : if NaN is stored as string, it does not detect it"
    }
    sum(vec_or_table == category) /  length(vec_or_table)
  } # else: is vector
} # fun


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title filter_survival_length
#' @description Parse a sentence reporting the % of filter survival.
#' @param length_new PARAM_DESCRIPTION
#' @param length_old PARAM_DESCRIPTION
#' @param prepend PARAM_DESCRIPTION, Default: ''
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
filter_survival_length <- function(length_new, length_old, prepend = "") { # Parse a sentence reporting the % of filter survival.
  pc = percentage_formatter(length_new/length_old)
  llprint(prepend, pc, " of ", length_old, " entries make through the filter")
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title remove_outliers
#' @description Remove values that fall outside the trailing N % of the distribution.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @param probs PARAM_DESCRIPTION, Default: c(0.05, 0.95)
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title simplify_categories
#' @description Replace every entry that is found in "replaceit", by a single value provided by "to".
#' @param category_vec PARAM_DESCRIPTION
#' @param replaceit PARAM_DESCRIPTION
#' @param to PARAM_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
simplify_categories <- function(category_vec, replaceit , to ) { # Replace every entry that is found in "replaceit", by a single value provided by "to".
  matches = which(category_vec %in% replaceit); iprint(length(matches), "instances of", replaceit, "are replaced by", to)
  category_vec[matches] = to
  return(category_vec)
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title lookup
#' @description Awesome pattern matching for a set of values in another set of values. Returns a list with all kinds of results.
#' @param needle PARAM_DESCRIPTION
#' @param haystack PARAM_DESCRIPTION
#' @param exact PARAM_DESCRIPTION, Default: TRUE
#' @param report PARAM_DESCRIPTION, Default: FALSE
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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





#' @title Translate values to a new set using a dictionary
#' @description Replaces a set of values in a vector with another set of values,
#' it translates your vector. Oldvalues and newvalues have to be 1-to-1
#' correspoding vectors.
#' @param vec set of values where you want to replace
#' @param oldvalues oldvalues (from)
#' @param newvalues newvalues (to)
#' @export
#' @examples A = 1:3; translate(vec = A, oldvalues = 2:3, newvalues = letters[1:2])

translate = replace_values <- function(vec, oldvalues, newvalues) {
  Nr = length(oldvalues)
  if (Nr > length(newvalues)) {
    if (length(newvalues) == 1) {
      newvalues = rep(newvalues, length(oldvalues))
    } else if (length(newvalues) > 1) {
      iprint("PROVIDE ONE NEWVALUE, OR THE SAME NUMEBR OF NEWVALUES AS OLDVALUES.")
    }
  }
  tmp = vec
  for (i in 1:Nr) {
    oldval = oldvalues[i]
    tmp[vec == oldval] = newvalues[i]
  }
  return(tmp)
}
# 'chartr("a-cX", "D-Fw", x) does the same as above in theory,
# but it did not seem very robust regarding your input...'





#' @title as.factor.numeric
#'
#' @description  Turn any vector into numeric categories as.numeric(as.factor(vec))
#' @param vec vector of factors or strings
#' @param rename Rename the vector?
#' @param ... Pass any other argument to as.factor()
#' @export
#'
#' @examples as.factor.numeric(LETTERS[1:4])

as.factor.numeric <- function(vec, rename = FALSE, ...) {
  vec2 = as.numeric(as.factor(vec, ...)) ;
  names (vec2) <- if ( !rename & !is.null(names(vec) ) ) { names (vec)
  } else { vec }
  return(vec2)
}

