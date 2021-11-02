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


# _______________________________________________________________________________________________
#' @title vec.fromNames
#' @description Create a vector from a vector of names.
#' @param name_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @export
vec.fromNames <- function(name_vec = LETTERS[1:5], fill = NA) { # Create a vector from a vector of names.
  v = numeric(length(name_vec))
  if (length(fill) == 1) {v = rep(fill, length(name_vec))}
  else if (length(fill == length(name_vec))) {v = fill}
  names(v) = name_vec
  return(v)
}



# _______________________________________________________________________________________________
#' @title list.fromNames
#' @description create list from a vector with the names of the elements.
#' @param name_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NaN
#' @export
list.fromNames <- function(name_vec = LETTERS[1:5], fill = NaN) { # create list from a vector with the names of the elements.
  liszt = as.list(rep(fill, length(name_vec)))
  names(liszt) = name_vec
  return(liszt)
}


# _______________________________________________________________________________________________
#' @title matrix.fromNames
#' @description Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.
#' @param rowname_vec PARAM_DESCRIPTION, Default: 1:10
#' @param colname_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @export
matrix.fromNames <- function(rowname_vec = 1:10, colname_vec = LETTERS[1:5], fill = NA) { # Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.
  mx = matrix(data = fill, nrow = length(rowname_vec), ncol = length(colname_vec), dimnames = list(rowname_vec, colname_vec))
  iprint("Dimensions:", dim(mx))
  return(mx)
}



# _______________________________________________________________________________________________
#' @title matrix.fromVector
#' @description Create a matrix from values in a vector repeated for each column / each row. Similar to rowNameMatrix and colNameMatrix.
#' @param vector Input vector, Default: 1:5
#' @param HowManyTimes PARAM_DESCRIPTION, Default: 3
#' @param IsItARow PARAM_DESCRIPTION, Default: TRUE
#' @export
matrix.fromVector <- function(vector = 1:5, HowManyTimes = 3, IsItARow = TRUE) { # Create a matrix from values in a vector repeated for each column / each row. Similar to rowNameMatrix and colNameMatrix.
  matt = matrix(vector, nrow = length(vector), ncol = HowManyTimes)
  if ( !IsItARow ) {matt = t(matt)}
  return(matt)
}



# _______________________________________________________________________________________________
#' @title array.fromNames
#' @description Create an N-dimensional array from N vectors defining the row-, column, etc names of the array.
#' @param rowname_vec PARAM_DESCRIPTION, Default: 1:3
#' @param colname_vec PARAM_DESCRIPTION, Default: letters[1:2]
#' @param z_name_vec PARAM_DESCRIPTION, Default: LETTERS[4:6]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @export
array.fromNames <- function(rowname_vec = 1:3, colname_vec = letters[1:2], z_name_vec = LETTERS[4:6], fill = NA) { # Create an N-dimensional array from N vectors defining the row-, column, etc names of the array.
  DimNames = list(rowname_vec, colname_vec, z_name_vec)
  Dimensions_ = lapply(DimNames, length)
  mx = array(data = fill, dim = Dimensions_, dimnames = DimNames)
  iprint("Dimensions:", dim(mx))
  return(mx)
}



# _______________________________________________________________________________________________
#' @title what
#' @description A better version of is(). It can print the first "printme" elements.
#' @param x PARAM_DESCRIPTION
#' @param printme PARAM_DESCRIPTION, Default: 0
#' @export
what <- function(x, printme = 0) { # A better version of is(). It can print the first "printme" elements.
  iprint(is(x), "; nr. of elements:", length(x))
  if ( is.numeric(x) )    { iprint("min&max:", range(x) ) } else {print("Not numeric")}
  if ( length(dim(x) ) > 0 )  { iprint("Dim:", dim(x) ) }
  if ( printme > 0)       { iprint("Elements:", x[0:printme] ) }
  head(x)
}



# _______________________________________________________________________________________________
#' @title idim
#' @description A dim() function that can handle if you pass on a vector: then, it gives the length.
#' @param any_object PARAM_DESCRIPTION
#' @export
idim <- function(any_object) { # A dim() function that can handle if you pass on a vector: then, it gives the length.
  if (is.null(dim(any_object))) {
    if (is.list(any_object)) { print("list") } #if
    print(length(any_object))
  }
  else { print(dim(any_object))  }
}



# _______________________________________________________________________________________________
#' @title idimnames
#' @description A dimnames() function that can handle if you pass on a vector: it gives back the names.
#' @param any_object PARAM_DESCRIPTION
#' @export
idimnames <- function(any_object) { # A dimnames() function that can handle if you pass on a vector: it gives back the names.
  if (!is.null(dimnames(any_object)))   { print(dimnames(any_object)) }
  else if (!is.null(colnames(any_object))) { iprint("colnames:", colnames(any_object))  }
  else if (!is.null(rownames(any_object))) { iprint("rownames:", rownames(any_object))  }
  else if (!is.null(names(any_object))) { iprint("names:", names(any_object)) }
}



# _______________________________________________________________________________________________
#' @title table_fixed_categories
#' @description Generate a table() with a fixed set of categories. It fills up the table with missing categories, that are relevant when comparing to other vectors.
#' @param vec Input vector
#' @param categories_vec PARAM_DESCRIPTION
#' @export
table_fixed_categories <- function(vec, categories_vec) { # generate a table() with a fixed set of categories. It fills up the table with missing categories, that are relevant when comparing to other vectors.
  if ( !is.vector(vec)) {print(is(vec[]))}
  table(factor(unlist(vec), levels = categories_vec))
}

## Vector operations -------------------------------------------------------------------------------------------------

# _______________________________________________________________________________________________
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
#' @export
grepv <- function(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE  # grep returning the value
                  , invert = FALSE, ...) grep(pattern, x, ignore.case = ignore.case, perl = perl, fixed = fixed
                                              , useBytes = useBytes, invert = invert, ..., value = TRUE)



# _______________________________________________________________________________________________
#' @title most_frequent_elements
#' @description Show the most frequent elements of a table.
#' @param vec input vector
#' @param topN PARAM_DESCRIPTION, Default: 10
#' @export
most_frequent_elements <- function(vec, topN = 10) { # Show the most frequent elements of a table.
  tail(sort(table(vec, useNA = "ifany")), topN)
}


# _______________________________________________________________________________________________
#' @title top_indices
#' @description Returns the position / index of the n highest values. For equal values, it maintains the original order.
#' @param x PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION, Default: 3
#' @param top PARAM_DESCRIPTION, Default: TRUE
#' @export
top_indices <- function(x, n = 3, top = TRUE) { # Returns the position / index of the n highest values. For equal values, it maintains the original order.
  head( order(x, decreasing = top), n )
}



# _______________________________________________________________________________________________
#' @title trail
#' @description A combination of head() and tail() to see both ends.
#' @param vec input vector
#' @param N PARAM_DESCRIPTION, Default: 10
#' @export
trail <- function(vec, N = 10) c(head(vec, n = N), tail(vec, n = N) ) # A combination of head() and tail() to see both ends.



# _______________________________________________________________________________________________
#' @title sort.decreasing
#' @description Sort in decreasing order.
#' @param vec input vector
#' @export
sort.decreasing <- function(vec) sort(vec, decreasing = TRUE) # Sort in decreasing order.


# _______________________________________________________________________________________________
#' @title sstrsplit
#' @description Alias for str_split_fixed in the stringr package.
#' @param string PARAM_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION, Default: '_'
#' @param n PARAM_DESCRIPTION, Default: 2
#' @seealso
#'  \code{\link[stringr]{str_split}}
#' @export
#' @importFrom stringr str_split_fixed
sstrsplit <- function(string, pattern = "_", n = 2) { stringr::str_split_fixed(string, pattern = pattern, n = n) } # Alias for str_split_fixed in the stringr package


# _______________________________________________________________________________________________
#' @title as.named.vector
#' @description Convert a dataframe column or row into a vector, keeping the corresponding dimension name.
#' @param df_col data frame column
#' @param WhichDimNames PARAM_DESCRIPTION, Default: 1
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



# _______________________________________________________________________________________________
#' @title col2named.vector
#' @description Convert a dataframe column into a vector, keeping the corresponding dimension name.
#' @param df_col data frame column
#' @export
col2named.vector <- function(df_col) { # Convert a dataframe column into a vector, keeping the corresponding dimension name.
  namez = rownames(df_col)
  vecc = as.vector(unlist(df_col))
  names(vecc) = namez
  return(vecc)
}



# _______________________________________________________________________________________________
#' @title row2named.vector
#' @description Convert a dataframe row into a vector, keeping the corresponding dimension name.
#' @param df_row data frame row
#' @export
row2named.vector <- function(df_row) { # Convert a dataframe row into a vector, keeping the corresponding dimension name.
  namez = colnames(df_row)
  vecc = as.vector(unlist(df_row))
  names(vecc) = namez
  return(vecc)
}



# _______________________________________________________________________________________________
#' @title tibble_summary_to_namedVec
#' @description Convert a key-value tibble into a named vector (as opposed to using rownames).
#' @param tbl PARAM_DESCRIPTION, Default: dplyr::tibble(key = sample(x = 1:5, size = 20, replace = T), value = rnorm(20))
#' @param idx PARAM_DESCRIPTION, Default: c(key = 1, value = 2)
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



# _______________________________________________________________________________________________
#' @title as_tibble_from_namedVec
#' @description Convert a vector with names into a tibble, keeping the names as rownames.
#' @param vec.w.names PARAM_DESCRIPTION, Default: c(a = 1, b = 2)
#' @param transpose PARAM_DESCRIPTION, Default: T
#' @export
as_tibble_from_namedVec <- function(vec.w.names =  c("a" = 1, "b" = 2), transpose = T) { # Convert a vector with names into a tibble, keeping the names as rownames.
  stopif(is_null(names(vec.w.names)))
  tbl <- bind_rows(vec.w.names)
  if (transpose) t(tbl) else tbl
}
# as_tibble_from_namedVec()



# _______________________________________________________________________________________________
#' @title Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
#' @description FUNCTION_DESCRIPTION.
#' @param vec input vector
#' @export
as.numeric.wNames <- function(vec) { # Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
  numerified_vec = as.numeric(as.factor(vec)) - 1 # as factor gives numbers [1:n] instead [0:n]
  if (!is.null(names(vec))) {names(numerified_vec) = names(vec)}
  return(numerified_vec)
}


# _______________________________________________________________________________________________
#' @title as.factor.numeric
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



# _______________________________________________________________________________________________
#' @title as.character.wNames
#' @description Converts your input vector into a character vector, and puts the original character values into the names of the new vector, unless it already has names.
#' @param vec input vector
#' @export
as.character.wNames <- function(vec) { # Converts your input vector into a character vector, and puts the original character values into the names of the new vector, unless it already has names.
  char_vec = as.character(vec)
  if (!is.null(names(vec))) {names(char_vec) = names(vec)}
  return(char_vec)
}


# _______________________________________________________________________________________________
#' @title Translate values to a new set using a dictionary
#' @description Replaces a set of values in a vector with another set of values,
#' it translates your vector. Oldvalues and newvalues have to be 1-to-1
#' correspoding vectors.  'chartr("a-cX", "D-Fw", x) does the same as above
#' in theory, but it did not seem very robust regarding your input...'
#' @param vec set of values where you want to replace
#' @param oldvalues oldvalues (from)
#' @param newvalues newvalues (to)
#' @export
#' @examples A = 1:3; translate(vec = A, oldvalues = 2:3, newvalues = letters[1:2])

translate <- function(vec, oldvalues, newvalues) {
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


# _______________________________________________________________________________________________
#' @title rescale
#' @description Linear transformation to a given range of values.
#' @param vec input vector
#' @param from PARAM_DESCRIPTION, Default: 0
#' @param upto PARAM_DESCRIPTION, Default: 100
#' @export
rescale <- function(vec, from = 0, upto = 100) { # Linear transformation to a given range of values.
  vec = vec - min(vec, na.rm = TRUE)
  vec = vec * ((upto - from)/max(vec, na.rm = TRUE))
  vec = vec + from
  return(vec)
} # fun


# _______________________________________________________________________________________________

#' @title flip_value2name
#' @description Flip the values and the names of a vector with names.
#' @param namedVector PARAM_DESCRIPTION
#' @param NumericNames PARAM_DESCRIPTION, Default: FALSE
#' @param silent PARAM_DESCRIPTION, Default: F
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


# _______________________________________________________________________________________________
#' @title sortbyitsnames
#' @description Sort a vector by the alphanumeric order of its names(instead of its values).
#' @param vec_or_list PARAM_DESCRIPTION
#' @param decreasing PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
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



# _______________________________________________________________________________________________
#' @title any.duplicated
#' @description How many entries are duplicated?.
#' @param vec input vector
#' @param summarize PARAM_DESCRIPTION, Default: TRUE
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



# _______________________________________________________________________________________________
#' @title which.duplicated
#' @description Which values are duplicated?.
#' @param vec input vector
#' @param orig PARAM_DESCRIPTION, Default: F
#' @export
which.duplicated <- function(vec, orig = F) { # Which values are duplicated?.
  DPL = vec[which(duplicated(vec))]; iprint(length(DPL), "Duplicated entries: ", DPL)
  # for (i in DPL ) {   print(grepv(i,orig)) } #for
  return(DPL)
}



# _______________________________________________________________________________________________
#' @title which.NA
#' @description Which values are NA?.
#' @param vec input vector
#' @param orig PARAM_DESCRIPTION, Default: F
#' @export
which.NA <- function(vec, orig = F) { # Which values are NA?.
  NANs = vec[which(is.na(vec))]; iprint(length(NANs), "NaN entries: ", NANs)
  NAs = vec[which(is.na(vec))]; iprint(length(NAs), "NA entries: ", NAs, "(only NA-s are returned)")
  # for (i in DPL ) {   print(grepv(i,orig)) } #for
  return(NAs)
}



# _______________________________________________________________________________________________
#' @title pad.na
#' @description Fill up with a vector to a given length with NA-values at the end.
#' @param x PARAM_DESCRIPTION
#' @param len PARAM_DESCRIPTION
#' @export
pad.na <- function(x, len) { c(x, rep(NA, len - length(x))) } # Fill up with a vector to a given length with NA-values at the end.



# _______________________________________________________________________________________________
#' @title clip.values
#' @description Signal clipping. Cut values above or below a threshold.
#' @param valz PARAM_DESCRIPTION
#' @param high PARAM_DESCRIPTION, Default: TRUE
#' @param thr PARAM_DESCRIPTION, Default: 3
#' @export
clip.values <- function(valz, high = TRUE, thr = 3) { # Signal clipping. Cut values above or below a threshold.
  if (high) { valz[valz > thr] = thr
  } else {    valz[valz < thr] = thr }
  valz
}


# _______________________________________________________________________________________________
#' @title clip.outliers
#' @description Signal clipping based on the input data's distribution. It clips values above or below the extreme N% of the distribution.
#' @param valz PARAM_DESCRIPTION
#' @param high PARAM_DESCRIPTION, Default: TRUE
#' @param probs PARAM_DESCRIPTION, Default: c(0.01, 0.99)
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param showhist PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @export
clip.outliers <- function(valz, high = TRUE, probs = c(.01, .99), na.rm = TRUE, showhist = FALSE, ...) { # Signal clipping based on the input data's distribution. It clips values above or below the extreme N% of the distribution.
  qnt <- quantile(valz, probs = probs, na.rm = na.rm)
  if (showhist) { whist(unlist(valz), breaks = 50 ,vline = qnt, filtercol = -1)} #if
  y <- valz
  y[valz < qnt[1]] <- qnt[1]
  y[valz > qnt[2]] <- qnt[2]
  y
}



# _______________________________________________________________________________________________
#' @title as.logical.wNames
#' @description Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @export
as.logical.wNames <- function(x, ...) { # Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.
  numerified_vec = as.logical(x, ...)
  if (!is.null(names(x))) {names(numerified_vec) = names(x)}
  return(numerified_vec)
}


# _______________________________________________________________________________________________
#' @title col2named.vec.tbl
#' @description Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.
#' @param tbl.2col PARAM_DESCRIPTION
#' @export
col2named.vec.tbl <- function(tbl.2col) { # Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.
  nvec = tbl.2col[[2]]
  names(nvec) = tbl.2col[[1]]
  nvec
}



# _______________________________________________________________________________________________
#' @title topN.dfCol
#' @description Find the n highest values in a named vector.
#' @param df_col data frame column, Default: as.named.vector(df[, 1, drop = FALSE])
#' @param n PARAM_DESCRIPTION, Default: 5
#' @export
topN.dfCol <- function(df_col = as.named.vector(df[ , 1, drop = FALSE]), n = 5)   { head(sort(df_col, decreasing = TRUE), n = n) } # Find the n highest values in a named vector


# _______________________________________________________________________________________________
#' @title bottomN.dfCol
#' @description Find the n lowest values in a named vector.
#' @param df_col data frame column, Default: as.named.vector(df[, 1, drop = FALSE])
#' @param n PARAM_DESCRIPTION, Default: 5
#' @export
bottomN.dfCol <- function(df_col = as.named.vector(df[ , 1, drop = FALSE]), n = 5) { head(sort(df_col, decreasing = FALSE), n = n) } # Find the n lowest values in a named vector



# _______________________________________________________________________________________________
#' @title iterBy.over
#' @description Iterate over a vector by every N-th element.
#' @param vec PARAM_DESCRIPTION
#' @param by PARAM_DESCRIPTION, Default: 9
#' @export
iterBy.over <- function(vec, by = 9) { # Iterate over a vec by every N-th element.
  steps = ceiling(length(vec)/by)
  lsX = split(vec, sort(rank(vec) %% steps))
  names(lsX) = 1:length(lsX)
  lsX
} # for (i in iterBy.over(vec = x)) { print(i) }



# _______________________________________________________________________________________________
#' @title zigzagger
#' @description Mix entries so that they differ.
#' @param vec input vector, Default: 1:9
#' @export
zigzagger <- function(vec = 1:9) { # mix entries so that they differ.
  intermingle2vec(vec, rev(vec))[1:length(vec)]
}


# _______________________________________________________________________________________________
#' @title numerate
#' @description Numerate from x to y with additonal zeropadding.
#' @param x PARAM_DESCRIPTION, Default: 1
#' @param y PARAM_DESCRIPTION, Default: 100
#' @param zeropadding PARAM_DESCRIPTION, Default: TRUE
#' @param pad_length PARAM_DESCRIPTION, Default: floor(log10(max(abs(x), abs(y)))) + 1
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



# _______________________________________________________________________________________________
#' @title MaxN
#' @description Find second (third…) highest/lowest value in vector.
#' @param vec input vector, Default: rpois(4, lambda = 3)
#' @param topN PARAM_DESCRIPTION, Default: 2
#' @export
MaxN <- function(vec = rpois(4, lambda = 3), topN = 2) { # find second (third…) highest/lowest value in vector.
  topN = topN - 1
  n <- length(vec)
  sort(vec, partial = n - topN)[n - topN]
}
# https://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column



# _______________________________________________________________________________________________
#' @title cumsubtract
#' @description Cumulative subtraction, opposite of cumsum().
#' @param numericVec PARAM_DESCRIPTION, Default: blanks
#' @export
cumsubtract <- function(numericVec = blanks) { # Cumulative subtraction, opposite of cumsum().
  DiffZ = numericVec[-1] - numericVec[-length(numericVec)]
  print(table(DiffZ))
  DiffZ
}



# _______________________________________________________________________________________________
#' @title sumBySameName
#' @description Sum up vector elements with the same name.
#' @param namedVec PARAM_DESCRIPTION
#' @export
sumBySameName <- function(namedVec) { # Sum up vector elements with the same name.
  # unlapply(splitbyitsnames(namedVec), sum)
  tapply(X = namedVec, INDEX = names(namedVec), sum)
}


### Vector filtering  -------------------------------------------------------------------------------------------------
# _______________________________________________________________________________________________
#' @title which_names
#' @description Return the names where the input vector is TRUE. The input vector is converted to logical.
#' @param namedVec PARAM_DESCRIPTION
#' @export
which_names <- function(namedVec) { # Return the names where the input vector is TRUE. The input vector is converted to logical.
  return(names(which(as.logical.wNames(namedVec)))) }



# _______________________________________________________________________________________________
#' @title which_names_grep
#' @description Return the vector elements whose names are partially matched.
#' @param namedVec PARAM_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION
#' @export
which_names_grep <- function(namedVec, pattern) { # Return the vector elements whose names are partially matched.
  idx = grepv(x = names(namedVec),pattern = pattern)
  return(namedVec[idx])
}



# _______________________________________________________________________________________________
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



# _______________________________________________________________________________________________
#' @title inf.omit
#' @description Omit infinite values from a vector.
#' @param vec input vector
#' @export
inf.omit <- function(vec) { # Omit infinite values from a vector.
  if (is.data.frame(vec)) {
    if ( min(dim(vec)) > 1 ) { iprint(dim(vec), "dimensional array is converted to a vector.") }
    vec = unlist(vec) }
  clean = vec[is.finite(vec)]
  # attributes(clean)$na.action <- NULL
  return(clean)
}



# _______________________________________________________________________________________________
#' @title zero.omit
#' @description Omit zero values from a vector.
#' @param vec input vector
#' @export
zero.omit <- function(vec) { # Omit zero values from a vector.
  v2 = vec[vec != 0]
  iprint("range: ", range(v2))
  if ( !is.null(names(vec)) ) {names(v2) = names(vec)[vec != 0]}
  return(v2)
}


# _______________________________________________________________________________________________
#' @title pc_TRUE
#' @description FUNCTION_DESCRIPTION.
#' @param logical_vector Percentage of true values in a logical vector, parsed as text (useful for reports).
#' @param percentify PARAM_DESCRIPTION, Default: TRUE
#' @param NumberAndPC PARAM_DESCRIPTION, Default: FALSE
#' @param NArm PARAM_DESCRIPTION, Default: TRUE
#' @param prefix PARAM_DESCRIPTION, Default: NULL
#' @param suffix PARAM_DESCRIPTION, Default: NULL
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



# _______________________________________________________________________________________________
#' @title pc_in_total_of_match
#' @description Percentage of a certain value within a vector or table.
#' @param vec_or_table PARAM_DESCRIPTION
#' @param category PARAM_DESCRIPTION
#' @param NA_omit PARAM_DESCRIPTION, Default: TRUE
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


# _______________________________________________________________________________________________
#' @title filter_survival_length
#' @description Parse a sentence reporting the % of filter survival.
#' @param length_new PARAM_DESCRIPTION
#' @param length_old PARAM_DESCRIPTION
#' @param prepend PARAM_DESCRIPTION, Default: ''
#' @export
filter_survival_length <- function(length_new, length_old, prepend = "") { # Parse a sentence reporting the % of filter survival.
  pc = percentage_formatter(length_new/length_old)
  llprint(prepend, pc, " of ", length_old, " entries make through the filter")
}


# _______________________________________________________________________________________________
#' @title remove_outliers
#' @description Remove values that fall outside the trailing N % of the distribution.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @param probs PARAM_DESCRIPTION, Default: c(0.05, 0.95)
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


# _______________________________________________________________________________________________
#' @title simplify_categories
#' @description Replace every entry that is found in "replaceit", by a single value provided by "to".
#' @param category_vec PARAM_DESCRIPTION
#' @param replaceit PARAM_DESCRIPTION
#' @param to PARAM_DESCRIPTION
#' @export
simplify_categories <- function(category_vec, replaceit , to ) { # Replace every entry that is found in "replaceit", by a single value provided by "to".
  matches = which(category_vec %in% replaceit); iprint(length(matches), "instances of", replaceit, "are replaced by", to)
  category_vec[matches] = to
  return(category_vec)
}


# _______________________________________________________________________________________________
#' @title lookup
#' @description Awesome pattern matching for a set of values in another set of values. Returns a list with all kinds of results.
#' @param needle PARAM_DESCRIPTION
#' @param haystack PARAM_DESCRIPTION
#' @param exact PARAM_DESCRIPTION, Default: TRUE
#' @param report PARAM_DESCRIPTION, Default: FALSE
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


# _______________________________________________________________________________________________
#' @title rowMedians
#' @description Calculates the median of each row of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
rowMedians <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, median, na.rm = na.rm) # Calculates the median of each row of a numeric matrix / data frame.


# _______________________________________________________________________________________________
#' @title colMedians
#' @description Calculates the median of each column of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
colMedians <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, median, na.rm = na.rm) # Calculates the median of each column of a numeric matrix / data frame.



# _______________________________________________________________________________________________
#' @title rowGeoMeans
#' @description Calculates the median of each row of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
rowGeoMeans <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, geomean, na.rm = na.rm) # Calculates the median of each row of a numeric matrix / data frame.


# _______________________________________________________________________________________________
#' @title colGeoMeans
#' @description Calculates the median of each column of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
colGeoMeans <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, geomean, na.rm = na.rm) # Calculates the median of each column of a numeric matrix / data frame.



# _______________________________________________________________________________________________
#' @title rowCV
#' @description Calculates the CV of each ROW of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
rowCV <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, cv, na.rm = na.rm ) # Calculates the CV of each ROW of a numeric matrix / data frame.


# _______________________________________________________________________________________________
#' @title colCV
#' @description Calculates the CV of each column of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
colCV <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, cv, na.rm = na.rm ) # Calculates the CV of each column of a numeric matrix / data frame.



# _______________________________________________________________________________________________
#' @title rowVariance
#' @description Calculates the CV of each ROW of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
rowVariance <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, var, na.rm = na.rm ) # Calculates the CV of each ROW of a numeric matrix / data frame.


# _______________________________________________________________________________________________
#' @title colVariance
#' @description Calculates the CV of each column of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
colVariance <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, var, na.rm = na.rm ) # Calculates the CV of each column of a numeric matrix / data frame.



# _______________________________________________________________________________________________
#' @title rowMin
#' @description Calculates the minimum of each row of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
rowMin <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, min, na.rm = na.rm) # Calculates the minimum of each row of a numeric matrix / data frame.


# _______________________________________________________________________________________________
#' @title colMin
#' @description Calculates the minimum of each column of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
colMin <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, min, na.rm = na.rm) # Calculates the minimum of each column of a numeric matrix / data frame.



# _______________________________________________________________________________________________
#' @title rowMax
#' @description Calculates the maximum of each row of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
rowMax <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, max, na.rm = na.rm) # Calculates the maximum of each row of a numeric matrix / data frame.


# _______________________________________________________________________________________________
#' @title colMax
#' @description Calculates the maximum of each column of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
colMax <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, max, na.rm = na.rm) # Calculates the maximum of each column of a numeric matrix / data frame.



# _______________________________________________________________________________________________
#' @title rowSEM
#' @description Calculates the SEM of each row of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
rowSEM <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, sem, na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.


# _______________________________________________________________________________________________
#' @title colSEM
#' @description Calculates the SEM of each column of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
colSEM <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, sem, na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.



# _______________________________________________________________________________________________
#' @title rowSD
#' @description Calculates the SEM of each row of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
rowSD <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, sd, na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.


# _______________________________________________________________________________________________
#' @title colSD
#' @description Calculates the SEM of each column of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
colSD <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, sd, na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.



# _______________________________________________________________________________________________
#' @title rowIQR
#' @description Calculates the SEM of each row of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
rowIQR <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, IQR, na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.


# _______________________________________________________________________________________________
#' @title colIQR
#' @description Calculates the SEM of each column of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
colIQR <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, IQR, na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.



# _______________________________________________________________________________________________
#' @title rowquantile
#' @description Calculates the SEM of each row of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @export
rowquantile <- function(x, na.rm = TRUE, ...) apply(data.matrix(x), 1, quantile, ..., na.rm = na.rm) # Calculates the SEM of each row of a numeric matrix / data frame.


# _______________________________________________________________________________________________
#' @title colquantile
#' @description Calculates the SEM of each column of a numeric matrix / data frame.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @export
colquantile <- function(x, na.rm = TRUE, ...) apply(data.matrix(x), 2, quantile, ..., na.rm = na.rm) # Calculates the SEM of each column of a numeric matrix / data frame.



# _______________________________________________________________________________________________
#' @title colDivide
#' @description See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r.
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @export
colDivide <- function(mat, vec) { # divide by column # See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
  stopifnot(NCOL(mat) == length(vec))
  mat / vec[col(mat)] # fastest
}



# _______________________________________________________________________________________________
#' @title colMutliply
#' @description See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r.
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @export
colMutliply <- function(mat, vec) { # Mutliply by column # See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
  stopifnot(NCOL(mat) == length(vec))
  mat * vec[col(mat)] # fastest
}



# _______________________________________________________________________________________________
#' @title rowDivide
#' @description Divide by row.
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @export
rowDivide <- function(mat, vec) { # divide by row
  stopifnot(NROW(mat) == length(vec))
  mat / vec[row(mat)] # fastest
}



# _______________________________________________________________________________________________
#' @title rowMutliply
#' @description Mutliply by row.
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @export
rowMutliply <- function(mat, vec) { # Mutliply by row
  stopifnot(NROW(mat) == length(vec))
  mat * vec[row(mat)] # fastest
}



# _______________________________________________________________________________________________
#' @title row.Zscore
#' @description Calculate Z-score over rows of data frame.
#' @param DF PARAM_DESCRIPTION
#' @export
row.Zscore <- function(DF) t(scale(t(DF))) # Calculate Z-score over rows of data frame.



# _______________________________________________________________________________________________
#' @title TPM_normalize
#' @description Normalize each column to 1 million.
#' @param mat PARAM_DESCRIPTION
#' @param SUM PARAM_DESCRIPTION, Default: 1e+06
#' @export
TPM_normalize <- function(mat, SUM = 1e6) { # normalize each column to 1 million
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * SUM
  return(norm_mat)
}



# _______________________________________________________________________________________________
#' @title median_normalize
#' @description Normalize each column to the median of all the column-sums.
#' @param mat PARAM_DESCRIPTION
#' @export
median_normalize <- function(mat) { # normalize each column to the median of all the column-sums
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * median(cs)
  iprint("colMedians: ", head(signif(colMedians(norm_mat), digits = 3)))
  return(norm_mat)
}



# _______________________________________________________________________________________________
#' @title mean_normalize
#' @description Normalize each column to the median of the columns.
#' @param mat PARAM_DESCRIPTION
#' @export
mean_normalize <- function(mat) { # normalize each column to the median of the columns
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * mean(cs)
  iprint("colMeans: ", head(signif(colMeans(norm_mat))))
  return(norm_mat)
}


## Matrix manipulations -------------------------------------------------------------------------------------------------


# _______________________________________________________________________________________________
#' @title rotate
#' @description Rotate a matrix 90 degrees.
#' @param x PARAM_DESCRIPTION
#' @param clockwise PARAM_DESCRIPTION, Default: TRUE
#' @export
rotate <- function(x, clockwise = TRUE) { # rotate a matrix 90 degrees.
  if (clockwise) { t( apply(x, 2, rev))  #first reverse, then transpose, it's the same as rotate 90 degrees
  } else {apply( t(x), 2, rev)}  #first transpose, then reverse, it's the same as rotate -90 degrees:
}



# _______________________________________________________________________________________________
#' @title sortEachColumn
#' @description Sort each column of a numeric matrix / data frame.
#' @param data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @export
sortEachColumn <- function(data, ...) sapply(data, sort, ...) # Sort each column of a numeric matrix / data frame.



# _______________________________________________________________________________________________
#' @title sort.mat
#' @description Sort a matrix. ALTERNATIVE: dd[with(dd, order(-z, b)), ]. Source: https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r.
#' @param df PARAM_DESCRIPTION
#' @param colname_in_df PARAM_DESCRIPTION, Default: 1
#' @param decrease PARAM_DESCRIPTION, Default: FALSE
#' @param na_last PARAM_DESCRIPTION, Default: TRUE
#' @export
sort.mat <- function(df, colname_in_df = 1, decrease = FALSE, na_last = TRUE) { # Sort a matrix. ALTERNATIVE: dd[with(dd, order(-z, b)), ]. Source: https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r
  if (length(colname_in_df) > 1) { print("cannot handle multi column sort") }
  else {df[ order(df[, colname_in_df], decreasing = decrease, na.last = na_last), ]}
}



# _______________________________________________________________________________________________
#' @title rowNameMatrix
#' @description Create a copy of your matrix, where every entry is replaced by the corresponding row name. Useful if you want to color by row name in a plot (where you have different number of NA-values in each row).
#' @param mat_w_dimnames PARAM_DESCRIPTION
#' @export
rowNameMatrix <- function(mat_w_dimnames) { # Create a copy of your matrix, where every entry is replaced by the corresponding row name. Useful if you want to color by row name in a plot (where you have different number of NA-values in each row).
  matrix(rep(rownames(mat_w_dimnames), ncol(mat_w_dimnames) ), nrow = nrow(mat_w_dimnames), ncol = ncol(mat_w_dimnames))
}



# _______________________________________________________________________________________________
#' @title colNameMatrix
#' @description Create a copy of your matrix, where every entry is replaced by the corresponding column name. Useful if you want to color by column name in a plot (where you have different number of NA-values in each column).
#' @param mat_w_dimnames PARAM_DESCRIPTION
#' @export
colNameMatrix <- function(mat_w_dimnames) { # Create a copy of your matrix, where every entry is replaced by the corresponding column name. Useful if you want to color by column name in a plot (where you have different number of NA-values in each column).
  x = rep(colnames(mat_w_dimnames), nrow(mat_w_dimnames) )
  t(matrix(x, nrow = ncol(mat_w_dimnames), ncol = nrow(mat_w_dimnames)))
}



# _______________________________________________________________________________________________
#' @title rownames.trimws
#' @description Trim whitespaces from the rownames.
#' @param matrix1 PARAM_DESCRIPTION
#' @export
rownames.trimws <- function(matrix1) { # trim whitespaces from the rownames
  rownames(matrix1) = trimws(rownames(matrix1))
  return(matrix1)
}



# _______________________________________________________________________________________________
#' @title colsplit
#' @description Split a data frame by a factor corresponding to columns.
#' @param df PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION, Default: colnames(df)
#' @export
colsplit <- function(df, f = colnames(df)) { # split a data frame by a factor corresponding to columns.
  ListOfDFs = NULL
  levelz = unique(f)
  for (i in 1:length(levelz)) {   ListOfDFs[[i]] = df[ , which(f == levelz[i]) ]  }
  names(ListOfDFs) = levelz
  return(ListOfDFs)
}



# _______________________________________________________________________________________________
#' @title rowsplit
#' @description Split a data frame by a factor corresponding to columns.
#' @param df PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION, Default: rownames(df)
#' @export
rowsplit <- function(df, f = rownames(df)) { # split a data frame by a factor corresponding to columns.
  ListOfDFs = NULL
  levelz = unique(f)
  for (i in 1:length(levelz)) {   ListOfDFs[[i]] = df[ which(f == levelz[i]), ]  }
  names(ListOfDFs) = levelz
  return(ListOfDFs)
}



# _______________________________________________________________________________________________
#' @title select.rows.and.columns
#' @description Subset rows and columns. It checks if the selected dimension names exist and reports if any of those they aren't found.
#' @param df PARAM_DESCRIPTION
#' @param RowIDs PARAM_DESCRIPTION, Default: NULL
#' @param ColIDs PARAM_DESCRIPTION, Default: NULL
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



# _______________________________________________________________________________________________
#' @title getRows
#' @description Get the subset of rows with existing rownames, report how much it could not find.
#' @param mat PARAM_DESCRIPTION
#' @param rownamez PARAM_DESCRIPTION
#' @param silent PARAM_DESCRIPTION, Default: FALSE
#' @param removeNAonly PARAM_DESCRIPTION, Default: FALSE
#' @param remove0only PARAM_DESCRIPTION, Default: FALSE
#' @export
getRows <- function(mat, rownamez, silent = FALSE, removeNAonly = FALSE, remove0only = FALSE ) { # Get the subset of rows with existing rownames, report how much it could not find.
  idx = intersect(rownamez, row.names(mat))
  if (removeNAonly) { idx = which_names(rowSums(!is.na(mat[ idx, ]), na.rm = TRUE) > 0) }
  if (remove0only) { idx = which_names(rowSums(mx != 0, na.rm = TRUE) > 0) }
  if (!silent) { iprint(length(idx), "/", length(rownamez), "are found. Missing: ", length(setdiff(row.names(mat), rownamez))  ) }
  mat[ idx, ]
}



# _______________________________________________________________________________________________
#' @title getCols
#' @description Get the subset of cols with existing colnames, report how much it could not find.
#' @param mat PARAM_DESCRIPTION
#' @param colnamez PARAM_DESCRIPTION
#' @param silent PARAM_DESCRIPTION, Default: FALSE
#' @param removeNAonly PARAM_DESCRIPTION, Default: FALSE
#' @param remove0only PARAM_DESCRIPTION, Default: FALSE
#' @export
getCols <- function(mat, colnamez, silent = FALSE, removeNAonly = FALSE, remove0only = FALSE ) { # Get the subset of cols with existing colnames, report how much it could not find.
  idx = intersect(colnamez, colnames(mat))
  print(symdiff(colnamez, colnames(mat)))
  if (removeNAonly) {   idx = which_names(colSums(!is.na(mat[ ,idx ]), na.rm = TRUE) > 0) }
  if (remove0only) { idx = which_names(colSums(mx != 0, na.rm = TRUE) > 0) }
  if (!silent) { iprint(length(idx), "/", length(colnamez), "are found. Missing: ", length(setdiff(colnames(mat), colnamez))  ) }
  mat[ ,idx ]
}



# _______________________________________________________________________________________________
#' @title get.oddoreven
#' @description Get odd or even columns or rows of a data frame.
#' @param df_ PARAM_DESCRIPTION, Default: NULL
#' @param rows PARAM_DESCRIPTION, Default: FALSE
#' @param odd PARAM_DESCRIPTION, Default: TRUE
#' @export
get.oddoreven <- function(df_ = NULL, rows = FALSE, odd = TRUE) { # Get odd or even columns or rows of a data frame
  counter = if (rows) NROW(df_) else NCOL(df_)
  IDX = if (odd) seq(1, to = counter, by = 2) else seq(2, to = counter, by = 2)
  df_out = if (rows) df_[IDX, ] else df_[, IDX]
  return(df_out)
}



# _______________________________________________________________________________________________
#' @title combine.matrices.intersect
#' @description Combine matrices by rownames intersect.
#' @param matrix1 PARAM_DESCRIPTION
#' @param matrix2 PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 2
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



# _______________________________________________________________________________________________
#' @title merge_dfs_by_rn
#' @description Merge any data frames by rownames. Required plyr package.
#' @param list_of_dfs PARAM_DESCRIPTION
#' @seealso
#'  \code{\link[plyr]{join_all}}
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



# _______________________________________________________________________________________________
#' @title merge_numeric_df_by_rn
#' @description Merge 2 numeric data frames by rownames.
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
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



# _______________________________________________________________________________________________
#' @title remove.na.rows
#' @description Cols have to be a vector of numbers corresponding to columns.
#' @param mat PARAM_DESCRIPTION
#' @param cols PARAM_DESCRIPTION, Default: 1:NCOL(mat)
#' @export
remove.na.rows <- function(mat, cols = 1:NCOL(mat)) { # cols have to be a vector of numbers corresponding to columns
  mat2 = mat[ , cols]
  idxOK = which(rowSums(!apply(mat2, 2, is.na)) == NCOL(mat)  )
  mat[idxOK, ]
}



# _______________________________________________________________________________________________
#' @title remove.na.cols
#' @description Cols have to be a vector of numbers corresponding to columns.
#' @param mat PARAM_DESCRIPTION
#' @export
remove.na.cols <- function(mat) { # cols have to be a vector of numbers corresponding to columns
  idxOK = !is.na(colSums(mat))
  return(mat[, idxOK])
}



# _______________________________________________________________________________________________
#' @title na.omit.mat
#' @description Omit rows with NA values from a matrix. Rows with any, or full of NA-s.
#' @param mat PARAM_DESCRIPTION
#' @param any PARAM_DESCRIPTION, Default: TRUE
#' @export
na.omit.mat <- function(mat, any = TRUE) { # Omit rows with NA values from a matrix. Rows with any, or full of NA-s
  mat = as.matrix(mat)
  stopifnot(length(dim(mat)) == 2)
  if (any) outMat = mat[ !is.na(rowSums(mat)), ]
  else outMat = mat[ (rowSums(is.na(mat)) <= ncol(mat)), ] # keep rows not full with NA
  outMat
}


# Multi-dimensional lists ----------------------------------------------------------------



# _______________________________________________________________________________________________
#' @title copy.dimension.and.dimnames
#' @description Copy dimension and dimnames.
#' @param list.1D PARAM_DESCRIPTION
#' @param obj.2D PARAM_DESCRIPTION
#' @export
copy.dimension.and.dimnames <- function(list.1D, obj.2D) { # copy dimension and dimnames
  dim(list.1D) <- dim(obj.2D)
  dimnames(list.1D) <- dimnames(obj.2D)
  list.1D
}



# _______________________________________________________________________________________________
#' @title mdlapply
#' @description lapply for multidimensional arrays.
#' @param list_2D PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @export
mdlapply <- function(list_2D, ...) { #  lapply for multidimensional arrays
  x = lapply(list_2D, ...)
  copy.dimension.and.dimnames(x,list_2D)
}



# _______________________________________________________________________________________________
#' @title arr.of.lists.2.df
#' @description Simplify 2D-list-array to a DF.
#' @param two.dim.arr.of.lists PARAM_DESCRIPTION
#' @export
arr.of.lists.2.df <- function(two.dim.arr.of.lists) { # simplify 2D-list-array to a DF
  list.1D = unlist(two.dim.arr.of.lists)
  dim(list.1D) <- dim(two.dim.arr.of.lists)
  dimnames(list.1D) <- dimnames(two.dim.arr.of.lists)
  list.1D
}



# _______________________________________________________________________________________________
#' @title mdlapply2df
#' @description Multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF).
#' @param list_2D PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @export
mdlapply2df <- function(list_2D, ...) { # multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF)
  x = lapply(list_2D, ...)
  z = copy.dimension.and.dimnames(x,list_2D)
  arr.of.lists.2.df(z)
}



# List operations -------------------------------------------------------------------------------------------------


# _______________________________________________________________________________________________
#' @title any.duplicated.rownames.ls.of.df
#' @description Check if there are any duplocated rownames in a list of dataframes.
#' @param ls PARAM_DESCRIPTION
#' @export
any.duplicated.rownames.ls.of.df <- function(ls) any.duplicated(rownames(ls)) # Check if there are any duplocated rownames in a list of dataframes.



# _______________________________________________________________________________________________
#' @title intersect.ls
#' @description Intersect any number of lists.
#' @param ls PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @export
intersect.ls <- function(ls, ...) { Reduce(intersect, ls) } # Intersect any number of lists.



# _______________________________________________________________________________________________
#' @title union.ls
#' @description Intersect any number of list elements. Faster than reduce.
#' @param ls PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @export
union.ls <- function(ls, ...) { sort(unique(do.call(c,ls))) } # Intersect any number of list elements. Faster than reduce.



# _______________________________________________________________________________________________
#' @title unlapply
#' @description Lapply, then unlist.
#' @param ... PARAM_DESCRIPTION
#' @export
unlapply <- function(...) { unlist(lapply(...)) } # lapply, then unlist



# _______________________________________________________________________________________________
#' @title list.wNames
#' @description Create a list with names from ALL variables you pass on to the function.
#' @param ... PARAM_DESCRIPTION
#' @export
list.wNames <- function(...) { # create a list with names from ALL variables you pass on to the function
  lst = list(...)
  names(lst) = as.character(match.call()[-1])
  return(lst)
}



# _______________________________________________________________________________________________
#' @title as.list.df.by.row
#' @description Split a dataframe into a list by its columns. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.
#' @param dtf PARAM_DESCRIPTION
#' @param na.omit PARAM_DESCRIPTION, Default: TRUE
#' @param zero.omit PARAM_DESCRIPTION, Default: FALSE
#' @param omit.empty PARAM_DESCRIPTION, Default: FALSE
#' @export
as.list.df.by.row <- function(dtf, na.omit = TRUE, zero.omit = FALSE, omit.empty = FALSE) { # Split a dataframe into a list by its columns. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.
  outList = as.list(as.data.frame(t( dtf ) ) )
  if (na.omit) {   outList = lapply(outList, na.omit.strip) }
  if (zero.omit) {   outList = lapply(outList, zero.omit) }
  if (omit.empty) {   outList = outList[(lapply(outList, length)) > 0] }
  print(str(outList, vec.len = 2))
  return(outList)
}



# _______________________________________________________________________________________________
#' @title as.list.df.by.col
#' @description OSplit a dataframe into a list by its rows. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.
#' @param dtf PARAM_DESCRIPTION
#' @param na.omit PARAM_DESCRIPTION, Default: TRUE
#' @param zero.omit PARAM_DESCRIPTION, Default: FALSE
#' @param omit.empty PARAM_DESCRIPTION, Default: FALSE
#' @export
as.list.df.by.col <- function(dtf, na.omit = TRUE, zero.omit = FALSE, omit.empty = FALSE) { # oSplit a dataframe into a list by its rows. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.
  outList = as.list(dtf)
  if (na.omit) {   outList = lapply(outList, na.omit.strip) }
  if (zero.omit) {   outList = lapply(outList, zero.omit) }
  if (omit.empty) {   outList = outList[(lapply(outList, length)) > 0] }
  print(str(outList, vec.len = 2))
  return(outList)
}



# _______________________________________________________________________________________________
#' @title reorder.list
#' @description Reorder elements of lists in your custom order of names / indices.
#' @param L PARAM_DESCRIPTION
#' @param namesOrdered PARAM_DESCRIPTION, Default: mixedsort(names(L))
#' @export
reorder.list <- function(L, namesOrdered = mixedsort(names(L))) { # reorder elements of lists in your custom order of names / indices.
  Lout = list(NA)
  for (x in 1:length(namesOrdered)) { Lout[[x]] = L[[namesOrdered[x] ]]  }
  if (length(names(L))) { names(Lout) = namesOrdered }
  return(Lout)
}



# _______________________________________________________________________________________________
#' @title range.list
#' @description Range of values in whole list.
#' @param L PARAM_DESCRIPTION
#' @param namesOrdered PARAM_DESCRIPTION
#' @export
range.list <- function(L, namesOrdered) { # range of values in whole list
  return(range(unlist(L), na.rm = TRUE))
}



# _______________________________________________________________________________________________
#' @title intermingle2lists
#' @description Combine 2 lists (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().
#' @param L1 PARAM_DESCRIPTION
#' @param L2 PARAM_DESCRIPTION
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



# _______________________________________________________________________________________________
#' @title as.listalike
#' @description Convert a vector to a list with certain dimensions, taken from the list it wanna resemble.
#' @param vec PARAM_DESCRIPTION
#' @param list_wannabe PARAM_DESCRIPTION
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



# _______________________________________________________________________________________________
#' @title reverse.list.hierarchy
#' @description Reverse list hierarchy.
#' @param ll PARAM_DESCRIPTION
#' @export
reverse.list.hierarchy <- function(ll) { # reverse list hierarchy
  ## https://stackoverflow.com/a/15263737
  nms <- unique(unlist(lapply(ll, function(X) names(X))))
  ll <- lapply(ll, function(X) setNames(X[nms], nms))
  ll <- apply(do.call(rbind, ll), 2, as.list)
  lapply(ll, function(X) X[!sapply(X, is.null)])
}



# _______________________________________________________________________________________________
#' @title list2fullDF.byNames
#' @description ..
#' @param your.list PARAM_DESCRIPTION, Default: list(set.1 = vec.fromNames(LETTERS[1:5], fill = 1), set.2 = vec.fromNames(LETTERS[3:9],
#'    fill = 2))
#' @param byRow PARAM_DESCRIPTION, Default: TRUE
#' @param FILL PARAM_DESCRIPTION, Default: NA
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



# _______________________________________________________________________________________________
#' @title list2fullDF.presence
#' @description Convert a list to a full matrix.  Designed for occurence counting, think tof table(). Rows = all ENTRIES of within your list, columns = names(your_list).
#' @param your.list PARAM_DESCRIPTION, Default: list(set.1 = LETTERS[1:5], set.2 = LETTERS[3:9])
#' @param byRow PARAM_DESCRIPTION, Default: TRUE
#' @param FILL PARAM_DESCRIPTION, Default: 0
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



# _______________________________________________________________________________________________
#' @title splitbyitsnames
#' @description Split a list by its names.
#' @param namedVec PARAM_DESCRIPTION
#' @export
splitbyitsnames <- function(namedVec) { # split a list by its names
  stopif(is.null(names(namedVec)), message = "NO NAMES")
  split(namedVec, f = names(namedVec))
}



# _______________________________________________________________________________________________
#' @title splititsnames_byValues
#' @description Split a list by its names.
#' @param namedVec PARAM_DESCRIPTION
#' @export
splititsnames_byValues <- function(namedVec) { # split a list by its names
  stopif(is.null(names(namedVec)), message = "NO NAMES")
  split(names(namedVec), f = namedVec)
}



# _______________________________________________________________________________________________
#' @title intermingle2vec
#' @description Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.
#' @param V1 PARAM_DESCRIPTION
#' @param V2 PARAM_DESCRIPTION
#' @param wNames PARAM_DESCRIPTION, Default: TRUE
#' @export
intermingle2vec <- function(V1, V2, wNames = TRUE) { # Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.
  stopifnot(length(V1) == length(V2) )
  Vout = c(rbind(V1, V2))
  if (wNames) {names(Vout) = c(rbind(names(V1), names(V2)))}
  return(Vout)
}



# _______________________________________________________________________________________________
#' @title intermingle.cbind
#' @description Combine 2 data frames (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().
#' @param df1 PARAM_DESCRIPTION
#' @param df2 PARAM_DESCRIPTION
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



# _______________________________________________________________________________________________
#' @title ls2categvec
#' @description Convert a list to a vector repeating list-element names, while vector names are the list elements.
#' @param your_list PARAM_DESCRIPTION
#' @export
ls2categvec <- function(your_list ) { # Convert a list to a vector repeating list-element names, while vector names are the list elements
  VEC = rep(names(your_list),unlapply(your_list, length))
  names(VEC) = unlist(your_list, use.names = TRUE)
  return(VEC)
}



# _______________________________________________________________________________________________
#' @title list.2.replicated.name.vec
#' @description Convert a list to a vector, with list elements names replicated as many times, as many elements each element had.
#' @param ListWithNames PARAM_DESCRIPTION, Default: Sections.ls.Final
#' @export
list.2.replicated.name.vec <- function(ListWithNames = Sections.ls.Final) { # Convert a list to a vector, with list elements names replicated as many times, as many elements each element had.
  NZ = names(ListWithNames)
  LZ = unlapply(ListWithNames, length)
  replicated.name.vec = rep(NZ, LZ)
  names(replicated.name.vec) = unlist(ListWithNames)
  return(replicated.name.vec)
}

## Set operations -------------------------------------------------------------------------------------------------



# _______________________________________________________________________________________________
#' @title symdiff
#' @description Quasy symmetric difference of any number of vectors.
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @export
symdiff <- function(x, y, ...) { # Quasy symmetric difference of any number of vectors
  big.vec <- c(x, y, ...)
  ls = list(x, y, ...); if ( length(ls) > 2) {print("# Not Mathematically correct, but logical for n>2 vectors: https://en.wikipedia.org/wiki/Symmetric_difference#Properties")}
  names(ls) = paste("Only in", as.character(match.call()[-1]))
  duplicates <- big.vec[duplicated(big.vec)]
  lapply(ls, function(x) setdiff(x, duplicates))
}



## Math & stats -------------------------------------------------------------------------------------------------



# _______________________________________________________________________________________________
#' @title iround
#' @description Rounds a value to the significant amount of digits. Its a wrapper for signif().
#' @param x Unrounded number.
#' @param digitz Number of digits to keep. 3 by default.
#' @export
#' @examples iround(x = 2.3232, digitz = 3)

iround <- function(x, digitz = 3) {
  signif(x, digits = digitz)
}



# _______________________________________________________________________________________________
#' @title modus
#' @description Calculates the mode (modus) of a numeric vector (it excludes NA-s by default). https://en.wikipedia.org/wiki/Mode_(statistics)
#' @param x A numeric vector
#' @import stats
#' @export
#' @examples modus(c(1, 1, 2, 3, 3, 3, 4, 5)); modus(1:4)

modus <- function(x) {
  x = unlist(na.exclude(x))
  ux <- unique(x)
  tab <- tabulate(match(x, ux));
  ux[tab == max(tab)]
}



# _______________________________________________________________________________________________
#' @title cv
#' @description ..
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
cv <- function(x, na.rm = TRUE) {
  sd( x, na.rm = na.rm)/mean(x, na.rm = na.rm)
}



# _______________________________________________________________________________________________
#' @title sem
#' @description Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default).
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
sem <-
  function(x, na.rm = TRUE)
    sd(unlist(x), na.rm = na.rm) / sqrt(length(na.omit.strip(as.numeric(x))))  # Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default)



# _______________________________________________________________________________________________
#' @title fano
#' @description Calculates the fano factor on a numeric vector (it excludes NA-s by default).
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param USE PARAM_DESCRIPTION, Default: 'na.or.complete'
#' @export
fano <-
  function(x, na.rm = TRUE, USE = "na.or.complete")
    var(x, na.rm = na.rm, use = USE) / mean(x, na.rm = na.rm) # Calculates the fano factor on a numeric vector (it excludes NA-s by default)



# _______________________________________________________________________________________________
#' @title geomean
#' @description Calculates the geometric mean of a numeric vector (it excludes NA-s by default).
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
geomean <- function(x, na.rm = TRUE) { # Calculates the geometric mean of a numeric vector (it excludes NA-s by default)
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x)) }



# _______________________________________________________________________________________________
#' @title mean_of_log
#' @description Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default).
#' @param x PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 2
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @export
mean_of_log <- function(x, k = 2, na.rm = TRUE) { # Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default)
  negs = sum(x < 0);  zeros = sum(x == 0)
  if (negs | zeros) { iprint("The input vector has", negs, "negative values and", zeros, "zeros." ) }
  mean(log(x, base = k), na.rm = na.rm) }



# _______________________________________________________________________________________________
#' @title movingAve
#' @description Calculates the moving / rolling average of a numeric vector.
#' @param x PARAM_DESCRIPTION
#' @param oneSide PARAM_DESCRIPTION, Default: 5
#' @export
movingAve <- function(x, oneSide = 5) { # Calculates the moving / rolling average of a numeric vector.
  y = NULL
  for (i in oneSide:length(x)) {
    y[i] = mean( x[ (i - oneSide):(i + oneSide) ] )
  };  return(y)
}



# _______________________________________________________________________________________________
#' @title movingAve2
#' @description Calculates the moving / rolling average of a numeric vector, using filter().
#' @param x PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION, Default: 5
#' @export
movingAve2 <- function(x, n = 5) {
  filter(x, rep(1 / n, n), sides = 2)
} # Calculates the moving / rolling average of a numeric vector, using filter().



# _______________________________________________________________________________________________
#' @title movingSEM
#' @description Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.
#' @param x PARAM_DESCRIPTION
#' @param oneSide PARAM_DESCRIPTION, Default: 5
#' @export
movingSEM <-
  function(x, oneSide = 5) {
    # Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.
    y = NULL
    for (i in oneSide:length(x)) {
      y[i] = sem(x[(i - oneSide):(i + oneSide)])
    }
    return(y)
  }



# _______________________________________________________________________________________________
#' @title imovingSEM
#' @description Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller window-size.
#' @param x PARAM_DESCRIPTION
#' @param oneSide PARAM_DESCRIPTION, Default: 5
#' @export
imovingSEM <- function(x, oneSide = 5) {
  # Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller window-size.
  y = NULL
  for (i in 1:length(x)) {
    oneSideDynamic = min(i - 1, oneSide, length(x) - i)
    oneSideDynamic
    indexx = (i - oneSideDynamic):(i + oneSideDynamic)
    indexx
    y[i] = sem(x[indexx])
  }
  return(y)
}


# _______________________________________________________________________________________________
#' @title shannon.entropy
#' @description Calculate shannon entropy.
#' @param p PARAM_DESCRIPTION
#' @export
shannon.entropy <- function(p) {
  # Calculate shannon entropy
  if (min(p) < 0 || sum(p) <= 0)
    return(NA)
  p.norm <- p[p > 0] / sum(p) - sum(log2(p.norm) * p.norm)
}


