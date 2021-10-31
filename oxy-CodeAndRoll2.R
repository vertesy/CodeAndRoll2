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
# -  Aliases for often used functions
# -  Generic functions
# -  File handling, export, import [read & write]
#   - Clipboard interaction (OS X)
#   - Reading files in
#   - Writing files out
# - Create and check variables
# -  Vector operations
#   - Vector filtering
# -  String operations
# -  Matrix operations
#   - Matrix filtering
# -  List operations
# -  Set operations
# -  Math and stats
# -  Plotting and Graphics
# -  Clustering heatmap tools
# -  Search query links
# -  Biology


# -  Generic
# -  New additions

wA4 = 8.27 # A4 inches
hA4 = 11.69


## Setup   -------------------------------------------------------------------------------------------------
# pdf.options(title = paste0('Copyright Abel Vertesy ', Sys.Date())) # Setup to your own name
debuggingState(on = FALSE)
# "gtools", "readr", "gdata", "colorRamps", "grDevices", "plyr"
print("Depends on MarkdownReports, gtools, readr, gdata, clipr. Some functions depend on other libraries.")

### Load the MarkdownReports Library -------------------------------------------------------------------------------------------------
# source("~/Github/MarkdownReports/MarkdownReports/R/MarkdownReports.R")
# try(require("MarkdownReports"))
# try(require("gtools"))
# try(ggplot2::theme_set( theme_bw()), silent = TRUE)



# Alisases ----------------

# Alisases --------------------------------------------------------------------------------

#' @title Alias for gtools::mixedsort.
#' @description Alias for gtools::mixedsort.
#' @rdname sort.natural
#' @export
sort.natural = gtools::mixedsort


#' @title Alias for paste0.
#' @description Alias for paste0.
#' @rdname p0
#' @export
p0 = paste0


#' @title Alias for paste0.
#' @description Alias for paste0.
#' @rdname l
#' @export
l = length


#' @title Paste by point separator.
#' @description Paste by point separator.
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#' ppp("Apple",2:3, letters[3:5])
#' }
#' @rdname ppp
#' @export
#'

ppp <- function(...) { paste(..., sep = '.') }



#' @title Paste by (forward) slash separator.
#' @description Paste by (forward) slash separator.
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  pps("Apple",2:3, letters[3:5])
#'  }
#' }
#' @rdname pps
#' @export

pps <- function(...) { paste(..., sep = '/') }



#' @title Paste by underscore separator.
#' @description Paste by underscore separator.
#' @param ... Provide here any string or simple numeric variables, they will be pasted together.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  ppu("Apple",2:3, letters[3:5])
#'  }
#' }
#' @rdname ppu
#' @export

ppu <- function(...) { paste(..., sep = '_') }



#' @title Paste by dash separator.
#' @description Paste by dash separator.
#' @param ... Provide here any string or simple numeric variables, they will be pasted together.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  ppd("Apple",2:3, letters[3:5])
#'  }
#' }
#' @rdname ppd
#' @export

ppd <- function(...) { paste(..., sep = '-') }




#' @title Paste and  collapse by point separator.
#' @description Paste and  collapse by point separator.
#' @param ... Provide here any string or simple numeric variables, they will be pasted and collapsed together.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  kpp("Apple",2:3, letters[3:5])
#'  }
#' }
#' @rdname kpp
#' @export

kpp <- function(...) { paste(..., sep = '.', collapse = '.') }



#' @title Paste and  collapse by underscore separator.
#' @description Paste and  collapse by underscore separator.
#' @param ... Provide here any string or simple numeric variables, they will be pasted and collapsed together.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  kppu("Apple",2:3, letters[3:5])
#'  }
#' }
#' @rdname kppu
#' @export

kppu <- function(...) { paste(..., sep = '_',  collapse = '_') }



#' @title Paste and  collapse by (forward) slash separator.
#' @description Paste and  collapse by (forward) slash separator.
#' @param ... Provide here any string or simple numeric variables, they will be pasted and collapsed together.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  kpps("Apple",2:3, letters[3:5])
#'  }
#' }
#' @rdname kpps
#' @export

kpps <- function(...) { paste(..., sep = '/', collapse = '/') }



#' @title Paste and collapse by dash separator.
#' @description Paste and collapse by dash separator.
#' @param ... Provide here any string or simple numeric variables, they will be pasted and collapsed together.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  kppd("Apple",2:3, letters[3:5])
#'  }
#' }
#' @rdname kppd
#' @export

kppd <- function(...) { paste(..., sep = '-', collapse = '-') }





#' @title Silent try.
#' @description Silent try.
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname stry
#' @export

stry <- function(...) {try(..., silent = T)}





## Generic -------------------------------------------------------------------------------------------------

#' @title Stop script if the condition is met. You can parse anything (e.g. variables) in the message.
#' @description Stop script if the condition is met. You can parse anything (e.g. variables) in the message.
#' @param condition PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname stopif2
#' @export


stopif2 <- function(condition, ...) { if (condition) {iprint(...); stop()} }


#' @title Use system voice to notify (after a long task is done).
#' @description Use system voice to notify (after a long task is done).
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname say
#' @export


say <- function(...) {
  sys <- Sys.info()["sysname"]
  if (sys == "Darwin") system("say Ready")
  if (sys == "Linux") system("echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'; sleep 0.5s; echo -e '\a'")  # For UNIX servers.
}
#' @title Use system voice to notify (after a long task is done).
#' @description Use system voice to notify (after a long task is done).
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sayy
#' @export


sayy <- function(...) {system("say Ready to roll")}



#' @title Grep returning the value.
#' @description Grep returning the value.
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
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname grepv
#' @export


grepv <- function(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE
                  , invert = FALSE, ...) grep(pattern, x, ignore.case = ignore.case, perl = perl, fixed = fixed
                                              , useBytes = useBytes, invert = invert, ..., value = TRUE)

### MarkdownReports Helpers ----------------------------------------------------------------
#' @title Open current working directory.
#' @description Open current working directory.

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname oo
#' @export


oo <- function() {
  system("open .")
}


#' @title Unload a package.
#' @description Unload a package. Source: https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r.
#' @param pkg PARAM_DESCRIPTION
#' @param character.only PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname unload
#' @export


unload <- function(pkg, character.only = FALSE) {
  if (!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while (search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

#' @title Show the most frequent elements of a table.
#' @description Show the most frequent elements of a table.
#' @param vec PARAM_DESCRIPTION
#' @param topN PARAM_DESCRIPTION, Default: 10
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname most_frequent_elements
#' @export


most_frequent_elements <- function(vec, topN = 10) {
  tail(sort(table(vec, useNA = "ifany")), topN)
}

#' @title Returns the position / index of the n highest values. For equal values, it maintains the original order.
#' @description Returns the position / index of the n highest values. For equal values, it maintains the original order.
#' @param x PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION, Default: 3
#' @param top PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname top_indices
#' @export


top_indices <- function(x, n = 3, top = TRUE) {
  head( order(x, decreasing = top), n )
}

#' @title Calculate what is the actual value of the N-th percentile in a distribution or set of numbers. Useful for calculating cutoffs, and displaying them by whist()'s "vline" paramter.
#' @description Calculate what is the actual value of the N-th percentile in a distribution or set of numbers. Useful for calculating cutoffs, and displaying them by whist()'s "vline" paramter.
#' @param distribution PARAM_DESCRIPTION
#' @param percentile PARAM_DESCRIPTION, Default: 0.95
#' @param FirstValOverPercentile PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname percentile2value
#' @export


percentile2value <- function(distribution, percentile = 0.95, FirstValOverPercentile = TRUE) {
  index = percentile * length(distribution)
  if (FirstValOverPercentile) { index = ceiling(index)
  } else {index = floor(index) }
  value = sort(distribution)[index]
  return(value)
}

#' @title Print at every e.g. 1000.
#' @description Report at every e.g. 1000.
#' @param i PARAM_DESCRIPTION
#' @param N PARAM_DESCRIPTION, Default: 1000
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname printEveryN
#' @export


printEveryN <- function(i, N = 1000) { if ((i %% N) == 0 ) iprint(i) }

#' @title Load a package. If it does not exist, try to install it from CRAN.
#' @description Load a package. If it does not exist, try to install it from CRAN.
#' @param package PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname irequire
#' @export


irequire <- function(package) { package_ = as.character(substitute(package)); print(package_);
if (!require(package = package_,  character.only = TRUE)) {
  print("Not Installed yet.");install.packages(pkgs = package_);
  Sys.sleep(1)
  print("Loading package:")
  require(package = package_, character.only = TRUE)
}
}  # install package if cannot be loaded

#' @title Parse current date, dot separated.
#' @description Parse current date, dot separated.
#' @param Format PARAM_DESCRIPTION, Default: c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname idate
#' @export


idate <- function(Format = c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]) { format(Sys.time(), format = Format ) }

#' @title View the head of an object by console.
#' @description View the head of an object by console.
#' @param matrix PARAM_DESCRIPTION
#' @param enn PARAM_DESCRIPTION, Default: 10
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname view.head
#' @export


view.head <- function(matrix, enn = 10) { matrix[1:min(NROW(matrix), enn), 1:min(NCOL(matrix), enn)] }


#' @title View the head of an object by View().
#' @description View the head of an object by View().
#' @param matrix PARAM_DESCRIPTION
#' @param enn PARAM_DESCRIPTION, Default: 10
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname view.head2
#' @export


view.head2 <- function(matrix, enn = 10) { View(head(matrix, n = min(NROW(matrix), NCOL(matrix), enn))) }

#' @title Test if names of two objects for being exactly equal.
#' @description Test if names of two objects for being exactly equal.
#' @param v1 PARAM_DESCRIPTION
#' @param v2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname iidentical.names
#' @export


iidentical.names <- function(v1, v2) {
  nv1 = names(v1)
  nv2 = names(v2)
  len.eq = (length(nv1) == length(nv2))
  if (!len.eq) iprint("Lenghts differ by:", (length(nv1) - length(nv2)) )
  Check = identical(nv1, nv2)
  if (!Check) {
    diff = setdiff(nv1, nv2)
    ld = length(diff)
    iprint(ld, "elements differ: ", head(diff))
  }
  Check
}

#' @title Test if two objects for being exactly equal.
#' @description Test if two objects for being exactly equal.
#' @param v1 PARAM_DESCRIPTION
#' @param v2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname iidentical
#' @export


iidentical <- function(v1, v2) {
  len.eq = (length(v1) == length(v2))
  if (!len.eq) iprint("Lenghts differ by:", (length(v1) - length(v2)) )
  Check = identical(v1,v2)
  if (!Check) {
    diff = setdiff(v1, v2)
    ld = length(diff)
    iprint(ld, "elements differ: ", head(diff))
  }
  Check
}

#' @title Test if two objects for being exactly equal.
#' @description Test if two objects for being exactly equal.
#' @param li PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname iidentical.all
#' @export


iidentical.all <- function(li) all(sapply(li, identical, li[[1]]))


#' @title Checks if a variable is defined, and its value is TRUE.
#' @description Internal function. Checks if a variable is defined, and its value is TRUE.
#' @param name PARAM_DESCRIPTION, Default: 'pi'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname IfExistsAndTrue
#' @export


IfExistsAndTrue <- function(name = "pi" ) {
  x = FALSE
  if (exists(name)) {
    if (isTRUE(get(name)))  {x = TRUE} else {x = FALSE; iprint(name, " exists, but != TRUE; ", get(name))}
  }
  return(x)
}

#' @title Show distribution of the largest objects and return their names.
#' @description  Show distribution of the largest objects and return their names. Modified from: https://stackoverflow.com/questions/17218404/should-i-get-a-habit-of-removing-unused-variables-in-r.
#' @param n PARAM_DESCRIPTION, Default: 5
#' @param saveplot PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'   memory.biggest.objects()
#'  }
#' }
#' @rdname memory.biggest.objects
#' @export


memory.biggest.objects <- function(n = 5, saveplot = F) {
  try.dev.off()
  gc()
  ls.mem <- ls( envir = .GlobalEnv)
  ls.obj <- lapply(ls.mem, get)
  Sizes.of.objects.in.mem <- unlapply(ls.obj, object.size)
  names(Sizes.of.objects.in.mem) <- ls.mem
  topX = sort(Sizes.of.objects.in.mem,decreasing = TRUE)[1:n]

  Memorty.usage.stat = c(topX, 'Other' = sum(sort(Sizes.of.objects.in.mem,decreasing = TRUE)[-(1:n)]))
  pie(Memorty.usage.stat, cex = .5, sub = make.names(date()))
  try(qpie(Memorty.usage.stat, w = 7,  ), silent = T)
  # Use wpie if you have MarkdownReports, from https://github.com/vertesy/MarkdownReports
  dput(names(topX))
  iprint("rm(list = c( 'objectA',  'objectB'))")
  # inline_vec.char(names(topX))
  # Use inline_vec.char if you have DataInCode, from https://github.com/vertesy/DataInCode
}



## File handling, export, import [read & write] -------------------------------------------------------------------------------------------------

### Reading files in -------------------------------------------------------------------------------------------------
#' @title Read each line of a file to an element of a vector.
#' @description Read each line of a file to an element of a vector (read in new-line separated values, no header!).
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname read.simple.vec
#' @export


read.simple.vec <- function(...) {
  pfn = kollapse(...) # merge path and filename
  read_in = as.vector(unlist(read.table( pfn , stringsAsFactors = FALSE, sep = "\n" )) )
  iprint(length(read_in), "elements")
  return(read_in);
}

#' @title It is essentially read.table() with file/path parsing.
#' @description It is essentially read.table() with file/path parsing.
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname read.simple
#' @export


read.simple <- function(...) {
  pfn = kollapse(...) # merge path and filename
  read_in = read.table( pfn , stringsAsFactors = FALSE)
  return(read_in)
}

#' @title Read in a file.
#' @description Read in a file (table).
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname read.simple_char_list
#' @export


read.simple_char_list <- function(...) {
  pfn = kollapse(...) # merge path and filename
  read_in = unlist(read.table( pfn , stringsAsFactors = FALSE ) )
  iprint("New variable head: ", what(read_in))
  return(read_in)
}

#' @title Read in a table.
#' @description  Read in a table. Default: header defines colnames, no rownames. For rownames give the col nr. with rownames, eg. 1 The header should start with a TAB / First column name should be empty.
#' @param ... PARAM_DESCRIPTION
#' @param colnames PARAM_DESCRIPTION, Default: TRUE
#' @param coltypes PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @rdname read.simple.table
#' @export
#' @importFrom readr read_tsv
#' @importFrom gtools na.replace


read.simple.table <- function(..., colnames = TRUE, coltypes = NULL) {
  pfn = kollapse(...) # merge path and filename
  # read_in = read.table( pfn , stringsAsFactors = FALSE, sep = "\t", header = colnames )
  read_in = readr::read_tsv( pfn, col_names = colnames, col_types = coltypes )
  iprint("New variable dim: ", dim(read_in))
  read_in = as.data.frame(gtools::na.replace(data.matrix(read_in), replace = 0))
  return(read_in)
}

#' @title Set First Col to Row Names.
#' @description Set First Col to Row Names.
#' @param Tibble PARAM_DESCRIPTION
#' @param rownamecol PARAM_DESCRIPTION, Default: 1
#' @param make_names PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname FirstCol2RowNames
#' @export


FirstCol2RowNames <- function(Tibble, rownamecol = 1, make_names = FALSE) {
  Tibble = as.data.frame(Tibble)
  NN = Tibble[[rownamecol]]
  rownames(Tibble) = if (make_names) make.names(NN, unique = TRUE) else NN
  return(Tibble[, -rownamecol, drop = F])
}

#' @title Read in a table with comma separeted values: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
#' @description  Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
#' @param ... PARAM_DESCRIPTION
#' @param sep_ PARAM_DESCRIPTION, Default: '  '
#' @param colnames PARAM_DESCRIPTION, Default: TRUE
#' @param wRownames PARAM_DESCRIPTION, Default: TRUE
#' @param coltypes PARAM_DESCRIPTION, Default: NULL
#' @param NaReplace PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @rdname read.simple.tsv
#' @export
#' @importFrom readr read_tsv
#' @importFrom gtools na.replace


read.simple.tsv <- function(..., sep_ = "\t", colnames = TRUE, wRownames = TRUE, coltypes = NULL, NaReplace = TRUE) {
  pfn = kollapse(...) # merge path and filename
  # read_in = read.delim( pfn , stringsAsFactors = FALSE, sep = , sep_, row.names = 1, header = TRUE )
  read_in = suppressWarnings(readr::read_tsv( pfn, col_names = colnames, col_types = coltypes ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
  return(read_in)
}


#' @title Read in a table with comma separeted values. Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
#' @description Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
#' @param ... PARAM_DESCRIPTION
#' @param colnames PARAM_DESCRIPTION, Default: TRUE
#' @param coltypes PARAM_DESCRIPTION, Default: NULL
#' @param wRownames PARAM_DESCRIPTION, Default: TRUE
#' @param NaReplace PARAM_DESCRIPTION, Default: TRUE
#' @param nmax PARAM_DESCRIPTION, Default: Inf
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @rdname read.simple.csv
#' @export
#' @importFrom readr read_csv
#' @importFrom gtools na.replace


read.simple.csv <- function(...,  colnames = TRUE, coltypes = NULL, wRownames = TRUE, NaReplace = TRUE, nmax = Inf) {
  pfn = kollapse(...) # merge path and filename
  read_in = suppressWarnings(readr::read_csv( pfn, col_names = colnames, col_types = coltypes, n_max = nmax ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
  return(read_in)
}

#' @title Read in a file with space separeted values. Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
#' @description Space separeted values. Read in a file with excel style data: rownames in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
#' @param ... PARAM_DESCRIPTION
#' @param sep_ PARAM_DESCRIPTION, Default: ' '
#' @param colnames PARAM_DESCRIPTION, Default: TRUE
#' @param wRownames PARAM_DESCRIPTION, Default: TRUE
#' @param NaReplace PARAM_DESCRIPTION, Default: TRUE
#' @param coltypes PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[gtools]{na.replace}}
#' @rdname read.simple.ssv
#' @export
#' @importFrom readr read_delim
#' @importFrom gtools na.replace


read.simple.ssv <- function(..., sep_ = " ", colnames = TRUE, wRownames = TRUE, NaReplace = TRUE, coltypes = NULL) {
  pfn = kollapse(...) # merge path and filename
  read_in = suppressWarnings(readr::read_delim( pfn, delim = sep_, col_names = colnames, col_types = coltypes ))
  iprint("New variable dim: ", dim(read_in) - 0:1)
  if (wRownames) { read_in = FirstCol2RowNames(read_in) }
  if (NaReplace) { read_in = as.data.frame(gtools::na.replace(read_in, replace = 0)) }
  return(read_in)
}


#' @title Read in a file with tab separeted values.
#' @description  Read in a file with excel style named vectors, names in col1, headers SHIFTED. The header should start with a TAB / First column name should be empty.
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{read_delim}}
#' @rdname read.simple.tsv.named.vector
#' @export
#' @importFrom readr read_tsv


read.simple.tsv.named.vector <- function(...) {
  pfn = kollapse(...) # merge path and filename
  # read_in = read.delim( pfn , stringsAsFactors = FALSE, sep = sep_, row.names = 1, header = TRUE )
  read_in = readr::read_tsv( pfn )
  vect = read_in[[2]]
  names(vect) = read_in[[1]]
  iprint("New vectors length is: ", length(vect))
  return(vect)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df_by_read.simple.tsv PARAM_DESCRIPTION, Default: x
#' @param digitz PARAM_DESCRIPTION, Default: 2
#' @param na_rep PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[gtools]{na.replace}}
#' @rdname convert.tsv.data
#' @export
#' @importFrom gtools na.replace
Fix NA issue in dataframes imported by the new read.simple.tsv. Set na_rep to NA if you want to keep NA-s

convert.tsv.data <- function(df_by_read.simple.tsv = x, digitz = 2, na_rep = 0 ) {
  DAT = data.matrix(df_by_read.simple.tsv)
  SNA = sum(is.na(DAT))
  try(iprint("Replaced NA values:", SNA, "or", percentage_formatter(SNA/length(DAT))), silent = TRUE)
  gtools::na.replace(round(DAT, digits = digitz), replace = na_rep)
}



#' @title Read multi-sheet excel files.
#' @description Read multi-sheet excel files. row_namePos = NULL for automatic names Look into: http://readxl.tidyverse.org/.
#' @param pfn PARAM_DESCRIPTION, Default: kollapse(...)
#' @param row_namePos PARAM_DESCRIPTION, Default: NULL
#' @param ... PARAM_DESCRIPTION
#' @param header_ PARAM_DESCRIPTION, Default: TRUE
#' @param WhichSheets PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[gdata]{read.xls}}
#' @rdname read.simple.xls
#' @export
#' @importFrom gdata read.xls


read.simple.xls <- function(pfn = kollapse(...), row_namePos = NULL, ..., header_ = TRUE, WhichSheets) {
  if (!require("gdata")) { print("Please install gplots: install.packages('gdata')") }
  if (grepl("^~/", pfn)) {
    iprint("You cannot use the ~/ in the file path! It is replaced by '/Users/abel.vertesy/'.")
    pfn = gsub(pattern = "^~/", replacement = "/Users/abel.vertesy/", x = pfn)
  } else {print(pfn)}

  if (!require("gdata")) { print("Please install gplots: install.packages('gdata')") }
  # merge path and filename
  TheSheetNames = sheetNames(pfn, verbose = FALSE);
  NrSheets = length(TheSheetNames)
  iprint(NrSheets, "sheets in the file.")
  ExpData = list.fromNames(TheSheetNames)
  RangeOfSheets = if (missing(WhichSheets)) 1:NrSheets else WhichSheets
  for (i in RangeOfSheets ) {
    iprint("sheet", i)
    ExpData[[i]] = gdata::read.xls(pfn, sheet = i, row.names = row_namePos, header = header_)
  } #for
  lapply(ExpData, function(x) print(dimnames(x)) )
  return(ExpData);
}

#' @title Source parts of another script.
#' @description Source parts of another script. Source: https://stackoverflow.com/questions/26245554/execute-a-set-of-lines-from-another-r-file.
#' @param fn PARAM_DESCRIPTION
#' @param startTag PARAM_DESCRIPTION, Default: '#1'
#' @param endTag PARAM_DESCRIPTION, Default: '#/1'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sourcePartial
#' @export


sourcePartial <- function(fn,startTag = '#1', endTag = '#/1') {
  lines <- scan(fn, what = character(), sep = "\n", quiet = TRUE)
  st <- grep(startTag,lines)
  en <- grep(endTag,lines)
  tc <- textConnection(lines[(st + 1):(en - 1)])
  source(tc)
  close(tc)
}



### Writing files out -------------------------------------------------------------------------------------------------

#' @title Write out a matrix-like R-object to a file with as tab separated values (.tsv).
#' @description  Write out a matrix-like R-object to a file with as tab separated values (.tsv). Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.
#' @param input_df PARAM_DESCRIPTION
#' @param extension PARAM_DESCRIPTION, Default: 'tsv'
#' @param ManualName PARAM_DESCRIPTION, Default: ''
#' @param o PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname write.simple
#' @export


write.simple <- function(input_df, extension = 'tsv', ManualName = "", o = FALSE, ...  ) {
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_vec) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP = ww.FnP_parser(fname, extension) }
  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
  iprint("Length: ", length(input_df))
} # fun

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param input_vec PARAM_DESCRIPTION
#' @param extension PARAM_DESCRIPTION, Default: 'vec'
#' @param ManualName PARAM_DESCRIPTION, Default: ''
#' @param o PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname write.simple.vec
#' @export
Write out a vector-like R-object to a file with as newline separated values (.vec). Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.

write.simple.vec <- function(input_vec, extension = 'vec', ManualName = "", o = FALSE, ... ) {
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_vec) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, extension) }
  write.table(input_vec, file = FnP, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE  )
  iprint("Length: ", length(input_vec))
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param named_list PARAM_DESCRIPTION
#' @param ManualName PARAM_DESCRIPTION, Default: ''
#' @param o PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @param TabColor PARAM_DESCRIPTION, Default: 'darkgoldenrod1'
#' @param Creator PARAM_DESCRIPTION, Default: 'Vertesy'
#' @param HeaderCex PARAM_DESCRIPTION, Default: 12
#' @param HeaderLineColor PARAM_DESCRIPTION, Default: 'darkolivegreen3'
#' @param HeaderCharStyle PARAM_DESCRIPTION, Default: c("bold", "italic", "underline")[1]
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{write.xlsx}}
#' @rdname write.simple.xlsx
#' @export
#' @importFrom openxlsx write.xlsx
Write out a list of matrices/ data frames WITH ROW- AND COLUMN- NAMES to a file with as an Excel (.xslx) file. Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.

write.simple.xlsx <- function(named_list, ManualName = "", o = FALSE,  ..., TabColor = "darkgoldenrod1", Creator = "Vertesy"
                              HeaderCex = 12, HeaderLineColor = "darkolivegreen3", HeaderCharStyle = c("bold", "italic", "underline")[1]  ) {
  irequire(openxlsx)
  fname = if (nchar(ManualName) < 2 ) { fname = substitute(named_list) }
  if (nchar(ManualName)) {FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, "xlsx") }

  hs <- createStyle(textDecoration = HeaderCharStyle, fontSize = HeaderCex, fgFill = HeaderLineColor)
  setwd(OutDir)
  openxlsx::write.xlsx(named_list, file = ppp(fname,"xlsx"), rowNames = TRUE, firstRow = TRUE, firstCol = TRUE, colWidths = "auto"
                       , headerStyle = hs, tabColour = TabColor, creator = Creator) #

  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param input_df PARAM_DESCRIPTION
#' @param extension PARAM_DESCRIPTION, Default: 'tsv'
#' @param ManualName PARAM_DESCRIPTION, Default: ''
#' @param o PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname write.simple.append
#' @export
Append an R-object WITHOUT ROWNAMES, to an existing .tsv file of the same number of columns. Your output filename will be either the variable's name. The output file will be located in "OutDir" specified by you at the beginning of the script, or under your current working directory. You can pass the PATH and VARIABLE separately (in order), they will be concatenated to the filename.

write.simple.append <- function(input_df, extension = 'tsv', ManualName = "", o = FALSE, ... ) {
  fname = kollapse(...) ; if (nchar(fname) < 2 ) { fname = substitute(input_df) }
  if (nchar(ManualName)) { FnP = kollapse(ManualName)} else  {FnP =  ww.FnP_parser(fname, extension) }
  write.table(input_df, file = FnP, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE  )
  if (o) { system(paste0("open ", FnP), wait = FALSE) }
} # fun


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param filename PARAM_DESCRIPTION
#' @param r PARAM_DESCRIPTION, Default: 225
#' @param q PARAM_DESCRIPTION, Default: 90
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname jjpegA4
#' @export
Setup an A4 size jpeg

jjpegA4 <- function(filename, r = 225, q = 90) {
  jpeg(file = filename,width = wA4, height = hA4, units = 'in', quality = q,res = r)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname extPDF
#' @export
add pdf as extension to a file name

extPDF <- function(vec) ppp(vec, "pdf")

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname extPNG
#' @export
add png as extension to a file name

extPNG <- function(vec) ppp(vec, "png")


### Clipboard interaction -------------------------------------------------------------------------------------------------
# https://github.com/vertesy/DataInCode
# try(source("~/Github/TheCorvinas/R/DataInCode/DataInCode.R"), silent = FALSE)

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[clipr]{read_clip}}, \code{\link[clipr]{write_clip}}
#'  \code{\link[utils]{capture.output}}
#' @rdname clip2clip.vector
#' @export
#' @importFrom clipr read_clip write_clip
#' @importFrom utils capture.output
Copy from clipboard (e.g. excel) to a R-formatted vector to the  clipboard

clip2clip.vector <- function() {
  x = dput(clipr::read_clip() )
  clipr::write_clip(
    utils::capture.output(x)
  )
  print(x)
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[clipr]{read_clip}}, \code{\link[clipr]{write_clip}}
#'  \code{\link[utils]{capture.output}}
#' @rdname clip2clip.commaSepString
#' @export
#' @importFrom clipr read_clip write_clip
#' @importFrom utils capture.output
Read a comma separated string (e.g. list of gene names) and properly format it for R.

clip2clip.commaSepString <- function() {
  x = unlist(strsplit(clipr::read_clip(), split = ','))
  clipr::write_clip(
    utils::capture.output(x)
  )
  print(x)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param var PARAM_DESCRIPTION, Default: df.markers
#' @param decimal_mark PARAM_DESCRIPTION, Default: ','
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname write_clip.replace.dot
#' @export
Clipboard export for da wonderful countries with where "," is the decimal

write_clip.replace.dot <- function(var = df.markers, decimal_mark = ',') {
  write_clip(format(var, decimal.mark = decimal_mark) )
}
# write_clip.replace.dot(df_markers)

## Create and check variables -------------------------------------------------------------------------------------------------

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param name_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname vec.fromNames
#' @export
create a vector from a vector of names

vec.fromNames <- function(name_vec = LETTERS[1:5], fill = NA) {
  v = numeric(length(name_vec))
  if (length(fill) == 1) {v = rep(fill, length(name_vec))}
  else if (length(fill == length(name_vec))) {v = fill}
  names(v) = name_vec
  return(v)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param name_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NaN
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list.fromNames
#' @export
create list from a vector with the names of the elements

list.fromNames <- function(name_vec = LETTERS[1:5], fill = NaN) {
  liszt = as.list(rep(fill, length(name_vec)))
  names(liszt) = name_vec
  return(liszt)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param rowname_vec PARAM_DESCRIPTION, Default: 1:10
#' @param colname_vec PARAM_DESCRIPTION, Default: LETTERS[1:5]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname matrix.fromNames
#' @export
Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.

matrix.fromNames <- function(rowname_vec = 1:10, colname_vec = LETTERS[1:5], fill = NA) {
  mx = matrix(data = fill, nrow = length(rowname_vec), ncol = length(colname_vec), dimnames = list(rowname_vec, colname_vec))
  iprint("Dimensions:", dim(mx))
  return(mx)
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vector PARAM_DESCRIPTION, Default: 1:5
#' @param HowManyTimes PARAM_DESCRIPTION, Default: 3
#' @param IsItARow PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname matrix.fromVector
#' @export
Create a matrix from values in a vector repeated for each column / each row. Similar to rowNameMatrix and colNameMatrix.

matrix.fromVector <- function(vector = 1:5, HowManyTimes = 3, IsItARow = TRUE) {
  matt = matrix(vector, nrow = length(vector), ncol = HowManyTimes)
  if ( !IsItARow ) {matt = t(matt)}
  return(matt)
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param rowname_vec PARAM_DESCRIPTION, Default: 1:3
#' @param colname_vec PARAM_DESCRIPTION, Default: letters[1:2]
#' @param z_name_vec PARAM_DESCRIPTION, Default: LETTERS[4:6]
#' @param fill PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname array.fromNames
#' @export
create an N-dimensional array from N vectors defining the row-, column, etc names of the array

array.fromNames <- function(rowname_vec = 1:3, colname_vec = letters[1:2], z_name_vec = LETTERS[4:6], fill = NA) {
  DimNames = list(rowname_vec, colname_vec, z_name_vec)
  Dimensions_ = lapply(DimNames, length)
  mx = array(data = fill, dim = Dimensions_, dimnames = DimNames)
  iprint("Dimensions:", dim(mx))
  return(mx)
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param printme PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname what
#' @export
A better version of is(). It can print the first "printme" elements.

what <- function(x, printme = 0) {
  iprint(is(x), "; nr. of elements:", length(x))
  if ( is.numeric(x) )    { iprint("min&max:", range(x) ) } else {print("Not numeric")}
  if ( length(dim(x) ) > 0 )  { iprint("Dim:", dim(x) ) }
  if ( printme > 0)       { iprint("Elements:", x[0:printme] ) }
  head(x)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param any_object PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname idim
#' @export
A dim() function that can handle if you pass on a vector: then, it gives the length.

idim <- function(any_object) {
  if (is.null(dim(any_object))) {
    if (is.list(any_object)) { print("list") } #if
    print(length(any_object))
  }
  else { print(dim(any_object))  }
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param any_object PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname idimnames
#' @export
A dimnames() function that can handle if you pass on a vector: it gives back the names.

idimnames <- function(any_object) {
  if (!is.null(dimnames(any_object)))   { print(dimnames(any_object)) }
  else if (!is.null(colnames(any_object))) { iprint("colnames:", colnames(any_object))  }
  else if (!is.null(rownames(any_object))) { iprint("rownames:", rownames(any_object))  }
  else if (!is.null(names(any_object))) { iprint("names:", names(any_object)) }
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vector PARAM_DESCRIPTION
#' @param categories_vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname table_fixed_categories
#' @export
generate a table() with a fixed set of categories. It fills up the table with missing categories, that are relevant when comparing to other vectors.

table_fixed_categories <- function(vector, categories_vec) {
  if ( !is.vector(vector)) {print(is(vector[]))}
  table(factor(unlist(vector), levels = categories_vec))
}

## Vector operations -------------------------------------------------------------------------------------------------

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @param N PARAM_DESCRIPTION, Default: 10
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname trail
#' @export
A combination of head() and tail() to see both ends.

trail <- function(vec, N = 10) c(head(vec, n = N), tail(vec, n = N) )

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sort.decreasing
#' @export
Sort in decreasing order.

sort.decreasing <- function(vec) sort(vec, decreasing = TRUE)

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param string PARAM_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION, Default: '_'
#' @param n PARAM_DESCRIPTION, Default: 2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_split}}
#' @rdname sstrsplit
#' @export
#' @importFrom stringr str_split_fixed
Alias for str_split_fixed in the stringr package

sstrsplit <- function(string, pattern = "_", n = 2) { stringr::str_split_fixed(string, pattern = pattern, n = n) }

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df_Col PARAM_DESCRIPTION, Default: as.named.vector(df[, 1, drop = FALSE])
#' @param n PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname topN.dfCol
#' @export
Find the n highest values in a named vector

topN.dfCol <- function(df_Col = as.named.vector(df[ , 1, drop = FALSE]), n = 5)   { head(sort(df_Col, decreasing = TRUE), n = n) }
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df_Col PARAM_DESCRIPTION, Default: as.named.vector(df[, 1, drop = FALSE])
#' @param n PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname bottomN.dfCol
#' @export
Find the n lowest values in a named vector

bottomN.dfCol <- function(df_Col = as.named.vector(df[ , 1, drop = FALSE]), n = 5) { head(sort(df_Col, decreasing = FALSE), n = n) }


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df_col PARAM_DESCRIPTION
#' @param WhichDimNames PARAM_DESCRIPTION, Default: 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.named.vector
#' @export
Convert a dataframe column or row into a vector, keeping the corresponding dimension name.

as.named.vector <- function(df_col, WhichDimNames = 1) {
  # use RowNames: WhichDimNames = 1 , 2: use ColNames
  # !!! might require drop = FALSE in subsetting!!! eg: df_col[, 3, drop = FALSE]
  # df_col[which(unlist(lapply(df_col, is.null)))] = "NULL" # replace NULLs - they would fall out of vectors - DOES not work yet
  namez = dimnames(df_col)[[WhichDimNames]]
  if (is.list(df_col) & !is.data.frame(df_col)) {namez = names(df_col)}
  vecc = as.vector(unlist(df_col))
  names(vecc) = namez
  return(vecc)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname col2named.vector
#' @export
Convert a dataframe column into a vector, keeping the corresponding dimension name.

col2named.vector <- function(df_col) {
  namez = rownames(df_col)
  vecc = as.vector(unlist(df_col))
  names(vecc) = namez
  return(vecc)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df_row PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname row2named.vector
#' @export
Convert a dataframe row into a vector, keeping the corresponding dimension name.

row2named.vector <- function(df_row) {
  namez = colnames(df_row)
  vecc = as.vector(unlist(df_row))
  names(vecc) = namez
  return(vecc)
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.numeric.wNames
#' @export
Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.

as.numeric.wNames <- function(vec) {
  numerified_vec = as.numeric(as.factor(vec)) - 1 # as factor gives numbers [1:n] instead [0:n]
  if (!is.null(names(vec))) {names(numerified_vec) = names(vec)}
  return(numerified_vec)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.numeric.wNames.old
#' @export
Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.

as.numeric.wNames.old <- function(vec) {
  numerified_vec = as.numeric(as.factor(vec))
  if (!is.null(names(vec))) {names(numerified_vec) = names(vec)}
  return(numerified_vec)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.character.wNames
#' @export
Converts your input vector into a character vector, and puts the original character values into the names of the new vector, unless it already has names.

as.character.wNames <- function(vec) {
  char_vec = as.character(vec)
  if (!is.null(names(vec))) {names(char_vec) = names(vec)}
  return(char_vec)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @param from PARAM_DESCRIPTION, Default: 0
#' @param upto PARAM_DESCRIPTION, Default: 100
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rescale
#' @export
linear transformation to a given range of values

rescale <- function(vec, from = 0, upto = 100) {
  vec = vec - min(vec, na.rm = TRUE)
  vec = vec * ((upto - from)/max(vec, na.rm = TRUE))
  vec = vec + from
  return(vec)
} # fun

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param named_vector PARAM_DESCRIPTION
#' @param NumericNames PARAM_DESCRIPTION, Default: FALSE
#' @param silent PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname flip_value2name
#' @export
Flip the values and the names of a vector with names

flip_value2name <- function(named_vector, NumericNames = FALSE, silent = F) {
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

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param named_vector PARAM_DESCRIPTION
#' @param NumericNames PARAM_DESCRIPTION, Default: FALSE
#' @param silent PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname value2name_flip
#' @export
value2name_flip = flip_value2name

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec_or_list PARAM_DESCRIPTION
#' @param decreasing PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[gtools]{mixedsort}}
#' @rdname sortbyitsnames
#' @export
#' @importFrom gtools mixedsort
Sort a vector by the alphanumeric order of its names(instead of its values).

sortbyitsnames <- function(vec_or_list, decreasing = FALSE, ...) {
  xx = names(vec_or_list)
  names(xx) = 1:length(vec_or_list)
  order = as.numeric(names(gtools::mixedsort(xx, decreasing = decreasing, ...)))
  vec_or_list[order]
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @param summarize PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname any.duplicated
#' @export
How many entries are duplicated

any.duplicated <- function(vec, summarize = TRUE) {
  y = sum(duplicated(vec))
  if (summarize & y) {
    x = table(vec); x = x[x > 1] - 1;
    print("The following elements have  > 1 extra copies:")
    print(x) # table formatting requires a separate entry
  }
  return(y)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @param orig PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname which.duplicated
#' @export
orig = rownames(sc@expdata)

which.duplicated <- function(vec, orig = F) {
  DPL = vec[which(duplicated(vec))]; iprint(length(DPL), "Duplicated entries: ", DPL)
  # for (i in DPL ) {   print(grepv(i,orig)) } #for
  return(DPL)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @param orig PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname which.NA
#' @export
orig = rownames(sc@expdata)

which.NA <- function(vec, orig = F) {
  NANs = vec[which(is.na(vec))]; iprint(length(NANs), "NaN entries: ", NANs)
  NAs = vec[which(is.na(vec))]; iprint(length(NAs), "NA entries: ", NAs, "(only NA-s are returned)")
  # for (i in DPL ) {   print(grepv(i,orig)) } #for
  return(NAs)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param len PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pad.na
#' @export
Fill up with a vector to a given length with NA-values at the end.

pad.na <- function(x, len) { c(x, rep(NA, len - length(x))) }


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param valz PARAM_DESCRIPTION
#' @param high PARAM_DESCRIPTION, Default: TRUE
#' @param thr PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname clip.values
#' @export
Signal clipping. Cut values above or below a threshold.

clip.values <- function(valz, high = TRUE, thr = 3) {
  if (high) { valz[valz > thr] = thr
  } else {    valz[valz < thr] = thr }
  valz
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
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
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname clip.outliers
#' @export
Signal clipping based on the input data's distribution. It clips values above or below the extreme N% of the distribution.

clip.outliers <- function(valz, high = TRUE, probs = c(.01, .99), na.rm = TRUE, showhist = FALSE, ...) {
  qnt <- quantile(valz, probs = probs, na.rm = na.rm)
  if (showhist) { whist(unlist(valz), breaks = 50 ,vline = qnt, filtercol = -1)} #if
  y <- valz
  y[valz < qnt[1]] <- qnt[1]
  y[valz > qnt[2]] <- qnt[2]
  y
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.logical.wNames
#' @export
Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.

as.logical.wNames <- function(x, ...) {
  numerified_vec = as.logical(x, ...)
  if (!is.null(names(x))) {names(numerified_vec) = names(x)}
  return(numerified_vec)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param tbl.2col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname col2named.vec.tbl
#' @export
Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.

col2named.vec.tbl <- function(tbl.2col) {
  nvec = tbl.2col[[2]]
  names(nvec) = tbl.2col[[1]]
  nvec
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param yourvec PARAM_DESCRIPTION
#' @param by PARAM_DESCRIPTION, Default: 9
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname iterBy.over
#' @export
Iterate over a vector by every N-th element.

iterBy.over <- function(yourvec, by = 9) {
  steps = ceiling(length(yourvec)/by)
  lsX = split(yourvec, sort(rank(yourvec) %% steps))
  names(lsX) = 1:length(lsX)
  lsX
} # for (i in iterBy.over(yourvec = x)) { print(i) }

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION, Default: 1:9
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname zigzagger
#' @export
mix entries so that they differ

zigzagger <- function(vec = 1:9) {
  intermingle2vec(vec, rev(vec))[1:length(vec)]
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION, Default: 1
#' @param y PARAM_DESCRIPTION, Default: 100
#' @param zeropadding PARAM_DESCRIPTION, Default: TRUE
#' @param pad_length PARAM_DESCRIPTION, Default: floor(log10(max(abs(x), abs(y)))) + 1
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_pad}}
#' @rdname numerate
#' @export
#' @importFrom stringr str_pad
numerate from x to y with additonal zeropadding

numerate <- function(x = 1, y = 100, zeropadding = TRUE, pad_length = floor( log10( max(abs(x), abs(y)) ) ) + 1) {
  z = x:y
  if (zeropadding) { z = stringr::str_pad(z, pad = 0, width = pad_length)   }
  return(z)
}
# (numerate(1, 122))


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION, Default: rpois(4, lambda = 3)
#' @param topN PARAM_DESCRIPTION, Default: 2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname MaxN
#' @export
find second (third…) highest/lowest value in vector

MaxN <- function(vec = rpois(4, lambda = 3), topN = 2) {
  topN = topN - 1
  n <- length(vec)
  sort(vec, partial = n - topN)[n - topN]
}
# https://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param numericV PARAM_DESCRIPTION, Default: blanks
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname cumsubtract
#' @export
Cumulative subtraction, opposite of cumsum()

cumsubtract <- function(numericV = blanks) {
  DiffZ = numericV[-1] - numericV[-length(numericV)]
  print(table(DiffZ))
  DiffZ
}


### Vector filtering  -------------------------------------------------------------------------------------------------

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param named_Vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname which_names
#' @export
Return the names where the input vector is TRUE. The input vector is converted to logical.

which_names <- function(named_Vec) {
  return(names(which(as.logical.wNames(named_Vec)))) }

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param named_Vec PARAM_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname which_names_grep
#' @export
Return the vector elements whose names are partially matched

which_names_grep <- function(named_Vec, pattern) {
  idx = grepv(x = names(named_Vec),pattern = pattern)
  return(named_Vec[idx])
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @param silent PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname na.omit.strip
#' @export
Calls na.omit() and returns a clean vector

na.omit.strip <- function(vec, silent = FALSE) {
  if (is.data.frame(vec)) {
    if (min(dim(vec)) > 1 & silent == FALSE) { iprint(dim(vec), "dimensional array is converted to a vector.") }
    vec = unlist(vec) }
  clean = na.omit(vec)
  attributes(clean)$na.action <- NULL
  return(clean)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname inf.omit
#' @export
Omit infinite values from a vector.

inf.omit <- function(vec) {
  if (is.data.frame(vec)) {
    if ( min(dim(vec)) > 1 ) { iprint(dim(vec), "dimensional array is converted to a vector.") }
    vec = unlist(vec) }
  clean = vec[is.finite(vec)]
  # attributes(clean)$na.action <- NULL
  return(clean)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname zero.omit
#' @export
Omit zero values from a vector.

zero.omit <- function(vec) {
  v2 = vec[vec != 0]
  iprint("range: ", range(v2))
  if ( !is.null(names(vec)) ) {names(v2) = names(vec)[vec != 0]}
  return(v2)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param logical_vector PARAM_DESCRIPTION
#' @param percentify PARAM_DESCRIPTION, Default: TRUE
#' @param NumberAndPC PARAM_DESCRIPTION, Default: FALSE
#' @param NArm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pc_TRUE
#' @export
Percentage of true values in a logical vector, parsed as text (useful for reports.)

pc_TRUE <- function(logical_vector, percentify = TRUE, NumberAndPC = FALSE, NArm = TRUE) {
  SUM = sum(logical_vector, na.rm = NArm)
  LEN = length(logical_vector)
  out = SUM / LEN
  if (percentify) {out = percentage_formatter(out) }
  if (NumberAndPC) { out = paste0(out, " or " , SUM, " of ", LEN) }
  return(out)
}

# deprecated :
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param logical_vec PARAM_DESCRIPTION, Default: idx_localised
#' @param total PARAM_DESCRIPTION, Default: TRUE
#' @param NArm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname NrAndPc
#' @export
Summary stat. text formatting for logical vectors (%, length)

NrAndPc <- function(logical_vec = idx_localised, total = TRUE, NArm = TRUE) {
  x = paste0(pc_TRUE(logical_vec), " or ", sum(logical_vec, na.rm = NArm))
  if (total) paste0(x, " of ", length(logical_vec))
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param vec_or_table PARAM_DESCRIPTION
#' @param category PARAM_DESCRIPTION
#' @param NA_omit PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pc_in_total_of_match
#' @export
Percentage of a certain value within a vector or table.

pc_in_total_of_match <- function(vec_or_table, category, NA_omit = TRUE) {
  if (is.table(vec_or_table)) { vec_or_table[category]/sum(vec_or_table, na.rm = NA_omit) }
  else {# if (is.vector(vec_or_table))
    if (NA_omit) {
      if (sum(is.na(vec_or_table))) { vec_or_table = na.omit(vec_or_table); iprint(sum(is.na(vec_or_table)), 'NA are omitted from the vec_or_table of:', length(vec_or_table))}
      "Not wokring complelety : if NaN is stored as string, it does not detect it"
    }
    sum(vec_or_table == category) /  length(vec_or_table)
  } # else: is vector
} # fun

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param length_new PARAM_DESCRIPTION
#' @param length_old PARAM_DESCRIPTION
#' @param prepend PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname filter_survival_length
#' @export
Parse a sentence reporting the % of filter survival.

filter_survival_length <- function(length_new, length_old, prepend = "") {
  pc = percentage_formatter(length_new/length_old)
  llprint(prepend, pc, " of ", length_old, " entries make through the filter")
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @param probs PARAM_DESCRIPTION, Default: c(0.05, 0.95)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname remove_outliers
#' @export
Remove values that fall outside the trailing N % of the distribution.

remove_outliers <- function(x, na.rm = TRUE, ..., probs = c(.05, .95)) {
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

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param category_vec PARAM_DESCRIPTION
#' @param replaceit PARAM_DESCRIPTION
#' @param to PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname simplify_categories
#' @export
Replace every entry that is found in "replaceit", by a single value provided by "to"

simplify_categories <- function(category_vec, replaceit , to ) {
  matches = which(category_vec %in% replaceit); iprint(length(matches), "instances of", replaceit, "are replaced by", to)
  category_vec[matches] = to
  return(category_vec)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param needle PARAM_DESCRIPTION
#' @param haystack PARAM_DESCRIPTION
#' @param exact PARAM_DESCRIPTION, Default: TRUE
#' @param report PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lookup
#' @export
Awesome pattern matching for a set of values in another set of values. Returns a list with all kinds of results.

lookup <- function(needle, haystack, exact = TRUE, report = FALSE) {
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



## String operations  -------------------------------------------------------------------------------------------------
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param pvalue PARAM_DESCRIPTION, Default: 0.01
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname parsepvalue
#' @export
Parse p-value from a number to a string.

parsepvalue <- function(pvalue = 0.01) paste0("(p<",pvalue,")");

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname eval_parse_kollapse
#' @export
evaluate and parse (dyn_var_caller)

eval_parse_kollapse <- function(...) {
  substitute(eval(parse(text = kollapse( ... , print = FALSE))))
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param ls.of.params PARAM_DESCRIPTION, Default: p
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname param.list.2.fname
#' @export
Take a list of parameters and parse a string from their names and values.

param.list.2.fname <- function(ls.of.params = p) {
  paste(names(ls.of.params), ls.of.params, sep = ".", collapse = "_")
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname PasteDirNameFromFlags
#' @export
Paste a dot (point) separated string from a list of inputs (that can be empty), and clean up the output string from dot multiplets (e.g: ..).

PasteDirNameFromFlags <- function(...) {
  flagList <- c(...)
  pastedFlagList <- kpp(flagList)
  CleanDirName <- gsub(x = pastedFlagList, pattern = '[\\..] + ',replacement = '\\.' )
  return(CleanDirName)
}
# PasteDirNameFromFlags("HCAB"
#                       , flag.nameiftrue(p$'premRNA')
#                       , flag.nameiftrue(p$"dSample.Organoids")
#                       , flag.names_list(p$'variables.2.regress')
#                       ,  flag.nameiftrue(p$'Man.Int.Order') )


### File name and path parsing ------------------------------------------------------------------------------------------------
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param path PARAM_DESCRIPTION, Default: '~/Dropbox/Abel.IMBA/AnalysisD'
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname PasteOutdirFromFlags
#' @export
Paste OutDir from (1) a path and (2) a from a list of inputs (that can be empty), and clean up the output string from dot and forward slash multiplets (e.g: ..).

PasteOutdirFromFlags <- function(path = "~/Dropbox/Abel.IMBA/AnalysisD", ...) {
  flagList <- c(path, ...)
  pastedFlagList <- kpp(flagList)
  CleanDirName <- gsub(x = pastedFlagList, pattern = '[\\..] + ',replacement = '\\.' )
  # pastedOutDir <- kpps(path, CleanDirName, "/")
  pastedOutDir <- p0(CleanDirName, "/")
  CleanDirName <- gsub(x = pastedOutDir, pattern = '[//] + ',replacement = '/' )
  return(CleanDirName)
}
# PasteOutdirFromFlags("~/Dropbox/Abel.IMBA/AnalysisD/HCAB"
#                      , flag.nameiftrue(p$'premRNA')
#                      , flag.nameiftrue(p$"dSample.Organoids")
#                      , flag.names_list(p$'variables.2.regress')
#                      ,  flag.nameiftrue(p$'Man.Int.Order') )

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param toggle PARAM_DESCRIPTION
#' @param Separator PARAM_DESCRIPTION, Default: '_'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname flag.name_value
#' @export
Returns the name and its value, if its not FALSE.

flag.name_value <- function(toggle, Separator = "_") {
  if (!isFALSE(toggle)) {
    output = paste(substitute(toggle), toggle, sep = Separator)
    if (length(output) > 1) output = output[length(output)]  # fix for when input is a list element like p$'myparam'
    return(output)
  }
}
# Xseed = p$'seed' = F; flag.name_value(Xseed); flag.name_value(p$'seed')

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param toggle PARAM_DESCRIPTION
#' @param prefix PARAM_DESCRIPTION, Default: NULL
#' @param suffix PARAM_DESCRIPTION, Default: NULL
#' @param name.if.not PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname flag.nameiftrue
#' @export
Returns the name and its value, if its TRUE.

flag.nameiftrue <- function(toggle, prefix = NULL, suffix = NULL, name.if.not = "") {
  output = if (toggle) { paste0(prefix, (substitute(toggle)), suffix)
  } else {paste0(prefix, name.if.not, suffix)}
  if (length(output) > 1) output = output[length(output)]  # fix for when input is a list element like p$'myparam'
  return(output)
} # returns the name if its value is true
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param toggle PARAM_DESCRIPTION
#' @param prefix PARAM_DESCRIPTION, Default: NULL
#' @param suffix PARAM_DESCRIPTION, Default: NULL
#' @param name.if.not PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname nameiftrue
#' @export
nameiftrue = flag.nameiftrue # backward compatible

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param par PARAM_DESCRIPTION, Default: p$umap.min_dist
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname flag.names_list
#' @export
Returns the name and value of each element in a list of parameters.

flag.names_list <- function(par = p$'umap.min_dist') {
  if (length(par)) paste(substitute(par), kppu(par) , sep = "_")[[3]]
};  # param.list.flag(par = p$umap.n_neighbors)


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param pl PARAM_DESCRIPTION, Default: p.hm
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname flag.names_list.all.new
#' @export
Returns the name and value of each element in a list of parameters.

flag.names_list.all.new <- function(pl = p.hm) {
  # if (length(pl)) paste(kppu(names(pl)), kppu(pl) , sep = "_")
  if (length(pl)) kppd(paste(names(pl), pl, sep = "_"))
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param par PARAM_DESCRIPTION, Default: p$umap.min_dist
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname param.list.flag
#' @export
Returns the name and value of each element in a list of parameters.

param.list.flag <- function(par = p$'umap.min_dist') {
  paste(substitute(par), par, sep = "_")[[3]]
};  # param.list.flag(par = p$umap.n_neighbors)


## Matrix operations -------------------------------------------------------------------------------------------------

### Matrix calculations  -------------------------------------------------------------------------------------------------
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowMedians
#' @export
Calculates the median of each row of a numeric matrix / data frame.

rowMedians <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, median, na.rm = na.rm)
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colMedians
#' @export
Calculates the median of each column of a numeric matrix / data frame.

colMedians <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, median, na.rm = na.rm)

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowGeoMeans
#' @export
Calculates the median of each row of a numeric matrix / data frame.

rowGeoMeans <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, geomean, na.rm = na.rm)
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colGeoMeans
#' @export
Calculates the median of each column of a numeric matrix / data frame.

colGeoMeans <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, geomean, na.rm = na.rm)

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowCV
#' @export
Calculates the CV of each ROW of a numeric matrix / data frame.

rowCV <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, cv, na.rm = na.rm )
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colCV
#' @export
Calculates the CV of each column of a numeric matrix / data frame.

colCV <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, cv, na.rm = na.rm )

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowVariance
#' @export
Calculates the CV of each ROW of a numeric matrix / data frame.

rowVariance <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, var, na.rm = na.rm )
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colVariance
#' @export
Calculates the CV of each column of a numeric matrix / data frame.

colVariance <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, var, na.rm = na.rm )

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowMin
#' @export
Calculates the minimum of each row of a numeric matrix / data frame.

rowMin <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, min, na.rm = na.rm)
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colMin
#' @export
Calculates the minimum of each column of a numeric matrix / data frame.

colMin <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, min, na.rm = na.rm)

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowMax
#' @export
Calculates the maximum of each row of a numeric matrix / data frame.

rowMax <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, max, na.rm = na.rm)
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colMax
#' @export
Calculates the maximum of each column of a numeric matrix / data frame.

colMax <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, max, na.rm = na.rm)

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowSEM
#' @export
Calculates the SEM of each row of a numeric matrix / data frame.

rowSEM <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, sem, na.rm = na.rm)
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colSEM
#' @export
Calculates the SEM of each column of a numeric matrix / data frame.

colSEM <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, sem, na.rm = na.rm)

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowSD
#' @export
Calculates the SEM of each row of a numeric matrix / data frame.

rowSD <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, sd, na.rm = na.rm)
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colSD
#' @export
Calculates the SEM of each column of a numeric matrix / data frame.

colSD <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, sd, na.rm = na.rm)

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowIQR
#' @export
Calculates the SEM of each row of a numeric matrix / data frame.

rowIQR <- function(x, na.rm = TRUE) apply(data.matrix(x), 1, IQR, na.rm = na.rm)
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colIQR
#' @export
Calculates the SEM of each column of a numeric matrix / data frame.

colIQR <- function(x, na.rm = TRUE) apply(data.matrix(x), 2, IQR, na.rm = na.rm)

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowquantile
#' @export
Calculates the SEM of each row of a numeric matrix / data frame.

rowquantile <- function(x, na.rm = TRUE, ...) apply(data.matrix(x), 1, quantile, ..., na.rm = na.rm)
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colquantile
#' @export
Calculates the SEM of each column of a numeric matrix / data frame.

colquantile <- function(x, na.rm = TRUE, ...) apply(data.matrix(x), 2, quantile, ..., na.rm = na.rm)

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colDivide
#' @export
See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r

colDivide <- function(mat, vec) { # divide by column
  stopifnot(NCOL(mat) == length(vec))
  mat / vec[col(mat)] # fastest
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colMutliply
#' @export
See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r

colMutliply <- function(mat, vec) { # Mutliply by column
  stopifnot(NCOL(mat) == length(vec))
  mat * vec[col(mat)] # fastest
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowDivide
#' @export
divide by row

rowDivide <- function(mat, vec) {
  stopifnot(NROW(mat) == length(vec))
  mat / vec[row(mat)] # fastest
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param mat PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowMutliply
#' @export
Mutliply by row

rowMutliply <- function(mat, vec) {
  stopifnot(NROW(mat) == length(vec))
  mat * vec[row(mat)] # fastest
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param DF PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname row.Zscore
#' @export
Calculate Z-score over rows of data frame.

row.Zscore <- function(DF) t(scale(t(DF)))

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param mat PARAM_DESCRIPTION
#' @param SUM PARAM_DESCRIPTION, Default: 1e+06
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname TPM_normalize
#' @export
normalize each column to 1 million

TPM_normalize <- function(mat, SUM = 1e6) {
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * SUM
  return(norm_mat)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param mat PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname median_normalize
#' @export
normalize each column to the median of all the column-sums

median_normalize <- function(mat) {
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * median(cs)
  iprint("colMedians: ", head(signif(colMedians(norm_mat), digits = 3)))
  return(norm_mat)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param mat PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mean_normalize
#' @export
normalize each column to the median of the columns

mean_normalize <- function(mat) {
  cs = colSums(mat, na.rm = TRUE)
  norm_mat = (t(t(mat) / cs)) * mean(cs)
  iprint("colMeans: ", head(signif(colMeans(norm_mat))))
  return(norm_mat)
}

### Distance and correlation calculations --------------
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df2col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname eucl.dist.pairwise
#' @export
Calculate pairwise euclidean distance

eucl.dist.pairwise <- function(df2col) {
  dist_ = abs(df2col[,1] - df2col[,2]) / sqrt(2)
  if (!is.null(rownames(df2col)))   names(dist_) = rownames(df2col)
  dist_
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df2col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sign.dist.pairwise
#' @export
Calculate absolute value of the pairwise euclidean distance

sign.dist.pairwise <- function(df2col) {
  dist_ = abs(df2col[,1] - df2col[,2]) / sqrt(2)
  if (!is.null(rownames(df2col)))   names(dist_) = rownames(df2col)
  dist_
}

# Auto correlation functions
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na_pass PARAM_DESCRIPTION, Default: na.pass
#' @param plot PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowACF
#' @export
RETURNS A LIST. Calculates the autocorrelation of each row of a numeric matrix / data frame.

rowACF <- function(x, na_pass = na.pass, plot = FALSE, ...) { apply(x, 1, acf, na.action = na_pass,  plot = plot, ...)}
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na_pass PARAM_DESCRIPTION, Default: na.pass
#' @param plot PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colACF
#' @export
RETURNS A LIST. Calculates the autocorrelation of each row of a numeric matrix / data frame.

colACF <- function(x, na_pass = na.pass, plot = FALSE, ...) { apply(x, 2, acf, na.action = na_pass,  plot = plot, ...)}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param lag PARAM_DESCRIPTION, Default: 1
#' @param na_pass PARAM_DESCRIPTION, Default: na.pass
#' @param plot PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname acf.exactLag
#' @export
Autocorrelation with exact lag

acf.exactLag <- function(x, lag = 1, na_pass = na.pass, plot = FALSE, ... ) {
  x = acf(x, na.action = na_pass,  plot = plot, ...)
  x[['acf']][(lag + 1)]
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na_pass PARAM_DESCRIPTION, Default: na.pass
#' @param lag PARAM_DESCRIPTION, Default: 1
#' @param plot PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowACF.exactLag
#' @export
RETURNS A Vector for the "lag" based autocorrelation. Calculates the autocorrelation of each row of a numeric matrix / data frame.

rowACF.exactLag <- function(x, na_pass = na.pass, lag = 1, plot = FALSE, ...) {
  signif(apply(x, 1, acf.exactLag, lag = lag, plot = plot, ...), digits = 2)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param na_pass PARAM_DESCRIPTION, Default: na.pass
#' @param lag PARAM_DESCRIPTION, Default: 1
#' @param plot PARAM_DESCRIPTION, Default: FALSE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colACF.exactLag
#' @export
RETURNS A Vector for the "lag" based autocorrelation. Calculates the autocorrelation of each row of a numeric matrix / data frame.

colACF.exactLag <- function(x, na_pass = na.pass, lag = 1, plot = FALSE, ...) {
  signif(apply(x, 2, acf.exactLag, lag = lag, plot = plot, ...), digits = 2)
}


### Matrix manipulations -------------------------------------------------------------------------------------------------
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param x PARAM_DESCRIPTION
#' @param clockwise PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rotate
#' @export
rotate a matrix 90 degrees.

rotate <- function(x, clockwise = TRUE) {
  if (clockwise) { t( apply(x, 2, rev))  #first reverse, then transpose, it's the same as rotate 90 degrees
  } else {apply( t(x), 2, rev)}  #first transpose, then reverse, it's the same as rotate -90 degrees:
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sortEachColumn
#' @export
Sort each column of a numeric matrix / data frame.

sortEachColumn <- function(data, ...) sapply(data, sort, ...)

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df PARAM_DESCRIPTION
#' @param colname_in_df PARAM_DESCRIPTION, Default: 1
#' @param decrease PARAM_DESCRIPTION, Default: FALSE
#' @param na_last PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sort.mat
#' @export
Sort a matrix. ALTERNATIVE: dd[with(dd, order(-z, b)), ]. Source: https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r

sort.mat <- function(df, colname_in_df = 1, decrease = FALSE, na_last = TRUE) {
  if (length(colname_in_df) > 1) { print("cannot handle multi column sort") }
  else {df[ order(df[, colname_in_df], decreasing = decrease, na.last = na_last), ]}
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param mat_w_dimnames PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowNameMatrix
#' @export
Create a copy of your matrix, where every entry is replaced by the corresponding row name. Useful if you want to color by row name in a plot (where you have different number of NA-values in each row).

rowNameMatrix <- function(mat_w_dimnames) {
  matrix(rep(rownames(mat_w_dimnames), ncol(mat_w_dimnames) ), nrow = nrow(mat_w_dimnames), ncol = ncol(mat_w_dimnames))
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param mat_w_dimnames PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colNameMatrix
#' @export
Create a copy of your matrix, where every entry is replaced by the corresponding column name. Useful if you want to color by column name in a plot (where you have different number of NA-values in each column).

colNameMatrix <- function(mat_w_dimnames) {
  x = rep(colnames(mat_w_dimnames), nrow(mat_w_dimnames) )
  t(matrix(x, nrow = ncol(mat_w_dimnames), ncol = nrow(mat_w_dimnames)))
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param matrix1 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rownames.trimws
#' @export
trim whitespaces from the rownames

rownames.trimws <- function(matrix1) {
  rownames(matrix1) = trimws(rownames(matrix1))
  return(matrix1)
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION, Default: colnames(df)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colsplit
#' @export
split a data frame by a factor corresponding to columns.

colsplit <- function(df, f = colnames(df)) {
  ListOfDFs = NULL
  levelz = unique(f)
  for (i in 1:length(levelz)) {   ListOfDFs[[i]] = df[ , which(f == levelz[i]) ]  }
  names(ListOfDFs) = levelz
  return(ListOfDFs)
}
#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION, Default: colnames(df)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname splitByCol
#' @export
splitByCol = colsplit

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION, Default: rownames(df)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rowsplit
#' @export
split a data frame by a factor corresponding to columns.

rowsplit <- function(df, f = rownames(df)) {
  ListOfDFs = NULL
  levelz = unique(f)
  for (i in 1:length(levelz)) {   ListOfDFs[[i]] = df[ which(f == levelz[i]), ]  }
  names(ListOfDFs) = levelz
  return(ListOfDFs)
}


#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param df PARAM_DESCRIPTION
#' @param RowIDs PARAM_DESCRIPTION, Default: NULL
#' @param ColIDs PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname select.rows.and.columns
#' @export
Subset rows and columns. It checks if the selected dimension names exist and reports if any of those they aren't found.

select.rows.and.columns <- function(df, RowIDs = NULL, ColIDs = NULL ) {
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

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param mat PARAM_DESCRIPTION
#' @param rownamez PARAM_DESCRIPTION
#' @param silent PARAM_DESCRIPTION, Default: FALSE
#' @param removeNAonly PARAM_DESCRIPTION, Default: FALSE
#' @param remove0only PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname getRows
#' @export
Get the subset of rows with existing rownames, report how much it could not find.

getRows <- function(mat, rownamez, silent = FALSE, removeNAonly = FALSE, remove0only = FALSE ) {
  idx = intersect(rownamez, row.names(mat))
  if (removeNAonly) { idx = which_names(rowSums(!is.na(mat[ idx, ]), na.rm = TRUE) > 0) }
  if (remove0only) { idx = which_names(rowSums(mx != 0, na.rm = TRUE) > 0) }
  if (!silent) { iprint(length(idx), "/", length(rownamez), "are found. Missing: ", length(setdiff(row.names(mat), rownamez))  ) }
  mat[ idx, ]
}

#' @title FUNCTION_TITLE.
#' @description FUNCTION_DESCRIPTION.
#' @param mat PARAM_DESCRIPTION
#' @param colnamez PARAM_DESCRIPTION
#' @param silent PARAM_DESCRIPTION, Default: FALSE
#' @param removeNAonly PARAM_DESCRIPTION, Default: FALSE
#' @param remove0only PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname getCols
#' @export
Get the subset of cols with existing colnames, report how much it could not find.

getCols <- function(mat, colnamez, silent = FALSE, removeNAonly = FALSE, remove0only = FALSE ) {
  idx = intersect(colnamez, colnames(mat))
  print(symdiff(colnamez, colnames(mat)))
  if (removeNAonly) {   idx = which_names(colSums(!is.na(mat[ ,idx ]), na.rm = TRUE) > 0) }
  if (remove0only) { idx = which_names(colSums(mx != 0, na.rm = TRUE) > 0) }
  if (!silent) { iprint(length(idx), "/", length(colnamez), "are found. Missing: ", length(setdiff(colnames(mat), colnamez))  ) }
  mat[ ,idx ]
}

#' @title Get odd or even columns or rows of a data frame
#' @description Get odd or even columns or rows of a data frame
#' @param df_ PARAM_DESCRIPTION, Default: NULL
#' @param rows PARAM_DESCRIPTION, Default: FALSE
#' @param odd PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get.oddoreven
#' @export


get.oddoreven <- function(df_ = NULL, rows = FALSE, odd = TRUE) {
  counter = if (rows) NROW(df_) else NCOL(df_)
  IDX = if (odd) seq(1, to = counter, by = 2) else seq(2, to = counter, by = 2)
  df_out = if (rows) df_[IDX, ] else df_[, IDX]
  return(df_out)
}


#' @title Combine matrices by rownames intersect.
#' @description Combine matrices by rownames intersect.
#' @param matrix1 PARAM_DESCRIPTION
#' @param matrix2 PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 2
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname combine.matrices.intersect
#' @export


combine.matrices.intersect <- function(matrix1, matrix2, k = 2) {
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


#' @title Merge any data frames by rownames. Required plyr package.
#' @description Merge any data frames by rownames. Required plyr package.
#' @param list_of_dfs PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[plyr]{join_all}}
#' @rdname merge_dfs_by_rn
#' @export
#' @importFrom plyr join_all


merge_dfs_by_rn <- function(list_of_dfs) {
  for (i in names(list_of_dfs) ) { colnames(list_of_dfs[[i]]) <- paste0(i,'.',colnames(list_of_dfs[[i]])) } # make unique column names
  for (i in names(list_of_dfs) ) { list_of_dfs[[i]]$rn <- rownames(list_of_dfs[[i]]) } #for
  COMBINED <- plyr::join_all(list_of_dfs, by = 'rn', type = 'full');   idim(COMBINED)
  rownames(COMBINED) = COMBINED$rn
  COMBINED$rn = NULL
  return(COMBINED)
}

#' @title Merge 2 numeric data frames by rownames.
#' @description Merge 2 numeric data frames by rownames.
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname merge_numeric_df_by_rn
#' @export


merge_numeric_df_by_rn <- function(x, y) {
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


#' @title Cols have to be a vector of numbers corresponding to columns.
#' @description Cols have to be a vector of numbers corresponding to columns.
#' @param mat PARAM_DESCRIPTION
#' @param cols PARAM_DESCRIPTION, Default: 1:NCOL(mat)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname remove.na.rows
#' @export


remove.na.rows <- function(mat, cols = 1:NCOL(mat)) {
  mat2 = mat[ , cols]
  idxOK = which(rowSums(!apply(mat2, 2, is.na)) == NCOL(mat)  )
  mat[idxOK, ]
}

#' @title Cols have to be a vector of numbers corresponding to columns.
#' @description Cols have to be a vector of numbers corresponding to columns.
#' @param mat PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname remove.na.cols
#' @export


remove.na.cols <- function(mat) {
  idxOK = !is.na(colSums(mat))
  return(mat[, idxOK])
}

#' @title Omit rows with NA values from a matrix. Rows with any, or full of NA-s.
#' @description Omit rows with NA values from a matrix. Rows with any, or full of NA-s.
#' @param mat PARAM_DESCRIPTION
#' @param any PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname na.omit.mat
#' @export


na.omit.mat <- function(mat, any = TRUE) {
  mat = as.matrix(mat)
  stopifnot(length(dim(mat)) == 2)
  if (any) outMat = mat[ !is.na(rowSums(mat)), ]
  else outMat = mat[ (rowSums(is.na(mat)) <= ncol(mat)), ] # keep rows not full with NA
  outMat
}





## List operations -------------------------------------------------------------------------------------------------
#' @title Check if there are any duplocated rownames in a list of dataframes.
#' @description Check if there are any duplocated rownames in a list of dataframes.
#' @param ls PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname any.duplicated.rownames.ls.of.df
#' @export


any.duplicated.rownames.ls.of.df <- function(ls) any.duplicated(rownames(ls))

#' @title Intersect any number of lists.
#' @description Intersect any number of lists.
#' @param ls PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname intersect.ls
#' @export


intersect.ls <- function(ls, ...) { Reduce(intersect, ls) }

#' @title Intersect any number of list elements. Faster than reduce.
#' @description Intersect any number of list elements. Faster than reduce.
#' @param ls PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname union.ls
#' @export


union.ls <- function(ls, ...) { sort(unique(do.call(c,ls))) }

#' @title Lapply, then unlist.
#' @description Lapply, then unlist.
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname unlapply
#' @export


unlapply <- function(...) { unlist(lapply(...)) }

#' @title Create a list with names from ALL variables you pass on to the function.
#' @description Create a list with names from ALL variables you pass on to the function.
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list.wNames
#' @export


list.wNames <- function(...) {
  lst = list(...)
  names(lst) = as.character(match.call()[-1])
  return(lst)
}

#' @title Split a dataframe into a list by its columns. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.
#' @description Split a dataframe into a list by its columns. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.
#' @param dtf PARAM_DESCRIPTION
#' @param na.omit PARAM_DESCRIPTION, Default: TRUE
#' @param zero.omit PARAM_DESCRIPTION, Default: FALSE
#' @param omit.empty PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.list.df.by.row
#' @export


as.list.df.by.row <- function(dtf, na.omit = TRUE, zero.omit = FALSE, omit.empty = FALSE) {
  outList = as.list(as.data.frame(t( dtf ) ) )
  if (na.omit) {   outList = lapply(outList, na.omit.strip) }
  if (zero.omit) {   outList = lapply(outList, zero.omit) }
  if (omit.empty) {   outList = outList[(lapply(outList, length)) > 0] }
  print(str(outList, vec.len = 2))
  return(outList)
}

#' @title Split a dataframe into a list by its rows.
#' @description Split a dataframe into a list by its rows. omit.empty for the listelments; na.omit and zero.omit are applied on entries inside each list element.
#' @param dtf PARAM_DESCRIPTION
#' @param na.omit PARAM_DESCRIPTION, Default: TRUE
#' @param zero.omit PARAM_DESCRIPTION, Default: FALSE
#' @param omit.empty PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.list.df.by.col
#' @export


as.list.df.by.col <- function(dtf, na.omit = TRUE, zero.omit = FALSE, omit.empty = FALSE) {
  outList = as.list(dtf)
  if (na.omit) {   outList = lapply(outList, na.omit.strip) }
  if (zero.omit) {   outList = lapply(outList, zero.omit) }
  if (omit.empty) {   outList = outList[(lapply(outList, length)) > 0] }
  print(str(outList, vec.len = 2))
  return(outList)
}

#' @title Reorder elements of lists in your custom order of names / indices.
#' @description Reorder elements of lists in your custom order of names / indices.
#' @param L PARAM_DESCRIPTION
#' @param namesOrdered PARAM_DESCRIPTION, Default: mixedsort(names(L))
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname reorder.list
#' @export


reorder.list <- function(L, namesOrdered = mixedsort(names(L))) {
  Lout = list(NA)
  for (x in 1:length(namesOrdered)) { Lout[[x]] = L[[namesOrdered[x] ]]  }
  if (length(names(L))) { names(Lout) = namesOrdered }
  return(Lout)
}

#' @title Range of values in whole list.
#' @description Range of values in whole list.
#' @param L PARAM_DESCRIPTION
#' @param namesOrdered PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname range.list
#' @export


range.list <- function(L, namesOrdered) {
  return(range(unlist(L), na.rm = TRUE))
}

#' @title Combine 2 lists (of the same length) so that form every odd and every even element of a unified list.
#' @description Combine 2 lists (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().
#' @param L1 PARAM_DESCRIPTION
#' @param L2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname intermingle2lists
#' @export


intermingle2lists <- function(L1, L2) {
  stopifnot(length(L1) == length(L2) )
  Lout = list(NA)
  for (x in 1:(2*length(L1)) ) {
    if (x  %% 2) {  Lout[[x]] = L1[[((x + 1) / 2)]]; names(Lout)[x] = names(L1)[((x + 1) / 2)]
    } else {    Lout[[x]] = L2[[x / 2]]; names(Lout)[x] = names(L2)[x / 2]      }
  } # for
  return(Lout)
}

#' @title Convert a vector to a list with certain dimensions, taken from the list it wanna resemble.
#' @description Convert a vector to a list with certain dimensions, taken from the list it wanna resemble.
#' @param vec PARAM_DESCRIPTION
#' @param list_wannabe PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname as.listalike
#' @export


as.listalike <- function(vec, list_wannabe) {
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



#' @title Reverse list hierarchy.
#' @description Reverse list hierarchy.
#' @param ll PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname reverse.list.hierarchy
#' @export


reverse.list.hierarchy <- function(ll) {
  ## https://stackoverflow.com/a/15263737
  nms <- unique(unlist(lapply(ll, function(X) names(X))))
  ll <- lapply(ll, function(X) setNames(X[nms], nms))
  ll <- apply(do.call(rbind, ll), 2, as.list)
  lapply(ll, function(X) X[!sapply(X, is.null)])
}



#' @title Convert a list to a full matrix.
#' @description  Convert a list to a full matrix. Rows = names(union.ls(your_list)) or all names of within list elements, columns = names(your_list).
#' @param your.list PARAM_DESCRIPTION, Default: list(set.1 = vec.fromNames(LETTERS[1:5], fill = 1), set.2 = vec.fromNames(LETTERS[3:9],
#'    fill = 2))
#' @param byRow PARAM_DESCRIPTION, Default: TRUE
#' @param FILL PARAM_DESCRIPTION, Default: NA
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list2fullDF.byNames
#' @export

list2fullDF.byNames <- function(your.list = list(
  "set.1" = vec.fromNames(LETTERS[1:5], fill = 1),
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


#' @title Convert a list to a full matrix.
#' @description Convert a list to a full matrix. Designed for occurence counting, think tof table(). Rows = all ENTRIES of within your list, columns = names(your_list).
#' @param your.list PARAM_DESCRIPTION, Default: list(set.1 = LETTERS[1:5], set.2 = LETTERS[3:9])
#' @param byRow PARAM_DESCRIPTION, Default: TRUE
#' @param FILL PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list2fullDF.presence
#' @export


list2fullDF.presence <- function(your.list = list("set.1" = LETTERS[1:5]
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

#' @title Split a list by its names.
#' @description Split a list by its names.
#' @param namedVec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname splitbyitsnames
#' @export


splitbyitsnames <- function(namedVec) {
  stopif(is.null(names(namedVec)), message = "NO NAMES")
  split(namedVec, f = names(namedVec))
}

#' @title Split a list by its names.
#' @description Split a list by its names.
#' @param namedVec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname splititsnames_byValues
#' @export


splititsnames_byValues <- function(namedVec) {
  stopif(is.null(names(namedVec)), message = "NO NAMES")
  split(names(namedVec), f = namedVec)
}

#' @title Combine 2 vectors.
#' @description Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.
#' @param V1 PARAM_DESCRIPTION
#' @param V2 PARAM_DESCRIPTION
#' @param wNames PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname intermingle2vec
#' @export


intermingle2vec <- function(V1, V2, wNames = TRUE) {
  stopifnot(length(V1) == length(V2) )
  Vout = c(rbind(V1, V2))
  if (wNames) {names(Vout) = c(rbind(names(V1), names(V2)))}
  return(Vout)
}



#' @title Combine 2 data frames.
#' @description Combine 2 data frames (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().
#' @param df1 PARAM_DESCRIPTION
#' @param df2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname intermingle.cbind
#' @export


intermingle.cbind <- function(df1, df2) {
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

#' @title Convert a list to a vector.
#' @description Convert a list to a vector repeating list-element names, while vector names are the list elements.
#' @param your_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ls2categvec
#' @export


ls2categvec <- function(your_list ) {
  VEC = rep(names(your_list),unlapply(your_list, length))
  names(VEC) = unlist(your_list, use.names = TRUE)
  return(VEC)
}


#' @title Convert a list to a vector.
#' @description Convert a list to a vector, with list elements names replicated as many times, as many elements each element had.
#' @param ListWithNames PARAM_DESCRIPTION, Default: Sections.ls.Final
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list.2.replicated.name.vec
#' @export


list.2.replicated.name.vec <- function(ListWithNames = Sections.ls.Final) {
  NZ = names(ListWithNames)
  LZ = unlapply(ListWithNames, length)
  replicated.name.vec = rep(NZ, LZ)
  names(replicated.name.vec) = unlist(ListWithNames)
  return(replicated.name.vec)
}

### Work with multi dimensional lists --------------------------------


#' @title Copy dimension and dimnames.
#' @description Copy dimension and dimnames.
#' @param list.1D PARAM_DESCRIPTION
#' @param obj.2D PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname copy.dimension.and.dimnames
#' @export

copy.dimension.and.dimnames <- function(list.1D, obj.2D) {
  dim(list.1D) <- dim(obj.2D)
  dimnames(list.1D) <- dimnames(obj.2D)
  list.1D
}

#' @title Lapply for multidimensional arrays.
#' @description Lapply for multidimensional arrays.
#' @param list_2D PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mdlapply
#' @export


mdlapply <- function(list_2D, ...) {
  x = lapply(list_2D, ...)
  copy.dimension.and.dimnames(x,list_2D)
}


#' @title Simplify 2D-list-array to a DF.
#' @description Simplify 2D-list-array to a DF.
#' @param two.dim.arr.of.lists PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname arr.of.lists.2.df
#' @export


arr.of.lists.2.df <- function(two.dim.arr.of.lists) {
  list.1D = unlist(two.dim.arr.of.lists)
  dim(list.1D) <- dim(two.dim.arr.of.lists)
  dimnames(list.1D) <- dimnames(two.dim.arr.of.lists)
  list.1D
}


#' @title Multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF).
#' @description Multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF).
#' @param list_2D PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mdlapply2df
#' @export


mdlapply2df <- function(list_2D, ...) {
  x = lapply(list_2D, ...)
  z = copy.dimension.and.dimnames(x,list_2D)
  arr.of.lists.2.df(z)
}



## Set operations -------------------------------------------------------------------------------------------------

#' @title Symmetric difference of any number of vectors.
#' @description Quasy symmetric difference of any number of vectors.
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname symdiff
#' @export


symdiff <- function(x, y, ...) {
  big.vec <- c(x, y, ...)
  ls = list(x, y, ...); if ( length(ls) > 2) {print("# Not Mathematically correct, but logical for n>2 vectors: https://en.wikipedia.org/wiki/Symmetric_difference#Properties")}
  names(ls) = paste("Only in", as.character(match.call()[-1]))
  duplicates <- big.vec[duplicated(big.vec)]
  lapply(ls, function(x) setdiff(x, duplicates))
}

## Math & stats -------------------------------------------------------------------------------------------------

#' @title Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default).
#' @description Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default).
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sem
#' @export


sem <- function(x, na.rm = TRUE) sd(unlist(x), na.rm = na.rm)/sqrt(length(na.omit.strip(as.numeric(x))))

#' @title Calculates the fano factor on a numeric vector (it excludes NA-s by default).
#' @description Calculates the fano factor on a numeric vector (it excludes NA-s by default).
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param USE PARAM_DESCRIPTION, Default: 'na.or.complete'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fano
#' @export


fano <- function(x, na.rm = TRUE, USE = "na.or.complete") var(x, na.rm = na.rm, use = USE )/mean(x, na.rm = na.rm)

#' @title Geometric mean.
#' @description Calculates the geometric mean of a numeric vector (it excludes NA-s by default). Old alias gm_mean = geomean.
#' @param x PARAM_DESCRIPTION
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname geomean
#' @export


geomean <- function(x, na.rm = TRUE) {
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x)) }


#' @title Calculates the mean of the log.
#' @description Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default).
#' @param x PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 2
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mean_of_log
#' @export


mean_of_log <- function(x, k = 2, na.rm = TRUE) {
  negs = sum(x < 0);  zeros = sum(x == 0)
  if (negs | zeros) { iprint("The input vector has", negs, "negative values and", zeros, "zeros." ) }
  mean(log(x, base = k), na.rm = na.rm) }

#' @title Moving / rolling average.
#' @description Calculates the moving / rolling average of a numeric vector.
#' @param x PARAM_DESCRIPTION
#' @param oneSide PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname movingAve
#' @export


movingAve <- function(x, oneSide = 5) {
  y = NULL
  for (i in oneSide:length(x)) {
    y[i] = mean( x[ (i - oneSide):(i + oneSide) ] )
  };  return(y)
}


#' @title Calculates the moving / rolling average.
#' @description Calculates the moving / rolling average of a numeric vector, using filter().
#' @param x PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname movingAve2
#' @export


movingAve2 <- function(x,n = 5) {filter(x,rep(1/n,n), sides = 2)}

#' @title Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.
#' @description Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.
#' @param x PARAM_DESCRIPTION
#' @param oneSide PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname movingSEM
#' @export


movingSEM <- function(x, oneSide = 5) {
  y = NULL
  for (i in oneSide:length(x)) {
    y[i] = sem( x[ (i - oneSide):(i + oneSide) ] )
  };  return(y)
}

#' @title Calculates the moving / rolling standard error of the mean (SEM).
#' @description Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller window-size.
#' @param x PARAM_DESCRIPTION
#' @param oneSide PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname imovingSEM
#' @export


imovingSEM <- function(x, oneSide = 5) {
  y = NULL
  for (i in 1:length(x)) {
    oneSideDynamic = min(i - 1, oneSide, length(x) - i); oneSideDynamic
    indexx = (i - oneSideDynamic):(i + oneSideDynamic);indexx
    y[i] = sem( x[ indexx ] )
  };  return(y)
}

#' @title Shannon entropy.
#' @description Calculate shannon entropy.
#' @param p PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname shannon.entropy
#' @export


shannon.entropy <- function(p) {
  if (min(p) < 0 || sum(p) <= 0) return(NA)
  p.norm <- p[p > 0]/sum(p) - sum(log2(p.norm)*p.norm)
}


## Plotting and Graphics -----------------------------------------------------------------------------------------------------

### Colors -----------------------------------------------------------------------------------------------------
#' @title Alias for rich.colors in gplots.
#' @description Alias for rich.colors in gplots.
#' @param n PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[gplots]{rich.colors}}
#' @rdname richColors
#' @export
#' @importFrom gplots rich.colors


richColors <- function(n = 3) { gplots::rich.colors(n) }


#' @title Display the colors encoded by the numbers / color-ID-s you pass on to this function.
#' @description Display the colors encoded by the numbers / color-ID-s you pass on to this function.
#' @param ... PARAM_DESCRIPTION
#' @param incrBottMarginBy PARAM_DESCRIPTION, Default: 0
#' @param savefile PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname Color_Check
#' @export


Color_Check <- function(..., incrBottMarginBy = 0, savefile = FALSE ) {
  if (incrBottMarginBy) { .ParMarDefault <- par("mar");   par(mar = c(par("mar")[1] + incrBottMarginBy, par("mar")[2:4]) ) }  # Tune the margin
  Numbers = c(...)
  if (length(names(Numbers)) == length(Numbers)) {labelz = names(Numbers)} else {labelz = Numbers}
  barplot(rep(10, length(Numbers)), col = Numbers, names.arg = labelz, las = 2 )
  if (incrBottMarginBy) { par("mar" = .ParMarDefault )}

  fname = substitute(...)
  if (savefile) { dev.copy2pdf(file = ww.FnP_parser(fname, "ColorCheck.pdf")) }
}


HeatMapCol_BGR <- grDevices::colorRampPalette(c("blue", "cyan", "yellow", "red"), bias = 1)
HeatMapCol_RedBlackGreen <- grDevices::colorRampPalette(c("red", "black", "green"), bias = 1)


#' @title Draw a barplot from ColSums of a matrix.
#' @description Draw a barplot from ColSums of a matrix.
#' @param df PARAM_DESCRIPTION
#' @param col PARAM_DESCRIPTION, Default: 'seagreen2'
#' @param na_rm PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname colSums.barplot
#' @export


colSums.barplot <- function(df, col = "seagreen2", na_rm = TRUE, ...) { barplot(colSums(df, na.rm = na_rm), col = col, ...) }

#' @title Renders the lm() function's output.
#' @description Renders the lm() function's output into a human readable text. (e.g. for subtitles).
#' @param lm PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lm_equation_formatter
#' @export


lm_equation_formatter <- function(lm) {
  eq = signif(lm$coefficients);
  kollapse("Intercept: ", eq[1], " Slope: ", eq[2]);
}

#' @title Renders the lm() function's output into a human readable text. (e.g. for subtitles).
#' @description Renders the lm() function's output into a human readable text. (e.g. for subtitles).
#' @param lm PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lm_equation_formatter2
#' @export


lm_equation_formatter2 <- function(lm) {
  eq = signif(lm$coefficients, digits = 3);
  kollapse("y = ", eq[2], "* x + ", eq[1]);
}

#' @title Renders the lm() function's output into a human readable text. (e.g. for subtitles).
#' @description Renders the lm() function's output into a human readable text. (e.g. for subtitles).
#' @param lm PARAM_DESCRIPTION
#' @param y.var.name PARAM_DESCRIPTION, Default: 'y'
#' @param x.var.name PARAM_DESCRIPTION, Default: 'x'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lm_equation_formatter3
#' @export


lm_equation_formatter3 <- function(lm, y.var.name = "y", x.var.name = "x") {
  eq = signif(lm$coefficients, digits = 3);
  plusSign = if (sign(eq[1] == 1)) "" else "-"
  kollapse(y.var.name, " = ", eq[2], "*",x.var.name," ",plusSign,"", eq[1]);
}

#' @title Split a one variable by another. Calculates equal bins in splitby, and returns a list of the corresponding values in toSplit.
#' @description Split a one variable by another. Calculates equal bins in splitby, and returns a list of the corresponding values in toSplit.
#' @param dfw2col PARAM_DESCRIPTION, Default: NULL
#' @param toSplit PARAM_DESCRIPTION, Default: 1:100
#' @param splitby PARAM_DESCRIPTION, Default: rnorm(100)
#' @param breaks_ PARAM_DESCRIPTION, Default: 20
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #ll = hist.XbyY(); wbarplot(unlapply(ll, length))
#'  }
#' }
#' @rdname hist.XbyY
#' @export


hist.XbyY <- function(dfw2col = NULL, toSplit = 1:100, splitby = rnorm(100), breaks_ = 20 ) {
  # http://stackoverflow.com/questions/8853735/get-index-of-the-histogram-bin-in-r
  if (NCOL(dfw2col) == 2) { toSplit = dfw2col[ , 1]; splitby = dfw2col[ , 2]; print(11) }
  xx = hist(splitby, breaks = breaks_, plot = TRUE)
  IDX = findInterval(x = splitby, vec = xx$breaks)
  ls = split(toSplit, IDX)
  iprint("Range of data:", range(xx$breaks))
  names(ls) = xx$breaks[-1]
  return(ls)
}

### Functions for pairs() plots  -----------------------------------------------------------------------------------------------------

#' @title Helper function for pairs().
#' @description A function to display correlation values for pairs() function. Default is pearson correlation, that can be set to  "kendall" or "spearman".
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param digits PARAM_DESCRIPTION, Default: 2
#' @param prefix PARAM_DESCRIPTION, Default: ''
#' @param cex.cor PARAM_DESCRIPTION, Default: 2
#' @param method PARAM_DESCRIPTION, Default: 'pearson'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname panel.cor.pearson
#' @export


panel.cor.pearson <- function(x, y, digits = 2, prefix = "", cex.cor = 2, method = "pearson") {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = method, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex <- 0.8/strwidth(txt)

  test <- cor.test(x, y)
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))

  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex = cex,  col = 2)
}

#' @title Helper function for pairs().
#' @description A function to display correlation values for pairs() function.
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param digits PARAM_DESCRIPTION, Default: 2
#' @param prefix PARAM_DESCRIPTION, Default: ''
#' @param cex.cor PARAM_DESCRIPTION, Default: 2
#' @param method PARAM_DESCRIPTION, Default: 'spearman'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname panel.cor.spearman
#' @export


panel.cor.spearman <- function(x, y, digits = 2, prefix = "", cex.cor = 2, method = "spearman") {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = method, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex <- 0.8/strwidth(txt)

  test <- cor.test(x, y)
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))

  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex = cex, col = 2)
}


#' @title Quantile breakpoints.
#' @description Quantile breakpoints in any data vector http://slowkow.com/notes/heatmap-tutorial/.
#' @param xs PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION, Default: 10
#' @param na.Rm PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname quantile_breaks
#' @export


quantile_breaks <- function(xs, n = 10, na.Rm = FALSE) {
  breaks <- quantile(xs, probs = seq(0, 1, length.out = n), na.rm = na.Rm)
  breaks[!duplicated(breaks)]
}




## Clustering heatmap tools -----------------------------------------------------------------------------------------------------

#' @title Extract ROW order from a pheatmap object.
#' @description Extract ROW order from a pheatmap object.
#' @param pheatmapObject PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname hclust.getOrder.row
#' @export


hclust.getOrder.row <- function(pheatmapObject) pheatmapObject$tree_row$labels[pheatmapObject$tree_row$order]
#' @title Extract COLUMN order from a pheatmap object.
#' @description Extract COLUMN order from a pheatmap object.
#' @param pheatmapObject PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname hclust.getOrder.col
#' @export

hclust.getOrder.col <- function(pheatmapObject) pheatmapObject$tree_col$labels[pheatmapObject$tree_col$order]


#' @title Extract cluster ID's for ROWS of a pheatmap object.
#' @description Extract cluster ID's for ROWS of a pheatmap object.
#' @param pheatmapObject PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname hclust.getClusterID.row
#' @export

hclust.getClusterID.row <- function(pheatmapObject, k = 3) cutree(pheatmapObject$tree_row, k = k)


#' @title Extract cluster ID's for COLUMNS of a pheatmap object.
#' @description Extract cluster ID's for COLUMNS of a pheatmap object.
#' @param pheatmapObject PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname hclust.getClusterID.col
#' @export

hclust.getClusterID.col <- function(pheatmapObject, k = 3) cutree(pheatmapObject$tree_col, k = k)



#' @title Calculate the position of ROW separating lines between clusters in a pheatmap object.
#' @description Calculate the position of ROW separating lines between clusters in a pheatmap object.
#' @param pheatmapObject PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname hclust.ClusterSeparatingLines.row
#' @export

hclust.ClusterSeparatingLines.row <- function(pheatmapObject, k = 3) which(!duplicated(cutree(pheatmapObject$tree_row, k = k)[pheatmapObject$tree_row$order])[-1])



#' @title Calculate the position of COLUMN separating lines between clusters in a pheatmap object.
#' @description Calculate the position of COLUMN separating lines between clusters in a pheatmap object.
#' @param pheatmapObject PARAM_DESCRIPTION
#' @param k PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname hclust.ClusterSeparatingLines.col
#' @export

hclust.ClusterSeparatingLines.col <- function(pheatmapObject, k = 3) which(!duplicated(cutree(pheatmapObject$tree_col, k = k)[pheatmapObject$tree_col$order])[-1])



#' @title Calculate gap positions for pheatmap, based a sorted annotation vector of categories.
#' @description Calculate gap positions for pheatmap, based a sorted annotation vector of categories.
#' @param annot.vec.of.categories PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname Gap.Postions.calc.pheatmap
#' @export


Gap.Postions.calc.pheatmap <- function(annot.vec.of.categories) {
  NAZ = sum(is.na(annot.vec.of.categories))
  if (NAZ) iprint("There are", NAZ, "NA values in your vector. They should be last and they are omitted.")
  consecutive.lengthes = rle( na.omit.strip(annot.vec.of.categories))$lengths
  cumsum(consecutive.lengthes) # return abs.positions
}

#' @title Create a Matlab-like color gradient using "colorRamps".
#' @description Create a Matlab-like color gradient using "colorRamps".
#' @param matrixx PARAM_DESCRIPTION
#' @param nr PARAM_DESCRIPTION, Default: 50
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[colorRamps]{matlab.like}}
#' @rdname matlabColors.pheatmap
#' @export
#' @importFrom colorRamps matlab.like


matlabColors.pheatmap <- function(matrixx, nr = 50) {colorRamps::matlab.like(length(quantile_breaks(matrixx, n = nr)) - 1)}


#' @title Helper for pheatmap.
#' @description For VECTORS. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap.
#' @param data PARAM_DESCRIPTION
#' @param annot_vec PARAM_DESCRIPTION
#' @param annot_names PARAM_DESCRIPTION, Default: 'Annot'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[gplots]{rich.colors}}
#' @rdname annot_col.create.pheatmap.vec
#' @export
#' @importFrom gplots rich.colors


annot_col.create.pheatmap.vec <- function(data, annot_vec, annot_names = "Annot") {
  stopifnot( length(annot_vec) == dim(data)[2] )
  namez = as.character(if (is.null(annot_names)) substitute(annot_vec) else annot_names)

  df = data.frame(x = annot_vec); df[, 1] = as.character(df[, 1])
  names(df) = namez # colnames but more flexible
  rownames(df) = colnames(data)
  assign(x = "annot", value = df, envir = .GlobalEnv)

  tt = table(annot_vec); nz = names(tt)
  if (is.numeric(annot_vec)) {
    coll = val2col(annot_vec[!duplicated(annot_vec)]); names(coll) = nz
  } else {
    coll = gplots::rich.colors(length(tt)); names(coll) = nz
  }
  col_list = list(annot_vec = coll)
  names(col_list) = namez
  assign(x = "annot_col", value = col_list, envir = .GlobalEnv)

  print("annot [data frame] and annot_col [list] variables are created. Use: pheatmap(..., annotation_col = annot, annotation_colors = annot_col)")
}


#' @title Helper for pheatmap.
#' @description For data frames. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap.
#' @param data PARAM_DESCRIPTION
#' @param annot_df_per_column PARAM_DESCRIPTION
#' @param annot_names PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[gplots]{rich.colors}}
#' @rdname annot_col.create.pheatmap.df
#' @export
#' @importFrom gplots rich.colors


annot_col.create.pheatmap.df <- function(data, annot_df_per_column, annot_names = NULL) {
  stopif( dim(annot_df_per_column)[1] != dim(data)[2] , message = "The number of rows in the annotation data != to the # columns in your data frame")

  df = as.data.frame(annot_df_per_column)
  if (any(rownames(df) != colnames(data))) { print("The rownames of annot_df_per_column are not the same as the colnames of data:")
    print(cbind("rownames(df)" = rownames(df) , "colnames(data)" = colnames(data))) }
  namez = as.character(if (is.null(annot_names)) colnames(annot_df_per_column) else annot_names)

  colnames(df) = namez
  rownames(df) = colnames(data)
  assign(x = "annot", value = df, envir = .GlobalEnv)

  col_list = list.fromNames(namez)
  for (i in 1:NCOL(df) ) {
    annot_column_i = df[, i]
    tt = table(annot_column_i); nz = names(tt)
    coll = if (is.numeric(annot_column_i)) { val2col(unique(annot_column_i));
    } else { gplots::rich.colors(length(tt)) }
    names(coll) = sort(nz)
    col_list[[i]] = coll
  } #for each column
  assign(x = "annot_col", value = col_list, envir = .GlobalEnv)

  print("annot [data frame] and annot_col [list] variables are created. Use: pheatmap(..., annotation_col = annot, annotation_colors = annot_col)")
}

#' @title Helper for pheatmap. fix class and color annotation in pheatmap.
#' @description Fix class and color annotation in pheatmap annotation data frame's and lists.
#' @param ListOfColnames PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname annot_col.fix.numeric
#' @export


annot_col.fix.numeric <- function(ListOfColnames) {
  for (i in 1:length(ListOfColnames) ) {
    j = ListOfColnames[i]
    annot[[j]] = as.numeric(annot[[j]])
    annot_col[[j]] = NULL # remove fixed colors -> auto determine by pheatmap
  } #for
  assign(x = "annot_col", value = annot_col, envir = .GlobalEnv)
  iprint("Columns in annot are as.numeric(), list elements in annot_col are removed")
}


#' @title Helper for pheatmap.
#' @description For data frames. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap.
#' @param data PARAM_DESCRIPTION
#' @param annot_df_per_row PARAM_DESCRIPTION
#' @param annot_names PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[gplots]{rich.colors}}
#' @rdname annot_row.create.pheatmap.df
#' @export
#' @importFrom gplots rich.colors


annot_row.create.pheatmap.df <- function(data, annot_df_per_row, annot_names = NULL) {
  stopif( dim(annot_df_per_row)[1] != dim(data)[1] , message = "The number of rows in the annotation data != to the # columns in your data frame")

  df = as.data.frame(annot_df_per_row)
  if (any(rownames(df) != rownames(data))) { print("The rownames of annot_df_per_row are not the same as the rownames of data:")
    print(cbind("rownames(df)" = rownames(df) , "rownames(data)" = rownames(data))) }
  namez = as.character(if (is.null(annot_names)) colnames(annot_df_per_row) else annot_names)

  colnames(df) = namez
  rownames(df) = rownames(data)
  assign(x = "annot_rows", value = df, envir = .GlobalEnv)

  col_list = list.fromNames(namez)
  for (i in 1:NCOL(df) ) {
    annot_column_i = df[, i]
    tt = table(annot_column_i); nz = names(tt)
    coll = if (is.numeric(annot_column_i)) { val2col(unique(annot_column_i));
    } else { gplots::rich.colors(length(tt)) }
    names(coll) = sort(nz)
    col_list[[i]] = coll
  } #for each column
  assign(x = "annot_rows.col", value = col_list, envir = .GlobalEnv)

  print("annot_rows [data frame] and annot_rows.col [list] variables are created. Use: pheatmap(..., annotation_row = annot_rows, annotation_colors = annot_rows.col)")
}



# Search query links ------------------------------------------------------------------------

# Google search URL / search query links
# b.dbl.writeOut = F
# b.dbl.Open = F

#' @title Parse google search query links.
#' @description Parse google search query links to your list of gene symbols. Strings "prefix" and ""suffix" will be searched for together with each gene ("Human ID4 neurons"). See many additional services in [DatabaseLinke.R](https://vertesy.github.io/DatabaseLinke.R/).
#' @param vector_of_gene_symbols PARAM_DESCRIPTION
#' @param google PARAM_DESCRIPTION, Default: 'http://www.google.com/search?as_q = '
#' @param prefix PARAM_DESCRIPTION, Default: ''
#' @param suffix PARAM_DESCRIPTION, Default: ''
#' @param writeOut PARAM_DESCRIPTION, Default: b.dbl.writeOut
#' @param Open PARAM_DESCRIPTION, Default: b.dbl.Open
#' @param sleep PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #link.google.clipboard = clipr::write_clip(link_google(clipr::read_clip()))
#'  }
#' }
#' @rdname link_google
#' @export


link_google <- function(vector_of_gene_symbols
                        , google = "http://www.google.com/search?as_q = ", prefix = "", suffix = ""
                        , writeOut = b.dbl.writeOut, Open = b.dbl.Open, sleep = 0) {
  links = paste0( google, prefix," ", vector_of_gene_symbols," ", suffix)
  if (writeOut) {
    bash_commands = paste0("open '", links, "'")
    if (sleep > 0) { bash_commands = paste0(bash_commands, ' ; sleep ', sleep) } # if wait
    write.simple.append("", ManualName = BashScriptLocation)
    write.simple.append(bash_commands, ManualName = BashScriptLocation)
  } else if (Open) { for (linkX in links) Sys.sleep(0.3 + runif(1)); browseURL(linkX, encodeIfNeeded = T) } else {return(links)}
}




#' @title Parse Bing search query links.
#' @description Parse bing search query links to your list of gene symbols. Strings "prefix" and ""suffix" will be searched for together with each gene ("Human ID4 neurons"). See many additional services in [DatabaseLinke.R](https://vertesy.github.io/DatabaseLinke.R/).
#' @param vector_of_gene_symbols PARAM_DESCRIPTION
#' @param bing PARAM_DESCRIPTION, Default: 'https://www.bing.com/search?q = '
#' @param prefix PARAM_DESCRIPTION, Default: ''
#' @param suffix PARAM_DESCRIPTION, Default: ''
#' @param writeOut PARAM_DESCRIPTION, Default: b.dbl.writeOut
#' @param Open PARAM_DESCRIPTION, Default: b.dbl.Open
#' @param sleep PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname link_bing
#' @export

link_bing <- function(vector_of_gene_symbols
                      , bing = "https://www.bing.com/search?q = ", prefix = "", suffix = ""
                      , writeOut = b.dbl.writeOut, Open = b.dbl.Open, sleep = 0) {
  links = paste0( bing, prefix," ", vector_of_gene_symbols," ", suffix)
  if (writeOut) {
    bash_commands = paste0("open '", links, "'")
    if (sleep > 0) { bash_commands = paste0(bash_commands, ' ; sleep ', sleep) } # if wait
    write.simple.append("", ManualName = BashScriptLocation)
    write.simple.append(bash_commands, ManualName = BashScriptLocation)
  } else if (Open) { for (linkX in links) Sys.sleep(0.3 + runif(1)); browseURL(linkX, encodeIfNeeded = T) } else {return(links)}
}

# Biology ------------------------------------------------------------

#' @title GC-content of a string (frequency of G and C letters among all letters).
#' @description GC-content of a string (frequency of G and C letters among all letters).
#' @param string PARAM_DESCRIPTION
#' @param len PARAM_DESCRIPTION, Default: nchar(string)
#' @param pattern PARAM_DESCRIPTION, Default: c("G", "C")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_split}}
#' @rdname GC_content
#' @export
#' @importFrom stringr str_split_fixed


GC_content <- function(string, len = nchar(string), pattern = c("G","C")) {
  char.list <- stringr::str_split_fixed(string, pattern = "", n = nchar(string))
  tbl = table(factor(unlist(char.list), levels = c("A", "T", "G", "C")))
  sum(tbl[  pattern ]) / sum(tbl)
}


# Temporary  ------------------------------------------------------------

# TMP ------------------------------------------------------------------------------------------------

