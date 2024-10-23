# _________________________________________________________________________________________________
#' #' @title sstrsplit
#' #'
#' #' @description Alias for `str_split_fixed` in the `stringr` package.
#' #' @param string A string.
#' #' @param pattern A character string pattern to split on.
#' #' @param n The number of elements to return in each split.
#' #' @return A list of character vectors.
#' #' @seealso
#' #'  \code{\link[stringr]{str_split}}
#' #' @importFrom stringr str_split_fixed
#' #' @export
#' sstrsplit <- function(string, pattern = "_", n = FALSE) {
#'   if (!n) n <- stringr::str_count(string = string, pattern = pattern)
#'   stringr::str_split_fixed(string, pattern = pattern, n = n)
#'   }
# "replace with str_split_1"
