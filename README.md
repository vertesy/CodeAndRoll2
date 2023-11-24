*Cite via:* [![DOI](https://zenodo.org/badge/319134544.svg)](https://zenodo.org/badge/latestdoi/319134544)

# CodeAndRoll2

Packaged version of the core functionalities (vector, matrix and list manipulations; math) of the formerly used [CodeAndRoll (v1)](https://github.com/vertesy/CodeAndRoll).  A standalone set of more than >130 productivity functions. 

Used by [MarkdownReports](https://github.com/vertesy/MarkdownReports), [ggExpress](https://github.com/vertesy/ggExpress), [Seurat.utils](https://github.com/vertesy/Seurat.utils).



*In 2021, function libraries got reorganized as below:*

<img width="1005" alt="R-package Tree" src="https://user-images.githubusercontent.com/5101911/143560128-065d8a49-0283-4a3a-9448-540fa424d0ef.png">



CodeAndRoll2 depends on:

- [Stringendo](https://github.com/vertesy/Stringendo)
- [ReadWriter](https://github.com/vertesy/ReadWriter)

... and provides functions for

- [MarkdownHelpers](https://github.com/vertesy/MarkdownHelpers)
- [MarkdownReports](https://github.com/vertesy/MarkdownReports)
- [ggExpress](https://github.com/vertesy/ggExpress)
- [Seurat.utils](https://github.com/vertesy/Seurat.utils)
- [Seurat.pipeline](https://github.com/vertesy/Seurat.pipeline)



<br><br>

## Installation

Install directly from **GitHub** via **devtools** with one R command:

```r
# install.packages("devtools"); # If you don't have it.
require("devtools")

# Install dependencies
devtools::install_github(repo = "vertesy/Stringendo", upgrade = F)
devtools::install_github(repo = "vertesy/ReadWriter", upgrade = F)

# Install CodeAndRoll2
devtools::install_github(repo = "vertesy/CodeAndRoll2", upgrade = F)

```
...then simply load the package:

```r
require("CodeAndRoll2")
```

Alternatively, you simply source it from the web. 
*This way function help will not work, and you will have no local copy of the code on your hard drive.*
```r
source("https://raw.githubusercontent.com/vertesy/CodeAndRoll2/master/CodeAndRoll2/R/CodeAndRoll2.R")
```

<br>

### Troubleshooting

*If you encounter a **bug**, something doesn't work or unclear, please let me know by raising an issue on [CodeAndRoll2](https://github.com/vertesy/CodeAndRoll2/issues) – Please check if it has been asked.*



<br>

=======

## List of Functions (153) 

Updated: 2023/11/24 15:46

- #### 1 `vec.fromNames()`

  vec.fromNames. Create a vector from a vector of names.

- #### 2 `list.fromNames()`

  list.fromNames. create list from a vector with the names of the elements.

- #### 3 `list.from.list()`

  list.from.list. create an empty list from named and structured as the template list.

- #### 4 `matrix.fromNames()`

  matrix.fromNames. Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.

- #### 5 `matrix.fromVector()`

  matrix.fromVector. Create a matrix from values in a vector repeated for each column / each row. Similar to rowNameMatrix and colNameMatrix.

- #### 6 `array.fromNames()`

  array.fromNames. Create an N-dimensional array from N vectors defining the row-, column, etc names of the array.

- #### 7 `what()`

  what. A better version of is(). It can print the first "printme" elements.

- #### 8 `idim()`

  idim. A dim() function that can handle if you pass on a vector: then, it gives the length.

- #### 9 `is.list2()`

  Test if object is a list. The 'is.list()' function fails on tibbles: it returns TRUE, as if it were a list. This distiguishes. Thaat's why we need this function.

- #### 10 `idimnames()`

  idimnames. A dimnames() function that can handle if you pass on a vector: it gives back the names.

- #### 11 `printEveryN()`

  printEveryN. Report iterator value at every e.g. 1000

- #### 12 `table_fixed_categories()`

  table_fixed_categories. Generate a table() with a fixed set of categories. It fills up the table with missing categories, that are relevant when comparing to other vectors. 

- #### 13 `getCategories()`

  getCategories. Extract unique entries with a corresponding name.

- #### 14 `grepv()`

  grep that returns the value of the match.. grep returning the value. A character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning. Missing values are allowed except for regexpr, gregexpr and regexec.

- #### 15 `most_frequent_elements()`

  most_frequent_elements. Show the most frequent elements of a table.

- #### 16 `count_occurrence_each_element()`

  count_occurrence_each_element. Count the number of times each element occurs in the full vector, AND give it back as a vector, that is the same length as the input vector, each element corresponding one-by-one.

- #### 17 `top_indices()`

  top_indices. Returns the positions of the `n` highest values in `x`. For equal values, it maintains the original order.

- #### 18 `trail()`

  trail. Returns a vector containing the first `N` and last `N` elements of `vec`.

- #### 19 `sort.decreasing()`

  sort.decreasing. Sorts `vec` in decreasing order.

- #### 20 `as.named.vector.df()`

  as.named.vector.df. Convert any column or row of a dataframe into a vector, keeping the corresponding dimension name.

- #### 21 `as.named.vector.2colDF()`

  as.named.vector.2colDF. Convert a 2-column dataframe (value, name) into a named vector. Use for simple tibbles.

- #### 22 `col2named.vector()`

  col2named.vector. Convert a dataframe column into a vector, keeping the corresponding dimension name.

- #### 23 `row2named.vector()`

  row2named.vector. Convert a dataframe row into a vector, keeping the corresponding dimension name.

- #### 24 `tibble_summary_to_namedVec()`

  tibble_summary_to_namedVec. Convert a key-value tibble into a named vector (as opposed to using rownames).

- #### 25 `as_tibble_from_namedVec()`

  as_tibble_from_namedVec. Convert a vector with names into a tibble, keeping the names as rownames.

- #### 26 `unique.wNames()`

  Unique elements. Get the unique elements of a vector, keep their names

- #### 27 `as.numeric.wNames.character()`

  as.numeric.wNames.character. Converts (1) a 'character' v. into a numeric v., or  a 'factor' v. as as.numeric(as.character(vec)) and preserves the original names.  The old 'as.numeric.wNames()' is deprecated as it was not clearly documented that it converts via facotr in any case. Code saved at the end.

- #### 28 `as.numeric.wNames.factor()`

  as.numeric.wNames.factor. Turn any vector into numeric categories as.numeric(as.factor(vec))  Forerly as.factor.numeric

- #### 29 `as.character.wNames()`

  as.character.wNames. Converts your input vector into a character vector, and puts the original  character values into the names of the new vector, unless it already has names.

- #### 30 `translate()`

  Translate values to a new set using a dictionary. Replaces a set of values in a vector with another set of values,  it translates your vector. Oldvalues and newvalues have to be 1-to-1  correspoding vectors.  'chartr("a-cX", "D-Fw", x) does the same as above  in theory, but it did not seem very robust regarding your input...'

- #### 31 `rescale()`

  rescale. Linear transformation to a given range of values.

- #### 32 `fractions()`

  fractions. x/sum(x)

- #### 33 `flip_value2name()`

  flip_value2name. Flip the values and the names of a vector with names.

- #### 34 `sortbyitsnames()`

  sortbyitsnames. Sort a vector or list by the alphanumeric order of its names (instead of its values).

- #### 35 `any.duplicated()`

  any.duplicated. How many entries are duplicated?.

- #### 36 `which.duplicated()`

  which.duplicated. Which values are duplicated?.

- #### 37 `which.NA()`

  which.NA. Which values are NA?.

- #### 38 `pad.na()`

  pad.na. This function fills up a vector to a given length by appending NA-values at the end.    If the input vector's length is less than the provided length, the function pads the vector    with NA. If the vector's length is already equal to or greater than the given length, no change    will be made.

- #### 39 `clip.at.fixed.value()`

  clip.at.fixed.value. Signal clipping. Cut values above or below a threshold.

- #### 40 `clip.outliers.at.percentile()`

  clip.outliers.at.percentile. Signal clipping based on the input data's distribution. It clips values above or below the extreme N% of the distribution.

- #### 41 `as.logical.wNames()`

  as.logical.wNames. Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.

- #### 42 `col2named.vec.tbl()`

  col2named.vec.tbl. Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.

- #### 43 `topN.dfCol()`

  topN.dfCol. Find the n highest values in a named vector.

- #### 44 `bottomN.dfCol()`

  bottomN.dfCol. Find the n lowest values in a named vector.

- #### 45 `split_vec_to_list_by_N()`

  Split a Vector into a List by Every N-th Element. This function divides a given vector into chunks of size `by` (default is 9).  The resulting list contains vectors of the specified chunk size or smaller.

- #### 46 `zigzagger()`

  zigzagger. Mix entries so that they differ.

- #### 47 `numerate()`

  Formats a sequence of numbers with zeropadding.. 

- #### 48 `MaxN()`

  MaxN. Find second (third…) highest/lowest value in vector.

- #### 49 `cumsubtract()`

  cumsubtract. Cumulative subtraction, opposite of cumsum().

- #### 50 `sumBySameName()`

  sumBySameName. Sum up vector elements with the same name.

- #### 51 `which_names()`

  which_names. Return the names where the input vector is TRUE. The input vector is converted to logical.

- #### 52 `which_names_grep()`

  which_names_grep. Return the vector elements whose names partially match a pattern.

- #### 53 `na.omit.strip()`

  na.omit.strip. Calls na.omit() and returns a clean vector.  Omit NA values from a vector and return a clean vector without any spam.

- #### 54 `inf.omit()`

  inf.omit. Omit infinite values from a vector.

- #### 55 `zero.omit()`

  zero.omit. Omit zero values from a vector.

- #### 56 `pc_TRUE()`

  pc_TRUE. Calculates the percentage of true values in a logical vector, parsed as text.

- #### 57 `pc_in_total_of_match()`

  pc_in_total_of_match. Calculates the percentage of a certain value within a vector or table.

- #### 58 `remove_outliers()`

  remove_outliers. Remove values that fall outside the trailing `probs` percentiles of the distribution.

- #### 59 `simplify_categories()`

  simplify_categories. Replace all occurrences of `replaceit` in `category_vec` with `to`.

- #### 60 `colSubtract()`

  colSubtract. Subtract a vector (length = nr. columns) column by column from each value of the matrix.

- #### 61 `rowSubtract()`

  rowSubtract. Subtract a vector (length = nr. rows) row by row from each value of the matrix

- #### 62 `colDivide()`

  colDivide. Divide by column. See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r.

- #### 63 `colMutliply()`

  colMutliply. Multiply by column. See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r.

- #### 64 `rowDivide()`

  rowDivide. Divide by row.

- #### 65 `rowMutliply()`

  rowMutliply. Mutliply by row.

- #### 66 `row.Zscore()`

  row.Zscore. Calculate Z-score over rows of data frame.

- #### 67 `rowMin()`

  rowMin. Calculates the minimum of each row of a numeric matrix / data frame.

- #### 68 `colMin()`

  colMin. Calculates the minimum of each column of a numeric matrix / data frame.

- #### 69 `rowMax()`

  rowMax. Calculates the maximum of each row of a numeric matrix / data frame.

- #### 70 `colMax()`

  colMax. Calculates the maximum of each column of a numeric matrix / data frame.

- #### 71 `rowMedians()`

  rowMedians. Calculates the median of each row of a numeric matrix / data frame.

- #### 72 `colMedians()`

  colMedians. Calculates the median of each column of a numeric matrix / data frame.

- #### 73 `rowGeoMeans()`

  rowGeoMeans. Calculates the median of each row of a numeric matrix / data frame.

- #### 74 `colGeoMeans()`

  colGeoMeans. Calculates the median of each column of a numeric matrix / data frame.

- #### 75 `rowCV()`

  rowCV. Calculates the CV of each ROW of a numeric matrix / data frame.

- #### 76 `colCV()`

  colCV. Calculates the CV of each column of a numeric matrix / data frame.

- #### 77 `rowVariance()`

  rowVariance. Calculates the CV of each ROW of a numeric matrix / data frame.

- #### 78 `colVariance()`

  colVariance. Calculates the CV of each column of a numeric matrix / data frame.

- #### 79 `rowSEM()`

  rowSEM. Calculates the SEM of each row of a numeric matrix / data frame.

- #### 80 `colSEM()`

  colSEM. Calculates the SEM of each column of a numeric matrix / data frame.

- #### 81 `rowSD()`

  rowSD. Calculates the SEM of each row of a numeric matrix / data frame.

- #### 82 `colSD()`

  colSD. Calculates the SD of each column of a numeric matrix / data frame.

- #### 83 `rowIQR()`

  rowIQR. Calculates the IQR of each row of a numeric matrix / data frame.

- #### 84 `colIQR()`

  colIQR. Calculates the IQR of each column of a numeric matrix / data frame.

- #### 85 `rowQuantile()`

  rowQuantile. Calculates the quantile of each row of a numeric matrix / data frame.

- #### 86 `colQuantile()`

  colQuantile. Calculates the quantile of each column of a numeric matrix / data frame.

- #### 87 `TPM_normalize()`

  TPM_normalize. Normalize each column to 1 million.

- #### 88 `median_normalize()`

  median_normalize. Normalize each column to the median of all the column-sums.

- #### 89 `mean_normalize()`

  mean_normalize. Normalize each column to the median of the columns.

- #### 90 `rotate()`

  rotate. Rotate a matrix 90 degrees.

- #### 91 `sortEachColumn()`

  sortEachColumn. Sort each column of a numeric matrix / data frame.

- #### 92 `sort.mat()`

  sort.mat. Sorts a matrix by a given column. This function can only handle single column sort.  An alternative is dd[with(dd, order(-z, b)), ] as found on StackOverflow (https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r).

- #### 93 `rowNameMatrix()`

  rowNameMatrix. Create a copy of your matrix, where every entry is replaced by the corresponding  row name. Useful if you want to color by row name in a plot (where you have different number of   NA-values in each row).

- #### 94 `colNameMatrix()`

  colNameMatrix. Create a copy of your matrix, where every entry is replaced by the corresponding  column name. Useful if you want to color by column name in a plot (where you have different  number of NA-values in each column).

- #### 95 `rownames.trimws()`

  rownames.trimws. Trim whitespaces from the rownames.

- #### 96 `colsplit()`

  colsplit. Split a data frame by a factor corresponding to columns.

- #### 97 `rowsplit()`

  rowsplit. Split a data frame by a factor corresponding to columns.

- #### 98 `select_rows_and_columns()`

  select_rows_and_columns. Subset rows and columns. It checks if the selected dimension names exist and reports if any of those they aren't found.

- #### 99 `getRows()`

  getRows. Returns a subset of rows based on their names and optionally removes rows with only NA or zero values. Reports the number of missing rows.

- #### 100 `getCols()`

  getCols. Returns a subset of columns based on their names and optionally removes columns with only NA or zero values. Reports the number of missing columns.

- #### 101 `get.oddoreven()`

  get.oddoreven. Returns either odd or even indexed rows or columns from a data frame.

- #### 102 `merge_dfs_by_rn()`

  merge_dfs_by_rn. Merge any data frames by rownames. Required plyr package.

- #### 103 `merge_1col_dfs_by_rn()`

  merge_1col_dfs_by_rn. A function that merges single-column data frames based on rownames.

- #### 104 `merge_numeric_df_by_rn()`

  merge_numeric_df_by_rn. Merge 2 numeric data frames by rownames.

- #### 105 `merge_2_named_vec_as_df()`

  merge_2_named_vec_as_df. Merge two named vectors by names, into a dataframe with 2 columns.

- #### 106 `merge_ls_of_named_vec_as_df_cols()`

  merge_ls_of_named_vec_as_df_cols. Merge any number of named vectors (presented as a list) by names, into a dataframe

- #### 107 `remove.na.rows()`

  remove.na.rows. Cols have to be a vector of numbers corresponding to columns.

- #### 108 `remove.na.cols()`

  remove.na.cols. Cols have to be a vector of numbers corresponding to columns.

- #### 109 `na.omit.mat()`

  na.omit.mat. Omit rows with NA values from a matrix. Rows with any, or full of NA-s.

- #### 110 `df.remove.empty.rows.and.columns()`

  Remove empty rows and columns from a data frame.. This function takes a data frame and a threshold value, and removes all rows and columns that contain only zeros or the threshold value. 

- #### 111 `get_col_types()`

  Extract and Display Column Types of a Data Frame or Tibble. This function returns the primary class/type of each column in a data frame or tibble.  Additionally, it can print a summary of the column types. 

- #### 112 `fix_tibble_lists()`

  Convert List Columns of a Tibble to String Vectors. This function identifies columns of type `list` in a tibble or data frame  and converts them to string vectors. 

- #### 113 `copy.dimension.and.dimnames()`

  copy.dimension.and.dimnames. Copy the dimension and dimnames of a 1D vector to a 2D array.

- #### 114 `mdlapply()`

  mdlapply. A wrapper for `lapply()` that works on multidimensional arrays.

- #### 115 `arr.of.lists.2.df()`

  arr.of.lists.2.df. Simplify 2D-list-array to a DF.

- #### 116 `mdlapply2df()`

  mdlapply2df. Multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF).

- #### 117 `any.duplicated.rownames.ls.of.df()`

  any.duplicated.rownames.ls.of.df. Check if there are any duplocated rownames in a list of dataframes.

- #### 118 `intersect.ls()`

  intersect.ls. Intersect any number of lists.

- #### 119 `union.ls()`

  union.ls. Intersect any number of list elements. Faster than reduce.

- #### 120 `symdiff.ls()`

  symdiff.ls. Calculate symmetric difference on a list (of 2 vectors).

- #### 121 `setdiff.ls()`

  setdiff.ls. Calculate set difference on a list (of 2 vectors).

- #### 122 `unlapply()`

  unlapply. Lapply, then unlist.

- #### 123 `list.wNames()`

  list.wNames. Create a list with names from ALL variables you pass on to the function.

- #### 124 `as.list.df.by.row()`

  as.list.df.by.row. Split a dataframe into a list by its columns.

- #### 125 `as.list.df.by.col()`

  as.list.df.by.col. Split a dataframe into a list by its rows.

- #### 126 `reorder.list()`

  reorder.list. Reorder elements of lists in your custom order of names / indices.

- #### 127 `range.list()`

  range.list. Calculates the range of values in a list.

- #### 128 `intermingle2lists()`

  intermingle2lists. Combine 2 lists (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().

- #### 129 `as.listalike()`

  as.listalike. Converts a vector to a list with the same dimensions as a given list.

- #### 130 `reverse.list.hierarchy()`

  Reverse the Hierarchy of a List. This function reverses the hierarchy of a given, 2 level, nested list. The  function will ensure that all lists at the same level have the same names,  and then transpose the structure, creating a new list for each unique name.  Any missing elements in the original lists are not included in the final  output. The result is a list where the top-level names are derived from  the unique names found at the lower levels of the input list. 

- #### 131 `list2fullDF.byNames()`

  list2fullDF.byNames. Converts a list to a full matrix, with rows and columns named by the elements of the list.

- #### 132 `list2fullDF.presence()`

  list2fullDF.presence. Converts a list to a full matrix, with rows and columns named by the elements of the list.  The matrix will contain a 1 in each cell where the corresponding element of the list is present, and a 0 otherwise.

- #### 133 `splitbyitsnames()`

  splitbyitsnames. Split a list by its names.

- #### 134 `splititsnames_byValues()`

  Split the names of list by its values.. Split the names of a list by its its values.

- #### 135 `intermingle2vec()`

  intermingle2vec. Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.

- #### 136 `intermingle.cbind()`

  intermingle.cbind. Combine 2 data frames (of the same length) so that form every odd and every even  element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().

- #### 137 `ls2categvec()`

  ls2categvec. Converts a list to a vector repeating list-element names, while vector names are the list elements.

- #### 138 `list.2.replicated.name.vec()`

  list.2.replicated.name.vec. Converts a list to a vector, with list elements names replicated as many times as many elements each element had.

- #### 139 `symdiff()`

  Symmetric difference. Quasi symmetric difference of any number of vectors.

- #### 140 `iround()`

  iround. Rounds a value to the significant amount of digits. Its a wrapper for signif().

- #### 141 `modus()`

  modus. Calculates the mode (modus) of a numeric vector (it excludes NA-s by default). https://en.wikipedia.org/wiki/Mode_(statistics)

- #### 142 `cv()`

  cv. Calculates the coefficient of variation (CV) for a numeric vector (it excludes NA-s by default).

- #### 143 `sem()`

  sem. Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default).

- #### 144 `fano()`

  fano. Calculates the fano factor on a numeric vector (it excludes NA-s by default).

- #### 145 `geomean()`

  geomean. Calculates the geometric mean of a numeric vector (it excludes NA-s by default).

- #### 146 `mean_of_log()`

  mean_of_log. Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default).

- #### 147 `movingAve()`

  Moving / rolling average. Calculates the moving / rolling average of a numeric vector.

- #### 148 `movingAve2()`

  Moving / rolling average (v2, filter). Calculates the moving / rolling average of a numeric vector, using `filter()`.

- #### 149 `movingSEM()`

  movingSEM. Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.

- #### 150 `imovingSEM()`

  imovingSEM. Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller window-size.

- #### 151 `dput_pretty()`

  Pretty Printing of R Objects (pretty_dput). This function modifies the output of the traditional dput() function  so that each key-value pair in a vector appears on a new line. It's useful for  creating more readable output.

- #### 152 `as.numeric.wNames.deprecated()`

  as.numeric.wNames.deprecated. Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.

- #### 153 `as.factor.numeric.deprecated()`

  as.factor.numeric.deprecated. Turn any vector into numeric categories as.numeric(as.factor(vec))




---

Vertesy, 2023.  ***Cite via:*** [![DOI](https://zenodo.org/badge/319134544.svg)](https://zenodo.org/badge/latestdoi/319134544)

<br>

