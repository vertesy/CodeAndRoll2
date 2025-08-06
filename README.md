*Cite via:* [![DOI](https://zenodo.org/badge/319134544.svg)](https://zenodo.org/badge/latestdoi/319134544)

# CodeAndRoll2

Packaged version of the core functionalities (vector, matrix and list manipulations; math) of the formerly used [CodeAndRoll (v1)](https://github.com/vertesy/CodeAndRoll).  A standalone set of more than >130 productivity functions. 

Used by [MarkdownReports](https://github.com/vertesy/MarkdownReports), [ggExpress](https://github.com/vertesy/ggExpress), [Seurat.utils](https://github.com/vertesy/Seurat.utils).

CodeAndRoll2 depends on:

- [Stringendo](https://github.com/vertesy/Stringendo)
- [ReadWriter](https://github.com/vertesy/ReadWriter)

... and provides functions for

- [MarkdownHelpers](https://github.com/vertesy/MarkdownHelpers)
- [MarkdownReports](https://github.com/vertesy/MarkdownReports)
- [ggExpress](https://github.com/vertesy/ggExpress)
- [Seurat.utils](https://github.com/vertesy/Seurat.utils)
- [Seurat.pipeline](https://github.com/vertesy/Seurat.pipeline)

<br>

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

----------------------------------------------------------------------------------------------------

## Function relationships
 > (of connected functions)

 ```mermaid
 flowchart LR 

  zigzagger(zigzagger) --> intermingle2vec(intermingle2vec)
  which_names_grep(which_names_grep) --> grepv(grepv)
  table_fixed_categories(table_fixed_categories) --> pc_TRUE(pc_TRUE)
  symdiff.ls(symdiff.ls) --> symdiff(symdiff)
  rowSEM(rowSEM) --> sem(sem)
  rowGeoMeans(rowGeoMeans) --> geomean(geomean)
  rowCV(rowCV) --> cv(cv)
  movingSEM(movingSEM) --> sem(sem)
  merge_numeric_df_by_rn(merge_numeric_df_by_rn) --> symdiff(symdiff)
  merge_ls_of_named_vec_as_df_cols(merge_ls_of_named_vec_as_df_cols) --> unlapply(unlapply)
  merge_dfs_by_rn(merge_dfs_by_rn) --> idim(idim)
  merge_1col_dfs_by_rn(merge_1col_dfs_by_rn) --> union.ls(union.ls)
  median_normalize(median_normalize) --> colMedians(colMedians)
  mdlapply2df(mdlapply2df) --> arr.of.lists.2.df(arr.of.lists.2.df)
  mdlapply2df(mdlapply2df) --> copy.dimension.and.dimnames(copy.dimension.and.dimnames)
  mdlapply(mdlapply) --> copy.dimension.and.dimnames(copy.dimension.and.dimnames)
  ls2categvec(ls2categvec) --> unlapply(unlapply)
  list2fullDF.presence(list2fullDF.presence) --> matrix.fromNames(matrix.fromNames)
  list2fullDF.byNames(list2fullDF.byNames) --> matrix.fromNames(matrix.fromNames)
  list.2.replicated.name.vec(list.2.replicated.name.vec) --> unlapply(unlapply)
  intermingle.cbind(intermingle.cbind) --> matrix.fromNames(matrix.fromNames)
  intermingle.cbind(intermingle.cbind) --> intermingle2vec(intermingle2vec)
  intermingle.cbind(intermingle.cbind) --> symdiff(symdiff)
  intermingle.cbind(intermingle.cbind) --> idim(idim)
  imovingSEM(imovingSEM) --> sem(sem)
  idimnames(idimnames) --> idim(idim)
  getRows(getRows) --> which_names(which_names)
  which_names(which_names) --> as.logical.wNames(as.logical.wNames)
  getCols(getCols) --> which_names(which_names)
  getCols(getCols) --> symdiff(symdiff)
  fix_tibble_lists(fix_tibble_lists) --> get_col_types(get_col_types)
  df.remove.empty.rows.and.columns(df.remove.empty.rows.and.columns) --> pc_TRUE(pc_TRUE)
  df.remove.empty.rows.and.columns(df.remove.empty.rows.and.columns) --> idim(idim)
  sem(sem) --> na.omit.strip(na.omit.strip)
  colSEM(colSEM) --> sem(sem)
  colGeoMeans(colGeoMeans) --> geomean(geomean)
  colCV(colCV) --> cv(cv)
  as.list.df.by.row(as.list.df.by.row) --> zero.omit(zero.omit)
  as.list.df.by.row(as.list.df.by.row) --> na.omit.strip(na.omit.strip)
  as.list.df.by.col(as.list.df.by.col) --> zero.omit(zero.omit)
  as.list.df.by.col(as.list.df.by.col) --> na.omit.strip(na.omit.strip)
  any.duplicated.rownames.ls.of.df(any.duplicated.rownames.ls.of.df) --> any.duplicated(any.duplicated)
 ```
 *created by `convert_igraph_to_mermaid()`*



----------------------------------------------------------------------------------------------------

## List of Functions in CodeAndRoll2 (166) 

Updated: 2024/10/23 18:22
- #### 1 `getScriptName()`
Get Current Script Name or Basename of Output Directory. This function attempts to retrieve the name of the currently opened script in  the RStudio editor. If the script name cannot be obtained or if the `rstudioapi` package is  not available, it returns the basename of the directory specified by `OutDir`.   @return A string containing the basename of the current script or the basename of `OutDir`  if the script name is unavailable.  @importFrom rstudioapi getSourceEditorContext 

- #### 2 `getProject()`
getProject. Try to get the project name you are working on in Rstudio.

- #### 3 `()`
Save Command History to "command_history.date.scriptname.txt".   This function saves the command history of the current R session to a text file. The file name  includes the current date and, if available, the name of the current R script (when running in  RStudio). The file is saved in the current working directory.   @return Nothing is returned, but the file path is printed to the console.   @importFrom rstudioapi getSourceEditorContext  @examples  \dontrun{  savehistory_2()  }

- #### 4 `vec.fromNames()`
vec.fromNames. Create a vector from a vector of names.

- #### 5 `list.fromNames()`
Create a Named List from a Character Vector of Names, or a names of an object. This function takes a character vector of names and creates a list where each  element is named according to the character vector and filled with a specified value.

- #### 6 `vec.from.template()`
vec.from.template. Create a vector from a names of another vector / list / etc.

- #### 7 `list.from.template()`
list.from.template. Create an empty list from a template list, copying names and filling values with NA.

- #### 8 `matrix.fromNames()`
matrix.fromNames. Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.

- #### 9 `data.frame.fromNames()`
data.frame.fromNames. Create a data frame from 2 vectors defining the row- and column names of the  data frame Default fill value: NA.

- #### 10 `matrix.fromVector()`
matrix.fromVector. Create a matrix from values in a vector repeated for each column / each row.  Similar to rowNameMatrix and colNameMatrix.

- #### 11 `array.fromNames()`
array.fromNames. Create an N-dimensional array from N vectors defining the row-, column, etc names of the array.

- #### 12 `what()`
what. A better version of is(). It can print the first "printme" elements.

- #### 13 `idim()`
idim. A dim() function that can handle if you pass on a vector: then, it gives the length.

- #### 14 `is.list2()`
Test if object is a list. The 'is.list()' function fails on tibbles: it returns TRUE, as if it were a list. This distiguishes. Thaat's why we need this function.

- #### 15 `idimnames()`
idimnames. A dimnames() function that can handle if you pass on a vector: it gives back the names.

- #### 16 `printEveryN()`
printEveryN. Report iterator value at every e.g. 1000

- #### 17 `printProgress()`
Print Loop Progress. Prints the progress of a loop as a number and percentage. 

- #### 18 `table_fixed_categories()`
table_fixed_categories. Generate a table() with a fixed set of categories. It fills up the table with  missing categories, that are relevant when comparing to other vectors. 

- #### 19 `table_decreasing()`
Frequency Table with Sorting Option.   This function generates a frequency table of the input vector `vec` and allows the option  to sort the table in decreasing or increasing order. It handles NA values. 

- #### 20 `table_decreasing_hybrid()`
Frequency Table with Hyrid Sorting: you can sort by frequency and by specified value.   This function generates a frequency table of the input vector `vec` and displays the table  sorted by frequency and by a set of specified values. It handles NA values. 

- #### 21 `getCategories()`
getCategories. Extract unique entries with a corresponding name.

- #### 22 `nr.unique()`
Count the number of unique values. Count the number of unique values

- #### 23 `grepv()`
grep that returns the value of the match.. grep returning the value. A character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning. Missing values are allowed except for regexpr, gregexpr and regexec.

- #### 24 `most_frequent_elements()`
most_frequent_elements. Show the most frequent elements of a table.

- #### 25 `count_occurrence_each_element()`
count_occurrence_each_element. Count the number of times each element occurs in the full vector, AND give it back as a vector, that is the same length as the input vector, each element corresponding one-by-one.

- #### 26 `top_indices()`
top_indices. Returns the positions of the `n` highest values in `x`. For equal values, it maintains the original order.

- #### 27 `trail()`
trail. A combination of `head()` and `tail()`: Returns a vector containing the first  `N` and last `N` elements of vector. 

- #### 28 `sort.decreasing()`
sort.decreasing. Sorts `vec` in decreasing order.

- #### 29 `# as.named.vector.df()`
as.named.vector.df. Convert any column or row of a dataframe into a vector, keeping the  corresponding dimension name.

- #### 30 `as.named.vector.2colDF()`
as.named.vector.2colDF. Convert a 2-column dataframe (value, name) into a named vector. Use for simple tibbles.

- #### 31 `# df.col.2.named.vector()`
df.col.2.named.vector. Convert a dataframe column into a vector, keeping the corresponding dimension name.

- #### 32 `df.row.2.named.vector()`
df.row.2.named.vector. Convert a dataframe row into a vector, keeping the corresponding dimension name.

- #### 33 `tibble_summary_to_namedVec()`
tibble_summary_to_namedVec. Convert a key-value tibble into a named vector (as opposed to using rownames).

- #### 34 `as_tibble_from_namedVec()`
as_tibble_from_namedVec. Convert a vector with names into a tibble, keeping the names as rownames.

- #### 35 `unique.wNames()`
Unique elements. Get the unique elements of a vector, keep their names

- #### 36 `as.numeric.wNames.character()`
as.numeric.wNames.character. Converts (1) a 'character' v. into a numeric v., or  a 'factor' v. as as.numeric(as.character(vec)) and preserves the original names.  The old 'as.numeric.wNames()' is deprecated as it was not clearly documented that it converts via facotr in any case. Code saved at the end.

- #### 37 `as.numeric.wNames.factor()`
as.numeric.wNames.factor. Turn any vector into numeric categories as.numeric(as.factor(vec))  Forerly as.factor.numeric

- #### 38 `as.character.wNames()`
as.character.wNames. Converts your input vector into a character vector, and puts the original  character values into the names of the new vector, unless it already has names.

- #### 39 `translate()`
Translate a set of values to a new set using a dictionary. Replaces a set of values in a vector with another set of values, so  it translates your vector. Oldvalues and newvalues have to be 1-to-1  corresponding vectors.  'chartr("a-cX", "D-Fw", x) does the same as above  in theory, but it did not seem very robust regarding your input...' 

- #### 40 `rescale()`
rescale. Linear transformation to a given range of values.

- #### 41 `fractions()`
fractions. x/sum(x)

- #### 42 `flip_value2name()`
flip_value2name. Flip the values and the names of a vector with names.

- #### 43 `sortbyitsnames()`
sortbyitsnames. Sort a vector or list by the alphanumeric order of its names (instead of its values).

- #### 44 `any.duplicated()`
any.duplicated. How many entries are duplicated?.

- #### 45 `which.duplicated()`
which.duplicated. Which values are duplicated?.

- #### 46 `which.NA()`
which.NA. Which values are NA?.

- #### 47 `pad.na()`
pad.na. This function fills up a vector to a given length by appending NA-values at the end.    If the input vector's length is less than the provided length, the function pads the vector    with NA. If the vector's length is already equal to or greater than the given length, no change    will be made.

- #### 48 `clip.at.fixed.value()`
clip.at.fixed.value. Signal clipping. Cut values in a distribution, above or below a threshold.

- #### 49 `clip.outliers.at.percentile()`
clip.outliers.at.percentile. Signal clipping based on the input data's distribution. It clips values  in a distribution above or below the extreme N% of the distribution. 

- #### 50 `as.logical.wNames()`
as.logical.wNames. Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.

- #### 51 `col2named.vec.tbl()`
col2named.vec.tbl. Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.

- #### 52 `topN.dfCol()`
topN.dfCol. Find the n highest values in a named vector.

- #### 53 `bottomN.dfCol()`
bottomN.dfCol. Find the n lowest values in a named vector.

- #### 54 `split_vec_to_list_by_N()`
Split a Vector into a List by Every N-th Element. This function divides a given vector into chunks of size `by` (default is 9).  The resulting list contains vectors of the specified chunk size or smaller.

- #### 55 `zigzagger()`
zigzagger. Mix entries so that they differ.

- #### 56 `numerate()`
Formats a Sequence of Numbers with Zero Padding. This function generates a sequence of numbers between two specified values,  optionally padding them with leading zeros to a specified length. It is useful  for creating numeric sequences with consistent character lengths.

- #### 57 `MaxN()`
MaxN. Find second (third…) highest/lowest value in vector.

- #### 58 `cumsubtract()`
cumsubtract. Cumulative subtraction, opposite of cumsum().

- #### 59 `sumBySameName()`
sumBySameName. Sum up vector elements with the same name.

- #### 60 `checkMinOverlap()`
Check Minimum Overlap Between Two Vectors. Checks if the overlap between two character vectors is at least a specified  percentage of the shorter vector. Stops execution with an error if the condition is not met. 

- #### 61 `which_names()`
which_names. Return the names where the input vector is TRUE. The input vector is converted to logical.

- #### 62 `which_names_grep()`
which_names_grep. Return the vector elements whose names partially match a pattern.

- #### 63 `na.omit.strip()`
na.omit.strip. Calls na.omit() and returns a clean vector.  Omit NA values from a vector and return a clean vector without any spam.

- #### 64 `inf.omit()`
inf.omit. Omit infinite values from a vector.

- #### 65 `zero.omit()`
zero.omit. Omit zero values from a vector.

- #### 66 `pc_TRUE()`
pc_TRUE. Calculates the percentage of true values in a logical vector, parsed as text.

- #### 67 `pc_overlap()`
Calculate Percentage Overlap Between Two Vectors. Computes the percentage of overlap between two vectors based on the specified basis of calculation. 

- #### 68 `pc_in_total_of_match()`
pc_in_total_of_match. Calculates the percentage of a certain value within a vector or table.

- #### 69 `remove_outliers()`
remove_outliers. Remove values that fall outside the trailing `probs` percentiles of the distribution.

- #### 70 `simplify_categories()`
simplify_categories. Replace all occurrences of `replaceit` in `category_vec` with `to`.

- #### 71 `colSubtract()`
colSubtract. Subtract a vector (length = nr. columns) column by column from each value of the matrix.

- #### 72 `rowSubtract()`
rowSubtract. Subtract a vector (length = nr. rows) row by row from each value of the matrix

- #### 73 `colDivide()`
Row-wise division of a matrix by a column vector. Each element of the matrix is divided by the corresponding element of the vector  that matches the column of the matrix element. This is typically used to normalize data,  for example, to scale values in each row by certain factors like totals or means. Source  \url{https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r}.

- #### 74 `colMultiply()`
colMultiply. Multiply each column by a vector. See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r.

- #### 75 `rowDivide()`
rowDivide. Divide by row.

- #### 76 `rowMultiply()`
rowMultiply. Multiply each row by a vector.

- #### 77 `row.Zscore()`
row.Zscore. Calculate Z-score over rows of data frame.

- #### 78 `TPM_normalize()`
TPM_normalize. Normalize each column to 1 million.

- #### 79 `median_normalize()`
median_normalize. Normalize each column to the median of all the column-sums.

- #### 80 `mean_normalize()`
mean_normalize. Normalize each column to the median of the columns.

- #### 81 `rowMin()`
rowMin. Calculates the minimum of each row of a numeric matrix / data frame.

- #### 82 `colMin()`
colMin. Calculates the minimum of each column of a numeric matrix / data frame.

- #### 83 `rowMax()`
rowMax. Calculates the maximum of each row of a numeric matrix / data frame.

- #### 84 `colMax()`
colMax. Calculates the maximum of each column of a numeric matrix / data frame.

- #### 85 `rowMedians()`
rowMedians. Calculates the median of each row of a numeric matrix / data frame.

- #### 86 `colMedians()`
colMedians. Calculates the median of each column of a numeric matrix / data frame.

- #### 87 `rowGeoMeans()`
rowGeoMeans. Calculates the median of each row of a numeric matrix / data frame.

- #### 88 `colGeoMeans()`
colGeoMeans. Calculates the median of each column of a numeric matrix / data frame.

- #### 89 `rowCV()`
rowCV. Calculates the CV of each ROW of a numeric matrix / data frame.

- #### 90 `colCV()`
colCV. Calculates the CV of each column of a numeric matrix / data frame.

- #### 91 `rowVariance()`
rowVariance. Calculates the CV of each ROW of a numeric matrix / data frame.

- #### 92 `colVariance()`
colVariance. Calculates the CV of each column of a numeric matrix / data frame.

- #### 93 `rowSEM()`
rowSEM. Calculates the SEM of each row of a numeric matrix / data frame.

- #### 94 `colSEM()`
colSEM. Calculates the SEM of each column of a numeric matrix / data frame.

- #### 95 `rowSD()`
rowSD. Calculates the SEM of each row of a numeric matrix / data frame.

- #### 96 `colSD()`
colSD. Calculates the SD of each column of a numeric matrix / data frame.

- #### 97 `rowIQR()`
rowIQR. Calculates the IQR of each row of a numeric matrix / data frame.

- #### 98 `colIQR()`
colIQR. Calculates the IQR of each column of a numeric matrix / data frame.

- #### 99 `rowQuantile()`
rowQuantile. Calculates the quantile of each row of a numeric matrix / data frame.

- #### 100 `colQuantile()`
colQuantile. Calculates the quantile of each column of a numeric matrix / data frame.

- #### 101 `cbind_vectors_by_names()`
Bind two named vectors by matching names. Combines two named vectors into a data frame by matching their names.  Missing values are filled with NA.

- #### 102 `sortEachColumn()`
sortEachColumn. Sort each column of a numeric matrix / data frame.

- #### 103 `sort_matrix_rows()`
Sort matrix or data frame by a column or row names.   Sorts a numeric matrix or data frame by a specified column or by row names. The function can only  handle sorting by a single column. It offers options to sort in increasing or decreasing order,  and to control the placement of `NA` values. 

- #### 104 `rownames.trimws()`
rownames.trimws. Trim whitespaces from the rownames.

- #### 105 `colsplit()`
colsplit. Split a data frame by a factor corresponding to columns.

- #### 106 `rowsplit()`
rowsplit. Split a data frame by a factor corresponding to columns.

- #### 107 `  which.max.multi()`
Get the Column Name corresponding to the Maximum Value in each Row (handles ambiguous matches).   This function takes a numeric matrix as input and returns a named vector where each element  corresponds to a row of the matrix. The names of the vector are the row names of the matrix,  and the values are the column names where the maximum value of each row is found. If there are  multiple columns with the maximum value in a row, the value for that row will be set to  `multi_max_str`. If `na.remove` is set to `TRUE`, NA values will be removed before finding the  maximum value. 

- #### 108 `select_rows_and_columns()`
select_rows_and_columns. Subset rows and columns. It checks if the selected dimension names exist and reports if any of those they aren't found.

- #### 109 `getRows()`
getRows. Returns a subset of rows based on their names and optionally removes rows with only NA or zero values. Reports the number of missing rows.

- #### 110 `getCols()`
getCols. Returns a subset of columns based on their names and optionally removes columns with only NA or zero values. Reports the number of missing columns.

- #### 111 `get.oddoreven()`
get.oddoreven. Returns either odd or even indexed rows or columns from a data frame.

- #### 112 `merge_dfs_by_rn()`
merge_dfs_by_rn. Merge any data frames by rownames. Required plyr package.

- #### 113 `merge_1col_dfs_by_rn()`
merge_1col_dfs_by_rn. A function that merges single-column data frames based on rownames.

- #### 114 `merge_numeric_df_by_rn()`
merge_numeric_df_by_rn. Merge 2 numeric data frames by rownames.

- #### 115 `merge_2_named_vec_as_df()`
merge_2_named_vec_as_df. Merge two named vectors by names, into a dataframe with 2 columns.

- #### 116 `merge_ls_of_named_vec_as_df_cols()`
merge_ls_of_named_vec_as_df_cols. Merge any number of named vectors (presented as a list) by names, into a dataframe

- #### 117 `get_col_types()`
Extract and Display Column Types of a Data Frame or Tibble. This function returns the primary class/type of each column in a data frame or tibble.  Additionally, it can print a summary of the column types. 

- #### 118 `fix_tibble_lists()`
Convert List Columns of a Tibble to String Vectors. Converts columns of type `list` in a tibble or data frame to string vectors.  It combines list elements into a single string per cell, using a specified separator.

- #### 119 `rotate_matrix()`
Rotate a Matrix by 90 Degrees. Rotates a given numeric matrix 90 degrees in a specified direction. The rotation  can be either clockwise or counterclockwise, determined by the `clockwise` parameter. 

- #### 120 `na.omit.mat()`
Omit Rows with NA Values from a Matrix. Removes rows from a matrix based on the presence of NA values. Can remove rows with any NA values or only those completely filled with NAs. 

- #### 121 `remove.na.rows()`
remove.na.rows. Cols have to be a vector of numbers corresponding to columns.

- #### 122 `remove.na.cols()`
remove.na.cols. Cols have to be a vector of numbers corresponding to columns.

- #### 123 `df.remove.empty.rows.and.columns()`
Remove empty rows and columns from a data frame.. This function takes a data frame and a threshold value, and removes all rows and columns that contain only zeros or the threshold value. 

- #### 124 `rowNameMatrix()`
rowNameMatrix. Create a copy of your matrix, where every entry is replaced by the corresponding  row name. Useful if you want to color by row name in a plot (where you have different number of   NA-values in each row).

- #### 125 `colNameMatrix()`
colNameMatrix. Create a copy of your matrix, where every entry is replaced by the corresponding  column name. Useful if you want to color by column name in a plot (where you have different  number of NA-values in each column).

- #### 126 `copy.dimension.and.dimnames()`
copy.dimension.and.dimnames. Copy the dimension and dimnames of a 1D vector to a 2D array.

- #### 127 `mdlapply()`
mdlapply. A wrapper for `lapply()` that works on multidimensional arrays.

- #### 128 `arr.of.lists.2.df()`
arr.of.lists.2.df. Simplify 2D-list-array to a DF.

- #### 129 `mdlapply2df()`
mdlapply2df. Multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF).

- #### 130 `any.duplicated.rownames.ls.of.df()`
any.duplicated.rownames.ls.of.df. Check if there are any duplocated rownames in a list of dataframes.

- #### 131 `intersect.ls()`
intersect.ls. Intersect any number of lists.

- #### 132 `union.ls()`
union.ls. Intersect any number of list elements. Faster than reduce.

- #### 133 `symdiff.ls()`
symdiff.ls. Calculate symmetric difference on a list (of 2 vectors).

- #### 134 `setdiff.ls()`
setdiff.ls. Calculate set difference on a list (of 2 vectors).

- #### 135 `unlapply()`
Like sapply, but with names preserved. Help in some other cases too.. Do an `lapply()`, then `unlist()`, with preserving the list element names.

- #### 136 `list.wNames()`
list.wNames. Create a list with names from ALL variables you pass on to the function.

- #### 137 `as.list.df.by.row()`
as.list.df.by.row. Split a dataframe into a list by its columns.

- #### 138 `as.list.df.by.col()`
as.list.df.by.col. Split a dataframe into a list by its rows.

- #### 139 `reorder.list()`
reorder.list. Reorder elements of lists in your custom order of names / indices.

- #### 140 `range.list()`
range.list. Calculates the range of values in the entire a list.

- #### 141 `intermingle2lists()`
intermingle2lists. Combine 2 lists (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().

- #### 142 `as.listalike()`
as.listalike. Converts a vector to a list with the same dimensions as a given list.

- #### 143 `reverse.list.hierarchy()`
Reverse the Hierarchy of a List. This function reverses the hierarchy of a given, 2 level, nested list. The  function will ensure that all lists at the same level have the same names,  and then transpose the structure, creating a new list for each unique name.  Any missing elements in the original lists are not included in the final  output. The result is a list where the top-level names are derived from  the unique names found at the lower levels of the input list. 

- #### 144 `list2fullDF.byNames()`
list2fullDF.byNames. Converts a list to a full matrix, with rows and columns named by the elements of the list.

- #### 145 `list2fullDF.presence()`
list2fullDF.presence. Converts a list to a full matrix, with rows and columns named by the elements of the list.  The matrix will contain a 1 in each cell where the corresponding element of the list is present, and a 0 otherwise.

- #### 146 `splitbyitsnames()`
splitbyitsnames. Split a list by its names.

- #### 147 `splititsnames_byValues()`
Split the names of list by its values.. Split the names of a list by its its values.

- #### 148 `intermingle2vec()`
intermingle2vec. Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.

- #### 149 `intermingle.cbind()`
intermingle.cbind. Combine 2 data frames (of the same length) so that form every odd and every even  element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().

- #### 150 `ls2categvec()`
ls2categvec. Converts a list to a vector repeating list-element names, while vector names are the list elements.

- #### 151 `list.2.replicated.name.vec()`
list.2.replicated.name.vec. Converts a list to a vector, with list elements names replicated as many times as many elements each element had.

- #### 152 `symdiff()`
Symmetric difference. Quasi symmetric difference of any number of vectors.

- #### 153 `iround()`
iround. Rounds a value to the significant amount of digits. Its a wrapper for signif().

- #### 154 `modus()`
modus. Calculates the mode (modus) of a numeric vector (it excludes NA-s by default). https://en.wikipedia.org/wiki/Mode_(statistics)

- #### 155 `cv()`
cv. Calculates the coefficient of variation (CV) for a numeric vector (it excludes NA-s by default).

- #### 156 `sem()`
sem. Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default).

- #### 157 `fano()`
fano. Calculates the fano factor on a numeric vector (it excludes NA-s by default).

- #### 158 `geomean()`
geomean. Calculates the geometric mean of a numeric vector (it excludes NA-s by default).

- #### 159 `mean_of_log()`
mean_of_log. Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default).

- #### 160 `movingAve()`
Moving / rolling average. Calculates the moving / rolling average of a numeric vector.

- #### 161 `movingAve2()`
Moving / rolling average (v2, filter). Calculates the moving / rolling average of a numeric vector, using `filter()`.

- #### 162 `movingSEM()`
movingSEM. Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.

- #### 163 `imovingSEM()`
imovingSEM. Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller window-size.

- #### 164 `dput_pretty()`
Pretty Printing of R Objects (pretty_dput). This function modifies the output of the traditional dput() function  so that each key-value pair in a vector appears on a new line. It's useful for  creating more readable output.

- #### 165 `as.numeric.wNames.deprecated()`
as.numeric.wNames.deprecated. Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.

- #### 166 `as.factor.numeric()`
as.factor.numeric.deprecated. Turn any vector into numeric categories as.numeric(as.factor(vec))







*In 2021, function libraries got reorganized as below:*

<img width="1005" alt="R-package Tree" src="https://user-images.githubusercontent.com/5101911/143560128-065d8a49-0283-4a3a-9448-540fa424d0ef.png">




---

Vertesy, 2023.  ***Cite via:*** [![DOI](https://zenodo.org/badge/319134544.svg)](https://zenodo.org/badge/latestdoi/319134544)

<br>

