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

## List of Functions

- ### `vec.fromNames()`
Create a vector from a vector of names.

- ### `list.fromNames()`
create list from a vector with the names of the elements.

- ### `matrix.fromNames()`
Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.

- ### `matrix.fromVector()`
Create a matrix from values in a vector repeated for each column / each row. Similar to rowNameMatrix and colNameMatrix.

- ### `array.fromNames()`
Create an N-dimensional array from N vectors defining the row-, column, etc names of the array.

- ### `what()`
A better version of is(). It can print the first "printme" elements.

- ### `idim()`
A dim() function that can handle if you pass on a vector: then, it gives the length.

- ### `is.list2()`
The 'is.list()' function fails on tibbles: it returns TRUE, as if it were a list. This distiguishes. Thaat's why we need this function.

- ### `idimnames()`
A dimnames() function that can handle if you pass on a vector: it gives back the names.

- ### `printEveryN()`
Report iterator value at every e.g. 1000

- ### `table_fixed_categories()`
Generate a table() with a fixed set of categories. It fills up the table with missing categories, that are relevant when comparing to other vectors. 

- ### `getCategories()`
Extract unique entries with a corresponding name.

- ### `grepv()`
grep returning the value. A character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning. Missing values are allowed except for regexpr, gregexpr and regexec.

- ### `most_frequent_elements()`
Show the most frequent elements of a table.

- ### `count_occurrence_each_element()`
Count the number of times each element occurs in the full vector, AND give it back as a vector, that is the same length as the input vector, each element corresponding one-by-one.

- ### `top_indices()`
Returns the positions of the `n` highest values in `x`.

- ### `trail()`
Returns a vector containing the first `N` and last `N` elements of `vec`.

- ### `sort.decreasing()`
Sorts `vec` in decreasing order.

- ### `sstrsplit()`
Alias for `str_split_fixed` in the `stringr` package.

- ### `as.named.vector.df()`
Convert any column or row of a dataframe into a vector, keeping the corresponding dimension name.

- ### `as.named.vector.2colDF()`
Convert a 2-column dataframe (value, name) into a named vector. Use for simple tibbles.

- ### `col2named.vector()`
Convert a dataframe column into a vector, keeping the corresponding dimension name.

- ### `row2named.vector()`
Convert a dataframe row into a vector, keeping the corresponding dimension name.

- ### `tibble_summary_to_namedVec()`
Convert a key-value tibble into a named vector (as opposed to using rownames).

- ### `unique.wNames()`
Convert a vector with names into a tibble, keeping the names as rownames.

- ### `as.numeric.wNames.character()`
Converts (1) a 'character' v. into a numeric v., or  a 'factor' v. as as.numeric(as.character(vec)) and preserves the original names.  The old 'as.numeric.wNames()' is deprecated as it was not clearly documented that it converts via facotr in any case. Code saved at the end.

- ### `as.numeric.wNames.factor()`
Turn any vector into numeric categories as.numeric(as.factor(vec))  Forerly as.factor.numeric

- ### `as.character.wNames()`
Converts your input vector into a character vector, and puts the original character values into the names of the new vector, unless it already has names.

- ### `translate()`
Replaces a set of values in a vector with another set of values,  it translates your vector. Oldvalues and newvalues have to be 1-to-1  correspoding vectors.  'chartr("a-cX", "D-Fw", x) does the same as above  in theory, but it did not seem very robust regarding your input...'

- ### `rescale()`
Linear transformation to a given range of values.

- ### `fractions()`
x/sum(x)

- ### `flip_value2name()`
Flip the values and the names of a vector with names.

- ### `sortbyitsnames()`
Sort a vector or list by the alphanumeric order of its names (instead of its values).

- ### `any.duplicated()`
How many entries are duplicated?.

- ### `which.duplicated()`
Which values are duplicated?.

- ### `which.NA()`
Which values are NA?.

- ### `pad.na()`
This function fills up a vector to a given length by appending NA-values at the end.    If the input vector's length is less than the provided length, the function pads the vector    with NA. If the vector's length is already equal to or greater than the given length, no change    will be made.

- ### `clip.at.fixed.value()`
Signal clipping. Cut values above or below a threshold.

- ### `clip.outliers.at.percentile()`
Signal clipping based on the input data's distribution. It clips values above or below the extreme N% of the distribution.

- ### `as.logical.wNames()`
Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.

- ### `col2named.vec.tbl()`
Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.

- ### `topN.dfCol()`
Find the n highest values in a named vector.

- ### `bottomN.dfCol()`
Find the n lowest values in a named vector.

- ### `iterBy.over()`
Iterate over a vector by every N-th element.

- ### `numerate()`
Mix entries so that they differ.

- ### `MaxN()`
Find second (third…) highest/lowest value in vector.

- ### `cumsubtract()`
Cumulative subtraction, opposite of cumsum().

- ### `sumBySameName()`
Sum up vector elements with the same name.

- ### `which_names()`
Return the names where the input vector is TRUE. The input vector is converted to logical.

- ### `which_names_grep()`
Return the vector elements whose names partially match a pattern.

- ### `na.omit.strip()`
Calls na.omit() and returns a clean vector.  Omit NA values from a vector and return a clean vector without any spam.

- ### `inf.omit()`
Omit infinite values from a vector.

- ### `zero.omit()`
Omit zero values from a vector.

- ### `pc_TRUE()`
Calculates the percentage of true values in a logical vector, parsed as text.

- ### `pc_in_total_of_match()`
Calculates the percentage of a certain value within a vector or table.

- ### `remove_outliers()`
Remove values that fall outside the trailing `probs` percentiles of the distribution.

- ### `simplify_categories()`
Replace all occurrences of `replaceit` in `category_vec` with `to`.

- ### `colSubtract()`
Subtract a vector (length = nr. columns) column by column from each value of the matrix.

- ### `rowSubtract()`
Subtract a vector (length = nr. rows) row by row from each value of the matrix

- ### `colDivide()`
See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r.

- ### `colMutliply()`
See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r.

- ### `rowDivide()`
Divide by row.

- ### `rowMutliply()`
Mutliply by row.

- ### `row.Zscore()`
Calculate Z-score over rows of data frame.

- ### `rowMin()`
Calculates the minimum of each row of a numeric matrix / data frame.

- ### `colMin()`
Calculates the minimum of each column of a numeric matrix / data frame.

- ### `rowMax()`
Calculates the maximum of each row of a numeric matrix / data frame.

- ### `colMax()`
Calculates the maximum of each column of a numeric matrix / data frame.

- ### `rowMedians()`
Calculates the median of each row of a numeric matrix / data frame.

- ### `colMedians()`
Calculates the median of each column of a numeric matrix / data frame.

- ### `rowGeoMeans()`
Calculates the median of each row of a numeric matrix / data frame.

- ### `colGeoMeans()`
Calculates the median of each column of a numeric matrix / data frame.

- ### `rowCV()`
Calculates the CV of each ROW of a numeric matrix / data frame.

- ### `colCV()`
Calculates the CV of each column of a numeric matrix / data frame.

- ### `rowVariance()`
Calculates the CV of each ROW of a numeric matrix / data frame.

- ### `colVariance()`
Calculates the CV of each column of a numeric matrix / data frame.

- ### `rowSEM()`
Calculates the SEM of each row of a numeric matrix / data frame.

- ### `colSEM()`
Calculates the SEM of each column of a numeric matrix / data frame.

- ### `rowSD()`
Calculates the SEM of each row of a numeric matrix / data frame.

- ### `colSD()`
Calculates the SD of each column of a numeric matrix / data frame.

- ### `rowIQR()`
Calculates the IQR of each row of a numeric matrix / data frame.

- ### `colIQR()`
Calculates the IQR of each column of a numeric matrix / data frame.

- ### `rowQuantile()`
Calculates the quantile of each row of a numeric matrix / data frame.

- ### `colQuantile()`
Calculates the quantile of each column of a numeric matrix / data frame.

- ### `TPM_normalize()`
Normalize each column to 1 million.

- ### `median_normalize()`
Normalize each column to the median of all the column-sums.

- ### `mean_normalize()`
Normalize each column to the median of the columns.

- ### `rotate()`
Rotate a matrix 90 degrees.

- ### `sortEachColumn()`
Sort each column of a numeric matrix / data frame.

- ### `sort.mat()`
Sorts a matrix by a given column. This function can only handle single column sort.  An alternative is dd[with(dd, order(-z, b)), ] as found on StackOverflow (https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r).

- ### `rowNameMatrix()`
Create a copy of your matrix, where every entry is replaced by the corresponding row name. Useful if you want to color by row name in a plot (where you have different number of NA-values in each row).

- ### `colNameMatrix()`
Create a copy of your matrix, where every entry is replaced by the corresponding column name. Useful if you want to color by column name in a plot (where you have different number of NA-values in each column).

- ### `rownames.trimws()`
Trim whitespaces from the rownames.

- ### `colsplit()`
Split a data frame by a factor corresponding to columns.

- ### `rowsplit()`
Split a data frame by a factor corresponding to columns.

- ### `select_rows_and_columns()`
Subset rows and columns. It checks if the selected dimension names exist and reports if any of those they aren't found.

- ### `getRows()`
Returns a subset of rows based on their names and optionally removes rows with only NA or zero values. Reports the number of missing rows.

- ### `getCols()`
Returns a subset of columns based on their names and optionally removes columns with only NA or zero values. Reports the number of missing columns.

- ### `get.oddoreven()`
Returns either odd or even indexed rows or columns from a data frame.

- ### `merge_1col_dfs_by_rn()`
Merge any data frames by rownames. Required plyr package.

- ### `merge_numeric_df_by_rn()`
Merge 2 numeric data frames by rownames.

- ### `merge_2_named_vec_as_df()`
Merge two named vectors by names, into a dataframe with 2 columns.

- ### `merge_ls_of_named_vec_as_df_cols()`
Merge any number of named vectors (presented as a list) by names, into a dataframe

- ### `remove.na.rows()`
Cols have to be a vector of numbers corresponding to columns.

- ### `remove.na.cols()`
Cols have to be a vector of numbers corresponding to columns.

- ### `na.omit.mat()`
Omit rows with NA values from a matrix. Rows with any, or full of NA-s.

- ### `df.remove.empty.rows.and.columns()`
This function takes a data frame and a threshold value, and removes all rows and columns that contain only zeros or the threshold value. 

- ### `copy.dimension.and.dimnames()`
Copy the dimension and dimnames of a 1D vector to a 2D array.

- ### `mdlapply()`
A wrapper for `lapply()` that works on multidimensional arrays.

- ### `arr.of.lists.2.df()`
Simplify 2D-list-array to a DF.

- ### `mdlapply2df()`
Multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF).

- ### `any.duplicated.rownames.ls.of.df()`
Check if there are any duplocated rownames in a list of dataframes.

- ### `intersect.ls()`
Intersect any number of lists.

- ### `union.ls()`
Intersect any number of list elements. Faster than reduce.

- ### `symdiff.ls()`
Calculate symmetric difference on a list (of 2 vectors).

- ### `setdiff.ls()`
Calculate set difference on a list (of 2 vectors).

- ### `unlapply()`
Lapply, then unlist.

- ### `list.wNames()`
Create a list with names from ALL variables you pass on to the function.

- ### `as.list.df.by.row()`
Split a dataframe into a list by its columns.

- ### `as.list.df.by.col()`
Split a dataframe into a list by its rows.

- ### `reorder.list()`
Reorder elements of lists in your custom order of names / indices.

- ### `range.list()`
Calculates the range of values in a list.

- ### `intermingle2lists()`
Combine 2 lists (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().

- ### `as.listalike()`
Converts a vector to a list with the same dimensions as a given list.

- ### `reverse.list.hierarchy()`
This function reverses the hierarchy of a given, 2 level, nested list. The  function will ensure that all lists at the same level have the same names,  and then transpose the structure, creating a new list for each unique name.  Any missing elements in the original lists are not included in the final  output. The result is a list where the top-level names are derived from  the unique names found at the lower levels of the input list. 

- ### `list2fullDF.byNames()`
Converts a list to a full matrix, with rows and columns named by the elements of the list.

- ### `list2fullDF.presence()`
Converts a list to a full matrix, with rows and columns named by the elements of the list.  The matrix will contain a 1 in each cell where the corresponding element of the list is present, and a 0 otherwise.

- ### `splititsnames_byValues()`
Split a list by its names.

- ### `intermingle2vec()`
Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.

- ### `intermingle.cbind()`
Combine 2 data frames (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().

- ### `ls2categvec()`
Converts a list to a vector repeating list-element names, while vector names are the list elements.

- ### `symdiff()`
Converts a list to a vector, with list elements names replicated as many times as many elements each element had.

- ### `iround()`
Rounds a value to the significant amount of digits. Its a wrapper for signif().

- ### `modus()`
Calculates the mode (modus) of a numeric vector (it excludes NA-s by default). https://en.wikipedia.org/wiki/Mode_(statistics)

- ### `cv()`
Calculates the coefficient of variation (CV) for a numeric vector (it excludes NA-s by default).

- ### `sem()`
Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default).

- ### `fano()`
Calculates the fano factor on a numeric vector (it excludes NA-s by default).

- ### `geomean()`
Calculates the geometric mean of a numeric vector (it excludes NA-s by default).

- ### `mean_of_log()`
Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default).

- ### `movingAve2()`
Calculates the moving / rolling average of a numeric vector.

- ### `movingSEM()`
Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.

- ### `imovingSEM()`
Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller window-size.

- ### `pretty_dput()`
This function modifies the output of the traditional dput() function  so that each key-value pair in a vector appears on a new line. It's useful for  creating more readable output.

- ### `as.numeric.wNames.deprecated()`
Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.

- ### `as.factor.numeric.deprecated()`
Turn any vector into numeric categories as.numeric(as.factor(vec))



---

Vertesy, 2021.  ***Cite via:*** [![DOI](https://zenodo.org/badge/319134544.svg)](https://zenodo.org/badge/latestdoi/319134544)

<br>

