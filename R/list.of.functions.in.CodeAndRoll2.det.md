## List of Functions in CodeAndRoll2.R (173) 
Updated: 2026/08/25 16:15
- #### 1 `getScriptName()`
Get Current Script Name or Basename of Output Directory. This function attempts to retrieve the name of the currently opened script in  the RStudio editor. If the script name cannot be obtained or if the `rstudioapi` package is  not available, it returns the basename of the directory specified by `OutDir`. 

- #### 2 `getProject()`
getProject. Try to get the project name you are working on in RStudio.  @return The final subfolder of your project, or `NULL` if you are not running one  @importFrom rstudioapi getActiveProject  @examples getProject() 

- #### 3 `savehistory_2rstudio()`

  Export RStudio Command History with Timestamps.   Exports the full RStudio console command history from RStudio's internal  history database to a human-readable text file. Each command is annotated  with a timestamp (year-month-day hour:minute). Multi-line commands are  preserved and grouped correctly. This does not rely on `.Rhistory` or  `savehistory()`, instead directly reading RStudio's history database.   The output file is named  `command_history.YYYY.MM.DD[.scriptname].txt` and is written to the current  working directory.   @return  Invisibly returns the path to the written history file.   @details  The history source is RStudio’s internal history log  (`~/.local/share/rstudio/history_database` on Linux). The log stores commands  as timestamped lines using milliseconds since the Unix epoch. These  timestamps are converted to local time during export.   This function is read-only and safe to run while RStudio is open.   @importFrom rstudioapi getSourceEditorContext   @examples  \dontrun{  savehistory_2rstudio()  }

- #### 4 `savehistory_Rhist()`

  Save Command History to "command_history.date.scriptname.txt".   This function saves the command history of the current R session to a text file. The file name  includes the current date and, if available, the name of the current R script (when running in  RStudio). The file is saved in the current working directory.   @return Nothing is returned, but the file path is printed to the console.   @importFrom rstudioapi getSourceEditorContext  @examples  \dontrun{  savehistory_2()  } 

- #### 5 `pFilter()`
Vector Filtering Helper for piping.   A universal vector filtering function that applies an inline logical condition  to a vector, similar to `dplyr::filter()`. The vector is passed as `.`, so  users can write conditions like `. > 5`, `. %in% c("a","b")`, or `. == 1`. 

- #### 6 `pSee()`
Print and Return an Object in a Pipe. Prints the input object and returns it, enabling you to inspect values inside a    pipe operation. It detects complex objects like ggplot, plotly, or Seurat to ensure    they are displayed correctly instead of showing their internal list structure.    For non-complex objects, it dynamically reports dimensions (if > 1D) or length (if 1D). 

- #### 7 `pLength()`
Print Length and Return an Object in a Pipe. Prints the length of the input object and returns it, allowing you to verify  length inside a pipe operation. 

- #### 8 `pU()`
Print the (Number of) Unique Elements and Return an Object in a Pipe. Prints the unique elements of the input object and returns it, enabling you to inspect  unique values inside a pipe.

- #### 9 `pAny()`
Apply Any Function and Return the Original Object in a Pipe.   Evaluates a specified function on the input object, prints the result to the console,  and returns the original input object unmodified. This is highly useful for inspecting  intermediate transformations (e.g., `rowMeans`, `dim`, `summary`) without breaking  the pipeline chain. 

- #### 10 `setIfNotDefined()`
Set object if not defined.   Checks whether a symbol exists in the calling environment. If it does not,  the expression provided as the second argument is evaluated, assigned to  that symbol, and returned to the global env. If it already exists, the existing object is  returned unchanged. 

- #### 11 `vec.fromNames()`
vec.fromNames. Create a vector from a vector of names.

- #### 12 `list.fromNames()`
Create a Named List from a Character Vector or the Names of an Object. This function takes a character vector of names and creates a list where each  element is named according to that vector and filled with a specified value.

- #### 13 `vec.from.template()`
vec.from.template. Create a vector using the names of another vector, list, or similar object.

- #### 14 `list.from.template()`
list.from.template. Create an empty list from a template list, copying names and filling values with NA.

- #### 15 `matrix.fromNames()`
matrix.fromNames. Create a matrix from 2 vectors defining the row- and column names of the matrix. Default fill value: NA.

- #### 16 `data.frame.fromNames()`
data.frame.fromNames. Create a data frame from 2 vectors defining the row- and column names of the  data frame Default fill value: NA.

- #### 17 `matrix.fromVector()`
matrix.fromVector. Create a matrix from values in a vector repeated for each column / each row.  Similar to rowNameMatrix and colNameMatrix.

- #### 18 `array.fromNames()`
array.fromNames. Create an N-dimensional array from N vectors defining the row-, column, etc names of the array.

- #### 19 `what()`
what. A better version of is(). It can print the first "printme" elements.

- #### 20 `is.list.simple()`
Test if object is a list. The 'is.list()' function "fails: on tibbles and data frames: it returns TRUE,  as if it were a list. This distinguishes and identifies simple lists. That's why we need this function.

- #### 21 `idim()`
idim. A dim() function that can handle if you pass on a vector: then, it gives the length.

- #### 22 `idimnames()`
idimnames. A dimnames() function that can handle if you pass on a vector: it gives back the names.

- #### 23 `printEveryN()`
printEveryN. Report iterator value at every e.g. 1000

- #### 24 `printProgress()`
Print Loop Progress. Prints the progress of a loop as a number and percentage. 

- #### 25 `table_fixed_categories()`
table_fixed_categories. Generate a table() with a fixed set of categories. It fills up the table with  missing categories, that are relevant when comparing to other vectors. 

- #### 26 `table_decreasing()`
Frequency Table with Sorting Option.   This function generates a frequency table of the input vector `vec` and allows the option  to sort the table in decreasing or increasing order. It handles NA values. 

- #### 27 `table_decreasing_hybrid()`
Frequency Table with Hyrid Sorting: you can sort by frequency and by specified value.   This function generates a frequency table of the input vector `vec` and displays the table  sorted by frequency and by a set of specified values. It handles NA values. 

- #### 28 `getCategories()`
getCategories. Extract unique entries with a corresponding name.

- #### 29 `nr.unique()`
Count the number of unique values. Count the number of unique values

- #### 30 `grepv()`
grep that returns the value of the match.. grep returning the value. A character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning. Missing values are allowed except for regexpr, gregexpr and regexec.

- #### 31 `most_frequent_elements()`
most_frequent_elements. Show the most frequent elements of a table.

- #### 32 `count_occurrence_each_element()`
count_occurrence_each_element. Count the number of times each element occurs in the full vector, AND give it back as a vector, that is the same length as the input vector, each element corresponding one-by-one.

- #### 33 `top_indices()`
top_indices. Returns the positions of the `n` highest values in `x`. For equal values, it maintains the original order.

- #### 34 `trail()`
trail. A combination of `head()` and `tail()`: Returns a vector containing the first  `N` and last `N` elements of vector. 

- #### 35 `sort.decreasing()`
sort.decreasing. Sorts `vec` in decreasing order.

- #### 36 `as.named.vector.table()`
as.named.vector.table. Convert a 1D table into a named vector.

- #### 37 `vtable()`
vtable. A version of table() that returns a named vector instead of a table object.  It can handle vectors and factors, and has the same useNA argument as table().

- #### 38 `as.named.vector.2colDF()`
as.named.vector.2colDF. Convert a 2-column dataframe (value, name) into a named vector. Use for simple tibbles.

- #### 39 `df.col.2.named.vector()`
df.col.2.named.vector. Convert a dataframe column into a vector, keeping the corresponding dimension name.

- #### 40 `df.row.2.named.vector()`
df.row.2.named.vector. Convert a dataframe row into a vector, keeping the corresponding dimension name.

- #### 41 `tibble_summary_to_namedVec()`
tibble_summary_to_namedVec. Convert a key-value tibble into a named vector (as opposed to using rownames).

- #### 42 `as_tibble_from_namedVec()`
as_tibble_from_namedVec. Convert a vector with names into a tibble, keeping the names as rownames.

- #### 43 `unique.wNames()`
Unique elements with names preserved. Get the unique elements of a vector, keep their names

- #### 44 `range.wNames()`
Range elements with names preserved. Get the range of a numeric vector, keep the names of the min and max values

- #### 45 `as.numeric.wNames.character()`
as.numeric.wNames.character. Converts (1) a 'character' v. into a numeric v., or  a 'factor' v. as as.numeric(as.character(vec)) and preserves the original names.  The old 'as.numeric.wNames()' is deprecated as it was not clearly documented that it converts via  factor in any case. Code saved at the end.

- #### 46 `as.numeric.wNames.factor()`
as.numeric.wNames.factor. Turn any vector into numeric categories as.numeric(as.factor(vec))  Forerly as.factor.numeric

- #### 47 `as.character.wNames()`
as.character.wNames. Converts your input vector into a character vector, and puts the original  character values into the names of the new vector, unless it already has names.

- #### 48 `translate()`
Translate a set of values to a new set using a dictionary. Replaces a set of values in a vector with another set of values, so  it translates your vector. Oldvalues and newvalues have to be 1-to-1  corresponding vectors.  'chartr("a-cX", "D-Fw", x) does the same as above  in theory, but it did not seem very robust regarding your input...' 

- #### 49 `rescale()`
rescale. Linear transformation to a given range of values.

- #### 50 `fractions()`
fractions. x/sum(x)

- #### 51 `flip_value2name()`
flip_value2name. Flip the values and the names of a vector with names.

- #### 52 `sortbyitsnames()`
sortbyitsnames. Sort a vector or list by the alphanumeric order of its names (instead of its values).

- #### 53 `any.duplicated()`
any.duplicated. How many entries are duplicated?.

- #### 54 `which.duplicated()`
which.duplicated. Which values are duplicated?.

- #### 55 `which.NA()`
which.NA. Which values are NA?.

- #### 56 `clip.at.fixed.value()`
clip.at.fixed.value. Signal clipping. Cut values in a distribution, above or below a threshold.

- #### 57 `clip.outliers.at.percentile()`
clip.outliers.at.percentile. Signal clipping based on the input data's distribution. It clips values  in a distribution above or below the extreme N% of the distribution. 

- #### 58 `as.logical.wNames()`
as.logical.wNames. Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it already has names.

- #### 59 `col2named.vec.tbl()`
col2named.vec.tbl. Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.

- #### 60 `split_vec_to_list_by_N()`
Split a Vector into a List by Every N-th Element. This function divides a given vector into chunks of size `by` (default is 9).  The resulting list contains vectors of the specified chunk size or smaller.

- #### 61 `zigzagger()`
zigzagger. Mix entries so that they differ.

- #### 62 `numerate()`
numerate. Formats a Sequence of Numbers with Zero Padding.  This function generates a sequence of numbers between two specified values,  optionally padding them with leading zeros to a specified length. It is useful  for creating numeric sequences with consistent character lengths.

- #### 63 `MaxN()`
MaxN. Find second (third…) highest/lowest value in vector.  Source: "https://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column"

- #### 64 `cumsubtract()`
cumsubtract. Cumulative subtraction, opposite of cumsum().

- #### 65 `sumBySameName()`
sumBySameName. Sum up vector elements with the same name.

- #### 66 `checkMinOverlap()`
Check Minimum Overlap Between Two Vectors. Checks if the overlap between two character vectors is at least a specified  percentage of the shorter vector. Stops execution with an error if the condition is not met. 

- #### 67 `which_names()`
which_names. Return the names where the input vector is TRUE. The input vector is converted to logical.

- #### 68 `which_names_grep()`
which_names_grep. Return the vector elements whose names partially match a pattern.

- #### 69 `na.omit.strip()`
na.omit.strip. Calls na.omit() and returns a clean vector.  Omit NA values from a vector and return a clean vector without any spam.

- #### 70 `inf.omit()`
inf.omit. Omit infinite values from a vector.

- #### 71 `zero.omit()`
zero.omit. Omit zero values from a vector.

- #### 72 `pc_TRUE()`
pc_TRUE. Calculates the percentage of true values in a logical vector, parsed as text.

- #### 73 `pc_overlap()`
Calculate Percentage Overlap Between Two Vectors. Computes the percentage of overlap between two vectors based on the specified basis of calculation. 

- #### 74 `pc_in_total_of_match()`
pc_in_total_of_match. Calculates the percentage of a certain value within a vector or table.

- #### 75 `remove_outliers()`
remove_outliers. Remove values that fall outside the trailing `probs` percentiles of the distribution.

- #### 76 `simplify_categories()`
simplify_categories. Replace all occurrences of `replaceit` in `category_vec` with `to`.

- #### 77 `apply2()`
Apply Function Without Transposition Quirk (Row-Wise Apply Fix).   A drop-in replacement for `apply()` that automatically corrects the common transposition quirk  when applying functions row-wise (`MARGIN = 1`) to a 2D matrix. For `MARGIN = 1`, the result  is transposed back to match the expected orientation. For higher-dimensional arrays or  `MARGIN != 1`, it behaves identically to `apply()`. 

- #### 78 `colSubtract()`
colSubtract. Subtract a vector (length = nr. columns) column by column from each value of the matrix.

- #### 79 `rowSubtract()`
rowSubtract. Subtract a vector (length = nr. rows) row by row from each value of the matrix

- #### 80 `colDivide()`
Row-wise division of a matrix by a column vector. Each element of the matrix is divided by the corresponding element of the vector  that matches the column of the matrix element. This is typically used to normalize data,  for example, to scale values in each row by factors such as totals or means. Source:  \url{https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r}. 

- #### 81 `colMultiply()`
Column-wise multiplication of a matrix by a vector. Multiply each column of a matrix by the corresponding element of a vector. See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r.

- #### 82 `rowDivide()`
rowDivide. Divide by row.

- #### 83 `rowMultiply()`
Row-wise multiplication of a matrix by a vector. Multiply each row of a matrix by the corresponding element of a vector.

- #### 84 `row.Zscore()`
row.Zscore. Calculate Z-score over rows of data frame.

- #### 85 `TPM_normalize()`
TPM_normalize. Normalize each column to 1 million.

- #### 86 `median_normalize()`
median_normalize. Normalize each column to the median of all the column-sums.

- #### 87 `mean_normalize()`
mean_normalize. Normalize each column to the median of the columns.

- #### 88 `rowMin()`
rowMin. Calculates the minimum of each row of a numeric matrix / data frame.

- #### 89 `colMin()`
colMin. Calculates the minimum of each column of a numeric matrix / data frame.

- #### 90 `rowMax()`
rowMax. Calculates the maximum of each row of a numeric matrix / data frame.

- #### 91 `colMax()`
colMax. Calculates the maximum of each column of a numeric matrix / data frame.

- #### 92 `rowMedians()`
rowMedians. Calculates the median of each row of a numeric matrix / data frame.

- #### 93 `colMedians()`
colMedians. Calculates the median of each column of a numeric matrix / data frame.

- #### 94 `rowGeoMeans()`
rowGeoMeans. Calculates the median of each row of a numeric matrix / data frame.

- #### 95 `colGeoMeans()`
colGeoMeans. Calculates the median of each column of a numeric matrix / data frame.

- #### 96 `rowCV()`
rowCV. Calculates the CV of each ROW of a numeric matrix / data frame.

- #### 97 `colCV()`
colCV. Calculates the CV of each column of a numeric matrix / data frame.

- #### 98 `rowVariance()`
rowVariance. Calculates the CV of each ROW of a numeric matrix / data frame.

- #### 99 `colVariance()`
colVariance. Calculates the CV of each column of a numeric matrix / data frame.

- #### 100 `rowSEM()`
rowSEM. Calculates the SEM of each row of a numeric matrix / data frame.

- #### 101 `colSEM()`
colSEM. Calculates the SEM of each column of a numeric matrix / data frame.

- #### 102 `rowSD()`
rowSD. Calculates the SEM of each row of a numeric matrix / data frame.

- #### 103 `colSD()`
colSD. Calculates the SD of each column of a numeric matrix / data frame.

- #### 104 `rowIQR()`
rowIQR. Calculates the IQR of each row of a numeric matrix / data frame.

- #### 105 `colIQR()`
colIQR. Calculates the IQR of each column of a numeric matrix / data frame.

- #### 106 `rowQuantile()`
rowQuantile. Calculates the quantile of each row of a numeric matrix / data frame.

- #### 107 `colQuantile()`
colQuantile. Calculates the quantile of each column of a numeric matrix / data frame.

- #### 108 `cbind_vectors_by_names()`
Bind two named vectors by matching names. Combines two named vectors into a data frame by matching their names.  Missing values are filled with NA.

- #### 109 `sortEachColumn()`
sortEachColumn. Sort each column of a numeric matrix / data frame.

- #### 110 `sort_matrix_rows()`
Sort matrix or data frame by a column or row names.   Sorts a numeric matrix or data frame by a specified column or by row names. The function can only  handle sorting by a single column. It offers options to sort in increasing or decreasing order,  and to control the placement of `NA` values. 

- #### 111 `rownames.trimws()`
rownames.trimws. Trim whitespaces from the rownames.

- #### 112 `combine.matrices.by.rowname.intersect()`
combine.matrices.by.rowname.intersect. Combine two matrices by the intersection of their row names.

- #### 113 `colsplit()`
colsplit. Split a data frame by a factor corresponding to columns.

- #### 114 `rowsplit()`
rowsplit. Split a data frame by a factor corresponding to columns.

- #### 115 `get_max_colname_per_row()`
Get the Column Name corresponding to the Maximum Value in each Row (handles ambiguous matches).   This function takes a numeric matrix as input and returns a named vector where each element  corresponds to a row of the matrix. The names of the vector are the row names of the matrix,  and the values are the column names where the maximum value of each row is found. If there are  multiple columns with the maximum value in a row, the value for that row will be set to  `multi_max_str`. If `na.remove` is set to `TRUE`, NA values will be removed before finding the  maximum value. 

- #### 116 `select_rows_and_columns()`
select_rows_and_columns. Subset rows and columns. It checks if the selected dimension names exist and reports if any of those they aren't found.

- #### 117 `getRows()`
getRows. Returns a subset of rows based on their names and optionally removes rows with only NA or zero values. Reports the number of missing rows.

- #### 118 `getCols()`
getCols. Returns a subset of columns based on their names and optionally removes columns with only NA or zero values. Reports the number of missing columns.

- #### 119 `get.oddoreven()`
get.oddoreven. Returns either odd or even indexed rows or columns from a data frame.

- #### 120 `merge_dfs_by_rn()`
merge_dfs_by_rn. Merge any data frames by rownames. Required plyr package.

- #### 121 `merge_1col_dfs_by_rn()`
merge_1col_dfs_by_rn. A function that merges single-column data frames based on rownames.

- #### 122 `merge_numeric_df_by_rn()`
merge_numeric_df_by_rn. Merge 2 numeric data frames by rownames.

- #### 123 `merge_2_named_vec_as_df()`
merge_2_named_vec_as_df. Merge two named vectors by names, into a dataframe with 2 columns.

- #### 124 `merge_ls_of_named_vec_as_df_cols()`
merge_ls_of_named_vec_as_df_cols. Merge any number of named vectors (presented as a list) by names, into a dataframe

- #### 125 `get_col_types()`
Extract and Display Column Types of a Data Frame or Tibble. This function returns the primary class/type of each column in a data frame or tibble.  Additionally, it can print a summary of the column types. 

- #### 126 `fix_tibble_lists()`
Convert List Columns of a Tibble to String Vectors. Converts columns of type `list` in a tibble or data frame to string vectors.  It combines list elements into a single string per cell, using a specified separator.

- #### 127 `rotate_matrix()`
Rotate a Matrix by 90 Degrees. Rotates a given numeric matrix 90 degrees in a specified direction. The rotation  can be either clockwise or counterclockwise, determined by the `clockwise` parameter. 

- #### 128 `na.omit.mat()`
Omit Rows with NA Values from a Matrix. Removes rows from a matrix based on the presence of NA values. Can remove rows with any NA values or only those completely filled with NAs. 

- #### 129 `remove.na.rows()`
remove.na.rows. Cols have to be a vector of numbers corresponding to columns.

- #### 130 `remove.na.cols()`
remove.na.cols. Cols have to be a vector of numbers corresponding to columns.

- #### 131 `df.remove.empty.rows.and.columns()`
Remove empty rows and columns from a data frame.. This function takes a data frame and a threshold value, and removes all rows and columns that contain only zeros or the threshold value. 

- #### 132 `rowNameMatrix()`
rowNameMatrix. Create a copy of your matrix, where every entry is replaced by the corresponding  row name. Useful if you want to color by row name in a plot (where you have different number of   NA-values in each row).

- #### 133 `colNameMatrix()`
colNameMatrix. Create a copy of your matrix, where every entry is replaced by the corresponding  column name. Useful if you want to color by column name in a plot (where you have different  number of NA-values in each column).

- #### 134 `copy.dimension.and.dimnames()`
copy.dimension.and.dimnames. Copy the dimension and dimnames of a 1D vector to a 2D array.

- #### 135 `mdlapply()`
mdlapply. A wrapper for `lapply()` that works on multidimensional arrays.

- #### 136 `arr.of.lists.2.df()`
arr.of.lists.2.df. Simplify 2D-list-array to a DF.

- #### 137 `mdlapply2df()`
mdlapply2df. Multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF).

- #### 138 `any.duplicated.rownames.ls.of.df()`
any.duplicated.rownames.ls.of.df. Check if there are any duplocated rownames in a list of dataframes.

- #### 139 `intersect.ls()`
intersect.ls. Intersect any number of lists.

- #### 140 `union.ls()`
union.ls. Intersect any number of list elements. Faster than reduce.

- #### 141 `symdiff.ls()`
symdiff.ls. Calculate symmetric difference on a list (of 2 vectors).

- #### 142 `setdiff.ls()`
setdiff.ls. Calculate set difference on a list (of 2 vectors).

- #### 143 `unlapply()`
Like sapply, but with names preserved. Help in some other cases too.. Do an `lapply()`, then `unlist()`, with preserving the list element names.

- #### 144 `list.wNames()`
list.wNames. Create a list with names from ALL variables you pass on to the function.

- #### 145 `as.list.df.by.row()`
as.list.df.by.row. Split a dataframe into a list by its columns.

- #### 146 `as.list.df.by.col()`
as.list.df.by.col. Split a dataframe into a list by its rows.

- #### 147 `reorder.list()`
reorder.list. Reorder elements of lists in your custom order of names / indices.

- #### 148 `range.list()`
range.list. Calculates the range of values in the entire a list.

- #### 149 `intermingle2lists()`
intermingle2lists. Combine 2 lists (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().

- #### 150 `as.listalike()`
as.listalike. Converts a vector to a list with the same dimensions as a given list.

- #### 151 `reverse.list.hierarchy()`
Reverse the Hierarchy of a List. This function reverses the hierarchy of a given, 2 level, nested list. The  function will ensure that all lists at the same level have the same names,  and then transpose the structure, creating a new list for each unique name.  Any missing elements in the original lists are not included in the final  output. The result is a list where the top-level names are derived from  the unique names found at the lower levels of the input list. 

- #### 152 `list2fullDF.byNames()`
list2fullDF.byNames. Converts a list to a full matrix, with rows and columns named by the elements of the list.

- #### 153 `list2fullDF.presence()`
list2fullDF.presence. Converts a list to a full matrix, with rows and columns named by the elements of the list.  The matrix will contain a 1 in each cell where the corresponding element of the list is present, and a 0 otherwise.

- #### 154 `splitbyitsnames()`
splitbyitsnames. Split a list by its names.

- #### 155 `splititsnames_byValues()`
Split the names of list by its values.. Split the names of a list by its values.

- #### 156 `intermingle2vec()`
intermingle2vec. Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.

- #### 157 `intermingle.cbind()`
intermingle.cbind. Combine 2 data frames (of the same length) so that form every odd and every even  element of a unified list. Useful for side-by-side comparisons, e.g. in wstripchart_list().

- #### 158 `ls2categvec()`
ls2categvec. Converts a list to a vector repeating list-element names, while vector names are the list elements.

- #### 159 `list.2.replicated.name.vec()`
list.2.replicated.name.vec. Converts a list to a vector, with list elements names replicated as many times as many elements each element had.

- #### 160 `symdiff()`
Symmetric difference. Quasi symmetric difference of any number of vectors.

- #### 161 `intersect.wNames()`
Intersect with Name Preservation. Intersects two character vectors while preserving names from the specified vector. 

- #### 162 `union.wNames()`
Union with Name Preservation. Unites two character vectors while preserving names from the specified vector.    Gives a warning if there are conflicts in names between `x` and `y`. 

- #### 163 `iround()`
iround. Rounds a value to the significant amount of digits. Its a wrapper for signif().

- #### 164 `modus()`
modus. Calculates the mode (modus) of a numeric vector (it excludes NA-s by default). https://en.wikipedia.org/wiki/Mode_(statistics)

- #### 165 `cv()`
cv. Calculates the coefficient of variation (CV) for a numeric vector (it excludes NA-s by default).

- #### 166 `sem()`
sem. Calculates the standard error of the mean (SEM) for a numeric vector (it excludes NA-s by default).

- #### 167 `fano()`
fano. Calculates the fano factor on a numeric vector (it excludes NA-s by default).

- #### 168 `geomean()`
geomean. Calculates the geometric mean of a numeric vector (it excludes NA-s by default).

- #### 169 `mean_of_log()`
mean_of_log. Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default).

- #### 170 `movingAve()`
Moving / rolling average. Calculates the moving / rolling average of a numeric vector.

- #### 171 `movingAve2()`
Moving / rolling average (v2, filter). Calculates the moving / rolling average of a numeric vector, using `filter()`.

- #### 172 `movingSEM()`
movingSEM. Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.

- #### 173 `imovingSEM()`
imovingSEM. Calculates the moving / rolling standard error of the mean (SEM). It computes values up to the edges of the vector using incrementally smaller window sizes.

