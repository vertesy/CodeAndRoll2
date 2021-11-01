# CodeAndRoll2

#### \*\*\*\*Package in development\*\*\*\*

Packaged version of the core functoionalities (vector, matrix and list manipulations; math) of the formerly used [CodeAndRoll (v1)](https://github.com/vertesy/CodeAndRoll). 
A standalone set of more than >130 productivity functions. 
Used by [MarkdownReports](https://github.com/vertesy/MarkdownReports), [ggExpress](https://github.com/vertesy/ggExpress), [SeuratUtils](https://github.com/vertesy/SeuratUtils).

<img width="723" alt="Package Reorganisation Diagram" src="https://user-images.githubusercontent.com/5101911/139642446-cde94051-ca6b-4ee6-84c1-3000cab51b78.png">

#### 

<br><br>

## Installation

Install directly from **GitHub** via **devtools** with one R command:

    # install.packages("devtools"); # If you don't have it.
    require("devtools")
    devtools::install_github(repo = "vertesy/CodeAndRoll2")

...then simply load the package:

    require("CodeAndRoll2")

Alternatively, you simply source it from the web. 
*This way function help will not work, and you will have no local copy of the code on your hard drive.*

    source("https://raw.githubusercontent.com/vertesy/CodeAndRoll2/master/CodeAndRoll2/R/CodeAndRoll2.R")

<br><br>


## List of Functions

- #### vec.fromNames 
create a vector from a vector of names

- #### list.fromNames 
create list from a vector with the names of the elements

- #### matrix.fromNames 
Create a matrix from 2 vectors defining the row- and column names of the matrix. Default 

- #### matrix.fromVector 
Create a matrix from values in a vector repeated for each column / each row. Similar to 

- #### array.fromNames 
create an N-dimensional array from N vectors defining the row-, 

- #### what 
A better version of is(). It can print the first "printme" elements.

- #### idim 
A dim() function that can handle if you pass on a vector: then, it gives the length.

- #### idimnames 
A dimnames() function that can handle if you pass on a vector: it gives back the names.

- #### table_fixed_categories 
generate a table() with a fixed set of categories. It fills up the table with missing categories, that are relevant 

- #### grepv 
grep returning the value

- #### most_frequent_elements 
Show the most frequent elements of a table

- #### top_indices 
Returns the position / index of the n highest values. For equal values, it maintains the original order

- #### trail 
A combination of head() and tail() to see both ends.

- #### sort.decreasing 
Sort in decreasing order.

- #### sstrsplit 
Alias for str_split_fixed in the stringr package

- #### topN.dfCol 
Find the n highest values in a named vector

- #### bottomN.dfCol 
Find the n lowest values in a named vector

- #### as.named.vector 
Convert a dataframe column or row into a vector, keeping the corresponding dimension name.

- #### col2named.vector 
Convert a dataframe column into a vector, keeping the corresponding dimension name.

- #### row2named.vector 
Convert a dataframe row into a vector, keeping the corresponding dimension name.

- #### tibble_summary_to_named_vec 
...

- #### as_tibble_from_named_vec 
Convert a vector with names into a tibble, keeping the names as rownames.

- #### as.numeric.wNames 
Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has 

- #### as.numeric.wNames.old 
Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has 

- #### as.character.wNames 
Converts your input vector into a character vector, and puts the original character values into the names of the new vector, unless it 

- #### rescale 
linear transformation to a given range of values

- #### flip_value2name 
Flip the values and the names of a vector with names

- #### sortbyitsnames 
Sort a vector by the alphanumeric order of its names(instead of its values).

- #### any.duplicated 
How many entries are duplicated

- #### which.duplicated 
which values are duplicated?

- #### which.NA 
which values are NA?

- #### pad.na 
Fill up with a vector to a given length with NA-values at the end.

- #### clip.values 
Signal clipping. Cut values above or below a threshold.

- #### clip.outliers 
Signal clipping based on the input data's distribution. It clips values 

- #### as.logical.wNames 
Converts your input vector into a logical vector, and puts the original character values into the names of the new vector, unless it 

- #### col2named.vec.tbl 
Convert a 2-column table(data frame) into a named vector. 1st column will be used as names.

- #### iterBy.over 
Iterate over a vector by every N-th element.

- #### zigzagger 
mix entries so that they differ

- #### numerate 
numerate from x to y with additonal zeropadding

- #### MaxN 
find second (third…) highest/lowest value in vector

- #### cumsubtract 
Cumulative subtraction, opposite of cumsum()

- #### sumBySameName 
Sum up vector elements with the same name

- #### which_names 
Return the names where the input vector is TRUE. The input vector is converted to logical.

- #### which_names_grep 
Return the vector elements whose names are partially matched

- #### na.omit.strip 
Calls na.omit() and returns a clean vector

- #### inf.omit 
Omit infinite values from a vector.

- #### zero.omit 
Omit zero values from a vector.

- #### pc_TRUE 
Percentage of true values in a logical vector, parsed as 

- #### pc_in_total_of_match 
Percentage of a certain value within a vector or table.

- #### filter_survival_length 
Parse a sentence reporting the % of filter survival.

- #### remove_outliers 
Remove values that fall outside the trailing N % of the distribution.

- #### simplify_categories 
Replace every entry that is found in "replaceit", by a single value provided by "to"

- #### lookup 
Awesome pattern matching for a set of values in another set of values. Returns a list with all kinds of 

- #### rowMedians 
Calculates the median of each row of a numeric matrix / data frame.

- #### colMedians 
Calculates the median of each column of a numeric matrix / data frame.

- #### rowGeoMeans 
Calculates the median of each row of a numeric matrix / data frame.

- #### colGeoMeans 
Calculates the median of each column of a numeric matrix / data frame.

- #### rowCV 
Calculates the CV of each ROW of a numeric matrix / data frame.

- #### colCV 
Calculates the CV of each column of a numeric matrix / data frame.

- #### rowVariance 
Calculates the CV of each ROW of a numeric matrix / data frame.

- #### colVariance 
Calculates the CV of each column of a numeric matrix / data frame.

- #### rowMin 
Calculates the minimum of each row of a numeric matrix / data frame.

- #### colMin 
Calculates the minimum of each column of a numeric matrix / data frame.

- #### rowMax 
Calculates the maximum of each row of a numeric matrix / data frame.

- #### colMax 
Calculates the maximum of each column of a numeric matrix / data frame.

- #### rowSEM 
Calculates the SEM of each row of a numeric matrix / data frame.

- #### colSEM 
Calculates the SEM of each column of a numeric matrix / data frame.

- #### rowSD 
Calculates the SEM of each row of a numeric matrix / data frame.

- #### colSD 
Calculates the SEM of each column of a numeric matrix / data frame.

- #### rowIQR 
Calculates the SEM of each row of a numeric matrix / data frame.

- #### colIQR 
Calculates the SEM of each column of a numeric matrix / data frame.

- #### rowquantile 
Calculates the SEM of each row of a numeric matrix / data frame.

- #### colquantile 
Calculates the SEM of each column of a numeric matrix / data frame.

- #### colDivide 
See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r

- #### colMutliply 
See more: https://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r

- #### rowDivide 
divide by row

- #### rowMutliply 
Mutliply by row

- #### row.Zscore 
Calculate Z-score over rows of data frame.

- #### TPM_normalize 
normalize each column to 1 million

- #### median_normalize 
normalize each column to the median of all the column-sums

- #### mean_normalize 
normalize each column to the median of the columns

- #### rotate 
rotate a matrix 90 degrees.

- #### sortEachColumn 
Sort each column of a numeric matrix / data frame.

- #### sort.mat 
Sort a matrix. ALTERNATIVE: dd[with(dd, order(-z, b)), ]. Source: https://stackoverflow.com/

- #### rowNameMatrix 
Create a copy of your matrix, where every entry is replaced by the corresponding row name. Useful if you want to color by row name in a 

- #### colNameMatrix 
Create a copy of your matrix, where every entry is replaced by the corresponding column name. Useful if you want to color by column 

- #### rownames.trimws 
trim whitespaces from the rownames

- #### colsplit 
split a data frame by a factor corresponding to columns.

- #### rowsplit 
split a data frame by a factor corresponding to columns.

- #### select.rows.and.columns 
Subset rows and columns. It checks if the selected dimension names exist and reports if any of those they 

- #### getRows 
Get the subset of rows with existing rownames, report how much it could not find.

- #### getCols 
Get the subset of cols with existing colnames, report how much it could not find.

- #### get.oddoreven 
Get odd or even columns or rows of a data frame

- #### combine.matrices.intersect 
combine matrices by rownames intersect

- #### merge_dfs_by_rn 
Merge any data frames by rownames. Required plyr package

- #### merge_numeric_df_by_rn 
Merge 2 numeric data frames by rownames

- #### remove.na.rows 
cols have to be a vector of numbers corresponding to columns

- #### remove.na.cols 
cols have to be a vector of numbers corresponding to columns

- #### na.omit.mat 
Omit rows with NA values from a matrix. Rows with any, or full of NA-s

- #### copy.dimension.and.dimnames 
copy dimension and dimnames

- #### mdlapply 
 lapply for multidimensional arrays

- #### arr.of.lists.2.df 
simplify 2D-list-array to a DF

- #### mdlapply2df 
multi dimensional lapply + arr.of.lists.2.df (simplify 2D-list-array to a DF)

- #### any.duplicated.rownames.ls.of.df 
Check if there are any duplocated rownames in a list of dataframes.

- #### intersect.ls 
Intersect any number of lists.

- #### union.ls 
Intersect any number of list elements. Faster than reduce.

- #### unlapply 
lapply, then unlist

- #### list.wNames 
create a list with names from ALL variables you pass on to the function

- #### as.list.df.by.row 
Split a dataframe into a list by its columns. omit.empty for the listelments; na.omit 

- #### as.list.df.by.col 
oSplit a dataframe into a list by its rows. omit.empty for the listelments; na.omit and 

- #### reorder.list 
reorder elements of lists in your custom order of names / indices.

- #### range.list 
range of values in whole list

- #### intermingle2lists 
Combine 2 lists (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side comparisons, 

- #### as.listalike 
convert a vector to a list with certain dimensions, taken from the list it wanna resemble

- #### reverse.list.hierarchy 
reverse list hierarchy

- #### list2fullDF.byNames 
...

- #### list2fullDF.presence 
Convert a list to a full matrix.  Designed for occurence counting, think tof table(). Rows = all ENTRIES 

- #### splitbyitsnames 
split a list by its names

- #### splititsnames_byValues 
split a list by its names

- #### intermingle2vec 
Combine 2 vectors (of the same length) so that form every odd and every even element of a unified vector.

- #### intermingle.cbind 
Combine 2 data frames (of the same length) so that form every odd and every even element of a unified list. Useful for side-by-side 

- #### ls2categvec 
Convert a list to a vector repeating list-element names, while vector names are the list elements

- #### list.2.replicated.name.vec 
Convert a list to a vector, with list elements names replicated as many times, as many elements each 

- #### symdiff 
Quasy symmetric difference of any number of vectors

- #### cv 
...

- #### sem 
Calculates the standard error of the mean (SEM) for a numeric vector (it 

- #### fano 
Calculates the fano factor on a numeric vector (it excludes NA-s 

- #### geomean 
Calculates the geometric mean of a numeric vector (it excludes NA-s by default)

- #### mean_of_log 
Calculates the mean of the log_k of a numeric vector (it excludes NA-s by default)

- #### movingAve 
Calculates the moving / rolling average of a numeric vector.

- #### movingAve2 
Calculates the moving / rolling average of a numeric vector, using filter().

- #### movingSEM 
Calculates the moving / rolling standard error of the mean (SEM) on a numeric vector.

- #### imovingSEM 
Calculates the moving / rolling standard error of the mean (SEM). It calculates it to the edge of the vector with incrementally smaller 

- #### shannon.entropy 
Calculate shannon entropy
