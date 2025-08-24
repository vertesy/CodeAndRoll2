## Apply returns differently transposed results, based on the margin (direction) it is applied on

In R, the `apply()` function is used to apply a function across **rows** or **columns** of a matrix. The behavior depends on the `MARGIN` argument:

1. `MARGIN = 1`: apply the function **row-wise**
2. MARGIN = 2`: apply the function **column-wise**

However there is a pretty unexpected quirk in what they return

1.  `apply(..., MARGIN = 1)` returns a **transposed result**, while 
2. `apply(..., MARGIN = 2)` does **not**. 

This means that **when applying a function row-wise, the result matrix has rows and columns flipped unless corrected with `t()`.** 
Column-wise application behaves as expected and preserves shape.


Here’s a simple example to demonstrate:

```r
m <- matrix(1:9, nrow = 3, byrow = TRUE)
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6
# [3,]    7    8    9
```

Row-wise (`MARGIN = 1`):

```r
apply(m, 1, function(x) x + 1)
#      [,1] [,2] [,3]
# [1,]    2    5    8
# [2,]    3    6    9
# [3,]    4    7   10
```

→ Output is **transposed** — each row result becomes a column. To fix:

```r
t(apply(m, 1, function(x) x + 1))
```

Column-wise (`MARGIN = 2`):

```r
apply(m, 2, function(x) x + 1)
#      [,1] [,2] [,3]
# [1,]    2    3    4
# [2,]    5    6    7
# [3,]    8    9   10
```

→ Output is in expected shape — no transposition needed.





------

###  What About Higher Dimensions? `apply()` on 3D and 4D Arrays

Yes, `apply()` can work on arrays with **3 or more dimensions**, using `MARGIN` to specify which dimensions to preserve.

For example:

```r
a <- array(1:24, dim = c(2, 3, 4))  # 3D array

# Apply over the 3rd dimension (preserve dim 3, collapse dim 1 & 2)
apply(a, 3, sum)
# Returns one sum per "slice" (2x3 matrix), result: vector of length 4

# Apply over dims 1 and 2 (preserve rows and columns)
apply(a, c(1, 2), mean)
# Returns a 2x3 matrix: mean across the 4 layers
```

##### 🔹 Key behavior:

- `apply()` **collapses** the dimensions **not** in `MARGIN`.
- The result’s shape follows the **dimensions in `MARGIN`**.
- There is **no transposition quirk** like with `MARGIN = 1` on a matrix.
- R **does not reorder axes** when `MARGIN` > 1 — it preserves axis order.

##### 🔸 Caveat:

The transpose behavior only appears **when applying over rows (MARGIN = 1) of a 2D matrix** and the function returns vectors of fixed length. This is a special case — higher-dimensional arrays behave as expected and **do not transpose** anything.

------

### ✅ Summary

| Case                            | Transposed Output? |
| ------------------------------- | ------------------ |
| `apply(matrix, 1, ...)`         | ✅ Yes              |
| `apply(matrix, 2, ...)`         | ❌ No               |
| `apply(array, MARGIN > 2, ...)` | ❌ No               |





------

## ✅ A Simple Fix: A Wrapper That Avoids the Transpose Quirk

I implemented a clean wrapper in my `CodeAndRoll2` package,  that behaves exactly like `apply()` but **automatically fixes the transpose quirk** when `MARGIN = 1`. Here's such a function:

```r
# See: ('https://raw.githubusercontent.com/vertesy/CodeAndRoll2/master/CodeAndRoll2.R')

# A drop-in replacement for apply() that fixes the rowwise transpose issue
apply2 <- function(X, MARGIN, FUN, ...) {
  # Input assertions
  stopifnot(
    is.array(X),                      # X must be an array or matrix
    is.numeric(MARGIN),              # MARGIN must be numeric
    all(MARGIN >= 1),                # MARGIN values must be >= 1
    max(MARGIN) <= length(dim(X)),   # MARGIN must refer to valid dimensions
    is.function(FUN)                 # FUN must be a valid function
  )

  # Apply function using base R
  result <- apply(X, MARGIN, FUN, ...)

  # Correct transposed result only when:
  # - input is a 2D matrix
  # - applying across rows (MARGIN = 1)
  # - and result is a matrix (i.e., all FUN outputs same-length vector)
  if (
    is.matrix(X) &&
    length(dim(X)) == 2 &&
    identical(MARGIN, 1) &&
    is.matrix(result)
  ) {
    result <- t(result)
  }

  # Output assertions (safe-guards, optional)
  stopifnot(!is.null(result))  # Ensure result exists

  return(result)
}
```

### 🔹 Example usage:

```r
m <- matrix(1:9, nrow = 3, byrow = TRUE)

apply2(m, 1, function(x) x + 1)
# Returns expected output, no transposition needed
```

I find that `apply2()` wrapper is a clean, safe solution that saves headaches and works as expected across common use cases.

