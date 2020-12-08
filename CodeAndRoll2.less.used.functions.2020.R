######################################################################
# A collection of lessfrequently used CodeAndRoll2 functions
######################################################################
# source('~/GitHub/Packages/CodeAndRoll2/CodeAndRoll2.less.used.functions.2020')
# source('https://raw.githubusercontent.com/vertesy/CodeAndRoll2/master/CodeAndRoll2.less.used.functions.2020')

# ------------------------------------------------------------------------------------------------
attach_w_rownames <- function(df_w_dimnames, removePreviousVariables = FALSE) { # Take a data frame (of e.g. metadata) from your memory space, split it into vectors so you can directly use them. E.g.: Instead of metadata$color[blabla] use color[blabla]
  if (removePreviousVariables) { rm(list = colnames(df_w_dimnames), envir = .GlobalEnv); print("removed") }
  if (!is.null(rownames(df_w_dimnames)) & !is.null(colnames(df_w_dimnames))) {
    namez = rownames(df_w_dimnames)
    iprint("Now directly available in the workspace:      ", colnames(df_w_dimnames))
    attach(df_w_dimnames)
    for (n in colnames(df_w_dimnames)) {
      # x = get(n); # it failed at some point in some columns for unknown reason??
      x = df_w_dimnames[, n]
      names(x) = namez
      assign(n, x, envir = .GlobalEnv) } # for
  } else {print("ERROR: the DF does not have some of the dimnames!")}
}

filter_InCircle <- function(df2col = cbind(rnorm(100),rnorm(100)) # Find points in/out-side of a circle.
                            , inside = TRUE, r = 1, coloffset = 0, drawCircle = FALSE
                            , center = list(c(0,0), "mean", "median")[[2]], ...) {
  xi = df2col[ ,1]; yi = df2col[ ,2]
  if (center == "mean") { x = mean(xi); y = mean(yi)
  } else if (center == "median") { x = median(xi);  y = median(yi)
  } else if  (length(center) == 2) { x = center[1]; y = center[1] }
  side = if (inside) "inside" else "outside"
  PASS = if (inside) ((xi - x)**2 + (yi - y)**2 < r**2) else ((xi - x)**2 + (yi - y)**2 > r**2)
  PASSTXT = paste0(pc_TRUE(PASS, NumberAndPC = TRUE), " points are ", side," the circle.")
  iprint(PASSTXT)

  SMRY = paste0("Radius: ",iround(r)," | Center X, Y = ",iround(c(x , y )) ,"(",center,")")
  iprint(SMRY)
  if (drawCircle) plotrix::draw.circle(xi, yi, r, ...)
  return(PASS + coloffset)
}



