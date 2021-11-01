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




#' @title Summary stat. text formatting for logical vectors (%, length)
#' @description Summary stat. text formatting for logical vectors (%, length)
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


NrAndPc <- function(logical_vec = idx_localised, total = TRUE, NArm = TRUE) {
  x = paste0(pc_TRUE(logical_vec), " or ", sum(logical_vec, na.rm = NArm))
  if (total) paste0(x, " of ", length(logical_vec))
}



#' @title Create a copy of your matrix filled with rownames
#' @description Create a copy of your matrix, where every entry is replaced by the corresponding row name. Useful if you want to color by row name in a plot (where you have different number of NA-values in each row).
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


rowNameMatrix <- function(mat_w_dimnames) {
  matrix(rep(rownames(mat_w_dimnames), ncol(mat_w_dimnames) ), nrow = nrow(mat_w_dimnames), ncol = ncol(mat_w_dimnames))

}


#' @title  Create a copy of your matrix filled with columnames
#' @description Create a copy of your matrix, where every entry is replaced by the corresponding column name. Useful if you want to color by column name in a plot (where you have different number of NA-values in each column).
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


colNameMatrix <- function(mat_w_dimnames) {
  x = rep(colnames(mat_w_dimnames), nrow(mat_w_dimnames) )
  t(matrix(x, nrow = ncol(mat_w_dimnames), ncol = nrow(mat_w_dimnames)))
}




#' @title Legend color.
#' @description Legend color. Source: https://aurelienmadouasse.wordpress.com/2012/01/13/legend-for-a-continuous-color-scale-in-r/
#' @param col PARAM_DESCRIPTION
#' @param lev PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname legend.col
#' @export


legend.col <- function(col, lev) { # Legend color.
  opar <- par
  n <- length(col)
  bx <- par("usr")
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  xx <- rep(box.cx, each = 2)

  par(xpd = TRUE)
  for (i in 1:n) {
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(lev), max(lev)),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = .25)
  par <- opar
  par(xpd = FALSE)
  # print("You might need to set par('mar' = c( 5.1, 4.1, 4.1, 2.1)) to higher values.")
}



#' @title CONTAINS A QUICK FIX FOR THE NUMBER OF COLOR LEVELS. See #59 on GitHub ###
#' @description This function converts a vector of values("yourdata") to a vector of color levels. One must define the number of colors. The limits of the color scale("zlim") or the break points for the color changes("breaks") can also be defined. When breaks and zlim are defined, breaks overrides zlim.
#' @param yourdata PARAM_DESCRIPTION
#' @param zlim PARAM_DESCRIPTION
#' @param col PARAM_DESCRIPTION, Default: rev(heat.colors(max(12, 3 * length(unique(yourdata)))))
#' @param breaks PARAM_DESCRIPTION
#' @param rename PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname val2col
#' @export


val2col <- function(yourdata,
                    zlim,
                    col = rev(heat.colors(max(12, 3 * length(unique(yourdata))))),
                    breaks,
                    rename = FALSE) {
  if (!missing(breaks)) {
    if (length(breaks) != (length(col) + 1)) {
      stop("must have one more break than color")
    }
  }
  if (missing(breaks) & !missing(zlim)) {
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 1))
  }
  if (missing(breaks) & missing(zlim)) {
    zlim <- range(yourdata, na.rm = TRUE)
    zlim[2] <- zlim[2] + c(zlim[2] - zlim[1]) * (0.001)
    zlim[1] <- zlim[1] - c(zlim[2] - zlim[1]) * (0.001)
    breaks <- seq(zlim[1], zlim[2], length.out = (length(col) + 1))
  }
  colorlevels <- col[((as.vector(yourdata) - breaks[1]) /
                        (range(breaks)[2] - range(breaks)[1])) * (length(breaks) - 1) + 1]
  if (length(names(yourdata))) {
    names(colorlevels) = yourdata
  }

  if (rename) {
    names(colorlevels) = yourdata
  } # works on vectors only"
  colorlevels
}




#' @title Converts any vector into a numeric vector.
#' @description Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
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


as.numeric.wNames.old <- function(vec) {
  numerified_vec = as.numeric(as.factor(vec))
  if (!is.null(names(vec))) {names(numerified_vec) = names(vec)}
  return(numerified_vec)
}



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title as.numeric.wNames.old
#' @description Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
#' @param vec PARAM_DESCRIPTION
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
as.numeric.wNames.old <- function(vec) { # Converts any vector into a numeric vector, and puts the original character values into the names of the new vector, unless it already has names. Useful for coloring a plot by categories, name-tags, etc.
  numerified_vec = as.numeric(as.factor(vec))
  if (!is.null(names(vec))) {names(numerified_vec) = names(vec)}
  return(numerified_vec)
}
