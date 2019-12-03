##'
##' Statistical data analysis support functions
##' Sergey "kol" Kolchin, 2019
##'
##' remove_outliers function
##'


#' Remove outliers
#'
#' The function takes a dataset, checks that given variables for outliers and
#' either removes them or approximate using other data in these variables.
#'
#' @param .data dataset (data.frame)
#' @param vars variables to inspect (character string, formula or vector).
#'             If omitted, all numeric columns are processed. If a single variable is provided
#'             and approx is TRUE, outlier values are removed from dataset. If it is a vector,
#'             data is not removed but replaced with NAs.
#' @param approx If TRUE, outliers are approximated using gam() function, if FALSE (default) - removed or replaced by NAs.
#'
#' @return resulting data.frame or a vector if source dataset contain only one column
#'
#' @examples
#' \dontrun{
#' d <- data.frame(
#'   id = c("1","2","3","4","5"),
#'   a = c(1,2,10,1000,100),
#'   b = c(19999, 30000000, 1, 5, 426798))
#' remove_outliers(d, ~a)
#' remove_outliers(d)
#' }
#'
#' @name remove_outliers
#' @export
remove_outliers <- function(.data, vars = NULL, approx = FALSE) {

  if (!missing(vars) && rlang::is_formula(vars))
    # Check if column names provided as formulas (~total)
    vars <- all.vars(vars)[1]
  else if (missing(vars)) {
    # Find all numeric/logical values
    cols <- sapply(names(.data), function(x) class(.data[[x]]))
    num_cols <- cols[ which(!is.na(match(cols, c("numeric", "logical")))) ]
    vars <- names(num_cols)
    if (length(vars) == 0)
      stop("No value columns identified")
  }
  else {
    # Check all variables exist in dataset
    if (length(setdiff(vars, names(.data)))>0)
      stop(paste0("Variables missing from source data: ", setdiff(vars, names(.data))))
  }

  # Check data is sufficient for approximation
  if (approx && nrow(.data) < 3)
    stop("Insufficient number of observations in source data")

  ds <- NULL
  if (length(vars) == 1 && !approx) {
    # Simple case - remove outliers of one variable
    out <- grDevices::boxplot.stats(.data[[vars[1]]])$out
    ds <- .data[ which(!(.data[[vars[1]]] %in% out)), ]
  }
  else {
    ds <- as.data.frame(
      sapply(vars, function(v) {
        # Prepare the data
        d <- .data[[v]]
        out <- grDevices::boxplot.stats(d)$out

        if (!approx) {
          d[ which(.data[[v]] %in% out) ] <- NA
          d
        }
        else {
          # build a dataset with outliers removed
          y <- .data[ which(!(.data[[v]] %in% out)), v]
          if (length(y) < 3)
            stop(paste0("Insufficient number of non-outlied observations of variable '",
                        v, "'"))
          x <- seq_along(.data[[v]])
          x <- x[ which(!(.data[[v]] %in% out)) ]

          # Do an approximation
          nx <- seq_along(.data[[v]])
          nd <- predict_data(.data = data.frame(x = x, y = y),
                             .interval = nx, bs = "tp", k = 3,
                             method = "gam",
                             merge = FALSE,
                             vars = c("x", "y"))

          # Merge data
          d[ which(.data[[v]] %in% out) ] <- nd[ which(.data[[v]] %in% out), "y" ]
          d
        }
      }))

    names(ds) <- vars
    ds <- cbind(.data[setdiff(names(.data), vars)], ds)
    ds <- ds[names(.data)]
  }
  return(ds)
}

