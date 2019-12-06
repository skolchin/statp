##'
##' Statistical data analysis support functions
##' Sergey "kol" Kolchin, 2019
##'
##' predict_data function
##'
library(dplyr)
library(mgcv)
library(stats)


#' Data approximation
#'
#' This function builds an approximation model on data containing some observation interval
#' and then use this model to appoximate another observation interval.
#'
#' @param .data Source data (data.frame). Must contain interval and observation variables (for example, year and value)
#' @param .interval Target interval variable (vector)
#' @param method Approximation function ('lm', 'loess', 'glm', 'gam')
#' @param ... Parameters for approximation function
#' @param merge If TRUE, results are merged with source data
#' @param mark If TRUE, a 'forecast' variable will be added to result
#' @param vars Names of interval and observation variable (1st and 2nd numeric columns by default)
#'
#' @return Approximation results (data.frame)
#'
#' @examples
#' \dontrun{
#' predict_data(.data = data.frame(
#'                y = c(2010, 2013, 2014),
#'                total = c(30800000.00, 32285714.00, 41500000.00)),
#'              .interval = seq(from = 2015, to = 2020, by = 1),
#'              method = "gam")
#' }
#'
#' @name predict_data
#' @export
predict_data <- function(.data,
                         .interval,
                         method = "lm",
                         ...,
                         merge = TRUE,
                         mark = FALSE,
                         vars = NULL) {

  # Parameter checks
  if (!missing(vars)) {
    if (length(vars) < 2)
      stop("vars has to contain at least 2 variables")
    for (v in vars)
      if (!(v %in% names(.data)))
        stop(paste0("Variable '", v, "' not found in source data"))
  }
  else {
    cols <- sapply(names(.data), function(x) class(.data[[x]]))
    num_cols <- cols[ which(!is.na(match(cols, c("numeric", "logical")))) ]
    if (length(num_cols) == 0)
      stop("No value columns identified")
    if (length(num_cols) < 2)
      stop("Insufficient number of value columns")
    vars <- names(num_cols)[1:2]
  }

  # Make an approximation model
  if (method == "loess") {
    f <- stats::as.formula(paste0(vars[2], " ~ ", vars[1]))
    m <- stats::loess(f, ..., data = .data, control = stats::loess.control(surface = "direct"))
  }
  else if (method == "lm") {
    f <- stats::as.formula(paste0(vars[2], " ~ ", vars[1]))
    m <- stats::lm(f, ..., data = .data)
  }
  else if (method == "glm") {
    f <- stats::as.formula(paste0(vars[2], " ~ ", vars[1]))
    m <- stats::glm(f, ..., data = .data)
  }
  else if (method == "gam") {
    # For GAM parameters are passed through the formula, not as extra parameters
    requireNamespace("mgcv")
    bs <- "tp"
    k = min(10, nrow(.data))

    mf <- match.call(expand.dots = TRUE)
    if ("bs" %in% names(mf)) bs <- mf$bs
    if ("k" %in% names(mf)) k <- mf$k

    mf2 <- call("gam")
    mf2[[2L]] <- stats::as.formula(paste0(vars[2], " ~ s(", vars[1], ", bs = '", bs, "', k = ", k, ")"))
    n <- length(mf2) + 1
    mf2[[n]] <- .data
    names(mf2)[n] <- "data"

    m <- eval(mf2)
  }
  else {
    stop('Unknown method')
  }

  # Create parameter dataset and do approximation
  param_data <- data.frame(.interval, rep(NA, length(.interval)))
  names(param_data) <- vars[1:2]

  ret_data <- data.frame(.interval, stats::predict(m, newdata = param_data))
  names(ret_data) <- vars[1:2]

  # Mark forecasting
  if (mark)
    ret_data <- cbind(ret_data, data.frame(forecast = rep(TRUE, nrow(ret_data))))

  if (merge) {
    # filter out data which exists in source
    new_data <- ret_data[ !(ret_data[[ vars[1] ]] %in% .data[[ vars[1] ]]), ]

    if (mark)
      old_data <- cbind(.data, data.frame(forecast = rep(FALSE, nrow(.data))))
    else
      old_data <- .data

    # add columns missing from ret_data
    sapply(setdiff(names(old_data), names(new_data)), function(v) new_data[v] <<- NA  )

    # merge and sort
    ret_data <- rbind(new_data, old_data)
    ret_data <- ret_data[ order(ret_data[,1]), ]
  }
  return(ret_data)
}

