##'
##' Statistical data analysis support functions
##' Sergey "kol" Kolchin, 2019
##'
##' calculate_formula function
##'
library(dplyr)
library(tidyr)
library(rlang)

#' Calculate a measures aggregation formula on a dataset
#'
#'
#' @param .data Dataset (data.frame), id column requred
#' @param f A formula
#' @param id_cols Identification columns except id (string vector or R formula, like ~y)
#' @param value_cols Calculation columns (string vector or R formula, like ~y).
#' If missing, all numeric columns not including id_cols are processed
#' @param na.rm TRUE to replace all NAs to zero
#'
#' @return Calculation results (data.frame containing id_cols and value_cols)
#'         Results are placed to value_cols, id_cols copied without changes
#'
calculate_formula <- function(.data, f, id_cols = NULL, value_cols = NULL, na.rm = FALSE) {
  # Checks
  stopifnot(!missing(.data))
  stopifnot("data.frame" %in% class(.data))

  stopifnot(!missing(f))
  stopifnot(!is.null(f))
  stopifnot(!is.na(f))
  stopifnot(is.vector(f))

  # Check if column names provided as formulas (~y)
  if (!missing(id_cols) && rlang::is_formula(id_cols))
    id_cols <- all.vars(id_cols)[1]
  if (!missing(value_cols) && rlang::is_formula(value_cols))
    value_cols <- all.vars(value_cols)[1]

  # Determine list of value columns, if not provided
  # NA column would have logical type
  if (missing(value_cols)) {
    cols <- sapply(names(.data), function(x) class(.data[[x]]))
    num_cols <- cols[ which(!is.na(match(cols, c("numeric", "logical")))) ]
    if (is.null(id_cols))
      value_cols <- names(num_cols)
    else
      value_cols <- names(num_cols[!(names(num_cols) %in% id_cols)])
    if (length(value_cols) == 0)
      stop("No value columns identified")
  }

  # Make value functions - reserved for further use
  # for (x in value_cols) {
  #   eval(parse(text = paste0(x,
  #                            " <- function(v) { if (value_col=='", x,
  #                            "') v else 0 }")))
  # }

  # Transpose a dataset so id values go to columns and value fields go to rows
  all_cols <- append(append(c("id"), id_cols), value_cols)
  d <- .data[all_cols] %>%
    tidyr::pivot_longer(names_to = "m", cols = value_cols, values_to = "v") %>%
    tidyr::pivot_wider(names_from = "id", values_from = "v")

  # Calculate
  r <- NULL
  for (v in value_cols) {
    td <- d[d$m == v, ]
    if (na.rm) td <- td %>% dplyr::mutate_all(~replace(., is.na(.), 0))

    t <- data.frame(r = rlang::eval_tidy(rlang::parse_expr(f), td))
    names(t) <- v
    if (!is.null(r)) r <- cbind(r, t) else r <- t
  }

  if (!is.null(id_cols)) {
    # Add ID columns to result
    for(x in id_cols) r <- cbind(unique(.data[[x]]), r)
    names(r)[1:length(id_cols)] <- id_cols
  }
  return(r)
}

