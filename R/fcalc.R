##'
##' Statistical data analysis support functions
##' Sergey "kol" Kolchin, 2019
##'
##' agregate_data function definition
##'
library(dplyr)
library(tidyr)
library(rlang)

#' Measure aggregation by formula
#'
#' Performs a measures aggregation by formula on a dataset
#'
#' The function transforms a dataset so measure IDs become dataframe columns and then
#' applies a formula to calculate aggregated measures.
#'
#' A formula can be a mesaure ID or any single-parameter function with
#' aggregation operators (+. -, etc). See examples below.
#'
#' If a dataset contain additional identification variables, id_cols should be provided.
#'
#' A value_cols parameter specifies which variables are to be aggregated. If omitted,
#' any numeric or logical columns are taken.
#'
#' @param .data Dataset (data.frame), 'id' column is required.
#' @param f A formula
#' @param id_cols Identification columns except id (string vector or R formula, like ~y)
#' @param value_cols Calculation columns (string vector or R formula, like ~y)
#' @param na.rm TRUE to replace all NAs to zero
#'
#' @return Calculation results (data.frame containing id_cols and value_cols)
#'         Results are placed to value_cols, id_cols copied without changes
#'
#' @examples
#' \dontrun{
#'
#' d <- data.frame(id = c("i1", "i1", "i2", "i3"), y = c(10, 20, 10, 10), a = c(4,2,3, 1), r = c(4,5,6,7))
#' agregate_data(d, f = "log(i1)+i2+i3", id_cols = ~y, value_cols = c("a", "r"), na.rm = TRUE)
#'
#' d <- data.frame(id = c("i1", "i2", "i3"), a = c(4, 3, 1), r = c(4, 6,7))
#' agregate_data(d, f = "i1+i2+ifelse(i3==1,0,-1)", value_cols = c("a", "r"))
#' }
#'
#' @name agregate_data
#' @export
aggregate_data <- function(.data, f, id_cols = NULL, value_cols = NULL, na.rm = FALSE) {
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

