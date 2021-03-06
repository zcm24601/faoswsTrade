#' Change position of flags.
#'
#' @export

swapFlags <- function(variable = stop("'variable' is required."),
                      swap = stop("'swap' is required."),
                      condition = NULL) {

  # example of swap: swap = '\\2\\1\\3'

  if (all(is.na(variable))) {
    warning("All NAs: no need to swap.", call. = TRUE)
    return(variable)
  }

  res <- sub('1(.)(.)(.)', paste0(1, swap), variable) %>%
           as.integer()

  if (is.null(condition)) {
    return(res)
  } else {
    return(ifelse(condition, res, variable))
  }
}
