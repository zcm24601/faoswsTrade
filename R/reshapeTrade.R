#' Reshape trade data.
#'
#' @param data data.
#'
#' @return Reshaped data.
#'
#' @import dplyr
#'
#' @export

reshapeTrade <- function(data = NA) {

  data <- data %>%
    # XXX If already a tible, remove
    tbl_df() %>%
    dplyr::mutate(
           flow      = ifelse(stringr::str_sub(measuredElementTrade, 1, 2) == '56', 1L, 2L),
           type      = ifelse(measuredElementTrade %in% c('5622', '5922'), 'value', 'qty'),
           flagTrade = paste(flagObservationStatus, flagMethod, sep = '-')
           ) %>%
    select(-flagObservationStatus, -flagMethod)

  data_flags <- data %>%
    select(-measuredElementTrade, -Value) %>%
    tidyr::spread(type, flagTrade) %>%
    dplyr::rename(flag_qty = qty, flag_value = value)

  data_value <- data %>%
    select(-measuredElementTrade, -flagTrade) %>%
    tidyr::spread(type, Value)

  data_qelement <- data %>%
    dplyr::filter(type != 'value') %>%
    select(flow, measuredElementTrade, measuredItemCPC) %>%
    unique()

  res <- left_join(data_value, data_flags) %>%
    left_join(data_qelement) %>%
    dplyr::rename(measuredElementTrade_q = measuredElementTrade)

  return(res)
}


