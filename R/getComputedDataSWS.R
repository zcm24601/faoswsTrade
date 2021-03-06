#' Get computed dataset on SWS.
#'
#' @param reporter Reporter.
#'
#' @return A data.table with results.
#'
#' @import dplyr
#'
#' @export

getComputedDataSWS <- function(reporter = NA) {
  # TODO: use error handling
  key <- DatasetKey(domain  = 'trade',
                    dataset = 'completed_tf_cpc_m49',
                    dimensions = list(
                      Dimension(name = Vars[['reporters']], keys = reporter),
                      Dimension(name = Vars[['partners']],  keys = Keys[['partners']]),
                      Dimension(name = Vars[['items']],     keys = Keys[['items']]),
                      Dimension(name = Vars[['elements']],  keys = Keys[['elements']]),
                      Dimension(name = Vars[['years']],     keys = Keys[['years']])))

  GetData(key = key, omitna = FALSE)
}

