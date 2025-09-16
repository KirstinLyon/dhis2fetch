#' Pull Indicators from DHIS2
#'
#' @param username credentials for DHIS2
#' @param password password credentials for DHIS2
#' @param base_url base url for DHIS2 instance
#'
#' @returns A tibble of indicators from DHIS2
#' @export
#'
#' @examples
#' \dontrun{
#'   get_indicators(username, password, base_url)
#' }
#'
get_indicators <- function(username, password, base_url) {

    #All columns to be pulled from the API
    cols <- c("id", "name", "description", "displayName", "displayDescription",
              "annualized", "numerator", "numeratorDescription", "displayNumeratorDescription",
              "denominator", "denominatorDescription", "displayDenominatorDescription",
              "dataSets")

    cols_string <- paste(cols, collapse = ",")

    url <- paste0(base_url, "/api/indicators?paging=false&fields=", cols_string)

    response <- dhis2fetch::pull_dhis2_element(url, username, password) %>%
        purrr::pluck("indicators")

    temp <- response %>%
        dplyr::select(id, name, description, displayName, displayDescription,
                      annualized, numerator, numeratorDescription, displayNumeratorDescription,
                      denominator, denominatorDescription, displayDenominatorDescription)


    return(temp)
}
