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
                      denominator, denominatorDescription, displayDenominatorDescription) |>
        dplyr::rename(indicator_id = id,
                      indicator_name = name,
                      indicator_display_name = displayName)


    return(temp)
}


#' Fetch indicator groups from DHIS2
#'
#' @param username username
#' @param password password
#' @param base_url base_url from DHIS2 instance
#'
#' @returns A tibble of indicator groups from DHIS2
#' @export
#'
#' @examples
#' \dontrun{
#'   get_indicatorGroup(username, password, base_url)
#' }
#'
get_indicatorGroup <- function(username, password, base_url) {

    cols <- c("id", "name", "displayName", "indicators")
    cols_string <- paste(cols, collapse = ",")

    url <- paste0(base_url, "/api/indicatorGroups?paging=false&fields=", cols_string)
    response <- dhis2fetch::pull_dhis2_element(url, username, password) %>%
        purrr::pluck("indicatorGroups")


    temp <- response %>%
        dplyr::select(id, name, displayName, indicators)%>%
        dplyr::rename(indicatorGroup_id = id,
                      indicatorGroup_name = name,
                      indicatorGroup_displayName = displayName,
                      indicator = indicators)

    return(temp)
}



#' Create extended indicators table including indicator group name
#'
#' @param username user credentials
#' @param password user credentials
#' @param base_Url base_url for DHIS2 instance
#'
#' @returns a tibble of indicators with indicator group
#' @export
#'
#' @examples
#' \dontrun{
#'   get_indicators_table(username, password, base_url)
#' }
#'

get_indicators_table <- function(username, password, base_Url){

    indicators <- dhis2fetch::get_indicators(username, password, base_url)

    indicator_groups <- dhis2fetch::get_indicatorGroup(username, password, base_url)

    indicator_groups_flat <- indicator_groups %>%
        tidyr::unnest(indicator, names_sep = "_")

    temp <- indicators |>
        dplyr::left_join(indicator_groups_flat, by = c("indicator_id" = "indicator_id"))

    return(temp)

}
