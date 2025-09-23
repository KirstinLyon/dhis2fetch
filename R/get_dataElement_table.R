#' Extract data elements from DHIS2
#'
#' @param username user credentials
#' @param password user credentials
#' @param base_url base url of the DHIS2 instance
#'
#' @returns a tibble of data elements
#' @export
#'
#' @examples
#' \dontrun{
#'   get_dataElements(username, password, base_url)
#' }
#'

get_dataElements <- function(username, password, base_url) {

    cols <- c("id", "name", "displayName", "dataElementGroups")
    cols_string <- paste(cols, collapse = ",")

    url <- paste0(base_url, "/api/dataElements?paging=false&fields=", cols_string)

    response <- dhis2fetch::pull_dhis2_element(username, password, url) %>%
        purrr::pluck("dataElements")

    temp <- response %>%
        dplyr::rename_with(~ paste0("dataElement_", .x), dplyr::everything()) |>
        tidyr::unnest(dataElement_dataElementGroups, names_sep = "_", keep_empty = TRUE) %>%
        dplyr::rename(dataElementGroups_id = dataElement_dataElementGroups_id)

    return(temp)
}

#' Get data element groups from DHIS2
#'
#' @param username username credentials
#' @param password password credentials
#' @param base_url base url of the DHIS2 instance
#'
#' @returns a list of data element groups
#' @export
#'
#' @examples
#' \dontrun{
#'   get_dataElementGroup(username, password, base_url)
#' }
#'

get_dataElementGroups <- function(username, password, base_url) {

    cols <- c("id", "name", "displayName")
    cols_string <- paste(cols, collapse = ",")

    url <- paste0(base_url, "/api/dataElementGroups?paging=false&fields=", cols_string)

    response <- dhis2fetch::pull_dhis2_element(username, password, url) %>%
        purrr::pluck("dataElementGroups")

    temp <- response %>%
        dplyr::rename_with(~ paste0("dataElementGroups_", .x), dplyr::everything())

    return(temp)
}


#' Get dataElement table with dataElement group names
#'
#' @param username user credentials
#' @param password user credentials
#' @param base_url base url of the DHIS2 instance
#'
#' @returns a tibble of data elements with data element group names
#' @export
#'
#' @examples
#' \dontrun{
#'   get_dataElement_table(username, password, base_url)
#' }
#'

get_dataElement_table <- function(username, password, base_url){

    dataElements <- dhis2fetch::get_dataElements(username, password, base_url)
    dataElementGroups <- dhis2fetch::get_dataElementGroups(username, password, base_url)

    temp <- dataElements %>%
        dplyr::left_join(dataElementGroups, by = c("dataElementGroups_id" = "dataElementGroups_id"))

    return(temp)

}


