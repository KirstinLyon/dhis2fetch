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
#'   get_data_element_group(username, password, base_url)
#' }
#'

get_data_element_group <- function(username, password, base_url) {

    cols <- c("id", "name", "displayName", "dataElements")
    cols_string <- paste(cols, collapse = ",")

    url <- paste0(base_url, "/api/dataElementGroups?paging=false&fields=", cols_string)

    response <- dhis2fetch::pull_dhis2_element(url, username, password) %>%
        purrr::pluck("dataElementGroups")

    temp <- response %>%
        dplyr::select(id, name, displayName, dataElements)

    return(temp)
}
