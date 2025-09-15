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
#'   get_data_elements(username, password, base_url)
#' }
#'

get_data_elements <- function(username, password, base_url) {
    url <- paste0(base_url, "/api/dataElements?paging=false&fields=id,name,displayName,dataSetElements")
    data_elements <- dhis2fetch::pull_dhis2_element(url, username, password) %>%
        purrr::pluck("dataElements")
    return(data_elements)
}


