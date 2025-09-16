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

    cols <- c("id", "name", "displayName", "dataSetElements")
    cols_string <- paste(cols, collapse = ",")


    url <- paste0(base_url, "/api/dataElements?paging=false&fields=", cols_string)

    response <- dhis2fetch::pull_dhis2_element(url, username, password) %>%
        purrr::pluck("dataElements")

    temp <- response %>%
        dplyr::select(id, name, displayName, dataSetElements) |>
        dplyr::rename(data_element_id = id,
                      data_element_name = name,
                      data_element_display_name = displayName)

    return(temp)
}


