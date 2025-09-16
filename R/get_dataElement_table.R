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

    cols <- c("id", "name", "displayName", "dataSetElements")
    cols_string <- paste(cols, collapse = ",")


    url <- paste0(base_url, "/api/dataElements?paging=false&fields=", cols_string)

    response <- dhis2fetch::pull_dhis2_element(username, password, url) %>%
        purrr::pluck("dataElements")

    temp <- response %>%
        dplyr::select(id, name, displayName, dataSetElements) %>%
        dplyr::rename(data_element_id = id,
                      data_element_name = name,
                      data_element_display_name = displayName)

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

get_dataElementGroup <- function(username, password, base_url) {

    cols <- c("id", "name", "displayName", "dataElements")
    cols_string <- paste(cols, collapse = ",")

    url <- paste0(base_url, "/api/dataElementGroups?paging=false&fields=", cols_string)

    response <- dhis2fetch::pull_dhis2_element(username, password, url) %>%
        purrr::pluck("dataElementGroups")

    temp <- response %>%
        dplyr::select(id, name, displayName, dataElements)%>%
        dplyr::rename(dataElementGroup_id = id,
                      dataElementGroup_name = name,
                      dataElementGroup_displayName = displayName,
                      dataElement = dataElements)

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


    data_elements <- dhis2fetch::get_dataElements(username, password, base_url)


    data_element_groups <- dhis2fetch::get_dataElementGroup(username, password, base_url)


    data_element_groups_flat <- data_element_groups |>
        tidyr::unnest(dataElement, names_sep = "_")

    temp <- data_elements |>
        dplyr::left_join(data_element_groups_flat, by = c("dataElement_id" = "dataElement_id"))

    return(temp)

}


