#' Pull out an element list from DHIS2
#'
#' @param base_url  base_url of DHIS2 instance
#' @param username username for DHIS2 instance
#' @param password password for DHIS2 instance
#'
#' @returns A list of elements from DHIS2
#' @export
#'
#' @examples
#' \dontrun{
#'   pull_dhis2_element(base_url, username, password)
#' }
#'

pull_dhis2_element <- function(base_url, username, password) {
    temp <- base_url |>
        httr::GET(httr::authenticate(username,password))  |>
        httr::content("text") |>
        jsonlite::fromJSON()

    return(temp)
}
