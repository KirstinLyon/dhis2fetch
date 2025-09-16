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
#'   pull_dhis2_element(username, password, base_url)
#' }
#'

pull_dhis2_element <- function(username, password, base_url) {
    temp <- base_url %>%
        httr::GET(httr::authenticate(username,password))  %>%
        httr::content("text") %>%
        jsonlite::fromJSON()

    return(temp)
}
