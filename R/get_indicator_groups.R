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
#'   get_indicator_groups(username, password, base_url)
#' }
#'
get_indicator_groups <- function(username, password, base_url) {

    cols <- c("id", "name", "displayName", "indicators")
    cols_string <- paste(cols, collapse = ",")

    url <- paste0(base_url, "/api/indicatorGroups?paging=false&fields=", cols_string)
    response <- dhis2fetch::pull_dhis2_element(url, username, password) |>
        purrr::pluck("indicatorGroups")


    temp <- response |>
        dplyr::select(id, name, displayName, indicators)

    return(temp)
}
