#' Get a tibble of all programs
#'
#' @param username DHIS2 username
#' @param password DHIS2 password
#' @param base_url base_url for DHIS2 instance
#'
#' @returns a tibble of programs
#' @export
#'
#' @examples
#' \dontrun{
#'   get_programs(username, password, base_url)
#' }
#'

get_programs <- function(username, password, base_url) {

    cols <- c("id", "displayName", "name")

    col_string <- paste(cols, collapse = ",")

    url <- paste0(base_url, "/api/programs?fields=", col_string)
    response <- dhis2fetch::pull_dhis2_element(username, password, url)
    programs <- response$programs

    temp <- programs %>%
        dplyr::rename(program_id = id,
                      program_name = name,
                      program_displayName = displayName)


    return(temp)
}

#' Get a tibble of program indicators groups
#'
#' @param username DHIS2 username
#' @param password DHIS2 password
#' @param base_url base_url for DHIS2 instance
#'
#' @returns a tibble of program inidcator groups
#' @export
#'
#' @examples
#' \dontrun{
#'   get_programIndicatorGroup(username, password, base_url)
#' }
#'
get_programIndicatorGroup <- function(username, password, base_url) {

    cols <- c("id", "displayName", "name")

    col_string <- paste(cols, collapse = ",")

    url <- paste0(base_url, "/api/programIndicatorGroups?fields=", col_string)
    response <- dhis2fetch::pull_dhis2_element(username, password, url)
    program_indicator_groups <- response$programIndicatorGroups

    temp <- program_indicator_groups |>
        dplyr::rename(programIndicator_id = id,
               programIndicator_name = name,
               programIndicator_displayName = displayName)

    return(temp)
}

#' Get all program inidcators
#'
#' @param username DHIS2 username
#' @param password DHIS2 password
#' @param base_url base_url for DHIS2 instance
#'
#' @returns a tibble of program inidcators
#' @export
#'
#' @examples
#' \dontrun{
#'   get_programIndicator(username, password, base_url)
#' }
#'

get_programIndicator <- function(username, password, base_url) {

    cols <- c("id", "displayName", "name", "programIndicatorGroups", "filter",
              "description", "expression",  "aggregationType",
              "program")

    col_string <- paste(cols, collapse = ",")

    url <- paste0(base_url, "/api/programIndicators?fields=", col_string)
    response <- dhis2fetch::pull_dhis2_element(username, password, url)
    program_indicators <- response$programIndicators
    return(program_indicators)
}


#' Program Indicator Table
#'
#' @param username DHIS2 username
#' @param password DHIS2 password
#' @param base_url DHIS2 base_url
#'
#' @returns a tibble of program indicators
#' @export
#'
#' @examples
#' \dontrun{
#'   get_programIndicator_table(username, password, base_url)
#' }
#'

get_programIndicator_table <- function(username, password, base_url) {

    program <- dhis2fetch::get_programs(username, password, base_url)
    programIdicatorGroup <- dhis2fetch::get_programIndicatorGroup(username, password, base_url)
    programIndicator <- dhis2fetch::get_programIndicator(username, password, base_url)


    temp <- programIndicators |>
        tidyr::unnest(program, names_sep = "_", keep_empty = TRUE) |>
        tidyr::unnest(programIndicatorGroups, names_sep = "_", keep_empty = TRUE) |>
        dplyr::left_join(program, by = c("program_id" = "id")) |>
        dplyr::left_join(programIndicatorGroup, by = c("programIndicatorGroups_id" = "id"))


    return(temp)

}
