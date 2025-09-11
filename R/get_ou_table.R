#Functions for extrating the Organsational Unit table from DSHI2



#' Create a table for Organisation Unit.
#'
#' @param username credentials
#' @param password credentials
#' @param base_url base_url of DHIS2 instances
#'
#' @returns a tibble of OUs
#' @export
#'
#' @examples
#' \dontrun{
#'   get_ou_table(username, password, base_url)
#' }
#'

get_ou_table <- function(username, password, base_url){

    response <- pull_dhis2_element(paste0(base_url, "/api/organisationUnits.json?fields=id,name,level,path,&paging=false"),
                                               username,
                                               password) %>%
        purrr::pluck("organisationUnits")


    process_data <- response %>%
        dplyr::mutate(full_path = path,
               path = stringr::str_remove(path, "^/")) %>%
        tidyr::separate_rows(path, sep = "/") %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(parent_level = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = parent_level, values_from = path, names_prefix = "ouid")

    table_of_ids <- response %>%
        dplyr::select(id, name) %>%
        dplyr::rename(ou_name = name)


    # Find all level_* columns in process_data
    level_cols <- process_data %>%
        dplyr::select(dplyr::starts_with("ouid")) %>%
        names()


    ou_table <- process_data %>%
        purrr::reduce(level_cols,
               function(df, col_name) {
                   i <- which(level_cols == col_name)
                   dplyr::left_join(df, table_of_ids, by = stats::setNames("id", col_name)) %>%
                       dplyr::rename(!!paste0("ou", i) := ou_name)
               },
               .init = .)


    return(ou_table)
}





#' Extract OU hierarchy from OU table
#'
#' @param ou_table Organisation Unit Table
#'
#' @returns clean hierarhcy
#' @export
#'
#' @examples
#' \dontrun{
#'   get_ou_hierarchy(ou_table)
#' }
#'

get_ou_hierarchy <- function(ou_table){
    temp <- ou_table %>%
        dplyr::select(-c(name, id, full_path, level)) %>%
        tidyr::drop_na()

    return(temp)

}



