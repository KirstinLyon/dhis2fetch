#Functions for extrating the Organsational Unit table from DSHI2



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
    temp <- ou_table |>
        dplyr::select(-c(name, id, full_path, level)) |>
        tidyr::drop_na()

    return(temp)

}
use
