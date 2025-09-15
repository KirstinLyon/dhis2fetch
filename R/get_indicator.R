get_indicator <- function(username, password, base_url){

    response <- pull_dhis2_element(paste0(base_url, "/api/organisationUnits.json?fields=id,name,level,path,&paging=false"),
                                   username,
                                   password) %>%
        purrr::pluck("indicators")


}
