#' @import tidyverse httr janitor rvest glue
.onLoad <- function(libname, pkgname){
  player_table <<- httr::GET("https://www70.myfantasyleague.com/2018/export?TYPE=players&DETAILS=1&SINCE=&PLAYERS=&JSON=1") %>%
    httr::content() %>% `[[`("players") %>% `[[`("player") %>%
    purrr::map(tibble::as.tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(position %in% c("QB", "RB", "WR", "TE", "PK", "Def", "DE", "DT", "LB", "CB", "S")) %>%
    dplyr::select(id, name, position, team, weight, draft_year, draft_team, draft_round, draft_pick, birthdate)
}
