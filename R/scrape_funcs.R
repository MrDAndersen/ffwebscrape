#' @export
scrape_data <- function(src, pos, season, week){
  names(pos) <- pos
  map(pos, ~ map(projection_sources[src], ~ .x)) %>% transpose() %>%
    map( ~ imap(.x, ~ scrape_source(.x, season, week, .y))) %>%
    transpose() %>% map(discard, is.null) %>% map(bind_rows, .id = "data_src")
}

scrape_source <- function(src, season, week, position){
  src_type <- intersect(c("html_source", "json_source", "xlsx_source"), class(src))
  src_res <- switch(src_type,
                    "html_source" = src$open_session(season, week, position)$scrape(),
                    src$scrape(season, week, position))
  return(src_res)
}




