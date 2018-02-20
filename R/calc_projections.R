weighted.sd <- function(x, w, na.rm = FALSE){

  sum.w <- sum(w, na.rm = na.rm)
  sum.w2 <- sum(w^2, na.rm = na.rm)
  mean.w <- sum(x * w,na.rm = na.rm) / sum(w, na.rm = na.rm)
  x.sd.w <- sqrt((sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2))
  return(x.sd.w)
}

wilcox.loc <- function(vec, na.rm = FALSE){
  n <- length(vec)
  # If number of observations is less than 2 then we just return mean as location estimate
  if(n <= 2){
    return(mean(vec, na.rm = na.rm))
  }

  # Calculating the paired avagerages
  pairAvg <- sort(c(vec, combn(vec, 2, function(x)mean(x, na.rm = na.rm))))
  return(median(pairAvg, na.rm = na.rm))
}


cohens_d <- function(x, y, na.rm = TRUE) {
  if(na.rm){
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  n.x <- length(x)- 1
  n.y <- length(y)- 1
  mean.diff  <- abs(mean(x) - mean(y))
  if(n.x == 0 & n.y > 0) {
    common.sd <- sqrt(n.y * var(y)/n.y)
  } else if (n.x > 0 & n.y == 0){
    common.sd <- sqrt(n.x * var(x)/n.x)
  } else if (n.x > 0 & n.y  > 0) {
    common.sd <- sqrt((n.x * var(x) + n.y * var(y))/(n.x + n.y))
  } else {
    common.sd <- sd(c(x, y)) / 2
  }

  return(mean.diff/common.sd)
}

default_weights = c(CBS = 0.344, Yahoo = 0.400,  ESPN = 0.329,  NFL = 0.329,
                    FFToday = 0.379, NumberFire = 0.322, FantasyPros = 0.000,
                    FantasySharks= 0.327, FantasyFootballNerd = 0.000,
                    Walterfootball = 0.281, RTSports= 0.330,
                    FantasyData= 0.428, Fleaflicker = 0.428)


quant_funcs <- list(average = quantile, robust = quantile,
                    weighted = Hmisc::wtd.quantile)
quant_args <- list(list(probs = c(0.05, 0.95)),  list(probs = c(0.05, 0.95)),
                   list(probs = c(0.05, 0.95), type = "i/n"))

get_quant <- function(pts, wt)invoke_map(quant_funcs, quant_args, x = pts, na.rm = TRUE, weights = wt)

sd_funcs <- list(average = function(x, w, na.rm)sd(x, na.rm = na.rm),
                 robust = function(x, w, na.rm)mad(x, na.rm = na.rm),
                 weighted = weighted.sd)
sd_args <- list(list(na.rm = TRUE), list(na.rm = TRUE), list(na.rm = TRUE))
get_sd <- function(pts, wt)invoke_map(sd_funcs, sd_args, x = pts, w = wt)
projected_points <- function(data_result, scoring_rules, src_weights = NULL){

  if(is.null(src_weights)){
    data_src <- data_result %>% map(`[[`, "data_src") %>% reduce(union)
    src_weights <- default_weights[data_src]
  }

  weight_tbl <- src_weights %>% as.tibble() %>%
    `names<-`("weight") %>% rownames_to_column('data_src')

  long_result <- data_result %>% stats_by_category() %>%
    map(inner_join, weight_tbl, by = "data_src") %>%
    map(gather, "data_col", "stat_value",
        -c(id, data_src, pos, weight)) %>%
    bind_rows()

  scoring_tbl <- make_scoring_tbl(scoring_rules)

  dst_pt_allow <- scoring_rules[[c("dst", "dst_pts_allowed")]]

  dst_bracket <- is.null(dst_pt_allow) & !is.null(scoring_rules$pts_bracket)

  stat_table <- long_result %>% split(long_result$pos) %>%
    map(spread, data_col, stat_value) %>% map(select, -weight)

  dst_src <- long_result %>% slice(0) %>% add_column(points = 0)
  if(dst_bracket){
    dst_src <- long_result %>%  filter(data_col == "dst_pts_allowed") %>%
      mutate(points = ffwebscrape:::dst_points(stat_value, scoring$pts_bracket))
  }

  src_points <- long_result %>%
    inner_join(scoring_tbl, by = c("pos", "data_col")) %>%
    mutate(points = stat_value * points)  %>%
    bind_rows(dst_src) %>%
    group_by(pos, data_src, id) %>%
    summarise(points = sum(points, na.rm = TRUE)) %>% ungroup()

  conf_int <- src_points %>% inner_join(weight_tbl, by = "data_src") %>%
    split(src_points$pos) %>% map(~ split(.x, .x$id)) %>%
    modify_depth(2, ~ get_quant(.x$points, .x$weight)) %>% modify_depth(3, t) %>%
    modify_depth(3, as.tibble) %>% modify_depth(2, bind_rows, .id  = "avg_type") %>%
    modify_depth(1, bind_rows, .id = "id") %>% bind_rows(.id = "pos") %>%
    mutate(`5%` = ifelse(is.na(`5%`),` 5%`, `5%`)) %>% select(-` 5%`) %>%
    rename(floor = "5%", ceiling = "95%")

  std_dev <- src_points %>% inner_join(weight_tbl, by = "data_src") %>%
    split(src_points$pos) %>% map(~ split(.x, .x$id)) %>%
    modify_depth(2, ~ get_sd(.x$points, .x$weight)) %>% modify_depth(2, as.tibble) %>%
    modify_depth(1, bind_rows, .id = "id") %>% bind_rows(.id = "pos") %>%
    gather("avg_type", "sd_pts", -id, -pos)

  agg_stats <-  long_result %>% group_by(pos, id, data_col) %>%
    summarise(robust = wilcox.loc(stat_value, na.rm = TRUE),
              average = mean(stat_value, na.rm = TRUE ),
              weighted = weighted.mean(stat_value, w = weight, na.rm = TRUE)) %>%
    gather("avg_type", "stat_value", -c(id, pos, data_col))

  dst_agg <- dst_src %>% slice(0)

  if(dst_bracket){
    dst_agg <- agg_stats %>%  filter(data_col == "dst_pts_allowed") %>%
      mutate(points = ffwebscrape:::dst_points(stat_value, scoring$pts_bracket))
  }
  player_points <- agg_stats  %>%
    inner_join(scoring_tbl, by = c("pos", "data_col")) %>%
    mutate(points = stat_value * points) %>%
    bind_rows(dst_agg) %>%
    group_by(pos, avg_type, id) %>%
    summarise(points = sum(points, na.rm = TRUE)) %>%
    mutate(pos_rank = dense_rank(-points),
           drop_off =  points - (lead(points, order_by = pos_rank) +
                                   lead(points, 2, order_by = pos_rank)) /2 ) %>%
    ungroup() %>%
    inner_join(std_dev, by = c("pos", "id", "avg_type")) %>%
    inner_join(conf_int, by = c("pos", "id", "avg_type")) %>%
    mutate(sd_pts = ifelse(is.na(sd_pts), (ceiling - floor) / 6, sd_pts))

 return(list(stats = stat_table %>% map(inner_join, src_points, by = c("id", "pos", "data_src")),
             projected = player_points))
}

default_baseline <- c(QB = 13, RB = 35, WR = 36, TE = 13, K = 8, DST = 3, DL = 10, LB = 10, DB = 10)

set_vor <- function(points_table, vor_baseline = NULL, vor_var = c("points", "floor", "ceiling")){
  if(is.null(vor_baseline))
    vor_baseline <- default_baseline

  vor_var <- match.arg(vor_var)

  vor_tbl <- select(points_table, "id", "pos", vor_var) %>%
    rename(vor_var = !!vor_var) %>% group_by(pos) %>%
    mutate(vor_rank = dense_rank(-vor_var), vor_base = vor_baseline[pos]) %>%
    filter(vor_rank >= vor_base - 1 &  vor_rank <= vor_base + 1)  %>%
    summarise(vor_base = mean(vor_var)) %>%  ungroup() %>%
    select(pos, vor_base) %>% inner_join(points_table, by = c("pos")) %>%
    rename(vor_var = !!vor_var) %>%
    mutate(vor = vor_var - vor_base,
           rank = dense_rank(-vor), !!vor_var := vor_var) %>%
    select(id, pos, vor, rank) %>% rename_if(is.numeric, funs(paste(vor_var, ., sep = "_"))) %>%
    ungroup()

  return(vor_tbl)
}

add_vor <- function(tbl, vor_baseline = NULL){
  accumulate(c("points", "floor", "ceiling"),
             ~ inner_join(.x, set_vor(.x, vor_baseline, vor_var = .y),
                          by = c("id", "pos")),
             .init = tbl)[[4]]
}

default_threshold <-  c(QB = 1, RB = 1, WR = 1, TE = 1, K = 1, DST = 0.1, DL = 1, DB = 1, LB = 1)

set_tiers <- function(data_tbl, d_threshold = NULL, src_points){
  if(is.null(d_threshold))
    d_threshold <- default_threshold

  tier_tbl <- data_tbl %>% filter(pos %in% names(d_threshold)) %>%
    mutate(dthres = d_threshold[pos], tier = ifelse(pos_rank == 1, 1L, NA))

  repeat{
    before_na <- sum(is.na(tier_tbl$tier))
    tier_tbl <-
      tier_tbl %>% group_by(pos) %>% filter(tier == tier[which.max(tier)]) %>%
      summarise(tier_id = first(id, order_by = -points),
                cur_tier = as.integer(max(tier, na.rm = TRUE)),
                dthres= max(dthres, na.rm = TRUE)) %>%
      inner_join(tier_tbl %>% group_by(pos) %>% filter(is.na(tier)) %>%
                   summarise(max_id = first(id, order_by = -points)), by = "pos") %>%
      group_by(pos) %>%
      mutate(d_val = cohens_d(src_points[src_points$id == tier_id,]$points,
                              src_points[src_points$id == max_id,]$points),
             tier = ifelse(d_val > dthres, cur_tier + 1L, cur_tier)) %>%
      select(pos, id = max_id, new_tier = tier) %>% right_join(tier_tbl, by = c("pos", "id")) %>%
      mutate(tier = ifelse(is.na(tier) & !is.na(new_tier), new_tier, tier)) %>%
      select(-new_tier)

    after_na <- sum(is.na(tier_tbl$tier))
    if(before_na == after_na | after_na == 0)
      break
  }

  tier_tbl %>% select(-dthres) %>% ungroup()
}


