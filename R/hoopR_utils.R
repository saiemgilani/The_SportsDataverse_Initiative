clean_shot_clock_range <- function(shot_clock_range_df){
  shot_clock_range_df <- shot_clock_range_df %>% 
    dplyr::mutate(
      fg3m=as.numeric(.data$fg3m),
      fg3a=as.numeric(.data$fg3a),
      shot_clock_range = dplyr::case_when(
        .data$shot_clock_range == "24-22" ~ "22-24", 
        .data$shot_clock_range == "22-18+Very+Early" ~ "18-22", 
        .data$shot_clock_range == "18-15+Early" ~ "15-18", 
        .data$shot_clock_range == "15-7+Average" ~ "7-15", 
        .data$shot_clock_range == "7-4+Late" ~ "4-7", 
        .data$shot_clock_range == "4-0+Very+Late" ~ "0-4",))
  
  shot_clock_range_df <- shot_clock_range_df %>% 
    dplyr::filter(.data$player_id %in% fg3a_leaders$player_id)
  
  shot_clock_range_df$shot_clock_range <- as.factor(shot_clock_range_df$shot_clock_range)
  shot_clock_range_df$shot_clock_range <- factor(shot_clock_range_df$shot_clock_range, 
                                                 levels = c("22-24", "18-22", "15-18", "7-15", "4-7", "0-4"))
  return(shot_clock_range_df)
}