
pacman::p_load(dplyr, ggplot2, janitor, forcats,
               ggchicklet, paletteer, prismatic, scales)

# Get and filter data to top 30 players in FG3A ----
fg3a_leaders <- hoopR::nba_leaguedashplayerstats(season="2020-21")$LeagueDashPlayerStats

fg3a_leaders <- fg3a_leaders %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(fg3a=as.numeric(.data$fg3a)) %>% 
  dplyr::arrange(desc(.data$fg3a)) %>% 
  slice(1:30) %>% 
  dplyr::select(.data$player_name, .data$player_id, .data$fg3a)

# Get player shot-clock range stats-----
shot_clock_opts <- c("24-22", "22-18+Very+Early", "18-15+Early", "15-7+Average", "7-4+Late", "4-0+Very+Late")

shot_clock_range_df <- purrr::map_df(shot_clock_opts, function(.x){
  return(data.frame(shot_clock_range=.x,
    hoopR::nba_leaguedashplayerptshot(shot_clock_range=.x)$LeagueDashPTShots))
})

shot_clock_range_df <- shot_clock_range_df %>% 
  janitor::clean_names() %>% 
  dplyr::select(player_id, player_name, fg3m, fg3a, shot_clock_range)
## Clean the shot_clock_range variable ----
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
## Create shot clock range FG3A frequency ----
shot_clock_range_df <- shot_clock_range_df %>% 
  dplyr::group_by(player_name, player_id) %>% 
  dplyr::mutate(shotclock_freq = fg3a / sum(fg3a)) %>% 
  dplyr::ungroup()
## Create combined text labels for the ranges -----
shot_clock_range_df <- shot_clock_range_df %>% 
  dplyr::mutate(
    shotclock_cat = NA, 
    shotclock_cat = ifelse(.data$shot_clock_range %in% c("0-4", "4-7"), "Late", shotclock_cat), 
    shotclock_cat = ifelse(.data$shot_clock_range %in% c("15-18", "7-15"), "Average", shotclock_cat), 
    shotclock_cat = ifelse(.data$shot_clock_range %in% c("22-24", "18-22"), "Early", shotclock_cat)) %>% 
  dplyr::group_by(.data$player_name, .data$shotclock_cat) %>% 
  dplyr::mutate(sum_shotclock_cat = sum(.data$shotclock_freq)) %>% 
  dplyr::ungroup() 

#### Convert overall shot clock category to a factor ----
shot_clock_range_df$shotclock_cat <- as.factor(shot_clock_range_df$shotclock_cat)
shot_clock_range_df$shotclock_cat <- factor(shot_clock_range_df$shotclock_cat, levels = c("Late", "Average", "Early"))

### Order players by the proporition of Late 3s----
shot_clock_range_df <- shot_clock_range_df %>% 
  dplyr::arrange(.data$shotclock_cat, .data$sum_shotclock_cat) %>%              
  dplyr::mutate(player_name = factor(.data$player_name, unique(.data$player_name)))

# Plot the Data -----
shot_clock_range_df %>% 
  ggplot(aes(player_name, shotclock_freq)) + 
  geom_chicklet(aes(fill = fct_rev(shot_clock_range), color = after_scale(clr_darken(fill, 0.5))), alpha = .75) +
  scale_y_continuous(position = "left", labels = c("0%", "25%", "50%", "75%", "100%"), limits = c(0, 1)) +
  coord_flip() +
  scale_fill_manual(values = c(paletteer_d("fishualize::Hypsypops_rubicundus", direction = -1), "#172869FF")) + 
  guides(fill=guide_legend(
    keywidth= .5,
    keyheight= .2,
    default.unit="inch", 
    title.position = 'top',
    label.position = 'bottom', 
    nrow = 1) 
  ) +
  theme_minimal(base_size = 10, base_family = "Consolas") +
  theme(legend.position = 'top', 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.text = element_text(size = 7, vjust = 4),
        legend.title = element_text(size = 8, hjust = .5, vjust = -2),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,-10,0),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
        plot.title = element_text(face = 'bold', size = 10.5), 
        plot.subtitle = element_text(size = 8), 
        plot.title.position = "plot", 
        plot.margin = unit(c(.5, 1.5, 1, .5), "lines"), 
        axis.text.y = element_text(margin=margin(0,-3,0,0), size = 6)) + 
  labs(title = "Proportion Of Three Point Attempts By \nTime Remaining On The Shot Clock", 
       subtitle = paste0("Among Top 30 In FG3A  (2020-21) | Updated ", format(Sys.Date(), "%B %d, %Y")), 
       fill = "Seconds Remaining On Shot Clock") 

  ggsave('figures/hoopR_chicklet.png')