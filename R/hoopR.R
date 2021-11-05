source("R/hoopR_utils.R")
install.packages("ggchicklet", repos = "https://cinc.rud.is")

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
    hoopR::nba_leaguedashplayerptshot(shot_clock_range=.x, season="2020-21")$LeagueDashPTShots))
})

shot_clock_range_df <- shot_clock_range_df %>% 
  janitor::clean_names() %>% 
  dplyr::select(player_id, player_name, fg3m, fg3a, shot_clock_range)
## Clean the shot_clock_range variable ----
shot_clock_range_df <- clean_shot_clock_range(shot_clock_range_df)

## Create shot clock range FG3A frequency ----
shot_clock_range_df <- shot_clock_range_df %>% 
  dplyr::group_by(.data$player_name, .data$player_id) %>% 
  dplyr::mutate(shotclock_freq = .data$fg3a / sum(.data$fg3a)) %>% 
  dplyr::ungroup()
## Create combined text labels for the ranges -----
shot_clock_range_df <- shot_clock_range_df %>% 
  dplyr::mutate(
    shotclock_cat = dplyr::case_when(
      .data$shot_clock_range %in% c("0-4", "4-7") ~ "Late",
      .data$shot_clock_range %in% c("15-18", "7-15") ~ "Average",
      .data$shot_clock_range %in% c("22-24", "18-22") ~ "Early",
      TRUE ~ NA_character_)) %>% 
  dplyr::group_by(.data$player_name, .data$shotclock_cat) %>% 
  dplyr::mutate(sum_shotclock_cat = sum(.data$shotclock_freq)) %>% 
  dplyr::ungroup() 

#### Convert overall shot clock category to a factor ----
shot_clock_range_df$shotclock_cat <- as.factor(shot_clock_range_df$shotclock_cat)
shot_clock_range_df$shotclock_cat <- factor(shot_clock_range_df$shotclock_cat,
                                            levels = c("Late", "Average", "Early"))

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
  theme_minimal(base_size = 10) +
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
        axis.text.y = element_text(margin=margin(0,-3,0,0), size = 7)) + 
  labs(title = "Proportion of Three Point Attempts \nBy Time Remaining on the Shot Clock", 
       subtitle ="Among Top 30 in FG3A  (2020-21)", 
       fill = "Seconds Remaining on Shot Clock") 

  ggsave('figures/hoopR_chicklet.png', width = 5.0, height = 6.5, units=c("in"))