pacman::p_load_current_gh("saiemgilani/wehoop")

## **Quick Start** ------

### **WNBA play-by-play ** -------------
future::plan("multisession")
tictoc::tic()
progressr::with_progress({
  wnba_pbp <- wehoop::load_wnba_pbp(2002:2021)
})
tictoc::toc()
## 13.91 sec elapsed
glue::glue("WNBA play-by-play data from {length(unique(wnba_pbp$game_id))} games.")
## WNBA play-by-play data from 4674 games.
dplyr::glimpse(wnba_pbp)

### **WNBA team box scores**------
future::plan("multisession")
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(2002:2021)
})
tictoc::toc()
glue::glue("WNBA team boxscore data from {length(unique(wnba_team_box$game_id))} games.")

### **WNBA full player box scores**
future::plan("multisession")
tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(2002:2021)
})
tictoc::toc()
glue::glue("WNBA player boxscore data from {length(unique(wnba_player_box$game_id))} games.")

###---- WBB Trimmed for length ---------- 
### **Women's college basketball full play-by-play seasons (2002-2021) ~ 45-90 seconds**
future::plan("multisession")
tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp(2002:2021)
})
tictoc::toc()
glue::glue("WBB play-by-play data from {length(unique(wbb_pbp$game_id))} games.")
dplyr::glimpse(wbb_pbp[1:53])
### **Women's college basketball full team box score seasons (2002-2021) ~ 5-30 seconds**
future::plan("multisession")
tictoc::tic()
progressr::with_progress({
  wbb_team_box <- wehoop::load_wbb_team_box(2002:2021)
})
tictoc::toc()
glue::glue("WBB team boxscore data from {length(unique(wbb_team_box$game_id))} games.")


### **Women's college basketball full player box score seasons (2002-2021) ~ 5-30 seconds**

future::plan("multisession")
tictoc::tic()
progressr::with_progress({
  wbb_player_box <- wehoop::load_wbb_player_box(2002:2021)
})
tictoc::toc()
glue::glue("WBB player boxscore data from {length(unique(wbb_player_box$game_id))} games.")
