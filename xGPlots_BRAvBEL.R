######## xG Plots - From Football Hackers ########
#1. Load Packages and Functions -----------------
pacman::p_load(tidyverse, StatsBombR, SBpitch, soccermatics, extrafont, ggupset, tibbletime, ggtext, ggrepel, glue, patchwork, cowplot, gtable, grid, magick, here, ggsoccer, janitor, rvest)

#Functions -
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.001 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}

#END------
#2. Import World Cup 2018 Data -----------------
comps <- FreeCompetitions()
wc18 <- comps %>% 
  filter(competition_id == 43) %>% 
  FreeMatches()
wc18_events <- StatsBombFreeEvents(MatchesDF = wc18)
wc18_events <- allclean(wc18_events)
wc18_events <- wc18_events %>% 
  left_join(select(comps, c(season_id, season_name)), by = "season_id") %>% 
  distinct()

wc18_events <- wc18_events %>% 
  left_join(select(wc18, c(match_id, home_team.home_team_name, away_team.away_team_name)), by = 'match_id')

#3. Filter to match ---------
req_match <- wc18_events %>% 
  #get brazil vs belgium
  filter(match_id == 8650) %>% 
  #remove NA xG values
  mutate(shot.statsbomb_xg = ifelse(is.na(shot.statsbomb_xg), 0, shot.statsbomb_xg))

#create separate data set for total team xG
req_match_xg <- req_match %>%
  group_by(team.name) %>%
  summarize(tot_xg = sum(shot.statsbomb_xg) %>% signif(digits = 2)) %>%
  mutate(team_label = glue::glue("{team.name}: {tot_xg} xG"))

#GET MATCHLENGTH AND TEAM NAMES TO JOIN WITH CROSSING DATA
minute <- c(0:max(req_match$minute))
team.name <-  unique(req_match$team.name)

#get accumulated xG
req_match_rollsum <- req_match %>% 
  group_by(minute, team.name, period) %>% 
  summarize(sumxg = sum(shot.statsbomb_xg)) %>% 
  ungroup() %>% 
  group_by(team.name) %>% 
  mutate(rollsum = lag(cumsum(sumxg)),
         #this line ensures that the first row is 0
         rollsum = if_else(is.na(rollsum), 0, rollsum)) %>% 
  select(team.name, minute, rollsum, sumxg) %>%
  #add sumxg to rollsum when an xG event happens
  mutate(rollsum = case_when(
    row_number() == n() & sumxg != 0 ~ rollsum + sumxg,
    TRUE ~ rollsum
  ))

#Bring rollsum, goal and crossing data together
req_match_rollsum_join <- req_match_rollsum %>% full_join(crossing(minute, team.name), by = c("minute", "team.name")) %>% 
  #join with main data set to highlight goals
  left_join(req_match %>% filter(shot.outcome.name == "Goal" | type.name == "Own Goal For") %>% select(minute, shot.outcome.name, type.name, team.name, player.name),by = c("minute", "team.name")) %>% 
  #add labels and columns for goals scored
  mutate(rollsum_goal = rollsum + sumxg,
         minute_goal = minute + 1,
         player_label = case_when(
           shot.outcome.name == "Goal" ~ glue::glue("{player.name}: {sumxg %>% signif(digits = 2)} xG"),
           type.name == "Own Goal For" ~  paste("OG"),
           TRUE ~ ""),
         goal_flag = case_when(
           shot.outcome.name == "Goal" ~ 1,
           type.name == "Own Goal For" ~  1,
           TRUE ~ 0)) %>% 
  arrange(team.name, minute) %>% 
  select(1,2,8:11) %>% 
  mutate(rollsum_goal = case_when(is.na(rollsum_goal) ~ lag(rollsum_goal),
                                  TRUE ~ rollsum_goal)) %>% 
  arrange(minute) %>% distinct()


#create version for sonification -------
xg_plot_export <- req_match_rollsum %>% 
  select(team.name, minute, rollsum_goal, goal_flag) %>% 
  pivot_wider(names_from = team.name, values_from = c(rollsum_goal, goal_flag))



#DELIST ENTRIES WITH LISTS
for(z in 2: ncol(xg_plot_export)){
  for (i in 1:nrow(xg_plot_export)){
    xg_plot_export[[i,z]][1] <-  ifelse(is.list(xg_plot_export[i,z]),
                                                        ifelse(!is.na(unlist(xg_plot_export[i,z])[1]),
                                                               unlist(xg_plot_export[i,z])[1],
                                                               unlist(xg_plot_export[i,z])[2]), 
                                                        unlist(xg_plot_export[i,z])[1])
  }
}

xg_plot_export <- xg_plot_export %>% 
  rename(Belgium_xG = rollsum_goal_Belgium,
         Brazil_xG = rollsum_goal_Brazil,
         Belgium_Goal = goal_flag_Belgium,
         Brazil_Goal = goal_flag_Brazil) %>% 
  mutate(Belgium_xG = as.numeric(Belgium_xG),
         Brazil_xG = as.numeric(Brazil_xG),
         Belgium_Goal = as.numeric(Belgium_Goal),
         Brazil_Goal = as.numeric(Brazil_Goal))


  #Scale to 0-5v range based on max values
xg_plot_export <- xg_plot_export %>% 
  mutate(Belgium_xG_scaled = 5*(Belgium_xG /(max(Belgium_xG,Brazil_xG))),
         Brazil_xG_scaled = 5*(Brazil_xG /(max(Belgium_xG,Brazil_xG))))



write_csv(xg_plot_export, "/Users/SikSik/Documents/Data Science/Datasets/xGPlot_BrazilvBelgium.csv")

#3. Plot xG -------

#3a. Set fonts and colours ----

plotfont <- "Open Sans Condensed Medium"
teamcolors <- c("Brazil" = "#FEDD00", "Belgium" = "#C8102E")
teamfills <- c("Brazil" = "#009739", "Belgium" = "#FFCD00")
bgcol <- "#171714"
textcol <- "#F5EED5"
linecol <- "#F5EED5"

#3b. plot ----
req_match_rollsumxg_plot <- req_match_rollsum_join %>% 
  ggplot(aes(x = minute, y = rollsum_goal, 
             group = team.name, color = team.name, label = player_label)) +
  geom_line(size = 2.5, show.legend = FALSE) +
  geom_text(aes(label = stringr::str_wrap(player_label, 25)),
                  nudge_x = 0, nudge_y = 0.15, 
                  family = plotfont,
                  angle = 90,
                  show.legend = FALSE,
                  colour = textcol,
            hjust = 0) +
  geom_point(data = req_match_rollsum_join %>% filter(goal_flag == 1),
             aes(x = minute, y = rollsum_goal, color = team.name, fill = team.name), show.legend = FALSE,
             size = 5, shape = 21, stroke = 1.25) +
  annotate(geom = "text", x = max(req_match_rollsum_join$minute) + 1, y = req_match_xg$tot_xg[1], label = req_match_xg$tot_xg[1], family = plotfont, size = 6, fontface = "bold", hjust = 0, angle = 0, alpha = 0.7, color = paste(teamcolors[[2]][1])) +
  annotate(geom = "text", x = max(req_match_rollsum_join$minute) + 1, y = req_match_xg$tot_xg[2], label = req_match_xg$tot_xg[2], family = plotfont, size = 6, fontface = "bold", hjust = 0, angle = 0, alpha = 0.7, color = paste(teamcolors[[1]][1])) +
  scale_fill_manual(values = teamfills) +
  scale_color_manual(values = teamcolors) +
  scale_x_continuous(breaks = c(seq(0, 90, by = 15)),
                     labels = c(seq(0, 90, by = 15)),
                     expand = c(0.05, 0),
                     limits = c(0, 100)) +
  scale_y_continuous(breaks = c(seq(0, 2.5, by = 0.5)),
                     labels = c(seq(0, 2.5, by = 0.5)),
                     expand = c(0.1, 0.1))+
  theme_minimal() +
  theme(text = element_text(family = plotfont, colour = textcol),
        plot.title = element_markdown(size = 24, family = plotfont, lineheight = 1.1),
        plot.subtitle = element_text(size = 18, family = plotfont,
                                     color = textcol),
        axis.title = element_text(size = 18, color = textcol),
        axis.text = element_text(size = 16, face = "bold", colour = textcol),
        axis.ticks = element_line(colour = linecol, size = 1),
        axis.ticks.length = unit(.15, "cm"),
        axis.line = element_line(colour = linecol),
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = bgcol, fill = bgcol),
        plot.background = element_rect(colour = bgcol, fill = bgcol),
        plot.caption = element_text(colour = textcol, size = 12)) +
  labs(title = "<span style='color:#FEDD00;'>Brazil</span>
    1 - 2
    <span style='color:#C8102E;'>Belgium</span>
    </span>",
       subtitle = paste(wc18$competition.competition_name, "(", format(as.Date(wc18$match_date[wc18$match_id == 8650]),"%d/%m/%Y"),")"),
       x="mins", y="ExpG", caption = "Event data courtesy of StatsBomb")

req_match_rollsumxg_plot


