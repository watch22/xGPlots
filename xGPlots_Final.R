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
  #get final
  filter(match_id == wc18[which(wc18$match_date == max(wc18$match_date)),1][[1]]) %>% 
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
req_match_rollsum_join <- req_match_rollsum %>%
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
xg_plot_export <- req_match_rollsum_join %>% 
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
  rename(Croatia_xG = rollsum_goal_Croatia,
         France_xG = rollsum_goal_France,
         Croatia_Goal = goal_flag_Croatia,
         France_Goal = goal_flag_France) %>% 
  mutate(Croatia_xG = as.numeric(Croatia_xG),
         France_xG = as.numeric(France_xG),
         Croatia_Goal = as.numeric(Croatia_Goal),
         France_Goal = as.numeric(France_Goal))



write_csv(xg_plot_export, "/Users/SikSik/Documents/Data Science/Datasets/xGPlot_CroatiavsFrance.csv")

#3. Plot xG -------

#3a. Set fonts and colours ----

plotfont <- "Open Sans Condensed Medium"
teamcolors <- c("France" = "#17548C", "Croatia" = "#ED1C24")
teamfills <- c("France" = "white", "Croatia" = "#DFB770")
bgcol <- "#F5EED5"
textcol <- "#171714"
linecol <- "#171714"

#3b. plot ----
req_match_rollsumxg_plot <- req_match_rollsum_join %>% 
  ggplot(aes(x = minute, y = rollsum_goal, 
             group = team.name, color = team.name, label = player_label)) +
  geom_line(size = 2.5, show.legend = FALSE) +
  geom_text(aes(label = stringr::str_wrap(player_label, 30)),
            nudge_x = 0, nudge_y = 0.05, 
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
        plot.caption = element_blank()) +
  labs(title = "<span style='color:#17548C;'>France</span>
    4 - 2
    <span style='color:#ED1C24;'>Croatia</span>
    </span>",
       subtitle = paste(wc18$competition.competition_name, sep = " ", paste("(",format(as.Date(max(wc18$match_date)),"%d/%m/%Y"),")", sep = "")),
       x="mins", y="ExpG", caption = "Event data courtesy of StatsBomb")

req_match_rollsumxg_plot

#EXPORT PLOTS -----
ggsave(plot = req_match_rollsumxg_plot,
       filename = here("outputs/France_vs_Croatia_xGPlot.png"),
       height = 8, width = 14)

#add W22 logo
plot_watch22_logo <- add_logo(
  plot_path = here("outputs/France_vs_Croatia_xGPlot.png"),
  logo_path = ("/Users/SikSik/Pictures/Watch22/WATCH22_CIRCLE.png"),
  logo_position = "top right",
  logo_scale = 17)

plot_watch22_logo

magick::image_write(
  image = plot_watch22_logo, 
  path = here::here("outputs/France_vs_Croatia_xGPlot_W22.png"))

#add StatsBomb logo

statsbomb_logo <- add_logo(
  plot_path = here::here("outputs/France_vs_Croatia_xGPlot_W22.png"),
  logo_path = ("/Users/SikSik/Documents/Data Science/StatsBomb/resources/SB - Icon Lockup - Colour positive.png"),
  logo_position = "bottom right",
  logo_scale = 8)

statsbomb_logo

magick::image_write(
  image = statsbomb_logo, 
  path = here::here("outputs/France_vs_Croatia_xGPlot_FINAL.png"))


