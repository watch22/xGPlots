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
  #get brazil vs belgium
  filter(match_id == 8650) %>% 
  #remove NA xG values
  mutate(shot.statsbomb_xg = ifelse(is.na(shot.statsbomb_xg), 0, shot.statsbomb_xg))

#create separate data set for total team xG
req_match_xg <- req_match %>% 
  group_by(team.name) %>% 
  summarize(tot_xg = sum(shot.statsbomb_xg) %>% signif(digits = 2)) %>% 
  mutate(team_label = glue::glue("{team.name}: {tot_xg} xG")) #glue is used to format and create string labels

req_match <- req_match %>% 
  left_join(req_match_xg, by = "team.name") %>% 
  mutate(player_label = case_when(shot.outcome.name == "Goal" ~ glue("{player.name}: {shot.statsbomb_xg %>% signif(digits = 2)} xG"),
                                  type.name == "Own Goal For" ~  paste("OG"),
    TRUE ~ ""))

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

req_match_rollsum <- req_match_rollsum %>% 
  #join with main data set to highlight goals
  left_join(req_match %>% filter(shot.outcome.name == "Goal" | type.name == "Own Goal For") %>% select(minute, shot.outcome.name, type.name, team.name, player.name),by = c("minute", "team.name")) %>% 
  #add labels and columns for goals scored
  mutate(rollsum_goal = rollsum + sumxg,
         minute_goal = minute + 1,
         player_label = case_when(
           shot.outcome.name == "Goal" ~ glue::glue("{player.name}: {sumxg %>% signif(digits = 2)} xG"),
           type.name == "Own Goal For" ~  paste("OG"),
           TRUE ~ ""))

tot_xg <- req_match_xg %>% 
  mutate(x = max(req_match_rollsum$minute)) %>% 
  select(x, tot_xg)

#3. Plot xG -------
plotfont <- "Open Sans Condensed Medium"
teamcolors <- c("Brazil" = "#FEDD00", "Belgium" = "#C8102E")

req_match_rollsumxg_plot <- req_match_rollsum %>% 
  ggplot(aes(x = minute, y = rollsum, 
             group = team.name, color = team.name)) +
  geom_line(size = 2.5) +
  geom_label_repel(data = req_match_rollsum %>% filter(shot.outcome.name == "Goal" | type.name == "Own Goal For"),
                   aes(x = minute_goal, y = rollsum_goal, 
                       color = team.name, label = player_label), 
                   nudge_x = 6, nudge_y = 0.15, family = plotfont,
                   show.legend = FALSE) +
  geom_point(data = req_match_rollsum %>% filter(shot.outcome.name == "Goal" | type.name == "Own Goal For"),
             aes(x = minute_goal, y = rollsum_goal, color = team.name), show.legend = FALSE,
             size = 5, shape = 21, fill = "white", stroke = 1.25) +
  scale_fill_manual(values = teamcolors) +
  scale_color_manual(values = teamcolors,
  c("<b style ='color:#FEDD00'>Brazil</b>", 
  "<b style='color: #C8102E'>Belgium</b>")) +
  scale_x_continuous(breaks = c(seq(0, 90, by = 15)),
                     labels = c(seq(0, 90, by = 15)),
                     expand = c(0.01, 0),
                     limits = c(0, 100)) +
  annotate(geom = "text", x = max(req_match_rollsum$minute) + 2, y = as.numeric(paste(tot_xg[1,2])), label = paste(tot_xg[1,2]), family = plotfont, size = 6, fontface = "bold", hjust = 0, angle = 0, alpha = 0.7, color = paste(teamcolors[[2]][1])) +
  annotate(geom = "text", x = max(req_match_rollsum$minute) + 2, y = as.numeric(paste(tot_xg[2,2])), label = paste(tot_xg[2,2]), family = plotfont, size = 6, fontface = "bold", hjust = 0, angle = 0, alpha = 0.7, color = paste(teamcolors[[1]][1])) +
  #using ggtext to embed HTML text formatting
  theme_minimal() +
  theme(text = element_text(family = plotfont),
        #element markdown used with ggtext
        plot.title = element_markdown(size = 35, family = plotfont),
        plot.subtitle = element_text(size = 18, family = plotfont,
                                     color = "grey20"),
        axis.title = element_text(size = 18, color = "grey20"),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_markdown(size = 16),
        legend.position = c(0.2, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())

req_match_rollsumxg_plot






