setwd("~/Documents/Baseball Scrimmages")
library(dplyr)
master_vt_pbp <- read.csv("vt_pbp_17-18.csv")
names(master_vt_pbp)
games = unique(master_vt_pbp$game_id)

vt_only_pbp <- data.frame()
# go through the database and get only the times tech is batting
for (game in games) {
  # create a temporary dataframe with only one game
  temp <- master_vt_pbp[master_vt_pbp$game_id == game, ]
  
  # check to see if tech is home or away
  isVThome = T
  if (temp$away_team == "Virginia Tech") {
    isVThome = F
  }
  
  # if they are home, they only play in the bottom of the inning, so set top_inning to 0.
  # if they're away, they play in the top, so 1
  VThalfinning = 0
  if (!isVThome) {
    VThalfinning = 1
  }
  
  # now create a temporary dataframe that filters out any non-tech at bats
  temp_only_tech <- temp %>%
    filter(top_inning == VThalfinning)
  # combine that previous temporary dataframe with an empty one.
  vt_only_pbp <- rbind(vt_only_pbp, temp_only_tech)
}

vt_only_pbp$RE24.init.state <- paste(paste(ifelse(vt_only_pbp$r1_name == "", 0, 1), ifelse(vt_only_pbp$r2_name == "", 0, 1), ifelse(vt_only_pbp$r3_name == "", 0, 1), sep = " "), vt_only_pbp$outs_before, sep = "   ")
vt_only_pbp$RE24.init.state
chart = aggregate(vt_only_pbp$runs_roi, by = list(vt_only_pbp$RE24.init.state), FUN = mean)

colnames(chart) <- c("Base-out state", "Expected Runs")
chart = chart[order(chart$`Base-out state`), ]
chart

write.csv(chart, "~/Documents/Baseball Scrimmages/RE24.csv")
