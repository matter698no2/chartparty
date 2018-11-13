setwd("~/Documents/Baseball Scrimmages")
source("NCAA-baseball-master/NCAA_baseball_pbp_parser.R")
source("NCAA-baseball-master/get_NCAA_base_schedule.R")
source("NCAA-baseball-master/get_NCAA_base_teams.R")
source("NCAA-baseball-master/get_NCAA_base_pbp.R")
source("NCAA-baseball-master/get_expected_runs_matrix.R")

# try to scrape the data, if that doesn't work, use one of the stored PBP below
vt_pbp <- get_team_pbp("Virginia Tech", 2018)
library(dplyr)
get_team_schedule("Virginia Tech", 2018)
vt_pbp_csv18 <- read.csv("NCAA-baseball-master/NCAA_baseball_pbp_2018.csv")
vt_pbp_csv17 <- read.csv("NCAA-baseball-master/NCAA_baseball_pbp_2017.csv")

totPBP <- rbind(vt_pbp_csv17, vt_pbp_csv18)
vt_pbp <- totPBP %>%
  filter(home_team == "Virginia Tech" | away_team == "Virginia Tech")

# create table structure
FBCHART <- data.frame(
  game_id = c(),
  opponent = c(),
  final_score = c(),
  game_res = c(),
  defBB = c(),
  defE = c(),
  defWP = c(),
  defPB = c(),
  defBK = c(),
  defHBP= c(),
  defSB = c(),
  defTot = c(),
  offBB = c(),
  offE = c(),
  offWP = c(),
  offPB = c(),
  offBK = c(),
  offHBP = c(),
  offSB = c(),
  offTot = c(),
  fbDiff = c(),
  fbRes = c()
)

# main loop
# take every unique game id and iterate through its play by play data
# 1. check if vt is home or away
# 2. tally up the various Freebase events
# 3. put the tallies in the table
# 4. add the results and totals
# 5. ???
# 6. Profit

gameIds = unique(vt_pbp$game_id)

# define the free bases outside any loops
freeBases <- c("error", "passed ball", "wild pitch", "walk", "hit by pitch", "balk", "stole")


for (game in gameIds) {
  # make a temporary isolated df of just the current game
  temp <- vt_pbp[vt_pbp$game_id == game, ]
  
  # get if tech is the home team or not
  isTechHome = T
  oppos = paste(toString(temp$away_team[1]))
  if (temp$away_team[1] == "Virginia Tech") {
    isTechHome = F
    oppos = paste(toString(temp$home_team[1]))
  }
  
  
  
  # if tech is home:
  if (isTechHome) {
    finalScore = paste(toString(temp$home_score[length(temp$away_score)]) , "-", toString(temp$away_score[length(temp$away_score)]))
    
    gameRES = "W"
    if(temp$home_score[length(temp$away_score)] < temp$away_score[length(temp$away_score)]) {
      gameRES = "L"
    }
    
    vtFB = sum(grepl(paste(freeBases, collapse = "|"), temp$home_text) == T)
    oppFB = sum(grepl(paste(freeBases, collapse = "|"), temp$away_text) == T)
    diffFB = vtFB - oppFB
    
    # pitching free bases
    pitchBB = sum(grepl("walk", temp$away_text) == T)
    pitchE = sum(grepl("error", temp$away_text) == T)
    pitchWP = sum(grepl("wild pitch", temp$away_text) == T)
    pitchPB = sum(grepl("passed ball", temp$away_text) == T)
    pitchBK = sum(grepl("balk", temp$away_text) == T)
    pitchHBP = sum(grepl("hit by pitch", temp$away_text) == T)
    pitchSB = sum(grepl("stole", temp$away_text) == T)
    
    pitchTot = pitchBB + pitchE + pitchWP + pitchPB + pitchBK + pitchHBP + pitchSB
    
    # offense free bases
    batterBB = sum(grepl("walk", temp$home_text) == T)
    batterE = sum(grepl("error", temp$home_text) == T)
    batterWP = sum(grepl("wild pitch", temp$home_text) == T)
    batterPB = sum(grepl("passed ball", temp$home_text) == T)
    batterBK = sum(grepl("balk", temp$home_text) == T)
    batterHBP = sum(grepl("hit by pitch", temp$home_text) == T)
    batterSB = sum(grepl("stole", temp$home_text) == T)
    
    batterTot = batterBB + batterE + batterWP + batterPB + batterBK + batterHBP + batterSB
    
  }
  # if tech is away:
  else if (!isTechHome) {
    finalScore = paste(toString(temp$away_score[length(temp$away_score)]) , "-", toString(temp$home_score[length(temp$away_score)]))
    
    gameRES = "W"
    if(temp$home_score[length(temp$away_score)] > temp$away_score[length(temp$away_score)]) {
      gameRES = "L"
    }
    
    
    vtFB = sum(grepl(paste(freeBases, collapse = "|"), temp$away_text) == T)
    oppFB = sum(grepl(paste(freeBases, collapse = "|"), temp$home_text) == T)
    diffFB = vtFB - oppFB
    
    # pitching free bases
    pitchBB = sum(grepl("walk", temp$home_text) == T)
    pitchE = sum(grepl("error", temp$home_text) == T)
    pitchWP = sum(grepl("wild pitch", temp$home_text) == T)
    pitchPB = sum(grepl("passed ball", temp$home_text) == T)
    pitchBK = sum(grepl("balk", temp$home_text) == T)
    pitchHBP = sum(grepl("hit by pitch", temp$home_text) == T)
    pitchSB = sum(grepl("stole", temp$home_text) == T)
    
    pitchTot = pitchBB + pitchE + pitchWP + pitchPB + pitchBK + pitchHBP + pitchSB
    
    # offense free bases
    batterBB = sum(grepl("walk", temp$away_text) == T)
    batterE = sum(grepl("error", temp$away_text) == T)
    batterWP = sum(grepl("wild pitch", temp$away_text) == T)
    batterPB = sum(grepl("passed ball", temp$away_text) == T)
    batterBK = sum(grepl("balk", temp$away_text) == T)
    batterHBP = sum(grepl("hit by pitch", temp$away_text) == T)
    batterSB = sum(grepl("stole", temp$away_text) == T)
    
    batterTot = batterBB + batterE + batterWP + batterPB + batterBK + batterHBP + batterSB
    
  }
  
  if (diffFB > 0) {
    FBbattle = "W"
  }
  else {
    FBbattle = "L"
  }
  
  # create a temp data frame to bind to the complete one
  tempFBCHART <- data.frame(
    game_id = c(game),
    opponent = c(oppos),
    final_score = c(finalScore),
    game_res = c(gameRES),
    defBB = c(pitchBB),
    defE = c(pitchE),
    defWP = c(pitchWP),
    defPB = c(pitchPB),
    defBK = c(pitchBK),
    defHBP= c(pitchHBP),
    defSB = c(pitchSB),
    defTot = c(pitchTot),
    offBB = c(batterBB),
    offE = c(batterE),
    offWP = c(batterWP),
    offPB = c(batterPB),
    offBK = c(batterBK),
    offHBP = c(batterHBP),
    offSB = c(batterSB),
    offTot = c(batterTot),
    fbDiff = c(diffFB),
    fbRes = c(FBbattle)
  )
  
  FBCHART <- rbind(FBCHART, tempFBCHART)
  
}
FBCHART

# write the data to a csv file
write.csv(FBCHART, "~/Documents/Baseball Scrimmages/NCAA-baseball-master/freeBases17-18.csv")

# now break the data down with simple analysis
fbSurrendered <- sort(unique(FBCHART$defTot))

fbLevelWins <- data.frame(
  FBlevel = c(),
  winPct = c(),
  n = c()
)

for (fb in fbSurrendered) {
  # get the dataset with the games in each level
  temp <- FBCHART[FBCHART$defTot == fb, ]
  dubyas <- nrow(temp[temp$game_res == "W", ]) # the number of wins at the fb level
  dubyaPct <- format(round(dubyas/nrow(temp), 3), nsmall = 3) # the winning percentage at the fb level
  
  # add all the data to a dataframe
  tempDF = data.frame(
    FBlevel = c(fb),
    winPct = c(dubyaPct),
    n = c(nrow(temp))
  )
  
  # append it all
  fbLevelWins <- rbind(fbLevelWins, tempDF)
}

# write it to a csv
write.csv(fbLevelWins, "~/Documents/Baseball Scrimmages/NCAA-baseball-master/freeBaseWinPCT.csv")