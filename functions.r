# --- WASTELAND OF VARIOUS FUNCTIONS --- #

# PA counter
# Returns the number of AB's recorded in
# a dataset
pa <- function(inDat) {
  numPa = 0
  for (pitch in inDat$PitchofPA) {
    if (pitch == 1) {
      numPa = numPa + 1
    }
  }
  return(numPa)
}

# gets the total number of ABs
# AB != BB, HBP, Sacrifice
ab <- function(inDat) {
  pas = pa(inDat)
  
  bbs = sum(inDat$KorBB == "Walk")
  hbp = sum(inDat$PitchCall == "HitByPitch")
  sac = sum(inDat$PlayResult == "Sacrifice")
  
  return(pas - (bbs + hbp + sac))
}

# returns the number of hits a player has had
hits <- function(inDat) {
  return(sum(inDat$PitchCall == "InPlay" & inDat$OutsOnPlay == 0))
}

# Batting AVG calculator
AVG <- function(inDat) {
  return(format(round(hits(inDat)/ab(inDat), 3), nsmall = 3))
}

# OBP calculator
OBP <- function(inDat) {
  plateApps = pa(inDat)
  hits = sum(inDat$PlayResult != "Undefined" & inDat$OutsOnPlay == 0)
  bbs = sum(inDat$KorBB == "Walk")
  
  onBase = hits + bbs
  
  return(format(round(onBase/plateApps, 3), nsmall = 3))
}

# SLG calc
SLG <- function(inDat) {
  abs = ab(inDat)
  singles = sum(inDat$PlayResult == "Single")
  doubles = sum(inDat$PlayResult == "Double") * 2
  triples = sum(inDat$PlayResult == "Triple") * 3
  dingers = sum(inDat$PlayResult == "Home Run") * 4 
  
  onBasePercent = (singles + doubles + triples + dingers) / abs
  return(format(round(onBasePercent, 3), nsmall = 3))
}

# HR% calc
hrPercent <- function(inDat) {
  hrs = sum(inDat$PlayResult == "HomeRun")
  totIP = sum(inDat$PlayResult != "Undefined")
  
  hrPerc = hrs/totIP
  return(format(round(hrPerc, 3), nsmall = 3))
}

# true ball/strike calc
# checks a range of x & y values to determine
# if the pitch was a ball of strike in the coordinate plane
isStrikeX <- function(pitch) {
  if (is.na(pitch)){
    return(FALSE)
  }
  else if ((pitch >= -0.7803) & (pitch <= 0.7803)) {
    return(TRUE)
  }
  return(FALSE)
}

isStrikeY <- function(pitch) {
  if (is.na(pitch)){
    return(FALSE)
  }
  else if (pitch >= 1.5 & pitch <= 3.5) {
    return(TRUE)
  }
  return(FALSE)
}

# takes a set of x and y coordinates, and checks
# to see if they fit within the designated range
# of the simmed strikezone
isStrike <- function(X, Y) {
  if (isStrikeX(X) & isStrikeY(Y)) {
    return(TRUE)
  }
  return(FALSE)
}

# takes two lists of x and y coordinates and 
# iterates through them to determine if they are both strikes
isStrikeCounter <- function(X, Y) {
  rowIndex = 1
  numStrikes = 0
  while(rowIndex < (length(X) + 1 )) {
    if(isTRUE(isStrike(X[rowIndex], Y[rowIndex]))){
      numStrikes = numStrikes + 1
    }
    rowIndex = rowIndex + 1
  }
  return(numStrikes)
}

# o-swing % calculator
# o-swing is balls swung at / total balls
oSwing <- function(inDat) {
  swingRes <- c("InPlay", "StrikeSwinging", "FoulBall")
  swingBalls = 0
  totBalls = 0
  
  X = inDat$PlateLocSide
  Y = inDat$PlateLocHeight
  res = inDat$PitchCall
  rowIndex = 1
  
  for (num in X) {
    if (!isStrike(X[rowIndex], Y[rowIndex])){
      totBalls = totBalls + 1
      if (res[rowIndex] %in% swingRes) {
        swingBalls = swingBalls + 1
      }
    }
    rowIndex = rowIndex + 1
  }
  
  oswing = (swingBalls / totBalls) * 100

  return(format(round(oswing, 4), nsmall = 3))
}
# z-swing % calculator
# z-swing is strikes swung at / total strikes
zSwing <- function(inDat) {
  swingRes <- c("InPlay", "StrikeSwinging", "FoulBall")
  swingStrike = 0
  totStrike = 0
  
  X = inDat$PlateLocSide
  Y = inDat$PlateLocHeight
  res = inDat$PitchCall
  rowIndex = 1
  
  # for the entire length of pitches, check if the 
  # pitch is a strike, and if it is, check to see if 
  # the batter swung at it. 
  for (num in X) {
    if (isStrike(X[rowIndex], Y[rowIndex])){
      totStrike = totStrike + 1
      if (res[rowIndex] %in% swingRes) {
        swingStrike = swingStrike + 1
      }
    }
    rowIndex = rowIndex + 1
  }
  
  zswing = (swingStrike / totStrike) * 100
  
  return(format(round(zswing, 3), nsmall = 3))
}

# z-contact% calculator
zContact <- function(inDat) {
  swingRes <- c("InPlay", "StrikeSwinging", "FoulBall")
  contRes <- c("InPlay", "FoulBall")
  swingStrike = 0
  contStrike = 0
  
  X = inDat$PlateLocSide
  Y = inDat$PlateLocHeight
  res = inDat$PitchCall
  
  rowIndex = 1
  
  for (num in X) {
    if (isStrike(X[rowIndex], Y[rowIndex]) & (res[rowIndex] %in% swingRes)){
      swingStrike = swingStrike + 1
      if (res[rowIndex] %in% contRes) {
        contStrike = contStrike + 1
      }
    }
    rowIndex = rowIndex + 1
  }
  
  return(format(round((contStrike/swingStrike) * 100, 3), nsmall = 3))
  
}

#oContact% calculator
oContact <- function(inDat) {
  swingRes <- c("InPlay", "StrikeSwinging", "FoulBall")
  contRes <- c("InPlay", "FoulBall")
  swingBall = 0
  contBall = 0
  
  X = inDat$PlateLocSide
  Y = inDat$PlateLocHeight
  res = inDat$PitchCall
  
  rowIndex = 1
  
  for (num in X) {
    if (!isStrike(X[rowIndex], Y[rowIndex]) & (res[rowIndex] %in% swingRes)){
      swingBall = swingBall + 1
      if (res[rowIndex] %in% contRes) {
        contBall = contBall + 1
      }
    }
    rowIndex = rowIndex + 1
  }
  
  return(format(round((contBall/swingBall) * 100, 3), nsmall = 3))
}

# Launch Angle Table Maker
laTable <- function(inDat) {
  # make an empty data frame
  LAdf <- data.frame()
  
  # get the different factor levels for a simpler loop time
  cats <- list("< 10", "10-20 (w/ 10)", "20-30 (w/ 20)", "> 30")
  for (cat in cats) {
    loopDF <- inDat %>%
      filter(AngleCats == cat & PitchCall == "InPlay")
    
    hits = sum(loopDF$PlayResult != "Out")
    bAVG = format(round(hits/nrow(loopDF), 3), nsmall = 3)
    hrPct = sum(loopDF$PlayResult == "HomeRun") / nrow(loopDF)
    
    slgPct = sum(loopDF$PlayResult == "Single") + (2 * sum(loopDF$PlayResult == "Double")) + (3 * sum(loopDF$PlayResult == "Triple")) + (4 * sum(loopDF$PlayResult == "HomeRun"))
    
    
    tempDF <- data.frame(
      launchAngles = c(cat),
      n = c(nrow(loopDF)),
      batAvg = c(bAVG),
      slugging = c(slgPct),
      avgExitVelo = c(mean(loopDF$ExitSpeed)),
      hrPct = c(format(round(hrPct, 3), nsmall=3))
    )
    
    LAdf <- rbind(LAdf, tempDF)
    
  }
  colnames(LAdf) <- c("LA", "n", "AVG", "SLG", "avg EV", "HR%")
  
  return(as.data.frame(LAdf))
}

#exit velo table
evTable <- function(inDat) {
  EVdf <- data.frame()
  
  cats <- c("< 75", "75-85", "85-95", "95-105", "> 105")
  for (cat in cats) {
    loopDF <- inDat %>%
      filter(inDat$exitVeloCats == cat & PitchCall == "InPlay")
    
    hits = sum(loopDF$PlayResult != "Out")
    bAVG = format(round(hits/nrow(loopDF), 3), nsmall = 3)
    hrPct = sum(loopDF$PlayResult == "HomeRun") / nrow(loopDF)
    
    slgPct = sum(loopDF$PlayResult == "Single") + (2 * sum(loopDF$PlayResult == "Double")) + (3 * sum(loopDF$PlayResult == "Triple")) + (4 * sum(loopDF$PlayResult == "HomeRun"))
    
    tempDF <- data.frame(
      exitVelo = c(cat),
      n = c(nrow(loopDF)),
      batAvg = c(bAVG),
      slugging = c(slgPct),
      avgLaunchAngle = c(mean(loopDF$Angle)),
      hrPct = c(hrPercent(loopDF))
    )
    
    EVdf <- rbind(EVdf, tempDF)
  }
  
  colnames(EVdf) <- c("EV", "n", "AVG", "SLG", "avg LA", "HR%")
  
  return(EVdf)
}


# K% calculator
kPct <- function(inDat) {
  pApps = pa(inDat)
  ks = sum(inDat$KorBB == "Strikeout")
  
  return(format(round(((ks/pApps) * 100), 3), nsmall = 3))
}

# BB% calculator
bbPct <- function(inDat) {
  pApps = pa(inDat)
  bbs = sum(inDat$KorBB == "Walk")
  
  return(format(round(((bbs/pApps) * 100), 3), nsmall = 3))
}

# swing and miss calculator (FB)
swingWMissFB <- function(inDat) {
  # filter the data to only fastballs
  fbDAT <- inDat %>%
    filter(TaggedPitchType == "Fastball")
  
  swing = c("InPlay", "StrikeSwinging", "FoulBall")
  
  numSwings = sum(fbDAT$PitchCall %in% swing)
  numMiss = sum(fbDAT$PitchCall == "StrikeSwinging")
  
  pct = numMiss/numSwings
  
  return(format(round(pct * 100, 3), nsmall = 3))
}

# swing and miss calculator (offspd)
swingWMissOffSpd <- function(inDat) {
  # filter the data to erase fastballs
  fbDAT <- inDat %>%
    filter(TaggedPitchType != "Fastball")
  
  swing = c("InPlay", "StrikeSwinging", "FoulBall")
  
  numSwings = sum(fbDAT$PitchCall %in% swing)
  numMiss = sum(fbDAT$PitchCall == "StrikeSwinging")
  
  pct = numMiss/numSwings
  
  return(format(round(pct * 100, 3), nsmall = 3))
}

# quality ab% calculator
# metrics:
# - Hard Hit Ball
# - XBH
# - ABs with 6 or more pitches
# - ABs where the batter sees 3 pitches after getting 2 strikes
# - Sacrifice fly/bunt
# - Walks
qAB <- function(inDat) {
  # first get all the at bats
  totAB = ab(inDat)
  
  # next sum the various metrics needed
  xbhVals <- c("Double", "Triple", "HomeRun")
  xbh = sum(inDat$PlayResult %in% xbhVals)
  
  rowIndex = 1
  pas6 = 0
  while (rowIndex <= nrow(inDat)) {
    if (inDat$PitchofPA[rowIndex] == 6) {
      pas6 = pas6 + 1
    }
    
    rowIndex = rowIndex + 1
  }
  
  
}

# makes a table of various batter's stats
batterTabGen <- function(inDat) {
  teamBatting <- data.frame(
    batterName = c("Team AVG"),
    plateApps = c(pa(inDat)),
    atBats = c(ab(inDat)),
    bAVG = c(AVG(inDat)),
    onBasePct = c(OBP(inDat)),
    slugPct = c(SLG(inDat)),
    kPercent = c(kPct(inDat)),
    bbPercent = c(bbPct(inDat)),
    baip = c(BABIP(inDat)),
    oswingPct = c(oSwing(inDat)),
    oContactPct = c(oContact(inDat)),
    zswingPct = c(zSwing(inDat)),
    zContactPct = c(zContact(inDat)),
    weightedOB = c(wOBA(inDat))
  )
  batters <- levels(inDat$Batter)
  
  for (batter in batters) {
    batterDS <- inDat %>%
      filter(Batter == batter)
    
    tempDF <- data.frame(
      batterName = c(batter),
      plateApps = c(pa(batterDS)),
      atBats = c(ab(batterDS)),
      bAVG = c(AVG(batterDS)),
      onBasePct = c(OBP(batterDS)),
      slugPct = c(SLG(batterDS)),
      kPercent = c(kPct(batterDS)),
      bbPercent = c(bbPct(batterDS)),
      baip = c(BABIP(batterDS)),
      oswingPct = c(oSwing(batterDS)),
      oContactPct = c(oContact(batterDS)),
      zswingPct = c(zSwing(batterDS)),
      zContactPct = c(zContact(batterDS)),
      weightedOB = c(wOBA(batterDS))
    )
    
    teamBatting <- rbind(teamBatting, tempDF)
  }
  
  colnames(teamBatting) <- c("Batter", "pa", "ab", "avg", "obp", "slg", "K%", "BB%", "BABIP", "oSwing%", "oContact%", "zSwing%", "zContact%", "wOBA")
  
  return(teamBatting)
}

# BABIP calculator
# (H - HR) / (AB - K - HR + SF)
#
BABIP <- function(inDat) {
  # filter out sac bunts
  inDat1 <- inDat %>%
    filter(inDat$HitType != "Bunt" & inDat$PlayResult != "Sacrifice")
  
  h = hits(inDat1)
  hr = sum(inDat1$PlayResult == "HomeRun")
  abs = ab(inDat1)
  k = sum(inDat1$KorBB == "Strikeout")
  sf = sum(inDat1$PlayResult == "Sacrifice")
  
  x = (h - hr) / (abs - k - hr + sf)
  
  return(format(round(x, 3), nsmall = 3))
  
}

# FB/Offspd chart for batters
elbinChart <- function(inDat) {
  swing = c("InPlay", "StrikeSwinging", "FoulBall")
  
  fbDF <- inDat %>%
    filter(TaggedPitchType == "Fastball")
  offSpdDF <- inDat %>%
    filter(TaggedPitchType != "Fastball")
  
  elbinDF <- data.frame(
    batterName = c("Team AVGs"),
    fbAVG = c(AVG(fbDF)),
    fbOBP = c(OBP(fbDF)),
    fbSLG = c(SLG(fbDF)),
    fbSWmiss = c(swingWMissFB(inDat)),
    fbTotalSwings = c(sum(fbDF$PitchCall %in% swing)),
    fbTotal = c(nrow(fbDF)),
    offSpdAVG = c(AVG(offSpdDF)),
    offSpdOBP = c(OBP(offSpdDF)),
    offSpdSLG = c(SLG(offSpdDF)),
    offSpdswingWMiss = c(swingWMissOffSpd(inDat)),
    osTotalSwings = c(sum(offSpdDF$PitchCall %in% swing)),
    osTotal = c(nrow(offSpdDF))
  )
  
  batters = levels(inDat$Batter)
  for (guy in batters) {
    guyFB <- fbDF %>%
      filter(Batter == guy)
    guyOffSpd <- offSpdDF %>%
      filter(Batter == guy)
    
    tempDF <- data.frame(
      batterName = c(guy),
      fbAVG = c(AVG(guyFB)),
      fbOBP = c(OBP(guyFB)),
      fbSLG = c(SLG(guyFB)),
      fbSWmiss = c(swingWMissFB(guyFB)),
      fbTotalSwings = c(sum(guyFB$PitchCall %in% swing)),
      fbTotal = c(nrow(guyFB)),
      offSpdAVG = c(AVG(guyOffSpd)),
      offSpdOBP = c(OBP(guyOffSpd)),
      offSpdSLG = c(SLG(guyOffSpd)),
      offSpdswingWMiss = c(swingWMissOffSpd(guyOffSpd)),
      osTotalSwings = c(sum(guyOffSpd$PitchCall %in% swing)),
      osTotal = c(nrow(guyOffSpd))
    )
    
    elbinDF <- rbind(elbinDF, tempDF)
  }
  
  colnames(elbinDF) <- c("Batter", "FB AVG", "FB OBP", "FB SLG", "FB miss%", "FB Swings", "FB seen", "OS AVG", "OS OBP", "OS SLG", "OS miss%", "OS swings", "OS seen")
  
  return(elbinDF)
}
# wOBA calculator
# estimated coefficiants
# 1b: 0.9 | 2b: 1.25 | 3b: 1.58 | hr: 1.76 
# ROE: 0.92 | uBB: 0.71 | hbp: 0.74 | scale: 0.93
wOBA <- function(inDat) {
  # numerator
  singles = sum(inDat$PlayResult == "Single")
  doubles = sum(inDat$PlayResult == "Double")
  triples = sum(inDat$PlayResult == "Triple")
  homers = sum(inDat$PlayResult == "HomeRun")
  hitbypitch = sum(inDat$PitchCall == "HitByPitch")
  walk = sum(inDat$KorBB == "Walk") - (sum(inDat$PitchCall == "BallIntentional") / 4)
  
  numerator = (0.9 * singles) + (1.25 * doubles) + (1.58 * triples) + (1.76 * homers) + (0.74 * hitbypitch) + (0.71 * walk)
  
  # denomerator
  abs = ab(inDat)
  sf = sum(inDat$PlayResult == "Sacrifice")
  
  denomerator = abs + walk + sf + hitbypitch
  
  weightOB = numerator / denomerator
  
  return(format(round(weightOB, 3), nsmall = 3))
}

# Win Probability calculator
# Inputs:
#   - bo: base out state, just the base-out code in the form of 0 0 0   0
#   - inning: Just the number for the current inning
#   - top_inning: 1 if its the top if the inning, 0 if it's the bottom
#   - bat_order: 1-9, the current batter up from the lineup
#   - home_score, away_score: self explanatory, just the scores from the scoreboard
# Output:
# A percentage that represents the chance that the HOME team wins in that position.
# If the away team's chances are desired, simply subtact the given percentage from 100%
#
# NOTE: This only works with master_pbp.csv, any trackman data will not work
WP <- function(bo, inning_in, top_in, batting_order, score_difference) {
  # Simple 4 step process for a WP.
  # 1. Find all instances of the scenario in the db
  # 2. Check the number of times the home team wins
  # 3. Math
  # 4. return %
  
  # Step 1, isolate the scenario
  library(dplyr)
  # master_pbp <- read.csv("master_pbp.csv") # read the massive data set
  temp <- wpDataset %>%
    filter(RE24.init.state == bo & inning == inning_in & top_inning == top_in & bat_order == batting_order & score_diff == score_difference)
  
  # step 2 check the number of times the home team wins
  wins = 0
  gameids = unique(temp$game_id)
  # with the game ids from the unique scenarios, create a different dataset that looks only at the end of each of 
  # those games to see if the home team won
  temp_res <- wpDataset %>%
    filter(game_id %in% gameids & game_end == 1)
  
  wins = sum(temp_res$home_score > temp_res$away_score)
  
  occurances = nrow(temp)
  win_pct = format(round(wins/occurances, 3) * 100, nsmall = 1)
  
  return(win_pct)
}

# make the baseout state from the app's inputs
make_baseout <- function(r1, r2, r3, outs) {
  run_pos = paste(toString(r1), toString(r2), toString(r3), " ", toString(outs))
  return(run_pos)
}
WP(make_baseout(0, 1, 0, 1), 3, 1, 4, 2)
