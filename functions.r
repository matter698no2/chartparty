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
ab <- function(inDat) {
  return(sum(inDat$PitchCall == "InPlay"))
}

# returns the number of hits a player has had
hits <- function(inDat) {
  return(sum(inDat$PitchCall == "InPlay" & inDat$OutsOnPlay == 0))
}

# true ball/strike calc
# checks a range of x & y values to determine
# if the pitch was a ball of strike in the coordinate plane
isStrikeX <- function(pitch) {
  if (pitch >= -0.7803 && pitch <= 0.7803) {
    return(TRUE)
  }
  return(FALSE)
}

isStrikeY <- function(pitch) {
  if (pitch >= 1.5 && pitch <= 3.5) {
    return(TRUE)
  }
  return(FALSE)
}

isStrike <- function(X, Y) {
  if (isStrikeX(X) && isStrikeY(Y)) {
    return(TRUE)
  }
  return(FALSE)
}

# takes two lists of x and y coordinates and 
# iterates through them to determine if they are both strikes
isStrikeCounter <- function(X, Y) {
  rowIndex = 1
  numStrikes = 0
  while(rowIndex != length(X)) {
    if(isStrikeX(X[rowIndex]) & isStrikeY(Y[rowIndex])){
      numStrikes = numStrikes + 1
    }
    rowIndex = rowIndex + 1
  }
  return(numStrikes)
}

ex <- dat$PlateLocSide
isStrikeCounter(dat$PlateLocSide, dat$PlateLocHeight)

ex[1]
