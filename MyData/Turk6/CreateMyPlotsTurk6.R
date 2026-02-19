#setwd("~/Dropbox/Lineups-nf/turk6")
library(ggplot2)
source("lineup_function.r")

alpha=0.5
# store images as png

files <- dir("data")

setwd("images")
firstbee <- TRUE

for (i in 1:length(files)) {
  filename <- files[i]
  df <- read.csv(sprintf("../data/%s",filename))
  getLineupPlots(data = df, output=gsub("csv","png", filename))
}


##########################

setwd("Turk6")
library(ggplot2)
library(plyr)
source("lineup_function.r")

alpha <- 0.5
dir.create("images", showWarnings = FALSE)

# Fresh, safe file list: only CSVs, return full paths too
csv_paths <- list.files("data", pattern = "\\.csv$", full.names = TRUE)
stopifnot(length(csv_paths) > 0)

# We will pass just the basename to your plotting function (it does gsub on names)
csv_names <- basename(csv_paths)

# Initialize beeswarm flag used inside getbeeswarmplots()
firstbee <- TRUE

# Set wd to where you want PNGs saved (your function uses relative names)
setwd("images")

for (i in seq_along(csv_names)) {
  filename <- csv_names[i]
  if (is.na(filename) || !nzchar(filename)) next  # extra guard
  df <- read.csv(file.path("..", "data", filename), check.names = FALSE)
  
  # Your function expects 'output' to be a filename it can gsub to make variants
  getLineupPlots(data = df, output = sub("\\.csv$", ".png", filename))
}
