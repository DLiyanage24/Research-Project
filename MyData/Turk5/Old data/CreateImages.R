# === Re-plot existing lineups from CSVs ===


dir.create("images", showWarnings = FALSE)

# plot params
width  <- 7.17
height <- 7.17
dpi    <- 75
ncol   <- 4
alpha  <- 0.5

library(ggplot2)
library(grid)     

# theme 
lineup.ops <- theme(
  legend.position = "none",
  axis.ticks = element_blank(),
  axis.text.y = element_blank(),
  axis.text.x = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm")
)
lpwrap <- facet_wrap(~.sample, scales = "free_y", ncol = ncol)

# parse turk5_1_15_5_12_16.csv -> rep=1 n1=15 n2=5 d10=12 truePos=16
parse_params <- function(fname) {
  m <- regexec("^turk5_(\\d+)_(\\d+)_(\\d+)_(\\d+)_(\\d+)\\.csv$", fname)
  got <- regmatches(fname, m)[[1]]
  if (length(got) != 6) return(NULL)
  list(
    rep     = as.integer(got[2]),
    n1      = as.integer(got[3]),
    n2      = as.integer(got[4]),
    d10     = as.integer(got[5]),
    truePos = as.integer(got[6]),
    d       = as.integer(got[5]) / 10
  )
}

# loop over all matching CSVs in data/
files <- list.files("data_old", pattern = "^turk5_\\d+_\\d+_\\d+_\\d+_\\d+\\.csv$", full.names = FALSE)

for (f in files) {
  params <- parse_params(f)
  if (is.null(params)) next
  
  # read lineup data
  lp <- read.csv(file.path("data_old", f))
  
  # build output name: images/XXX_<rep>_<n1>_<n2>_<d10>_<truePos>.png
  ch <- sprintf("images/XXX_%d_%d_%d_%d_%d.png",
                params$rep, params$n1, params$n2, params$d10, params$truePos)
  
  # histogram stacked
  ggplot(aes(x=values, fill=factor(group)), data=lp) +
    lpwrap +
    geom_histogram(binwidth = 1 / sqrt(1 + params$d)) +
    lineup.ops + xlab("") + ylab("")
  ggsave(gsub("XXX", "histogram", ch), width=width, height=height, dpi=dpi)
  
  # boxplots
  ggplot(aes(x=factor(group), y=values, fill=factor(group), colour=factor(group)), data=lp) +
    facet_wrap(~.sample, ncol=ncol) +
    geom_boxplot(alpha=alpha) +
    coord_flip() +
    lineup.ops + xlab("") + ylab("")
  ggsave(gsub("XXX", "boxplot", ch), width=width, height=height, dpi=dpi)
  
  # density
  ggplot(aes(x=values, colour=factor(group), fill=factor(group)), data=lp) +
    facet_wrap(~.sample, ncol=ncol, scales="free_y") +
    geom_density(alpha=alpha) +
    lineup.ops + xlab("") + ylab("")
  ggsave(gsub("XXX", "density", ch), width=width, height=height, dpi=dpi)
  
  # dotplots
  ggplot(aes(x=values, y=factor(group), colour=factor(group)), data=lp) +
    facet_wrap(~.sample, ncol=ncol) +
    geom_jitter(position=position_jitter(height=0.1, width=0), size=4, alpha=alpha) +
    lineup.ops + xlab("") + ylab("")
  ggsave(gsub("XXX", "dotplot", ch), width=width, height=height, dpi=dpi)
}

# count all CSVs in the data folder
datasets <- list.files("data_old", pattern = "\\.csv$")
length(datasets)

#count all image files (png)
images <- list.files("images", pattern = "\\.png$")
length(images)


