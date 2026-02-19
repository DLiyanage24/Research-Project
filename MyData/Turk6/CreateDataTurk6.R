

#setwd("~/Dropbox/Lineups-nf/turk6")
setwd("MyData/Turk6")
require(nullabor)

width = 7.17
height = 7.17
dpi = 75

ncol=5
alpha=0.5
# store images as png


n <- 96
simulation <- rep(1:3, each=9) 
# 1 for mean shift
# 2 for outliers
# 3 for bimodal


mus <- c(seq(0.5, 1.3, by=0.1), 
         seq(3.25, 5.25, by=0.25),
         rep(4:6, each=3))
n1s <- c(rep(96, 9),
         rep(6, 9),
         rep(c(48, 36, 30), 3))

set.seed(20120416)
foo <- rnorm(20000) # get away from seed

for (i in 1:length(mus)) {
  sgn <- c(-1,1)[rbinom(1,1, 0.5)+1]
  mu <- sgn*mus[i]
  
  n1 <- n1s[i]			
  x <- rnorm(n, 0,1)
  y <- rnorm(n, 0,1)
  z <- rnorm(n=n1, mean=mu, sd=1)
  
  y <- c(z,y)[1:n]
  if (simulation[i] == 3) y <- (y-mean(y))/sd(y)
  
  vals <- c(x, y)
  vals <- (vals*0.03 + 0.82)
  group <- rep(c(1,2), each=n)
  df <- data.frame(vals=vals, group=group)
  
  truePos <- sample(20,1)
  inf <- lineup(null_permute("group"), df , n=20, pos=truePos)
  
  write.csv(inf, file=sprintf("data/turk6_%d_%d_%d_%d.csv", round(mu*100), round(n1), simulation[i], truePos), row.names=FALSE)	
}

