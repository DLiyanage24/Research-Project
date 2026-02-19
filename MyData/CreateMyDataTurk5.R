# rep 1 of d=1.2, n1=5, n2=15 is exchanged for the conc.data plots
# the conc.lineup.csv is identical to turk5_1_15_5_12_16.csv

width = 7.17
height = 7.17
dpi = 75

ncol=4
alpha=0.5

#setwd("/Users/heike/Dropbox/Lineups-nf/turk5/images")
setwd("MyData/Turk5")
# ensure folders exist
dir.create("data", showWarnings = FALSE)
dir.create("images", showWarnings = FALSE)
getSample <- function(d=1, n1=15, n2=5) {
  #	y <- rnorm(n1)
  #	x <- rnorm(n2, mean=d)
  y <- rexp(n1, rate=1)
  x <- rexp(n2, rate=1/(1+d)) 
  group <- rep(c(1,2), c(length(y), length(x)))
  values = c(y,x)
  data.frame(group, values)
}

library(ggplot2)
library(nullabor)

#old ggplot2 version
#lineup.ops <- opts(legend.position="none", axis.ticks = theme_blank(), axis.text.y = theme_blank(), axis.text.x = theme_blank(), plot.margin=unit(c(0,0,0,0), "cm"))
lineup.ops <- theme(
  legend.position = "none",
  axis.ticks = element_blank(),
  axis.text.y = element_blank(),
  axis.text.x = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm")
)



lpwrap <- facet_wrap(~.sample, scales="free_y", ncol=4)

reps <- 1:3
ds = c(0.4, 0.6, 0.8, 1.0, 1.2)

n1s = c(15, 45, 135)
n2ratio=c(3,2,1)

#setwd("lineups-pvals")
set.seed(20120321)
for (rep in reps) {
  for (d in ds) {
    for (n1 in n1s) {
      for (n2 in n2ratio) {
        n2 <- round(n1/3*n2)
        frame <- getSample(d=d, n1=n1, n2=n2)
        truePos <- sample(20,1)
        lp <- lineup(null_permute("group"), frame, pos=truePos, n=20)
        lpsub <- subset(lp, .sample==truePos)
        pval <- t.test(lpsub$values[lpsub$group==1], lpsub$values[lpsub$group==2], alternative="less")$p.value
        
        while(pval > 0.2) {
          frame <- getSample(d=d, n1=n1, n2=n2)
          truePos <- sample(20,1)
          lp <- lineup(null_permute("group"), frame, pos=truePos, n=20)
          lpsub <- subset(lp, .sample==truePos)
          pval <- t.test(lpsub$values[lpsub$group==1], lpsub$values[lpsub$group==2], alternative="less")$p.value
        }
        pval2 <- t.test(log(lpsub$values[lpsub$group==1]), log(lpsub$values[lpsub$group==2]), alternative="less")
        write.csv(lp, file=sprintf("data/turk5_%d_%d_%d_%d_%d.csv",rep, n1, n2, round(10*d), truePos), row.names=F)
        
        # histogram stacked
        ggplot(aes(x=values,   fill=factor(group)), data=lp)+ lpwrap + geom_histogram(binwidth=1/sqrt(1+d))+ lineup.ops + xlab("")  + ylab("")
        ch <- sprintf("images/XXX_%d_%d_%d_%d_%d.png",rep, n1, n2, round(10*d), truePos)
        ggsave(gsub("XXX", "histogram", ch), width=width, height=height, dpi=dpi)
        
        
        # # histogram dodged
        # ggplot(aes(x=values,   fill=factor(group)), data=lp)+ facet_wrap(~.sample, scales="free_y") + geom_histogram(binwidth=10/(n1+n2), position="dodge")+ lineup.ops + xlab("")  + ylab("")
        # ch <- sprintf("XXX_%d_%d_%d_%d.png",rep, n2, d, truePos)
        # ggsave(gsub("XXX", "histogram", ch), width=width, height=height, dpi=dpi)
        
        # boxplots
        ggplot(aes(x=factor(group), y=values, fill=factor(group), colour=factor(group)), data=lp)  + facet_wrap(~.sample, ncol=ncol) + geom_boxplot(alpha=alpha) + coord_flip() + lineup.ops + xlab("")  + ylab("") 
        ch <- sprintf("images/XXX_%d_%d_%d_%d_%d.png",rep, n1, n2, round(10*d), truePos)
        ggsave(gsub("XXX", "boxplot", ch), width=width, height=height, dpi=dpi)
        
        
        
        # density
        ggplot(aes(x=values,  colour=factor(group), fill=factor(group)), data=lp) +  facet_wrap(~.sample, ncol=ncol, scales="free_y") + geom_density(alpha=alpha)+ lineup.ops + xlab("")  + ylab("") 
        ch <- sprintf("images/XXX_%d_%d_%d_%d_%d.png",rep, n1, n2, round(10*d), truePos)
        ggsave(gsub("XXX", "density", ch), width=width, height=height, dpi=dpi)
        
        
        # dotplots
        ggplot(aes(x=values, y=factor(group), colour=factor(group)), data=lp) + facet_wrap(~.sample, ncol=ncol) + geom_jitter(position=position_jitter(height=0.1, width=0), size=4, alpha=alpha)+ lineup.ops + xlab("")  + ylab("") 
        ch <- sprintf("images/XXX_%d_%d_%d_%d_%d.png",rep, n1, n2, round(10*d), truePos)
        ggsave(gsub("XXX", "dotplot", ch), width=width, height=height, dpi=dpi)
        
        
        
      }
    }
  }
}

