width = 7.17
height = 7.17
dpi = 75

getLineupPlots <- function(data, output = "", ncol=4){
	## set parameters

	#lineup.ops <- opts(legend.position="none", 
		#axis.ticks = theme_blank(),  
		#axis.text.x = theme_blank(), 
		#plot.margin=unit(c(0,0,0,0), "cm"))
	
	# theme 
	lineup.ops <- theme(
	  legend.position = "none",
	  axis.ticks = element_blank(),
	  #axis.text.y = element_blank(),
	  axis.text.x = element_blank(),
	  plot.margin = unit(c(0, 0, 0, 0), "cm")
	)
	



	## step 0: load the packages and check for errors
	require(beeswarm)
	require(vioplot)
	require(ggplot2)
	require(nullabor)
	

	## step 3: plot boxplot lineup
	p2 <- ggplot(data = data, aes(x = factor(group), 
				y = vals, colour=factor(group), fill=factor(group))) + 
				geom_boxplot(alpha=alpha) + xlab("") + ylab("") + 
				facet_wrap(~.sample, ncol=ncol) + lineup.ops
	ggsave(width=width, height=height, dpi=dpi, filename = gsub("turk6", "turk6_boxplot", output), plot = p2)

	## step 4: plot boxplot with jitter overlay
	p3 <- ggplot(data = data, aes (x = factor(group), 
				y = vals)) + 
		geom_boxplot(outlier.colour  = 'NA', colour="grey60") + 
		geom_jitter(aes(colour=factor(group)), position=position_jitter(width=0.15), shape=1) + 
		facet_wrap(~.sample, ncol=ncol) + lineup.ops +
		xlab("") + ylab("")

	ggsave(width=width, height=height, dpi=dpi, filename = gsub("turk6", "turk6_jitter", output), plot = p3)
	
		
	## step 4: plot beeswarm  
	
	getbeeswarmplots(data = data, methods = c("swarm", "square"), 
			output = output, ops=lineup.ops)


	
	## step 5: plot vioplot overlays
	getvioplots(data = data, bandwidth = c( 0.01, 0.005, 0.001), 
		output = output, ops=lineup.ops)

	for (bw in c(0.005, 0.01, 0.15)) {
		p1 <- vase_lineup(data, bw = bw, ncol = ncol) + 
			lineup.ops + ylab("") + xlab("")

		ggsave(filename=gsub("turk6", sprintf("turk6_vase%d",round(1000*bw)), output), 
		  plot=p1, width=width, height=height, dpi=dpi)
	}
}

### extract the values for plotting kernel density according to package vioplot
### copied (except for return statement) verbatim from function vioplot, package vioplot v0.2
vioplot.kernel <- function(x, at, h = NA, range = 1.5, wex = 1){
	datas <- list(x)
	n <- length(datas)
    if (missing(at)) {
        at <- 1:n
	}
    upper <- vector(mode = "numeric", length = n)
    lower <- vector(mode = "numeric", length = n)
    q1 <- vector(mode = "numeric", length = n)
    q3 <- vector(mode = "numeric", length = n)
    med <- vector(mode = "numeric", length = n)
    base <- vector(mode = "list", length = n)
    height <- vector(mode = "list", length = n)
    baserange <- c(Inf, -Inf)
    args <- list(display = "none")
	if (!(is.na(h))){ 
        args <- c(args, h = h)
	}
    for (i in 1:n) {
        data <- datas[[i]]
        data.min <- min(data)
        data.max <- max(data)
        q1[i] <- quantile(data, 0.25)
        q3[i] <- quantile(data, 0.75)
        med[i] <- median(data)
        iqd <- q3[i] - q1[i]

        upper[i] <- min(q3[i] + range * iqd, data.max)
        lower[i] <- max(q1[i] - range * iqd, data.min)
        est.xlim <- c(min(lower[i], data.min), max(upper[i], 
            data.max))
        smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
            args))
        hscale <- 0.4/max(smout$estimate) * wex
        base[[i]] <- smout$eval.points
        height[[i]] <- smout$estimate * hscale
        t <- range(base[[i]])
        baserange[1] <- min(baserange[1], t[1])
        baserange[2] <- max(baserange[2], t[2])
    }
	i <- 1
	return(data.frame(x = c(base[[i]], rev(base[[i]])), y = c(at[i] - height[[i]], 
                rev(at[i] + height[[i]]))))
}


getvioplots <- function(data, bandwidth, output, ops, ncol=4){
	for(h in bandwidth){

		lineup5 <- ddply(data, .(.sample, group), function(x) {
			vio <- vioplot.kernel(x$vals, h = h)
			data.frame( x = vio$y + x$group[1]-1, y = vio$x,
				group = x$group[1], .sample = x$.sample[1])
		})

		p5 <- ggplot(lineup5, aes(x = x, y = y)) + 
			geom_polygon(aes(group = group, fill=factor(group)), alpha=1.25*alpha) + 
			xlab("") + ylab("") + ops + facet_wrap(~.sample, ncol=ncol) +
			geom_boxplot(outlier.colour  = 'NA', aes(x = group, y=vals, 
			 group = group), colour="grey60", width = .1, data=data)   

		ggsave(width=width, height=height, dpi=dpi, filename = gsub("turk6",sprintf("turk6_vio%d", round(1000*h)), output), plot = p5)

	}
}

getbeeswarmplots <- function(data, methods = c("swarm", "square"), output, ops, ncol=4) {
	for(method in methods){
		lineup4 <- ddply(data,.(.sample), function(x) {
			if (firstbee) {
				dev.new(width=width, height=height)
				beeswarm(formula = vals~group, data = x, add = F, do.plot = T, method = method, cex=2.5)
				firstbee <<- FALSE
			}
			bs <- beeswarm(formula = vals~group, data = x, add = F, do.plot = F, method = method, cex=2.5)
			data.frame(vals = bs$y.orig, group=bs$x.orig, newy=bs$y, 
						newx = bs$x, .sample=x$.sample[1])
		})
		p4 <- ggplot(lineup4, aes(x = group, y = vals)) + 
				geom_boxplot(outlier.colour = 'NA', colour="grey60", width=0.75) + 
				geom_point(aes(x = newx, y = newy, colour=factor(group)), shape=1, size=1) + 
				facet_wrap(~.sample, ncol=ncol) + 
				ops + xlab("") + ylab("") 
					
		ggsave(width=width, height=height, dpi=dpi, filename = gsub("turk6", sprintf("turk6_%s",method), output), plot = p4)
	}
}

source("vase.r")
vase_lineup <- function(file, bw, ncol=4) {
	require(plyr)

	vd <- dlply(file, .(.sample, group), function(x) {
		vd <- vase(x=list(x$vals), bw = bw, plot=FALSE)

		df <- data.frame(vd[[1]]$body)
		df$group <- x$group[1]
		df$x <- df$x + df$group[1]
		df$.sample <- x$.sample[1]
		df$order <- 1:nrow(df)
		body <- df
		
		df <- data.frame(vd[[1]]$median)
		df$group <- x$group[1]
		df$x <- df$x + df$group
		df$xend <- df$xend + df$group
		df$.sample <- x$.sample[1]
		median <- df

		df <- data.frame(vd[[1]]$whisker)
		df$group <- x$group[1]
		df$x <- df$x + df$group
		df$xend <- df$xend + df$group
		df$.sample <- x$.sample[1]
		whisker <- df

		df <- data.frame(vd[[1]]$outlier)
		if (nrow(df) > 0) {
			df$group <- x$group[1]
			df$x <- df$x + df$group
			df$.sample <- x$.sample[1]
		}
		outlier <- df

		
		return(list(body=body, median=median, whisker=whisker, outlier=outlier))
	})

	vdbody <- ldply(1:length(vd), function(x) {
		df <- data.frame(vd[[x]]$body)
		df$order <- 1:nrow(df)
		df
	})
	
	vdoutlier <- ldply(1:length(vd), function(x) 
		data.frame(vd[[x]]$outlier)
	)
	
	vdmedian <- ldply(1:length(vd), function(x) 
		data.frame(vd[[x]]$median)
	)
	
	vdwhisker <- ldply(1:length(vd), function(x) 
		data.frame(vd[[x]]$whisker)
	)

	p <- ggplot(aes(x, y, group=group, fill=factor(group), colour=factor(group)),  data=vdbody) + 
	geom_polygon(alpha=0.5) + facet_wrap(~.sample, ncol=ncol) +
	geom_segment(aes(xend=xend,yend=yend), data=vdmedian) +
	geom_segment(aes(xend=xend,yend=yend), data=vdwhisker) 
	
	if (!is.null(vdoutlier))	
		p <- p + geom_point(colour="grey10", size=1.5, data=vdoutlier)

	p
}
