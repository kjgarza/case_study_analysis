# load required libraries
require(maps)
require(mapdata)
require(RColorBrewer)

# Create a dataframe with the reported observations
# loc <- c('USA', 'China', 'Canada', 'Spain')
# cases <- c(40, 26, 6, 1)
r <- read.csv(file=paste0("/Users/kristian/Dropbox/Data/rawdata_2228.csv"),sep=",",head=FALSE)
loc<-r$V2
cases<-r$V3

flu <- data.frame(loc,cases)

# Setup the coordinate system
png(width=67,height=41.4,units="cm",res=75)
m <- map('worldHires',plot=TRUE, fill=T)

# Match up our observations
stm <- match.map(m, flu$loc)

# Rank the cases and assign colors using the RColorBrewer YlOrRd palette
flu$rank <- rank(flu$cases, ties='min')
# pal <- brewer.pal(max(flu$rank),'YlOrRd')
pal <- colorRampPalette(brewer.pal(9,"YlGnBu"))(max(flu$rank))
color <- pal[flu$rank]
flu.color <- color[stm]

# Do the drawing
map(m,col=flu.color,fill=T, lty=0,boundary=F,interior=F) # fill regions
map('worldHires',interior=T,add=T,col='grey30', exact=FALSE) # plot boundaries
map.axes()

grid(col='grey50')
title('Country Comparison :: \n Obesity - adult prevalence rate (2008)', cex=2)
graphics.off()
# legend('bottomleft', legend=paste(flu$loc,flu$cases),
#        fill=color, bg='white', horiz=T, cex=0.75,
#        title=paste('Jason B. Smith | 28 April 2009 | Source: CNN.com'))