par(family="serif", las=1, ps=18)

source('~/Dropbox/case_sysmo/Scripts/userSysmo.R')
source('~/Dropbox/case_sysmo/Scripts/assetsSysmo.R')
source('~/Dropbox/case_sysmo/Scripts/projectSysmo.r')
source('~/Dropbox/case_sysmo/Scripts/sharedAssetSysmo.r')
source('~/Dropbox/case_sysmo/Scripts/contrastCodeSysmo.r')
source('~/Dropbox/case_sysmo/Scripts/parallelsetGeneral.r')
# library(plotrix)
library(psych)
library(ggplot2)
library(data.table)
library(MASS)

secndR<- as.Date("2010-03-01")  # http://www.bbsrc.ac.uk/pa/grants/AwardDetails.aspx?FundingReference=BB%2FI004637%2F1
eval10<- as.Date("2010-05-01")  # http://www.bbsrc.ac.uk/pa/grants/AwardDetails.aspx?FundingReference=BB%2FI004637%2F1
eval11<- as.Date("2011-02-01")
eval12<- as.Date("2012-05-01")  # https://seek.sysmo-db.org/presentations/38
eval13<- as.Date("2013-02-01")  # https://seek.sysmo-db.org/presentations/85
dps_date<- as.Date("2011-06-01")

eval1<-eval11
eval2<-eval12

processCsv <- function(filename){
  r <- read.csv(file=paste0("/Users/kristian/Dropbox/case_sysmo/Data/Sysmo/",filename,".csv"),sep=",",head=TRUE)
  if(length(r$created_at) > 1){
    r$ncreated_at <- as.Date(r$created_at)
  }
  if(length(r$updated_at) > 1){
    r$nupdated_at <- as.Date(r$updated_at)
  }
  if(length(r$contributor_id) > 1){
    r<-r[r$contributor_id != "NULL", ]
  }
  return(r)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
