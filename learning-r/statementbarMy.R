
statemt <- read.csv(file="/Users/kristian/Downloads/statement(1).csv",sep=",",head=TRUE)

#y2 <- y1[y1$item == 'LIDL UK MANCHESTER',]


cash <- statemt[grep("CASH", statemt$item), ]
cash<- cash[cash$cost < 0,]
payin <- statemt[grep("CASH IN", statemt$item), ]

food <- statemt[grep("LIDL", statemt$item), ]
aldi <- statemt[grep("ALLEN", statemt$item), ]





#train <- statemt[grep("VIRGIN", statemt$item), ]
#transport <- statemt[grep("LUL", statemt$item),grep("SCOACH", statemt$item),grep("TFL", statemt$item),grep("LUL", statemt$item), ]
#rent <- statemt[grep("CORSTORPHINE", statemt$item),grep("ALLEN-BURT", statemt$item), ]



cashg <- aggregate(cash$cost,by=list((substr(as.Date(cash$date, format='%d/%m/%Y'),1,7))),sum) 
paying <- aggregate(payin$cost,by=list((substr(as.Date(payin$date, format='%d/%m/%Y'),1,7))),sum) 
foodg <- aggregate(food$cost,by=list((substr(as.Date(food$date, format='%d/%m/%Y'),1,7))),sum) 

z<-cbind(foodg$x,paying$x)

cds <- foodg[grep("2012-05", foodg$Group.1), ]



# Grouped Bar Plot
mycounts <- table(foodg$x, paying$x )
barplot(z, main="Car Distribution by Gears and VS", xlab="Number of Gears", col=c("darkblue","red"), beside=TRUE)

