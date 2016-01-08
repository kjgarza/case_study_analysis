
y1 <- read.csv(file="/Users/kristian/Downloads/statement(1).csv",sep=",",head=TRUE)

y2 <- y1[y1$item == 'LIDL UK MANCHESTER',]


y3 <- y1[grep("CASH", y1$item), ]


plot(as.Date(y1$date, format='%d/%m/%Y'),y1$debit,type="l", col="blue", xlab="Time", ylab="Debit", ylim=range(c(y1$debit,y2$debit)), xlim=range(c(as.Date(y1$date, format='%d/%m/%Y'),as.Date(y2$date, format='%d/%m/%Y'))))


par(new=TRUE)
plot(as.Date(y3$date, format='%d/%m/%Y'),y3$debit, col="red", xlab="", ylab="", xlim=range(c(as.Date(y1$date, format='%d/%m/%Y'),as.Date(y3$date, format='%d/%m/%Y'))), ylim=range(c(y1$debit,y3$debit)), axes = FALSE)

y2 <- y1[grep("CASH IN", y1$item), ]
par(new=TRUE)
plot(as.Date(y2$date, format='%d/%m/%Y'),y2$debit, col="black", xlab="", ylab="", xlim=range(c(as.Date(y1$date, format='%d/%m/%Y'),as.Date(y2$date, format='%d/%m/%Y'))), ylim=range(c(y1$debit,y2$debit)), axes = FALSE)