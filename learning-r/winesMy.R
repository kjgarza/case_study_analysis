
library(psych)

ratings <- read.table(file="/Users/kristian/Documents/stats1_ex01.txt", head=TRUE)


class(ratings)
names(ratings)

layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

hist(ratings$WoopWoop, xlab="Rating")
hist(ratings$RedTruck, xlab="Rating")
hist(ratings$HobNob, xlab="Rating")
hist(ratings$FourPlay, xlab="Rating")

describe(ratings)