
library(psych)

survey <- read.csv(file="/Users/kristian/Documents/Rscripts/survey.csv", sep=",", head=TRUE)

bins=seq(0,5,by=1)

if(0){
#layout(matrix(c(1,2,3,4,5,6,7,8),4,2,byrow=TRUE))
layout(matrix(c(1,2,3,4),4,1,byrow=TRUE))
#hist(survey$continue[survey$challenge =="CH1"], xlab="Rating", main="I desire to continue collaborating with the \n participants  of my challenge to it's completion (poster)",xlim = range(0,5),breaks=bins)
hist(survey$satisfaction[survey$challenge =="CH1"], xlab="Rating", main="Satisfaction",xlim = range(0,5),breaks=bins)

hist(survey$expectation[survey$challenge =="CH1"], xlab="Rating", main="The expectations I had when joining the \n Challenge were fullfiled with the challenge's outcome",xlim = range(0,5),breaks=bins)
#hist(survey$traditional[survey$challenge =="CH1"], xlab="Rating", main="With my traditional tools I could have \n perform the same work that we perform during \n  the Challenge with HELIO",xlim = range(0,5),breaks=bins)

#hist(survey$mygoals[survey$challenge =="CH1"], xlab="Rating", main="My scientific goals were considered in the \n Challenge's goals",xlim = range(0,5),breaks=bins)

hist(survey$understanding[survey$challenge =="CH1"], xlab="Rating", main="I consider I have gain enough knowledge of \n the HELIO capabilities to use it and \n explore it more frequently",xlim = range(0,5),breaks=bins)

#hist(survey$dependency[survey$challenge =="CH1"], xlab="Rating", main="There was NOT a high dependency on each and \n every member of the group to work in the Challenge",xlim = range(0,5),breaks=bins)

#hist(survey$benefitial[survey$challenge =="CH1"], xlab="Rating", main="The outcome of the Challenge is beneficial \n  to me and/or my research",xlim = range(0,5),breaks=bins)

hist(survey$complete[survey$challenge =="CH1"], xlab="Rating", main="The Scientific Challenge was completed succefully",xlim = range(0,5),breaks=bins)
}







if(1){
layout(matrix(c(1,2,3,4,5,6,7,8),4,2,byrow=TRUE))
	
hist(survey$satisfaction, xlab="Rating", main="Satisfaction",xlim = range(0,5),breaks=bins)	
#hist(survey$continue, xlab="Rating", main="I desire to continue collaborating with the \n participants  of my challenge to it's completion (poster)",xlim = range(0,5),breaks=bins)

hist(survey$expectation, xlab="Rating", main="The expectations I had when joining the \n Challenge were fullfiled with the challenge's outcome",xlim = range(0,5),breaks=bins)
hist(survey$traditional, xlab="Rating", main="With my traditional tools I could have \n perform the same work that we perform during \n  the Challenge with HELIO",xlim = range(0,5),breaks=bins)

hist(survey$mygoals, xlab="Rating", main="My scientific goals were considered in the \n Challenge's goals",xlim = range(0,5),breaks=bins)

hist(survey$understanding, xlab="Rating", main="I consider I have gain enough knowledge of \n the HELIO capabilities to use it and \n explore it more frequently",xlim = range(0,5),breaks=bins)

hist(survey$dependency, xlab="Rating", main="There was NOT a high dependency on each and \n every member of the group to work in the Challenge",xlim = range(0,5),breaks=bins)

hist(survey$benefitial, xlab="Rating", main="The outcome of the Challenge is beneficial \n  to me and/or my research",xlim = range(0,5),breaks=bins)

hist(survey$complete, xlab="Rating", main="The Scientific Challenge was completed succefully",xlim = range(0,5),breaks=bins)
}


describe(survey$benefitial[survey$challenge =="CH1"])
