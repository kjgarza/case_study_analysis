
library(psych)

training <- read.table(file="/Users/kristian/Documents/DAA.01.txt", head=TRUE)

if(1){
layout(matrix(c(1,2,3,4,5,6,7,8),4,2,byrow=TRUE))

hist(training$post.wm.s[training$cond =="aer"], xlab="Rating", main="Aerobic Post Spatial")
hist(training$post.wm.v[training$cond =="aer"], xlab="Rating", main="Aerobic Post Verbal")
hist(training$pre.wm.s[training$cond =="aer"], xlab="Rating", main="Aerobic Pre Spatial")
hist(training$pre.wm.v[training$cond =="aer"], xlab="Rating", main="Aerobic Pre Verbal")


hist(training$post.wm.s[training$cond =="des"], xlab="Rating", main="Designed Post Spatial")
hist(training$post.wm.v[training$cond =="des"], xlab="Rating", main="Designed Post Verbal")
hist(training$pre.wm.s[training$cond =="des"], xlab="Rating", main="Designed Pre Spatial")
hist(training$pre.wm.v[training$cond =="des"], xlab="Rating", main="Designed Pre Verbal")
}


describe(training$post.wm.s[training$cond =="aer"])
describe(training$pre.wm.s[training$cond =="aer"])
describe(training$post.wm.v[training$cond =="aer"])
describe(training$pre.wm.v[training$cond =="aer"])
describe(training$post.wm.s[training$cond =="des"])
describe(training$pre.wm.s[training$cond =="des"])
describe(training$post.wm.v[training$cond =="des"])
describe(training$pre.wm.v[training$cond =="des"])