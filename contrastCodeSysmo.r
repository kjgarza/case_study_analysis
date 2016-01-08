
processDummy<-function(data,n) {

	data<-data[!(data$rs == "Inf"),]

	tapply(data$rs, data$project_role_id, mean)

	contrasts(data$project_role_id) = contr.treatment(n)
	summary(lm(rs ~ project_role_id, data))

}


processSimple<-function(data,n) {

	data<-data[!(data$rs == "Inf"),]

	tapply(data$rs, data$title, mean)

	c <- contr.treatment(n)

	# my.coding<-matrix(rep(1/4,12),ncol=3)
	my.coding<-matrix(rep(1/n,(n*(n-1))),ncol=(n-1))
	my.simple<-c-my.coding
	my.simple

	contrasts(data$title) = my.simple
	summary(lm(rs ~ title, data))

}


processDev<-function(data,n) {

	data<-data[!(data$rs == "Inf"),]

	tapply(data$rs, data$project_role_id, mean)

	contrasts(data$project_role_id) = contr.sum(n)
	summary(lm(rs ~ project_role_id, data))

}

getUsersDataStat<-function() {
	r<-getContributors3()
	r<-joinAssoc(r)
	r<-joinRole(r)
	r<-joinSharingRatio(r)
	return(r)
}


factRwN<-function(data) {
	contrasts(data$project_role_id) = contr.sum(2)
	contrasts(data$project_name) = contr.sum(15)
	contrasts(data$title) = contr.sum(4)
	r<-lm(rs ~ title * project_name * project_role_id, data)
	plot(r, which=1)
	bc <- boxcox(r)
	which.max(bc$y)
	lambda <- bc$x[which.max(bc$y)]
	z <- data$rs^lambda
	m2 <- lm(z ~ data$title * data$project_name * data$project_role_id)
	summary(m2)

	print(anova(m2))
}

multiRwN2<-function(data) {
	fit <- lm(rs ~ title + project_name + project_role_id, data=data)
	summary(fit)

	# Other useful functions
	coefficients(fit) # model coefficients
	confint(fit, level=0.95) # CIs for model parameters
	fitted(fit) # predicted values
	residuals(fit) # residuals
	anova(fit) # anova table
	vcov(fit) # covariance matrix for model parameters
	influence(fit) # regression diagnostics
	# diagnostic plots
	layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
	plot(fit) 

	# compare models
	fit1 <- lm(rs ~ title + project_name + project_role_id, data=data)
	fit2 <- lm(rs ~ title + project_name, data=data)
	anova(fit1, fit2) 

	# K-fold cross-validation
	# library(DAAG)
	# cv.lm(df=data, fit, m=3) # 3 fold cross-validation


	# Assessing R2 shrinkage using 10-Fold Cross-Validation

	# fit <- lm(rs ~ title + project_name + project_role_id, data=data)

	# library(bootstrap)
	# # define functions
	# theta.fit <- function(x,y){lsfit(x,y)}
	# theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}

	# # matrix of predictors
	# X <- as.matrix(data[c("title","project_name","project_role_id")])
	# # vector of predicted values
	# y <- as.matrix(data[c("rs")])

	# results <- crossval(X,y,theta.fit,theta.predict,ngroup=10)
	# cor(y, fit$fitted.values)**2 # raw R2
	# cor(y,results$cv.fit)**2 # cross-validated R2 

	# Stepwise Regression
	library(MASS)
	fit <- lm(rs ~ title + project_name + project_role_id, data=data)
	step <- stepAIC(fit, direction="both")
	step$anova # display results 


}


processAnov<-function(data) {

	data<-data[!(data$rs == "Inf"),]

# Discipline Analysis
	print("Discipline Analysis")
	contrasts(data$title) = contr.sum(4)
	r<-lm(rs ~ title, data)

	print(summary(r))
	print(anova(r))

	plot(r, which=1)
	bc <- boxcox(r)
	which.max(bc$y)
	lambda <- bc$x[which.max(bc$y)]
	z <- data$rs^lambda
	m2 <- lm(z ~ data$title)
	print("Discipline Analysis NORMAL")
	print(summary(m2))
	print(anova(m2))



	p1<-ggplot(r, aes(x=title, y=rs)) + geom_boxplot()
	p1  + theme_bw() + theme(strip.text.x = element_text(size=25),
          strip.text.y = element_text(size=25)) + theme(text = element_text(size=18, family="serif"))


# Project Analysis
	# print("Project Analysis")
	# contrasts(data$project_name) = contr.sum(15)
	# r<-lm(rs ~ project_name, data)
	# print(summary(r))	
	# print(anova(r))

	# p2<-ggplot(r, aes(x=project_name, y=rs)) + geom_boxplot()
	

# PALS Analysis	
	# print("Role Analysis")
	# contrasts(data$project_role_id) = contr.sum(2)
	# r<-lm(rs ~ project_role_id, data)
	# print(summary(r))	
	# print(anova(r))

	# p3<-ggplot(r, aes(x=project_role_id, y=rs)) + geom_boxplot()
	

	# multiplot(p1,p2,p3, cols=1)

}


processMAnov<-function(data) {
	print("Multi Analysis")
	contrasts(data$project_role_id) = contr.sum(2)
	contrasts(data$project_name) = contr.sum(15)
	contrasts(data$title) = contr.sum(4)
	r<-lm(rs ~ project_role_id * project_name * title, data)
	print(summary(r))	
	print(anova(r))

	boxcox(rs ~ project_role_id, data = data,interp = TRUE)
	boxcox(rs ~ project_name, data = data,interp = TRUE)
	boxcox(rs ~ title, data = data,interp = TRUE)
	return(r)
}

getassetsDataStat<-function() {
	r<-getAllresc(0)
	r<-joinRole(r)
	r<-isOpenShared(r)
	r<-joinFormat(r)
	return(r)
}

processAssMAnov<-function(data) {
	print("Multi  ASS Analysis")
	# contrasts(data$project_role_id) = contr.sum(2)
	contrasts(data$project_name) = contr.treatment(15)
	contrasts(data$lab) = contr.treatment(2)
	contrasts(data$content.type) = contr.treatment(20)
	contrasts(data$type) = contr.treatment(3)
	r<-lm(rs ~ lab + project_name + type+content.type, data)
	print(summary(r))	
	print(anova(r))
	boxcox(rs ~ content.type, data = data)
}
