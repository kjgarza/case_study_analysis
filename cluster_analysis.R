library(tm)


options(header=FALSE, stringsAsFactor=FALSE, fileEcoding="latin1")
data <- read.csv(file=paste0("/Users/kristian/Downloads/interviewees_table_test.csv"),sep=",",head=TRUE)
# keeps <- c('Project','Role','Location','Collaboration.Activities','Compatibility','Descision','Complementarity','Mutuality','Trustworthiness','Commitment','Motivations','Meeting','Coordination','Autonomy','Goal.clarity','Responsability')
thmodel <- c('Project','Role','Administration','Complementarity','Mutuality','TrustworthinessCombined','Autonomy','Descision')
haramodel <- c('Project','Role','Compatibility','Complementarity','Motivations')
# corpus <- Corpus(DataframeSource(data))
datah<-data[haramodel]
datat<-data[thmodel]
corpus <- Corpus(VectorSource(datat[datat$Role =="Modeller"]))

cleanset<- tm_map(corpus, removeWords, stopwords("english"))
cleanset <- tm_map(cleanset, stripWhitespace)
cleanset <- tm_map(cleanset, tolower)

dtm<-DocumentTermMatrix(cleanset)

mydata.df<- as.data.frame(inspect(dtm))

mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method="euclidean")
fit<-hclust(d, method="ward")
plot(fit)
groups <- cutree(fit, k=2)
rect.hclust(fit, k=2, border="red")