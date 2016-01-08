

joinSize<- function(data, type){
  keeps<- c("id","filesize","project_id")    
  if(type == "data_files"){
    # dataFiles    <- processCsv('data_files')
    dataFilessize<- processCsv('filesizes-and-versions-DataFile')
    r<- merge(data, dataFilessize[keeps], by="id", all.x=TRUE)
  }
  if(type == "models"){   
    # models= 
      # models       <- processCsv('models')
    modelssize     <- processCsv('filesizes-and-versions-Model')
    r<- merge(data, modelssize[keeps], by="id", all.x=TRUE)
  }
  if(type == "sops"){
    # sops         <- processCsv('sops')
    sopssize     <- processCsv('filesizes-and-versions-Sop')
    r<- merge(data, sopssize[keeps], by="id", all.x=TRUE)
  }
  # colnames(r)[17] <- "ncreated_at"
  return(r)
} 

joinFormat<- function(data, type){
  keeps<- c("id","content.type","type")    
  # if(type == "data_files"){
    # dataFiles    <- processCsv('data_files')
    dataFilessize<- processCsv('filesizes-and-versions-DataFile')
    dataFilessize$type <- as.character(dataFilessize$type)
    dataFilessize$type[dataFilessize$type == "DataFile"] <- "data_file"    
    d<- merge(data, dataFilessize[keeps], by=c("type","id"), all.x=TRUE)
  # }
  # if(type == "models"){   
    # models= 
      # models       <- processCsv('models')
    
    modelssize     <- processCsv('filesizes-and-versions-Model')
    modelssize$type <- as.character(modelssize$type)
    modelssize$type[modelssize$type == "Model"] <- "model"
    m<- merge(data, modelssize[keeps], by=c("type","id"), all.x=TRUE)
  # }
  # if(type == "sops"){
    # sops         <- processCsv('sops')
    sopssize     <- processCsv('filesizes-and-versions-Sop')
    sopssize$type <- as.character(sopssize$type)
    sopssize$type[sopssize$type == "Sop"] <- "sop"    
    s<- merge(data, sopssize[keeps], by=c("type","id"), all.x=TRUE)

    r<-rbind(d[d$type=="data_file",],m[m$type=="model",],s[s$type=="sop",])

    r$type<-as.factor(r$type)
  # }
  # colnames(r)[17] <- "ncreated_at"
  return(r)
} 

getUserAss<-function(s, data){
  
  dataFiles <- data[data$contributor_id == s, ]
  r <- as.data.frame(table(dataFiles$contributor_id))
  return(median(r$Freq))
}

joinPubliStat<-function(data) {

  if(is.null(data$type) ){
    print("wrong data. I need type")
    stop()
  }
  
  rel <- processCsv('relationships')
  rel$type<-rel$subject_type
  rel$id<-rel$subject_id
  rel<-rel[!(rel$predicate %in% c("related_to_publication")),]

  rel$type <- as.character(rel$type)
  rel$type[rel$type == "Model"] <- "model"
  rel$type[rel$type == "DataFile"] <- "data_file"
  rel$type[rel$type == "Sop"] <- "sop"
  rel$type <- as.factor(rel$type)


  keeps<- c("type","object_id","id")    
  r<- merge(data, rel[keeps], by=c("type","id"), all.x=TRUE)    
  return(r)
}



describeContr<-function() {
  users <- c(1:327)
  data  <- processCsv("data_files")
  r<-sapply(users, data, FUN=getUserAss)
  return(r)
}


graphLongTail<-function() {
  r<- getAllresc(0)
  r<- as.data.frame(table(r$contributor_id))
  y<- as.data.frame(table(r$Freq))
  
  z<-r[with(r, order(Freq)), ]

  plot(z$Freq,xlab="Users", ylab="Assets Uploaded")
  print(y)
  plot(y$Freq,y$Var1)
}

getAllresc<- function(uid){
  dataFiles    <- processCsv('data_files')
  models       <- processCsv('models')
  sops         <- processCsv('sops')
  dataFiles$type<- "data_file"
  models$type<- "model"
  sops$type<- "sop"

  # if(uid){
  #   dataFiles <- dataFiles[dataFiles$contributor_id == uid, ]
  #   models <- models[models$contributor_id == uid, ]
  #   sops <- sops[sops$contributor_id == uid, ]
  #   print(c("Sops: ", length(sops$ncreated_at)))
  #   print(c("Data: ", length(dataFiles$ncreated_at)))
  #   print(c("Models: ", length(models$ncreated_at))) 
    
  # }
  
  keeps<-c("id","contributor_id","ncreated_at","nupdated_at","contributor_type","version","type")
  total <- rbind(sops[keeps], dataFiles[keeps], models[keeps]) 
  r <-total[with(total, order(ncreated_at)), ]

  r <- joinPerson(r)
  r <- joinAssoc(r)
  if(uid) r<-r[r$contributor_id == uid,]
  return(r)
}


graphGrowth <- function(){

  dataFiles <- processCsv('data_files')
  models <- processCsv('models')
  sops <- processCsv('sops')
  publs <- processCsv('publications')

  #    newdata <- users[order(users$ncreated_at),]
  plotlabels<- c("Year", "Assets Accumulation")
  legends <- c("Publications","Data files", "Models", "Sops")
  
  models<-filterByPeriod(models, as.Date("2010-01-01"), as.Date("2013-12-12"))
  dataFiles<-filterByPeriod(dataFiles, as.Date("2010-01-01"), as.Date("2013-12-12"))
  sops<-filterByPeriod(sops, as.Date("2010-01-01"), as.Date("2013-12-12"))
  publs<-filterByPeriod(publs, as.Date("2010-01-01"), as.Date("2013-12-12"))

  dataFiles<-dataFiles[order(dataFiles$ncreated_at),]
  dataFiles$id<-c(1:length(dataFiles$id))

  models<-models[order(models$ncreated_at),]
  models$id<-c(1:length(models$id))

  sops<-sops[order(sops$ncreated_at),]
  sops$id<-c(1:length(sops$id))

  publs<-publs[order(publs$ncreated_at),]
  publs$id<-c(1:length(publs$id))    

  dataFiles<- joinSize(dataFiles, "data_files")
  models<- joinSize(models, "models")
  sops<- joinSize(sops, "sops")

  
  plot(dataFiles$ncreated_at, dataFiles$id, type='b',col="blue", xlab=plotlabels[1],ylab=plotlabels[2], pch=1)# ,xlim=c(14830,15700))
  lines(models$ncreated_at, models$id, type='b', col="green", pch=8)
  lines(sops$ncreated_at, sops$id, type='b', col="brown", pch=3)
  lines(publs$ncreated_at, publs$id, type='b', col="red", pch=4)
 
  abline(v=eval10,lty=3)    
  abline(v=eval11,lty=3)    
  abline(v=eval12,lty=3)    
  abline(v=eval13,lty=3)   

  legend("topleft", legends, fill = c("red","blue","green","brown"), col = c("red","blue","green","brown"),border = NA)


  ee<-getAccSize("data_files")
  twoord.plot(dataFiles$ncreated_at,dataFiles$id,ee$Group.1,ee$acc)
  abline(v=eval10,lty=3)    
  abline(v=eval11,lty=3)    
  abline(v=eval12,lty=3)    
  abline(v=eval13,lty=3) 

  par(mfrow=c(2,2))

  res1<-lm(dataFiles$id~dataFiles$ncreated_at)
  res2<-lm(models$id~models$ncreated_at)
  res3<-lm(sops$id~sops$ncreated_at)
  res4<-lm(publs$id~publs$ncreated_at)
  
  plot(res1,col="blue",which=1:1)
  plot(res2,col="green",which=1:1)
  plot(res3,col="brown",which=1:1)
  plot(res4,col="red",which=1:1)

  print(summary(res1))
  print(summary(res2))
  print(summary(res3))
  print(summary(res4))


  par(mfrow=c(2,2))

  a<-as.data.frame(table(publs$contributor_id))
  hist(a$Freq, col=c("#823483"), border=NA)
  
  b<-as.data.frame(table(a$Freq))
  x <- b[!(b$Var1 %in% c(0)),]
  plot(log(as.numeric(x$Var1)),log(as.numeric(x$Freq)))

  a<-as.data.frame(table(sops$contributor_id))
  hist(a$Freq, col=c("#823483"), border=NA)
  b<-as.data.frame(table(a$Freq))
  x <- b[!(b$Var1 %in% c(0)),]
  plot(log(as.numeric(x$Var1)),log(as.numeric(x$Freq)))

  a<-as.data.frame(table(models$contributor_id))
  hist(a$Freq, col=c("#823483"), border=NA)
  b<-as.data.frame(table(a$Freq))
  x <- b[!(b$Var1 %in% c(0)),]
  plot(log(as.numeric(x$Var1)),log(as.numeric(x$Freq)))

  a<-as.data.frame(table(dataFiles$contributor_id))
  hist(a$Freq, col=c("#823483"), border=NA)
  b<-as.data.frame(table(a$Freq))
  x <- b[!(b$Var1 %in% c(0)),]
  plot(log(as.numeric(x$Var1)),log(as.numeric(x$Freq)))#,  ylim=rev(range(x$Freq)))


  keeps<-c("id","contributor_id","ncreated_at", "contributor_type")
  total <- rbind(sops[keeps], dataFiles[keeps], models[keeps], publs[keeps]) 

  par(mfrow=c(1,2))
  a<-as.data.frame(table(total$contributor_id))
  hist(a$Freq, col=c("#823483"), border=NA, main="SEEK Contributions", ylab="Number of Contributors",xlab="Number of contributions")

  print(describe(a$Freq))

  graphContributionMyExperiment()

  b<-as.data.frame(table(a$Freq))
  x <- b[!(b$Var1 %in% c(0)),]
  plot((as.numeric(x$Var1)),(as.numeric(x$Freq)),log="xy",col=c("#823483"),pch = 19,main="SEEK Contributions", ylab="Number of Contributors",xlab="Number of contributions")#,  ylim=rev(range(x$Freq)))
  # res5<-lm(log(as.numeric(x$Freq))~log(as.numeric(x$Var1)))
  # abline(res5, col="red")


}



graphContributionMyExperiment<- function() {

  myexp<-processCsv("8EcwGLPm")
  a<-as.data.frame(table(myexp$a))
  hist(a$Freq, col=c("#823483"), border=NA, breaks=55, xlim=c(0,200), main="MyExperiment Contributions", ylab="Number of Contributors",xlab="Number of contributions")
  b<-as.data.frame(table(a$Freq))
  x <- b[!(b$Var1 %in% c(0)),]
  plot((as.numeric(x$Var1)),(as.numeric(x$Freq)), col=c("#823483"),pch = 19, log="xy",main="MyExperiment Contributions", ylab="Number of Contributors",xlab="Number of contributions")#,  ylim=rev(range(x$Freq)))
  # res5<-lm(log(as.numeric(x$Freq))~log(as.numeric(x$Var1)))
  # abline(res5, col="red")

  print(describe(a$Freq))

}


graphImage<- function(filename) {
  library(png)

#Replace the directory and file information with your info
# ima <- readPNG("/Users/kristian/Dropbox/Images/numcontrNumUserMyExp.png")
ima <- readPNG(paste0("/Users/kristian/Dropbox/Images/",filename,".csv"))

#Set up the plot area
plot(1:2, axes=FALSE, xlab="", ylab="",main="")

#Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])



}


# Graph Growth
graphGrowthUVA <- function(){
  dataFiles <- processCsv('data_files')
  users1 <- processCsv('users')
  models <- processCsv('models')
  sops <- processCsv('sops')
  publs <- processCsv('publications')

  #    newdata <- users[order(users$ncreated_at),]
  plotlabels<- c("Year", "Assets Accumulation")
  legends <- c("Users","Data files", "Models", "Total")
  
  users<-filterByPeriod(users1, as.Date("2010-01-01"), as.Date("2013-12-12"))
  models<-filterByPeriod(models, as.Date("2010-01-01"), as.Date("2013-12-12"))
  dataFiles<-filterByPeriod(dataFiles, as.Date("2010-01-01"), as.Date("2013-12-12"))
  sops<-filterByPeriod(sops, as.Date("2010-01-01"), as.Date("2013-12-12"))
  publs<-filterByPeriod(publs, as.Date("2010-01-01"), as.Date("2013-12-12"))

  dataFiles<- joinSize(dataFiles, "data_files")
  models<- joinSize(models, "models")
  sops<- joinSize(sops, "sops")



  plot(users$ncreated_at, users$id, type='s', log="y",col="red", xlab=plotlabels[1],ylab=plotlabels[2])# ,xlim=c(14830,15700))
  res = lm(users$id~users$ncreated_at)

  # plot(dataFiles$ncreated_at, dataFiles$size)

  # abline(res)
  legend("topleft", legends, fill = c("red","blue","green","brown"), col = c("red","blue","green","brown"),border = "black")
  
  dataFiles<-dataFiles[order(dataFiles$ncreated_at),]
  dataFiles$index<-c(1:length(dataFiles$id));
  # lines(dataFiles$ncreated_at, dataFiles$index, type='p', col="blue")
  res = lm(dataFiles$index~dataFiles$ncreated_at)
  # abline(res)  
  
  
  models<-models[order(models$ncreated_at),]
  models$index<-c(1:length(models$id));
  # lines(models$ncreated_at, models$index, type='p', col="green")
  res = lm(models$index~models$ncreated_at)
  # abline(res) 
  
  sops<-sops[order(sops$ncreated_at),]
  sops$index<-c(1:length(sops$id));
  # lines(sops$ncreated_at, sops$index, type='p', col="purple")
  res = lm(sops$index~sops$ncreated_at)
  # abline(res)    
  
  total <- append(sops$ncreated_at, dataFiles$ncreated_at) 
  total <- append(total, models$ncreated_at)
  total3 <- dataFiles$ncreated_at
  total2 <- models$ncreated_at
  
  total<-sort(total)
  idt <- c(1:length(total))
  idt3 <- c(1:length(total3))
  
  
  idt2 <- c(1:length(total2))   
  totald <- data.frame(total, idt)
  lines(total, idt, type='s', col="brown")

  # par(new=TRUE)
  # dbGrowth<-getAccSize()

  # lines(total3, idt3, type='p', col="blue")
  # lines(total2, idt2, type='p', col="green")
  
  res = lm(totald$idt~totald$total)
  # abline(res)
  abline(v=eval10)    
  abline(v=eval11)    
  abline(v=eval12)    
  abline(v=eval13)    
}


# Graph users uploads
graphUploadByUser <- function(){
  #    data <- processCsv('data_files')
  data  <- getAllresc(0)
  labs<- c("Time","freq","Contributions over time")
  #data<- sort(data, by="contributor_id")
  hist(data$ncreated_at[data$contributor_id != "NULL"], breaks=36, freq=TRUE,
       xlab=labs[1],main=labs[3], col=c("#823483"), border=NA)
  plot(data$contributor_id[data$contributor_id != "NULL"], col=c("#823483"), border=NA)
} 


# Distribution of versions
graphVerDistribution <- function(data){

  rdata<- data$version[data$contributor_id != "NULL"]
  if(NROW(rdata)){
    labs<- c("Versions" , "freq", "Distribution of Assets Version")
    hist(rdata, breaks=(max(data$version)*2),  col=c("#823483"), border=NA, freq=TRUE,main=labs[3], xlab=labs[1], xlim=c(0,7), ylim=c(0,120))#,yaxt="n")
print(rdata)
    b<-as.data.frame(table(rdata))
    print(b)
    x <- b[!(b$rdata %in% c(0)),]
  #  plot((as.numeric(x$rdata)),(as.numeric(x$Freq)), col=c("#823483"),pch = 19, log="xy",main="MyExperiment Contributions", ylab="Number of Contributors",xlab="Number of contributions")
  }  

  return(rdata)   
}


# Distribution of versions
graphFileSizeDistribution <- function(data){

  # data<-joinSize(data,type)

  rdata<- as.numeric(data$filesize)/(1024) #[data$contributor_id != "NULL"]
  br<-(max(rdata)/500)
  if(NROW(rdata)){
    labs<- c("Filesize (kb)" , "freq", "Distribution of Assets Size")
    hist(rdata,  breaks=br, col=c("#823483"),  border=NA,freq=TRUE,main=labs[3], xlab=labs[1], xlim=c(0,5000), ylim=c(0,120))
  }   
  return(rdata) 
} 




graphDSPEffect<-function(){
  # layout(matrix(c(1,2,3,4),2,2,))
  layout(matrix(c(1,2),1,2))
  
  data <-processCsv("data_files")
  r <- data[data$ncreated_at < "2011-05-05",]
  b<-graphVerDistribution(r)
  
  mtext( "Before 2011-May", side=3)

  # Size distribution
  # data2 <-joinSize(data,"data_files")
  # r2 <- data2[data2$ncreated_at < "2011-05-05",]
  # graphFileSizeDistribution(r2)

  # mtext( "Distribution of Assets Size", side=2)

  r <- data[data$ncreated_at > "2011-05-05",]
  a<-graphVerDistribution(r)

  test<-ks.test(a,b)
  
  print(test$p)

  #   data <- processCsv('models')
  #   r <- data[data$ncreated_at < "2011-05-05",]
  #   graphVerDistribution(r)
  #   
  #   
  #   r <- data[data$ncreated_at > "2011-05-05",]
  #   graphVerDistribution(r)
  mtext( "After 2011-May", side=3)
  mtext(paste("p-v=", round(test$p, digits = 3) ,""), side = 4)

  # Size distribution
  # r2 <- data2[data2$ncreated_at > "2011-05-05",]
  # graphFileSizeDistribution(r2)  
  
  return( (ks.test(a,b))
)

}

# Distribution of users contributions
graphDistrEvents <- function(s){
  data <- processCsv('data_files')
  #Obtain frequecies by users
  labels<-c("Frequency of Uploads", "Users", "Uploads distribution (Data only)")
  lims<-c(0,20,0,20)
  sop<- getContributors3() #as.data.frame(table(data$contributor_id))
  #remove nulls
  nulls<-as.numeric(rownames(sop[sop$contributor_id == 'NULL',]))
  sop$Freq[nulls]<-0
  hist(sop$Freq,breaks=max(sop$Freq), col=c("#823483"), border=NA, xlab=labels[1], ylab=labels[2],main=labels[3], ylim= lims[3:4],freq=TRUE) 
  #lines(density(sop$Freq), col="red")  
  return(TRUE)  
} 

getFileSharing<- function(aid){
  data <- processCsv('data_file_auth_lookup')
  data_2<- data[data$asset_id == aid,]
  data_2[data_2$can_download == 1,]
  return(data_2)


# a<-getFileSharing(123)
# > a$contributor_id <- a$user_id
# > b<-joinPerson(a)
# > View(b)
# > b$person_id <- b$user_id
# > View(b)
# > c<-joinAssoc(b)
# > View(c)
# > d<-filterOutSysDB(c)
# > View(d)
  
} 

getCreators<- function(){
  data <- processCsv('data_file_auth_lookup')
  r <- data[data$can_edit == 1,]
  creators <-sapply(r$user_id , selectC) 
  
} 

getSizeD <- function() {
  d <- processCsv('filesizes-and-versions-DataFile')
  return(d)
}




joinDiversityCoeff <- function(){
  
  library(vegan)
  library(reldist)
  
  data<-getContributors3()
  projects<-processCsv("projects")
  wg<-processCsv("work_groups")
  data<-merge(data,wg, by="project_id")
  inst<-processCsv("institutions")
  colnames(inst)[1]<-"institution_id"
  data<-merge(data,inst, by="institution_id")
  
  loc_shannon <- function(s){
    diversity(data$institution_id[data$project_id == s], "shannon")
  }
  
  loc_gini <- function(s){
    gini(data$institution_id[data$project_id == s])
  }
  
  lang_shannon <- function(s){
    diversity(as.numeric(data$country[data$project_id == s]), "shannon")
  }
  
  lang_gini <- function(s){
    gini(as.numeric(data$country[data$project_id == s]))
  }  
  
  projects$loc_shannon<-sapply(projects$id,loc_shannon )
  projects$loc_gini<-sapply(projects$id,loc_gini)
  projects$lang_shannon<-sapply(projects$id, lang_shannon )
  projects$lang_gini<-sapply(projects$id, lang_gini)  
  projects$project_name<-projects$name
  return(projects)
} 

getAccSize<-function(asset_type) {
  dataFiles<-processCsv(asset_type)
  dataFiles<-joinSize(dataFiles,asset_type)
  rrr<-data.frame(dataFiles$ncreated_at, dataFiles$filesize)
  qas<-aggregate(rrr$dataFiles.filesize, by=list(rrr$dataFiles.ncreated_at), sum)
  qas[with(qas, order(Group.1)), ]
  qas$acc<-0
  qas$acc[1]<-qas$x[1]
  for (i in 2:NROW(qas)) {qas$acc[i] <- (qas$acc[i-1] + qas$x[i])}
  return(qas)
}

graphContvsSize<- function(){
  
  r<-projectContBySize2()
  
  rbPal <- colorRampPalette(c('red','blue'))
  #      barplot(r$ttl, names.arg=r$name, beside=TRUE,col=rainbow(15),cex.names=0.5,las=2) 
  
  r$Col <- rbPal(30)[as.numeric(cut(r$loc_shannon,breaks = 30))]      
  plot(r$siz,r$Freq, log="x",col=r$Col,pch = 19)
  
  r$Col2 <- rbPal(30)[as.numeric(cut(r$lang_shannon,breaks = 30))]   
  plot(log(r$siz),(r$Freq),col=r$Col2,pch = 19)

  #   r$Col2 <- rbPal(30)[as.numeric(cut(r$frequency.y,breaks = 30))]   
  #   plot(r$siz,r$Freq, log="x",col=r$Col2,pch = 19)
  
  r<-projectContBySize()
  r$Col <- rbPal(30)[as.numeric(cut(r$loc_gini,breaks = 30))]      
  plot(log(r$siz),log(r$Freq), col=r$Col,pch = 19)  
  
  r$Col <- rbPal(30)[as.numeric(cut(r$lang_gini,breaks = 30))]      
  plot(r$siz,r$Freq, log="yx",col=r$Col,pch = 19) 
  
}



