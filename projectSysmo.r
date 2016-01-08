
getProjInfo<-function() {
  r<-processCsv("projects")
  r$interview<-c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)
  r$fund<-c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,"N","N")
  r$openShare<-c(FALSE)
  r$activity<-c(FALSE)

  r$openShare[1]<-TRUE
  r$openShare[8]<-TRUE
  r$openShare[15]<-TRUE
  r$openShare[14]<-TRUE
  r$openShare[3]<-TRUE

  r$openShare[1]<-TRUE
  r$openShare[2]<-TRUE
  r$openShare[15]<-TRUE
  r$openShare[14]<-TRUE
  r$openShare[3]<-TRUE


  return(r)
}


getSummary<-function() {
  x<-getProjInfo()
  y<-getProjectMore()
  r<-merge(x,y, by="name")
  return(r)
}




# Contribution by project  
graphProjectContribution <- function(){
  data <- processCsv('data_files_projects')
  numberProjects<-length(unique(unlist(data$project_id, use.names = FALSE)))
  hist(data$project_id, breaks=numberProjects, freq=TRUE)
}




## Acitivyt frquecy vs number of particpants with a gradient
graphProjMore<-function() {
  r<-getProjectMore()
  sp <- ggplot(r, aes(x=project_name, y=actFreq))  + geom_point()
  sp + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

  sp <- ggplot(r[1:12], aes(x=participants, y=actFreq, label=project_name))  + geom_point() + geom_text(size=5)
  sp + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_point(aes(colour = total, size=200)) + scale_colour_gradient(trans = "log", low="red")
    sp
}



projectContBySize2<- function() {
  dataf<-processCsv("data_files")
  d<-joinSize(dataf,'data_files')
  
  selectC2<- function(s){
    median(d$filesize[d$contributor_id == as.character(s)])
    # sum(d$filesize[d$contributor_id == as.character(s)])
  }  
  
  v<-getContributors3()    
  v$siz<-sapply(v$contributor_id,selectC2)
  
  #     data <- read.csv(file="/Users/kristian/Documents/Rscripts/Sysmo/seekDwigini.r",sep=",",head=TRUE)
  data <- joinDiversityCoeff()
  
  
  #     colnames(data)[4]   <- "name"
  v<-merge(v,data, by="project_name")
  
  return(v)
} 


## Gives you the files size distribution of all projects by project
graphProjSizeDis <- function() {

  layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

  projs <- processCsv('projects')
  d<-getSizeD()
  #    topl<-max(d$filesize)/10
  for(i in 1:NROW(c(1:12))) {
    dp<-(d[d$project_id == i,])
    if(NROW(d[d$project_id == i,]) >1){
      hist(dp$filesize, main=as.character(projs$name[projs$id == i]), breaks=NROW(dp))
    }
  }
}



## Gini info plus assets uploaded table
projectContBySize<- function() {
  dataf<-processCsv("data_files")
  d<-joinSize(dataf,'data_files')
  projs <- processCsv('projects')
  for(i in 1:NROW(c(1:12))) {
    projs$siz[i] <- median(d$filesize[d$project_id == i])
    # projs$siz[i] <- sum(d$filesize[d$project_id == i])
  }
  
  data <- processCsv('data_files_projects')
  projNum<-as.data.frame(table(data$project_id))
  colnames(projNum)[1] <- "project_id"
  colnames(projs)[1]   <- "project_id"
  
  r<-merge(projs,projNum, by="project_id")
  
  #    data <- read.csv(file="/Users/kristian/Documents/Rscripts/Sysmo/seekDwigini.r",sep=",",head=TRUE)
  #    colnames(data)[4]   <- "name"
  data <- joinDiversityCoeff()
  r<-merge(r,data, by="name")


  return(r)
}


getProjectMore<-function() {

  projs <- processCsv('projects')
  data  <- processCsv('data_files_projects')
  pdata<-as.data.frame(table(data$project_id))
  sops  <- processCsv('sops_projects')
  psops<-as.data.frame(table(sops$project_id))
  mods  <- processCsv('models_projects')
  pmods<-as.data.frame(table(mods$project_id))

  a<-merge(pdata,pmods, by="Var1", all.x=TRUE)
  b<-merge(a,psops, by="Var1", all.x=TRUE)
  b[is.na(b)] <- 0
  b$total<-(b$Freq.x+b$Freq.y+b$Freq)

  colnames(b)[1]   <- "project_id"
  colnames(projs)[1]   <- "project_id"
  projs$project_name<-projs$name
  r<-merge(b,projs, by="project_id", all.x=TRUE)



  # users<-processCsv("users")
  # users<-joinAssoc(users)
  users <-getMembers(0) 

  users<-users[users$ncreated_at <= "2010-01-01",]

library(data.table)

  ds <- as.data.table(users)
  setkeyv(ds, cols=c("project_name"))
  ds <- ds[, list(count=.N), by=c("project_name")]

  r<-merge(r,ds, by="project_name", all.x=TRUE)

  # r$lifetime<-c(3,3,3,1,3,1,3,3,3,3,3,3)
  # r$lifetime<-c(3,3,3,3,3,3,3,3,3,3,3)
  r$lifetime<-3
  # r$active<-(r$total/((r$count/sum(r$count))*r$lifetime))
  r$active<-(r$count/(365*r$lifetime))

  print(r)


# ------------

# careful these are different. particualry what you are bining is not the same. 
# chek it out date of what? you want to add evertyinhg including uploads


  resc<-getAllresc(0)
  resc<-joinPerson(resc)
  # resc<-joinAssoc(resc)

  df <- as.data.table(resc)

  df <- df[ which(df$ncreated_at<= eval11), ]
  # df <- df[ which(df$ncreated_at<= "2010-01-01" & mydata$age > 65), ]


  setkeyv(df, cols=c('ncreated_at',"project_name"))
  df <- df[, list(count=.N), by=c('ncreated_at',"project_name")]

  df2 <- as.data.table(df)
  setkeyv(df2, cols=c("project_name"))
  df2 <- df2[, list(count=.N), by=c("project_name")]
  df2$actFreq<-0
  df2$lifetime<-3
  df2$lifetime[df2$project_name == "SilicoTryp"]<-2
  df2$lifetime[df2$project_name == "Noisy-Strep"]<-2

  df2$actFreq <- (df2$count)/(df2$lifetime*365)
  print(df2)
  r$participants<-r$count
  df2<-merge(df2,r[c("participants","project_name","total")], by="project_name", all.x=TRUE)
  df2$participants[df2$project_name == "SilicoTryp"]<-12
  df2$participants[df2$project_name == "Noisy-Strep"]<-9
  df2$actFreq[df2$project_name == "SysMO DB"]<-0


  r<-df2
return(r)
}



graphProjGrowth <-function(p) {



graphProjGrowthSops(p)
graphProjGrowthData(p)
graphProjGrowthMod(p)

}





graphProjGrowthSops <- function(p) {
  projects<-processCsv("projects")
  dataFiles <- processCsv('sops')
  dataFiles$sop_id <- dataFiles$id
  dataFP <- processCsv('sops_projects')  
  r<-merge(dataFP,dataFiles, by="sop_id", all.x=TRUE)
  r<-r[!(r$contributor_id %in% c(NA)),]
  r<-r[(r$project_id %in% c(p)),]
  r <-r[with(r, order(ncreated_at)), ]

  if(NROW(r) != 0){

  r$y<- c(1:NROW(r))
  plot(r$ncreated_at, r$y, main=projects$name[p],type='b',col="blue", ylim=c(0,35),xlim=c(eval10,eval13))

  }
}

graphProjGrowthData <- function(p) {

  dataFiles <- processCsv('data_files')
  dataFiles$data_file_id <- dataFiles$id
  dataFP <- processCsv('data_files_projects')  
  r<-merge(dataFP,dataFiles, by="data_file_id", all.x=TRUE)
  r<-r[!(r$contributor_id %in% c(NA)),]
  r<-r[(r$project_id %in% c(p)),]
  r <-r[with(r, order(ncreated_at)), ]

  if(NROW(r) != 0){
  r$y<- c(1:NROW(r))
  lines(r$ncreated_at, r$y, type='b',col="red", ylim=c(0,35),xlim=c(eval10,eval13))
  }


}

graphProjGrowthMod <- function(p) {

  dataFiles <- processCsv('models')
  dataFiles$model_id <- dataFiles$id
  dataFP <- processCsv('models_projects')  
  r<-merge(dataFP,dataFiles, by="model_id", all.x=TRUE)
  r<-r[!(r$contributor_id %in% c(NA)),]
  r<-r[(r$project_id %in% c(p)),]
  r <-r[with(r, order(ncreated_at)), ]

  if(NROW(r) != 0){
  r$y<- c(1:NROW(r))
  lines(r$ncreated_at, r$y, type='b',col="green", ylim=c(0,35),xlim=c(eval10,eval13))
  }


}
