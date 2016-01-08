 #converts csv into dataframe
 processCsv <- function(filename){
   r <- read.csv(file=paste0("/Users/kristian/Documents/Rscripts/Sysmo/",filename,".csv"),sep=",",head=TRUE)
    if(length(r$created_at) > 1){
      r$ncreated_at <- as.Date(r$created_at)
      r$nupdated_at <- as.Date(r$updated_at)
    }
   if(length(r$contributor_id) > 1){
     r<-r[r$contributor_id != "NULL", ]
   }
   return(r)
 } 


 getAllresc<- function(uid){
   dataFiles <- processCsv('data_files')
   models <- processCsv('models')
   sops <- processCsv('sops')
  
   if(uid){
     dataFiles <- dataFiles[dataFiles$contributor_id == uid, ]
     models <- models[models$contributor_id == uid, ]
     sops <- sops[sops$contributor_id == uid, ]
     print(c("Sops: ", length(sops$ncreated_at)))
     print(c("Data: ", length(dataFiles$ncreated_at)))
     print(c("Models: ", length(models$ncreated_at))) 
     
   }
   
   total <- append(sops$ncreated_at, dataFiles$ncreated_at) 
   total <- append(total, models$ncreated_at)
   #total<-sort(total)

   contributor_id <- append(as.character(sops$contributor_id), as.character(dataFiles$contributor_id)) 
   contributor_id <- append(contributor_id, as.character(models$contributor_id))
   contributor_id <- as.numeric(contributor_id)

   idt <- c(1:length(total))
   ncreated_at <-total
   r <- data.frame(ncreated_at, idt, contributor_id)
   r <- r[order(ncreated_at),] 
   return(r)
 }
 
 
 # plots evens of each of user uploading
 plotHist <- function(user){    
   data <- getAllresc(user)
   print(user)
   if((length(data$ncreated_at))>0){
#      if((length(data$ncreated_at[data$contributor_id == user]))>0){
       numEvents<-length(unique(unlist(data$ncreated_at, use.names = FALSE)))
 #    hist(data$ncreated_at[data$contributor_id == user], breaks=36, freq=TRUE, col=c("#823483"), main=numEvents, ylim=c(0,10))    
     hist(data$ncreated_at, breaks=36, freq=TRUE, col=c("#823483"), main=numEvents, ylim=c(0,10), border=NA)    
   }
 }
 
 
 metaPlotHist <- function(){   
   
    users <- c(1:327)
    lapply(users, plotHist)
 }
 
# Contribution by project  
 projectContribution <- function(){
   data <- processCsv('data_files_projects')
   numberProjects<-length(unique(unlist(data$project_id, use.names = FALSE)))
   hist(data$project_id, breaks=numberProjects, freq=TRUE)
 }
 

# Graph Growth
 graphGrowth <- function(){
   dataFiles <- processCsv('data_files')
   users <- processCsv('users')
   models <- processCsv('models')
   sops <- processCsv('sops')
   #    newdata <- users[order(users$ncreated_at),]
   plotlabels<- c("Time", "Quantity")
   legends <- c("Users","Data files", "Models", "Total")
   
   
   
   plot(users$ncreated_at, users$id, type='l', col="red", ylim=c(0,500), xlab=plotlabels[1],ylab=plotlabels[2])# ,xlim=c(14830,15700))
   res = lm(users$id[users$ncreated_at > 14830]~users$ncreated_at[users$ncreated_at > 14830])
   abline(res)
   legend("topleft", legends, fill = c("red","blue","green","brown"), col = c("red","blue","green","brown"),border = "black")
   
   dataFiles<-dataFiles[order(dataFiles$ncreated_at),]
   dataFiles$index<-c(1:length(dataFiles$id));
  # lines(dataFiles$ncreated_at, dataFiles$index, type='p', col="blue")
   res = lm(dataFiles$index[dataFiles$ncreated_at > 14830]~dataFiles$ncreated_at[dataFiles$ncreated_at > 14830])
  # abline(res)  
   
   
   
   models<-models[order(models$ncreated_at),]
   models$index<-c(1:length(models$id));
  # lines(models$ncreated_at, models$index, type='p', col="green")
   res = lm(models$index[models$ncreated_at > 14830]~models$ncreated_at[models$ncreated_at > 14830])
  # abline(res) 
   
   
   sops<-sops[order(sops$ncreated_at),]
   sops$index<-c(1:length(sops$id));
  # lines(sops$ncreated_at, sops$index, type='p', col="purple")
   res = lm(sops$index[sops$ncreated_at > 14830]~sops$ncreated_at[sops$ncreated_at > 14830])
  # abline(res)    
   
   
   total <- append(sops$ncreated_at, dataFiles$ncreated_at) 
   total <- append(total, models$ncreated_at)
   total<-sort(total)
   idt <- c(1:length(total))
   totald <- data.frame(total, idt)
   lines(total, idt, type='l', col="brown")
   res = lm(totald$idt[totald$total > 14830]~totald$total[totald$total > 14830])
   abline(res)  
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
 graphVerDistribution <- function(){
   data <- processCsv('data_files')
   rdata<- data$version[data$contributor_id != "NULL"]
   labs<- c("Versions" , "freq", "Distribution of Assets Version")
   hist(rdata, breaks=(max(data$version)*2),  col=c("#823483"), border=NA, freq=TRUE,
        main=labs[3], xlab=labs[1]
   )#,yaxt="n")
   
   #foo <- data$version[data$contributor_id != "NULL"]
   #curve(dnorm(x, mean=mean(foo), sd=sd(foo)), add=TRUE)
   #lines(density(rdata), col="red") 
   #par(new=TRUE)
   #plot(density(rdata), xlab="", ylab="", main="", xaxt="n", yaxt="n", col="red")
   #axis(2, at=rdata,labels=rdata, col.axis="black", las=2)
   #axis(4, at=rdata,labels=rdata, col.axis="black", las=2)
   
 } 
 
 
 # Distribution of users contributions
 graphDistrEvents <- function(s){
   data <- processCsv('data_files')
   #Obtain frequecies by users
   labels<-c("Frequency of Uploads", "Users", "Uploads distribution (Data only)")
   lims<-c(0,20,0,20)
   sop<- getContributors(s) #as.data.frame(table(data$contributor_id))
   #remove nulls
   nulls<-as.numeric(rownames(sop[sop$contributor_id == 'NULL',]))
   sop$Freq[nulls]<-0
   hist(sop$Freq,breaks=max(sop$Freq), col=c("#823483"), border=NA, xlab=labels[1], ylab=labels[2],main=labels[3], ylim= lims[3:4],freq=TRUE) 
   #lines(density(sop$Freq), col="red")    
 } 
 
 
 # Distribution of users contributions
 graphPop <- function(){
   data  <- processCsv('data_files')
   users <- processCsv('users')
   
  
 } 
 
 # Distribution of users contributions
 graphActivty <- function(){
   
   myfunc <- function(s) {
     last  <- tail(data_2$ncreated_at[data_2$contributor_id == s], n=1)
     first <- head(data_2$ncreated_at[data_2$contributor_id == s], n=1)
     if(last[1] == first[1]){
#        if(last[1] != first[1]){ change for different plot
         return(c(14200))
     }
     return(last)
   }

   myfunc3 <- function(s) {
     head(data_2$ncreated_at[data_2$contributor_id == s], n=1)
   }   
   
   myfunc2 <- function(s) {
     users$ncreated_at[users$id == s]
   }
   
   labels <- c("Registration Date", "Last Upload", "Users Activity")
   #data  <- processCsv('data_files')
   data  <- getAllresc(0)
   users <- processCsv('users')
   disci <- processCsv('disciplines_people')
   
   #contrList <- unique(unlist((data$contributor_id[data$contributor_id != "NULL"]), use.names = FALSE))
   contrList <- getContributors(0)
   # merge disciplines with data
   colnames(disci)[2] <- "contributor_id"
   data_2<- merge(data, disci, by="contributor_id", all.x=TRUE)   
   
   
   contr<-contrList$contributor_id  #levels(contrList)
   last_upload <-sapply(contr , myfunc)
   last_upload_2 <- as.Date(last_upload, origin = "1970-01-01")
#    last_upload_x <-sapply(contr , myfunc3)
#    last_upload_3 <- as.Date(last_upload_x, origin = "1970-01-01")   
   registry <-sapply(contr , myfunc2)
   registry_2 <- as.Date(registry, origin = "1970-01-01")
   #contr_2 <- contr[1:47]
   #sopa <- data.frame(contr_2, kaka2,pola2 )
   
   totaldf<-data.frame(registry_2, last_upload_2)
   
   
   
   plot(totaldf[totaldf$last_upload_2 != 14200,], ylim=c(14200,15735), xlim=c(14200,15735), xlab=labels[1],ylab=labels[2], main=labels[3]) #,  col=contrList$project_id)
   #Linear regression
#    points(registry_2, last_upload_3, col = "red",pch='*')
#    print(last_upload_3)
   ss<- totaldf[totaldf$last_upload_2 != 14200,]
   res = lm(ss$last_upload_2~ss$registry_2)
#    res2 = lm(last_upload_3~registry_2)
   abline(res, col="red")
#    abline(res2, col="red")
   #  abline(v=14730, col="blue")
   
   #    abline(res, col="red")
   #projects <- processCsv('projects')
   #legend(1, 2, projects$name, cex=0.8,col=projects$id, pch=21:22, lty=1:2);
 } 
 
 getContributors <- function(s){
   data <- processCsv('data_files')
   users <- processCsv('users')
   people <- processCsv('people')
   
  # sop<-getProjectsSubset() #as.data.frame(table(data$contributor_id))
   contrIds<-as.data.frame(table(data$contributor_id))
   
   colnames(people)[1] <- "person_id"
   peoplePlusUsers<- merge(users, people, by="person_id", all.x=TRUE)   
   
   colnames(contrIds)[1] <- "id"
   peoplePlusUsersData<- merge(contrIds, peoplePlusUsers, by="id", all.x=TRUE) 
   keeps <- c("person_id","id", "Freq", "email.y","ncreated_at.x","nupdated_at.x" )
   r<- peoplePlusUsersData[keeps]
   colnames(r)[2] <- "contributor_id"
   colnames(r)[5] <- "ncreated_at"
   colnames(r)[6] <- "nupdated_at"
   
   if(s){
   subs <- processCsv('project_subscriptions')
   subs2<-subset(subs, project_id == 9 | project_id == 2 | project_id == 8 | project_id == 6 | project_id == 3)
   #  9,2,8,3,6
   
   contrIds_2 <- merge(r, subs2, by="person_id", all.x=TRUE)
   newdata <- contrIds_2[order(contrIds_2$project_id),]
   r <- newdata[!is.na(newdata$project_id),]
   #keeps <- c("person_id","project_id", "Freq","email.y")
   #return(newdata2)   
   }
   return(r[r$contributor_id != "NULL",])
#    write.csv(peoplePlusUsersData$email.y)
 }
 
 getLogs <- function(){
  data <- processCsv('activity_logs')
  hist(data$activity_loggable_id, freq=TRUE, breaks=36)
   
 }
 

