UsersTable <- R6Class("UsersTable",
  public = list(
    data = "data.frame",
    loadtable = function(type){
      self$set_data(processCsv("users"))
    },
    set_data = function(val) {
      print(class(val))
      before<-NROW(self$data)
      self$data <- val
      after<-NROW(self$data)
      if(before != after){
        print("You changed the size of the data")
        print(before)
        print(after)
      }
    },
    getMembers= function(project=0) {
      # users<-processCsv("users")
      r<-users[!duplicated(self$data[,c('person_id')]),]
      r<-joinAssoc(r)
      # r<-filterOutSysDB(r)
      # r<-filterOutPals(r)
      if(project) r<-filterProject(r,project)
      r<-guessDiscipline(r)

      drops <- c("crypted_password","email","salt","remember_token","remember_token_expires_at","activation_code","reset_password_code","reset_password_code_until","posts_count","uuid")
      r<-r[,!(names(r) %in% drops)]
      return(r)
    },
    joinDiscipline= function(){

      if(is.null(self$data$person_id)){
        print("wrong data. I need person_id")
        stop()
      }

      disci <- processCsv('disciplines_people')
      disci<-disci[!duplicated(disci[,c('person_id')]),]

      titles <- processCsv('disciplines')
      titles$discipline_id <- titles$id

      disci<- merge(disci,titles, by="discipline_id", all.x=TRUE)
      keeps<- c("person_id","discipline_id","title")
      r<- merge(self$data, disci[keeps], by="person_id", all.x=TRUE)
      self$set_data(r)
    },
    joinRole= function(){

      if(is.null(self$data$person_id)){
        print("wrong data. I need person_id")
        stop()
      }

      data<- processCsv("group_memberships_project_roles")

      gm<- processCsv("group_memberships")
      gm$group_membership_id<- gm$id

      keeps<- c("project_role_id","group_membership_id")
      memberships<- merge(gm, data[keeps], by="group_membership_id", all.x=TRUE)

      memberships<-memberships[!duplicated(memberships[,c('person_id')]),]

      keeps<- c("project_role_id","person_id")
      r<- merge(self$data, memberships[keeps], by="person_id", all.x=TRUE)
      r$project_role_id <- as.character(r$project_role_id)

      r$project_role_id[is.na(r$project_role_id)]<-"Member"
      r$project_role_id[r$project_role_id != 6]<-"Member"
      r$project_role_id[r$project_role_id == 6]<-"PAL"

      r$project_role_id <- as.factor(r$project_role_id)

      # r<-r[r$person_id != "NULL",]
      self$set_data(r)
    },
    joinAssoc= function(){
      if(is.null(self$data$person_id)){
        print("wrong data. I need person_id")
        stop()
      }
      projs <- processCsv('projects')
      colnames(projs)[1] <- "project_id"
      colnames(projs)[2] <- "project_name"
      subs <- processCsv('project_subscriptions')
      subs<-subs[!duplicated(subs[,c('person_id')]),]

      s<- merge(subs,projs, by="project_id", all=TRUE)
      keeps<- c("person_id","project_id","project_name")
      r<- merge(self$data, s[keeps], by="person_id", all.x=TRUE)
      # r<-r[!duplicated(r[,c('id')]),]
      # r<-r[complete.cases(r),]
      r<-r[r$person_id != "NULL",]
      self$set_data(r)
    },
    filterOutSysDB= function(){

      if(is.null(self$data$project_id)){
        self$joinAssoc()
      }
      r<-users[!(self$data$project_id %in% c(12)),]
      r<-users[!(self$data$person_id %in% c(136,411,433,134,132,372,205,355,318,48,133,1,130)),]
      self$set_data(r)
    },
    filterProject= function(project_id){

      if(is.null(self$data$project_id)){
        self$joinAssoc()
      }
      r<-users[(self$data$project_id %in% c(project_id)),]
      r<-r[r$person_id != "NULL",]
      self$set_data(r)
    },
    filterOutPals= function(){

      if(is.null(self$data$person_id)){
        print("wrong data. I need person_id")
        stop()
      }

      self$joinRole()

      r<-r[!(self$data$project_role_id %in% c(6)),]
      r<-r[!duplicated(r[,c('person_id')]),]

      r<-r[r$person_id != "NULL",]
      self$set_data(r)
    },
    joinPersonInfo= function(){

      if(is.null(self$data$person_id)){
        print("wrong data. I need person_id")
        stop()
      }

      people <- processCsv('people')
      colnames(people)[1] <- "person_id"

      keeps<- c("person_id","email","first_name","last_name","skype_name","description")
      r<- merge(self$data, people[keeps], by="person_id", all.x=TRUE)
      r<-r[!duplicated(r[,c('person_id')]),]
      r<-r[r$person_id != "NULL",]
      self$set_data(r)
    },
    guessDiscipline=function(){

      self$joinDiscipline()
      r<-self$data
      if(is.null(self$data$contributor_id)){
        self$data$contributor_id<- users$id
        print("I need contributor_id guessDiscipline")
        # stop()
      }

      data <- processCsv('data_files')
      data<-as.data.frame(table(data$contributor_id))
      data$contributor_id<-data$Var1
      # colnames(data)[1] <- "contributor_id"

      keeps<- c("contributor_id","Freq")
      r<- merge(self$data, data[keeps], by="contributor_id", all.x=TRUE)

      for(i in 1:NROW(r)) {
        if(!is.na(r$Freq[i]) && is.na(r$discipline_id[i])){
          r$discipline_id[i]<-2
          r$title[i]<-"Experimentalist"
        }
      }

      data <- processCsv('models')
      data<-as.data.frame(table(data$contributor_id))
      data$contributor_id<-data$Var1
      data$Freq2<-data$Freq
      # colnames(data)[1] <- "contributor_id"
      # colnames(data)[2] <- "Freq2"

      keeps<- c("contributor_id","Freq2")
      r<- merge(r, data[keeps], by="contributor_id", all.x=TRUE)

      for(i in 1:NROW(r)) {
        if(!is.na(r$Freq2[i]) && is.na(r$discipline_id[i])){
          r$discipline_id[i]<-1
          r$title[i]<-"Modeller"
        }
      }

      webdata<-processCsv("webdata")
      webdata<-webdata[!is.na(webdata$discipline_id),]
      webdatar<-webdata[c("discipline_id", "contributor_id")]
      webdatar$dis<-webdatar$discipline_id

      drops <- c("discipline_id")
      webdatar[,!(names(webdatar) %in% drops)]
      keeps<-c("contributor_id","dis")
      r<-merge(r,webdatar[keeps],by="contributor_id", all.x=TRUE)

      a<-r[is.na(r$discipline_id),]
      a$discipline_id<-a$dis
      b<-r[!is.na(r$discipline_id),]
      r<-rbind(a,b)

      drops <- c("Freq","Freq2","dis")
      r<-r[,!(names(r) %in% drops)]
      self$set_data(r)
    }
  )
)


joinPerson<- function(users){

  if(NROW(users$person_id) ){
    print("person_id already exist")
    return(users)
  }

  if(is.null(users$contributor_id) ){
    print("wrong data. I need contributor_id")
    stop()
  }

  disci <- processCsv('users')
  disci$contributor_id<-disci$id
  # colnames(disci)[1] <- "contributor_id"

  keeps<- c("person_id","contributor_id")
  r<- merge(users, disci[keeps], by="contributor_id", all.x=TRUE)
  return(r)
}

getMembers<-function(project=0) {
  users<-processCsv("users")
  r<-users[!duplicated(users[,c('person_id')]),]
  r<-joinAssoc(r)
  # r<-filterOutSysDB(r)
  # r<-filterOutPals(r)
  if(project) r<-filterProject(r,project)
  r<-guessDiscipline(r)

  drops <- c("crypted_password","email","salt","remember_token","remember_token_expires_at","activation_code","reset_password_code","reset_password_code_until","posts_count","uuid")
  r<-r[,!(names(r) %in% drops)]
  return(r)
}


joinDiscipline<- function(users){

  if(is.null(users$person_id)){
    print("wrong data. I need person_id")
    stop()
  }

  disci <- processCsv('disciplines_people')
  disci<-disci[!duplicated(disci[,c('person_id')]),]

  titles <- processCsv('disciplines')
  titles$discipline_id <- titles$id

  disci<- merge(disci,titles, by="discipline_id", all.x=TRUE)
  keeps<- c("person_id","discipline_id","title")
  r<- merge(users, disci[keeps], by="person_id", all.x=TRUE)
  return(r)
}

joinRole<- function(users){

  if(is.null(users$person_id)){
    print("wrong data. I need person_id")
    stop()
  }

  data<- processCsv("group_memberships_project_roles")

  gm<- processCsv("group_memberships")
  gm$group_membership_id<- gm$id

  keeps<- c("project_role_id","group_membership_id")
  memberships<- merge(gm, data[keeps], by="group_membership_id", all.x=TRUE)

  memberships<-memberships[!duplicated(memberships[,c('person_id')]),]

  keeps<- c("project_role_id","person_id")
  r<- merge(users, memberships[keeps], by="person_id", all.x=TRUE)
  r$project_role_id <- as.character(r$project_role_id)

  r$project_role_id[is.na(r$project_role_id)]<-"Member"
  r$project_role_id[r$project_role_id != 6]<-"Member"
  r$project_role_id[r$project_role_id == 6]<-"PAL"

  r$project_role_id <- as.factor(r$project_role_id)


  # r<-r[r$person_id != "NULL",]
  return(r)
}

joinAssoc<- function(users){

  if(is.null(users$person_id)){
    print("wrong data. I need person_id")
    stop()
  }

  projs <- processCsv('projects')
  colnames(projs)[1] <- "project_id"
  colnames(projs)[2] <- "project_name"
  subs <- processCsv('project_subscriptions')
  subs<-subs[!duplicated(subs[,c('person_id')]),]

  s<- merge(subs,projs, by="project_id", all=TRUE)
  keeps<- c("person_id","project_id","project_name")
  r<- merge(users, s[keeps], by="person_id", all.x=TRUE)
  # r<-r[!duplicated(r[,c('id')]),]
  # r<-r[complete.cases(r),]
  r<-r[r$person_id != "NULL",]
  return(r)
}

filterOutSysDB<- function(users){

  if(is.null(users$project_id)){
    users<-joinAssoc(users)
  }
  r<-users[!(users$project_id %in% c(12)),]
  r<-users[!(users$person_id %in% c(136,411,433,134,132,372,205,355,318,48,133,1,130)),]
  return(r)
}

filterProject<- function(users, project_id){

  if(is.null(users$project_id)){
    users<-joinAssoc(users)
  }
  r<-users[(users$project_id %in% c(project_id)),]
  return(r)
}



filterOutPals<- function(users){

  if(is.null(users$person_id)){
    print("wrong data. I need person_id")
    stop()
  }

  r<-joinRole(users)

  r<-r[!(r$project_role_id %in% c(6)),]
  r<-r[!duplicated(r[,c('person_id')]),]

  return(r[r$person_id != "NULL",])
}


joinPersonInfo<- function(users){

  if(is.null(users$person_id)){
    print("wrong data. I need person_id")
    stop()
  }

  people <- processCsv('people')
  colnames(people)[1] <- "person_id"

  keeps<- c("person_id","email","first_name","last_name","skype_name","description")
  r<- merge(users, people[keeps], by="person_id", all.x=TRUE)
  r<-r[!duplicated(r[,c('person_id')]),]
  r<-r[r$person_id != "NULL",]
  return(r)
}


guessDiscipline<- function(users){

  users<-joinDiscipline(users)
  r<-users
  if(is.null(users$contributor_id)){
    users$contributor_id<- users$id
    print("I need contributor_id guessDiscipline")
    # stop()
  }

  data <- processCsv('data_files')
  data<-as.data.frame(table(data$contributor_id))
  data$contributor_id<-data$Var1
  # colnames(data)[1] <- "contributor_id"

  keeps<- c("contributor_id","Freq")
  r<- merge(users, data[keeps], by="contributor_id", all.x=TRUE)

  for(i in 1:NROW(r)) {
    if(!is.na(r$Freq[i]) && is.na(r$discipline_id[i])){
      r$discipline_id[i]<-2
      r$title[i]<-"Experimentalist"
    }
  }

  data <- processCsv('models')
  data<-as.data.frame(table(data$contributor_id))
  data$contributor_id<-data$Var1
  data$Freq2<-data$Freq
  # colnames(data)[1] <- "contributor_id"
  # colnames(data)[2] <- "Freq2"

  keeps<- c("contributor_id","Freq2")
  r<- merge(r, data[keeps], by="contributor_id", all.x=TRUE)

  for(i in 1:NROW(r)) {
    if(!is.na(r$Freq2[i]) && is.na(r$discipline_id[i])){
      r$discipline_id[i]<-1
      r$title[i]<-"Modeller"
    }
  }

  webdata<-processCsv("webdata")
  webdata<-webdata[!is.na(webdata$discipline_id),]
  webdatar<-webdata[c("discipline_id", "contributor_id")]
  webdatar$dis<-webdatar$discipline_id

  drops <- c("discipline_id")
  webdatar[,!(names(webdatar) %in% drops)]
  keeps<-c("contributor_id","dis")
  r<-merge(r,webdatar[keeps],by="contributor_id", all.x=TRUE)

  a<-r[is.na(r$discipline_id),]
  a$discipline_id<-a$dis
  b<-r[!is.na(r$discipline_id),]
  r<-rbind(a,b)

  drops <- c("Freq","Freq2","dis")
  r<-r[,!(names(r) %in% drops)]


  return(r)
}


# plots evens of each of user uploading
graphHist <- function(user){
  data <- getAllresc(user)

  if((length(data$ncreated_at))>0){
    #      if((length(data$ncreated_at[data$contributor_id == user]))>0){
    numEvents<-length(unique(unlist(data$ncreated_at, use.names = FALSE)))
    #    hist(data$ncreated_at[data$contributor_id == user], breaks=36, freq=TRUE, col=c("#823483"), main=numEvents, ylim=c(0,10))
    hist(data$ncreated_at, breaks=36,freq=TRUE, col=c("#823483"), main=numEvents, ylim=c(0,10), border=NA)
    plot(data$ncreated_at,data$id)
  }
}



graphTenure<-function() {
  data<-joinTenure()
  data<-joinPerson(data)
  data<-joinDiscipline(data)

  data<-filterOutPals(data)
  data<-filterOutSysDB(data)

  labels<-c("Tenure","Log(Tenure)", "Contributions")
  # ,main=labels[1], ylab=labels[2], xlab=labels[3]
  # par(mfrow=c(1,3))


  # plot(log(data$tenure[data$discipline_id == 1]), data$Freq[data$discipline_id == 1])
  # res<-lm(data$Freq[data$discipline_id == 1]~data$tenure[data$discipline_id == 1])
  # abline(res, col="red")

  # plot(log(data$tenure[data$discipline_id == 2]), data$Freq[data$discipline_id == 2])
  # res<-lm(data$Freq[data$discipline_id == 2]~data$tenure[data$discipline_id == 2])
  # abline(res, col="red")

  plot(log(data$tenure), data$Freq,main=labels[1], ylab=labels[3], xlab=labels[2], col=c("#823483"), pch=19 )
  res<-lm(data$Freq~data$tenure)
  abline(res, col="red")


}


joinTenure <-function() {
  contr<-getAllresc(0)
  contr<-filterByPeriod(contr, eval11, eval12)

  contr2<-as.data.frame(table(contr$contributor_id))
  colnames(contr2)[1] <- "contributor_id"


  users<-processCsv("users")
  colnames(users)[1] <- "contributor_id"

  for (i in 1:NROW(users)) {
    users$tenure[i]<-(as.Date("2013-08-01")-as.Date(users$ncreated_at[i]))
  }

  keeps<-c("tenure","contributor_id")
  r<-merge(contr2,users[keeps], by="contributor_id", all.x=TRUE)
  return(r[r$Freq != 0,])
}



graphMetaHist <- function(){

  users <- c(1:327)
  lapply(users, graphHist)
}



graphActivty2 <- function(){
  users <- processCsv('users')
  colnames(users)[1] <- "contributor_id"
  users <- joinDiscipline(users)

  contributions  <- getAllresc(0)
  contributions[with(contributions, order(ncreated_at)), ]

  data<- merge(users, contributions, by="contributor_id", all.x=TRUE)

}

# Distribution of users contributions
graphActivty <- function(){
  layout(matrix(c(1)))
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

  # users<-filterOutSysDB(users)


  #contrList <- unique(unlist((data$contributor_id[data$contributor_id != "NULL"]), use.names = FALSE))
  # contrList <- getContributors(0)
  contrList <- getContributors3()
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




getContributors3 <- function(s){
  data  <- getAllresc(0)
  contrIds<-as.data.frame(table(data$contributor_id))
  # colnames(contrIds)[1] <- "contributor_id"
  contrIds$contributor_id<-contrIds$Var1

  users<-getMembers(0)
  keeps<-c("contributor_id","person_id","project_id","discipline_id","title")
  r<- merge(contrIds, users[keeps], by="contributor_id", all.x=TRUE)
  r<-r[complete.cases(r$person_id),]

  r<-r[with(r, order(project_id)), ]
  return(r)
}


selectC<- function(s){
  users <- processCsv('users')
  users[users$id == s,]

}


getContCreationFreq <- function(evalx1, evalx2){
  myf <- function(s){
    dtByC<-dt[dt$contributor_id == s,]
    r<-as.numeric(zoo(dtByC$ncreated_at))
    freq<-0
    for(i in 1:NROW(r)) {
      if((evaluations[2]) >= (r[i])  && (r[i]) >= (evaluations[1])){ freq<-freq+1 }
    }
    return(freq)
  }

  library(zoo)
  dt <- getAllresc(0)
  evaluations <- as.numeric(zoo(c(evalx1,evalx2)))

  contrList<- dt[!duplicated(dt[,c('contributor_id')]),]
  contributor_id<- contrList$contributor_id
  frequency <- sapply((contributor_id), FUN = myf)
  r<- data.frame(contributor_id,frequency )



  return(r)#[with(r, order(contributor_id)), ])
}

graphFreq <- function() {

  library(ggplot2)

  # layout(matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE))
  layout(matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE))

  #   p<- data.frame(start=c(eval10,eval11,eval12), end=c(eval11,eval12,eval13))
  #
  #
  #   for(i in 1:NROW(p)) {
  #     a<-getContCreationFreq(p$start[i],p$end[i])
  #     b<-getContributors(1)
  #     nn<-merge(a,b, by="contributor_id")
  #     nn<-nn[nn$frequency != 0,]
  #     op<-nn[!is.na(nn$discipline_id),]
  #   }



  a<-getContCreationFreq(eval11,eval12)
  # b<-getContributors(1)
  b<-getContributors3()
    # b<-filterOutPals(b)
  nn<-merge(a,b, by="contributor_id")
  nn<-filterOutSysDB(nn)
  nn<-nn[nn$frequency != 0,]
  op1<-nn[!is.na(nn$discipline_id),]

  #op[op$discipline_id == 3,]

  a<-getContCreationFreq(eval12,eval13)
  # b<-getContributors(1)
  b<-getContributors3()
    # b<-filterOutPals(b)
  nn<-merge(a,b, by="contributor_id")
  nn<-filterOutSysDB(nn)
  nn<-nn[nn$frequency != 0,]
  op2<-nn[!is.na(nn$discipline_id),]

  a<-getContCreationFreq(eval10,eval11)
  # b<-getContributors(1)
  b<-getContributors3()
  # b<-filterOutPals(b)
  nn<-merge(a,b, by="contributor_id")
  nn<-filterOutSysDB(nn)
  nn<-nn[nn$frequency != 0,]
  op<-nn[!is.na(nn$discipline_id),]

  ss<-max(max(op$frequency,op1$frequency,op2$frequency))
  topl<-c(0,ss)


  # sp<-ggplot(data=op, aes(x=frequency)) + geom_histogram(binwidth=.5)
  # sp + facet_grid(. ~ title)

  # sp1<-ggplot(data=op1, aes(x=frequency)) + geom_histogram(binwidth=.5)
  # sp1 + facet_grid(. ~ title)

  # sp2<-ggplot(data=op2 ) + geom_histogram(mapping=aes(x=frequency),binwidth=.5)
  # sp2 + facet_grid(. ~ title)

  ope<-op[op$discipline_id == 2,]
  ope1<-op1[op1$discipline_id == 2,]
  ope2<-op2[op2$discipline_id == 2,]

  opx<-op[op$discipline_id == 1,]
  opx1<-op1[op1$discipline_id == 1,]
  opx2<-op2[op2$discipline_id == 1,]
  hor<-c(0,300)

  plot(ope$contributor_id, ope$frequency, main=ope$title[ope$discipline_id == 2][1], ylim=topl, xlim=hor)
  plot(ope1$contributor_id,ope1$frequency, main=ope$title[ope$discipline_id == 2][1], ylim=topl, xlim=hor)
  plot(ope2$contributor_id,ope2$frequency, main=ope$title[ope$discipline_id == 2][1], ylim=topl, xlim=hor)

  plot(opx$contributor_id,opx$frequency, main=opx$title[op$discipline_id == 1][1], ylim=topl, xlim=hor)
  plot(opx1$contributor_id,opx1$frequency, main=opx$title[op$discipline_id == 1][1], ylim=topl, xlim=hor)
  plot(opx2$contributor_id,opx2$frequency, main=opx$title[op$discipline_id == 1][1], ylim=topl, xlim=hor)

  topl<-c(0,10)
  limx<-c(0,25)

  mod<- opx$title[op$discipline_id == 1][1]
  exp<- ope$title[ope$discipline_id == 2][1]
  per<-c(2010,2011,2012)


  hist(op$frequency[op$discipline_id == 2], ylim=topl, main="Experimentalist", xlab="", border=NA, col=c("#823483"))
  # mtext( "Experimentalist", side=2)

  hist(op1$frequency[op1$discipline_id == 2], ylim=topl, main="", xlab="", border=NA, col=c("#823483"))
  mtext( "Distribution of Contribution  per year by Contributor type", side=3)

  hist(op2$frequency[op2$discipline_id == 2], ylim=topl, main="", xlab="", border=NA, col=c("#823483"))

  print(describe(op$frequency[op$discipline_id == 2]))
  print(describe(op1$frequency[op1$discipline_id == 2]))
  print(describe(op2$frequency[op2$discipline_id == 2]))

  hist(op$frequency[op$discipline_id == 1], ylim=topl, main="Modeller", xlab=per[1], border=NA, col=c("#823483"))
  # mtext( "Modeller", side=2)

  hist(op1$frequency[op1$discipline_id == 1], ylim=topl, main="", xlab=per[2], border=NA, col=c("#823483"))
  hist(op2$frequency[op2$discipline_id == 1], ylim=topl, main="", xlab=per[3], border=NA, col=c("#823483"))


  print(describe(op$frequency[op$discipline_id == 1]))
  print(describe(op1$frequency[op1$discipline_id == 1]))
  print(describe(op2$frequency[op2$discipline_id == 1]))



  print(wilcox.test(op$frequency[op$discipline_id == 2],op$frequency[op$discipline_id == 1]))
  print(wilcox.test(op1$frequency[op$discipline_id == 2],op1$frequency[op$discipline_id == 1]))
  print(wilcox.test(op2$frequency[op$discipline_id == 2],op2$frequency[op$discipline_id == 1]))


  print(ks.test(op$frequency[op$discipline_id == 2],op$frequency[op$discipline_id == 1]))
  print(ks.test(op1$frequency[op$discipline_id == 2],op1$frequency[op$discipline_id == 1]))
  print(ks.test(op2$frequency[op$discipline_id == 2],op2$frequency[op$discipline_id == 1]))

}


filterByPeriod<- function(data,date1,date2){

  if(is.null(data$ncreated_at)){
    print("wrong data. I need ncreated_at")
    stop()
  }

  d1<- data[data$ncreated_at > date1,]
  d2<- d1[d1$ncreated_at < date2,]
  return(d2)
}


filterByDiscipline<- function(data, disc){

  if(is.null(data$discipline_id)){
    print("wrong data. I need discipline_id")
    stop()
  }
  r<- subset(data, discipline_id == disc)
  return(r)
}


getContSpecificFreq <- function(evalx1, evalx2, type){

  myf <- function(s){
    dtByC<-df[df$user_id == s,]
    freq<-0
    if(NROW(dtByC)){
      r<-as.numeric(zoo(dtByC$ncreated_at))
      for(i in 1:NROW(r)) {
        if((evaluations[2]) >= (r[i])  && (r[i]) >= (evaluations[1])){ freq<-freq+1 }
      }
    }
    return(freq)
  }

  library(zoo)
  data <- processCsv('activity_logs')
  dr<- data[data$culprit_type == "User",]
  dt<- dr[dr$action == type,]

# filter by discipline

  colnames(dt)[7] <- "contributor_id"
  ds<-joinPerson(dt)
  ds<-joinDiscipline(ds)
  df<- subset(ds, discipline_id == 1)

  df$user_id <- as.numeric(as.vector(df$contributor_id))

  dt <- getAllresc(0)
  evaluations <- as.numeric(zoo(c(evalx1,evalx2)))

  contrList<- df[!duplicated(df[,c('user_id')]),]
  contributor_id<- contrList$user_id
  frequency <- sapply(contributor_id, FUN = myf)
  r<- data.frame(contributor_id,frequency )
  return(r)
}

LogsTable <- R6Class("LogsTable",
  public = list(
    data = "data.frame",
    xuser = "numeric",
    type = "character",
    loadtable = function(){
      self$set_data(processCsv("activity_logs"))
    },
    set_data = function(val) {
      print(class(val))
      before<-NROW(val)
      self$data <- val
      after<-NROW(val)
      if(before != after){
        print("You changed the size of the data")
        print(before)
        print(after)
      }
      print(after)
    },
    ## ggplots all proejcts acitivty by type file
    getLogBy=function(type="create",xuser=454) {
      dt <- self$data
      # dt<- data[data$culprit_type == "User",]

      if(xuser){
          #dt<-dt[(dt$culprit_type %in% c("User")),]
      }
      else{
          dt<-dt[!(dt$culprit_type %in% c("User")),]
      }
      # dt<- dt[dt$action == type,]

      if(type=="create"){
        dt<-dt[(dt$action %in% c("create","update")),]
      }
      else{
          dt<-dt[(dt$action %in% c(type)),]
      }


      colnames(dt)[7] <- "contributor_id"
      # ds<-joinPerson(dt)
      # ds<-joinDiscipline(ds)
      # ds<-joinAssoc(ds)

      projs <- processCsv('projects')
      colnames(projs)[1] <- "project_id"
      colnames(projs)[2] <- "project_name"
      colnames(dt)[9] <- "project_id"
      keeps<- c("project_id","project_name")
      ds<- merge(dt, projs[keeps], by="project_id", all.x=TRUE)

      ds<-ds[!(ds$activity_loggable_type %in% c("Person")),]
      ds<-ds[!(ds$activity_loggable_type %in% c("Event")),]
      ds<-ds[!(ds$activity_loggable_type %in% c("User")),]
    #  return(ds)
      self$set_data(ds)
    },
    joinVersion=function(){
      ds<-self$data
      datafiles <- AssetsTable$new()
      datafiles$type <- "data_file"
      datafiles$loadtable("data_files")

      ds<- merge(ds, datafiles, by=c("id","type"), all.x=TRUE)

      models <- AssetsTable$new()
      models$type <- "model"
      models$loadtable("models")

      sops <- AssetsTable$new()
      sops$type <- "sop"
      sops$loadtable("sops")





    },
    graphLogs=function(){

      ds<-self$data

      ds$project_name <- factor(ds$project_name, levels = c("COSMIC",  "SysMO-LAB", "BaCell-SysMO","SUMO", "SilicoTryp","MOSES"))

      ds<-ds[(ds$action %in% c("create")),]
      ds<-ds[!(ds$activity_loggable_type %in% c("Publication")),]
      ds<-ds[!(ds$activity_loggable_type %in% c("Study")),]
      ds<-ds[!(ds$activity_loggable_type %in% c("Presentation")),]
      ds<-ds[!(ds$activity_loggable_type %in% c("Investigation")),]

      dx <- as.data.table(ds)
      setkeyv(dx, cols=c('ncreated_at',"project_name","activity_loggable_type"))
      ds <- dx[, list(count=.N), by=c('ncreated_at',"project_name","activity_loggable_type")]

      ds<-ds[(ds$project_name %in% c("COSMIC", "SUMO", "MOSES","SysMO-LAB","SilicoTryp", "BaCell-SysMO")),]

      ds$Date<-ds$ncreated_at
      ds$Asset_type<-ds$activity_loggable_type
      ds$Activity<-ds$count

      ds1 <- ds
      pdf <- ds1 %>% group_by(project_name) %>% arrange(Date)  %>% mutate(cmml = cumsum(Activity))




      pdf <- pdf %>% group_by(project_name)  %>% mutate(norm = (Activity-min(Activity))/(max(Activity)-min(Activity)))
      pdf4 <- ds1 %>% group_by(project_name)  %>% mutate(norm = (Activity-min(Activity))/(max(Activity)-min(Activity)))
      ds2 <- ds
      pdf2 <- ds2  %>% arrange(Date)  %>% mutate(cmml = cumsum(Activity))


      sp <- ggplot(pdf4, aes(x=Date, y=norm, group=Asset_type, color=Asset_type))  + geom_bar(stat="identity")
      p1 <- sp + facet_grid(project_name ~ . , scales="free_y")       + geom_vline(xintercept = as.numeric(eval11), linetype=5)   + geom_vline(xintercept = as.numeric(eval12), linetype=5)  + geom_vline(xintercept = as.numeric(eval13), linetype=5)   + geom_vline(xintercept = as.numeric(dps_date), linetype=3)

      cmmlp <- ggplot(pdf, aes(x=Date, y=cmml))  +
      geom_rect(data = pdf,alpha = 0.3,aes(fill = project_name),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
      geom_step() +
      geom_point(aes(colour = norm+0.1)) +  scale_colour_gradient(trans="log") #+  geom_line( aes(x=Date, y=2*cmml)) #geom_histogram(aes(y = 3*..density..), alpha = 0.2, binwidth = 3) + stat_ecdf(aes(x = Activity))
      p2 <- cmmlp + facet_grid(project_name ~ ., scales="free_y" )  + geom_vline(xintercept = as.numeric(eval11), linetype=5)   + geom_vline(xintercept = as.numeric(eval12), linetype=5)  + geom_vline(xintercept = as.numeric(eval13), linetype=5)   + geom_vline(xintercept = as.numeric(dps_date), linetype=3)



      cmmlpagg <- ggplot(pdf2, aes(x=Date, y=cmml)) +
      geom_rect(aes(xmin=eval13 - 30, xmax=eval13 + 30, ymin=0, ymax=Inf), fill="grey70",alpha=0.5)  +
      geom_step() +  geom_point(aes(colour = Activity))#+  geom_line( aes(x=Date, y=2*cmml)) #geom_histogram(aes(y = 3*..density..), alpha = 0.2, binwidth = 3) + stat_ecdf(aes(x = Activity))

      p3 <- cmmlpagg  + geom_vline(xintercept = as.numeric(eval11), linetype=5)   +
      geom_vline(xintercept = as.numeric(eval12), linetype=5)  +
      geom_vline(xintercept = as.numeric(eval13), linetype=5)

      multiplot(p1,p2,p3, cols=2)
    }

  )
)



## ggplots all proejcts acitivty by type file
getLogBy<-function(type,xuser) {
  dt <- processCsv('activity_logs')
  # dt<- data[data$culprit_type == "User",]

  if(xuser){
      #dt<-dt[(dt$culprit_type %in% c("User")),]
  }
  else{
      dt<-dt[!(dt$culprit_type %in% c("User")),]
  }
  # dt<- dt[dt$action == type,]

  if(type=="create"){
    dt<-dt[(dt$action %in% c("create","update")),]
  }
  else{
      dt<-dt[(dt$action %in% c(type)),]
  }

  colnames(dt)[7] <- "contributor_id"
  # ds<-joinPerson(dt)
  # ds<-joinDiscipline(ds)
  # ds<-joinAssoc(ds)

  projs <- processCsv('projects')
  colnames(projs)[1] <- "project_id"
  colnames(projs)[2] <- "project_name"
  colnames(dt)[9] <- "project_id"
  keeps<- c("project_id","project_name")
  ds<- merge(dt, projs[keeps], by="project_id", all.x=TRUE)

  ds<-ds[!(ds$activity_loggable_type %in% c("Person")),]
  ds<-ds[!(ds$activity_loggable_type %in% c("Event")),]
  ds<-ds[!(ds$activity_loggable_type %in% c("User")),]


  if(type == "create"){
  #   ds<-ds[!(ds$activity_loggable_type %in% c("Publication")),]
  }


  #Filter Projects
#  ds<-ds[!(ds$project_name %in% c("SysMO DB","SilicoTryp","TRANSLUCENT","Noisy-Strep")),]
#   ds<-ds[(ds$project_name %in% c("COSMIC", "SUMO", "MOSES","SysMO-LAB","SilicoTryp", "BaCell-SysMO")),]

  # a<-aggregate(ds, by=list(ds$ncreated_at, ds$project_name,ds$activity_loggable_type), FUN=length)
  # ds <- count(ds, c('ncreated_at'))


 return(ds)
}

graphLogs<-function(type,xuser){
  library(ggplot2)

  ds<-getLogBy(type,xuser)

  if(type == "create"){
    # ds<-ds[!(ds$activity_loggable_type %in% c("Publication")),]
  }

  ds <- as.data.table(ds)
  # setkeyv(ds, cols=c('ncreated_at',"project_name"))
  # ds <- ds[, list(count=.N), by=c('ncreated_at',"project_name")]
  setkeyv(ds, cols=c('ncreated_at',"project_name","activity_loggable_type"))
# Commented to check difference
  ds <- ds[, list(count=.N), by=c('ncreated_at',"project_name","activity_loggable_type")]

  ds$Date<-ds$ncreated_at
  ds$Asset_type<-ds$activity_loggable_type
  ds$Activity<-ds$count

  # sp <- ggplot(ds, aes(x=ncreated_at, y=count)) + geom_point(shape=1)
  # sp <- ggplot(ds, aes(x=ncreated_at, y=count, group=activity_loggable_type, color=activity_loggable_type))  + geom_point()

  #By Asset
    sp <- ggplot(ds, aes(x=Date, y=Activity, group=Asset_type, color=Asset_type))  + geom_bar(stat="identity") #+ geom_point()
    sp2 <- sp
  #All aggregated
  # sp <- ggplot(ds, aes(x=Date, y=Activity)) + geom_point(colour = "red") + theme_bw() #+ geom_point()



  # sp <- ggplot(ds, aes(x=ncreated_at, y=activity_loggable_id, group=activity_loggable_type, color=activity_loggable_type)) + geom_point(shape=1)
  # sp <- ggplot(ds, aes(x=ncreated_at, y=activity_loggable_id, group=activity_loggable_type, fill=activity_loggable_type)) + geom_raster(hjust = 0, vjust = 0) #geom_point(shape=1)
  # sp <- ggplot(ds, aes(x=ncreated_at, y=activity_loggable_type, group=count, fill=count)) + geom_raster(hjust = 0, vjust = 0) #geom_point(shape=1)

  #Separated by Projects
  sp + facet_grid(project_name ~ . , scales="free_y")  + geom_vline(xintercept = as.numeric(eval11), linetype=5) + geom_vline(xintercept = as.numeric(eval12), linetype=5) + geom_vline(xintercept = as.numeric(eval13), linetype=5) + coord_fixed()



  #All aggregated
  # sp2  + geom_vline(xintercept = as.numeric(eval11), linetype=5) + geom_vline(xintercept = as.numeric(eval12), linetype=5) + geom_vline(xintercept = as.numeric(eval13), linetype=5)  + theme(text = element_text(size=18, family="serif"))

  # sp2 <-ggplot(ds, aes(date, asset_type)) + geom_tile(aes(fill = Activity)) +
    # scale_fill_gradient(low = "white", high = "red")
 # sp2
}

contProportion<-function(){
  d<-processCsv("data_files")
  data<-joinSize(d,"data_files")
  nCont<-NROW(as.data.frame(table(data$contributor_id)))
  #   v<-merge(data,nCont, by="contributor_id")

  # users<-processCsv("users")
  users<-getMembers()
  users$contributor_id<-users$id
  # colnames(users)[1]<-"contributor_id"
  guess<-users
  # guess<-guessDiscipline(users)

  tExp<-NROW(subset(guess, discipline_id == 2))
  expRatio<- nCont/tExp
  print(nCont)
  print(expRatio)
  #get total of experimentalist
  #get total of experimentalist contributors
  m<-processCsv("models")
  data<-joinSize(m,"models")
  nCont<-NROW(as.data.frame(table(data$contributor_id)))
  #   v<-merge(data,nCont, by="contributor_id")

  tMod<-NROW(subset(guess, discipline_id == 1))
  emodRatio<- nCont/tMod
  print(nCont)
  print(emodRatio)
}


getCategories <- function() {
  users <-getMembers()
  # users<-processCsv("users")
  # users<-joinDiscipline(users)
   # users<-guessDiscipline(users)
  # users<-joinAssoc(users)
  users<-joinRole(users)
  users$contributor_id<-users$id

  contr<-getContributors3()
  contr$contributes<- "Contributor"
  keeps<-c("contributes", "contributor_id")
  users<-merge(users, contr[keeps], by="contributor_id", all.x=TRUE)

  r<-data.frame(project_role_id=users$project_role_id, discipline=users$title, contributes=users$contributes )

  r$project_role_id <- as.character(r$project_role_id)
  r$project_role_id[r$project_role_id != "6"] <- "No"
  r$project_role_id[r$project_role_id == "6"] <- "Yes"
  r$project_role_id <- as.factor(r$project_role_id)

  r$discipline <- as.character(r$discipline)
  r$discipline[is.na(r$discipline)] <- "UNK"
  r$discipline <- as.factor(r$discipline)

  r$contributes <- as.character(r$contributes)
  r$contributes[is.na(r$contributes)] <- "Lurker"
  r$contributes <- as.factor(r$contributes)

  write.csv(r, file = "categories.csv")
  r<-aggregate(r$project_role_id,  by=list( r$project_role_id, r$discipline, r$contributes), NROW)

  colnames(r)[1]<-"PAL"
  colnames(r)[2]<-"Discipline"
  colnames(r)[3]<-"Contributes"
  colnames(r)[4]<-"Freq"
  return(r)
}


graphCategories<-function() {

  r<-getCategories()

  myt <- subset(r, select=c("PAL","Contributes","Discipline","Freq"))

myt <- within(myt, {
  PAL <- factor(PAL, levels=c("Yes","No"))
  levels(Discipline) <- c(paste(c("Admin", "Bioinformatician", "Experimentalist", "Modeller", "UNK")),"Contributes")
  color <- ifelse(PAL=="Yes","#008888","#330066")
})

with(myt, parallelset(Contributes, PAL, Discipline, freq=Freq, col=color, alpha=0.2))
# return(myt)

}
