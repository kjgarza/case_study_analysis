
users.getUsers<- function(){
	users<-processCsv("users")
}

users.joinPerson<- function(){
  
  if(NROW(users$person_id) ){
    print("person_id already exist")
    return(users)
  }

  if(is.null(users$contributor_id) ){
    print("wrong data. I need contributor_id")
    stop()
  }
  
  disci <- users
  disci$contributor_id<-disci$id
  # colnames(disci)[1] <- "contributor_id"
  
  keeps<- c("person_id","contributor_id")    
  r<- merge(users, disci[keeps], by="contributor_id", all.x=TRUE)    
  return(r)
}

users.joinDiscipline<- function(){
  
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

users.joinRole<- function(){
  
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

users.joinAssoc<- function(){
  
  if(is.null(users$person_id)){
    print("wrong data. I need person_id")
    stop()
  }
  
	class(projs) <- "joinSubscriptions"




	lala<-projects()


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

users.filterOutSysDB<- function(){
  
  if(is.null(users$project_id)){
    users<-joinAssoc(users)
  }
  r<-users[!(users$project_id %in% c(12)),]
  r<-users[!(users$person_id %in% c(136,411,433,134,132,372,205,355,318,48,133,1,130)),]
  return(r)
}

users.filterProject<- function(project_id){
  
  if(is.null(users$project_id)){
    users<-joinAssoc(users)
  }
  r<-users[(users$project_id %in% c(project_id)),]
  return(r)
}



users.filterOutPals<- function(){
  
  if(is.null(users$person_id)){
    print("wrong data. I need person_id")
    stop()
  }

  r<-joinRole()

  r<-r[!(r$project_role_id %in% c(6)),]
  r<-r[!duplicated(r[,c('person_id')]),]   

  return(r[r$person_id != "NULL",])
}


users.joinPersonInfo<- function(){
  
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


users.guessDiscipline<- function(){
  
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

users<-users.getUsers()


