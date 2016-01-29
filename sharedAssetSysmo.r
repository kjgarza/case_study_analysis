# users: is dataframe with contributor_id
# retunrs: a dataframe with users and their Sharing Ratio
joinSharingRatio<-function(users) {

  # users: is dataframe with contributor_id
  # uid:
  # s:
  # retunrs: Return the sharing ratio of a user
  processSharingRatio<-function(uid, users, s) {
    uassets<-s[s$contributor_id==uid,]
    ratio<-nrow(s[(s$lab %in% c("opensharing")),])/nrow(uassets)
    return(ratio)
  }



  d<-getAllresc(0)
  s<-isOpenShared(d)
  r<-sapply(users$contributor_id, processSharingRatio, s=s, users=users)
  users$rs <- r
  return(users)
}


# data:
# retunrs: A dataframe
isOpenShared<-function(data) {

  if(is.null(data$type) ){
    print("wrong data. I need type")
    stop()
  }

  permissm<-getOpenAssets("model",0)
  permissd<-getOpenAssets("data_file",0)
  permisss<-getOpenAssets("sop",0)

  permiss<-rbind(permissm,permissd,permisss)
  permiss$id<-permiss$asset_id

  keeps<- c("type","lab","id")
  r<- merge(data, permiss[keeps], by=c("type","id"), all.x=TRUE)
  r$lab[is.na(r$lab)] <- "private"
  r$lab<-as.factor(r$lab)
  r$type<-as.factor(r$type)
  return(r)
}


isInterShared<-function(data) {

  if(is.null(data$type) ){
    print("wrong data. I need type")
    stop()
  }

  permissm<-getInterSharedAssets("model",0)
  permissd<-getInterSharedAssets("data_file",0)
  permisss<-getInterSharedAssets("sop",0)

  permiss<-rbind(permissm,permissd,permisss)
  permiss$id<-permiss$asset_id

  keeps<- c("type","lab","id")
  r<- merge(data, permiss[keeps], by=c("type","id"), all.x=TRUE)
  return(r)
}


getSharingPermissionsUid<-function(dataType,uid) {

  users <-getMembers(project)
  users$user_id <-users$id
  users<-users[!duplicated(users[,c('user_id')]),]
  users <- users[(users$user_id %in% c(uid)),]

  assets <- getAllresc(0)
  # assets <- filterOutPals(assets)
  assets <- filterOutSysDB(assets)
  # if(project) assets <- filterProject(assets,project)

  assets <- guessDiscipline(assets)
  assets$project_creator_id <- assets$project_id
  assets$asset_id <- assets$id
  assets <- assets[(assets$user_id %in% c(uid)),]

  switch(dataType,
    model={
      assets <- assets[(assets$type %in% c('model')),]
      lookuptbl <- processCsv('model_auth_lookup')
    },
    data_file={
      assets <- assets[(assets$type %in% c('data_file')),]
      lookuptbl <- processCsv('data_file_auth_lookup')
    },
    sop={
       assets <- assets[(assets$type %in% c('sop')),]
      lookuptbl <- processCsv('sop_auth_lookup')
    }
  )

  keeps<-c("asset_id","project_creator_id","type","discipline_id", "person_id")
  lookuptbl <- merge(lookuptbl,assets[keeps], by="asset_id", all.x=TRUE)

  keeps<-c("user_id","project_id")
  lookuptbl <- merge(lookuptbl,users[keeps], by="user_id", all.x=TRUE)
  lookuptbl$project_observer_id<-lookuptbl$project_id
  lookuptbl$observer_id<-lookuptbl$user_id
  lookuptbl<-lookuptbl[!is.na(lookuptbl$project_creator_id),]
# print(NROW(lookuptbl))
  return(lookuptbl)
}

# dataType: Data type string
# project: Projects ID integer
# Returns: A data frame lookuptbl with Sharing Permissions
getlookuptbl<-function(dataType,project=0) {

  users <-getMembers(project)
  users$user_id <-users$id
  users<-users[!duplicated(users[,c('user_id')]),]


  assets <- getAllresc(0)
  # assets <- filterOutPals(assets)
  # assets <- filterOutSysDB(assets)
  if(project) assets <- filterProject(assets,project)

  assets <- guessDiscipline(assets)
  assets$project_creator_id <- assets$project_id
  assets$asset_id <- assets$id


  switch(dataType,
    model={
      assets <- assets[(assets$type %in% c('model')),]
      lookuptbl <- processCsv('model_auth_lookup')
    },
    data_file={
      assets <- assets[(assets$type %in% c('data_file')),]
      lookuptbl <- processCsv('data_file_auth_lookup')
    },
    sop={
       assets <- assets[(assets$type %in% c('sop')),]
      lookuptbl <- processCsv('sop_auth_lookup')
    }
  )

  keeps<-c("asset_id","project_creator_id","type","discipline_id", "person_id")
  lookuptbl <- merge(lookuptbl,assets[keeps], by="asset_id", all.x=TRUE)

  keeps<-c("user_id","project_id")
  lookuptbl <- merge(lookuptbl,users[keeps], by="user_id", all.x=TRUE)
  lookuptbl$project_observer_id<-lookuptbl$project_id
  lookuptbl$observer_id<-lookuptbl$user_id
  lookuptbl<-lookuptbl[!is.na(lookuptbl$project_creator_id),]
# print(NROW(lookuptbl))
  return(lookuptbl)
}

# dataType: a data type string
# project: a project ID
# returns: a Data frame lookuptbl of accesible assets
getOpenAssets<- function(dataType, project=0){
  data <- getlookuptbl(dataType, project)

  r <- data[(data$observer_id %in% c(0)),]
  r <- r[(r$can_view %in% c(1) ),]
  if(NROW(r)){
    r$lab<-"opensharing"
    print(r$project_creator_id)
  }
  return(r)
}

# dataType: is the type of material
# project: ID of the project
# returns: dataframe with assets that shared with other SYSMO projects
getInterSharedAssets<- function(dataType, project){
  data <- getlookuptbl(dataType, project)

  projectdata <- data

  projectdata <- projectdata[projectdata$project_observer_id != 12,]
  r <- projectdata[projectdata$project_creator_id != projectdata$project_observer_id,]
  r <- r[(r$can_view %in% c(1)),]
  r <- r[!duplicated(r[,c('asset_id')]),]
  if(NROW(r)){
  r$lab<-"intersharing"
  print(r$project_creator_id)
  }
  # r <- r[!duplicated(r[,c('asset_id','project_observer_id')]),]
  return(r)
}

# dataType: is the type of material
# project: ID of the project
# returns: dataframe with assets that shared with collaborators
getIntraSharedAssets<- function(dataType, project){
  data <- getlookuptbl(dataType, project)

  projectdata <- data
  # projectdata <- getByProject(data, project_id)


  r <- projectdata[projectdata$project_creator_id == projectdata$project_observer_id,]
  r <- r[(r$can_download %in% c(1)),]
  r <- r[!duplicated(r[,c('asset_id')]),]
  # r <- r[!duplicated(r[,c('asset_id','project_observer_id')]),]
  return(r)
}

# getByProject<- function(data, project_id) {
#   # r <- r[data$project_id == project_id,]
#   r <- data[(data$project_id %in% c(project_id)),]
#   return(r)
# }


# processSharingReport<-function(project_id) {
#   intersharing<-getInterSharedAssets()
#   open<-getOpenAssets()
#   open$project_id<-open$project_creator_id
#   open<-getByProject(open,project_id)
#   r<-c(NROW(intersharing),NROW(open))
#   return(r)
# }


# returns: percentage of dataset shared by a Projects
processSharingReportSum<-function(project=0) {

all<- getAllresc(project)
a<-getOpenAssets("model",project)
b<-getOpenAssets("sop",project)
c<-getOpenAssets("data_file",project)
dd<-rbind(a,b,c)
print("General Shared")
print(NROW(dd)/NROW(all))


a<-getInterSharedAssets("model",project)
b<-getInterSharedAssets("sop",project)
c<-getInterSharedAssets("data_file",project)
dd<-rbind(a,b,c)
print("Specific Shared out")
print(NROW(dd)/NROW(all))

a<-getIntraSharedAssets("model",project)
b<-getIntraSharedAssets("sop",project)
c<-getIntraSharedAssets("data_file",project)
dd<-rbind(a,b,c)
print("Specific Shared within: doesn't make sense")
print(all)
print(dd)
print(NROW(dd)/NROW(all))



  # se<-processSharingReportSharing("model",0)
  # ses<-processSharingReportSharing("sop",0)
  # sed<-processSharingReportSharing("data_file",0)
  # cse<-rbind(se,ses,sed)

  # aa<-rapply( cse, f=function(x) ifelse(is.nan(x),0,x), how="replace" )

  # cse$qty<-aa$qty

  # aggregate(qty ~ lab, data=cse, sum)

}

processSharingReportSharing<-function(dataType, project) {
  intersharing<-getInterSharedAssets(dataType, project)
  open<-getOpenAssets(dataType, project)

  # intrasharing<-getIntraSharedAssets(dataType)

  assets<-getAllresc(0)

  assets <- filterOutSysDB(assets)
  assets <- filterOutPals(assets)
  if(project) assets <- filterProject(assets,project)

  assets <- guessDiscipline(assets)

  nAssets<-NROW(assets)



  mod<-c(
    NROW(intersharing)/nAssets,
    # NROW(intrasharing[(intrasharing$discipline_id %in% c(1)),])/nMods,
    NROW(open)/nAssets
  )

  lab<-c("intersharing","opensharing")
  qty<-c(mod)
  p<- data.frame(qty,lab)
  p$tab<-dataType

  return(p)
}

#
#
# returns: Data frame with comparison of modeller and experimentlist sharing
processSharingReportDiscipline<-function(dataType, project) {
  intersharing<-getInterSharedAssets(dataType, project)
  open<-getOpenAssets(dataType, project)
  print(NROW(open))


  # intrasharing<-getIntraSharedAssets(dataType)

  assets<-getAllresc(0)

  assets <- filterOutSysDB(assets)
  assets <- filterOutPals(assets)
  if(project) assets <- filterProject(assets,project)

  assets <- guessDiscipline(assets)

  assetsm<-assets[(assets$discipline_id %in% c(1)),]
  nMods<-NROW(assetsm)

  assetsx<-assets[(assets$discipline_id %in% c(2)),]
  nData<-NROW(assetsx)

  mod<-c(
    NROW(intersharing[(intersharing$discipline_id %in% c(1)),]),
    # NROW(intrasharing[(intrasharing$discipline_id %in% c(1)),])/nMods,
    NROW(open[(open$discipline_id %in% c(1)),])
  )
  exp<-c(
    NROW(intersharing[(intersharing$discipline_id %in% c(2)),]),
    # NROW(intrasharing[(intrasharing$discipline_id %in% c(2)),])/nData,
    NROW(open[(open$discipline_id %in% c(2)),])
  )
  lab<-c("intersharing","opensharing","intersharing","opensharing")
  disc<-c("modeller","modeller","experimentalist","experimentalist")
  qty<-c(mod,exp)
  p<- data.frame(disc,qty,lab,nData,nMods )
  p$tab<-dataType

  # sp<-ggplot(data=p, aes(x=disc, y=qty ,fill=lab)) + geom_bar(stat="identity")
  # sp
  return(p)
}


## face plot of modeller vs experimentalist and their type of sharing they do
# Returns: Plot
graphSharingReportDiscipline<-function(dataType, project){

  p<-processSharingReportDiscipline(dataType, project)
  sp<-ggplot(data=p, aes(x=disc, y=qty ,fill=lab)) + geom_bar(stat="identity")
  sp
}


# Returns: Plot
graphMultiSharing<-function(project) {

  # for (i in c(1:15)) {

  m<-processSharingReportDiscipline("model",project)
  d<-processSharingReportDiscipline("data_file",project)
  s<-processSharingReportDiscipline("sop",project)

  p<- rbind(m,d,s)
  print(p)

  sp<-ggplot(data=p, aes(x=disc, y=qty ,fill=lab)) + geom_bar(stat="identity")
  sp + facet_grid(. ~ tab) + theme(strip.text.x = element_text(size=18),
          strip.text.y = element_text(size=18))


  sp2<-ggplot(data=p, aes(x=disc, y=qty ,fill=tab)) + geom_bar(stat="identity")
  sp2 + facet_grid(. ~ lab)  + theme( strip.text.x = element_text(size=18),
          strip.text.y = element_text(size=18)) + labs(title = project)

#return(p)
#
}

# Returns: Plot
graphSharingComparison<-function() {

  N <- 11  # some magic number, possibly an overestimate

  df <- data.frame(num=rep(NA, N), txt=rep("", N),  # as many cols as you need
                   stringsAsFactors=FALSE)          # you don't know levels yet

  for (i in 8:N) {
    print("project")
    print(i)
    m<-processSharingReportDiscipline("model",i)
    d<-processSharingReportDiscipline("data_file",i)
    s<-processSharingReportDiscipline("sop",i)
    p<- rbind(m,d,s)
    print(p)
    open  <- sum(p[p$lab == 'opensharing',]$qty)
    total <- sum(p$nData[1]+p$nMods[1])
    g<-c(open,total)


    df[i, ] <- g
  }

  write.csv(df, file = "SharingComparison.csv")

   ggplot(data=df, aes(x=project, y=quantity, fill=share_type)) + geom_bar(stat="identity")


  }





## don't know
graphSharing<-function() {

N <- 15  # some magic number, possibly an overestimate

df <- data.frame(num=rep(NA, N), txt=rep("", N),  # as many cols as you need
                 stringsAsFactors=FALSE)          # you don't know levels yet

    for (i in 1:N) {
      df[i, ] <- getSharingReport(i)
    }



  ggplot(data=df, aes(x=project, y=quantity, fill=share_type)) + geom_bar(stat="identity")
}
