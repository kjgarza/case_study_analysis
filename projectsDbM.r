
projects.getProjects<-function() {
	projects <- processCsv('projects')
}

projects.joinSubscriptions<-function() {
  colnames(projects)[1] <- "project_id"
  colnames(projects)[2] <- "project_name"
  subs <- processCsv('project_subscriptions')
  subs<-subs[!duplicated(subs[,c('person_id')]),]
  s<- merge(subs,projects, by="project_id", all=TRUE)
  return(r)
}


projects<-projects.getProjects()