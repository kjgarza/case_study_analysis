sampleUsers<-function(smpl) {
	data<-processCsv("users")
	data<-data[!duplicated(data[,c('person_id')]),]
	data<-filterOutSysDB(data)
	data<-guessDiscipline(data)
	# data<-data[!duplicated(data[,c('person_id')]),]
	data$contributor_id<-data$id
	zero<-data[(data$contributor_id %in% c(15)),]  
	zero$contributor_id<-0
	zero$id<-0
	

  	exp<-data[(data$discipline_id %in% c(1)),]  
  	mod<-data[(data$discipline_id %in% c(2)),]
	exp<-exp[sample(nrow(exp), 50), ]
	mod<-mod[sample(nrow(mod), 50), ]

	data<-rbind(exp,mod,zero)
	
	keeps<-c("contributor_id","person_id","id","login","email","crypted_password","salt","created_at","updated_at","remember_token","remember_token_expires_at","activation_code","activated_at","reset_password_code","reset_password_code_until","posts_count","last_seen_at","uuid","openid","show_guide_box","ncreated_at","nupdated_at")
	# data<-data[sample(nrow(data), 30), ]
	write.table(data[keeps], file = paste0("~/Dropbox/Data/Sysmo/sample",smpl,"/users.csv"), sep = ",", col.names = NA, qmethod = "double")
	return(data[keeps])
}


filterTables<-function(smpl) {
	sample<-sampleUsers(smpl)
	d<-processCsv("data_files")
	s<-processCsv("sops")
	m<-processCsv("models")
	dl<-processCsv("data_file_auth_lookup")
	dl$contributor_id<-dl$user_id
	ml<-processCsv("model_auth_lookup")
	ml$contributor_id<-ml$user_id
	sl<-processCsv("sop_auth_lookup")
	sl$contributor_id<-sl$user_id

	keep<-c("contributor_id")
	rd<-merge(sample[keep],d,by="contributor_id")
	write.table(rd, file = paste0("~/Dropbox/Data/Sysmo/sample",smpl,"/data_files.csv"), sep = ",", col.names = NA, qmethod = "double")

	rs<-merge(sample[keep],s,by="contributor_id")
	write.table(rs, file = paste0("~/Dropbox/Data/Sysmo/sample",smpl,"/sops.csv"), sep = ",", col.names = NA, qmethod = "double")

	rm<-merge(sample[keep],m,by="contributor_id")
	write.table(rm, file = paste0("~/Dropbox/Data/Sysmo/sample",smpl,"/models.csv"), sep = ",", col.names = NA, qmethod = "double")

	rdl<-merge(sample[keep],dl,by="contributor_id")
	write.table(rdl, file = paste0("~/Dropbox/Data/Sysmo/sample",smpl,"/data_file_auth_lookup.csv"), sep = ",", col.names = NA, qmethod = "double")

	rml<-merge(sample[keep],ml,by="contributor_id")
	write.table(rml, file = paste0("~/Dropbox/Data/Sysmo/sample",smpl,"/model_auth_lookup.csv"), sep = ",", col.names = NA, qmethod = "double")

	rsl<-merge(sample[keep],sl,by="contributor_id")
	write.table(rsl, file = paste0("~/Dropbox/Data/Sysmo/sample",smpl,"/sop_auth_lookup.csv"), sep = ",", col.names = NA, qmethod = "double")

}