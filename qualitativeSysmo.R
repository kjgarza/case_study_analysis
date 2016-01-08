 # library(RQDA)
 # openProject("/Users/kristian/Dropbox/Scripts/seek_casestudy.rqda", updateGUI = TRUE)
# openProject("/Users/kristian/Dropbox/Scripts/seek_casestudy.rqda")
benkler<-c("emotional_response","material_interest","moral_commitment","social_motivations")
share<-c("reason_openshare")
contr<-c("contribution_motivation_q1_5")
ryan<-c("extrinsic_motivation","intrinsic_motivation")


getCodesFreq<- function(){
  s<-summaryCodings()
  r<-as.data.frame(s$NumOfCoding)
  return(r[with(r, order(-Freq)), ])
}


xMotivVec<- function() {
# 	data<-getCodingTable() 
# 
#   library(data.table)
#   data <- as.data.table(data)
# 
# 	setkeyv(data, cols=c('codename'))
#   data <- data[, list(count=.N), by=c("codename")]

  benklerModel<-c("emotional_response","material_interest","moral_commitment","social_motivations")
  ryanModel<-c("extrinsic_motivation","intrinsic_motivation")
  motivations<-c("contribution_motivation_q1_5","reason_openshare")
  crossCodes(codeList=c(motivations,ryanModel),relation=c("overlap"), print=TRUE)

# return(data)
}


getMotivationByUser<-function(u) {
	data<-getCodingTable()
	x <- data[(data$fid %in% c(u)),]
  	benklerModel<-c("emotional_response","material_interest","moral_commitment","social_motivations")
 	motivations<-c("contribution_motivation_q1_5","reason_openshare")
	crossCodes(codeList=c(motivations,benklerModel),data=x,relation=c("exact"), print=TRUE)
	crossCodes(codeList=c(motivations,benklerModel),data=x,relation=c("overlap"), print=TRUE)
	crossCodes(codeList=c(motivations,benklerModel),data=x,relation=c("inclusion"), print=TRUE)
	
}


joinInterInfo<-function() {
  data<-getCodingTable()
  iinfo<-processCsv("interviewInfo")
  data<-merge(data,iinfo, by="filename")  
  return(data)
}


getMotivationByDisci<-function(discipline_id,motivation_model,motivatio_to) {
  data<-joinInterInfo()
  cc<-data[(data$discipline_id %in% c(discipline_id)),]
  r<-crossCodes(codeList=c(motivatio_to,motivation_model),data=cc,relation=c("exact"))
  r<-crossCodes(codeList=c(motivatio_to,motivation_model),data=cc,relation=c("overlap"))
  r<-crossCodes(codeList=c(motivatio_to,motivation_model),data=cc,relation=c("inclusion"))
}

getMotivationByProject<-function(project_id,motivation_model,motivatio_to) {
  data<-joinInterInfo()
  cc <- data[(data$fid %in% c(project_id)),]
  r<-crossCodes(codeList=c(motivatio_to,motivation_model),data=cc,relation=c("exact"))
  r<-crossCodes(codeList=c(motivatio_to,motivation_model),data=cc,relation=c("overlap"))
  r<-crossCodes(codeList=c(motivatio_to,motivation_model),data=cc,relation=c("inclusion"))
}



mySerachAcross<- function(terms){
  files<-searchFiles("file like '%sharing%'")
  s
  
}



