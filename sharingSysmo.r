getOpensharedAssets<- function(user_id){
  lookuptbl <- processCsv('data_file_auth_lookup')
  lookuptbl<- lookuptbl[lookuptbl$user_id == user_id,]
  lookuptbl[lookuptbl$can_download == 1,]
  return(nrow(lookuptbl[lookuptbl$can_view == 1,]))
  # return(nrow(lookuptbl[lookuptbl$can_download == 1,]))


} 


display<- function(){

} 