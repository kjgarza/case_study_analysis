library(xtable)

convertCsvFLatex <- function(filepath, caption){
	file <- read.csv(file=paste0("/Users/kristian/Downloads/",filepath,".csv"), sep=",", head=TRUE)
	label<-tolower(abbreviate(c(caption), 6))
	my.table<-xtable(file, caption=caption, label=label)
	digits(my.table)
	print(my.table, booktabs=TRUE, include.rownames=FALSE, tabular.environment="tabularx", width="textwidth")	
}









