p <- function(arg1){
return(arg1)
}

pCompl <- function(arg1){
return(1 - arg1)
}	

pComposite <- function(arg1){
return(arg1*arg1)
}

treeCoinFlips <- function(arg1){
	r <- 3*(p(arg1)*pCompl(arg1)*pCompl(arg1))
	return(r)
}

twoCoinFlipsTwoH <- function(arg1, arg2){
	r <- arg1*arg2
	return(r)
}


treeprobabilities <- function(p0, p1, p2){
	r <- p(p0)*p(p1)+ pCompl(p0)*p2
	return(r)
}


bayes <- function(p0, p1, p2){
	p0x <- pCompl(p0)
	p1x <- pCompl(p1)
	p2x <- pCompl(p2)
	
	joint1 <- p(p0)*p(p1)
	joint2 <- p0x*p2x
	norm <- joint1+joint2
	
	post <- joint1/norm
	
	
	r <- post
	return(r)
}