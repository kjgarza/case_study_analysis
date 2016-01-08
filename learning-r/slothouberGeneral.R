corr_test <- function( t_x, t_y, m_x, m_y, kind ) {
  #png( file=paste(kind,'png',sep='.'), pointsize=14 )
  #par( mar=c(3,3,2,2)-0.2 )
  layout(matrix(1:6,3,2,byrow=TRUE))
  hist( t_x, main='true x', col="blue" )
  hist( t_y, main='true y' )
  hist( m_x, main='measured x' )
  hist( m_y, main='measured y' )
  plot( m_x, m_y, main='measured response' )
  abline( lm( m_y ~ m_x ), col='red' )
  plot( t_x, t_y, main='true response' )
  abline( lm( m_y ~ m_x ), col='red' )
  #print( cor.test(m_x,m_y) )
  #dev.off()
}

bias <- 10

true_x      <- runif(n=100,min=0,max=100)
true_y      <- 2.5*true_x + 10
measured_x  <- true_x + bias + rnorm(n=100)
measured_y  <- true_y + bias + rnorm(n=100)
corr_test( true_x, true_y, measured_x, measured_y, 'uniform' )


true_x      <- rnorm(n=100,mean=50,sd=25)
true_y      <- 2.5*true_x + 10
measured_x  <- true_x + bias + rnorm(n=100)
measured_y  <- true_y + bias + rnorm(n=100)
corr_test( true_x, true_y, measured_x, measured_y, 'normal' )


true_x      <- c( rnorm(n=50,mean=5,sd=2), rnorm(n=50,mean=90,sd=5) )
true_y      <- 2.5*true_x + 10
measured_x  <- true_x + bias + rnorm(n=100)
measured_y  <- true_y + bias + rnorm(n=100)
corr_test( true_x, true_y, measured_x, measured_y, 'two_peaks' )


true_x      <- rnorm(n=100,mean=50,sd=25)
true_y      <- 2.5*true_x + 10
measured_x  <- true_x + bias + 0.4*true_x*rnorm(n=100)
measured_y  <- true_y + bias + rnorm(n=100)
corr_test( true_x, true_y, measured_x, measured_y, 'hetroskedasticity' )
