


##library(pixmap) ; library(rtiff) ; library(corpcor)

library(png)

library(grImport)

haar1D <- function(x) { 
	bpos <-  as.numeric( x >= 0 & x < 0.5 )
	bneg <-  -as.numeric( x >= 0.5 & x < 1 )
	return(bpos+ bneg)
}

haar1D.basis <- function(x, k, j) {
	return(  haar1D( 2^j*x-k ) )
}

build.WT <- function( N ) {
	x <- (1:N-0.5)/N
	J <- log(N, 2)
	Mx.out <- matrix(NA, N, N)
	Mx.out[1,] <- rep( sqrt(1/N), N)
	for(j in 0:(J-1)) {
		xwidth <- N/2^j
		for(jk in 0:(2^j-1)) {
			Mx.out[2^j+jk+1, ] <- haar1D.basis(x, jk, j) / sqrt(xwidth)
		}
	}
	return(Mx.out)
}


########################### A SMALL EXAMPLE
ximage <- matrix(1:64, 8, 8)
ximage
HH <- build.WT(8)
D <- HH %*% ximage %*% t(HH)
D
Dc <- as.numeric( abs(D) >= 1 ) * D
yimage <- t(HH) %*% Dc %*% HH
yimage
##################################


################### ortho normality

sum( (t(HH) - solve(HH))^2 )


################################## A REAL EXAMPLE

xxpng <- readPNG( file.path("__pics", "funny-cat.png") )

class(xxpng)

dim(xxpng)





library(magick)

library(broman)

#xmypic <- image_read( file.path("__pics", "funny-cat.png") ) %>% image_scale('128x128') %>% image_combine( colorspace="gray" )

xmypic <- image_read( file.path("__pics", "funny-cat.png") ) %>% image_scale('128x128') %>% image_convert( colorspace="gray" )


dim(xmypic[[1]])

myRaster <- xmypic[[1]][ 1, , ]

xpalette <- gray.colors(n=256, start = 0.0, end = 1.0, gamma = 2.2, alpha=1, rev = TRUE)






par(mfrow=c(2, 4))



mxMyPic <- 1 - matrix( hex2dec( ( myRaster ) ) / 255, nrow(myRaster), ncol(myRaster) )
mxMyPic <- mxMyPic[ , nrow(mxMyPic):1 ]

image(mxMyPic, col=xpalette)


#mxMyPic <- matrix( hex2dec( ( myRaster ) ) / 255, nrow(myRaster), ncol(myRaster) )
#grid.raster(mxMyPic)



N.x = ncol( mxMyPic ) ; N.y <- N.x

HH <- build.WT(N.y)



D <- HH %*% mxMyPic %*% t(HH) # can threshold by j in SECOND multiplication
#plot( D )

threshold = 0.1
Dc = as.numeric( abs(D) >= threshold ) * D
sum( as.numeric( abs(D) >= threshold ) ) / (N.x * N.y)

yimage <- t(HH) %*% Dc %*% HH

image(yimage, col=xpalette)


################# eliminate high frequencies


HHuse <- HH[ 1:100,  ]


D <- HHuse %*% mxMyPic %*% t(HHuse) # can threshold by j in SECOND multiplication
#plot( D )

Dc <- D


yimage <- t(HHuse) %*% Dc %*% HHuse

image(yimage, col=xpalette)



############ elimimate low intensity frequencies


HHuse <- HH

D = HHuse %*% mxMyPic %*% t(HHuse) # can threshold by j in SECOND multiplication
#plot( D )


xintensity <- apply( D, 2, function(x) { return( mean(abs(x)) ) } )

xfreq_mask <- xintensity > 0.05

sum(xfreq_mask)


Dc <- D[ xfreq_mask, xfreq_mask ]

yimage = t(HHuse[ xfreq_mask, ]) %*% Dc %*% HHuse[ xfreq_mask, ]

image(yimage, col=xpalette)




#######################


library(mrbsizeR)

CC <- dctMatrix(n=128)

############# orthonormaility
sum( (t(CC) - solve(CC))^2 )


##plot(0, 0, type="n")
image(CC, col=xpalette)



D <- CC %*% mxMyPic %*% t(CC) # can threshold by j in SECOND multiplication
#plot( D )

threshold = 0.1
Dc <- as.numeric( abs(D) >= threshold ) * D
sum( as.numeric( abs(D) >= threshold ) ) / (N.x * N.y)

yimage = t(CC) %*% Dc %*% CC

image(yimage, col=xpalette)





################# eliminate high frequencies

CCuse <- CC[ 1:50,  ]

D <- CCuse %*% mxMyPic %*% t(CCuse) # can threshold by j in SECOND multiplication
#plot( D )

Dc <- D

yimage <- t(CCuse) %*% Dc %*% CCuse

image(yimage, col=xpalette)








############ elimimate low intensity frequencies

CCuse <- CC

D <- CCuse %*% mxMyPic %*% t(CCuse) # can threshold by j in SECOND multiplication
#plot( D )

xintensity <- apply( D, 2, function(x) { return( mean(abs(x)) ) } )

xfreq_mask <- xintensity > 0.05

sum(xfreq_mask)


Dc <- D[ xfreq_mask, xfreq_mask ]

yimage = t(CCuse[ xfreq_mask, ]) %*% Dc %*% CCuse[ xfreq_mask, ]

image(yimage, col=xpalette)


