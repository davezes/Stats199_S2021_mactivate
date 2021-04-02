


ffun <- function(x, y) { return( sqrt( mean( (y - x)^2 ) ) ) }




f_stndX <- function(X) {
     Y <- t( (t(X) - apply(X, 2, mean, na.rm=TRUE)) / apply(X, 2, sd, na.rm=TRUE) )
    return(Y)
}

f_sign_root <- function(x, pow) {
    y <- sign(x) * abs(x)^(pow)
    return(y)
}


f_sign_log <- function(x) {
    y <- sign(x) * log( abs(x) + 1 )
    return(y)
}



##### x <- xdf[ , "weathersit"] ; facname <- "weather_"

f_dummy <- function(x, facname=NULL) {
    u.x <- sort( unique(x) )
    d <- length(u.x)
    mx.out <- matrix(0, length(x), d)
    for(i in 1:d) {
        mx.out[ , i] <- as.integer( u.x[i] == x )
    }
    colnames(mx.out) <- paste0(facname, u.x)
    return(mx.out)
}




f_mact_find_polys <- function(xxls_fit) {
    
    xWhat <- xxls_fit[[ length(xxls_fit) ]][[ "What" ]]
    xcc <- xxls_fit[[ length(xxls_fit) ]][[ "cchat" ]]

    xsumWmagsWght <- apply( t(xWhat) * xcc, 2, function(x) { return(sum(abs(x))) } )
    
    return(xsumWmagsWght)
}



