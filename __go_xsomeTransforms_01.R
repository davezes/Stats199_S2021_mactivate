


library(mactivate)






source("___f_funs.R")


###################### this is our data set

#xdf <- read.csv( file.path( "___data", "Bike-Sharing-Dataset", "hour.csv" ) )
#head(xdf)
#dim(xdf)



set.seed(777)




N <- 80000


x1 <- rchisq(N, df=2)

x2 <- rexp(N, 1)

x3 <- rcauchy(N)


X <- cbind(x1, x2, x3)



par(mfrow=c(3, 3))


for(jj in 1:ncol(X)) {
    hist(X[ , jj])
}



Xstnd <- f_stndX(X=X)

for(jj in 1:ncol(X)) {
    hist(Xstnd[ , jj])
}



Xogive <- apply(X, 2, f_ogive)

for(jj in 1:ncol(X)) {
    hist(Xogive[ , jj])
}







