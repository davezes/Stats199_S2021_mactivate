


library(mactivate)






source("___f_funs.R")


###################### this is our data set

xdf <- read.csv( file.path( "___data", "Bike-Sharing-Dataset", "hour.csv" ) )

head(xdf)

dim(xdf)




Nall <- nrow(xdf)


xd_season <- f_dummy(x=xdf[ , "season"], facname="seas_")

xd_yr <- f_dummy(x=xdf[ , "yr"], facname="yr_")

## xd_month <- f_dummy(x=xdf[ , "mnth"], facname="mnth_")

xd_hr <- f_dummy(x=xdf[ , "hr"], facname="hr_")

xd_holiday <- f_dummy(x=xdf[ , "holiday"], facname="holi_")

xd_weekday <- f_dummy(x=xdf[ , "weekday"], facname="wday_")

xd_workingday <- f_dummy(x=xdf[ , "workingday"], facname="workday_")

xd_weathersit <- f_dummy(x=xdf[ , "weathersit"], facname="weather_")





X <-
cbind(
xd_season[ , ],
xd_yr[ , ],
#xd_month[ , ],
xd_hr[ , ],
xd_holiday[ , ],
xd_weekday[ , ],
xd_workingday[ , ],
xd_weathersit[ , ],
"temp"=xdf[ , "temp"],
"atemp"=xdf[ , "atemp"],
"hum"=xdf[ , "hum"],
"windspeed"=xdf[ , "windspeed"]
#xdf[ , "casual"]
)

head(X)





yall <- xdf[ , "cnt" ]




Xstnd <- t( ( t(X) - apply(X, 2, mean) ) / apply(X, 2, sd) )

Uall <- Xstnd

ydf <- data.frame("y"=yall, Xstnd)

xlm <- lm(y~., data=ydf)
summary(xlm)

xnamesRm <-
c(
"seas_4",
"yr_1",
"hr_23",
"holi_1",
"wday_6",
"workday_0",
"workday_1",
"weather_4"
)


Xall <- Xstnd[ , -which( colnames(Xstnd) %in% xnamesRm ) ]


xlo <- 2000

xndx_train <- I(1:(Nall-2000))
xndx_test <- setdiff( 1:Nall, xndx_train )


X_train <- Xall[ xndx_train, ]
U_train <- Uall[ xndx_train, ]
y_train <- yall[ xndx_train ]

X_test <- Xall[ xndx_test, ]
U_test <- Uall[ xndx_test, ]
y_test <- yall[ xndx_test ]


zdf <- data.frame("y"=yall, Xall)

xlm <- lm(y~., data=zdf, subset=xndx_train)
yhat_lm <- predict(xlm, newdata=zdf[ xndx_test, ])

####### usual LM
sqrt( mean( ( y_test - yhat_lm )^2 ) )






########################## mactivate I



m_tot <- 10
## m_tot <- 21

xcmact_hybrid <-
f_control_mactivate(
param_sensitivity = 10^9,
bool_free_w       = TRUE,
#w0_seed           = 0.1,
w0_seed           = 0.01,
#w_col_search      = "alternate",
w_col_search      = "one",
bool_headStart    = TRUE,
antifreeze        = TRUE, #### must be true to escape
max_internal_iter = 100, #####
ss_stop           = 10^(-12), ### -14 very small
escape_rate       = 1.002,
step_size         = 1,
Wadj              = 1/1,
force_tries       = 0,
lambda            = 1/300, ####
tol               = 10^(-12) ### -14 hybrid only
)





xxnow <- Sys.time()

xxls_out <-
f_fit_hybrid_01(
#f_fit_gradient_01(
X = X_train,
y = y_train,
m_tot = m_tot,
U = U_train,
m_start = 1,
mact_control = xcmact_hybrid,
verbosity = 3
)

cat( difftime(Sys.time(), xxnow, units="mins"), "\n" )






yhatTT <- matrix(NA, xlo, m_tot+1)

for(iimm in 0:m_tot) {
    yhat_test <- predict(object=xxls_out, X0=X_test, U0=U_test, mcols=iimm )
    yhatTT[ , iimm + 1 ] <- yhat_test
}



mx_errs <- y_test - yhatTT
xxers_mm <- apply( mx_errs, 2, function(x) { return( sqrt( mean( x^2 ) ) ) } ) ; xxers_mm
which.min(xxers_mm)
cat("\n", "Best testing RMSE:", xxers_mm[ which.min(xxers_mm) ], " -- occurs at w =", which.min(xxers_mm), "\n")
plot( 0:(length(xxers_mm)-1), xxers_mm, type="l", col="#22FF11", lwd=3, main="Test RMSE ~vs~ First # Columns of What")




########################## mactivate II



m_tot <- 10
## m_tot <- 21

xcmact_hybrid <-
f_control_mactivate(
param_sensitivity = 10^9,
bool_free_w       = TRUE, ####### set to false
#w0_seed           = 0.1,
w0_seed           = 0.01,
w_col_search      = "alternate",
#w_col_search      = "one",
bool_headStart    = TRUE,
antifreeze        = TRUE, #### must be true to escape
max_internal_iter = 100, #####
ss_stop           = 10^(-12), ### -14 very small
escape_rate       = 1.002,
step_size         = 1,
Wadj              = 1/1,
force_tries       = 0,
lambda            = 1/500, ####
tol               = 10^(-12) ### -14 hybrid only
)






xxnow <- Sys.time()

xxls_out <-
f_fit_hybrid_01(
#f_fit_gradient_01(
X = X_train,
y = y_train,
m_tot = m_tot,
U = U_train,
m_start = 1,
mact_control = xcmact_hybrid,
verbosity = 1
)

cat( difftime(Sys.time(), xxnow, units="mins"), "\n" )






yhatTT <- matrix(NA, xlo, m_tot+1)

for(iimm in 0:m_tot) {
    yhat_test <- predict(object=xxls_out, X0=X_test, U0=U_test, mcols=iimm )
    yhatTT[ , iimm + 1 ] <- yhat_test
}



mx_errs <- y_test - yhatTT
xxers_mm <- apply( mx_errs, 2, function(x) { return( sqrt( mean( x^2 ) ) ) } ) ; xxers_mm
which.min(xxers_mm)
cat("\n", "Best testing RMSE:", xxers_mm[ which.min(xxers_mm) ], " -- occurs at w =", which.min(xxers_mm)-1, "\n")
plot( 0:(length(xxers_mm)-1), xxers_mm, type="l", col="#22FF11", lwd=3, main="Test RMSE ~vs~ First # Columns of What")



###################################### 
######################################
######################################

library(glmnet)



gfit1 <- glmnet(x=X_train, y=y_train)

summary(gfit1)

coef(gfit1)

Yhat <- predict(gfit1, newx=X_test)


mx_errs <- y_test - Yhat
xxers_mm <- apply( mx_errs, 2, function(x) { return( sqrt( mean( x^2 ) ) ) } ) ; xxers_mm
which.min(xxers_mm)
cat("\n", "Best GLMNET testing RMSE:", xxers_mm[ which.min(xxers_mm) ], " -- occurs at s =", which.min(xxers_mm)-1, "\n")
plot( 0:(length(xxers_mm)-1), xxers_mm, type="l", col="#22FF11", lwd=3, main="GLMNET Test RMSE ~vs~ s val")






################## ridge

y_all_stnd <- (yall - mean(y_train)) / sd(y_train)

tXX_train <- crossprod(U_train) / length(xndx_train)
tXy_train <- ( t(U_train) %*% y_all_stnd[ xndx_train ] ) / length(xndx_train)




## xreg_vec <- seq(0.0001, 0.2, by=0.0001)

xreg_vec <- 10^seq(-14, 0, by=1)

Yhat <- matrix(NA, xlo, length(xreg_vec))

for(ii in 1:length(xreg_vec)) {
    
    xbhat <- solve( tXX_train + diag( xreg_vec[ii], ncol(tXX_train) ) ) %*% tXy_train
    
    yhat_test <- Xstnd[ xndx_test, ] %*% xbhat
    
    Yhat[ , ii ] <- yhat_test * sd(y_train) + mean(y_train)
    
}


mx_errs <- y_test - Yhat
xxers_mm <- apply( mx_errs, 2, function(x) { return( sqrt( mean( x^2 ) ) ) } ) ; #xxers_mm
which.min(xxers_mm)
cat("\n", "Best Ridge testing RMSE:", xxers_mm[ which.min(xxers_mm) ], " -- occurs at lambda =", xreg_vec[ which.min(xxers_mm) ], "\n")
plot( 0:(length(xxers_mm)-1), xxers_mm, type="l", col="#22FF11", lwd=3, main="GLMNET Test RMSE ~vs~ s val")

sd(yall[ xndx_test ])







################## ridge the olde fashioned way -- center y -- all first-order interaction

y_all_stnd <- (yall - mean(y_train)) / sd(y_train)


############## create giant matrix of all 1st-order interactions
dim(Uall)

Xx_big <- matrix(NA, nrow(Uall), ncol(Uall)^2)

kk <- 0
for(ii in 1:ncol(Uall)) {
    for(jj in 1:ncol(Uall)) {
        kk <- kk + 1
        Xx_big[ , kk ] <- Uall[ , ii ] * Uall[ , jj ]
    }
}

head(Xx_big)
dim(Xx_big)

Xx_train <- Xx_big[ xndx_train, ]
Xx_test  <- Xx_big[ xndx_test, ]


tXX_train <- crossprod(Xx_train) / length(xndx_train)
tXy_train <- ( t(Xx_train) %*% y_all_stnd[ xndx_train ] ) / length(xndx_train)




## xreg_vec <- seq(0.0001, 0.2, by=0.0001)

xreg_vec <- 10^seq(-7, 0, by=1)

Yhat <- matrix(NA, xlo, length(xreg_vec))

for(ii in 1:length(xreg_vec)) {
    
    xbhat <- solve( tXX_train + diag( xreg_vec[ii], ncol(tXX_train) ) ) %*% tXy_train
    
    yhat_test <- Xx_test %*% xbhat
    
    Yhat[ , ii ] <- yhat_test * sd(y_train) + mean(y_train)
    
    cat(ii, "\n")
    
}


mx_errs <- y_test - Yhat
xxers_mm <- apply( mx_errs, 2, function(x) { return( sqrt( mean( x^2 ) ) ) } ) ; #xxers_mm
which.min(xxers_mm)
cat("\n", "Best Ridge 1st-order interaction testing RMSE:", xxers_mm[ which.min(xxers_mm) ], " -- occurs at lambda =", xreg_vec[ which.min(xxers_mm) ], "\n")
plot( 0:(length(xxers_mm)-1), xxers_mm, type="l", col="#22FF11", lwd=3, main="Ridge 1OI Test RMSE ~vs~ s val", xaxt="n")
axis(1, xreg_vec, at=0:(length(xxers_mm)-1))
#plot( xreg_vec, xxers_mm, type="l", col="#22FF11", lwd=3, main="Ridge 1OI Test RMSE ~vs~ s val")

sd(yall[ xndx_test ])

xxers_mm[ which.min(xxers_mm) ]


######################################## XGBoost
######################################## XGBoost
######################################## XGBoost
######################################## XGBoost


library(xgboost)


#data(agaricus.train, package='xgboost')

#dtrain <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)


dtrain <- xgb.DMatrix(U_train, label = y_train)


param <-
list(
max_depth = 2,
eta = 1.0,
verbose = 0,
nthread = 2,
objective = "reg:squarederror"
)



bst <- xgb.train(param, dtrain, nrounds = 2000)

yhat <- predict(bst, newdata=U_test)

sqrt( mean( (y_test - yhat)^2 ) )



######################################### Neural Net
######################################### Neural Net
######################################### Neural Net
######################################### Neural Net


### I had to use python 2.7 (tensorflow not available for 3.9)

#which python

#python -m pip install pip

### may have to use sudo for these

#pip install --upgrade pip
#pip install tornado
#pip install nose
#pip install keras
#pip install tensorflow



library(keras)

library(tensorflow)


################## model 1

model <- keras_model_sequential()

model %>%
##layer_flatten(input_shape = c(28, 28)) %>% ############ converts image to vector
#layer_flatten(input_shape = 1) %>% ############ converts image to vector
#layer_dense(units = 300, activation = 'softmax') %>%
#layer_dense(units = 300, input_shape=ncol(Uall), activation = 'relu') %>%
layer_dense(units = 100, input_shape=ncol(Uall), activation = 'relu') %>%
layer_dense(units = 10, activation = 'relu') %>%
layer_dense(units = 1, activation = 'relu') ### but should use this -- only two categories



model %>% compile(
optimizer = 'adam',
#optimizer = 'sgd',
#loss = 'sparse_categorical_crossentropy',
loss = 'mse',
#metrics = c('root_mean_squared_error')
#metrics = c('MeanSquaredError')
metrics = c('RootMeanSquaredError')
)


## model %>% fit(U_train, y_train, epochs = 200) ## too much

model %>% fit(U_train, y_train, epochs = 42)

str(model)


get_weights(object=model)


score <- model %>% evaluate(U_test, y_test)

cat('Test loss:', score$loss, "\n")
#cat('Test accuracy:', score$acc, "\n")




################## model 2

model <- keras_model_sequential()

model %>%
##layer_flatten(input_shape = c(28, 28)) %>% ############ converts image to vector
#layer_flatten(input_shape = 1) %>% ############ converts image to vector
#layer_dense(units = 300, activation = 'softmax') %>%
#layer_dense(units = 300, input_shape=ncol(Uall), activation = 'relu') %>%
layer_dense(units = 40, input_shape=ncol(Uall), activation = 'relu') %>%
layer_dense(units = 7, activation = 'relu') %>%
layer_dense(units = 1, activation = 'relu') ### but should use this -- only two categories



model %>% compile(
optimizer = 'adam',
#optimizer = 'sgd',
#loss = 'sparse_categorical_crossentropy',
loss = 'mse',
#metrics = c('root_mean_squared_error')
#metrics = c('MeanSquaredError')
metrics = c('RootMeanSquaredError')
)


## model %>% fit(U_train, y_train, epochs = 200) ## too much

model %>% fit(U_train, y_train, epochs = 42)

str(model)


get_weights(object=model)


score <- model %>% evaluate(U_test, y_test)

cat('Test loss:', score$loss, "\n")
#cat('Test accuracy:', score$acc, "\n")



######################################### fold over -- only really appropriate with hybrid
######################################### fold over
######################################### fold over
######################################### fold over
######################################### fold over


m_tot <- 10
## m_tot <- 21

######################### this is good -- 63.79350
xcmact_hybrid <-
f_control_mactivate(
param_sensitivity = 10^9,
bool_free_w       = TRUE,
#w0_seed           = 0.1,
w0_seed           = 0.01,
#w_col_search      = "alternate",
w_col_search      = "one",
bool_headStart    = TRUE,
antifreeze        = TRUE, #### must be true to escape
max_internal_iter = 100, #####
ss_stop           = 10^(-10), ### -14 very small
escape_rate       = 1.003,
step_size         = 1,
Wadj              = 1/1,
force_tries       = 0,
lambda            = 1/10000, ####
tol               = 10^(-10) ### -14 hybrid only
)


xcmact_hybrid <-
f_control_mactivate(
param_sensitivity = 10^9,
bool_free_w       = TRUE,
#w0_seed           = 0.1,
w0_seed           = 0.01,
#w_col_search      = "alternate",
w_col_search      = "one",
bool_headStart    = TRUE,
antifreeze        = TRUE, #### must be true to escape
max_internal_iter = 100, #####
ss_stop           = 10^(-12), ### -14 very small
escape_rate       = 1.003,
step_size         = 1,
Wadj              = 1/1,
force_tries       = 0,
lambda            = 1/100, ####
tol               = 10^(-12) ### -14 hybrid only
)




X_BIG_train <- X_train
X_BIG_test <- X_test
## X_BIGOUT_test <- XbigTest

y_train <- yall[ xndx_train ]
y_test <- yall[ xndx_test ]


#Y <- f_stndX(X_BIG_train)
#apply(Y, 2, sd)
#apply(Y, 2, mean)

xls_y_train <- list()
xls_y_test <- list()
#xls_y_bigTest <- list()

xBIG_ls <- list()


xxnow <- Sys.time()
for(xxi in 1:m_tot) {
    
    
    xBIG_ls[[ xxi ]] <-
    f_fit_hybrid_01(
    X = X_BIG_train,
    y = y_train,
    m_tot = 1,
    U = U_train,
    m_start = 1,
    mact_control = xcmact_hybrid,
    verbosity = 1
    )

    

    xls_y_train[[ xxi ]]   <- predict(object=xBIG_ls[[ xxi ]], X0=X_BIG_train, U0=U_train, mcols=1 )
    xls_y_test[[ xxi ]]    <- predict(object=xBIG_ls[[ xxi ]], X0=X_BIG_test, U0=U_test, mcols=1 )
    #xls_y_bigTest[[ xxi ]] <- predict(object=xBIG_ls[[ xxi ]], X0=X_BIGOUT_test, U0=UbigTest_trim, mcols=1 )
    
    xWhat <- xBIG_ls[[ xxi ]][[ 1 + 1 ]][[ "What" ]]
    
    #Xstar_train <- f_sign_log( x = f_stndX( f_mactivate(U=U_train, W=xWhat) ) )
    Xstar_train <- f_stndX( f_mactivate(U=U_train, W=xWhat) )
    X_BIG_train <- cbind(X_BIG_train, Xstar_train)
    
    #Xstar_test <- f_sign_log( x = f_stndX( f_mactivate(U=U_test, W=xWhat) ) )
    Xstar_test <- f_stndX( f_mactivate(U=U_test, W=xWhat) )
    X_BIG_test <- cbind(X_BIG_test, Xstar_test)
    
    #Xstar_bigTest <- f_sign_log( x = f_stndX( f_mactivate(U=UbigTest_trim, W=xWhat) ) )
    #X_BIGOUT_test <- cbind(X_BIGOUT_test, Xstar_bigTest)

    
    cat("\n\n\n\n\n", "Done with big loop:",  xxi, "\n\n\n\n\n")
    
}
cat( difftime(Sys.time(), xxnow, units="mins"), "\n" )

##################### use Xstar 1







######### check test error


xerr_train <- unlist( lapply( xls_y_train, ffun, y=y_train ) ) ; xerr_train
xerr_test <- unlist( lapply( xls_y_test, ffun, y=y_test ) ) ; xerr_test
#xerr_bigTest <- unlist( lapply( xls_y_bigTest, ffun, y=dfBigSolution[ , "yhat" ] ) ) ; xerr_bigTest



par(mfrow=c(3,1))
plot(1:(length(xerr_train)), xerr_train, type="l", xlab="m", ylab="RMSE Cost")
plot(1:(length(xerr_test)), xerr_test, type="l", xlab="m", ylab="RMSE Cost")
#plot(1:(length(xerr_bigTest)), xerr_bigTest, type="l", xlab="m", ylab="RMSE Cost")



xlm <- lm( y_test ~ X_BIG_test )

summary(xlm)




