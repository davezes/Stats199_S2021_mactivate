




options(width=300)


library(mactivate)






source("___f_funs.R")


###################### this is our data set

xdf_train <- read.csv( file.path( "___data", "adult", "adult.data" ), header=FALSE )

head(xdf_train)
dim(xdf_train)

xtrain_vec <- rep("train", nrow(xdf_train))




xdf_test <- read.csv( file.path( "___data", "adult", "adult.test" ), header=FALSE, skip=1 )

head(xdf_test)
dim(xdf_test)

xdf_test[ , "V15"] <- gsub("\\.$", "", xdf_test[ , "V15"])

xtest_vec <- rep("test", nrow(xdf_test))




xdf <- rbind(xdf_train, xdf_test)

xtt_vec <- c( xtrain_vec, xtest_vec )

xndx_train <- which( xtt_vec %in% "train" )
xndx_test <- which( xtt_vec %in% "test" )

#xndx_train <- I(1:(Nall-2000))
#xndx_test <- setdiff( 1:Nall, xndx_train )



head(xdf)

Nall <- nrow(xdf)

yall <- f_dummy(x=xdf[ , "V15"], facname="")[ , 1]


x_age <- as.numeric(xdf[ , "V1"])


xd_empType   <- f_dummy(x=xdf[ , "V2"], facname="empType_")
head(xd_empType)


x_fnlwgt <- as.numeric(xdf[ , "V3"])


xd_edu <- f_dummy(x=xdf[ , "V4"], facname="edu_")
head(xd_edu)

x_edu <- as.numeric(xdf[ , "V5"])


xd_mstatus   <- f_dummy(x=xdf[ , "V6"], facname="mstatus_")
head(xd_mstatus)

xd_function    <- f_dummy(x=xdf[ , "V7"], facname="function_")
head(xd_function)


xd_relationship    <- f_dummy(x=xdf[ , "V8"], facname="relationship_")
head(xd_relationship)


xd_race    <- f_dummy(x=xdf[ , "V9"], facname="race_")
head(xd_race)


xd_gender    <- f_dummy(x=xdf[ , "V10"], facname="gender_")
head(xd_gender)


x_capgain <- as.numeric(xdf[ , "V11"])

x_caploss <- as.numeric(xdf[ , "V12"])

x_hrsperwk <- as.numeric(xdf[ , "V13"])

xd_nativeCountry    <- f_dummy(x=xdf[ , "V14"], facname="nativeCountry_")
head(xd_nativeCountry)






X <-
cbind(
x_age,
x_fnlwgt,

#x_edu,
xd_edu,

x_capgain,
x_caploss,
x_hrsperwk,

xd_empType,
#xd_edu,
xd_mstatus,
#xd_function,
xd_relationship,
xd_race,
xd_gender
#xd_nativeCountry
)



head(X)

dim(X)


Xstnd <- t( ( t(X) - apply(X, 2, mean) ) / apply(X, 2, sd) )

head(Xstnd)

Uall <- Xstnd

ydf <- data.frame("y"=yall, Xstnd)

#xlm <- lm(y~., data=ydf)
#summary(xlm)

xglm <- glm(y~., data=ydf, family=binomial(link="logit"))
summary(xglm)


xnamesRm <-
c(
"edu_ 10th",
"empType_ ?",
"mstatus_ Divorced",
"function_ ?",
"function_ Transport-moving",
"relationship_ Husband",
"race_ Amer-Indian-Eskimo",
#"race_ White",
"gender_ Female",
"nativeCountry_ ?"
)


Xall <- Xstnd[ , -which( colnames(Xstnd) %in% xnamesRm ) ]

head(Xall)
dim(Xall)



X_train <- Xall[ xndx_train, ]
U_train <- Uall[ xndx_train, ]
y_train <- yall[ xndx_train ]

X_test <- Xall[ xndx_test, ]
U_test <- Uall[ xndx_test, ]
y_test <- yall[ xndx_test ]


zdf <- data.frame("y"=yall, Xall)

xglm <- glm(y~., data=zdf, subset=xndx_train, family=binomial(link="logit"))
summary(xglm)
yhat_glm <- predict(xglm, newdata=zdf[ xndx_test, ], type="response")


mean( f_logit_cost(y=y_test, yhat=yhat_glm) )

####### usual LM
##sqrt( mean( ( y_test - yhat_lm )^2 ) )






########################## mactivate I



m_tot <- 7
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
ss_stop           = 10^(-15), ### -14 very small
escape_rate       = 1.002,
step_size         = 1,
Wadj              = 1/1,
force_tries       = 0,
lambda            = 1/300, ####
tol               = 10^(-15) ### -14 hybrid only
)



##X_train_use <- X_train + rnorm( prod(dim(X_train) ), 0, 1 )
X_train_use <- X_train

U_train_use <- U_train
##U_train_use <- X_train_use[ , c(1, 15) ]


xxnow <- Sys.time()
xxls_out <-
#f_fit_hybrid_01(
#f_fit_gradient_01(
f_fit_gradient_logistic_01(
X = X_train_use,
y = y_train,
m_tot = m_tot,
U = U_train_use,
m_start = 1,
mact_control = xcmact_hybrid,
verbosity = 3
)

cat( difftime(Sys.time(), xxnow, units="mins"), "\n" )







######################################################### numerical
yhatTT       <- matrix(NA, length(xndx_test), m_tot+1)


for(iimm in 0:m_tot) {
    yhat_test <- predict(object=xxls_out, X0=X_test, U0=U_test, mcols=iimm )
    yhatTT[ , iimm + 1 ] <- yhat_test

}


mx_errs <- y_test - yhatTT
xxers_mm <- apply( mx_errs, 2, function(x) { return( sqrt( mean( x^2 ) ) ) } ) ; xxers_mm
which.min(xxers_mm)
cat("\n", "Best testing RMSE:", xxers_mm[ which.min(xxers_mm) ], " -- occurs at w =", which.min(xxers_mm), "\n")
plot( 0:(length(xxers_mm)-1), xxers_mm, type="l", col="#22FF11", lwd=3, main="Test RMSE ~vs~ First # Columns of What")

plot(y_test, yhatTT[ , 5])

cor(y_test, yhatTT[ , 5])


########################################################## dichotomous

yhatTT       <- matrix(NA, length(xndx_test), m_tot+1)
mx_errs_dich <- matrix(NA, length(xndx_test), m_tot+1)

for(iimm in 0:m_tot) {
    yhat_test <- predict(object=xxls_out, X0=X_test, U0=U_test, mcols=iimm )
    yhatTT[ , iimm + 1 ] <- yhat_test[[ "p0hat" ]]
    mx_errs_dich[ , iimm + 1 ] <- f_logit_cost(y=y_test, yhat=yhat_test[[ "p0hat" ]])
}


xxers_mm <- apply( mx_errs_dich, 2, mean ) ; xxers_mm
which.min(xxers_mm)
cat("\n", "Best testing Logit Cost:", xxers_mm[ which.min(xxers_mm) ], " -- occurs at m =", which.min(xxers_mm)-1, "\n")
plot( 0:(length(xxers_mm)-1), xxers_mm, type="l", col="#22FF11", lwd=3, main="Test Logit Cost ~vs~ First # Columns of What")



yhat_test <- yhatTT[ , which.min(xxers_mm) - 1 ]

xtb <- table(y_test, yhat_test > median(yhat_test))

(xtb[1,1] + xtb[2,2]) / sum(xtb)



xtb <- table(y_test, yhat_test >= 0.5)

(xtb[1,1] + xtb[2,2]) / sum(xtb)

1 - (xtb[1,1] + xtb[2,2]) / sum(xtb)



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
eta = 1,
verbose = 0,
nthread = 2,
#objective = "reg:squarederror"
objective = "binary:logistic"
)



bst <- xgb.train(param, dtrain, nrounds = 2000)

yhat <- predict(bst, newdata=U_test)

## sqrt( mean( (y_test - yhat)^2 ) )

mean( f_logit_cost(y=y_test, yhat=yhat) )


xtb <- table(y_test, yhat > 0.5)

(xtb[1,1] + xtb[2,2]) / sum(xtb)






















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
#layer_dense(units = 1, activation = 'relu') ### but should use this -- only two categories
layer_dense(units = 1, activation = 'sigmoid') ### but should use this -- only two categories



model %>% compile(
optimizer = 'adam',
#loss = 'mse',
#optimizer = 'sgd',
loss = 'binary_crossentropy',
#loss = 'categorical_crossentropy',
#metrics = c('root_mean_squared_error')
#metrics = c('MeanSquaredError')
#metrics = c('RootMeanSquaredError')
metrics = c('accuracy')
)


## model %>% fit(U_train, y_train, epochs = 200) ## too much

model %>% fit(U_train, y_train, epochs = 42)

str(model)


get_weights(object=model)


score <- model %>% evaluate(U_test, y_test)

cat('Test loss:', score$loss, "\n")
#cat('Test accuracy:', score$acc, "\n")

cat('Test accuracy:', score$accuracy, "\n")



