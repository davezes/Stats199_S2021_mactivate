




options(width=300)


library(mactivate)






source("___f_funs.R")


###################### this is our data set

xdf <- read.csv( file.path( "___data", "mushrooms_audobon", "agaricus-lepiota.data" ), header=FALSE )

head(xdf)

dim(xdf)




Nall <- nrow(xdf)

yall <- f_dummy(x=xdf[ , "V1"], facname="")[ , 1]

xd_capShape   <- f_dummy(x=xdf[ , "V2"], facname="capShape_")
xd_capSurface <- f_dummy(x=xdf[ , "V3"], facname="capSurface_")
xd_capColor   <- f_dummy(x=xdf[ , "V4"], facname="capColor_")


xd_bruises    <- f_dummy(x=xdf[ , "V5"], facname="bruises_")

xd_odor       <- f_dummy(x=xdf[ , "V6"], facname="odor_")

xd_gillAttach    <- f_dummy(x=xdf[ , "V7"], facname="gillAttach_")
xd_gillSpace     <- f_dummy(x=xdf[ , "V8"], facname="gillSpace_")
xd_gillSize      <- f_dummy(x=xdf[ , "V9"], facname="gillSize_")
xd_gillColor     <- f_dummy(x=xdf[ , "V10"], facname="gillColor_")


xd_stalkShape    <- f_dummy(x=xdf[ , "V11"], facname="stalkShape_")
#xd_stalkRoot    <- f_dummy(x=xdf[ , "V12"], facname="stalkRoot_")
xd_stalkSurfAR   <- f_dummy(x=xdf[ , "V13"], facname="stalkSurfAR_")
xd_stalkSurfBR   <- f_dummy(x=xdf[ , "V14"], facname="stalkSurfBR_")
xd_stalkColAR    <- f_dummy(x=xdf[ , "V15"], facname="stalkColAR_")
xd_stalkColBR    <- f_dummy(x=xdf[ , "V16"], facname="stalkColBR_")


xd_veilType     <- f_dummy(x=xdf[ , "V17"], facname="veilType_")
xd_veilCol      <- f_dummy(x=xdf[ , "V18"], facname="veilCol_")

xd_ringNum      <- f_dummy(x=xdf[ , "V19"], facname="ringNum_")
xd_ringType     <- f_dummy(x=xdf[ , "V20"], facname="ringType_")


xd_sporeCol     <- f_dummy(x=xdf[ , "V21"], facname="sporeCol_")


xd_population   <- f_dummy(x=xdf[ , "V22"], facname="population_")


xd_habitat      <- f_dummy(x=xdf[ , "V23"], facname="habitat_")






X <-
cbind(
xd_capShape,
xd_capSurface,
xd_capColor,
xd_bruises,
xd_odor,
xd_gillAttach,
xd_gillSpace,
xd_gillSize,
xd_gillColor,
xd_stalkShape,
###xd_stalkRoot, ### missing
xd_stalkSurfAR,
xd_stalkSurfBR,
xd_stalkColAR,
xd_stalkColBR,
### xd_veilType, only one category
xd_veilCol,
xd_ringNum,
xd_ringType,
xd_sporeCol,
xd_population,
xd_habitat
)


########## GOOD
X <-
cbind(
#xd_capShape,
#xd_capSurface,
xd_capColor,
xd_bruises,
xd_odor,
#xd_gillAttach,
#xd_gillSpace,
#xd_gillSize,
#xd_gillColor,
xd_stalkShape
###xd_stalkRoot, ### missing
#xd_stalkSurfAR,
#xd_stalkSurfBR,
#xd_stalkColAR,
#xd_stalkColBR,
### xd_veilType, only one category
#xd_veilCol,
#xd_ringNum,
#xd_ringType,
#xd_sporeCol,
#xd_population,
#xd_habitat
)


######## GOOD
X <-
cbind(
#xd_capShape,
#xd_capSurface,
xd_capColor,
xd_bruises,
xd_odor,
#xd_gillAttach,
#xd_gillSpace,
#xd_gillSize,
#xd_gillColor,
xd_stalkShape,
###xd_stalkRoot, ### missing
#xd_stalkSurfAR,
#xd_stalkSurfBR,
#xd_stalkColAR,
#xd_stalkColBR,
### xd_veilType, only one category
#xd_veilCol,
#xd_ringNum,
#xd_ringType,
#xd_sporeCol,
xd_population,
xd_habitat
)




######## So-So
X <-
cbind(
#xd_capShape,
#xd_capSurface,
#xd_capColor,
#xd_bruises,
#xd_odor,
xd_gillAttach,
xd_gillSpace,
xd_gillSize,
xd_gillColor,
xd_stalkShape,
###xd_stalkRoot, ### missing
xd_stalkSurfAR,
xd_stalkSurfBR,
xd_stalkColAR,
xd_stalkColBR,
### xd_veilType, only one category
#xd_veilCol,
#xd_ringNum,
xd_ringType,
xd_sporeCol,
xd_population,
xd_habitat
)





head(X)

dim(X)


Xstnd <- t( ( t(X) - apply(X, 2, mean) ) / apply(X, 2, sd) )

Uall <- Xstnd

ydf <- data.frame("y"=yall, Xstnd)

#xlm <- lm(y~., data=ydf)
#summary(xlm)

xglm <- glm(y~., data=ydf, family=binomial(link="logit"))
summary(xglm)


xnamesRm <-
c(
"capShape_b",
"capSurface_f",
"capColor_b",
"bruises_f",
"odor_a",
"gillAttach_a",
"gillSpace_c",
"gillSize_b",

"gillColor_b",
"stalkShape_e",
"stalkSurfAR_f",
"stalkSurfBR_f",
"stalkColAR_b",
"stalkColBR_b",

"veilType_p",  #### only one cat

"veilCol_n",

"ringNum_n",
"ringNum_t",

"ringType_e",
"ringType_n",

"sporeCol_b",
"sporeCol_h",

"population_a",
"habitat_d",

"stalkColBR_c",
"stalkColBR_o"

)


Xall <- Xstnd[ , -which( colnames(Xstnd) %in% xnamesRm ) ]

head(Xall)
dim(Xall)

#xlo <- 2000

#xndx_train <- I(1:(Nall-2000))
#xndx_test <- setdiff( 1:Nall, xndx_train )


set.seed(777)
xndx_test <- sample(1:Nall, size=800)
#xndx_test <- which( rep_len(seq(1, 10, by=1), Nall) %in% 1 )

xndx_train <- setdiff( 1:Nall, xndx_test )



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
w_col_search      = "one",
#w_col_search      = "one",
bool_headStart    = FALSE,
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


########################## mactivate II








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
max_depth = 20,
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





