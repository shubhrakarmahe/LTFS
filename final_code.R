#--------------------------------------------------------------------------
# Vehicle Loan default prediction
#-------------------------------------------------------------------------

# load required libraries

load.libraries <- c('data.table', 'dplyr', 'MASS', 'dummies','car',
                    'lubridate', 'stringr', 'tidyr', 'ggplot2')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]

for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)

# Read dataset - train and test
train <- h2o.importFile('train.csv')
#train <- fread('train.csv',na.strings = c('',NA))
#test <- fread('test_bqCt9Pv.csv', na.strings = c('',NA))

# Preliminary analysis
head(train)
str(train)
summary(train)
dim(train) # 233154 * 41

head(test)
str(test)
summary(test)
dim(test) # 112392 * 40

#Missing values - Employment.Type - 3.28 % (train)
#                Employment.type - 3.08% (test)
sapply(train, function(x) sum(is.na(x))/nrow(train)*100)
sapply(test, function(x) sum(is.na(x))/nrow(test)*100)

# -------------------------Univariate Analysis and Data Preparation ----------------
# uniqueId (train) - No duplicates
# This column will be dropped during  model building
nrow(train) # 233154
uniqueN(train$UniqueID) # 233154

# uniqueId (test) - no duplicates
nrow(test) # 112392
uniqueN(test$UniqueID) # 112392

train$UniqueID <- NULL
#test$UniqueID <- NULL

# Date.of.birth - current format to be changed to yyyy-mm-dd
# function to convert year properly( limitation of lubridate package while converting yy to yyyy format)
year_conv <- function(x, year=1968){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}
train$dob_con <- year_conv(dmy(train$Date.of.Birth),1940)

test$dob_con <- year_conv(dmy(test$Date.of.Birth),1940)

#Min.      1st Qu.       Median         Mean      3rd Qu.           Max
#"1949-09-15" "1977-05-04" "1986-01-01" "1984-04-04" "1992-05-19"  "2000-10-20"
summary(train$dob_con)

summary(test$dob_con)

#derived variable age from date of birth
train$age <- as.integer(round((Sys.Date()-train$dob_con)/365,0))

test$age <- as.integer(round((Sys.Date()-test$dob_con)/365,0))

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#19      27      33      35      42      70 
summary(train$age)

summary(test$age)

# outlier - none
quantile(train$age, seq(0,1,0.01))
quantile(test$age, seq(0,1,0.01))

# derived variable age_bin
#train[which(age>=20 & age<=29),'age']
train$Date.of.Birth <- NULL
test$Date.of.Birth <- NULL

train$dob_con <- NULL
test$dob_con <- NULL

# Employment type
#Na's     Salaried Self employed 
#7661         97858        127635 
train$Employment.Type <- as.factor(train$Employment.Type)
summary(train$Employment.Type)

ggplot(data = train,aes(x = train$Employment.Type)) +
  geom_bar(fill= 'blue',color = 'black')

test$Employment.Type <- as.factor(test$Employment.Type)
summary(test$Employment.Type)

# let's impute missing values with mode value i.e. Self employed
train[which(is.na(Employment.Type)),'Employment.Type'] <- 'Self employed'

test[which(is.na(Employment.Type)),'Employment.Type'] <- 'Self employed'

# convert to 1 as Self employed and 0 as Salaried
train$Employment.Type <- ifelse(train$Employment.Type == 'Self employed' , 1, 0)
test$Employment.Type <- ifelse(test$Employment.Type == 'Self employed' , 1, 0)

# DisbursalDate 
train$disbursaldate_con <- year_conv(dmy(train$DisbursalDate),1940)

test$disbursaldate_con <- year_conv(dmy(test$DisbursalDate),1940)

#  Min.      1st Qu.       Median         Mean      3rd Qu.       Max
#"2018-08-01" "2018-08-30" "2018-09-25" "2018-09-23" "2018-10-21" "2018-10-31" 
summary(train$disbursaldate_con)

summary(test$disbursaldate_con)

# Disbursal date - to be dropped from model building
train$DisbursalDate <- NULL
train$disbursaldate_con <- NULL

test$DisbursalDate <- NULL
test$disbursaldate_con <- NULL

#Employee_code_id - seems like irrelevant column
train$Employee_code_ID <- NULL
test$Employee_code_ID <- NULL

#MobileNo_Avl_Flag
train$MobileNo_Avl_Flag <-as.factor(train$MobileNo_Avl_Flag)

test$MobileNo_Avl_Flag <-as.factor(test$MobileNo_Avl_Flag)

# redundant value of 1 - column to be dropped before model building
summary(train$MobileNo_Avl_Flag)
summary(test$MobileNo_Avl_Flag)

test$MobileNo_Avl_Flag <- NULL
train$MobileNo_Avl_Flag <- NULL


#PERFORM_CNS.SCORE.DESCRIPTION - can be dropped as score is sufficient
train$PERFORM_CNS.SCORE.DESCRIPTION <- NULL
test$PERFORM_CNS.SCORE.DESCRIPTION <- NULL


# average.acct.age.

avg.acct.age_temp <- matrix(unlist(strsplit(train$AVERAGE.ACCT.AGE," ")),ncol = 2,byrow=TRUE)
avg.acct.age_temp <- as.data.frame(avg.acct.age_temp)
avg.acct.age_temp$V1 <- str_remove(avg.acct.age_temp$V1,"yrs")
avg.acct.age_temp$V2 <- str_remove(avg.acct.age_temp$V2,"mon")

train$AVERAGE.ACCT.AGE.mons <- (as.integer(avg.acct.age_temp$V1)*12 + as.integer(avg.acct.age_temp$V2))

avg.acct.age_temp_test <- matrix(unlist(strsplit(test$AVERAGE.ACCT.AGE," ")),ncol = 2,byrow=TRUE)
avg.acct.age_temp_test <- as.data.frame(avg.acct.age_temp_test)
avg.acct.age_temp_test$V1 <- str_remove(avg.acct.age_temp_test$V1,"yrs")
avg.acct.age_temp_test$V2 <- str_remove(avg.acct.age_temp_test$V2,"mon")

test$AVERAGE.ACCT.AGE.mons <- (as.integer(avg.acct.age_temp_test$V1)*12 + as.integer(avg.acct.age_temp_test$V2))

train$AVERAGE.ACCT.AGE <- NULL
test$AVERAGE.ACCT.AGE <- NULL

# credit history length

credit.len_temp <- matrix(unlist(strsplit(train$CREDIT.HISTORY.LENGTH," ")),ncol = 2,byrow=TRUE)
credit.len_temp <- as.data.frame(credit.len_temp)
credit.len_temp$V1 <- str_remove(credit.len_temp$V1,"yrs")
credit.len_temp$V2 <- str_remove(credit.len_temp$V2,"mon")

train$CREDIT.HISTORY.LEN.mons <- (as.integer(credit.len_temp$V1)*12 + as.integer(credit.len_temp$V2))

credit.len_temp_t <- matrix(unlist(strsplit(test$CREDIT.HISTORY.LENGTH," ")),ncol = 2,byrow=TRUE)
credit.len_temp_t <- as.data.frame(credit.len_temp_t)
credit.len_temp_t$V1 <- str_remove(credit.len_temp_t$V1,"yrs")
credit.len_temp_t$V2 <- str_remove(credit.len_temp_t$V2,"mon")

test$CREDIT.HISTORY.LEN.mons <- (as.integer(credit.len_temp_t$V1)*12 + as.integer(credit.len_temp_t$V2))

train$CREDIT.HISTORY.LENGTH <- NULL
test$CREDIT.HISTORY.LENGTH <- NULL

# % of default in train dataset 78%
182543/nrow(train) * 100

#-------------------------H2o------------------------------

y <- "loan_default"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[,'loan_default'] <- as.factor(train[,'loan_default'])
test[,y] <- as.factor(test[,y])

# Run AutoML for 20 base models (limited to 1 hour max runtime by default)
aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  max_models = 20,
                  seed = 1)

# View the AutoML Leaderboard
lb <- aml@leaderboard
lb

#-------------------------------------------------------------

test_uniqueId <- test$UniqueID
# loan_default column is missing , so adding a place holder to execute xgboost
test_label <- as.data.frame(rep(0,nrow(test)))
colnames(test_label) <- 'loan_default'


train_label <- train[,c('loan_default'),with=F]
train_label$loan_default <- as.numeric(train_label$loan_default)

new_tr <- model.matrix(~.+0,data = train[,-c("loan_default"),with=F]) 
new_ts <- model.matrix(~.+0,data = test[,-c(1),with=F])

library(xgboost)

dtrain <- xgb.DMatrix(data = new_tr, label = train_label$loan_default) 
dtest <- xgb.DMatrix(data = new_ts, label = test_label$loan_default )

#default parameters
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=6, min_child_weight=1, 
               subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, 
                 nfold = 5, showsd = T, stratified = T, print_every_n = 10, 
                 early_stopping_rounds = 20, maximize = F)

# best iteration is 18


#first default - model training
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 18, 
                     watchlist = list(val=dtest,train=dtrain), 
                     print_every_n = 10, early_stopping_rounds = 20,
                     maximize = F , eval_metric = "error")
#model prediction
xgbpred <- predict (xgb1,dtest)

submission <- as.data.frame(cbind(test$UniqueID,xgbpred))
write.csv(submission,'submision_xg_14.csv',row.names = F)

# variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:10]) 

# ------------- Driver variables identified in logistic regression-------
# 1. Asset cost
# 2. ltv
# 3. Aadhar_flag
# 4. Pan_flag
# 5. Driving_Flag
# 6. Passport_Flag
# 7. PERFORM_CNS.SCORE  
# 8. PRI.ACTIVE.ACCTS 
# 9. PRI.OVERDUE.ACCTS 
# 10. PRI.CURRENT.BALANCE
# 11. DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS 
# 12. NO.OF_INQUIRIES 
# 13. age 


