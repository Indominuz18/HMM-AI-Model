# install needed packages by running these lines once
#install.packages("corrr")
#install.packages("ggcorrplot")
#install.packages("FactoMineR")
#install.packages('factoextra')

library(dplyr)
library(xts)
library('corrr')
library(zoo)
library(ggcorrplot)
library("FactoMineR")
library('factoextra')
library(depmixS4)


#df <- read.table("C:/Users/amy36/Desktop/HOMEWORK/CMPT318/Assignments/CMPT-318/Assignments/Term Project/datasets/TermProjectData.txt", header = TRUE, sep = ",")


loadDataset <- function(path){
  df <- read.table(path, header = TRUE, sep = ",")
  df$Date <- as.Date(df$Date, format="%d/%m/%Y")
  df$timestamp <- as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S", tz="UTC")
  df$Global_intensity <- na.approx(df$Global_intensity, na.rm=FALSE, rule=2)
  df$Global_active_power <- na.approx(df$Global_active_power, na.rm=FALSE, rule=2)
  df$Global_reactive_power <- na.approx(df$Global_reactive_power, na.rm=FALSE, rule=2)
  df$Voltage <- na.approx(df$Voltage, na.rm=FALSE, rule=2)
  df$Sub_metering_1 <- na.approx(df$Sub_metering_1, na.rm=FALSE, rule=2)
  df$Sub_metering_2 <- na.approx(df$Sub_metering_2, na.rm=FALSE, rule=2)
  df$Sub_metering_3 <- na.approx(df$Sub_metering_3, na.rm=FALSE, rule=2)
  return(df)
}

# use the portion of the dataset before September as training data, every value after the start of September can be used as test data.




df <- loadDataset("C:/Users/Chris/Documents/University/Fall 2023/CMPT 318/Assignments/Term Project/datasets/TermProjectData.txt")

df.names <- colnames(df)[3:9]
resp.names <- c("resp1", "resp2", "resp3") #<-----

weekday = "Wednesday"
timewindow = "T08:00:00/T13:59:00"

# --- Scale the train/test data-set
df_numeric <- df %>% select_if((is.numeric))
df_scaled <- scale(df_numeric)
df.scaled <- data.frame(df_scaled)
df.scaled$timestamp <- df$timestamp

df.scaled$timestamp <- as.POSIXct(df.scaled$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
for (name in df.names){
  df.scaled[,name] <- as.numeric(df.scaled[,name])
}

## Do Principal Component Analysis in train_scaled
data.pca <- prcomp(df_scaled)
summary(data.pca)

# --- feature selection
## extract loading/rotation scores
loading_scores <- data.pca$rotation

# scree plot visualization. we see that the first 3 principal components cover 74% of the data variances
fviz_eig(data.pca, addlabels = TRUE)


## Rank the most important features of df based off of PCA loading scores
## This gives the importance of the each feature in explaining the variance in the data
## features with the highest ranking will be chosen for our responses
feature_sums <- abs((loading_scores[,1]))
important_features <- feature_sums[order(feature_sums)[5:7]] #<----
#important_features <- feature_sums[order(feature_sums)[5:7]] #<----



# --- Split into train and test sets

# Window the data, then split the data set into test/train sets, and whole set
data.xts <- xts(df.scaled, order.by = df.scaled$timestamp)
data.win <- data.xts[weekdays(index(data.xts)) == weekday]
data.win <- data.win[timewindow]
weeks.win <- split(data.win, f="weeks")
sample <- sample(c(TRUE, FALSE), length(weeks.win), replace=TRUE, prob=c(0.8,0.2))
train <- weeks.win[sample]#data.win$timestamp >= "2008-01-01"]
test <- weeks.win[!sample]#data.win$timestamp < "2008-01-01"]
train <-  data.frame(do.call(rbind, train))
test <- data.frame(do.call(rbind, test))
data <- data.frame(data.win)



# Renumerise test/train data
test$timestamp <- as.POSIXct(test$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
for (name in df.names){
  test[,name] <- as.numeric(test[,name])
}

train$timestamp <- as.POSIXct(train$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
for (name in df.names){
  train[,name] <- as.numeric(train[,name])
}

data$timestamp <- as.POSIXct(data$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
for (name in df.names){
  data[,name] <- as.numeric(data[,name])
}

# compute the correlation matrix
corr_matrix <- cor(df[,df.names])
g1<-ggcorrplot(corr_matrix)
g1

corr_matrix <- cor(df[,names(important_features)])
g2<-ggcorrplot(corr_matrix)
g2



# --- Scale the train/test data-set
#train_numeric <- train %>% select_if((is.numeric))
#train_scaled <- scale(train_numeric ,scale=attributes(df_scaled)$'scaled:scale', center=attributes(df_scaled)$'scaled:center')

#test_numeric <- test %>% select_if((is.numeric))
#test_scaled <- scale(test_numeric, scale=attributes(df_scaled)$'scaled:scale', center=attributes(df_scaled)$'scaled:center')
# reframe test data
#test.scaled <- data.frame(test_scaled)
#test.scaled$timestamp <- test$timestamp


## Do Principal Component Analysis in train_scaled
#data.pca <- prcomp(train_scaled)
#summary(data.pca)
#pca.data <- data.frame(data.pca$x[,1:3])

# re-frame train data
#train.scaled <- data.frame(train_scaled)
#train.scaled$timestamp <- train$timestamp

# --- feature selection
## extract loading/rotation scores
#loading_scores <- data.pca$rotation

# scree plot visualization. we see that the first 3 principal components cover 74% of the data variances
scREE <-fviz_eig(data.pca, addlabels = TRUE)
scREE



## Rank the most important features of df based off of PCA loading scores
## This gives the importance of the each feature in explaining the variance in the data
## features with the highest ranking will be chosen for our responses
#feature_sums <- abs(rowSums(loading_scores[,1:4]))
#important_features <- feature_sums[order(feature_sums)[5:7]] #<----
#important_features <- feature_sums[order(feature_sums)[5:7]] #<----


# --- Window the train data-set
#data.xts <- xts(train.scaled, order.by = train.scaled$timestamp)
#data.win <- data.xts[weekdays(index(data.xts)) == weekday]
#data.win <- data.win[timewindow]
#weeks.win <- split(data.win, f="weeks")
#sample <- sample(c(TRUE, FALSE), length(weeks.win), replace=TRUE, prob=c(0.65,0.35))
#train.win <- weeks.win[sample]#data.win$timestamp >= "2008-01-01"]
#responses.test <- weeks.win[!sample]#data.win$timestamp < "2008-01-01"]
#train <- data.frame(do.call(rbind, train.win))
#responses.test <- data.frame(do.call(rbind, responses.test))
#responses.data <- data.frame(data.win)

# --- Window the test data-set
#data.xts <- xts(test.scaled, order.by = test.scaled$timestamp)
#data.win <- data.xts[weekdays(index(data.xts)) == weekday]
#data.win <- data.win[timewindow]
#weeks.win <- split(data.win, f="weeks")
#sample <- sample(c(TRUE, FALSE), length(weeks.win), replace=TRUE, prob=c(0.65,0.35))
#test.win <- weeks.win[sample]#data.win$timestamp >= "2008-01-01"]
#responses.test <- weeks.win[!sample]#data.win$timestamp < "2008-01-01"]
#test <- data.frame(do.call(rbind, test.win))
#responses.test <- data.frame(do.call(rbind, responses.test))
#responses.data <- data.frame(data.win)


# Renumerise train data
#train$timestamp <- as.POSIXct(train$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
#for (name in df.names){
 # train[,name] <- as.numeric(train[,name])
#}

# Renumerise test data
#test$timestamp <- as.POSIXct(test$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
#for (name in df.names){
#  test[,name] <- as.numeric(test[,name])
#}

#responses.test$timestamp <- as.POSIXct(responses.test$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
#for (name in resp.names){
#  responses.test[,name] <- as.numeric(responses.test[,name])
#}

#responses.data$timestamp <- as.POSIXct(responses.data$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
#for (name in resp.names){
# responses.data[,name] <- as.numeric(responses.data[,name])
#}

## load our train response data
responses.train <- train[,names(important_features)]
responses.train$timestamp <- train$timestamp
#colnames(responses.train)[1:2] <- c("resp2", "resp3") #<----
colnames(responses.train)[1:3]<- c("resp1","resp2", "resp3") 

## load our test response data
responses.test <- test[,names(important_features)]
responses.test$timestamp <- test$timestamp
#colnames(responses.test)[1:2] <- c("resp2", "resp3") #<----
colnames(responses.test)[1:3] <- c("resp1","resp2", "resp3") 



# check how much of each variable contributes to the principal components
fviz_cos2(data.pca, choice = "var", axes = 1:3)

set.seed(123)

logLik_values <- numeric(length = 6) # For states 4 to 20
BIC_values <- numeric(length = 6)
DoNLL_values <- numeric(length = 6)
NLL_values <- numeric(length = 6)

options(warn=-1)

for(nstates in 1:6){
  print((nstates *4))
  ntimes = c(rep(360,nrow(responses.train)/360))
  
  model <- depmix(response= list(responses.train$resp2 ~1, responses.train$resp3 ~1),data=train, ntimes=ntimes, nstates=(nstates *4), family = list(gaussian(),gaussian()))
  #model <- depmix(response= list(responses.train$resp1 ~1,responses.train$resp2 ~1, responses.train$resp3 ~1),data=train, ntimes=ntimes, nstates=(nstates *4), family = list(gaussian(),gaussian(),gaussian()))
  
  #model <- depmix(response= list(responses.train$resp1 ~1,responses.train$resp2 ~1), ntimes=ntimes, nstates=(nstates *4), family = list(gaussian(),multinomial("identity")))
  #model <- depmix(response = c(pca.train[, 'PC1'] ~ 1, pca.train[, 'PC2'] ~ 1), data=pca.train, nstates = (nstates *4), ntimes = ntimes, family = list(gaussian(), gaussian()))
  #model <- depmix(response = c(test.win[, 'Global_active_power'] ~ 1, test.win[, 'Global_reactive_power'] ~ 1, test.win[, 'Global_intensity'] ~ 1, test.win[, 'Voltage'] ~ 1, test.win[, 'Sub_metering_1'] ~ 1, test.win[, 'Sub_metering_2'] ~ 1, test.win[, 'Sub_metering_3'] ~ 1), data=test.win, nstates = nstates, ntimes = ntimes, family = list(gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian()))
  fitment<-NULL
  fitment<- try({
    fit(model, emcontrol=em.control(random.start = TRUE), verbose=TRUE)
  })
  if(inherits(fitment, "try-error")){
    fitment <- tryCatch({fit(model, emcontrol=em.control(random.start = TRUE), verbose=TRUE)}, error=function(e){})
  }
  print(fitment) 
  logLik_values[(nstates)] <- logLik(fitment)
  BIC_values[(nstates)] <- BIC(fitment)
  
  
  ntimes = c(rep(360,nrow(responses.test)/360))
  test.model <- depmix(response = list(responses.test$resp2 ~1,responses.test$resp3 ~1),data=test, nstates = (nstates *4), ntimes = ntimes, family = list(gaussian(),gaussian()))
  #test.model <- depmix(response = list(responses.test$resp1 ~1,responses.test$resp2 ~1,responses.test$resp3 ~1),data=test, nstates = (nstates *4), ntimes = ntimes, family = list(gaussian(),gaussian(),gaussian()))
  sp <- setpars(test.model, values=getpars(fitment))
  #print(forwardbackward(sp)$logLike)
  #DoNLL_values[nstates] <- (logLik(fitment)/nrow(responses.train) + forwardbackward(sp)$logLike/nrow(responses.test))
  NLL_values[nstates] <- forwardbackward(sp)$logLike/nrow(responses.test)
} 
DoNNL_values = logLik_values + NLL_values

results_df <- data.frame(States = 1:6, LogLikelihood = logLik_values, BIC = BIC_values, DoNLL = DoNLL_values)

p<- ggplot(results_df, aes(x = States)) +
  geom_line(aes(y = LogLikelihood, color = "LogLikelihood")) +
  geom_line(aes(y = BIC, color = "BIC")) +
  geom_line(aes(y = DoNLL, color = "DoNLL")) +
  labs(title = "Model Selection Criteria", x = "Number of States x4", y = "Criteria Value") +
  theme_minimal() +  scale_x_continuous(breaks=seq(1,20,2))

p

## Fit best model: 
best_fit <-NULL
best_logLik <- -Inf

ntimes = c(rep(360,nrow(responses.train)/360))
nstates = 12
model <- depmix(response= list(responses.train$resp2 ~1, responses.train$resp3 ~1),data=train, ntimes=ntimes, nstates=nstates, family = list(gaussian(),gaussian()))

for (i in 1:5) {
  fit_attempt<-tryCatch({
    fit(model, verbose=TRUE)
  }, error=function(e){})
  if(!inherits(fit_attempt, "try-error") && logLik(fit_attempt)>best_logLik){
    best_fit <- fit_attempt
    best_logLik <- logLik(fit_attempt)
  }
}
if(!is.null(best_fit)){
  trained.model<-best_fit
} else{
  simpleError("No model was fit")
}

#ntimes = c(rep(360,nrow(responses.train)/360))

#model <- depmix(response= list(responses.train$resp1 ~1,responses.train$resp2 ~1, responses.train$resp3 ~1),data=train, ntimes=ntimes, nstates=nstates, family = list(gaussian(),gaussian(),gaussian()))

trainedmodel.fitment <- (logLik(trained.model)/nrow(responses.train))

ntimes = c(rep(360,nrow(responses.test)/360))
#test.model <- depmix(response = list(pca.test$PC1 ~ 1, pca.test$PC2 ~ 1), data=test, nstates = nstates, ntimes = ntimes, family = list(gaussian(), gaussian(), gaussian()))
test.model <- depmix(response = list(responses.test$resp2 ~1,responses.test$resp3 ~1), data=test, nstates = nstates, ntimes = ntimes, family = list(gaussian(),gaussian()))
sp <- setpars(test.model, values=getpars(trained.model))

# Moment of truth: Scaled log-likelihood ratios: fitment ratio
##  log-likelihood for each model is divided by the number of observations
##  Higher log-likelihood values for the test data indicate a more accurate fitment

test.logLik <- forwardbackward(sp)$logLike
train.logLik <- logLik(trained.model)

testmodel.fitment <- (test.logLik/nrow(responses.test))#/(train.logLik/nrow(pca.train)) #llratio(model1, model2)
testmodel.fitment
test.logLik
train.logLik

#Analyse log-likelihood's of anomalous data sets

anom1 <- loadDataset("C:/Users/Chris/Documents/University/Fall 2023/CMPT 318/Assignments/Term Project/datasets/Anomalous Datasets/DataWithAnomalies1.txt")

# scale anomaly data
anom1_numeric <- anom1 %>% select_if((is.numeric))
anom1_scaled <- scale(anom1_numeric, scale=attributes(df_scaled)$'scaled:scale', center=attributes(df_scaled)$'scaled:center')
anom1.scaled <- data.frame(anom1_scaled)
anom1.scaled$timestamp <- anom1$timestamp


# Window anomaly data
data.xts <- xts(anom1.scaled, order.by = anom1.scaled$timestamp)
data.win <- data.xts[weekdays(index(data.xts)) == weekday]
data.win <- data.win[timewindow]
anom1.data <- data.frame(data.win)

# Renumerise anomaly data
anom1.data$timestamp <- as.POSIXct(anom1.data$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
for (name in df.names){
  anom1.data[,name] <- as.numeric(anom1.data[,name])
}

## load our anom1 response data
responses.anom1 <- anom1.data[,names(important_features)]
responses.anom1$timestamp <- anom1.data$timestamp
#colnames(responses.anom1)[1:2] <- c("resp2", "resp3") #<----
colnames(responses.anom1)[1:3] <- c("resp1","resp2", "resp3") #<----

ntimes = c(rep(360,nrow(responses.anom1)/360))
anom1.model <- depmix(response = list(responses.anom1$resp2 ~ 1, responses.anom1$resp3 ~ 1), data=anom1.data, nstates = nstates, ntimes = ntimes, family = list(gaussian(), gaussian()))
#anom1.model <- depmix(response = list(responses.anom1$resp1 ~ 1, responses.anom1$resp2 ~ 1, responses.anom1$resp3 ~ 1), data=anom1.data, nstates = nstates, ntimes = ntimes, family = list(gaussian(), gaussian(), gaussian()))
#anom1.fitment <- fit(anom1.model)
anom1.sp <- setpars(anom1.model, values=getpars(trained.model))
anom1.logLik <- forwardbackward(anom1.sp)$logLike
#train.logLik <- logLik(trained.model)

anom1.fitment <- (anom1.logLik/nrow(responses.anom1))#/(train.logLik/nrow(pca.train))
anom1.fitment
anom1.logLik
#train.logLik



anom2 <- loadDataset("C:/Users/Chris/Documents/University/Fall 2023/CMPT 318/Assignments/Term Project/datasets/Anomalous Datasets/DataWithAnomalies2.txt")
anom2_numeric <- anom2 %>% select_if((is.numeric))
anom2_scaled <- scale(anom2_numeric, scale=attributes(df_scaled)$'scaled:scale', center=attributes(df_scaled)$'scaled:center')
anom2.scaled <- data.frame(anom2_scaled)
anom2.scaled$timestamp <- anom2$timestamp

# Window anomaly data
data.xts <- xts(anom2.scaled, order.by = anom2.scaled$timestamp)
data.win <- data.xts[weekdays(index(data.xts)) == weekday]
data.win <- data.win[timewindow]
anom2.data <- data.frame(data.win)


# Renumerise anomaly data
anom2.data$timestamp <- as.POSIXct(anom2.data$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
for (name in df.names){
  anom2.data[,name] <- as.numeric(anom2.data[,name])
}

## load our anom2 response data
responses.anom2 <- anom2.data[,names(important_features)]
responses.anom2$timestamp <- responses.anom2$timestamp
#colnames(responses.anom2)[1:2] <- c("resp2", "resp3") #<----
colnames(responses.anom2)[1:3] <- c("resp1","resp2", "resp3") 

ntimes = c(rep(360,nrow(responses.anom2)/360))
anom2.model <- depmix(response = list(responses.anom2$resp2 ~ 1, responses.anom2$resp3 ~ 1), data=anom2.data, nstates = nstates, ntimes = ntimes, family = list(gaussian(), gaussian()))
#anom2.model <- depmix(response = list(responses.anom2$resp1 ~ 1, responses.anom2$resp2 ~ 1, responses.anom2$resp3 ~ 1), data=anom2.data, nstates = nstates, ntimes = ntimes, family = list(gaussian(), gaussian(), gaussian()))
anom2.sp <- setpars(anom2.model, values=getpars(trained.model))
anom2.logLik <- forwardbackward(anom2.sp)$logLike
#train.logLik <- logLik(trained.model)

anom2.fitment <- (anom2.logLik/nrow(responses.anom2))#/(train.logLik/nrow(pca.train))
anom2.fitment
anom2.logLik
#train.logLik



anom3 <- loadDataset("C:/Users/Chris/Documents/University/Fall 2023/CMPT 318/Assignments/Term Project/datasets/Anomalous Datasets/DataWithAnomalies3.txt")
anom3_numeric <- anom3 %>% select_if((is.numeric))
anom3_scaled <- scale(anom3_numeric, scale=attributes(df_scaled)$'scaled:scale', center=attributes(df_scaled)$'scaled:center')
anom3.scaled <- data.frame(anom3_scaled)
anom3.scaled$timestamp <- anom3$timestamp

# Window anomaly data
data.xts <- xts(anom3.scaled, order.by = anom3.scaled$timestamp)
data.win <- data.xts[weekdays(index(data.xts)) == weekday]
data.win <- data.win[timewindow]
anom3.data <- data.frame(data.win)

# Renumerise anomaly data
anom3.data$timestamp <- as.POSIXct(anom3.data$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
for (name in df.names){
  anom3.data[,name] <- as.numeric(anom3.data[,name])
}

## load our anom3 response data
responses.anom3 <- anom3.data[,names(important_features)]
responses.anom3$responses.anom3 <- responses.anom3$timestamp
#colnames(responses.anom2)[1:2] <- c("resp2", "resp3") #<----
colnames(responses.anom3)[1:3] <- c("resp1","resp2", "resp3") 

ntimes = c(rep(360,nrow(responses.anom3)/360))
anom3.model <- depmix(response = list(responses.anom3$resp2 ~ 1, responses.anom3$resp3 ~ 1), data=anom3.data, nstates = nstates, family = list(gaussian(), gaussian()))
#anom3.model <- depmix(response = list(responses.anom3$resp1 ~ 1, responses.anom3$resp2 ~ 1, responses.anom3$resp3 ~ 1), data=anom3.data, nstates = nstates, family = list(gaussian(), gaussian(), gaussian()))
anom3.sp <- setpars(anom3.model, values=getpars(trained.model))
anom3.logLik <- forwardbackward(anom3.sp)$logLike
#train.logLik <- logLik(trained.model)

anom3.fitment <- (anom3.logLik/nrow(responses.anom3))#/(train.logLik/nrow(pca.train))
anom3.fitment
anom3.logLik
#train.logLik

## All the anomalous data sets have negative fitment ratio, or Nan values
#   The non-anomalous testing data set has a positive fitment ratio.

## HMM anomaly detection model: 2023-11-20

#  - Amy Cao, caoamyc
#  - Khaled Taseen, kta93
#  - Kit Peer, kpeer
#  - Manmeet Singh, msa299
# Good work everyone! :)

