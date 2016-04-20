install.packages("lubridate")
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
install.packages('randomForest')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('MLmetrics')
install.packages('caTools')

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest) # classification algorithm
library(ggplot2)#Charts
library(dplyr)#Regex 
library(lubridate)#For Date Functions
library(rpart)#For CART trees
library(MLmetrics)#For calculating Log Loss
library(caTools)#for splitting training data into training and testing sets

setwd("C:\\Users\\v-arsol\\Documents\\kaggle")
train=read.csv('train.csv')
test=read.csv('test.csv')
summary(train)
names(train)[1] <- 'ID'
test$ID <- as.character(test$ID)
train=bind_rows(train,test)

train$TimeValue <- sapply(as.character(train$AgeuponOutcome),  
                         function(x) strsplit(x, split = " ")[[1]][1])

train$UnitofTime <- sapply(as.character(train$AgeuponOutcome),  
                          function(x) strsplit(x, split = " ")[[1]][2])

train$UnitofTime <- gsub('s', '', train$UnitofTime)
summary(train$UnitofTime)

multiplier <- ifelse(train$UnitofTime == 'day', 1,
                     ifelse(train$UnitofTime == 'week', 7,
                            ifelse(train$UnitofTime == 'month', 30, # Close enough
                                   ifelse(train$UnitofTime == 'year', 365, NA))))

# Apply our multiplier
train$TimeValue=as.numeric(train$TimeValue)
train$AgeinDays <- train$TimeValue * multiplier

summary(train$AgeinDays)
#Operations on Name. Creating a flag for animals having a name
train$Name=as.character(train$Name)
train$Name <- ifelse(nchar(train$Name)==0, 'Nameless', train$Name)
train$HasName[train$Name == 'Nameless'] <- 0
train$HasName[train$Name != 'Nameless'] <- 1

#Splitting the date time column
train$year=year(train$DateTime)
train$month=month(train$DateTime)
train$hour=hour(train$DateTime)
train$DayofWeek=weekdays(as.Date(train$DateTime))
train$IsWeekend <- ifelse(train$DayofWeek == 'Saturday', 1,
                     ifelse(train$DayofWeek == 'Sunday', 1,0))

#Predicting missing values for age
age_fit <- rpart(AgeinDays ~ AnimalType + HasName, 
                 data = train[!is.na(train$AgeinDays), ], 
                 method = 'anova')
train$AgeinDays[is.na(train$AgeinDays)] <- predict(age_fit, train[is.na(train$AgeinDays), ])

train$SexuponOutcome[is.na(train$SexuponOutcome)]="Unknown"
sum(is.na(train$SexuponOutcome))


train$Lifestage[train$AgeinDays < 365] <- 'baby'
train$Lifestage[train$AgeinDays >= 365] <- 'adult'
train$Lifestage[train$AgeinDays >= 5475] <- 'old'


train$Breed <- gsub('Mix', '/', train$Breed)
train$IsMix <- ifelse(grepl(c('/'), train$Breed), 1, 0)
sum(train$IsMix)
train$IsDomestic <- ifelse(grepl(c('Domestic'), train$Breed), 1, 0)


train$NewColor <- sapply(train$Color, 
                           function(x) strsplit(x, split = '/| ')[[1]][1])
train$NewBreed <- sapply(train$Breed, 
                         function(x) strsplit(x, split = '/| ')[[1]][1])



train$Lifestage <- factor(train$Lifestage)                                   
train$SexuponOutcome <- factor(train$SexuponOutcome)                                   
train$IsMix <- factor(train$IsMix)    
train$NewColor <- factor(train$NewColor)    
train$IsDomestic <- factor(train$IsDomestic)    
train$NewBreed <- factor(train$NewBreed)    

summary(train$NewColor)
levels(train$NewColor)
levels(train$NewBreed)



test  <- train[26730:nrow(train), ]
train <- train[1:26729, ]

set.seed(101)
sample = sample.split(train$OutcomeType, SplitRatio = .75)
train1 = subset(train, sample == TRUE)
train2 = subset(train, sample == FALSE)

#Random Forest
rf_mod <- randomForest(OutcomeType ~ AnimalType+Lifestage+HasName+IsWeekend+hour+SexuponOutcome+NewColor+AgeinDays+IsMix+IsDomestic+month, 
                       data = train1, 
                       ntree = 500, 
                       importance = TRUE)

rf_mod
plot(rf_mod, ylim=c(0,1))
legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)

importance    <- importance(rf_mod)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

varImportance
# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

plot(rf_mod)
varImpPlot(rf_mod)

#testing the model on remaining training set and calculating log loss
prediction <- predict(rf_mod, train2, type = 'vote')
MultiLogLoss(train2$OutcomeType,prediction)



#Applying the model on the test set
prediction <- predict(rf_mod, test, type = 'vote')
solution <- data.frame('ID' = test$ID, prediction)
write.csv(solution, 'rf_solution.csv', row.names = F)
