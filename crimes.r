library(ggplot2)
#library(tidyr)
library(dplyr)
library(stringr) 

crimes_data <- read.csv("crime.csv")
crimes_data <- crimes_data%>% filter (OFFENSE_CODE!=0 & OFFENSE_CODE_GROUP!='' & YEAR ==2018 & MONTH >=1 & MONTH<10 &  
                                        !is.na(REPORTING_AREA) & !is.na(Lat) & !is.na(Long) & Lat>42 & Long< (-70) & DISTRICT!='')

crimes_data <- crimes_data %>% filter (!is.na(UCR_PART) & UCR_PART!='Other' & UCR_PART!='' & UCR_PART!='Part Three')


#change to factor and rename levels
crimes_data$UCR_PART <- factor(crimes_data$UCR_PART)
crimes_data$DISTRICT <- factor(crimes_data$DISTRICT)
levels(crimes_data$UCR_PART) <- list(Part.one="Part One", Part.two="Part Two")
summary(crimes_data)

# this classification is done based on definition of violent crimes in US and filtered based on data found relevant in dataset
crimes_data <- crimes_data%>% mutate(Category = ifelse(OFFENSE_CODE_GROUP %in% c("Explosives","Aggravated Assault","Arson","Ballistics",
                                                                                 "Bomb Hoax","Burglary - No Property Taken","Commercial Burglary",
                                                                                 "Criminal Harassment","Drug Violation","Explosives","Firearm Discovery",
                                                                                 "Harassment","Firearm Violations","Homicide","HUMAN TRAFFICKING",
                                                                                 "Larceny","Other Burglary","Other Burglary","Residential Burglary",
                                                                                 "Simple Assault","HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE"),"Violent","NonViolent"))
crimes_data$Category <- factor(crimes_data$Category)



summary(crimes_data)
#------------EDA---------------------------------------------------
ggplot(data=crimes_data, aes(x=OFFENSE_CODE)) +geom_histogram()
#ggplot(data=crimes_data, aes(x=log(REPORTING_AREA),y=UCR_PART)) +geom_point()
ggplot(data=crimes_data, aes(x=HOUR)) +geom_histogram()
ggplot(data=crimes_data, aes(x=Lat)) +geom_histogram()
ggplot(data=crimes_data, aes(x=Long)) +geom_histogram()

ggplot(data=crimes_data, aes(x=as.factor(MONTH), fill=as.factor(MONTH))) +geom_bar()
ggplot(data=crimes_data, aes(x=UCR_PART,fill=UCR_PART)) +geom_bar()
ggplot(data=crimes_data, aes(x=DAY_OF_WEEK,fill=DAY_OF_WEEK)) +geom_bar()
ggplot(data=crimes_data, aes(x=HOUR,colour=HOUR)) +geom_bar()
ggplot(data=crimes_data, aes(x=DISTRICT,fill=DISTRICT)) +geom_bar()



#----------------bivariate analysis ---------------------------------


ggplot(crimes_data,aes(x=HOUR,y=MONTH,color=Category))+geom_boxplot()

ggplot(crimes_data,aes(x=HOUR,y=OFFENSE_CODE,color=Category))+geom_boxplot()

ggplot(crimes_data, aes(x = OFFENSE_CODE, fill = DAY_OF_WEEK)) +
  geom_density(alpha = .3)

ggplot(crimes_data, aes(y =HOUR, x = Category)) +
  geom_boxplot()

ggplot(data = crimes_data, aes(x = Category, fill=DAY_OF_WEEK) )+
  geom_bar(position = "dodge") 

#-------------------------spatial analysis---------------------------

#spatial data analysis
library(rgdal)
library(dplyr)
library(sp)
library(rgeos)
library(leaflet)

library(ggplot2)
ldn1 <- readOGR(file.path("C:/Users/Yaminie/Desktop/DataAnalytics/boston_file"), layer = "boston_neighborhood_shapefiles_iq5")
proj4string(ldn1) <- CRS("+init=+ellps:clrk66")
ldn1.wgs84 <- spTransform(ldn1, CRS("+init=epsg:4326"))

# convert to dataframe
boston.map_df <- fortify(ldn1.wgs84)
summary(boston.map_df)


#creating a map

map1 <- ggplot(boston.map_df) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", colour = "black")
map1 + labs(x = "Longitude", y = "Latitude", title = "Map of Boston")


map1 + geom_point(data = crimes_data, aes(x = Long, y = Lat, colour = DISTRICT)) + 
  scale_colour_manual(values = rainbow(67)) + labs(x = "Longitude", y = "Latitude", title = "Map of Boston")


#dec <- crimes_data[crimes_data$Month == "6", ]
dec.assault <- crimes_data[crimes_data$OFFENSE_CODE_GROUP %in% c("Aggravated Assault","Assault"), ]
map1 + geom_point(data = dec.assault, aes(x = Long, y = Lat), colour = "red") + labs(title = "Boston assault regions")

dec.shoot <- crimes_data[crimes_data$SHOOTING=='Y',]
map1+ geom_point(data = dec.shoot, aes(x = Long, y = Lat), colour = "red") + labs(title = "Shootings in 2018")

dec.violent <- crimes_data[crimes_data$OFFENSE_CODE=='111',]
map1+ geom_point(data = dec.violent, aes(x = Long, y = Lat), colour = "red") + labs(title = "Homicide in 2018")


#---------------------split data-------------------------------------




set.seed(123)

#split train and test data in an 80/20 proportion
crimes_data[, "train"] <- ifelse(runif(nrow(crimes_data))<0.8, 1, 0)

#assign training rows to data frame trainset
trainset <- crimes_data[crimes_data$train == 1, ]
#assign test rows to data frame testset
testset <- crimes_data[crimes_data$train == 0, ]

#find index of "train" column
trainColNum <- grep("train", names(crimes_data))

#remove "train" column from train and test dataset
trainset <- trainset[, -trainColNum]
testset <- testset[, -trainColNum]
#---------------GLM---------------------------------------------------
#install.packages("mctest")
#mctest(crimes_data)


#---------------------------logistic regression model;-----------------------
library(class)
library(caret)
library(caTools)
model <- glm(Category ~ 
               DISTRICT+
               UCR_PART+
               MONTH+
               DAY_OF_WEEK+
               HOUR+
               Lat+
               Long, data=trainset, family = binomial)


varImp(model)
plot(model)
summary(model)
model
anova(model, test="Chisq")


pred.prob = predict(model, type="response")
pred.prob = ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, trainset$Category)
#mean(pred.prob == trainset$Category)



library(ROCR)
p <- predict(model, newdata=testset, type="response")
p <- ifelse(p>0.5,1,0)
table(p,testset$Category)
pr <- prediction(p, testset$Category)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

model2 <- glm(Category ~ 
               DISTRICT+
               
               MONTH+
               DAY_OF_WEEK+
               HOUR+
               Lat+
               Long, data=trainset, family = binomial)


library(ROCR)
p2 <- predict(model2, newdata=testset, type="response")
p2 <- ifelse(p2>0.5,1,0)
table(p2,testset$Category)
pr2 <- prediction(p2, testset$Category)
prf2<- performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2)

auc2 <- performance(pr2, measure = "auc")
auc2 <- auc@y.values[[1]]
auc2

# -------------------------------Load the rpart package-----------------------------------
# building a simple rpart classification tree
library(rpart)


prune_control <- rpart.control(maxdepth = 30, minsplit = 20,cp=0.048) 
m.recur <- rpart(Category ~ UCR_PART+
             DISTRICT+
             DAY_OF_WEEK+
             HOUR+
             Lat+
             Long, data = trainset, method = "class", control = prune_control)

# Compute the accuracy of the pruned tree
pred <- predict(m.recur,testset,type="class")
confusionMatrix(pred,testset$Category)
mean(pred == testset$Category)
#cross validation model preparation
plotcp(m.recur)


#library(caret)
set.seed(123)
ctrl <- trainControl(method = "cv", savePred=T, classProb=T)
mod <- train(Category ~ UCR_PART+
               DISTRICT+
               DAY_OF_WEEK+
               HOUR+
               Lat+
               Long, data=trainset, method = "rpart", trControl = ctrl)
head(mod$pred)
plot(mod)
mod
 pred <- predict(mod,newdata = testset)
confusionMatrix(pred,testset$Category)



#-------------------------------Random forest -----------------------



# Load the randomForest package
library(randomForest)

# Build a random forest model
set.seed(123)
rfmodel <- randomForest(Category ~ 
                          DISTRICT+
                          UCR_PART+
                          MONTH+
                          DAY_OF_WEEK+
                          HOUR+
                          Lat+
                          Long, data = trainset)


# Compute the accuracy of the random forest
pred_rf <- predict(rfmodel, trainset)
table(pred_rf, trainset$Category)
plot(rfmodel)

varImpPlot(rfmodel)
attributes(rfmodel)
rfmodel$confusion
rfmodel$classes
#hist treesize rf


plot(rfmodel$err.rate)

#tuning
#outofbag error visibility
tuneRF(trainset[,c("DISTRICT","UCR_PART","MONTH","DAY_OF_WEEK","HOUR","Lat","Long")],trainset[,"Category"],stepFactor = 0.5,plot=TRUE, ntreeTry = 400,
       improve = 0.05,trace=TRUE)

hist(treesize(rfmodel),colour="red", main="Nodes of a tree in Random Forest")
#partialPlot(rfmodel,trainset,trainset$Category,"Violent")

#ntree , mtry,importance=TRUE,proximity=TRUE
print(rfmodel)
