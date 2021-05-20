print(getwd())
setwd("C:\\Users\\Jamie\\Documents\\R Projects\\Shrooms")

Shroom_dat = read.csv("mushrooms.csv")
dim(Shroom_dat)


anyNA(Shroom_dat) #No missing values 
class(Shroom_dat) #data frame 

Shroom_dat['class']

table(Shroom_dat$class) #classes reasonably balanced

###Label encoding the target###

target = ifelse(Shroom_dat$class == "e", 0,1 ) #if class = edible, make it 0, else make other class 1
table(target)

Shroom_dat = cbind(target, Shroom_dat)
Shroom_dat$target

#Visualising some of the features 

library(ggplot2)
library(ggthemes)

ggplot(data = Shroom_dat, aes(x = Shroom_dat$bruises, fill = Shroom_dat$bruises))+
  geom_bar(stat = 'count', position = position_dodge()) +
  facet_grid(Shroom_dat$target)

#Looking at the graph, top panel is not poisonous and indicates most shrooms that have bruises
# are not poisonous or edible. 
#Bottom panel is poisonous shrooms: Most shrooms that have no bruises are poisonous

ggplot(data = Shroom_dat, aes(x = Shroom_dat$odor, fill = Shroom_dat$odor))+
  geom_bar(stat = 'count', position = position_dodge())+
  facet_grid(Shroom_dat$target)

#Starting off with 6 features from the dataset 
X = Shroom_dat[,c("bruises", "odor", "cap.shape", "cap.surface", "population", "habitat")]
class(X)
head(X)

y = Shroom_dat[,c("target")]
head(y)
class(y)

# one hot encoding X 

library(caret)

# ~. is the formula to say all variables. 

Dum_X = dummyVars( "~ .", data = X, fullRank = TRUE) #full rank will avoid dummy variable trap, class - 1
DumX_tran = data.frame(predict(Dum_X, newdata = X))

Dum_tran_y = cbind(y, DumX_tran) #added y to the X dataframe. This is so ~ can be used when fitting
head(Dum_tran_y)

head(DumX_tran) #the values match the values from the python analysis :)
dim(Dum_tran_y)

#Fitting a Logistic regression model 
library(caTools)

set.seed(1)
split =sample.split(Dum_tran_y,SplitRatio = 2/3 ) #2/3 of data goes to training and 1/3 to test set

train=subset(Dum_tran_y, split = TRUE) # use as training data set, subset only  #returns values that meet criteria of true. 

test=subset(Dum_tran_y, split == FALSE)

model = glm(y~., data = train, family = 'binomial')
summary(model)

#Testing the model on the test set 

pred = predict(model, test, type = 'response')
pred

#Confusion matrix 

conf = table(Actual_Value = test$y, Predicted_Value = pred > 0.5)
conf

#Accuracy 

(conf[[1,1]] + conf [[2,2]]) / sum(conf) #essentially 100% accurate!



######Now using all features for logistic regression model######

X = Shroom_dat[,3:24] 
head(X)
dim(X)

sapply(X, class) #all factors
str(X) #reveals 1 factor has 1 level. this is why the one hot encoding is not working. this problem didn't arise in python
#even though veil type is a factor, it only has one unique value which is p (partial) an doesn't have any u (unversal) values
#Need to remove veil type

X = subset(X, select = - veil.type)
dim(X)

y = Shroom_dat[,c("target")]
head(y)
class(y)

# one hot encoding X 

library(caret)

Dum_X = dummyVars(" ~ .", data = X, fullRank = TRUE) #full rank will avoid dummy variable trap, class - 1
DumX_tran = data.frame(predict(Dum_X, newdata = X))

Dum_tran_y2 = cbind(y, DumX_tran) #added y to the X dataframe. This is so ~ can be used when fitting
head(Dum_tran_y2)

head(DumX_tran) #the values match the values from the python analysis :)
dim(Dum_tran_y2)


#Fitting a Logistic regression model with all features
library(caTools)

set.seed(2)
split =sample.split(Dum_tran_y2,SplitRatio = 2/3 ) #2/3 of data goes to training and 1/3 to test set

train=subset(Dum_tran_y2, split = TRUE) # use as training data set, subset only  #returns values that meet criteria of true. 

test=subset(Dum_tran_y2, split == FALSE)

model = glm(y~., data = train, family = 'binomial')
summary(model)

#Testing the model on the test set 

pred = predict(model, test, type = 'response')
pred

#Confusion matrix 

conf = table(Actual_Value = test$y, Predicted_Value = pred > 0.5)
conf

#Accuracy 

(conf[[1,1]] + conf [[2,2]]) / sum(conf) #100% accurate!

#####Building a decision tree model####

install.packages("tree")
library(tree)
library(ISLR)

X = Shroom_dat[,3:24]
head(X)

X = subset(X, select = - veil.type)
dim(X)

y = Shroom_dat[,c("class")]
head(y)
class(y)

# one hot encoding X then we add y to the dataframe since we want it to represent e and p, not 0 and 1

Dum_X = dummyVars(" ~ .", data = X, fullRank = TRUE) #full rank will avoid dummy variable trap, class - 1
DumX_tran = data.frame(predict(Dum_X, newdata = X))

Dum_tran_y3 = cbind(y, DumX_tran) 
head(Dum_tran_y3)

#creating split and training model 

set.seed(3)
split =sample.split(Dum_tran_y3,SplitRatio = 2/3 ) #2/3 of data goes to training and 1/3 to test set

train=subset(Dum_tran_y3, split = TRUE) # use as training data set, subset only  #returns values that meet criteria of true. 

test=subset(Dum_tran_y3, split == FALSE)

#building decision tree model

dt = tree(y~.,train)

#plotting tree model
plot(dt) 
text(dt,pretty=0) #odor considered most important variable to seperate the data

#time to use cross validation to check best model

dt_cv=cv.tree(dt)
plot(dt_cv$size,dt_cv$dev,type="b",xlab="Tree size",ylab="CV error")

#tree size of 8 had lowest cv error

prune_dt =prune.misclass(dt,best=8) #means there will be 8 leaf nodes.

dt_pred=predict(prune_dt,test,type="class") 

conf = table(dt_pred,test$y)
conf

accuracy = (conf[[1,1]] + conf [[2,2]]) / sum(conf)
paste("The overall accuracy of the decison tree model was",accuracy,"%")
#Essentially 100% accurate!

####Creating a random forest model###

#Unlike the decision tree proces, we will use the response as 0 and 1. 

Shroom_dat$target[0:20] #remember created target earlier.
dim(DumX_tran) #DumX_tran is dummy variables of all variables
str(DumX_tran)
#Attach target to X

Rf_dumb = cbind(target, DumX_tran )
str(Rf_dumb[0:10])

#COnverting target to factor. think this required for random forest model.
#Classification does required target to be factor in R 

Rf_dumb$target = as.factor(Rf_dumb$target)
str(Rf_dumb)

#subsetting data

set.seed(4)
split =sample.split(Rf_dumb,SplitRatio = 2/3 ) #2/3 of data goes to training and 1/3 to test set

train=subset(Rf_dumb, split == TRUE) # use as training data set, subset only  #returns values that meet criteria of true. 

test=subset(Rf_dumb, split == FALSE)
dim(train)
dim(test)

#Importing random forest 
install.packages("randomForest")
library(randomForest)

Rf = randomForest(target ~., data = train)
print(Rf) #classifcation default is square root of number of variables. here it is 9 square root of 96 is around 9

#Predicting
#Note: in Python we used 5 trees, here we used default 500 trees to build model

library(caret)
pred = predict(Rf, test)
confusionMatrix(pred,test$target) #100 % accurate like in Python!

plot(Rf) #between 5-10 trees, accuracy doesn't improve. it gets to 100% within 5 trees.

#Will retry but with parameter using 5 trees 

Rf2 = randomForest(target ~., data = train, ntree = 5)
pred2 = predict(Rf2, test)
confusionMatrix(pred2, test$target) #100% accurate with 5 trees

####Buidling neural network####
#tensorflow::install_tensorflow()
install.packages("keras")
install.packages("mlbench")
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

#Note: neuralnet need target to be numerical. Not as a factor 

head(X)
head(y)

Neural = Rf_dumb 
str(Neural)

Neural$target = as.numeric(Neural$target) #converts 0 to 1 and 1 to 2

Neural$target= ifelse(Neural$target == 1, 0,1 )
str(Neural)

#subsetting data

set.seed(5)
split =sample.split(Neural,SplitRatio = 2/3 ) #2/3 of data goes to training and 1/3 to test set

train=subset(Neural, split == TRUE) # use as training data set, subset only  #returns values that meet criteria of true. 

test=subset(Neural, split == FALSE)
dim(train)
dim(test)

#10 neurons in first layer then 20 in second layer
#5 networks will be trained. rep = 5 e.g.

Neu_mod = neuralnet(target ~ ., data = train, hidden = c(10,20), linear.output = FALSE, rep = 5, lifesign = "full", act.fct = "logistic")
plot(Neu_mod, show.weights = F, information = F, fill = "purple",  rep = 'best') 

#####Creating a Keras model#####
dim(train) #96 - 1 variables as input

#%>% means output of left hand side goes into right hand side.

Mod = keras_model_sequential()
Mod %>%
  layer_dense(units = 20, activation = 'relu', input_shape = c(95)) %>%
  layer_dense(units = 1, activation = "sigmoid")

###Compiling the model###

Mod %>% compile(loss = "binary_crossentropy", optimizer = 'adam', metrics = list("accuracy"))

##Fitting the model###

###Note: model fitted without value error once I used metrics = list('accuracy')
##       and made the training (X) data a matrix.

dim(train[,-c(target)])
length(train$target)

train_mat = as.matrix(train[-c(target)])
dim(train_mat)

Model = Mod %>%
      fit(train_mat, train$target, epochs = 50, batch_size = 32, validation_split = 0.2, shuffle = TRUE)

##Evaluating the model on the test set##. Need test X to be matrix too.
test_mat = as.matrix(test[-c(target)])
Mod %>% evaluate(test_mat, test$target)

#Neural network acheived 100% accuracy!

