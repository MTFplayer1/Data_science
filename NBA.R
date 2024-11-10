library(dplyr)
library(glmnet)
library(tree)
library(neuralnet)
nba_train <- read.table("nba_train.txt",sep=";",header=TRUE)
nba_test <- read.table("nba_test.txt",sep=";",header=TRUE)
nba_train
#
#First Ill replace all NAs with 0
#I'll try to estimate the number of 3 Point attempts instead of 3 Point %
#This value can be better predicted then the value of % since its more consistent
#and depends more on the consistent team strategy rather than player performance
#calculating 3point % from 3point made is easy, given that pthrees is the number
#of 3 pointers made by a player
#Position is a very important factor in shooting 3s. Statistically Centers have
#way less 3point attempts than Point Guards. So separating position with binar
#values makes a lot of sense
nba_train[is.na(nba_train)] <- 0
nba_test[is.na(nba_test)] <- 0
pthrees<- numeric(0)
for (i in 1:nrow(nba_train)) {
  ile <- (nba_train[i,"PTS"]-2*nba_train[i,"X2P"]-nba_train[i,"FT"])/3
  pthrees <- c(pthrees, ile)
}
nba_train$X3PA = pthrees
nba_train
for (i in 1:nrow(nba_train)){
  if(nba_train[i,"X3P."]>0){
    nba_train[i,"X3PA"] = nba_train[i,"X3PA"]*100/nba_train[i,"X3P."]
  }
}
#the formula below made me realize that its 2018/2019 regular season
#curry shooting 43% from three
for (i in 1:nrow(nba_train)){
  if(nba_train[i,"X3PA"]>10){
    print(nba_train[i,])
  }
}
#I knew some players are listed as 2 positions
group_positions <- unique(nba_train$Pos)
group_positions
counted_data <- nba_train %>% count(Pos, name = "Count")
counted_data
for (i in 1:nrow(nba_train)){
  if(nba_train[i,"Pos"]=="C-PF"){
    nba_train[i,"Pos"]="C"
  }else if(nba_train[i,"Pos"]=="PF-C"){
    nba_train[i,"Pos"]="PF"
  }else if(nba_train[i,"Pos"]=="PF-SF"){
    nba_train[i,"Pos"]="PF"
  }else if(nba_train[i,"Pos"]=="SF-PF"){
    nba_train[i,"Pos"]="SF"
  }else if(nba_train[i,"Pos"]=="SF-SG"){
    nba_train[i,"Pos"]="SF"
  }else if(nba_train[i,"Pos"]=="SG-PF"){
    nba_train[i,"Pos"]="SG"
  }else if(nba_train[i,"Pos"]=="SG-SF"){
    nba_train[i,"Pos"]="SG"
  }
}
group_positions <- unique(nba_train$Pos)
for (i in 1:nrow(nba_train)){
  nba_train$PG = 0
}
for (i in 1:nrow(nba_train)){
  nba_train$SG = 0
}
for (i in 1:nrow(nba_train)){
  nba_train$SF = 0
}
for (i in 1:nrow(nba_train)){
  nba_train$PF = 0
}
for (i in 1:nrow(nba_train)){
  if (nba_train[i,"Pos"]=="PG"){
    nba_train[i,"PG"] = 1
  }
}
for (i in 1:nrow(nba_train)){
  if (nba_train[i,"Pos"]=="SG"){
    nba_train[i,"SG"] = 1
  }
}
for (i in 1:nrow(nba_train)){
  if (nba_train[i,"Pos"]=="SF"){
    nba_train[i,"SF"] = 1
  }
}
for (i in 1:nrow(nba_train)){
  if (nba_train[i,"Pos"]=="PF"){
    nba_train[i,"PF"] = 1
  }
}
#Variables Ill use in the regression: Age, G, MP, X2PA, FTA, PG, SG, SF, PF, AST, TOV
#I'll try to find the X3PA variable
nba_train$PGAST = nba_train$PG*nba_train$AST
linear_model <- lm(X3PA ~ X2PA+FTA+PG+SG+SF+PF+TOV+MP+PGAST, data = nba_train)
print(coef(linear_model))
summary_result <- summary(linear_model)
print(summary_result$r.squared)
x_train <- as.matrix(nba_train[, c("Age", "G", "MP","X2PA","FTA","PG","SG","SF","PF","AST","TOV")])
y_train <- nba_train$X3PA
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)
print(coef(lasso_model, s = "lambda.min"))
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)
print(coef(ridge_model, s = "lambda.min"))
plot(ridge_model)
tree_model <- tree(X3PA ~ Age + G +X2PA+FTA+PG+SG+SF+PF+AST+TOV+MP, data = nba_train,mincut = 50)
plot(tree_model)
text(tree_model, pretty = 0)
data_pg <- nba_train[nba_train$Pos == "PG", ]
tree_model_pg <- tree(X3PA ~ Age + G +X2PA+FTA+AST+TOV+MP, data = data_pg,mincut = 5)
plot(tree_model_pg)
text(tree_model_pg, pretty = 0)
tab <-rep(0,20)
for (i in seq(1:100)){
  for (j in seq_along(tab)){
    tree_model <- tree(X3PA ~ Age + G +X2PA+FTA+PG+SG+SF+PF+AST+TOV+MP, data = nba_training_train,mincut = j)
    predictions_tree <- predict(tree_model,newdata = as.data.frame(x_test))
    mse_tree <- mean((predictions_tree - y_test)^2)
    tab[j] = tab[j]+mse_tree
  }
}
a <- which.min(tab)


#TESTS
mse_tab <- c(0,0,0,0)
for (z in seq(1:200)){
  nba_train$r <- 0
  for (i in 1:nrow(nba_train)){
    nba_train[i,"r"] = random_number <- sample(1:3, 1)
  }
  nba_training_train <- subset(nba_train, r != 1)
  nba_test_train <- subset(nba_train, r == 1)
  x_train <- as.matrix(nba_training_train[, c( "MP","X2PA","FTA","PG","SG","SF","PF","TOV")])
  y_train <- nba_training_train$X3PA
  x_test <- as.matrix(nba_test_train[, c("MP","X2PA","FTA","PG","SG","SF","PF","TOV")])
  y_test <- nba_test_train$X3PA
  linear_model <- lm(X3PA ~ X2PA+FTA+PG+SG+SF+PF+TOV+MP, data = nba_training_train)
  lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)
  ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)
  tree_model <- tree(X3PA ~ X2PA+FTA+PG+SG+SF+PF+TOV+MP, data = nba_training_train,mincut = a)
  predictions_ridge <- predict(ridge_model,s="lambda.min",newx = x_test)
  mse_ridge <- mean((predictions_ridge - y_test)^2)
  predictions_lasso <- predict(lasso_model,s="lambda.min",newx = x_test)
  mse_lasso <- mean((predictions_lasso - y_test)^2)
  predictions_tree <- predict(tree_model,newdata = as.data.frame(x_test))
  mse_tree <- mean((predictions_tree - y_test)^2)
  predictions_linear <- predict(linear_model,newdata = as.data.frame(x_test))
  mse_linear <- mean((predictions_linear - y_test)^2)
  mse_tab[1] = mse_tab[1]+mse_ridge
  mse_tab[2] = mse_tab[2]+mse_lasso
  mse_tab[3] = mse_tab[3]+mse_tree
  mse_tab[4] = mse_tab[4]+mse_linear
}
print(which.min(mse_tab))
mse_tab
#Im gonna be using lasso


nba_test$Predictions <- predict(lasso_model,s="lambda.min",newx = x_test)

group_positions <- unique(nba_test$Pos)
group_positions
for (i in 1:nrow(nba_test)){
  if(nba_test[i,"Pos"]=="SF-SG"){
    nba_test[i,"Pos"]="SF"
  }
}
for (i in 1:nrow(nba_test)){
  nba_test$PG = 0
}
for (i in 1:nrow(nba_train)){
  nba_test$SG = 0
}
for (i in 1:nrow(nba_train)){
  nba_test$SF = 0
}
for (i in 1:nrow(nba_train)){
  nba_test$PF = 0
}
for (i in 1:nrow(nba_test)){
  if (nba_test[i,"Pos"]=="PG"){
    nba_test[i,"PG"] = 1
  }
}
for (i in 1:nrow(nba_test)){
  if (nba_test[i,"Pos"]=="SG"){
    nba_test[i,"SG"] = 1
  }
}
for (i in 1:nrow(nba_test)){
  if (nba_test[i,"Pos"]=="SF"){
    nba_test[i,"SF"] = 1
  }
}
for (i in 1:nrow(nba_test)){
  if (nba_test[i,"Pos"]=="PF"){
    nba_test[i,"PF"] = 1
  }
}
x_train <- as.matrix(nba_train[, c( "MP","X2PA","FTA","PG","SG","SF","PF","TOV")])
y_train <- nba_train$X3PA
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)
x_test <- as.matrix(nba_test[, c("MP","X2PA","FTA","PG","SG","SF","PF","TOV")])
nba_test$Predictions <- predict(lasso_model,s="lambda.min",newx = x_test)
for(i in 1:nrow(nba_test)){
  if(nba_test[i,"Predictions"]<0){
    nba_test[i,"Predictions"]=0
  }
}
nba_test
nba_test$Predictions3<- 0
for (i in 1:nrow(nba_test)){
  if(nba_test[i,"Predictions"]!=0){
    ile <- round((nba_test[i,"PTS"]-2*nba_test[i,"X2P"]-nba_test[i,"FT"])/3/nba_test[i,"Predictions"],digits=5)
    nba_test[i,"Predictions3"] = ile
  }else{
    nba_test[i,"Predictions3"]=0
  }
}
for (i in 1:nrow(nba_test)){
  if (nba_test[i,"Predictions3"]<0){
    nba_test[i,"Predictions3"]=0
  }else if(nba_test[i,"Predictions3"]>0){
    nba_test[i,"Predictions3"]<-nba_test[i,"Predictions3"]*100
  }
}
print(nba_test["Predictions3"])