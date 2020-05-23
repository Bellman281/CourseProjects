## 7 Stations
## (1) Alashtar

library(randomForest)
library(ROCR)

setwd("/Documents")

Zone_name = "Alashtar"

Filename =paste0(Zone_name,".csv")
dataf <- read.csv ( Filename , header = TRUE, sep = ",")

str(dataf)
summary(dataf)

set.seed(4543)


train_index <- sample(1:nrow(dataf), 0.8 * nrow(dataf))
test_index <- setdiff(1:nrow(dataf), train_index)

X_train <- dataf[train_index, c("elevation","NDVI","Tday","Tnight","PET") ]
y_train <- dataf[train_index, "prec"]

str(X_train)

X_test <- dataf[test_index, c("elevation","NDVI","Tday","Tnight","PET")]
y_test <- dataf[test_index, "prec"]
str(X_test)

dataf.rf <- randomForest ( y_train ~  elevation +NDVI + Tday  + Tnight + PET ,  data = X_train, xtest   = X_test,
                           ytest   = y_test,ntree=1000, importance=TRUE)

#dataf.rf.pr = predict(dataf.rf,newdata=X_test)

dataf.rf$mse  


# number of trees with lowest MSE
which.min(dataf.rf$mse)
## [1] 494

# RMSE of this optimal random forest
sqrt(dataf.rf$mse[which.min(dataf.rf$mse)])
## [1] 28.44874


oob <- sqrt(dataf.rf$mse)
validation <- sqrt(dataf.rf$test$mse)

# compare error rates
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:dataf.rf$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous() +
  xlab("Number of trees")



rf.tuned<- randomForest::tuneRF(
  x          = X_train,
  y          = y_train,
  ntreeTry   = 10000,
  mtryStart  = 2,
  stepFactor = 0.5,
  improve    = 0.0001,
  trace      = FALSE      # to not show real-time progress 
)


dataf.rf <- randomForest ( y_train ~  elevation +NDVI + Tday  + Tnight + PET ,  data = X_train, xtest   = X_test
                        ,mtry=2,keep.forest=TRUE,   ytest   = y_test,ntree=10000, importance=TRUE)

x1<-data.frame(cbind(X_test,y_test))
pred_randomForest <- predict(dataf.rf,newdata=x1 )
head(pred_randomForest)


varImpPlot(dataf.rf,main =paste0("importance plot ", Zone_name))

varImpPlot(dataf.rf, type = 2)

#cbind(pred_randomForest,y_test)

err3 <- mean((pred_randomForest-y_test)^2)
print(paste("test-error=", err3))




write.csv(pred_randomForest,paste0("pred_",Filename))
