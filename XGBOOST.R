library(xgboost)


total <- read.table(file="base.csv", sep=";", dec=",", header=TRUE)

#variables in data frame
feature.names <- names(total)[3:99]


#transform character to integer
for (f in feature.names) {
  if (class(total[[f]])=="character") {
    levels <- unique(c(total[[f]]))
    total[[f]] <- as.integer(factor(total[[f]], levels=levels))
  }
}


#sampling
smp_size <- floor(0.75 * nrow(total))
set.seed(123)
train_ind <- sample(seq_len(nrow(total)), size = smp_size)
train <- total[train_ind, ]
test <- total[-train_ind, ]

train[] <- lapply(train, as.numeric)
test[] <- lapply(test, as.numeric)


#model
clf <- xgboost(data        = data.matrix(train[,feature.names]),
               label       = train$target,
               nrounds     = 20,
               missing     = NA,
               objective   = "binary:logistic",
               eval_metric = "auc")



#scoring
test$predict <- predict(clf, data.matrix(test[,feature.names]))


#save results for ROC analysis
a<-c('target', 'predict')
total2 <- (test)[c(a)]
write.csv(total2, "score.csv", row.names = FALSE)
rm(total)