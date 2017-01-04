#install.packages("ROCR")
library("ROCR")

#setting working directory
getwd()
setwd('C:/Users/mazcona/desktop/')

#dataframe with class and probability
df<-read.csv('bbdd.csv')

#ROCR object for plotting AUC
pred <- prediction(df$predict, df$class)
perf <- performance(pred,"tpr","fpr")

# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)
# calculating AUC
auc <- performance(pred,"auc")
# now converting S4 class to vector
auc <- unlist(slot(auc, "y.values"))
# adding min and max ROC AUC to the center of the plot
minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC)  = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
legend(0.4,0.8,c(minauct,maxauct,"\n"),border="white",cex=1,box.col = "white")

#confusion matrix
df$predict2 <- ifelse(df$predict>0.5,1,0)
confusionMatrix(df$predict2, df$class)

