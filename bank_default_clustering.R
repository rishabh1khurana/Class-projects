library(plot)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(e1071)
#install.packages("RSNNS")
library(klaR)
library(RSNNS)
library(kernlab)
library(caret)
library(ggplot2)
library(dplyr)
library(arules)
library(arulesViz)
library(apriori)

#1 Read data in and transform to usable format for basic classifiers and clustering

data1<-read.csv("C:/Users/Rishabh Khurana/Documents/MSU/Fall semester/CSE891/UCI_Credit_Card.csv")
data5=read.csv("C:/Users/Rishabh Khurana/Documents/MSU/Fall semester/CSE891/oversampled_data.csv")
attach(data1)

library("corrplot")
data4=data5[,c("LIMIT_BAL","avgUsage","avgBillAmt","percentLate","avgPayAmt","Total_Due")]

data1$EDUCATION=factor(data1$EDUCATION)

colnames(data5)


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

M <- cor(data4)

corrplot(M, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
                  # hide correlation coefficient on the principal diagonal
         diag = FALSE)


data1$Age

data1$AGE_cat=cut(data1$AGE, c(18,33,46,70), right=FALSE, labels=c(1:3))
data1$SEX=factor(data1$SEX)
data1$MARRIAGE=data1$MARRIAGE

data2=data1[,c("LIMIT_BAL","avgUsage","avgBillAmt","oneMonthLates","twoMonthLates","threePlusMonthLates",
              "percentLate","avgPayAmt")]

p = list()
for (i in 1:ncol(data2)) p[[i]] <- qplot(data2[,i],xlim=c(0, 75000), xlab=names(data2)[[i]])
do.call(grid.arrange, p)

p = list()
p=

  
  

par(mfrow = c(3,1)) 

plots <- list()
qplot(data2$LIMIT_BAL, xlab=names(data2)[[1]])
qplot(data2$avgBillAmt, xlab=names(data2)[[3]])
qplot(data2$avgPayAmt, xlab=names(data2)[[8]],xlim=c(0, 80000))
multiplot(plotlist = plots, cols = 3)


data3=data.matrix(data2)
#mba=as(data3,"itemMatrix")
data3=as(data2, "transactions")

data3
rules<-apriori(data2, parameter=list(supp=0.05, conf=0.9))





# calculate the correlation matrix
corr = data.corr()

# plot the heatmap
sns.heatmap(corr, 
            xticklabels=corr.columns,
            yticklabels=corr.columns)

str(data)

data1$ID<-as.factor(data1$ID) #change ID to factor
data1$Male<-ifelse(data1$SEX==1,1,0)
data1$Married<-ifelse(data1$MARRIAGE==1,1,0)
data1$GradSchool<-ifelse(data1$EDUCATION==1,1,0)
data1$College<-ifelse(data1$EDUCATION==2,1,0)
data1$HS<-ifelse(data1$EDUCATION==3,1,0)

#bill amount avg and sd
data1$avgBillAmt<-rowMeans(data1[,13:18])
data$sdBillAmt<-apply(data[,13:18],1,sd)

#cap avg bill amount at 0
data1$avgBillAmt[data1$avgBillAmt<=0]<-0

#avg usage
data1$avgUsage<-data1$avgBillAmt/data1$LIMIT_BAL



colnames(data1[,19:24])
#repayment Vars
data1$oneMonthLates<-apply(data1[,7:12],1,function(x) sum(x==1))
data1$twoMonthLates<-apply(data1[,7:12],1,function(x) sum(x==2))
data1$threePlusMonthLates<-apply(data1[,7:12],1,function(x) sum(x>=3))
data1$percentLate<-apply(data1[,7:12],1,function(x) sum(x>=0)/6)

#paymentVars
data1$avgPayAmt<-rowMeans(data1[,19:24])
data1$sdPayAmt<-apply(data1[,19:24],1,sd)

validVars<-c("ID","LIMIT_BAL","AGE",colnames(data)[25:39])






####CLASSIFICATION, TUNE PARAMETERS FOR KNN, NB, MNN, Boosting
classData<-data[,validVars]
set.seed(7)
control<-trainControl(method = "repeatedcv",5,repeats = 2) #5 fold cv with 2 repeats

#knn
knnData<-classData%>%select(-c(ID))
knnData$default.payment.next.month<-as.factor(knnData$default.payment.next.month)
knnGrid<-expand.grid(k=floor(seq(4,floor(sqrt(nrow(knnData))),floor(sqrt(nrow(knnData)))/5)))
knnMod<-train(default.payment.next.month~.,knnData,method="knn",trControl=control,tuneGrid=knnGrid)
#107 is best value for k among those choices so thats what we'll use
#achieved 77.94% accuracy and we havent even scaled the variables

#lets try using PCA as predictors
pr1<-prcomp(knnData%>%select(-c(default.payment.next.month)),scale. = T) 
summary(pr1) #we can take the first 10 for 90% explenation of variance
predPCs<-pr1$x[,1:10]
pcaKnnFrame<-data.frame(predPCs)%>%mutate("default"=knnData$default.payment.next.month)
knnPCAMod<-train(default~.,pcaKnnFrame,method="knn",trControl=control,tuneGrid=knnGrid)
knnPCAMod #73 was best with accuracy of 80.45%


###NB

#for naive bayes we can keep categorical predictors as factors
badNbCol<-c("ID",colnames(data)[c(grep("PAY",colnames(data)),grep("BILL",colnames(data)))],"Male","Married","GradSchool","College","HS")
nbData<-data[,colnames(data)%in% badNbCol==F]
nbData[,c(2:4,6)]<-data.frame(apply(nbData[,c(2:4,6)],2,as.factor),stringsAsFactors = T)
nbData$EDUCATION[nbData$EDUCATION=="0"]<-6  #education 0 is actually unknown

nbMod<-train(default.payment.next.month~.,data=nbData,method="nb",trControl=control) #kernel density is used, accuary is 78.4%

#We got a lot of warnings, lets try discretizing continous vars
nbDataDis<-nbData

#we will bucket these by intuition
nbDataDis$LIMIT_BAL<-as.factor(ifelse(nbDataDis$LIMIT_BAL<50000,"very_low",ifelse(nbDataDis$LIMIT_BAL<150000,"below_avg",
                                                                                  ifelse(nbDataDis$LIMIT_BAL<250000,"above_avg","very_high"))))
nbDataDis$AGE<-as.factor(ifelse(nbData$AGE<30,"young",ifelse(nbData$AGE<40,"middleAge",ifelse(nbData$AGE<65,"preRetire","retired"))))


nbDataDis$avgUsage<-as.factor(ifelse(nbData$avgUsage==0,"none",ifelse(nbData$avgUsage<.10,"low",ifelse(nbData$avgUsage<.30,"average",
                                                                                                       ifelse(nbData$avgUsage<.70,"high",

                                                                                                              
                                                                                                                                                                                                                            ifelse(nbData$avgUsage<1,"very_high","over"))))))
#put these in roughly equal sized buckets

nbDataDis$percentLate<-as.factor(ifelse(nbData$percentLate==0,"none",ifelse(nbData$percentLate<.5,"some",
                                                                            ifelse(nbData$percentLate<1,"most","all"))))

nbDataDis$avgBillAmt<-as.factor(ifelse(nbDataDis$avgBillAmt<5000,"low",ifelse(nbDataDis$avgBillAmt<22000,"belowAv",
                                                                              ifelse(nbDataDis$avgBillAmt<60000,"aboveAv","High"))))


nbDataDis$sdBillAmt<-as.factor(ifelse(nbDataDis$sdBillAmt<1500,"low",ifelse(nbDataDis$sdBillAmt<5000,"belowAv",
                                                                            ifelse(nbDataDis$sdBillAmt<15000,"aboveAv","High"))))

nbDataDis$avgPayAmt<-as.factor(ifelse(nbDataDis$avgPayAmt<1200,"low",ifelse(nbDataDis$avgPayAmt<2500,"belowAv",
                                                                            ifelse(nbDataDis$avgPayAmt<6000,"aboveAv","High"))))


nbDataDis$sdPayAmt<-as.factor(ifelse(nbDataDis$sdPayAmt<650,"low",ifelse(nbDataDis$sdPayAmt<1500,"belowAv",
                                                                         ifelse(nbDataDis$sdPayAmt<4000,"aboveAv","High"))))

nbDataDis$oneMonthLates<-as.factor(ifelse(nbData$oneMonthLates==0,"none",ifelse(nbData$oneMonthLates<=3,"some",
                                                                                ifelse(nbData$oneMonthLates<6,"most","all"))))

nbDataDis$twoMonthLates<-as.factor(ifelse(nbData$twoMonthLates==0,"none",ifelse(nbData$twoMonthLates<=3,"some",
                                                                                ifelse(nbData$twoMonthLates<6,"most","all"))))
nbDataDis$threePlusMonthLates<-as.factor(ifelse(nbData$threePlusMonthLates==0,"none",ifelse(nbData$threePlusMonthLates<=3,"some",
                                                                                            ifelse(nbData$threePlusMonthLates<6,"most","all"))))

nbMod1<-train(default.payment.next.month~.,data=nbDataDis,method="nb",trControl=control) #kernel density is used, accuary is 77.88%

##Naive bayes isnt working great because there are records that are given a probability of 0 for both classes do to the are outside the range of the data

#Also condition of independence should be checked
write.csv(file = "varCors.csv",cor(classData[,sapply(classData,function(x) is.numeric(x))]))

#Last try lets drop some highly corr vars
nbDataLT<-nbDataDis%>%select(-c(avgBillAmt,percentLate,avgPayAmt))

nbMod2<-train(default.payment.next.month~.,data=nbDataLT,method="nb",trControl=control) #kernel density is used, accuary is 77.88%





###NN

#######NEEDS WORK

#it is advised to scale data before fitting nn
nnData<-classData%>%select(-c(ID))
nnMin<-apply(nnData,2,min)
nnMax<-apply(nnData,2,max)
nnData<-data.frame(scale(nnData,center = nnMin,scale=nnMax-nnMin))

nnGrid<-expand.grid(size=c(3,5,7,9,11))
mlpMod2<-train(x=nnData%>%select(-c(default.payment.next.month)),y=as.factor(nnData$default.payment.next.month),method = "mlp",
               trControl = control,tuneGrid = nnGrid) 



###SVM
svmData<-classData%>%select(-c(ID))
svmData$default.payment.next.month<-as.factor(svmData$default.payment.next.month)
lsvmMod<-train(default.payment.next.month~.,data=svmData,method="svmLinear2",trControl=control)

bla<-lssvm(default.payment.next.month~.,data=nnData)

#PCA PLOT
summary(pr1)     #2 dimensions explains 97% of variance, lets plot!!!


prFrame<-data.frame("x"=pr1$x[,1],"y"=pr1$x[,2],"class"=as.factor(data$default.payment.next.month))

ggplot(data=prFrame,aes(x=x,y=y,color=class))+geom_point()  #plotting first two rotations, colored by class




####CLUSTER, K MEANS, HEIRACHAL, Density based

clusterData<-data[,validVars]%>%select(-c(default.payment.next.month,ID))

write.csv(file="clusterAll1.csv",data)
write.csv(file="clusterNewOnly.csv",data[,validVars])


