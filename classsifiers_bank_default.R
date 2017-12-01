library(e1071)
library(klaR)
library(RSNNS)
library(kernlab)
library(caret)
library(ggplot2)
library(dplyr)
library(class)
#install.packages('fastAdaboost')
library(fastAdaboost)
#1 Read data in and transform to usable format for basic classifiers and clustering

data<-read.csv("C:/Users/Rishabh Khurana/Documents/MSU/Fall semester/CSE891/oversampled_data_final.csv")

str(data)

data$ID<-as.factor(data$ID) #change ID to factor
data$Male<-ifelse(data$SEX==1,1,0)
data$Married<-ifelse(data$MARRIAGE==1,1,0)
data$GradSchool<-ifelse(data$EDUCATION==1,1,0)
data$College<-ifelse(data$EDUCATION==2,1,0)
data$HS<-ifelse(data$EDUCATION==3,1,0)


#bill amount avg and sd
data$avgBillAmt<-rowMeans(data[,13:18])
data$sdBillAmt<-apply(data[,13:18],1,sd)

#cap avg bill amount at 0
data$avgBillAmt[data$avgBillAmt<=0]<-0

#avg usage
data$avgUsage<-data$avgBillAmt/data$LIMIT_BAL



#repayment Vars
data$oneMonthLates<-apply(data[,7:12],1,function(x) sum(x==1))
data$twoMonthLates<-apply(data[,7:12],1,function(x) sum(x==2))
data$threePlusMonthLates<-apply(data[,7:12],1,function(x) sum(x>=3))
data$percentLate<-apply(data[,7:12],1,function(x) sum(x>=0)/6)

#paymentVars
data$avgPayAmt<-rowMeans(data[,19:24])
data$sdPayAmt<-apply(data[,19:24],1,sd)



validVars<-c("ID","LIMIT_BAL","AGE",colnames(data)[25:39])




####CLASSIFICATION, TUNE PARAMETERS FOR KNN, NB, MNN, Boosting
classData<-data[,validVars]
set.seed(7)

#lets make a custom summary function!
unbalanced<-function(data, lev = NULL, model = NULL){
  conf<-table(data$pred,data$obs)
  acc<-sum(diag(conf))/sum(conf)
  rec<-recall(conf,"yes")
  pres<-precision(conf,"yes")
  out<-c(acc,rec,pres)
  names(out)<-c("ACC","Recall","Pres")
  return(out)
}

control<-trainControl(method = "repeatedcv",5,repeats = 2,classProbs = T,summaryFunction = unbalanced) #5 fold cv with 2 repeats
#knn
knnData<-classData%>%select(-c(ID))
knnData$default.payment.next.month<-as.factor(knnData$default.payment.next.month)
levels(knnData$default.payment.next.month)<-c("no","yes")
knnGrid<-expand.grid(k=floor(seq(4,floor(sqrt(nrow(knnData))),floor(sqrt(nrow(knnData)))/5)))
knnMod<-train(default.payment.next.month~.,knnData,method="knn",trControl=control,tuneGrid=knnGrid,metric="Recall")
knnMod
#lets try using PCA as predictors
pr1<-prcomp(knnData%>%select(-c(default.payment.next.month)),scale. = T)
screeplot(pr1,type = "l") #elbow at about 5 so we only need the first 5 princomps, but the low var components could be crucial. Lets try 10 also.
predPCs<-pr1$x[,1:5]
pcaKnnFrame<-data.frame(predPCs)%>%mutate("default"=knnData$default.payment.next.month)
knnPCAMod<-train(default~.,pcaKnnFrame,method="knn",trControl=control,tuneGrid=knnGrid,metric="Recall")
knnPCAMod

#With 10 princomps
predPCs1<-pr1$x[,1:10]
pca1KnnFrame<-data.frame(predPCs1)%>%mutate("default"=knnData$default.payment.next.month)
knnPCAMod1<-train(default~.,pcaKnnFrame,method="knn",trControl=control,tuneGrid=knnGrid,metric="Recall")
knnPCAMod1

###NB

#for naive bayes we can keep categorical predictors as factors
badNbCol<-c("ID",colnames(data)[c(grep("PAY",colnames(data)),grep("BILL",colnames(data)))],"Male","Married","GradSchool","College","HS")
nbData<-data[,colnames(data)%in% badNbCol==F]
nbData[,c(2:4,6)]<-data.frame(apply(nbData[,c(2:4,6)],2,as.factor),stringsAsFactors = T)
nbData$EDUCATION[nbData$EDUCATION=="0"]<-6  #education 0 is actually unknown
levels(nbData$default.payment.next.month)<-c("no","yes")
nbMod<-train(default.payment.next.month~.,data=nbData,method="nb",trControl=control,metric="Recall")
nbMod

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


nbMod1<-train(default.payment.next.month~.,data=nbDataDis,method="nb",trControl=control,metric="Recall") #kernel density is used, accuary is 77.88%
nbMod1   #Not Good!
##Naive bayes isnt working great because there are records that are given a probability of 0 for both classes do to the are outside the range of the data

#Also condition of independence should be checked
write.csv(file = "varCors.csv",cor(classData[,sapply(classData,function(x) is.numeric(x))]))

#Last try lets drop some highly corr vars
nbDataLT<-nbData%>%select(-c(avgBillAmt,percentLate,avgPayAmt))

nbMod2<-train(default.payment.next.month~.,data=nbDataLT,method="nb",trControl=control) #kernel density is used, accuary is 77.88%
nbMod2




###NN

#it is advised to scale data before fitting nn
nnData<-classData%>%select(-c(ID))
nnMin<-apply(nnData,2,min)
nnMax<-apply(nnData,2,max)
nnData<-data.frame(scale(as.matrix(nnData),center = nnMin,scale=nnMax-nnMin))
nnData$default.payment.next.month<-as.factor(classData$default.payment.next.month)
levels(nnData$default.payment.next.month)<-c("no","yes")
nnGrid<-expand.grid(size=c(3,5,7,9,11))
mlpMod<-train(x=nnData%>%select(-c(default.payment.next.month)),y=nnData$default.payment.next.month,method = "mlp",
               trControl = control,tuneGrid = nnGrid,metric = "Recall") 
mlpMod
#what if we try using princomps
#5
mlpMod1<-train(x=predPCs,y=nnData$default.payment.next.month,method = "mlp",
               trControl = control,tuneGrid = nnGrid,metric = "Recall") 
mlpMod1
#10
mlpMod2<-train(x=predPCs1,y=nnData$default.payment.next.month,method = "mlp",
               trControl = control,tuneGrid = nnGrid,metric = "Recall") 
mlpMod2

###SVM
  svmData<-classData%>%select(-c(ID))
svmData$default.payment.next.month<-as.factor(svmData$default.payment.next.month)
levels(svmData$default.payment.next.month)<-c("no","yes")
lsvmMod<-train(default.payment.next.month~.,data=svmData,method="svmLinear2",trControl=control,metric="Recall")
lsvmMod      ####IF WE CANT RERUN COST .5 ACC=.804367,Recall=.6556,Pres=.2438

###radial kernal
rsvm<-train(default.payment.next.month~.,data=svmData,method="lssvmRadial",trControl = control,metric = "Recall")
rsvm

###try with 10 princomps
svmPCAData<-data.frame(predPCs1)%>%mutate("default"=svmData$default.payment.next.month)
lsvmPCAMod<-train(default.payment.next.month~.,data=svmData,method="lssvmRadial",trControl=control,metric="Recall")
lsvmPCAMod

###Adaboost
adaData<-svmData
adaGrid<-expand.grid(nIter=c(50,100,150),method=c("ada"))
adaMod<-train(default.payment.next.month~.,data=svmData,method="adaboost",trControl=control,tuneGrid=adaGrid,metric="Recall")
adaMod
