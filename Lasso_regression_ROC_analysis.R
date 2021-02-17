#####################################
# Lasso regression and ROC analysis #
#####################################

#Select "working directory (path to dataset.csv)" 

data<-read.csv("dataset.csv",sep=";",dec=",")

install.packages("glmnet")
install.packages("ROCR")

library(glmnet)
library(ROCR)
data.DF<-data.frame(data[,2:12])
str(data.DF)

group1=which(data.DF$covid.19=='severe COVID-19')
group2=which(data.DF$covid.19=='mild COVID-19')
group3=which(data.DF$covid.19=='mild non COVID-19')

data.DF[data.DF$covid.19=='severe COVID-19'|data.DF$covid.19=='mild COVID-19',"covid.19"] <- "1"
data.DF[data.DF$covid.19=='mild non COVID-19',"covid.19"] <- "0"

data.DF$covid.19<-as.factor(data.DF$covid.19)

model1=data.DF
str(model1)

model1<-model1[-4]

#LASSO REGRESSION
library(foreign) #for importing SPSS data
library(glmnet) #for fitting regularized glm's, cite: Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models via Coordinate Descent. Journal of Statistical Software, 33(1), 1-22. 
library(ROCR) #for plotting ROC curves, cite: Sing et al. (2005). ROCR; visualizing classifier performance in R. Bioinformatics, 21(20), 3940-3941.

###################################################################
# discriminate between COVID-19 and non-COVID-19                  #
###################################################################

data.DF<-data.frame(data[,2:12])
str(data.DF)

group1=which(data.DF$covid.19=='severe COVID-19')
group2=which(data.DF$covid.19=='mild COVID-19')
group3=which(data.DF$covid.19=='mild non COVID-19')


data.DF[data.DF$covid.19=='severe COVID-19'|data.DF$covid.19=='mild COVID-19',"covid.19"] <- "1"
data.DF[data.DF$covid.19=='mild non COVID-19',"covid.19"] <- "0"

data.DF=data.DF[which(data.DF$covid.19==1|data.DF$covid.19==0),]
data.DF$covid.19<-as.factor(data.DF$covid.19)
model2=data.DF

model2<-model2[-4]
str(model2)

#make the model matrix
voc.matrix<-model.matrix(covid.19~.-1,data=model2)

#LOO crossvalidation
ssize=dim(model2)[1]
risk.hat<-array(NA,ssize)
class<-as.factor(model2$covid.19)
set.seed(25100505)
subset<-sample(c(1:ssize),ssize)
coef<-matrix(NA,(dim(model2)[2]),ssize)
for(i in 1:ssize){
  
  #create fold
  subset.i<-subset[i]
  train<-voc.matrix[-subset.i,]
  test<-rbind(voc.matrix[subset.i,],voc.matrix[subset.i,])
  class.train<-class[-subset.i]
  
  #select lambda via crossvalidation
  glm.lasso<-cv.glmnet(x=data.matrix(train),y=class.train,family="binomial",standardize=T,type.measure="class")
  lambda<-(glm.lasso)$lambda.min
  
  #create some figures if wanted
  if(i==1){
    par(mfrow=c(1,2))
    #determining the optimal lambda
    plot(glm.lasso)
    #coefficients for varying lambda, optimal lambda indicated by vertical black line
    glm.lasso.temp<-glmnet(x=data.matrix(train),y=class.train,family="binomial",standardize=T)
    plot(glm.lasso.temp,xvar="lambda",label=T,xlab="log(Lambda)")
    abline(v=log(lambda),lty=3)
  }
  
  #fit model with optimal lambda
  glm.lasso<-glmnet(x=data.matrix(train),y=class.train,family="binomial",lambda=lambda,standardize=T)
  
  #extract beta's
  coef[,i]<-as.matrix(coef(glm.lasso))
  
  #predict
  risk.hat[i]<-predict(glm.lasso,newx=data.matrix(test),type="response")[1,]
}


#AUC
fitpreds<-as.matrix(risk.hat)
fitpred = prediction(fitpreds,as.factor(class)[subset])
auc.tmp <- performance(fitpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc

#ROC
fitperf = performance(fitpred,"tpr","fpr")
plot(fitperf,lty=1,col="orange",lwd=4,main="ROC Curve",xlab="False Positive Rate (1-Specificity)",ylab="True Positive Rate (Sensitivity)")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#sensitivity/specificity
risk.hat2<-ifelse(risk.hat>=0.608,1,0)
ctab<-table(risk.hat2,as.factor(class)[subset])
ctab
spec<-ctab[1,1]/sum(ctab[,1]) # spec
spec
sens<-ctab[2,2]/sum(ctab[,2]) # sens
sens
npv<-ctab[1,1]/sum(ctab[1,]) # npv
npv
ppv<-ctab[2,2]/sum(ctab[2,]) #ppv
ppv
acc<-sum(diag(prop.table(ctab))) #acc
acc

#add point and/or text to curve
points((1-ctab[1,1]/sum(ctab[,1])),ctab[2,2]/sum(ctab[,2]),pch=16,cex=1.5,col="skyblue4")
#text(1-spec-0.25,sens+0.24,cex=0.8,adj=0,labels=paste("Sensitivity: 0.70 (0.49-0.86)\nSpecificity: 0.77 (0.57-0.91)\nAUC: 0.71 (0.54-0.86)"))
fitperf2 <- performance(fitpred, "acc")
plot(fitperf2, avg= "vertical", spread.estimate="boxplot", lwd=3,col='blue', show.spread.at= seq(0.1, 0.9, by=0.1), main= "Accuracy across the range of possible cutoffs",ylim=c(0,1))
fitperf2@y.values #Zoek maximale waarde in de tabel
fitperf2@x.values 
abline(a=0.7162,b=0,lwd=2,lty=2,col="gray")

library(PropCIs)
cisens<-midPci(ctab[2,2],sum(ctab[,2]),conf.level=0.95) #95%CI sens
cisens$conf.int[1:2]
cispec<-midPci(ctab[1,1],sum(ctab[,1]),conf.level=0.95) #95%CI spec
cispec$conf.int[1:2]
cippv<-midPci(ctab[2,2],sum(ctab[2,]),conf.level=0.95) #95%CI PPV
cippv$conf.int[1:2]
cinpv<-midPci(ctab[1,1],sum(ctab[1,]),conf.level=0.95) #95%CI NPV
cinpv$conf.int[1:2]
ciacc<-midPci((ctab[1,1]+ctab[2,2]),sum(ctab),conf.level=0.95) #95%CI Acc
ciacc$conf.int[1:2]

##using pROC
library(pROC)
risk.hat<-as.numeric(risk.hat)
ciauc<-ci.auc(roc(as.factor(class)[subset],risk.hat), conf.level=0.95,method="bootstrap",boot.n=10000)

#Maken van samenvattende tabel
sumtab=matrix(NA,6,2)
sumtab[1,1]=round(sens,3)
sumtab[2,1]=round(spec,3)
sumtab[3,1]=round(ppv,3)
sumtab[4,1]=round(npv,3)
sumtab[5,1]=round(acc,3)
sumtab[6,1]=round(auc,3)
sumtab[1,2]=paste("(",round(cisens$conf.int[1],3),"-", round(cisens$conf.int[2],3),")")
sumtab[2,2]=paste("(",round(cispec$conf.int[1],3),"-", round(cispec$conf.int[2],3),")")
sumtab[3,2]=paste("(",round(cippv$conf.int[1],3),"-", round(cippv$conf.int[2],3),")")
sumtab[4,2]=paste("(",round(cinpv$conf.int[1],3),"-", round(cinpv$conf.int[2],3),")")
sumtab[5,2]=paste("(",round(ciacc$conf.int[1],3),"-", round(ciacc$conf.int[2],3),")")
sumtab[6,2]=paste("(",round(ciauc[1],3),"-", round(ciauc[3],3),")")
rownames(sumtab)<-c('Sensitivity','Specificity','Positive Predictive Value','Negative Predictive Value','Accuracy','AUCROC')
colnames(sumtab)<-c('Value','(95% Confidence Interval)')
model2sumtab<-noquote(sumtab)
model2sumtab

#which variables are most often selected by the LASSO?
nonzero<-array(NA,10)
for(i in 1:10){
  nonzero[i]<-sum((coef[i,]!=0))
}
imp.variables<-data.frame(cbind(rownames(coef(glm.lasso))[nonzero>0],nonzero[nonzero>0]))
colnames(imp.variables)<-c("variable","frequency")
imp.variables$frequency<-as.numeric(as.character(imp.variables$frequency))
imp.variables<-imp.variables[order(-imp.variables$frequency),]
imp.variables

###################################################################
# discriminate between mild and severe COVID-19                   #
###################################################################

data.DF<-data.frame(data[,2:12])
str(data.DF)

group1=which(data.DF$covid.19=='severe COVID-19')
group2=which(data.DF$covid.19=='mild COVID-19')
group3=which(data.DF$covid.19=='mild non COVID-19')


data.DF[data.DF$covid.19=='severe COVID-19',"covid.19"] <- "1"
data.DF[data.DF$covid.19=='mild COVID-19',"covid.19"] <- "0"

data.DF=data.DF[which(data.DF$covid.19==1|data.DF$covid.19==0),]
data.DF$covid.19<-as.factor(data.DF$covid.19)
model2=data.DF

model2<-model2[-4]
str(model2)

#make the model matrix
voc.matrix<-model.matrix(covid.19~.-1,data=model2)

#LOO crossvalidation
ssize=dim(model2)[1]
risk.hat<-array(NA,ssize)
class<-as.factor(model2$covid.19)
set.seed(25100505)
subset<-sample(c(1:ssize),ssize)
coef<-matrix(NA,(dim(model2)[2]),ssize)
for(i in 1:ssize){
  
  #create fold
  subset.i<-subset[i]
  train<-voc.matrix[-subset.i,]
  test<-rbind(voc.matrix[subset.i,],voc.matrix[subset.i,])
  class.train<-class[-subset.i]
  
  #select lambda via crossvalidation
  glm.lasso<-cv.glmnet(x=data.matrix(train),y=class.train,family="binomial",standardize=T,type.measure="class")
  lambda<-(glm.lasso)$lambda.min
  
  #create some figures if wanted
  if(i==1){
    par(mfrow=c(1,2))
    #determining the optimal lambda
    plot(glm.lasso)
    #coefficients for varying lambda, optimal lambda indicated by vertical black line
    glm.lasso.temp<-glmnet(x=data.matrix(train),y=class.train,family="binomial",standardize=T)
    plot(glm.lasso.temp,xvar="lambda",label=T,xlab="log(Lambda)")
    abline(v=log(lambda),lty=3)
  }
  
  #fit model with optimal lambda
  glm.lasso<-glmnet(x=data.matrix(train),y=class.train,family="binomial",lambda=lambda,standardize=T)
  
  #extract beta's
  coef[,i]<-as.matrix(coef(glm.lasso))
  
  #predict
  risk.hat[i]<-predict(glm.lasso,newx=data.matrix(test),type="response")[1,]
}


#AUC
fitpreds<-as.matrix(risk.hat)
fitpred = prediction(fitpreds,as.factor(class)[subset])
auc.tmp <- performance(fitpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc

#ROC
fitperf = performance(fitpred,"tpr","fpr")
plot(fitperf,lty=1,col="orange",lwd=4,main="ROC Curve",xlab="False Positive Rate (1-Specificity)",ylab="True Positive Rate (Sensitivity)")
abline(a=0,b=1,lwd=2,lty=2,col="gray")

#sensitivity/specificity
risk.hat2<-ifelse(risk.hat>=0.608,1,0)
ctab<-table(risk.hat2,as.factor(class)[subset])
ctab
spec<-ctab[1,1]/sum(ctab[,1]) # spec
spec
sens<-ctab[2,2]/sum(ctab[,2]) # sens
sens
npv<-ctab[1,1]/sum(ctab[1,]) # npv
npv
ppv<-ctab[2,2]/sum(ctab[2,]) #ppv
ppv
acc<-sum(diag(prop.table(ctab))) #acc
acc

#add point and/or text to curve
points((1-ctab[1,1]/sum(ctab[,1])),ctab[2,2]/sum(ctab[,2]),pch=16,cex=1.5,col="skyblue4")
#text(1-spec-0.25,sens+0.24,cex=0.8,adj=0,labels=paste("Sensitivity: 0.70 (0.49-0.86)\nSpecificity: 0.77 (0.57-0.91)\nAUC: 0.71 (0.54-0.86)"))
fitperf2 <- performance(fitpred, "acc")
plot(fitperf2, avg= "vertical", spread.estimate="boxplot", lwd=3,col='blue', show.spread.at= seq(0.1, 0.9, by=0.1), main= "Accuracy across the range of possible cutoffs",ylim=c(0,1))
fitperf2@y.values #Zoek maximale waarde in de tabel
fitperf2@x.values 
abline(a=0.6370,b=0,lwd=2,lty=2,col="gray")

library(PropCIs)
cisens<-midPci(ctab[2,2],sum(ctab[,2]),conf.level=0.95) #95%CI sens
cisens$conf.int[1:2]
cispec<-midPci(ctab[1,1],sum(ctab[,1]),conf.level=0.95) #95%CI spec
cispec$conf.int[1:2]
cippv<-midPci(ctab[2,2],sum(ctab[2,]),conf.level=0.95) #95%CI PPV
cippv$conf.int[1:2]
cinpv<-midPci(ctab[1,1],sum(ctab[1,]),conf.level=0.95) #95%CI NPV
cinpv$conf.int[1:2]
ciacc<-midPci((ctab[1,1]+ctab[2,2]),sum(ctab),conf.level=0.95) #95%CI Acc
ciacc$conf.int[1:2]

##using pROC
library(pROC)
risk.hat<-as.numeric(risk.hat)
ciauc<-ci.auc(roc(as.factor(class)[subset],risk.hat), conf.level=0.95,method="bootstrap",boot.n=10000)

#Maken van samenvattende tabel
sumtab=matrix(NA,6,2)
sumtab[1,1]=round(sens,3)
sumtab[2,1]=round(spec,3)
sumtab[3,1]=round(ppv,3)
sumtab[4,1]=round(npv,3)
sumtab[5,1]=round(acc,3)
sumtab[6,1]=round(auc,3)
sumtab[1,2]=paste("(",round(cisens$conf.int[1],3),"-", round(cisens$conf.int[2],3),")")
sumtab[2,2]=paste("(",round(cispec$conf.int[1],3),"-", round(cispec$conf.int[2],3),")")
sumtab[3,2]=paste("(",round(cippv$conf.int[1],3),"-", round(cippv$conf.int[2],3),")")
sumtab[4,2]=paste("(",round(cinpv$conf.int[1],3),"-", round(cinpv$conf.int[2],3),")")
sumtab[5,2]=paste("(",round(ciacc$conf.int[1],3),"-", round(ciacc$conf.int[2],3),")")
sumtab[6,2]=paste("(",round(ciauc[1],3),"-", round(ciauc[3],3),")")
rownames(sumtab)<-c('Sensitivity','Specificity','Positive Predictive Value','Negative Predictive Value','Accuracy','AUCROC')
colnames(sumtab)<-c('Value','(95% Confidence Interval)')
model2sumtab<-noquote(sumtab)
model2sumtab

#which variables are most often selected by the LASSO?
nonzero<-array(NA,10)
for(i in 1:10){
  nonzero[i]<-sum((coef[i,]!=0))
}
imp.variables<-data.frame(cbind(rownames(coef(glm.lasso))[nonzero>0],nonzero[nonzero>0]))
colnames(imp.variables)<-c("variable","frequency")
imp.variables$frequency<-as.numeric(as.character(imp.variables$frequency))
imp.variables<-imp.variables[order(-imp.variables$frequency),]
imp.variables



