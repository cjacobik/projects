### The R code for Boosting
library(gbm)
library(nnet)
library(MASS)
library(gam)
library(e1071)
library(rpart)
library(class)
library(randomForest)
library(dplyr)
library(stringdist)
library(acss)
setwd("C://Users/cjaco/Desktop/GATech/ISYE7406/project/data")

## Read Data 
legit=read.table(file="top-1m.csv",sep=",",header=F)
colnames(legit)=c("rank","domain")
legit$dga=0
legit$family="none"
malic=read.table(file="dga.txt",header=F,sep="\t",skip=18)
colnames(malic)=c("family","domain","starttime","endtime")
malic$dga=1

legitmalic=rbind(legit[,c("dga","domain","family")],malic[,c("dga","domain","family")])

#test string functions on smaller dataset
# ll=head(legit)
# ll$dom=sapply(strsplit(as.character(ll$domain),"\\."),`[`,1)
legitmalic$dom=sapply(strsplit(as.character(legitmalic$domain),"\\."),`[`,1)
legit$dom=sapply(strsplit(as.character(legit$domain),"\\."),`[`,1)

#probs from alexa
alexasize=500
b=qgrams(legit[c(1:alexasize),"dom"],q=1)
unigrams=b/sum(b)
b=qgrams(legit[c(1:alexasize),"dom"],q=2)
bigrams=b/sum(b)
b=qgrams(legit[c(1:alexasize),"dom"],q=3)
trigrams=b/sum(b)

#functions to sum alexa probabilieis
getalexaprob<-function(g,bigrams){
  sumalexabigram=0.0
  for(i in colnames(g)){
    sumalexabigram=sumalexabigram+g[,i]*tryCatch(bigrams[,i],error=function(e) 0)
  }
  return(sumalexabigram/length(g))
}

#function to get vowelpercents
getvowelpct<-function(g){
  sum(g[colnames(g) %in% c('a','e','i','o','u')])/sum(g)
}

#function findsubwords
subwords=function(word,ww){
  subword=c()
  n=nchar(word)
  for(i in 1:(n-3)){
    for(j in (i+3):n){
      w=substr(word,i,j)
      # print(c(i,j,w))
      if(match(w,ww,nomatch=0)>0){
        subword=append(subword,w)
      }
    }
  }
  return(subword)
}

#function to find outstanding
findoutstanding<-function(word,sw){
  if(length(sw)<=1){
    return(0)
  }
  sw2=sw[order(nchar(sw),sw,decreasing=T)]
  n=length(sw)
  for(i in 1:n){
    word=gsub(sw2[i],"_",word)
  }
  return(nchar(gsub("_","",word)))
}

#function to return dictionary features
dictfunc=function(word,ww){
  sw=subwords(word,ww)
  outsandinglen=findoutstanding(word,sw)
  return(c(outsandinglen,length(sw)))
}

#function to get freqcount
freqcountstats=function(g){
  return(c(max(g),mean(g),getvowelpct(g)))
}

#read word dictionary
words=read.table(file="words_alpha.txt",header=F)
ww=words[nchar(words$V1)>=4,]

#apply functions to dataframe
# ll$unigram=sapply(ll$dom,function(x) getalexaprob(qgrams(x,q=1),bigrams=unigrams)) #unigram probs
# ll$bigram=sapply(ll$dom,function(x) getalexaprob(qgrams(x,q=2),bigrams=bigrams)) #bigram probs
# ll$trigram=sapply(ll$dom,function(x) getalexaprob(qgrams(x,q=3),bigrams=trigrams)) #trigram probs
# ll$length=sapply(ll$dom,nchar) #length of string
# ll$uniquelength=sapply(ll$dom,function(x) length(unique(strsplit(x,"")[[1]]))) #number of unique characters
# ll$freqmax=sapply(ll$dom,function(x) max(qgrams(x,q=1))) #max character frequency
# ll$freqmean=sapply(ll$dom,function(x) mean(qgrams(x,q=1))) #average character frequency
# ll$pctvowels=sapply(ll$dom,function(x) getvowelpct(qgrams(x,q=1))) #vowel percentage
# ll$entropy=sapply(ll$dom,function(x) entropy(x)) #string entropy
# res=sapply(ll$dom,function(x) dictfunc(x,ww))
# ll$dictoutstanding=res[1,] #length of outstanding characters after removing dictionary words
# ll$dictcountsubwords=res[2,] #length of dictionary words found


legitmalic$unigram=sapply(legitmalic$dom,function(x) getalexaprob(qgrams(x,q=1),bigrams=unigrams)) #unigram probs
legitmalic$bigram=sapply(legitmalic$dom,function(x) getalexaprob(qgrams(x,q=2),bigrams=bigrams)) #bigram probs
legitmalic$trigram=sapply(legitmalic$dom,function(x) getalexaprob(qgrams(x,q=3),bigrams=trigrams)) #trigram probs
legitmalic$length=sapply(legitmalic$dom,nchar) #length of string
legitmalic$uniquelength=sapply(legitmalic$dom,function(x) length(unique(strsplit(x,"")[[1]]))) #number of unique characters

res=sapply(legitmalic$dom,function(x) freqcountstats(qgrams(x,q=1)))
legitmalic$freqmax=res[1,] #max character frequency
legitmalic$freqmean=res[2,] #average character frequency
legitmalic$pctvowels=res[3,] #vowel percentage

legitmalic$entropy=sapply(legitmalic$dom,function(x) entropy(x)) #string entropy

#run at your own risk...takes FORRRREVVVVEEEERRRRR
res=sapply(legitmalic$dom,function(x) dictfunc(x,ww))
legitmalic$dictoutstanding=res[1,] #length of outstanding characters after removing dictionary words
legitmalic$dictcountsubwords=res[2,] #length of dictionary words found

#write.table(legitmalic,file="final_legitmalic.txt",sep="\t",row.names=F)
##################################################
rm(list=ls())
dgadata=read.table("final_legitmalic.txt",sep="\t",header=T)
dgadata$dga=as.factor(dgadata$dga)
dgadata[is.na(dgadata)]<-0

#########################################################
#EDA on "training" set
#########################################################
dim(dgadata)
s=summary(dgadata)
write.table(t(s),"summarystats.txt",sep=",",col.names=F)
#fill in nas with 0s since they're in bigram/trigram columns


#filter down to the columns I care about: dga,pclass, sex, age, SibSp
#Parch,Fare,Embarked
traincols=c(5:15)
#boxplots
par(mfrow = c(3,4),mar=c(4,4,2,.5))
for(i in traincols){
  boxplot(dgadata[,i],las=1,main=colnames(dgadata)[i],xlab="Variable",ylab="Value")
}
dev.off()

#boxplots given dga
par(mfrow = c(3,4),mar=c(4,4,2,.5))
#par(mfrow = c(3,3))
cols=colnames(dgadata)
for(i in traincols){
  boxplot(dgadata[,cols[i]]~dgadata[,cols[1]],main=paste(cols[i],cols[1],sep=" | "),ylab=cols[i],xlab=cols[1],cex.axis=1.1,cex.lab=1.1)
}
dev.off()
par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1))

#correlation matrix
library("PerformanceAnalytics")
set.seed(123)
nrows=nrow(dgadata)
flag <- sort(sample(nrows,nrows*.01, replace = FALSE))
dgacorr=dgadata[flag,c(1,traincols)]
dgacorr[,1]=as.numeric(dgacorr[,1])-1
chart.Correlation(dgacorr,histogram = T)

#########################################################
## MCCV using training set (crossvalidation from training set)
#########################################################
#recreate train/test sets
set.seed(123)
outputdf=data.frame(m=numeric(),modelid=numeric(),model=character(),trainerror=numeric(),cverror=numeric(),testerror=numeric())
rowi=1
traincols=c(1,5:15)
nrows=nrow(dgadata)

pdf("mccv_models.pdf")
for(m in 1:100){
  flag <- sort(sample(nrows,nrows*.01, replace = FALSE))
  mccvdata=dgadata[flag,]
  flag <- sort(sample(nrow(mccvdata),nrow(mccvdata)*.8, replace = FALSE))
  dgatrain <- mccvdata[flag,traincols] #80% of data
  dgatest2 <- mccvdata[-flag,traincols]
  
  flag <- sort(sample(nrow(dgatest2),nrow(dgatest2)*.5, replace = FALSE))
  dgatest <- dgatest2[flag,] #10% of data
  dgacv <- dgatest2[-flag,] #10% of data
  
  modelj=1
  
  y1    <- dgatrain$dga;
  y2    <- dgacv$dga;
  y3=dgatest$dga
  
  #create numerical columns for adaboost and other methods using onehotencoding
  dgatrain2=dgatrain
  dgatrain2[,1]=as.numeric(dgatrain2[,1])-1

  dgacv2=dgacv
  dgacv2[,1]=as.numeric(dgacv2[,1])-1

  dgatest2=dgatest
  dgatest2[,1]=as.numeric(dgatest2[,1])-1

  # adaboost cross validation on parameters
  print("gbm")
  cvdf=data.frame(shrinkage=numeric(),depth=numeric(),trainerror=numeric(),cverror=numeric())
  ii=1
  for(shrinkage in c(.01,.05)){
    for(depth in c(2,4)){
      gbm.dga <- gbm(dga ~ .,data=dgatrain2,
                          distribution = 'bernoulli',
                          n.trees = 5000, 
                          shrinkage = shrinkage,
                          interaction.depth = depth
                          ,cv.folds=10)
      
      ## Model Inspection 
      ## Find the estimated optimal number of iterations
      perf_gbm1 = gbm.perf(gbm.dga, method="cv") 
      # perf_gbm1
      # summary(gbm.dga)
      
      ## Training error
      pred1gbm <- predict(gbm.dga,newdata = dgatrain2, n.trees=perf_gbm1, type="response")
      y1hat <- ifelse(pred1gbm < 0.5, 0, 1)
      sum(y1hat != y1)/length(y1)  ##Training error = 0.1428571
      
      ## Testing Error
      y2hat <- ifelse(predict(gbm.dga,newdata = dgacv2[,-1], n.trees=perf_gbm1, type="response") < 0.5, 0, 1)
      mean(y2hat != y2) 
      cvdf[ii,]=c(shrinkage,depth,sum(y1hat != y1)/length(y1),mean(y2hat != y2))
      ii=ii+1
    }
  }
  
  gbm.dga <- gbm(dga ~ .,data=dgatrain2,
                      distribution = 'bernoulli',
                      n.trees = 5000, 
                      shrinkage = cvdf[cvdf$cverror==min(cvdf$cverror),"shrinkage"][1], 
                      interaction.depth = cvdf[cvdf$cverror==min(cvdf$cverror),"depth"][1]
                      ,cv.folds=10)
  
  ## Model Inspection 
  ## Find the estimated optimal number of iterations
  perf_gbm1 = gbm.perf(gbm.dga, method="cv") 
  perf_gbm1
  summary(gbm.dga)
  
  ## Training error
  pred1gbm <- predict(gbm.dga,newdata = dgatrain2, n.trees=perf_gbm1, type="response")
  y1hat <- ifelse(pred1gbm < 0.5, 0, 1)
  sum(y1hat != y1)/length(y1)  ##Training error = 0.1428571
  
  ## Testing Error
  y2hat <- ifelse(predict(gbm.dga,newdata = dgacv2[,-1], n.trees=perf_gbm1, type="response") < 0.5, 0, 1)
  mean(y2hat != y2) 
  y3hat <- ifelse(predict(gbm.dga,newdata = dgatest2[,-1], n.trees=perf_gbm1, type="response") < 0.5, 0, 1)
  mean(y3hat != y3)
  ## Testing error = 0.1977612
  outputdf[rowi,]=c(m,modelj,'GBM',sum(y1hat != y1)/length(y1),mean(y2hat != y2),mean(y3hat != y3))
  rowi=rowi+1
  modelj=modelj+1
  
  #########################################################
  ## Testing errors of several algorithms on the titanic dataset:
  #A. Logistic regression:
  print("logistic")
  modA <- multinom(dga ~ ., data = dgatrain);
  y1hatA <- ifelse(predict(modA, dgatrain[,-1], type="probs" ) < 0.5, 0, 1)
  sum(y1hatA != y1)/length(y1) #0.1845907
  y2hatA <- ifelse(predict(modA, dgacv[,-1], type="probs" ) < 0.5, 0, 1)
  sum(y2hatA != y2)/length(y2) #0.2164179
  y3hatA <- ifelse(predict(modA, dgatest[,-1], type="probs" ) < 0.5, 0, 1)
  outputdf[rowi,]=c(m,modelj,'Logistic Regression',sum(y1hatA != y1)/length(y1),sum(y2hatA != y2)/length(y2),sum(y3hatA != y3)/length(y3))
  rowi=rowi+1
  modelj=modelj+1
  
  #B.Linear Discriminant Analysis :
  print("lda")
  modB <- lda(dgatrain2[,-1], dgatrain2[,1])
  y1hatB <- predict(modB, dgatrain2[,-1])$class
  mean( y1hatB  != y1) #0.1910112
  y2hatB <- predict(modB, dgacv2[,-1])$class
  mean( y2hatB  != y2) #0.2164179
  y3hatB <- predict(modB, dgatest2[,-1])$class
  outputdf[rowi,]=c(m,modelj,'LDA',mean( y1hatB  != y1),mean( y2hatB  != y2),mean( y3hatB  != y3))
  rowi=rowi+1
  modelj=modelj+1
  
  #B.1 QDA
  #need to update
  print("qda")
  modB1 <- qda(dgatrain2[,-1], dgatrain2[,1]); 
  y1hatB1 <- predict(modB1, dgatrain2[,-1])$class
  mean( y1hatB1  != y1) #0.1797753
  y2hatB1 <- predict(modB1, dgacv2[,-1])$class
  mean( y2hatB1  != y2) #0.2014925
  y3hatB1 <- predict(modB1, dgatest2[,-1])$class
  outputdf[rowi,]=c(m,modelj,'QDA',mean( y1hatB1  != y1),mean( y2hatB1  != y2),mean( y3hatB1  != y3))
  rowi=rowi+1
  modelj=modelj+1
  
  ## C. Naive Bayes (with full X)
  print("naive bayes")
  cvdf=data.frame(laplace=numeric(),trainerror=numeric(),cverror=numeric())
  ii=1
  for(laplace in c(0:1)){
    modC <- naiveBayes(as.factor(dga) ~. , data = dgatrain,laplace=laplace)
    y1hatC <- predict(modC, newdata = dgatrain)
    mean( y1hatC != y1) #0.1942215
    y2hatC <- predict(modC, newdata = dgacv)
    mean( y2hatC != y2) #0.1977612
    cvdf[ii,]=c(laplace,mean( y1hatC != y1),mean( y2hatC != y2))
    ii=ii+1
  }
  
  modC <- naiveBayes(as.factor(dga) ~. , data = dgatrain,laplace=cvdf[cvdf$cverror==min(cvdf$cverror),"laplace"][1])
  y1hatC <- predict(modC, newdata = dgatrain)
  mean( y1hatC != y1) #0.1942215
  y2hatC <- predict(modC, newdata = dgacv)
  mean( y2hatC != y2) #0.1977612
  y3hatC <- predict(modC, newdata = dgatest)
  outputdf[rowi,]=c(m,modelj,'Naive Bayes',mean( y1hatC != y1),mean( y2hatC != y2),mean( y3hatC != y3))
  rowi=rowi+1
  modelj=modelj+1
  
  ## D. Generalized additive model (GAM) with splines:
  print("gam")
  modD <- gam( dga ~ . + s(unigram) + s(bigram) + s(trigram) 
               + s(length)+s(uniquelength)+s(freqmax)+s(freqmean)
               +s(pctvowels)+s(entropy)+s(dict_outstandinglen)
               +s(dict_lensubwords), 
               family = binomial, data= dgatrain, trace=TRUE)
  y1hatDprob <- predict(modD, dgatrain[,-1],type="response")
  y1hatD <- ifelse(y1hatDprob <0.5,0,1)
  sum(y1hatD != y1)/length(y1) #0.176565
  y2hatDprob <- predict(modD, dgacv[,-1],type="response")
  y2hatD <- ifelse(y2hatDprob <0.5,0,1)
  sum(y2hatD != y2)/length(y2) #0.1940299
  y3hatDprob <- predict(modD, dgatest[,-1],type="response")
  y3hatD <- ifelse(y3hatDprob <0.5,0,1)
  outputdf[rowi,]=c(m,modelj,'GAM w/ Splines',sum(y1hatD != y1)/length(y1),sum(y2hatD != y2)/length(y2),sum(y3hatD != y3)/length(y3))
  rowi=rowi+1
  modelj=modelj+1
  
  
  #E: a single Tree:
  print("cart")
  cvdf=data.frame(minsplit=numeric(),cp=numeric(),maxdepth=numeric(),trainerror=numeric(),cverror=numeric())
  ii=1
  for(minsplit in c(10,20)){
    for(cp in c(.01,.05,.1)){
      for(maxdepth in c(20,30)){
        modE0 <- rpart(dga ~ .,data=dgatrain, method="class", 
                       parms=list(split="gini"),control = rpart.control(cp=cp,minsplit=minsplit,maxdepth=maxdepth))
        opt <- which.min(modE0$cptable[, "xerror"]); 
        cp1 <- modE0$cptable[opt, "CP"];
        modE <- prune(modE0,cp=cp1);
        y1hatE <-  predict(modE, dgatrain[,-1],type="class")
        mean(y1hatE != y1) #0.1637239
        y2hatE <-  predict(modE, dgacv[,-1],type="class")
        mean(y2hatE != y2) #0.1977612
        cvdf[ii,]=c(minsplit,cp,maxdepth,mean(y1hatE != y1),mean(y2hatE != y2))
        ii=ii+1
      }
    }
  }
  
  modE0 <- rpart(dga ~ .,data=dgatrain, method="class", 
                 parms=list(split="gini"),control = rpart.control(cp=cvdf[cvdf$cverror==min(cvdf$cverror),"cp"][1],minsplit=cvdf[cvdf$cverror==min(cvdf$cverror),"minsplit"][1],maxdepth=cvdf[cvdf$cverror==min(cvdf$cverror),"maxdepth"][1]))
  opt <- which.min(modE0$cptable[, "xerror"]); 
  cp1 <- modE0$cptable[opt, "CP"];
  modE <- prune(modE0,cp=cp1);
  plot(modE,compress=TRUE)
  text(modE)
  y1hatE <-  predict(modE, dgatrain[,-1],type="class")
  mean(y1hatE != y1) #0.1637239
  y2hatE <-  predict(modE, dgacv[,-1],type="class")
  mean(y2hatE != y2) #0.1977612
  y3hatE <-  predict(modE, dgatest[,-1],type="class")
  outputdf[rowi,]=c(m,modelj,'CART',mean(y1hatE != y1),mean(y2hatE != y2),mean(y3hatE != y3))
  rowi=rowi+1
  modelj=modelj+1
  
  #F: Random Forest:
  print("randomforest")
  cvdf=data.frame(ntree=numeric(),mtry=numeric(),maxnodes=numeric(),trainerror=numeric(),cverror=numeric())
  ii=1
  for(ntree in c(250,500)){
    for(mtry in c(2,3,4)){
      for(maxnodes in c(5,8)){
        modF <- randomForest(as.factor(dga) ~., data=dgatrain,ntree=ntree,mtry=mtry,maxnodes=maxnodes)
        y1hatF = predict(modF, dgatrain, type='class')
        mean(y1hatF != y1) #0.08346709
        y2hatF = predict(modF, dgacv, type='class')
        mean(y2hatF != y2) #0.1604478
        cvdf[ii,]=c(ntree,mtry,maxnodes,mean(y1hatF != y1),mean(y2hatF != y2))
        ii=ii+1
      }
    }
  }
  
  modF <- randomForest(as.factor(dga) ~., data=dgatrain, importance=TRUE,ntree=cvdf[cvdf$cverror==min(cvdf$cverror),"ntree"][1],mtry=cvdf[cvdf$cverror==min(cvdf$cverror),"mtry"][1],maxnodes=cvdf[cvdf$cverror==min(cvdf$cverror),"maxnodes"][1])
  varImpPlot(modF)
  y1hatF = predict(modF, dgatrain, type='class')
  mean(y1hatF != y1) #0.08346709
  y2hatF = predict(modF, dgacv, type='class')
  mean(y2hatF != y2) #0.1604478
  y3hatF = predict(modF, dgatest, type='class')
  outputdf[rowi,]=c(m,modelj,'RF',mean(y1hatF != y1),mean(y2hatF != y2),mean(y3hatF != y3))
  rowi=rowi+1
  modelj=modelj+1
  
  #G: KNN need to update code here
  print("knn")
  modelname="KNN"
  for (kk in c(1,3,5,7,9,11,13,15)){
    modG <- knn(dgatrain2[,-1], dgatrain2[,-1], dgatrain2[,1], k=kk);
    modG2 <- knn(dgatrain2[,-1], dgacv2[,-1], dgatrain2[,1], k=kk);
    modG3 <- knn(dgatrain2[,-1], dgatest2[,-1], dgatrain2[,1], k=kk);
    outputdf[rowi,]=c(m,modelj,paste(modelname,kk,sep="-"),mean( modG  != y1),mean(modG2 != y2),mean(modG3 != y3))
    rowi=rowi+1
    modelj=modelj+1
  }
  print(m)
}
dev.off()

#Analyze Output
outputdf[,c(1:2,4:6)] <- sapply(outputdf[,c(1:2,4:6)], as.numeric)
aggoutput=outputdf %>% group_by(modelid,model) %>% summarise(medianTrain=round(median(trainerror),6),avgTrain=round(mean(trainerror),6),varTrain=round(var(trainerror),6),medianTest=round(median(testerror),6),avgTest=round(mean(testerror),6),varTest=round(var(testerror),6))
write.table(aggoutput,file="aggoutputcvdf.txt",sep="\t")
boxplot(outputdf$testerror~outputdf$modelid,ylab="Test Error",xlab="Model",main="Boxplot of Monte Carlo Cross Validation Testing Errors")
dev.off()
#write.table(outputdf,file="mccvoutputcvdf.txt",sep="\t")


