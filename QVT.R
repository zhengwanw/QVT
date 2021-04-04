#the required R packages, which can be installed using install.packges("XX") function.
#install.packages("irr")
library(irr)
library(ROCR)
library(caret)
library(infotheo)
library(e1071)

#function of computationg macro-F, which works as the performance metric.
F <-function(ct){
  ct<-ct+0.000001
  if(ncol(ct)==1){
    zero<-matrix(0.000001,2,1)
    names<-c("A","B")
    diff<-setdiff(names,colnames(ct))
    if(is.element("0",diff)){
      ct<-cbind(zero,ct)
    }else{
      ct<-cbind(ct,zero)
    }
  }
  pre1<-ct[1,1]/sum(ct[,1])
  rec1<-ct[1,1]/sum(ct[1,])
  pre2<-ct[2,2]/sum(ct[,2])
  rec2<-ct[2,2]/sum(ct[2,])
  pre<-mean(pre1,pre2)
  rec<-mean(rec1,rec2)
  2*pre*rec/(pre+rec)
}

#read in data. Here read.delim function is used as an example.
data<-read.delim("clipboard",row.names=1)

#data preprocessing
data.tra<-data[,-1]
group1<-sum(data[,1])#1
group0<-nrow(data)-group1#0
group1
group0
nrow<-nrow(data.tra)
randomGroup0<-sample(1:group0,50,replace=FALSE)
randomGroup1<-sample((group0+1):nrow,50,replace=FALSE)
y<-LETTERS[c(rep(1,50),rep(2,50))]
data.lab<-cbind(y,data.tra[c(randomGroup0,randomGroup1),])

#make training data and testing data. The training data are used to build learning model, and the testing data are used to evaluate the performance of learning model.
test1<-sample(51:100,17,replace=FALSE)
test0<-sample(1:50,17,replace=FALSE)
Test<-data.lab[c(test0,test1),]
Train<-data.lab[-c(test0,test1),]
numOfA<-which(Train$y=="A")
numOfB<-which(Train$y=="B")

#data normalization
ScaleTrain<-cbind(y=LETTERS[Train$y],scale(Train[,-1]))
ScaleTest<-cbind(y=LETTERS[Test$y],scale(Test[,-1]))
TrainA<-ScaleTrain[1:33,-1]

#difine the representitives, which are defined as RealData
dat<-discretize(t(TrainA))
I <- mutinformation(dat,method= "emp") 
multiInfo<-cbind(1:33,apply(I,1,sum))
orderA<-multiInfo[order(multiInfo[,2],decreasing=TRUE)]

TrainB<-ScaleTrain[34:66,-1]
dat<-discretize(t(TrainB))
I <- mutinformation(dat,method= "emp") 
multiInfo<-cbind(1:33,apply(I,1,sum))
orderB<-multiInfo[order(multiInfo[,2],decreasing=TRUE)]

train1<-c(numOfA[orderA[1:23]])
train2<-c(numOfB[orderB[1:23]])
RealData<-as.data.frame(Train[c(train1,train2),])

#fist run of feature selection(Lasso)   
is_minus <- function(x) {x < 0 | x == 0}
ncol1<-ncol(RealData)-1
exp<-as.matrix(RealData[,-1])
target<-as.matrix(c(rep(0,23),rep(1,23)))

#computation of lasso
lasso.model.cv <- cv.glmnet(x = exp, y =target, family = "gaussian")
bestLamda<-lasso.model.cv$lambda.min
lasso.model <- glmnet(x = exp, y = target, family = "gaussian", lambda = bestLamda, alpha = 1)
ig.train<-lasso.model$beta[,1]
num1<-c(1:ncol1)
imp.num1<-cbind(num1,ig.train)
colnames(imp.num1)<-c("num","score")
ig_sort1<-imp.num1[order(imp.num1[,2],decreasing = TRUE),]
unnecessary_col <- is_minus(ig_sort1[,2])
nnum1<- ig_sort1[!unnecessary_col,]
      
if(is.null(nrow(nnum1))){
nnum1<-nnum1[1]
}else{    
nnum1<-nnum1[,1]
}

#feature training 
for(aa in 24:33){
     train1<-c(train1,numOfA[orderA[aa]]) 
     train2<-c(train2,numOfB[orderB[aa]]) 
     RealData<-as.data.frame(Train[c(train1,train2),])    
     ncol2<-ncol(RealData)-1
     exp<-as.matrix(RealData[,-1])
     target<-as.matrix(c(rep(0,aa),rep(1,aa)))
     #computation of lasso
     lasso.model.cv <- cv.glmnet(x = exp, y =target, family = "gaussian")
     bestLamda<-lasso.model.cv$lambda.min
     lasso.model <- glmnet(x = exp, y = target, family = "gaussian", lambda = bestLamda, alpha = 1)
     ig.train<-lasso.model$beta[,1]

     num2<-c(1:ncol2)
     imp.num2<-cbind(num2,ig.train)
     colnames(imp.num2)<-c("num","score")
     ig_sort2<-imp.num2[order(imp.num2[,2],decreasing = TRUE),]
     unnecessary_col <- is_minus(ig_sort2[,2])
     nnum2<- ig_sort2[!unnecessary_col,]
     if(is.null(nrow(nnum2))){
        nnum2<-nnum2[1]
     }else{    
        nnum2<-nnum2[,1]
     }
     same<-intersect(nnum1, nnum2)
     len.same<-length(same)
     same.value<-matrix(0,len.same,1)
     if(len.same==0){
       same_sort<-c()
     }else{
       for(s in 1:len.same){
          if(is.null(nrow(ig_sort1))){
            ss2<-match(same[s],ig_sort2[,1])
            same.value[s,1]<-mean(c(ig_sort1[2],ig_sort2[ss2,2]))
          }else{
            ss1<-match(same[s],ig_sort1[,1])
            ss2<-match(same[s],ig_sort2[,1])
            same.value[s,1]<-mean(c(ig_sort1[ss1,2],ig_sort2[ss2,2]))
          }
        }
 @@@same.vector<-cbind(same,same.value)
       rownames(same.vector)<-colnames(data.tra[,same])
       colnames(same.vector)<-c("num","score")
       same_sort<-same.vector[order(same.vector[,2],decreasing=TRUE),]        
    }
      len1<-length(nnum1)
      len2<-length(nnum2)

     if((len.same < len1) || (len.same < len2)){
        diff1<-ig_sort1[match(setdiff(nnum1,nnum2),ig_sort1[,1]),]
        diff2<-imp.num2[setdiff(nnum2,nnum1),]
        diff<-rbind(diff1,diff2)      
     if(is.null(nrow(diff))){
        diff.vector<-diff
     }else{
        diff.vector<-diff[order(diff[,2],decreasing=TRUE),]
     }    
       ig_sort1<-rbind(same_sort,diff.vector)
       ig_sort1<-ig_sort1[order(ig_sort1[,2],decreasing=TRUE),]
       nnum1<-ig_sort1[,1]
     }else{
       ig_sort1<-same_sort
       if(is.null(nrow(ig_sort1))){
         nnum1<-ig_sort1[1]
        }else{
         nnum1<-ig_sort1[,1]
        }
     }
}
#output
ig_sort1

#using svm to build the learning model, and test the performance
results_of_learningModel<-function(rankingOffeatures,Train, Test){
  nnum<-data.frame(rankingOffeatures)$num
  #svm 
  numOfvar<-2
  lenOfsel<-length(nnum)-1
  res<-matrix(0,lenOfsel,3)
  colnames(res)<-c("numOfvari","QVT(Lasso)_SVM_F","QVT(Lasso)_SVM_AUC")
  numOftest = nrow(Test)
  for(re in 1:lenOfsel){  
    Umatrix<-matrix(0,numOftest,3)
    colnames(Umatrix) <- c("real","predict","score")
    data.svm<-svm(y~.,data =Train[,c(1,nnum[1:numOfvar]+1)], probability=TRUE)
    result_predict<-predict(data.svm, Test[,-1], probability=TRUE)
    Umatrix[,1] <- as.character(Test$y)
    Umatrix[,2]<-as.character(result_predict)
    Umatrix[,3] <-attr(result_predict,"probabilities")[,1]
    pred <- prediction(as.numeric(Umatrix[,3]),c(rep(1,numOftest/2),rep(0,numOftest/2)))
    perf <- performance(pred, "tpr", "fpr")
    auc.tmp <- performance(pred,"auc")  
    res[re,1]<-numOfvar
    res[re,2]<-F(table(Umatrix[,1],Umatrix[,2]))
    res[re,3]<- as.numeric(auc.tmp@y.values)
    numOfvar<-numOfvar+1
  }
  return(res)
}

train = apply(Train[,-1], 2, function(x){
  as.numeric(as.character(x))
})
Train = cbind.data.frame(y=ScaleTrain[,1],train)

test = apply(Test[,-1], 2, function(x){
  as.numeric(as.character(x))
})
Test = cbind.data.frame(y=ScaleTest[,1],test)
#output
results_of_learningModel(ig_sort1, Train, Test)



