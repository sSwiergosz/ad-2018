
library(caret)
ctrl.loo <- trainControl(method = 'LOOCV',
                         search = 'grid')

# 1
data.set <- MASS::painters
model.lda <- train(School ~.,
                   data = data.set,
                   method = 'lda',
                   trControl = ctrl.loo)
1 - model.lda$results[2] #cvloo error rate (LDA)

mean(predict(model.lda) != data.set$School) #Resub error rate
confusionMatrix(predict(model.lda),data.set$School)

#2

data.set <- DAAG::leafshape
model.lda <- train(location ~ bladelen + petiole + bladewid,
                   data =  data.set,
                   method = 'lda',
                   trControl = ctrl.loo)

1 - model.lda$results[2] #cvloo error rate (LDA)


model.qda <- train(location ~ bladelen + petiole + bladewid,
                   data =  data.set,
                   method = 'qda',
                   trControl = ctrl.loo)

1 - model.qda$results[2] #cvloo error rate (QDA)
mean(predict(model.qda) != data.set$location) #Resub error rate

#3

depresja <- c(6,4,0,4,0,11,11,5,8,4,12,8,9,8,11)
niepokoj <- c(8,3,2,1,8,9,6,7,6,9,11,8,6,10,4)
chaos <- c(9,3,8,6,4,8,6,4,5,4,6,5,7,8,3)

group <- factor(rep(1:3, each=5))

data.set <- data.frame(depresja,niepokoj,chaos,group)

model.lda <- train(group ~. ,
                   data = data.set,
                   method = 'lda',
                   trControl =ctrl.loo)
1 - model.lda$results[2] #cvloo error rate (LDA)

model.qda <- train(group ~. ,
                   data = data.set,
                   method = 'qda',
                   trControl =ctrl.loo)
1 - model.qda$results[2] #cvloo error rate (LDA)

model.nb <- train(group ~. ,
                   data = data.set,
                   method = 'nb',
                   trControl =ctrl.loo,
                   tuneGrid = data.frame(usekernel = FALSE)) #tu cos brakuje nie zdazylem zapytac
1 - model.nb$results[4] #cvloo error rate (LDA)

#zad4

data.set <-read.table('http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data',
                      header = TRUE,
                      row.names = 1,
                      sep = ',')
model.1NN <- train(famhist ~. ,
                   data = data.set,
                   method = 'knn',
                   tuneGrid = data.frame(k = 1),
                   trControl =ctrl.loo)
1 - model.1NN$results[2] #cvloo error rate (LDA)
mean(predict(model.1NN) != data.set$famhist)

model.rf <- train(famhist ~. ,
                   data = data.set,
                   method = 'rf',
                   tuneGrid = data.frame(mtry =2),
                   trControl =ctrl.loo)
1 - model.rf$results[2] #cvloo error rate (LDA)
mean(predict(model.rf) != data.set$famhist)
model.rf$finalModel$confusion
