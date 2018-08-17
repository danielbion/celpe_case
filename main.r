pacman::p_load('h2o')
h2o.init(nthreads=-1)
library('ROSE')
library('rpart')
library('xgboost')
library('caret')

path = 'C:/Projects/case_celpe/celpe_case'
setwd(path)
source('utils.r')

# Decision Tree
dtTest = function(dataset){
    dt_model = rpart(dataset$trainLabels ~ ., data = dataset$train_NO_TGT)
    pred = predict(dt_model, newdata = dataset$test_NO_TGT)

    pred_hard = ifelse(pred < 0.50, 0, 1)
    print ("Decision Tree")
    print(confusionMatrix(as.factor(pred_hard), as.factor(dataset$testLabels)))
    print(roc.curve(dataset$testLabels, pred, plotit = F))
}

# Balanced Deep Learning
deepTest = function(dataset){
    # Transformando alvo em fator, para o modelo entender que a variável é binária e não numérica
    x = dataset
    dataset$train[, 'TARGET'] = as.factor(dataset$train[, 'TARGET'])
    dataset$test[, 'TARGET'] = as.factor(dataset$test[, 'TARGET'])
    dl_model = h2o.deeplearning(y='TARGET', training_frame=as.h2o(dataset$train), validation_frame=as.h2o(dataset$test),
        hidden=c(100, 100), epochs=5, activation='Tanh',
        score_training_samples=0, 
        score_validation_samples=0,
        balance_classes=TRUE)
    print("Deep Learning")
    print (dl_model)
}

# Extreme Gradient Boosting
xgboostTest = function(dataset){
    # Alvo precisa ser 0 e 1 e numérico
    trainLabels = as.numeric(dataset$trainLabels)
    trainLabels[which(trainLabels == 2)] = 0

    bst= xgboost(data = data.matrix(dataset$train_NO_TGT, rownames.force = NA), label = trainLabels, nrounds = 50, objective = "binary:logistic")
    pred = predict(bst, data.matrix(dataset$test_NO_TGT, rownames.force = NA))
    
    pred_hard = ifelse(pred < 0.50, 0, 1)
    print ("Decision Tree")
    print(confusionMatrix(as.factor(pred_hard), as.factor(dataset$testLabels)))
    print(roc.curve(dataset$testLabels, pred, plotit = F))    
}


# Lendo a base pré processada
dataset = read.table('processed_train.csv', header=T, sep=';')
# Dividindo a base em 70% treinamento e 30% teste
set.seed(2)
dataset = splitDataset(dataset, 0.7)

dtTest(dataset)
deepTest(dataset)
xgboostTest(dataset)