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
    dt_model = rpart(dataset$trainLabels ~ ., data = dataset$trainNoLabels)
    pred = predict(dt_model, newdata = dataset$testNoLabels)

    pred_hard = ifelse(pred < 0.50, 0, 1)
    cfMatrix = confusionMatrix(as.factor(pred_hard), as.factor(dataset$testLabels))
    auc = roc.curve(dataset$testLabels, pred, plotit = F)$auc
    return (list(auc, cfMatrix))
}

# Balanced Deep Learning
deepTest = function(dataset, balanced){
    # Transformando alvo em fator, para o modelo entender que a variável é binária e não numérica
    dataset$train[, 'TARGET'] = as.factor(dataset$train[, 'TARGET'])
    dataset$test[, 'TARGET'] = as.factor(dataset$test[, 'TARGET'])
    dl_model = h2o.deeplearning(y='TARGET', training_frame=as.h2o(dataset$train), validation_frame=as.h2o(dataset$test),
        hidden=c(50, 50), epochs=1, activation='Tanh',
        score_training_samples=0, 
        score_validation_samples=0,
        balance_classes=balanced,
        quiet_mode = TRUE)
    pred = h2o.predict(dl_model, as.h2o(dataset$test))
    pred = as.data.frame(pred)

    pred_hard = pred[,1]
    pred_prob = pred[,3]

    cfMatrix = confusionMatrix(as.factor(pred_hard), as.factor(dataset$testLabels))
    auc = roc.curve(dataset$testLabels, pred_prob, plotit = F)$auc
    return (list(auc, cfMatrix))
}

# Extreme Gradient Boosting
xgboostTest = function(dataset){
    # Alvo precisa ser 0 e 1 e numérico
    trainLabels = as.numeric(dataset$trainLabels)
    trainLabels[which(trainLabels == 2)] = 0

    bst= xgboost(data = data.matrix(dataset$trainNoLabels, rownames.force = NA), 
        label = trainLabels, nrounds = 10, objective = "binary:logistic", verbose = 0)
    pred = predict(bst, data.matrix(dataset$testNoLabels, rownames.force = NA))
    
    pred_hard = ifelse(pred < 0.50, 0, 1)
    
    cfMatrix = confusionMatrix(as.factor(pred_hard), as.factor(dataset$testLabels))
    auc = roc.curve(dataset$testLabels, pred, plotit = F)$auc
    return (list(auc, cfMatrix))
}

# Lendo a base pré processada
dataset = read.table('processed_train.csv', header=T, sep=';')

# Dividindo a base em k folds para validação cruzada
numOfFolds = 10
dataset = splitDataset(dataset, numOfFolds)

# Guardando resultados da área sob a curva ROC
aucResults = list()
for(i in 1:numOfFolds){
    dt = dtTest(dataset[[i]])
    aucResults$dt = c(aucResults$dt, dt[[1]])
    
    boost = xgboostTest(dataset[[i]])
    aucResults$boost = c(aucResults$boost, boost[[1]])

    deep = deepTest(dataset[[i]], FALSE)
    aucResults$deep = c(aucResults$deep, deep[[1]])

    deep_balanced = deepTest(dataset[[i]], TRUE)
    aucResults$deep_balanced = c(aucResults$deep_balanced, deep_balanced[[1]])
}

saveRDS(aucResults, "aucResults.rds") 
#aucResults = readRDS("aucResults.rds")

# Analisando os resultados
summary(aucResults$boost)
summary(aucResults$dt)
summary(aucResults$deep)
summary(aucResults$deep_balanced)

# Aplicando testes de hipótese para garantir que os resultados diferem
# Rejeita a hipótese que os resultados são iguais
testModels = friedman.test(cbind( aucResults$dt, aucResults$boost, aucResults$deep, aucResults$deep_balanced)) 

# Não rejeita a hipótese de que um método é melhor que o outro
testDeep = wilcox.test(aucResults$deep_balanced, aucResults$deep, paired = TRUE, alternative = "less", conf.level = 0.95)
testBoostDt = wilcox.test(aucResults$boost, aucResults$dt, paired = TRUE, alternative = "less", conf.level = 0.95)
testBoostDeep = wilcox.test(aucResults$boost, aucResults$deep_balanced, paired = TRUE, alternative = "less", conf.level = 0.95)
testDtDeep = wilcox.test(aucResults$dt, aucResults$deep, paired = TRUE, alternative = "less", conf.level = 0.95)

