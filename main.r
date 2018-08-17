# install.packages('devtools')
# install.packages('pacman')
# install.packages('ROSE')
# install.packages('rpart')

pacman::p_load('h2o')
h2o.init(nthreads=-1)

library('ROSE')
library('rpart')

path = 'C:/Projects/case_celpe/celpe_case'
setwd(path)
source('utils.r')

## (Executar esta etapa apenas uma vez)
# Lendo a base de treinamento
# dataset = read.table('Train.csv', header=T, sep=';', stringsAsFactors = FALSE)

## Pre processando e salvando em outro arquivo
# dataset = preProcessDataset(dataset)
# write.table(as.data.frame(dataset), file = 'processed_train.csv', sep=';', row.names = FALSE)


####################################


# Lendo a base já processada
dataset = read.table('processed_train.csv', header=T, sep=';')

# Transformando alvo em fator, para o modelo entender que a variável é binária e não numérica
dataset[, 'TARGET'] = as.factor(dataset[, 'TARGET'])

# Dividindo a base em treinamento e teste
sample = sampleDataset(dataset, 0.7)

dataset_train = sample[[1]]
target_train = dataset_train[, 'TARGET']

dataset_test = sample[[2]]
target_test = dataset_test[, 'TARGET']

# Retirando o alvo para facilitar a fórmula
train_NO_TGT = dataset_train[, -ncol(dataset_train)]
test_NO_TGT = dataset_test[, -ncol(dataset_test)]


####################################


# Modelos: 

# Regressão Logística
glm_model = glm(target_train ~ ., family=binomial(link='logit'), data = train_NO_TGT)
summary(glm_model)

# Decision Tree
dt_model = rpart(target_train ~ . , data = train_NO_TGT)
dt_pred = predict(dt_model, newdata = test_NO_TGT)
accuracy.meas(target_test, dt_pred[,2])
roc.curve(target_test, dt_pred[,2], plotit = F)

# Balanced Deep Learning
dl_model = h2o.deeplearning(y='TARGET', training_frame=as.h2o(dataset_train), validation_frame=as.h2o(dataset_test),
    hidden=c(50,50), epochs=0.1, activation='Tanh',
    score_training_samples=1000, 
    score_validation_samples=1000,
    balance_classes=TRUE,
    score_validation_sampling='Stratified')

# Entender os outputs, fazer predições
# random forests, gbm, xgboost, lightgbm, catboost
# cross fold 10 fold

#gbm_model = gbm(target_train ~ ., data = train_NO_TGT)