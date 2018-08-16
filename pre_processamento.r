# install.packages(pacman)
# install.packages(class)
# install.packages(ROSE)
# install.packages(rpart)
pacman::p_load(h2o)
h2o.init(nthreads=-1)
library('class')
library('ROSE')
library('rpart')

path = "C:/Projects/case_celpe/celpe_case"
setwd(path)
source('utils.r')

dataset = read.table("Train.csv", header=T, sep=";", stringsAsFactors = FALSE) 

numOfRows = dim(dataset)[1]
numOfFeatures = dim(dataset)[2]

# Retirar variáveis ID, X1, X2, X3
toRemove = c(1,2,3,4)

# Retirar variáveis com mais de 90% de NA
toRemove = c(toRemove, removeNA(dataset, 0.9))

# Retirar variáveis com valor estático
toRemove = c(toRemove, removeNumericalStatic(dataset))
toRemove = c(toRemove, removeCategorialStatic(dataset))
toRemove = unique(toRemove)

dataset = dataset[, -toRemove]
numOfFeatures = dim(dataset)[2]

# Substituir NA por min caso numérico
# Substituir NA por categoria MISSING caso categórica
dataset = replaceNA(dataset)
dataset = replaceCategorialWithNumerical(dataset);


dataset[, 'TARGET'] = as.factor(dataset[, 'TARGET'])

#table(dataset[, 'TARGET'])
dataset = overBalanceDataset(dataset)
numOfRows = dim(dataset)[1]
#table(dataset[, 'TARGET'])

sample = sampleDataset(dataset, 0.7)
dataset_train = sample[[1]]
target_train = dataset_train[, 'TARGET']
dataset_test = sample[[2]]
target_test = dataset_test[, 'TARGET']

train_NO_TGT = dataset_train[, -numOfFeatures]
test_NO_TGT = dataset_test[, -numOfFeatures]

# Modelo: 
# GLM
glm_model = glm(target_train ~ . - dataset_train[, 'TARGET'], family=binomial(link='logit'), data = dataset_train)
summary(gml_model)

# KNN
knn_model = knn(train_NO_TGT, test_NO_TGT, target_train, k = 2)
table(target_test, knn_model)

# DT
dt_model = rpart(target_train ~ . , data = train_NO_TGT)
dt_pred = predict(dt_model, newdata = test_NO_TGT)
accuracy.meas(target_test, dt_pred[,2])
roc.curve(target_test, dt_pred[,2], plotit = F)

# Balanced Deep Learning

dl_model = h2o.deeplearning(y='TARGET', training_frame=as.h2o(dataset_train), validation_frame=as.h2o(dataset_test),
                            hidden=c(50,50), epochs=0.1, activation="Tanh",
                            score_training_samples=1000, 
                            score_validation_samples=1000,
                            balance_classes=TRUE,
                            score_validation_sampling="Stratified")

#train_over = ovun.sample(trainTarget ~ ., data = train, method = "over", N = 15222)$data
# Entender os outputs, fazer predições, testar undersampling
