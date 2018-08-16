# install.packages(pacman)
# pacman::p_load(h2o)
h2o.init(nthreads=-1)
library('class')
library('ROSE')
library('rpart')

path = "C:/Projects/case_celpe/celpe_case"
setwd(path)

dataset = read.table("Train.csv", header=T, sep=";", stringsAsFactors = FALSE) 

# Checando se a Variável 4 é a concatenação das variáveis 1, 2 e 3
#X4= paste(dataset[,'X1'], dataset[,'X2'], dataset[,'X3'], sep="")
#compare = X4 == dataset[, 'X4']
#summary(compare)
#   Mode    TRUE 
#logical    8000 

# Checando desbalanceamento do alvo
# table(dataset[, 'TARGET']))
# Alvo desbalanceado
#  0    1 
# 7611  389 


numOfRows = dim(dataset)[1]
numOfFeatures = dim(dataset)[2] #311

# Retirar variáveis ID, X1, X2, X3
toRemove = c(1,2,3,4)

# Retirar variáveis com mais de X% de NA
percent = 0.9
for(i in 1:numOfFeatures){
    na = sum(is.na(dataset[,i]))
    if(!is.na(na) && na/numOfRows > percent){
        toRemove = c(toRemove, i)
    }
}

#Retirar variáveis numéricas com valor estático
for(i in 1:numOfFeatures){
    if(!is.numeric(dataset[,i])){
        next
    }
    mini = min(dataset[,i], na.rm=T)
    maxi = max(dataset[,i], na.rm=T)
    if(is.na(mini) || is.na(maxi)){
        next
    }
    if(mini == maxi){
        toRemove = c(toRemove, i)
    }
}

#Retirar variáveis categóricas com valor estático
for(i in 1:numOfFeatures){
    len = length(levels(as.factor(dataset[,i])))
    if(len < 2){        
        toRemove = c(toRemove, i)
    }
}

toRemove = unique(toRemove)

dataset = dataset[, -toRemove]
numOfFeatures = dim(dataset)[2] #278


# Substituir NA por min caso numérico
# Substituir NA por categoria MISSING caso categórica

for(i in 1:numOfFeatures){
    numeric = is.numeric(dataset[,i])
    idxs = which(is.na(dataset[,i]))
    if(length(idxs) > 0){
        if(numeric){        
            dataset[idxs, i] = min(dataset[-idxs,i])
        }else{
            dataset[idxs, i] = "MISSING"            
        }
    }
}
for(i in 1:numOfFeatures){
    numeric = is.numeric(dataset[,i])
    if(!numeric){
        dataset[, i] = as.numeric(as.factor(dataset[,i]))
    }
}

## Split in train + test set
dataset[, 'TARGET'] = as.factor(dataset[, 'TARGET'])
target = dataset[, 'TARGET']

class1 = table(target)[1]
class2 = table(target)[2]
idxs = sample(which(target == 1), class1 - class2, replace = TRUE)
dataset = rbind(dataset, dataset[idxs,])
target = c(target, target[idxs])
table(target)

numOfRows = dim(dataset)[1]

idxs = sample(1:numOfRows, 0.7*numOfRows)
train = dataset[idxs,]
trainTarget = target[idxs]
test = dataset[-idxs,]
testTarget = target[-idxs]



# GLM
glm_model = glm(trainTarget ~ . - train[, 'TARGET'], family=binomial(link='logit'), data = train)
summary(gml_model)

# KNN
train_NO_TGT = train[, -numOfFeatures]
test_NO_TGT = test[, -numOfFeatures]
knn_model = knn(train_NO_TGT, test_NO_TGT, trainTarget, k = 2)
table(testTarget,knn_model)

# DT
dt_model = rpart(trainTarget ~ . - train[, 'TARGET'], data = train)
dt_pred = predict(dt_model, newdata = test)
accuracy.meas(testTarget, dt_pred)
roc.curve(testTarget, dt_pred, plotit = F)


#train_over = ovun.sample(trainTarget ~ ., data = train, method = "over", N = 15222)$data

# Balanced Deep Learning

dl_model = h2o.deeplearning(y='TARGET', training_frame=as.h2o(train), validation_frame=as.h2o(test),
                            hidden=c(50,50), epochs=0.1, activation="Tanh",
                            score_training_samples=1000, 
                            score_validation_samples=1000,
                            balance_classes=TRUE,
                            score_validation_sampling="Stratified")

