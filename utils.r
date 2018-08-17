splitDataset = function(dataset, trainPercent){
    numOfRows = nrow(dataset)
    idxs = sample(1:numOfRows, trainPercent * numOfRows)
    train = dataset[idxs,]
    test = dataset[-idxs,]
    
    set = list()
    set$train = train
    set$trainLabels = set$train[, 'TARGET']

    set$test = test
    set$testLabels = set$test[, 'TARGET']

    # Retirando o alvo para facilitar a fórmula
    set$train_NO_TGT = set$train[, -ncol(set$train)]
    set$test_NO_TGT = set$test[, -ncol(set$test)]
    return (set)
}

selectBestFeatures = function(dataset){
    sig = getSignificance(dataset)
    dataset = dataset[, which(sig < 0.05)]
    return (dataset)
}

getSignificance = function(dataset){
    sig = list()
    numOfFeatures = ncol(dataset)
    for(i in 1: (numOfFeatures-1)){
        # print(names(dataset)[i])
        glm_model = glm(dataset[,'TARGET'] ~ dataset[, i], family=binomial(link='logit'), data = dataset)
        sig[[i]] = summary(glm_model)$coefficients[2,4]
    }
    return (sig)
}

preProcessDataset = function(dataset){
    toRemove = c(1,2,5,6,76,77,78) # Retiradas por análise manual

    # Retirar variáveis com mais de 90% de NA
    toRemove = c(toRemove, removeNA(dataset, 0.9))

    # Retirar variáveis com valor estático
    toRemove = c(toRemove, removeNumericalStatic(dataset))
    toRemove = c(toRemove, removeCategorialStatic(dataset))
    toRemove = unique(toRemove)
    dataset = dataset[, -toRemove]

    # Substituir NA por min caso numérico ou por MISSING caso categórica
    dataset = replaceNA(dataset)
    #dataset = replaceCategorialWithNumerical(dataset)
    return (dataset)
}

removeNA = function(dataset, percent) {
    indexesToRemove = c()
    numOfRows = nrow(dataset)
    numOfFeatures = ncol(dataset)
    for(i in 1:numOfFeatures){
        na = sum(is.na(dataset[,i]))
        if(!is.na(na) && na/numOfRows > percent){
            indexesToRemove = c(indexesToRemove, i)
        }
    }
    return (indexesToRemove)
}

removeNumericalStatic = function(dataset){
    indexesToRemove = c()
    numOfFeatures = ncol(dataset)
    for(i in 1:numOfFeatures){
        if(!is.numeric(dataset[,i])){
            next
        }
        minimun = min(dataset[,i], na.rm=T)
        maximun = max(dataset[,i], na.rm=T)
        if(is.na(minimun) || is.na(maximun)){
            next
        }
        if(minimun == maximun){
            indexesToRemove = c(indexesToRemove, i)
        }
    }
    return (indexesToRemove)
}

removeCategorialStatic = function(dataset){
    indexesToRemove = c()
    numOfFeatures = ncol(dataset)
    for(i in 1:numOfFeatures){
        len = length(levels(as.factor(dataset[,i])))
        if(len < 2){        
            indexesToRemove = c(indexesToRemove, i)
        }
    }
    return (indexesToRemove)
}

replaceNA = function(dataset){
    numOfFeatures = ncol(dataset)
    # toRemove = c()
    for(i in 1:numOfFeatures){
        numeric = is.numeric(dataset[,i])
        idxs = which(is.na(dataset[,i]))
        if(length(idxs) > 0){
            if(numeric){        
                dataset[idxs, i] = min(dataset[-idxs,i])
            }else{
                dataset[idxs, i] = "MISSING"
                # if(length(levels(as.factor(dataset[,i]))) > 1000){
                    # toRemove = c(toRemove, i)                    
                # }
            }
        }
    }
    # dataset = dataset[, -toRemove]
    return (dataset)
}

replaceCategorialWithNumerical = function(dataset){
    numOfFeatures = ncol(dataset)
    for(i in 1:numOfFeatures){
        numeric = is.numeric(dataset[,i])
        if(!numeric){
            dataset[, i] = as.factor(as.numeric(dataset[,i]))
        }
    }
    return (dataset)
}