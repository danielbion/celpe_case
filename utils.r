splitDataset = function(dataset, numOfFolds){
    numOfRows = nrow(dataset)

    dataset = dataset[sample(nrow(dataset)),]
    folds = cut(seq(1,nrow(dataset)), breaks=numOfFolds, labels=FALSE)

    splitedDataset = list()

    for(i in 1:numOfFolds){        
        testIndexes = which(folds==i, arr.ind=TRUE)
        set = list()
        set$test = dataset[testIndexes, ]
        set$train = dataset[-testIndexes, ]
        set$trainLabels = set$train[, 'TARGET']
        set$testLabels = set$test[, 'TARGET']
        set$trainNoLabels = set$train[, -ncol(set$train)]
        set$testNoLabels = set$test[, -ncol(set$test)]
        splitedDataset[[i]] = set
    }

    return (splitedDataset)
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