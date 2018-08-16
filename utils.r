removeNA = function(dataset, percent) {
    indexesToRemove = c()
    numOfFeatures = dim(dataset)[2]
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
    numOfFeatures = dim(dataset)[2]
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
    numOfFeatures = dim(dataset)[2]
    for(i in 1:numOfFeatures){
        len = length(levels(as.factor(dataset[,i])))
        if(len < 2){        
            indexesToRemove = c(indexesToRemove, i)
        }
    }
    return (indexesToRemove)
}

replaceNA = function(dataset){
    numOfFeatures = dim(dataset)[2]
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
    return (dataset)
}

replaceCategorialWithNumerical = function(dataset){
    numOfFeatures = dim(dataset)[2]
    for(i in 1:numOfFeatures){
        numeric = is.numeric(dataset[,i])
        if(!numeric){
            dataset[, i] = as.numeric(as.factor(dataset[,i]))
        }
    }
    return (dataset)
}

overBalanceDataset = function(dataset){
    target = dataset[, 'TARGET']

    class1 = table(target)[1]
    class2 = table(target)[2]
    idxs = sample(which(target == 1), class1 - class2, replace = TRUE)
    dataset = rbind(dataset, dataset[idxs,])
    target = c(target, target[idxs])
    return (dataset)
}

sampleDataset = function(dataset, trainPercent){
    numOfRows = dim(dataset)[1]
    idxs = sample(1:numOfRows, trainPercent * numOfRows)
    train = dataset[idxs,]
    test = dataset[-idxs,]
    return (list(train, test))
}