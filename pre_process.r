path = 'C:/Projects/case_celpe/celpe_case'
setwd(path)
source('utils.r')

# Lendo a base de treinamento
dataset = read.table('Train.csv', header=T, sep=';', stringsAsFactors = FALSE)

# Pre processando e salvando em outro arquivo
dataset = preProcessDataset(dataset)
 
write.table(as.data.frame(dataset), file = 'processed_train.csv', sep=';', row.names = FALSE)