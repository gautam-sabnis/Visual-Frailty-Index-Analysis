rm(list = ls())
libraries <- c('glmnet','leaps','caret','e1071','reshape')
lapply(libraries, require, character.only = TRUE)

setwd("/Users/sabnig/Documents/Projects/Aging/Sumner")

df <- read.csv('aging.csv', header = TRUE, stringsAsFactors = FALSE)
df <- abs(df)
df <- cbind(id = 1:nrow(df), df)
df.melt <- melt(df,id.vars = 'id')
df.melt <- df.melt[-1]
ggplot(df.melt, aes(y = value, x = variable)) + geom_boxplot()


df <- read.csv('aging.csv', header = TRUE, stringsAsFactors = FALSE)
df <- abs(df)
df2 <- data.frame(Method = c('Enet','GBM','RF','SVM'), MAE = apply(df,2,mean), MAESD = apply(df,2,sd))
ggplot(df2,aes(x = Method, y = MAE)) + geom_bar(stat = 'identity', color = 'black', position = 
	position_dodge()) + geom_errorbar(aes(ymin = MAE - MAESD, ymax = MAE + MAESD), width=.2,position=position_dodge(.9))

