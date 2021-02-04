rm(list = ls())
libraries <- c('glmnet','leaps','caret','e1071','reshape')
lapply(libraries, require, character.only = TRUE)

setwd('/Users/sabnig/Documents/Projects/Aging/Temp')

frailty_parameters <- c('Alopecia','Loss.of.fur.colour','Dermatitis','Loss.of.whiskers','Coat.condition',
	'Piloerection','Cataracts','Eye.discharge.swelling','Microphthalmia','Corneal.opacity','Nasal.discharge',
	'Rectal.prolapse','Vaginal.uterine.','Diarrhea','Vestibular.disturbance','Vision.loss..Visual.Placing.',
	'Menace.reflex','Tail.stiffening','Gait.disorders','Tremor','Tumours','Distended.abdomen','Kyphosis',
	'Body.condition','Breathing.rate.depth','Malocclusions','Righting.Reflex')
avg_gait_measures_linear <- c('avg_angular_velocity','avg_base_tail_lateral_displacement',
	'avg_limb_duty_factor','avg_nose_lateral_displacement','avg_speed_cm_per_sec',
	'avg_step_length1','avg_step_length2','avg_step_width','avg_stride_length','avg_temporal_symmetry',
	'avg_tip_tail_lateral_displacement')
avg_gait_measures_circular <- c('avg_base_tail_lateral_displacement_phase','avg_nose_lateral_displacement_phase',
'avg_tip_tail_lateral_displacement_phase')
median_gait_measures_linear <- c('median_angular_velocity','median_base_tail_lateral_displacement',
	'median_limb_duty_factor','median_nose_lateral_displacement','median_speed_cm_per_sec',
	'median_step_length1','median_step_length2','median_step_width','median_stride_length','median_temporal_symmetry',
	'median_tip_tail_lateral_displacement')
var_gait_measures_linear <- c('angular_velocity_var','base_tail_lateral_displacement_var',
	'limb_duty_factor_var','nose_lateral_displacement_var','speed_cm_per_sec_var',
	'step_length1_var','step_length2_var','step_width_var','stride_length_var',
	'tip_tail_lateral_displacement_var')
std_gait_measures_linear <- c('angular_velocity_std','base_tail_lateral_displacement_std',
	'limb_duty_factor_std','nose_lateral_displacement_std','speed_cm_per_sec_std',
	'step_length1_std','step_length2_std','step_width_std','stride_length_std',
	'tip_tail_lateral_displacement_std')
iqr_gait_measures_linear <- c('angular_velocity_iqr','base_tail_lateral_displacement_iqr',
	'limb_duty_factor_iqr','nose_lateral_displacement_iqr','speed_cm_per_sec_iqr',
	'step_length1_iqr','step_length2_iqr','step_width_iqr','stride_length_iqr',
	'tip_tail_lateral_displacement_iqr')
median_gait_measures_circular <- c('median_base_tail_lateral_displacement_phase','median_nose_lateral_displacement_phase',
'median_tip_tail_lateral_displacement_phase')
OFA_measures <- c('stride_count','Distance.cm.sc','center_time_secs','periphery_time_secs','corner_time_secs',
	'center_distance_cm','periphery_distance_cm','corner_distance_cm','grooming_number_bouts',
	'grooming_duration_secs')
engineered_features_mean <- c('dAC_mean','dB_mean','aABC_mean')
engineered_features_stdev <- c('dAC_stdev','dB_stdev','aABC_stdev')
engineered_features_min <- c('dAC_min','dB_min','aABC_min')
engineered_features_max <- c('dAC_max','dB_max','aABC_max')
animal_features <- c('Sex','Weight') #TestAge, Weight were removed

aging <- read.csv("~/Documents/Projects/Aging/Data/completeagedb6.csv", header=TRUE, stringsAsFactors = FALSE) 
ellipsefit <- read.csv("~/Documents/Projects/Aging/Data/ellipsefit_all.csv", header=TRUE, stringsAsFactors = FALSE)
rearpaw <- read.csv("~/Documents/Projects/Aging/Data/rearpaw.csv", header=TRUE, stringsAsFactors = FALSE)
var_agingall <- read.csv("~/Documents/Projects/Aging/Data/var_agingall.csv", header=TRUE, stringsAsFactors = FALSE)

aging <- cbind(aging, ellipsefit[1:nrow(aging),],rearpaw[1:nrow(aging),])
aging <- cbind(aging, var_agingall)
names(aging)[names(aging) == 'Overall.Score'] <- 'score'
names(aging)[names(aging) == 'Age.at.Test'] <- 'TestAge'
names(aging)[names(aging) == 'Body.Weight'] <- 'Weight'
names(aging)[names(aging) == 'Collected.By'] <- 'Tester'
aging$Tester <- factor(aging$Tester, levels = c('Amanda','Gaven','Hannah','Mackenzie'))
aging$Sex <- factor(aging$Sex, levels = c('-1','1'))
#levels(aging$Sex) <- c('Male','Female')

df <- aging[,names(aging) %in% c('score',animal_features,
	OFA_measures,engineered_features_mean, avg_gait_measures_linear, median_gait_measures_linear,
	std_gait_measures_linear, var_gait_measures_linear,iqr_gait_measures_linear,
	engineered_features_stdev)]
#df <- aging[,names(aging) %in% c('score',animal_features,avg_gait_measures_linear, median_gait_measures_linear)]

#EXPLORATORY MODELING
#Some plots
pp_df <- preProcess(df[,-which(names(df) %in% c('score','Sex','Tester'))], method = c('center','scale'))
dfX <- predict(pp_df, newdata = df[,-which(names(df) %in% c('score','Sex','Tester'))])
dfX <- cbind(id = 1:dim(dfX)[1], dfX)
dfX.melt <- melt(dfX, id.vars = 'id')
ggplot(dfX.melt, aes(x = value)) + geom_density() + facet_wrap(~ variable)
ggsave('../Plots/densityPlot.pdf')

pp_df <- preProcess(df[,-which(names(df) %in% c('score','Sex','Tester'))])
dfX <- predict(pp_df, newdata = df[,-which(names(df) %in% c('score','Sex','Tester'))])
dfX <- cbind(id = 1:dim(dfX)[1], dfX)
dfX.melt <- melt(dfX, id.vars = 'id')
dfX.melt <- cbind(dfX.melt, score = rep(df$score, dim(dfX)[2]-1), Sex = rep(df$Sex, dim(dfX)[2]-1))
ggplot(dfX.melt, aes(x = value, y = score)) + geom_point(alpha = 0.6, aes(color = as.factor(Sex))) + geom_smooth(method = 'loess', aes(color = 'black'), se = FALSE) + 
facet_wrap(~ variable, scales = 'free') + theme(legend.position = 'none') + scale_color_manual(values = c('#f16913','#2b8cbe','#000000'))
ggsave('../Plots/featurePlot2.pdf')

corrplot::corrplot(cor(df[,-which(names(df) %in% c('Sex','score','Tester'))]), order = "hclust", 
	tl.col = 'black',type = 'upper', tl.cex = 0.7)
dev.print(pdf,'../Plots/corrplot.pdf')

corr.mat <- cor(df[,-which(names(df) %in% c('score','Sex','Tester'))])[upper.tri(cor(df[,-which(names(df) %in% c('score','Sex','Tester'))]), diag = FALSE)]
hist(corr.mat, xlab = 'Correlation', main = 'Summary of Correlations' )
dev.print(pdf,'../Plots/corr-summ.pdf')

corr.mat <- cor(df[,sapply(df,is.numeric)])
summary(corr.mat[upper.tri(corr.mat,diag = FALSE)])

corr.mat <- cor(df[,-which(names(df) %in% c('score','Sex','Tester'))])


#PCA 
pp_df <- preProcess(df[,-which(names(df) %in% c('score','Sex','Tester'))], method = c('center','scale'))
df.X <- predict(pp_df, newdata = df[,-which(names(df) %in% c('score','Sex','Tester'))])
X.cov <- cov(df.X[,sapply(df.X, is.numeric)])
X.eigen <- eigen(X.cov)
plot(X.eigen$values/sum(X.eigen$values), xlab = 'Number of PCs', ylab = '% Variance Explained', main = 'Scree plot')
lines(X.eigen$values/sum(X.eigen$values))
dev.print(pdf,'../Plots/scree-plot.pdf')

#featurePlot(x = df[,-which(names(df) %in% c('score','Tester','Sex'))], y = df$score, grid = TRUE, 
#	type = c('p','smooth'), col.line = 'red', lwd = 3, alpha = 0.6, col = 'black', pch = 16)


#PREDICTIVE MODELING
#Repeated 10-fold CV
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10, savePredictions = 'final')

#Data preprocess
set.seed(100)
trainIndex <- createDataPartition(df$score, p = 0.8, times = 1, list = FALSE)
dfTrain <- df[trainIndex,-which(names(df) %in% c('Sex'))]
names(dfTrain) <- colnames(dfTrain)
dfTrainY <- dfTrain$score
dfTrainX <- dfTrain[,-which(names(dfTrain) %in% c('score'))]
#dfTrainXcat <- model.matrix(score ~ Sex, data = dfTrain)
#dfTrainX <- cbind(dfTrainXnum,dfTrainXcat)
dfTest <- df[-trainIndex,]
names(dfTest) <- colnames(dfTest)
dfTestX <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
dfTestY <- dfTest$score

#Simple Linear Model
#dfTrainXfilt <- dfTrainX[,-findCorrelation(corr.mat, cutoff = 0.75)]
#names(df)[names(df) %in% -which(names(df) %in% c('score','Sex','Tester'))][findCorrelation(corr.mat, cutoff = 0.75]
set.seed(4)
linearFit <- train(dfTrainX,dfTrainY, method = 'lm', trControl = fitControl, 
  preProc = c('center','scale'))
set.seed(4)
linearFit.rlm <- train(dfTrainX,dfTrainY, method = 'rlm', trControl = fitControl, 
  preProc = c('center','scale'))
boost_Grid <- expand.grid(.mstop = seq(50,300,50), .nu = c(0.1,0.2,0.3))
set.seed(4)
linearFit.boosted <- train(dfTrainX,dfTrainY, method = 'BstLm', trControl = fitControl, 
	preProc = c('center','scale'), tuneGrid = boost_Grid)
set.seed(4)
linearFit.cubist <- train(dfTrainX,dfTrainY, method = 'cubist', trControl = fitControl, 
  preProc = c('center','scale'))
ridgeGrid <- expand.grid(lambda = seq(0.1,0.5, length = 20)) 
set.seed(4)
linearFit.ridge <- train(dfTrainX, dfTrainY, method = 'ridge', trControl = fitControl, 
  preProc = c('center','scale'), tuneGrid = ridgeGrid)
#enetGrid <- expand.grid(.lambda = c(0,0.01,0.1,0.2,0.3), .fraction = seq(.05,1,length = 20)) 
glmn_grid <- expand.grid(alpha = seq(0, 1, by = .25), lambda = 10^seq(-3, -.5, length = 20))
set.seed(4)
linearFit.enet <- train(dfTrainX, dfTrainY, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))
set.seed(4)
linearFit.glm <- train(dfTrainX,dfTrainY, method = 'glmboost', trControl = fitControl, 
  preProc = c('center','scale'))

idGrid <- expand.grid(.n.comp = seq(1,10,1)) 
set.seed(4)
linearFit.IC <- train(dfTrainX,dfTrainY, method = 'icr', trControl = fitControl, 
  preProc = c('center','scale'), tuneGrid = idGrid)
idGrid <- expand.grid(.ncomp = seq(1,10,1)) 
set.seed(4)
linearFit.pcr <- train(dfTrainX,dfTrainY, method = 'pcr', trControl = fitControl, 
  preProc = c('center','scale'), tuneGrid = idGrid)
set.seed(4)
linearFit.pls <- train(dfTrainX, dfTrainY, method = 'pls', tuneLength = 20, trControl = fitControl,
  preProc = c('center','scale'))

set.seed(4)
nonlinearFit.mars <- train(dfTrainX,dfTrainY, method = 'earth', trControl = fitControl, 
  preProc = c('center','scale'))
set.seed(4)
nonlinearFit.gam <- train(dfTrainX,dfTrainY, method = 'gamboost', trControl = fitControl, 
  preProc = c('center','scale'))
set.seed(4)
nonlinearFit.svm <- train(dfTrainX, dfTrainY, method = 'svmRadial', preProc = c('center','scale'), tuneLength = 14, 
  trControl = fitControl)

rfGrid <- expand.grid(mtry = seq(2,ncol(dfTrainX), by = 10), min.node.size = seq(3,9,by = 2), 
	splitrule = 'variance')
set.seed(4)
nonlinearFit.rf <- train(score ~ ., data = dfTrain, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid)

gbmGrid <- expand.grid(interaction.depth = seq(1,11,by = 2), n.trees = seq(100,1000,by = 50), 
  shrinkage = c(0.01,0.02),n.minobsinnode = c(5,7,10))
set.seed(4)
nonlinearFit.gbm <- train(dfTrainX, dfTrainY, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, trControl = fitControl)


modelList <- list(LM = linearFit, RLM = linearFit.rlm, LMB = linearFit.boosted, 
	LMnet = linearFit.enet, PCR = linearFit.pcr, PLS = linearFit.pls, SVM = nonlinearFit.svm,
	RF = nonlinearFit.rf, GBM = nonlinearFit.gbm)
resamps <- resamples(modelList)

finalResults <- resamps$values
finalResults <- finalResults[,-1]

finalResultsX <- cbind(id = 1:dim(finalResults)[1], finalResults)
finalResultsX.melt <- melt(finalResultsX, id.vars = 'id')
finalResultsX.melt <- cbind(finalResultsX.melt, metric = matrix(replicate(ncol(finalResults)/3,c(rep('MAE',100), rep('RMSE',100), 
	rep('Rsquared',100))), ncol = 1), 
	model = c(rep(names(modelList)[1],300), rep(names(modelList)[2],300),rep(names(modelList)[3],300),
		rep(names(modelList)[4],300),rep(names(modelList)[5],300),rep(names(modelList)[6],300),
		rep(names(modelList)[7],300), rep(names(modelList)[8],300), rep(names(modelList)[9],300)))
ggplot(finalResultsX.melt, aes(x = model, y = value)) + geom_boxplot(alpha = 0.1) + 
facet_wrap(~ metric, scales = 'free') + coord_flip() + labs(y = 'Repeated 10-fold CV Error', x= 'Model') + 
theme_bw(base_size = 22)
ggsave('Reg-train-Res.pdf', width = 18, height = 6)


testResults <- data.frame(obs = dfTestY)
testResults$LM <- predict(linearFit, dfTestX)
testResults$RLM <- predict(linearFit.rlm, dfTestX)
testResults$LMB <- predict(linearFit.boosted, dfTestX)
testResults$ElasticNet <- predict(linearFit.enet, dfTestX)
testResults$PCR <- predict(linearFit.pcr, dfTestX)
testResults$PLS <- predict(linearFit.pls, dfTestX)
testResults$SVM <- predict(nonlinearFit.svm, dfTestX)
testResults$RandomForest <- predict(nonlinearFit.rf, dfTestX)
testResults$GBM <- predict(nonlinearFit.gbm, dfTestX)


Results.Test <- data.frame(Model = c('LM','RLM','LMB','LMnet','PCR','PLS','SVM','RF','GBM'),
	RMSE = c(RMSE(dfTestY, testResults$LM), RMSE(dfTestY, testResults$RLM),RMSE(dfTestY, testResults$LMB),RMSE(dfTestY, testResults$ElasticNet),
		RMSE(dfTestY, testResults$PCR),RMSE(dfTestY, testResults$PLS), RMSE(dfTestY, testResults$SVM), RMSE(dfTestY, testResults$RandomForest),
		RMSE(dfTestY, testResults$GBM)), 
	Rsquared = c(R2(dfTestY, testResults$LM), R2(dfTestY, testResults$RLM),R2(dfTestY, testResults$LMB),R2(dfTestY, testResults$ElasticNet),
		R2(dfTestY, testResults$PCR),R2(dfTestY, testResults$PLS), R2(dfTestY, testResults$SVM), R2(dfTestY, testResults$RandomForest),
		R2(dfTestY, testResults$GBM)),
	Correlation = c(cor(dfTestY, testResults$LM), cor(dfTestY, testResults$RLM),cor(dfTestY, testResults$LMB),cor(dfTestY, testResults$ElasticNet),
		cor(dfTestY, testResults$PCR),cor(dfTestY, testResults$PLS), cor(dfTestY, testResults$SVM), cor(dfTestY, testResults$RandomForest),
		cor(dfTestY, testResults$GBM)))



p1 <- ggplot(Results.Test, aes(x = Model, y = RMSE, color = Model)) + geom_point(size = 11) + theme_bw(base_size = 24) + coord_flip() + theme(legend.position = 'none') 
p2 <- ggplot(Results.Test, aes(x = Model, y = Rsquared, color = Model)) + geom_point(size = 11) + theme_bw(base_size = 24) + coord_flip() + theme(legend.position = 'none')
p3 <- ggplot(Results.Test, aes(x = Model, y = Correlation, color = Model)) + geom_point(size = 11) + coord_flip() + theme_bw(base_size = 24) + theme(legend.position = 'none')
plot_grid(p1,p2,p3,nrow = 1, align = 'h', labels = c('A','B','C'))
ggsave('Reg-test-Res.pdf', width = 25.1, height = 7)




tmp.df <- data.frame(pred = predict(linearFit, newdata = dfTest), obs = dfTest$score, Sex = dfTest$Sex)

ggplot(tmp.df, aes(y = obs, x = pred, color = Sex)) + geom_point(alpha = 0.6) + 
geom_abline(intercept = 0, slope = 1) + labs(y = 'Observed', x = 'Predicted') + 
scale_color_manual(values = c('#f16913','#2b8cbe')) + scale_y_continuous(breaks = seq(2,8,2)) + 
theme(legend.position = 'none') 
ggsave('../Plots/linmod-perf.pdf')

ggplot(tmp.df, aes(y = obs - pred, x = pred, color = Sex)) + geom_point(alpha = 0.6) + 
geom_hline(yintercept = 0) + labs(y = 'Residuals', x = 'Predicted') + 
scale_color_manual(values = c('#f16913','#2b8cbe')) + theme(legend.position = 'none')
ggsave('../Plots/linmod-res.pdf')

#Partial Least Squares 
set.seed(4)
plsFit <- train(dfTrainX, dfTrainY, method = 'pls', tuneLength = 20, trControl = fitControl,
	preProc = c('center','scale'))
ggplot(plsFit)
ggsave('../Plots/plsmod-cv.pdf')

ggplot(varImp(plsFit))
ggsave('../Plots/pls-varImp.pdf')

tmp.df <- data.frame(pred = predict(plsFit, newdata = dfTest), obs = dfTest$score, Sex = dfTest$Sex)

ggplot(tmp.df, aes(y = obs, x = pred, color = Sex)) + geom_point(alpha = 0.6) + 
geom_abline(intercept = 0, slope = 1) + labs(y = 'Observed', x = 'Predicted') + 
scale_color_manual(values = c('#f16913','#2b8cbe')) + scale_y_continuous(breaks = seq(2,8,2)) + 
theme(legend.position = 'none') 
ggsave('../Plots/plsmod-perf.pdf')

ggplot(tmp.df, aes(y = obs - pred, x = pred, color = Sex)) + geom_point(alpha = 0.6) + 
geom_hline(yintercept = 0) + labs(y = 'Residuals', x = 'Predicted') + 
scale_color_manual(values = c('#f16913','#2b8cbe')) + theme(legend.position = 'none')
ggsave('../Plots/plsmod-res.pdf')

#Ridge Regression
dfTrainX <- data.frame(dfTrainX)
ridgeGrid <- expand.grid(lambda = seq(0.1,0.2, length = 20)) 
set.seed(4)
ridgeFit <- train(dfTrainX, dfTrainY, method = 'ridge', trControl = fitControl, 
	preProc = c('center','scale'), tuneGrid = ridgeGrid)

ggplot(ridgeFit)
ggsave('../Plots/plsmod-cv.pdf')

ggplot(varImp(ridgeFit))
ggsave('../Plots/ridge-varImp.pdf')

ggplot(data.frame(predict(ridgeFit, newdata = dfTest),dfTest$score), aes(y = dfTest$score, x = predict(ridgeFit, newdata = dfTest))) + 
geom_point(alpha = 0.6) + geom_abline(intercept = 0, slope = 1) + labs(x = 'Observed', y = 'Predicted')

ggplot(data.frame(predict(ridgeFit, newdata = dfTest),dfTest$score), aes(y = dfTest$score - predict(ridgeFit, newdata = dfTest), 
	x = predict(ridgeFit, newdata = dfTest))) + geom_point(alpha = 0.6) + 
geom_hline(yintercept = 0) + labs(y = 'Residuals', x = 'Predicted')

#Elastic Net Regression
enetGrid <- expand.grid(.lambda = c(0,0.01,0.1,0.2,0.3), .fraction = seq(.05,1,length = 20)) 
set.seed(4)
enetFit <- train(dfTrainX, dfTrainY, method = 'enet', tuneGrid = enetGrid, 
	trControl = fitControl, preProc = c('center','scale'))

ggplot(enetFit)
ggsave('../Plots/enet-cv.pdf')

ggplot(varImp(enetFit))
ggsave('../Plots/enet-varImp.pdf')

ggplot(data.frame(predict(ridgeFit, newdata = dfTest),dfTest$score), aes(y = dfTest$score, x = predict(ridgeFit, newdata = dfTest))) + 
geom_point(alpha = 0.6) + geom_abline(intercept = 0, slope = 1) + labs(x = 'Observed', y = 'Predicted')

ggplot(data.frame(predict(ridgeFit, newdata = dfTest),dfTest$score), aes(y = dfTest$score - predict(ridgeFit, newdata = dfTest), 
  x = predict(ridgeFit, newdata = dfTest))) + geom_point(alpha = 0.6) + 
geom_hline(yintercept = 0) + labs(y = 'Residuals', x = 'Predicted')

#MARS
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
set.seed(4)
marsFit <- train(dfTrainX, dfTrainY, method = "earth", tuneGrid = marsGrid, trControl = fitControl, 
  preProc = c('center','scale')) 


#SVM
set.seed(4)
svmFit <- train(dfTrainX, dfTrainY, method = 'svmRadial', preProc = c('center','scale'), tuneLength = 14, 
  trControl = fitControl)


#Random Forest
rfFit <- randomForest(dfTrainX, dfTrainY, importance = TRUE, ntrees = 10000)


#Gradient Boosted Trees
gbmGrid <- expand.grid(.interaction.depth = seq(1,11,by = 2), .n.trees = seq(100,1000,by = 50), 
  .shrinkage = c(0.01,0.001))
set.seed(4)
gbmFit <- train(dfTrainX, dfTrainY, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE)


#XGBoost
set.seed(316)
xgbFit <- train(dfTrainX, dfTrainY, method = 'xgbTree', trControl = fitControl)


Test <- sample(1:nrow(df),50)
Train <- setdiff(1:nrow(df), Test)
dfTrain <- df[Train,]
dfTest <- df[Test,]

dfTrainX <- dfTrain[,-which(names(dfTrain) %in% c('score'))]
dfTrainY <- dfTrain$score
pp_Train <- preProcess(dfTrain[,-which(names(dfTrain) %in% c('score'))], method = c('center','scale','BoxCox'))
dfTrainXtrans <- predict(pp_df, newdata = dfTrain[,-which(names(dfTrain) %in% c('score'))])

dfTestX <- dfTest[,-which(names(dfTest) %in% c('score'))]
dfTestY <- dfTest$score
pp_Test <- preProcess(dfTest[,-which(names(dfTest) %in% c('score'))], method = c('center','scale','BoxCox'))
dfTestXtrans <- predict(pp_df, newdata = dfTest[,-which(names(dfTrain) %in% c('score'))])

trainingData <- dfTrainX
trainingData$score <- dfTrainY

#Simple Linear Regression
lmFitAllPredictors <- lm(score ~ ., data = trainingData)
lmPred1 <- predict(lmFitAllPredictors, dfTestX)

lmValuesTest <- data.frame(obs = dfTestY, pred = lmPred1)
defaultSummary(lmValuesTest)

lmValuesTrain <- data.frame(obs = dfTrainY, pred = predict(lmFitAllPredictors, dfTrainX))
defaultSummary(lmValuesTrain)


ggplot(lmValuesTrain, aes(x = pred, y = obs)) + geom_point(alpha = 0.6) + labs(x = 'Predicted', y = 'Observed')

ctrl <- trainControl(method = 'cv', number = 10)

lmFit1 <- train(x = dfTrainX, y = dfTrainY, method = 'lm', trControl = ctrl)
lmValuesTrain <- 



df[,-which(names(df) %in% c('Sex','Tester'))] <- apply(df[,-which(names(df) %in% c('Sex','Tester'))],MARGIN = 2, FUN = function(x) (x - mean(x[!is.na(x)]))/sd(x[!is.na(x)]))





X <- data.matrix(df[,-which(names(df) %in% c('score'))]) #Column 4 contains the scores. 
y <- df['score']

df$Tester <- factor(df$Tester)


#Preliminary Model
mod1 <- lm(score ~ ., data = df)
mod2 <- lm(resid(lm(score ~ Tester)) ~ ., data = df)


indx <- createFolds(df[,'score'], returnTrain = TRUE)
ctrl <- trainControl(method = 'cv', index = indx)
lmTune0 <- train(x = df[,-which(names(df) %in% c('TestAge'))], y = df[,'score'], method = 'lm', trControl = ctrl)

testResults <- data.frame(obs = df[,'score'], Linear_Regression = predict(lmTune0, df[,-which(names(df) %in% c('TestAge'))]))



#Best Subset Selection
mod1 <- regsubsets(score ~ ., data = df)
best.summary <- summary(mod1)
plot(best.summary$adjr2, type = 'l', xlab = 'Number of variables', ylab = 'Adjusted Rsq', frame = FALSE)
axis(1,at = 1:28)
points(which.max(best.summary$adjr2), best.summary$adjr2[which.max(best.summary$adjr2)], col = 'red', cex = 2, pch = 20)
plot(mod1,scale = 'adjr2')
plot(mod1, scale = 'Cp')
plot(mod1, scale = 'bic')


#Validation Approach
df$cut <- cut(df$TestAge, breaks = c(0,50,101,134), labels =  c('Y','M','O'), right = TRUE)
#test <- stratified(df, 'cut', size = round(100*prop.table(table(df$cut)),0))
test <- stratified(df, 'cut', size = c(17,17,16))
train <- df[!test,]



#Naive approach
test <- sample(1:nrow(df),50)
train <- setdiff(1:nrow(df), test)

train.mod <- regsubsets(score ~ . , data = df[train,],nvmax = 28)

test.mat <- model.matrix(score ~ . , data = df[test,])

val.errors = rep(NA, 28)
for (i in 1:28){
	coefi = coef(train.mod, id = i)
	pred = test.mat[,names(coefi)]%*%coefi
	val.errors[i] = mean((df$score[test] - pred)^2)
}





###Checking for presence of multicollinearity
corrplot::corrplot(cor(df[,-which(names(df) %in% c('Sex','score','Tester'))]), order = "hclust", 
	tl.col = 'black',type = 'upper', tl.cex = 0.4)
dev.print(pdf,'../Plots/corrplot.pdf')

corr.mat <- cor(df[,sapply(df,is.numeric)])[upper.tri(cor(df[,sapply(df,is.numeric)]), diag = FALSE)]
hist(corr.mat, xlab = 'Correlation', main = 'Summary of Correlations' )
dev.print(pdf,'../Plots/corr-summ.pdf')

corr.mat <- cor(df[,sapply(df,is.numeric)])
summary(corr.mat[upper.tri(corr.mat,diag = FALSE)])


#PCA 
df.X <- df[,-which(names(df) %in% c('score'))]
X.cov <- cov(df.X[,sapply(df.X, is.numeric)])
X.eigen <- eigen(X.cov)
plot(X.eigen$values/sum(X.eigen$values), xlab = 'Number of PCs', ylab = '% Variance Explained', main = 'Scree plot')
lines(X.eigen$values/sum(X.eigen$values))
dev.print(pdf,'../Plots/scree-plot.pdf')




ridge.mod <- cv.glmnet(X,y, alpha = 0)




lapply(1:nrow(aging), function(i) )







stratified <- function(df, group, size, select = NULL, 
                       replace = FALSE, bothSets = FALSE) {
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)
  
  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}




