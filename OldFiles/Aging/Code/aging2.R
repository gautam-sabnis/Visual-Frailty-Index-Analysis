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
#df <- df[(df$score <= 4) | (df$score > 8),]
#df$score <- as.factor(ifelse(df$score <= 4, 'Class1','Class2'))
df$score <- as.factor(ifelse(df$score <= 4, 0,1))
dfX <- df[, -which(names(df) %in% c('score','Sex'))]


#Preliminary Plots
dfX <- cbind(id = 1:dim(dfX)[1], dfX)
dfX.melt <- reshape::melt(dfX, id.vars = 'id')
dfX.melt <- cbind(dfX.melt, score = rep(df$score, dim(dfX)[2]-1), Sex = rep(df$Sex, dim(dfX)[2]-1))
ggplot(dfX.melt, aes(y = score, x = value)) + geom_point(alpha = 0.8,aes(color = Sex)) + 
facet_wrap(~ variable, scales = 'free') + stat_smooth(method='loess',color='black') + 
scale_color_manual(values = c('#ffc100','#006291')) + labs(y = NULL, x = NULL) + theme_bw(base_size = 15) + 
theme(legend.position='none')
ggsave('../Plots/class-featurePlot.pdf', height = 11.8, width = 21.4)

dfX.tmp <- dfX[,names(dfX) %in% c('id','Weight','grooming_number_bouts','avg_step_width','dB_mean')]
dfX.melt <- melt(dfX.tmp, id.vars = 'id')
dfX.melt <- cbind(dfX.melt, score = rep(df$score, dim(dfX.tmp)[2]-1), Sex = rep(df$Sex, dim(dfX.tmp)[2]-1))
A <- ggplot(dfX.melt, aes(x = score, y = value, fill = score)) + geom_boxplot(alpha = 0.8) + 
facet_wrap(~ variable, scales = 'free') + 
scale_fill_manual(values = c('#ffc100','#006291')) + labs(y = NULL, x = NULL) + theme_bw(base_size = 15) + 
theme(legend.position = 'none')
ggsave('illust.pdf', width = 6.0, height = 6.0)




corr.mat <- cor(df[,-which(names(df) %in% c('score','Sex'))])[upper.tri(cor(df[,-which(names(df) %in% c('score','Sex'))]), diag = FALSE)]
A <- ggplot(data.frame(corr.mat), aes(x = corr.mat)) + geom_histogram(bins = 10, color = 'black') + labs(x = 'Correlation', y =' Frequency') + 
ggtitle('Summary of Correlations')


#Defining a cost function

#PREDICTIVE MODELING
#Repeated 10-fold CV
fitControl <- trainControl(method = 'repeatedcv', repeats = 10,summaryFunction = twoClassSummary,
	classProbs = TRUE,savePredictions = TRUE)


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

set.seed(4)
linearFit.glmnet <- train(dfTrainX, dfTrainY, method = 'glmnet', metric = 'ROC',trControl = fitControl,preProc = c("center","scale"))
log.roc <- roc(response = linearFit.Log$pred$obs, predictor = linearFit.Log$pred$Class1, levels = rev(levels(linearFit.Log$pred$obs)))
linearFit.Boosted <- train(dfTrainX, dfTrainY, method = 'LogitBoost', metric = 'ROC',trControl = fitControl,preProc = c("center","scale"))


#Discriminant Analysis
set.seed(4)
linearFit.LDA <- train(dfTrainX, dfTrainY, method = 'lda2', metric = 'ROC',
	trControl = fitControl,preProc = c("center","scale"), tuneGrid = expand.grid(dimen = c(1,2)))
set.seed(4)
linearFit.MDA <- train(dfTrainX, dfTrainY, method = 'mda', metric = 'ROC',
	trControl = fitControl,preProc = c("center","scale"), tuneGrid = expand.grid(subclasses = seq(1,10,1)))
set.seed(4)
linearFit.PDA <- train(dfTrainX, dfTrainY, method = 'pda', metric = 'ROC',
	trControl = fitControl,preProc = c("center","scale"),tuneGrid = expand.grid(lambda = seq(5,10,0.1)))
set.seed(4)
linearFit.QDA <- train(dfTrainX, dfTrainY, method = 'qda', metric = 'ROC',
	trControl = fitControl,preProc = c("center","scale"))
set.seed(4)
linearFit.sparseLDA <- train(dfTrainX, dfTrainY, method = 'sparseLDA', metric = 'ROC',
	trControl = fitControl,preProc = c("center","scale"), tuneGrid = expand.grid(NumVars = seq(1,10,1), lambda = seq(0,1,0.1)))

lda.roc <- roc(response = linearFit.LDA$pred$obs, predictor = linearFit.LDA$pred$Class1, levels = rev(levels(linearFit.LDA$pred$obs)))
plot(log.roc, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = TRUE)
plot(lda.roc, add = TRUE, type = "s", legacy.axes = TRUE)

modelList.LDA <- list(LDA = linearFit.LDA, MDA = linearFit.MDA, PDA = linearFit.PDA, QDA = linearFit.QDA, SLDA = 
	linearFit.sparseLDA)
resamps.LDA <- resamples(modelList.LDA)

#Partial Least Squares 
plsGrid <- expand.grid(ncomp = 1:10)
set.seed(4)
linearFit.pls <- train(dfTrainX, dfTrainY, method = 'pls', tuneGrid = plsGrid, metric = 'ROC', trControl = fitControl, preProc = c("center","scale")) 
#linearFit.spls <- train(dfTrainX, dfTrainY, method = 'spls',metric = 'ROC', trControl = fitControl, preProc = c("center","scale")) 


glmnGrid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),lambda = seq(.01, .2, length = 40))
set.seed(4)
linearFit.glmnet <- train(dfTrainX, dfTrainY, method = 'glmnet',tuneGrid = glmnGrid, preProc = c("center", "scale"),
	metric = "ROC",trControl = fitControl)
glmnet.roc <- roc(response = linearFit.glmnet$pred$obs, predictor = linearFit.glmnet$pred$Class1, levels = rev(levels(linearFit.glmnet$pred$obs)))

#ldaGrid <- expand.grid(lambda = c(.1),NumVars = c(1:20, 50, 75, 100, 250, 500, 750, 1000))
#set.seed(4)
#linearFit.sparselda <- train(dfTrainX, dfTrainY, method = 'sparseLDA', tuneGrid = ldaGrid, preProc = c("center","scale"),
#	metric = 'ROC', trControl = fitControl)
#sparselda.roc <- roc(response = linearFit.sparselda$pred$obs, predictor = linearFit.sparselda$pred$Class1, levels = rev(levels(linearFit.sparselda$pred$obs)))

pamGrid <- data.frame(threshold = seq(0, 25, length = 30))
set.seed(4)
linearFit.pam <- train(dfTrainX, dfTrainY, method = 'pam', tuneGrid = pamGrid, preProc = c('center','scale'),
	metric = 'ROC', trControl = fitControl)
pam.roc <- roc(response = linearFit.pam$pred$obs, predictor = linearFit.pam$pred$Class1, levels = rev(levels(linearFit.pam$pred$obs)))

set.seed(4)
nonlinearFit.mda <- train(dfTrainX, dfTrainY, method = 'mda', preProc = c('center','scale'), metric = 'ROC', 
	trControl = fitControl, tuneGrid = expand.grid(.subclasses =  1:8), tries = 40)
mda.roc <- roc(response = nonlinearFit.mda$pred$obs, predictor = nonlinearFit.mda$pred$Class1, levels = rev(levels(nonlinearFit.mda$pred$obs)))

set.seed(4)
sigmaRangeFull <- kernlab::sigest(as.matrix(dfTrainX))
svmGrid <- expand.grid(sigma = as.vector(sigmaRangeFull)[1], C = 2^(-3:4))
set.seed(4)
nonlinearFit.svm <- train(dfTrainX, dfTrainY, method = 'svmRadial', metric = 'ROC', preProc = c('center','scale'),
tuneGrid = svmGrid, trControl = fitControl)
svm.roc <- roc(response = nonlinearFit.svm$pred$obs, predictor = nonlinearFit.svm$pred$Class1, levels = rev(levels(nonlinearFit.svm$pred$obs)))

svmPGrid <- expand.grid(degree = 1:2, scale = c(0.01,0.005), C = 2^(seq(-6,-2,length = 10)))
set.seed(4)
nonlinearFit.svmP <- train(dfTrainX, dfTrainY, method = 'svmPoly', metric = 'ROC', preProc = c('center', 'scale'),
	tuneGrid = svmPGrid, trControl = fitControl)  
svmP.roc <- roc(response = nonlinearFit.svmP$pred$obs, predictor = nonlinearFit.svmP$pred$Class1, levels = rev(levels(nonlinearFit.svmP$pred$obs)))

set.seed(4)
nonlinearFit.nBayes <- train(dfTrainX, dfTrainY, method = 'nb', metric = 'ROC', trControl = fitControl)
nBayes.roc <- roc(response = nonlinearFit.nBayes$pred$obs, predictor = nonlinearFit.nBayes$pred$Class1, levels = rev(levels(nonlinearFit.nBayes$pred$obs)))


modelList <- list(SparseLDA = linearFit.sparseLDA, PLS = linearFit.pls, Enet = linearFit.glmnet,
	SVM = nonlinearFit.svmP, RandomForest = nonlinearFit.rf, GBM = nonlinearFit.gbm)
resamps <- resamples(modelList)

dfTrain$scorenew <- ifelse(dfTrain$score == 'Class1', 0, 1)
gbm.fit <- gbm(scorenew ~ ., distribution = 'bernoulli', data = dfTrain, cv.folds = 5)

rfGrid <- expand.grid(mtry = seq(2,ncol(dfTrainX), by = 10), min.node.size = seq(3,9,by = 2), 
	splitrule = 'gini')
set.seed(4)
nonlinearFit.rf <- train(score ~ ., data = dfTrain, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid)

gbmGrid <- expand.grid(interaction.depth = seq(1,11,by = 2), n.trees = seq(100,1000,by = 100), 
  shrinkage = c(0.01,0.02),n.minobsinnode = c(5,7,10))
set.seed(4)
nonlinearFit.gbm <- train(dfTrainX, dfTrainY, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, trControl = fitControl)

modelList <- list(SparseLDA = linearFit.sparseLDA, PLS = linearFit.pls, Enet = linearFit.glmnet,
	SVM = nonlinearFit.svmP, RandomForest = nonlinearFit.rf, GBM = nonlinearFit.gbm)
resamps <- resamples(modelList)


modelList <- list(LDA = linearFit.LDA, MDA = linearFit.MDA, QDA = linearFit.QDA, 
	SparseLDA = linearFit.sparseLDA, ElasticNet = linearFit.glmnet, PLS = linearFit.pls, SVM = nonlinearFit.svm, SVMP = nonlinearFit.svmP,
	RandomForest = nonlinearFit.rf, GBM = nonlinearFit.gbm)
resamps <- resamples(modelList)

finalResults <- resamps$values
finalResults <- finalResults[,-1]

finalResultsX <- cbind(id = 1:dim(finalResults)[1], finalResults)
finalResultsX.melt <- melt(finalResultsX, id.vars = 'id')
finalResultsX.melt <- cbind(finalResultsX.melt, metric = matrix(replicate(ncol(finalResults)/3,c(rep('ROC',100), rep('Specificity',100), 
	rep('Sensitivity',100))), ncol = 1), 
	model = c(rep(names(modelList)[1],300), rep(names(modelList)[2],300),rep(names(modelList)[3],300),
		rep(names(modelList)[4],300),rep(names(modelList)[5],300),rep(names(modelList)[6],300),
		rep(names(modelList)[7],300), rep(names(modelList)[8],300), rep(names(modelList)[9],300), 
		rep(names(modelList)[10],300)))
ggplot(finalResultsX.melt, aes(x = model, y = value)) + geom_boxplot(alpha = 0.1) + 
facet_wrap(~ metric, scales = 'free') + coord_flip() + labs(y = 'Repeated 10-fold CV Error', x= 'Model') + 
theme_bw(base_size = 22)
ggsave('train-Res.pdf', width = 18, height = 6)

testResults <- data.frame(obs = dfTestY)
testResults$LDA <- predict(linearFit.LDA, dfTestX)
testResults$QDA <- predict(linearFit.QDA, dfTestX)
testResults$SparseLDA <- predict(linearFit.sparseLDA, dfTestX)
testResults$ElasticNet <- predict(linearFit.glmnet, dfTestX)
testResults$PLS <- predict(linearFit.pls, dfTestX)
testResults$SVM <- predict(nonlinearFit.svm, dfTestX)
testResults$SVMP <- predict(nonlinearFit.svmP, dfTestX)
testResults$RandomForest <- predict(nonlinearFit.rf, dfTestX)
testResults$GBM <- predict(nonlinearFit.gbm, dfTestX)


testResults.LDA <- cbind(testResults, predict(linearFit.LDA, dfTestX, type = 'prob'))
testResults.LDA <- cbind(testResults.LDA, pred = ifelse(testResults.LDA$Class1 > 0.5, 'Class1', 'Class2'))
twoClassSummary(testResults.LDA, lev = levels(testResults.LDA$obs))

testResults.MDA <- cbind(testResults, predict(linearFit.MDA, dfTestX, type = 'prob'))
testResults.MDA <- cbind(testResults.MDA, pred = ifelse(testResults.MDA$Class1 > 0.5, 'Class1', 'Class2'))
twoClassSummary(testResults.MDA, lev = levels(testResults.MDA$obs))

testResults.QDA <- cbind(testResults, predict(linearFit.QDA, dfTestX, type = 'prob'))
testResults.QDA <- cbind(testResults.QDA, pred = ifelse(testResults.QDA$Class1 > 0.5, 'Class1', 'Class2'))
twoClassSummary(testResults.QDA, lev = levels(testResults.QDA$obs))

testResults.sparseLDA <- cbind(testResults, predict(linearFit.sparseLDA, dfTestX, type = 'prob'))
testResults.sparseLDA <- cbind(testResults.sparseLDA, pred = ifelse(testResults.sparseLDA$Class1 > 0.5, 'Class1', 'Class2'))
twoClassSummary(testResults.sparseLDA, lev = levels(testResults.sparseLDA$obs))

testResults.glmnet <- cbind(testResults, predict(linearFit.glmnet, dfTestX, type = 'prob'))
testResults.glmnet <- cbind(testResults.glmnet, pred = ifelse(testResults.glmnet$Class1 > 0.5, 'Class1', 'Class2'))
twoClassSummary(testResults.glmnet, lev = levels(testResults.glmnet$obs))
 
testResults.pls <- cbind(testResults, predict(linearFit.pls, dfTestX, type = 'prob'))
testResults.pls <- cbind(testResults.pls, pred = ifelse(testResults.pls$Class1 > 0.5, 'Class1', 'Class2'))
twoClassSummary(testResults.pls, lev = levels(testResults.pls$obs))

testResults.svm <- cbind(testResults, predict(nonlinearFit.svm, dfTestX, type = 'prob'))
testResults.svm <- cbind(testResults.svm, pred = ifelse(testResults.svm$Class1 > 0.5, 'Class1', 'Class2'))
twoClassSummary(testResults.svm, lev = levels(testResults.svm$obs))

testResults.svmP <- cbind(testResults, predict(nonlinearFit.svmP, dfTestX, type = 'prob'))
testResults.svmP <- cbind(testResults.svmP, pred = ifelse(testResults.svmP$Class1 > 0.5, 'Class1', 'Class2'))
twoClassSummary(testResults.svmP, lev = levels(testResults.svmP$obs))

testResults.rf <- cbind(testResults, predict(nonlinearFit.rf, dfTestX, type = 'prob'))
testResults.rf <- cbind(testResults.rf, pred = ifelse(testResults.rf$Class1 > 0.5, 'Class1', 'Class2'))
twoClassSummary(testResults.rf, lev = levels(testResults.rf$obs))

testResults.gbm <- cbind(testResults, predict(nonlinearFit.gbm, dfTestX, type = 'prob'))
testResults.gbm <- cbind(testResults.gbm, pred = ifelse(testResults.gbm$Class1 > 0.5, 'Class1', 'Class2'))
twoClassSummary(testResults.gbm, lev = levels(testResults.gbm$obs))

Results.Test <- data.frame(Model = c('LDA','MDA','QDA','S-LDA','PLS','LR','SVM','SVM-P','GBM',
	'RF'),
	ROC = c(unname(twoClassSummary(testResults.LDA, lev = levels(testResults.LDA$obs))['ROC']),
	unname(twoClassSummary(testResults.MDA, lev = levels(testResults.MDA$obs))['ROC']),
	unname(twoClassSummary(testResults.QDA, lev = levels(testResults.QDA$obs))['ROC']),
	unname(twoClassSummary(testResults.sparseLDA, lev = levels(testResults.sparseLDA$obs))['ROC']),
	unname(twoClassSummary(testResults.pls, lev = levels(testResults.pls$obs))['ROC']),
	unname(twoClassSummary(testResults.glmnet, lev = levels(testResults.glmnet$obs))['ROC']),
	unname(twoClassSummary(testResults.svm, lev = levels(testResults.svm$obs))['ROC']),
	unname(twoClassSummary(testResults.svmP, lev = levels(testResults.svmP$obs))['ROC']), 
	unname(twoClassSummary(testResults.gbm, lev = levels(testResults.gbm$obs))['ROC']),
	unname(twoClassSummary(testResults.rf, lev = levels(testResults.rf$obs))['ROC'])),
	Sensitivity = c(unname(twoClassSummary(testResults.LDA, lev = levels(testResults.LDA$obs))['Sens']),
	unname(twoClassSummary(testResults.MDA, lev = levels(testResults.MDA$obs))['Sens']),
	unname(twoClassSummary(testResults.QDA, lev = levels(testResults.QDA$obs))['Sens']),
	unname(twoClassSummary(testResults.sparseLDA, lev = levels(testResults.sparseLDA$obs))['Sens']),
	unname(twoClassSummary(testResults.pls, lev = levels(testResults.pls$obs))['Sens']),
	unname(twoClassSummary(testResults.glmnet, lev = levels(testResults.glmnet$obs))['Sens']),
	unname(twoClassSummary(testResults.svm, lev = levels(testResults.svm$obs))['Sens']),
	unname(twoClassSummary(testResults.svmP, lev = levels(testResults.svmP$obs))['Sens']), 
	unname(twoClassSummary(testResults.gbm, lev = levels(testResults.gbm$obs))['Sens']),
	unname(twoClassSummary(testResults.rf, lev = levels(testResults.rf$obs))['Sens'])),
	Specificity = c(unname(twoClassSummary(testResults.LDA, lev = levels(testResults.LDA$obs))['Spec']),
	unname(twoClassSummary(testResults.MDA, lev = levels(testResults.MDA$obs))['Spec']),
	unname(twoClassSummary(testResults.QDA, lev = levels(testResults.QDA$obs))['Spec']),
	unname(twoClassSummary(testResults.sparseLDA, lev = levels(testResults.sparseLDA$obs))['Spec']),
	unname(twoClassSummary(testResults.pls, lev = levels(testResults.pls$obs))['Spec']),
	unname(twoClassSummary(testResults.glmnet, lev = levels(testResults.glmnet$obs))['Spec']),
	unname(twoClassSummary(testResults.svm, lev = levels(testResults.svm$obs))['Spec']),
	unname(twoClassSummary(testResults.svmP, lev = levels(testResults.svmP$obs))['Spec']), 
	unname(twoClassSummary(testResults.gbm, lev = levels(testResults.gbm$obs))['Spec']),
	unname(twoClassSummary(testResults.rf, lev = levels(testResults.rf$obs))['Spec'])))

p1 <- ggplot(Results.Test, aes(x = Model, y = ROC, color = Model)) + geom_point(size = 11) + theme_bw(base_size = 24) + coord_flip() + theme(legend.position = 'none') 
p2 <- ggplot(Results.Test, aes(x = Model, y = Sensitivity, color = Model)) + geom_point(size = 11) + theme_bw(base_size = 24) + coord_flip() + theme(legend.position = 'none')
p3 <- ggplot(Results.Test, aes(x = Model, y = Specificity, color = Model)) + geom_point(size = 11) + coord_flip() + theme_bw(base_size = 24) + theme(legend.position = 'none')
plot_grid(p1,p2,p3,nrow = 1, align = 'h', labels = c('A','B','C'))
ggsave('test-Res.pdf', width = 25.1, height = 7)



#Interpretable ML 

#DALEX

p_fun <- function(object, newdata){predict(object, newdata = newdata, type = 'prob')[,2]}
yTest <- as.numeric(dfTestY)
explainer_classif_rf <- explain(nonlinearFit.rf, label = 'rf', data = dfTest, y = yTest, 
	predict_function = p_fun, colorize = FALSE)
explainer_classif_gbm <- explain(nonlinearFit.gbm, label = 'gbm', data = dfTest, y = yTest,
	predict_function = p_fun, colorize = FALSE)

mp_classif_rf <- model_performance(explainer_classif_rf)
mp_classif_gbm <- model_performance(explainer_classif_gbm)

plot(mp_classif_rf, mp_classif_gbm)


vi_classif_rf <- variable_importance(explainer_classif_rf,loss_function = loss_root_mean_square)
vi_classif_gbm <- variable_importance(explainer_classif_gbm, loss_function = loss_root_mean_square)
plot(vi_classif_rf, vi_classif_gbm)