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

df0 <- aging[,names(aging) %in% c('score','TestAge')]
#ggplot(df0, aes(x = score, y = TestAge)) + geom_point(alpha = 0.2) + labs(x = 'CFI', y = 'Age (in weeks)') + theme_bw(base_size = 18)
#ggsave('M0-scatter-plot.pdf')

df1 <- aging[,names(aging) %in% c('score','TestAge',frailty_parameters)]
#nzv <- nearZeroVar(df1, saveMetrics = TRUE)
#df1 <- df1[,-which(as.numeric(nzv$zeroVar)!=0)]
nzv <- nearZeroVar(df1)
df1 <- df1[,-nzv]
df1X <- df1[, names(df1) %in% c(frailty_parameters)]
df1Y <- df1[,'TestAge']


df2 <- aging[,names(aging) %in% c('score','TestAge',
	OFA_measures,engineered_features_mean, avg_gait_measures_linear, median_gait_measures_linear,
	std_gait_measures_linear,
	engineered_features_stdev)]

#df0$TestAge <- as.numeric(scale(df0$TestAge, scale = TRUE, center = TRUE))
#df1$TestAge <- as.numeric(scale(df1$TestAge, scale = TRUE, center = TRUE))
#df2$TestAge <- as.numeric(scale(df2$TestAge, scale = TRUE, center = TRUE))
#df0$score <- as.numeric(scale(df0$score, scale = TRUE, center = TRUE))
#df1$score <- as.numeric(scale(df1$score, scale = TRUE, center = TRUE))
#df2$score <- as.numeric(scale(df2$score, scale = TRUE, center = TRUE))

#df0$TestAge <- (df0$TestAge - min(df0$TestAge))/(max(df0$TestAge) - min(df0$TestAge))
#df1$TestAge <- (df1$TestAge - min(df1$TestAge))/(max(df1$TestAge) - min(df1$TestAge))
#df2$TestAge <- (df2$TestAge - min(df2$TestAge))/(max(df2$TestAge) - min(df2$TestAge))
#df0$score <- (df0$score - min(df0$score))/(max(df0$score) - min(df0$score))
#df1$score <- (df1$score - min(df1$score))/(max(df1$score) - min(df1$score))
#df2$score <- (df2$score - min(df2$score))/(max(df2$score) - min(df2$score))

#df1$score <- df1$score/length(frailty_parameters)
#df2$score <- df2$score/length(frailty_parameters)

#PREDICTIVE MODELING
#Repeated 10-fold CV
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10, savePredictions = 'final')

#Data preprocess
#df1
set.seed(100)
trainIndex <- createDataPartition(df1$TestAge, p = 0.8, times = 1, list = FALSE)
df1Train <- df1[trainIndex,]
names(df1Train) <- colnames(df1Train)
df1TrainY <- df1Train$TestAge
df1TrainX <- df1Train[,-which(names(df1Train) %in% c('TestAge','score'))]
df1Test <- df1[-trainIndex,]
names(df1Test) <- colnames(df1Test)
df1TestX <- df1Test[,-which(names(df1Test) %in% c('TestAge','score'))]
df1TestY <- df1Test$TestAge

#df2
df2Train <- df2[trainIndex,]
names(df2Train) <- colnames(df2Train)
df2TrainY <- df2Train$TestAge
df2TrainX <- df2Train[,-which(names(df2Train) %in% c('TestAge','score'))]
df2Test <- df2[-trainIndex,]
df2TestX <- df2Test[,-which(names(df2Test) %in% c('TestAge','score'))]
df2TestY <- df2Test$TestAge

#df3
dfTrain <- df2[trainIndex,]
names(df2Train) <- colnames(df2Train)
dfTrainY <- df2Train$score
dfTrainX <- df2Train[,-which(names(df2Train) %in% c('TestAge','score'))]
dfTest <- df2[-trainIndex,]
dfTestX <- df2Test[,-which(names(df2Test) %in% c('TestAge','score'))]
dfTestY <- df2Test$score

#df0 
df0Train <- df0[trainIndex,]
names(df0Train) <- colnames(df0Train)
df0TrainY <- df0Train$score
df0TrainX <- data.frame(score = df0Train$TestAge)
df0Test <- df0[-trainIndex,]
df0TestX <- data.frame(score = df0Test$TestAge)
df0TestY <- df0Test$score

#df4
df4Train <- df2[trainIndex,]
names(df4Train) <- colnames(df4Train)
df4TrainY <- df4Train$score
df4TrainX <- df4Train[,-which(names(df4Train) %in% c('score'))]
df4Test <- df2[-trainIndex,]
df4TestX <- df4Test[,-which(names(df4Test) %in% c('score'))]
df4TestY <- df4Test$score

set.seed(4)
linearFit0 <- train(df0TrainX,df0TrainY, method = 'lm', trControl = fitControl, preProc = c('center','scale'))
tmp <- data.frame(obs = df0TestY, pred = predict(linearFit0, df0TestX)) #M0
set.seed(4)
linearFit1 <- train(df1TrainX,df1TrainY, method = 'lm', trControl = fitControl, preProc = c('center','scale'))
set.seed(4)
linearFit2 <- train(df2TrainX,df2TrainY, method = 'lm', trControl = fitControl, preProc = c('center','scale'))
set.seed(4)
linearFit <- train(dfTrainX,dfTrainY, method = 'lm', trControl = fitControl, preProc = c('center','scale'))

glmn_grid <- expand.grid(alpha = seq(0.1, 1, by = .1), lambda = 10^seq(-2, -.01, length = 30))
set.seed(4)
linearFit1.enet <- train(df1TrainX, df1TrainY, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))
set.seed(4)
linearFit2.enet <- train(df2TrainX, df2TrainY, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))

glmn_grid <- expand.grid(alpha = seq(0.1, 1, by = .1), lambda = 10^seq(-2, -1, length = 30))
set.seed(4)
linearFit.enet <- train(dfTrainX, dfTrainY, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))
set.seed(4)
linearFit4.enet <- train(df4TrainX, df4TrainY, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))

set.seed(4)
linearFit1.pls <- train(df1TrainX, df1TrainY, method = 'pls', tuneLength = 10, trControl = fitControl,
  preProc = c('center','scale'))
set.seed(4)
linearFit2.pls <- train(df2TrainX, df2TrainY, method = 'pls', tuneLength = 10, trControl = fitControl,
  preProc = c('center','scale'))
set.seed(4)
linearFit.pls <- train(dfTrainX, dfTrainY, method = 'pls', tuneLength = 10, trControl = fitControl,
  preProc = c('center','scale'))

#svm_grid <- expand.grid(C = seq(0,2,by = 0.5), sigma = seq(0,0.1,by = 0.05))
set.seed(4)
nonlinearFit1.svm <- train(df1TrainX, df1TrainY, method = 'svmRadial', preProc = c('center','scale'), tuneLength = 14, 
  trControl = fitControl)
set.seed(4)
nonlinearFit2.svm <- train(df2TrainX, df2TrainY, method = 'svmRadial', preProc = c('center','scale'), tuneLength = 14, 
  trControl = fitControl)
set.seed(4)
nonlinearFit.svm <- train(dfTrainX, dfTrainY, method = 'svmRadial', preProc = c('center','scale'), tuneLength = 14, 
  trControl = fitControl)
set.seed(4)
nonlinearFit4.svm <- train(df4TrainX, df4TrainY, method = 'svmRadial', preProc = c('center','scale'), tuneLength = 14, 
  trControl = fitControl)

rfGrid <- expand.grid(mtry = seq(2,ncol(df1TrainX), by = 5), min.node.size = seq(1,5,by = 2), 
	splitrule = 'variance')
set.seed(4)
df1Train <- df1Train[, -which(names(df1Train) %in% c('score'))]
nonlinearFit1.rf <- train(TestAge ~ ., data = df1Train, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid)
rfGrid <- expand.grid(mtry = seq(2,ncol(df2TrainX), by = 5), min.node.size = seq(1,5,by = 2), 
	splitrule = 'variance')
set.seed(4)
df2Train <- df2Train[, -which(names(df2Train) %in% c('score'))]
nonlinearFit2.rf <- train(TestAge ~ ., data = df2Train, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid)
rfGrid <- expand.grid(mtry = seq(2,ncol(dfTrainX), by = 10), min.node.size = seq(3,5,by = 2), 
	splitrule = 'variance')
set.seed(4)
dfTrain <- dfTrain[, -which(names(dfTrain) %in% c('TestAge'))]
nonlinearFit.rf <- train(score ~ ., data = dfTrain, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid)
set.seed(4)
nonlinearFit4.rf <- train(score ~ ., data = df4Train, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid)

gbmGrid <- expand.grid(interaction.depth = seq(1,11,by = 5), n.trees = seq(100,200,by = 100), 
  shrinkage = c(0.01,0.02),n.minobsinnode = c(5,7))
set.seed(4)
nonlinearFit1.gbm <- train(df1TrainX, df1TrainY, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, 
	trControl = fitControl)
set.seed(4)
nonlinearFit2.gbm <- train(df2TrainX, df2TrainY, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, 
	trControl = fitControl)
set.seed(4)
nonlinearFit.gbm <- train(dfTrainX, dfTrainY, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, 
	trControl = fitControl)
set.seed(4)
nonlinearFit4.gbm <- train(df4TrainX, df4TrainY, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, 
	trControl = fitControl)

#TRAINING SET RESULTS
#RMSE
rmse.results1 <- data.frame(Enet1 = linearFit1.enet$resample$RMSE, Enet2 = linearFit2.enet$resample$RMSE)
rmse.results12 <- data.frame(Enet3 = linearFit.enet$resample$RMSE, Enet4 = linearFit4.enet$resample$RMSE)
rmse.results1 <- cbind(id = 1:dim(rmse.results1)[1], rmse.results1)
rmse.results12 <- cbind(id = 1:dim(rmse.results12)[1], rmse.results12)
rmse.results1.melt <- melt(rmse.results1, id.vars = 'id')
rmse.results12.melt <- melt(rmse.results12, id.vars = 'id')
rmse.results1.melt <- cbind(rmse.results1.melt, Method = rep('Enet', dim(rmse.results1)[1]*(dim(rmse.results1)[2]-1)), 
	Model = c(rep('M1', dim(rmse.results1)[1]),rep('M2', dim(rmse.results1)[1])))
rmse.results12.melt <- cbind(rmse.results12.melt, Method = rep('Enet', dim(rmse.results12)[1]*(dim(rmse.results12)[2]-1)), 
	Model = c(rep('M3', dim(rmse.results12)[1]),rep('M4', dim(rmse.results12)[1])))
rmse.results1.melt <- rmse.results1.melt[,-1]
rmse.results12.melt <- rmse.results12.melt[,-1]

#p1 <- ggplot(rmse.results12.melt, aes(y = value, x = Method, fill = Model)) + geom_boxplot() + 
#scale_fill_manual(values = c('#f16913','#2b8cbe')) + theme_bw(base_size = 20) + labs(y = 'RMSE') + 
#geom_hline(yintercept = linearFit0$results$RMSE)

rmse.results2 <- data.frame(svm1 = nonlinearFit1.svm$resample$RMSE, svm2 = nonlinearFit2.svm$resample$RMSE)
rmse.results22 <- data.frame(svm3 = nonlinearFit.svm$resample$RMSE, svm4 = nonlinearFit4.svm$resample$RMSE)
rmse.results2 <- cbind(id = 1:dim(rmse.results2)[1], rmse.results2)
rmse.results22 <- cbind(id = 1:dim(rmse.results22)[1], rmse.results22)
rmse.results2.melt <- melt(rmse.results2, id.vars = 'id')
rmse.results22.melt <- melt(rmse.results22, id.vars = 'id')
rmse.results2.melt <- cbind(rmse.results2.melt, Method = rep('SVM', dim(rmse.results2)[1]*(dim(rmse.results2)[2]-1)), 
	Model = c(rep('M1', dim(rmse.results2)[1]),rep('M2', dim(rmse.results2)[1])))
rmse.results22.melt <- cbind(rmse.results22.melt, Method = rep('SVM', dim(rmse.results22)[1]*(dim(rmse.results22)[2]-1)), 
	Model = c(rep('M3', dim(rmse.results22)[1]),rep('M4', dim(rmse.results22)[1])))
rmse.results2.melt <- rmse.results2.melt[,-1]
rmse.results22.melt <- rmse.results22.melt[,-1]

rmse.results3 <- data.frame(rf1 = nonlinearFit1.rf$resample$RMSE, rf2 = nonlinearFit2.rf$resample$RMSE)
rmse.results32 <- data.frame(rf3 = nonlinearFit.rf$resample$RMSE, rf4 = nonlinearFit4.rf$resample$RMSE)
rmse.results3 <- cbind(id = 1:dim(rmse.results3)[1], rmse.results3)
rmse.results32 <- cbind(id = 1:dim(rmse.results32)[1], rmse.results32)
rmse.results3.melt <- melt(rmse.results3, id.vars = 'id')
rmse.results32.melt <- melt(rmse.results32, id.vars = 'id')
rmse.results3.melt <- cbind(rmse.results3.melt, Method = rep('RF', dim(rmse.results3)[1]*(dim(rmse.results3)[2]-1)), 
	Model = c(rep('M1', dim(rmse.results3)[1]),rep('M2', dim(rmse.results3)[1])))
rmse.results32.melt <- cbind(rmse.results32.melt, Method = rep('RF', dim(rmse.results32)[1]*(dim(rmse.results32)[2]-1)), 
	Model = c(rep('M3', dim(rmse.results32)[1]),rep('M4', dim(rmse.results32)[1])))
rmse.results3.melt <- rmse.results3.melt[,-1]
rmse.results32.melt <- rmse.results32.melt[,-1]

rmse.results4 <- data.frame(gbm1 = nonlinearFit1.gbm$resample$RMSE, gbm2 = nonlinearFit2.gbm$resample$RMSE)
rmse.results42 <- data.frame(gbm3 = nonlinearFit.gbm$resample$RMSE, gbm4 = nonlinearFit4.gbm$resample$RMSE)
rmse.results4 <- cbind(id = 1:dim(rmse.results4)[1], rmse.results4)
rmse.results42 <- cbind(id = 1:dim(rmse.results42)[1], rmse.results42)
rmse.results4.melt <- melt(rmse.results4, id.vars = 'id')
rmse.results42.melt <- melt(rmse.results42, id.vars = 'id')
rmse.results4.melt <- cbind(rmse.results4.melt, Method = rep('GBM', dim(rmse.results4)[1]*(dim(rmse.results4)[2]-1)), 
	Model = c(rep('M1', dim(rmse.results4)[1]),rep('M2', dim(rmse.results4)[1])))
rmse.results42.melt <- cbind(rmse.results42.melt, Method = rep('GBM', dim(rmse.results42)[1]*(dim(rmse.results42)[2]-1)), 
	Model = c(rep('M3', dim(rmse.results42)[1]),rep('M4', dim(rmse.results42)[1])))
rmse.results4.melt <- rmse.results4.melt[,-1]
rmse.results42.melt <- rmse.results42.melt[,-1]

RMSE.results1 <- rbind(rmse.results1.melt,rmse.results2.melt,rmse.results3.melt,rmse.results4.melt)
RMSE.results2 <- rbind(rmse.results12.melt,rmse.results22.melt,rmse.results32.melt,rmse.results42.melt)
#p1 <- ggplot(RMSE.results, aes(y = value, x = Method)) + geom_boxplot() + facet_wrap(~ Model, scales = 'free') + theme(legend.position = 'none') + labs(y = 'RMSE') + 
#theme_bw(base_size = 20)
p1 <- ggplot(RMSE.results1, aes(y = value, x = Method, fill = Model)) + geom_boxplot() + 
scale_fill_manual(values = c('#f16913','#2b8cbe')) + theme_bw(base_size = 20) + labs(y = 'RMSE')
p2 <- ggplot(RMSE.results2, aes(y = value, x = Method, fill = Model)) + geom_boxplot() + 
scale_fill_manual(values = c('#f16913','#2b8cbe')) + theme_bw(base_size = 20) + labs(y = 'RMSE') 
#geom_hline(yintercept = linearFit0$results$RMSE)

cowplot::plot_grid(p1 + labs(x = NULL),p2,ncol = 1, align = 'h')
ggsave('rmse-comparisons-new.png')

#MAE
MAE.results1 <- data.frame(Enet1 = linearFit1.enet$resample$MAE, Enet2 = linearFit2.enet$resample$MAE, 
	Enet = linearFit.enet$resample$MAE)
MAE.results1 <- cbind(id = 1:dim(MAE.results1)[1], MAE.results1)
MAE.results1.melt <- melt(MAE.results1, id.vars = 'id')
MAE.results1.melt <- cbind(MAE.results1.melt, Method = rep('Enet', dim(MAE.results1)[1]*(dim(MAE.results1)[2]-1)), 
	Model = c(rep('M1', dim(MAE.results1)[1]),rep('M2', dim(MAE.results1)[1]),rep('M3', dim(MAE.results1)[1])))
MAE.results1.melt <- MAE.results1.melt[,-1]


MAE.results2 <- data.frame(svm1 = nonlinearFit1.svm$resample$MAE, svm2 = nonlinearFit2.svm$resample$MAE,
	svm = nonlinearFit.svm$resample$MAE)
MAE.results2 <- cbind(id = 1:dim(MAE.results2)[1], MAE.results2)
MAE.results2.melt <- melt(MAE.results2, id.vars = 'id')
MAE.results2.melt <- cbind(MAE.results2.melt, Method = rep('SVM', dim(MAE.results2)[1]*(dim(MAE.results2)[2]-1)), 
	Model = c(rep('M1', dim(MAE.results2)[1]),rep('M2', dim(MAE.results2)[1]),rep('M3', dim(MAE.results2)[1])))
MAE.results2.melt <- MAE.results2.melt[,-1]

MAE.results3 <- data.frame(rf1 = nonlinearFit1.rf$resample$MAE, rf2 = nonlinearFit2.rf$resample$MAE,
	rf = nonlinearFit.rf$resample$MAE)
MAE.results3 <- cbind(id = 1:dim(MAE.results3)[1], MAE.results3)
MAE.results3.melt <- melt(MAE.results3, id.vars = 'id')
MAE.results3.melt <- cbind(MAE.results3.melt, Method = rep('RF', dim(MAE.results3)[1]*(dim(MAE.results3)[2]-1)), 
	Model = c(rep('M1', dim(MAE.results3)[1]),rep('M2', dim(MAE.results3)[1]),rep('M3', dim(MAE.results3)[1])))
MAE.results3.melt <- MAE.results3.melt[,-1]

MAE.results4 <- data.frame(gbm1 = nonlinearFit1.gbm$resample$MAE, gbm2 = nonlinearFit2.gbm$resample$MAE,
	gbm = nonlinearFit.gbm$resample$MAE)
MAE.results4 <- cbind(id = 1:dim(MAE.results4)[1], MAE.results4)
MAE.results4.melt <- melt(MAE.results4, id.vars = 'id')
MAE.results4.melt <- cbind(MAE.results4.melt, Method = rep('GBM', dim(MAE.results4)[1]*(dim(MAE.results4)[2]-1)), 
	Model = c(rep('M1', dim(MAE.results4)[1]),rep('M2', dim(MAE.results4)[1]),rep('M3', dim(MAE.results4)[1])))
MAE.results4.melt <- MAE.results4.melt[,-1]

MAE.results <- rbind(MAE.results1.melt,MAE.results2.melt,MAE.results3.melt,MAE.results4.melt)

#p2 <- ggplot(MAE.results, aes(y = value, x = Method)) + geom_boxplot() + facet_wrap(~ Model, scales = 'free') + theme(legend.position = 'none') + labs(y = 'MAE') + 
#theme_bw(base_size = 20)
p2 <- ggplot(MAE.results, aes(y = value, x = Method, fill = Model)) + geom_boxplot() + 
scale_fill_manual(values = c('#f16913','#2b8cbe','#aec800')) + theme_bw(base_size = 20) + labs(y = 'MAE')

ggsave('mae-comparisons.pdf')

#Rsquared
Rsquared.results1 <- data.frame(Enet1 = linearFit1.enet$resample$Rsquared, Enet2 = linearFit2.enet$resample$Rsquared, 
	Enet = linearFit.enet$resample$Rsquared)
Rsquared.results1 <- cbind(id = 1:dim(Rsquared.results1)[1], Rsquared.results1)
Rsquared.results1.melt <- melt(Rsquared.results1, id.vars = 'id')
Rsquared.results1.melt <- cbind(Rsquared.results1.melt, Method = rep('Enet', dim(Rsquared.results1)[1]*(dim(Rsquared.results1)[2]-1)), 
	Model = c(rep('M1', dim(Rsquared.results1)[1]),rep('M2', dim(Rsquared.results1)[1]),rep('M3', dim(Rsquared.results1)[1])))
Rsquared.results1.melt <- Rsquared.results1.melt[,-1]


Rsquared.results2 <- data.frame(svm1 = nonlinearFit1.svm$resample$Rsquared, svm2 = nonlinearFit2.svm$resample$Rsquared,
	svm = nonlinearFit.svm$resample$Rsquared)
Rsquared.results2 <- cbind(id = 1:dim(Rsquared.results2)[1], Rsquared.results2)
Rsquared.results2.melt <- melt(Rsquared.results2, id.vars = 'id')
Rsquared.results2.melt <- cbind(Rsquared.results2.melt, Method = rep('SVM', dim(Rsquared.results2)[1]*(dim(Rsquared.results2)[2]-1)), 
	Model = c(rep('M1', dim(Rsquared.results2)[1]),rep('M2', dim(Rsquared.results2)[1]),rep('M3', dim(Rsquared.results2)[1])))
Rsquared.results2.melt <- Rsquared.results2.melt[,-1]

Rsquared.results3 <- data.frame(rf1 = nonlinearFit1.rf$resample$Rsquared, rf2 = nonlinearFit2.rf$resample$Rsquared,
	rf = nonlinearFit.rf$resample$Rsquared)
Rsquared.results3 <- cbind(id = 1:dim(Rsquared.results3)[1], Rsquared.results3)
Rsquared.results3.melt <- melt(Rsquared.results3, id.vars = 'id')
Rsquared.results3.melt <- cbind(Rsquared.results3.melt, Method = rep('RF', dim(Rsquared.results3)[1]*(dim(Rsquared.results3)[2]-1)), 
	Model = c(rep('M1', dim(Rsquared.results3)[1]),rep('M2', dim(Rsquared.results3)[1]),rep('M3', dim(Rsquared.results3)[1])))
Rsquared.results3.melt <- Rsquared.results3.melt[,-1]

Rsquared.results4 <- data.frame(gbm1 = nonlinearFit1.gbm$resample$Rsquared, gbm2 = nonlinearFit2.gbm$resample$Rsquared,
	gbm = nonlinearFit.gbm$resample$Rsquared)
Rsquared.results4 <- cbind(id = 1:dim(Rsquared.results4)[1], Rsquared.results4)
Rsquared.results4.melt <- melt(Rsquared.results4, id.vars = 'id')
Rsquared.results4.melt <- cbind(Rsquared.results4.melt, Method = rep('GBM', dim(Rsquared.results4)[1]*(dim(Rsquared.results4)[2]-1)), 
	Model = c(rep('M1', dim(Rsquared.results4)[1]),rep('M2', dim(Rsquared.results4)[1]),rep('M3', dim(Rsquared.results4)[1])))
Rsquared.results4.melt <- Rsquared.results4.melt[,-1]

Rsquared.results <- rbind(Rsquared.results1.melt,Rsquared.results2.melt,Rsquared.results3.melt,Rsquared.results4.melt)

#p3 <- ggplot(Rsquared.results, aes(y = value, x = Method)) + geom_boxplot() + facet_wrap(~ Model, scales = 'free') + theme(legend.position = 'none') + labs(y = 'Rsquared') + 
#theme_bw(base_size = 20)
p3 <- ggplot(Rsquared.results, aes(y = value, x = Method, fill = Model)) + geom_boxplot() + 
scale_fill_manual(values = c('#f16913','#2b8cbe','#aec800')) + theme_bw(base_size = 20) + labs(y = 'Rsquared')

ggsave('r2-comparisons.pdf')

cowplot::plot_grid(p1,p2,p3, ncol = 1, align = 'v')
ggsave('all-comparisons.pdf')


#TEST SET RESULTS
testResults <- data.frame(obs1 = df1TestY, obs2 = df2TestY, obs = dfTestY,obs4 = df4TestY)
testResults$enet1 <- predict(linearFit1.enet,df1TestX)
testResults$enet2 <- predict(linearFit2.enet,df2TestX)
testResults$enet <- predict(linearFit.enet,dfTestX)
testResults$enet4 <- predict(linearFit4.enet,df4TestX)
testResults$svm1 <- predict(nonlinearFit1.svm, df1TestX)
testResults$rf1 <- predict(nonlinearFit1.rf, df1TestX)
testResults$gbm1 <- predict(nonlinearFit1.gbm, df1TestX)
testResults$svm2 <- predict(nonlinearFit2.svm, df2TestX)
testResults$rf2 <- predict(nonlinearFit2.rf, df2TestX)
testResults$gbm2 <- predict(nonlinearFit2.gbm, df2TestX)
testResults$svm <- predict(nonlinearFit.svm, dfTestX)
testResults$rf <- predict(nonlinearFit.rf, dfTestX)
testResults$gbm <- predict(nonlinearFit.gbm, dfTestX)
testResults$svm4 <- predict(nonlinearFit4.svm, df4TestX)
testResults$rf4 <- predict(nonlinearFit4.rf, df4TestX)
testResults$gbm4 <- predict(nonlinearFit4.gbm, df4TestX)

Test.RMSE1 <- data.frame(RMSE = c(RMSE(testResults$obs1, testResults$enet1), RMSE(testResults$obs2, testResults$enet2),
RMSE(testResults$obs1, testResults$svm1), RMSE(testResults$obs2, testResults$svm2), 
RMSE(testResults$obs1, testResults$rf1), RMSE(testResults$obs2, testResults$rf2), 
RMSE(testResults$obs1, testResults$gbm1), RMSE(testResults$obs2, testResults$gbm2)))
Test.RMSE2 <- data.frame(RMSE = c(RMSE(testResults$obs, testResults$enet), RMSE(testResults$obs4, testResults$enet4),
RMSE(testResults$obs1, testResults$svm), RMSE(testResults$obs4, testResults$svm), 
RMSE(testResults$obs, testResults$rf), RMSE(testResults$obs4, testResults$rf4), 
RMSE(testResults$obs, testResults$gbm), RMSE(testResults$obs4, testResults$gbm4)))

Test.RMSE1 <- cbind(Test.RMSE1, Model = c(rep(c('M1','M2'),2)), Method = c(rep('Enet',2), rep('SVM',2),
	rep('RF',2), rep('GBM',2)))
Test.RMSE2 <- cbind(Test.RMSE2, Model = c(rep(c('M3','M4'),2)), Method = c(rep('Enet',2), rep('SVM',2),
	rep('RF',2), rep('GBM',2)))

p1 <- ggplot(Test.RMSE1, aes(x = Method, y = RMSE, color = Model, shape = Model)) + geom_point(size = 6) + theme_bw(base_size = 16) + 
coord_flip() + scale_color_manual(values = c('#f16913','#2b8cbe'))

p2 <- ggplot(Test.RMSE2, aes(x = Method, y = RMSE, color = Model, shape = Model)) + geom_point(size = 6) + theme_bw(base_size = 16) + 
coord_flip() + scale_color_manual(values = c('#f16913','#2b8cbe'))

cowplot::plot_grid(p1,p2,nrow = 1, align = 'h')
#scale_color_manual(values = c('#f16913','#2b8cbe','#aec800')) 
ggsave('test-comparison.png')

Test.R2 <- data.frame(Rsquared = c(R2(testResults$obs1, testResults$enet1), R2(testResults$obs2, testResults$enet2), R2(testResults$obs, testResults$enet),
R2(testResults$obs1, testResults$svm1), R2(testResults$obs2, testResults$svm2), R2(testResults$obs, testResults$svm), R2(testResults$obs1, testResults$rf1), R2(testResults$obs2, testResults$rf2), 
R2(testResults$obs, testResults$rf), R2(testResults$obs1, testResults$gbm1), R2(testResults$obs2, testResults$gbm2), 
R2(testResults$obs, testResults$gbm)))

Test.R2 <- cbind(Test.R2, Model = c(rep(c('M1','M2','M3'),4)), Method = c(rep('Enet',3), rep('SVM',3),
	rep('RF',3), rep('GBM',3)))

p2 <- ggplot(Test.R2, aes(x = Method, y = Rsquared, color = Model, shape = Model)) + geom_point(size = 6) + theme_bw(base_size = 24) + 
coord_flip() + scale_color_manual(values = c('#f16913','#2b8cbe','#aec800')) + labs(x = NULL)

Test.cor <- data.frame(Correlation = c(cor(testResults$obs1, testResults$enet1), cor(testResults$obs2, testResults$enet2), cor(testResults$obs, testResults$enet),
cor(testResults$obs1, testResults$svm1), cor(testResults$obs2, testResults$svm2), cor(testResults$obs, testResults$svm), cor(testResults$obs1, testResults$rf1), cor(testResults$obs2, testResults$rf2), 
cor(testResults$obs, testResults$rf), cor(testResults$obs1, testResults$gbm1), cor(testResults$obs2, testResults$gbm2), 
cor(testResults$obs, testResults$gbm)))

Test.cor <- cbind(Test.cor, Model = c(rep(c('M1','M2','M3'),4)), Method = c(rep('Enet',3), rep('SVM',3),
	rep('RF',3), rep('GBM',3)))

p3 <- ggplot(Test.cor, aes(x = Method, y = Correlation, color = Model, shape = Model)) + geom_point(size = 6) + theme_bw(base_size = 24) + 
coord_flip() + scale_color_manual(values = c('#f16913','#2b8cbe','#aec800')) + labs(x = NULL)

prow <- plot_grid(p1 + theme(legend.position = 'none'), p2 + theme(legend.position = 'none'), 
	p3 + theme(legend.position = 'none'), align = 'vh', labels = c('A','B','C'),hjust = -1, nrow = 1)

legend <- get_legend(p1 + theme(legend.box.margin = margin(0, 0, 0, 12)))
plot_grid(prow, legend, rel_widths = c(3,.4))
ggsave('all-test-comparisons.pdf')

#IMPL 
predictor1.gbm <- Predictor$new(model = nonlinearFit1.gbm, data = df1TrainX, y = df1TrainY, 
	class = 'regression')
imp1 <- FeatureImp$new(predictor1.gbm, loss = 'mse')
p1 <- plot(imp1) + ggtitle('M1')

predictor2.gbm <- Predictor$new(model = nonlinearFit2.gbm, data = df2TrainX, y = df2TrainY, 
	class = 'regression')
imp2 <- FeatureImp$new(predictor2.gbm, loss = 'mse')
p2 <- plot(imp2) + ggtitle('M2')

predictor.gbm <- Predictor$new(model = nonlinearFit.gbm, data = dfTrainX, y = dfTrainY, 
	class = 'regression')
imp <- FeatureImp$new(predictor.gbm, loss = 'mse')
p3 <- plot(imp) + ggtitle('M3')

pdp_obj <- FeatureEffect$new(predictor.gbm, feature = 'median_step_width')
pdp_obj$center(min(dfTrain$median_step_width))
pdp_obj$plot()


p1 <- ggplot(varImp(nonlinearFit1.gbm)) + ggtitle('M1')
p2 <- ggplot(varImp(nonlinearFit2.gbm)) + ggtitle('M2')
p3 <- ggplot(varImp(nonlinearFit.gbm)) + ggtitle('M3')
p4 <- ggplot(varImp(nonlinearFit4.gbm)) + ggtitle('M4')
cowplot::plot_grid(p1,p2,p3,p4,nrow = 1)
ggsave('gbm-varImp.png')


gbm2.msw <- FeatureEffect$new(predictor2.gbm, 'median_step_width', grid.size = 20)
p1 <- plot(gbm2.msw)
gbm2.asw <- FeatureEffect$new(predictor2.gbm, 'avg_step_width', grid.size = 20)
p2 <- plot(gbm2.asw)
gbm2.msl <- FeatureEffect$new(predictor2.gbm, 'median_stride_length', grid.size = 20)
p3 <- plot(gbm2.msl)
gbm2.asl <- FeatureEffect$new(predictor2.gbm, 'avg_stride_length', grid.size = 20)
p4 <- plot(gbm2.asl)
gbm2.ms <- FeatureEffect$new(predictor2.gbm, 'median_speed_cm_per_sec', grid.size = 20)
p5 <- plot(gbm2.ms)
gbm2.as <- FeatureEffect$new(predictor2.gbm, 'avg_speed_cm_per_sec', grid.size = 20)
p6 <- plot(gbm2.as)
gbm2.msc <- FeatureEffect$new(predictor2.gbm, 'median_stride_count', grid.size = 20)
p7 <- plot(gbm2.msc)
gbm2.asc <- FeatureEffect$new(predictor2.gbm, 'avg_stride_count', grid.size = 20)
p8 <- plot(gbm2.asc)
gbm2.D <- FeatureEffect$new(predictor2.gbm, 'Distance.cm.sc', grid.size = 20)
p7 <- plot(gbm2.D)
gbm2.gb <- FeatureEffect$new(predictor2.gbm, 'grooming_number_bouts', grid.size = 20)
p8 <- plot(gbm2.gb)

plot_grid(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2, ncol = 4)
dev.print(pdf, 'pdp-gbm2.pdf')