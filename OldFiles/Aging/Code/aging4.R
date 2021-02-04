rm(list = ls())
libraries <- c('glmnet','leaps','caret','e1071','reshape','xgboost')
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

df5 <- aging[,names(aging) %in% c('score','TestAge','Tester','Sex','Weight')]
tmp <- lmer(score ~ TestAge + Sex + Weight + (1|Tester), data = df5)
#df5$score <- resid(tmp) + fitted(tmp)

A <- ggplot(df5, aes(x = factor(TestAge), y = score)) + geom_boxplot() + 
labs(x = 'TestAge (in weeks)', y = 'CFI')

B <- ggplot(df5, aes(x = TestAge, y = score)) + geom_point(alpha = 0.5) + geom_smooth(method='lm') + 
labs(x = 'TestAge (in weeks)', y = 'CFI')


df1 <- aging[,names(aging) %in% c('score','TestAge',frailty_parameters)]
#nzv <- nearZeroVar(df1, saveMetrics = TRUE)
#df1 <- df1[,-which(as.numeric(nzv$zeroVar)!=0)]
nzv <- nearZeroVar(df1)
df1 <- df1[,-nzv]
df1X <- df1[, names(df1) %in% c(frailty_parameters)]
df1Y <- df1[,'TestAge']


df2 <- aging[,names(aging) %in% c('TestAge',
	OFA_measures, median_gait_measures_linear, avg_gait_measures_linear,
	std_gait_measures_linear, iqr_gait_measures_linear,engineered_features_mean, engineered_features_stdev, 'median_width', 'median_length',
	'median_rearpaw')]
df2X <- df2[,-which(names(df2) %in% c('TestAge'))]
df2Y <- df2[,'TestAge']


df3 <- aging[,names(aging) %in% c('score',
	OFA_measures, median_gait_measures_linear, avg_gait_measures_linear,
	std_gait_measures_linear, iqr_gait_measures_linear,engineered_features_mean, engineered_features_stdev, 'median_width', 'median_length', 
	'median_rearpaw')]
df3X <- df3[,-which(names(df3) %in% c('score'))]
df3Y <- df3[,'score']

df4 <- aging[,names(aging) %in% c('score','TestAge')]
df4X <- df4[,'TestAge']
df4Y <- df4[,'score']



#Repeated 10-fold CV
set.seed(4) 
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 1, savePredictions = 'final')

set.seed(4)
linearFit0 <- train(data.frame(df4X),df4Y, method = 'lm', trControl = fitControl, preProc = c('center','scale'))

set.seed(4)
linearFit1 <- train(data.frame(df1X),df1Y, method = 'lm', trControl = fitControl, preProc = c('center','scale'))
set.seed(4)
linearFit2 <- train(data.frame(df2X),df2Y, method = 'lm', trControl = fitControl, preProc = c('center','scale'))


glmn_grid <- expand.grid(alpha = seq(0.1,0.5,by=0.1), lambda = seq(0, 10, length = 30))
set.seed(4)
linearFit1.enet <- train(df1X, df1Y, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))
ggplot(linearFit1.enet)
glmn_grid <- expand.grid(alpha = as.numeric(linearFit1.enet$bestTune[1]), lambda = as.numeric(linearFit1.enet$bestTune[2]))
set.seed(4)
linearFit1.enet.tuned <- train(df1X, df1Y, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))

glmn_grid <- expand.grid(alpha = seq(0.1,1,by=0.1), lambda = seq(0,0.5, length = 30))
set.seed(4)
linearFit2.enet <- train(df2X, df2Y, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))
ggplot(linearFit2.enet)
glmn_grid <- expand.grid(alpha = as.numeric(linearFit2.enet$bestTune[1]), lambda = as.numeric(linearFit2.enet$bestTune[2]))
set.seed(4)
linearFit2.enet.tuned <- train(df2X, df2Y, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))
linearFit2.enet.tuned

glmn_grid <- expand.grid(alpha = seq(0, 1, by = .1), lambda = seq(0,1,length = 30))
set.seed(4)
linearFit.enet <- train(df3X, df3Y, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))
ggplot(linearFit.enet)
glmn_grid <- expand.grid(alpha = as.numeric(linearFit.enet$bestTune[1]), lambda = as.numeric(linearFit.enet$bestTune[2]))
set.seed(4)
linearFit.enet.tuned <- train(df3X, df3Y, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))


svm_grid <- expand.grid(C = seq(1,5,by = 1), sigma = seq(0.10,0.30,length = 10))
set.seed(4)
nonlinearFit1.svm <- train(df1X, df1Y, method = 'svmRadial', preProc = c('center','scale'), tuneGrid = svm_grid, 
  trControl = fitControl)
ggplot(nonlinearFit1.svm)
svm_grid <- expand.grid(C = as.numeric(nonlinearFit1.svm$bestTune[2]), sigma = as.numeric(nonlinearFit1.svm$bestTune[1]))
set.seed(4)
nonlinearFit1.svm.tuned <- train(df1X, df1Y, method = 'svmRadial', preProc = c('center','scale'), tuneGrid = svm_grid, 
  trControl = fitControl)

svm_grid <- expand.grid(C = seq(1,4,by = 1), sigma = seq(0.0010,0.050,length = 10))
set.seed(4)
nonlinearFit2.svm <- train(df2X, df2Y, method = 'svmRadial', preProc = c('center','scale'), tuneGrid = svm_grid,
  trControl = fitControl)
ggplot(nonlinearFit2.svm)
svm_grid <- expand.grid(C = as.numeric(nonlinearFit2.svm$bestTune[2]), sigma = as.numeric(nonlinearFit2.svm$bestTune[1]))
set.seed(4)
nonlinearFit2.svm.tuned <- train(df2X, df2Y, method = 'svmRadial', preProc = c('center','scale'), tuneGrid = svm_grid, 
  trControl = fitControl)


svm_grid <- expand.grid(C = seq(1,3,by = 1), sigma = seq(0.001,0.030,length = 10))
set.seed(4)
nonlinearFit.svm <- train(df3X, df3Y, method = 'svmRadial', preProc = c('center','scale'), tuneGrid = svm_grid, 
  trControl = fitControl)
ggplot(nonlinearFit.svm)
svm_grid <- expand.grid(C = as.numeric(nonlinearFit.svm$bestTune[2]), sigma = as.numeric(nonlinearFit.svm$bestTune[1]))
set.seed(4)
nonlinearFit.svm.tuned <- train(df3X, df3Y, method = 'svmRadial', preProc = c('center','scale'), tuneGrid = svm_grid, 
  trControl = fitControl)



rfGrid <- expand.grid(mtry = seq(2,ncol(df1X), by = 5), min.node.size = seq(1,5,by = 2), 
	splitrule = 'variance')
set.seed(4)
nonlinearFit1.rf <- train(TestAge ~ ., data = df1, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid, importance = 'impurity')
ggplot(nonlinearFit1.rf)

rfGrid <- expand.grid(mtry = as.numeric(nonlinearFit1.rf$bestTune[1]), min.node.size = as.numeric(nonlinearFit1.rf$bestTune[3]), 
	splitrule = 'variance')
set.seed(4)
nonlinearFit1.rf.tuned <- train(TestAge ~ ., data = df1, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid, num.trees = 1000, importance = 'impurity')


rfGrid <- expand.grid(mtry = c(2,ncol(df2X),by = 10), min.node.size = seq(3,7,by = 2),
	splitrule = 'variance')
set.seed(4)
nonlinearFit2.rf <- train(TestAge ~ ., data = df2, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid, num.trees = 1000, importance = 'impurity')
ggplot(nonlinearFit2.rf)

rfGrid <- expand.grid(mtry = as.numeric(nonlinearFit2.rf$bestTune[1]), min.node.size = as.numeric(nonlinearFit2.rf$bestTune[3]), 
	splitrule = 'variance')
set.seed(4)
nonlinearFit2.rf.tuned <- train(TestAge ~ ., data = df2, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid, num.trees = 1000, importance = 'impurity')

rfGrid <- expand.grid(mtry = c(2,ncol(df3X), by = 10), min.node.size = seq(9,13,by = 2), 
	splitrule = 'variance')
set.seed(4)
nonlinearFit.rf <- train(score ~ ., data = df3, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid, num.trees = 1000, importance = 'impurity')
ggplot(nonlinearFit.rf)

rfGrid <- expand.grid(mtry = as.numeric(nonlinearFit.rf$bestTune[1]), min.node.size = as.numeric(nonlinearFit.rf$bestTune[3]), 
	splitrule = 'variance')
set.seed(4)
nonlinearFit.rf.tuned <- train(score ~ ., data = df3, method = 'ranger', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = rfGrid, num.trees = 1000, importance = 'impurity')



tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(4)
xgb_tune <- train(df1X, df1Y, method = 'xgbTree', tuneGrid = tune_grid, verbose = FALSE, 
	trControl = fitControl)


tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 100),
  eta = xgb_tune$bestTune$eta,
  max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
    c(xgb_tune$bestTune$max_depth:4),
    xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

set.seed(4)
xgb_tune2 <- train(df1X, df1Y, method = 'xgbTree', tuneGrid = tune_grid2, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune2)

tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 100),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

set.seed(4)
xgb_tune3 <- train(df1X, df1Y, method = 'xgbTree', tuneGrid = tune_grid3, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune3)


tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 100),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- train(df1X, df1Y, method = 'xgbTree', tuneGrid = tune_grid4, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune4)



tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- train(df1X, df1Y, method = 'xgbTree', tuneGrid = tune_grid5, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune5)


(final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
))

nonlinearFit1.gbm.tuned <- train(df1X, df1Y, method = 'xgbTree', tuneGrid = final_grid, verbose = FALSE, 
	trControl = fitControl)


tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(4)
xgb_tune <- train(df2X, df2Y, method = 'xgbTree', tuneGrid = tune_grid, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune)

tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 100),
  eta = xgb_tune$bestTune$eta,
  max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
    c(xgb_tune$bestTune$max_depth:4),
    xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

set.seed(4)
xgb_tune2 <- train(df2X, df2Y, method = 'xgbTree', tuneGrid = tune_grid2, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune2)

tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 100),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

set.seed(4)
xgb_tune3 <- train(df2X, df2Y, method = 'xgbTree', tuneGrid = tune_grid3, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune3)


tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 100),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- train(df2X, df2Y, method = 'xgbTree', tuneGrid = tune_grid4, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune4)



tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- train(df2X, df2Y, method = 'xgbTree', tuneGrid = tune_grid5, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune5)


(final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
))

nonlinearFit2.gbm.tuned <- train(df2X, df2Y, method = 'xgbTree', tuneGrid = final_grid, verbose = FALSE, 
	trControl = fitControl)



tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(4)
xgb_tune <- train(df3X, df3Y, method = 'xgbTree', tuneGrid = tune_grid, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune)

tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 100),
  eta = xgb_tune$bestTune$eta,
  max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
    c(xgb_tune$bestTune$max_depth:4),
    xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

set.seed(4)
xgb_tune2 <- train(df3X, df3Y, method = 'xgbTree', tuneGrid = tune_grid2, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune2)

tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 100),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

set.seed(4)
xgb_tune3 <- train(df3X, df3Y, method = 'xgbTree', tuneGrid = tune_grid3, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune3)


tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 100),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- train(df3X, df3Y, method = 'xgbTree', tuneGrid = tune_grid4, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune4)



tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- train(df3X, df3Y, method = 'xgbTree', tuneGrid = tune_grid5, verbose = FALSE, 
	trControl = fitControl)
ggplot(xgb_tune5)


(final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
))

nonlinearFit.gbm.tuned <- train(df3X, df3Y, method = 'xgbTree', tuneGrid = final_grid, verbose = FALSE, 
	trControl = fitControl)




gbmGrid <- expand.grid(interaction.depth = seq(10,16,by = 2), n.trees = seq(1100,1500,by = 100), 
  shrinkage = c(0.03),n.minobsinnode = c(9,11,13))
set.seed(4)
nonlinearFit1.gbm <- train(df1X, df1Y, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, 
	trControl = fitControl)

gbmGrid <- expand.grid(n.trees = as.numeric(nonlinearFit1.gbm$bestTune[1]), interaction.depth = as.numeric(nonlinearFit1.gbm$bestTune[2]), 
	shrinkage = as.numeric(nonlinearFit1.gbm$bestTune[3]), n.minobsinnode = as.numeric(nonlinearFit1.gbm$bestTune[4]))
set.seed(4)
nonlinearFit1.gbm.tuned <- train(df1X, df1Y, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, 
	trControl = fitControl)

set.seed(4)
gbmGrid <- expand.grid(interaction.depth = seq(10,12,by = 2), n.trees = seq(1500,1700,by = 100), 
  shrinkage = c(0.05,0.50),n.minobsinnode = c(3,5))
nonlinearFit2.gbm <- train(df2X, df2Y, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, 
	trControl = fitControl)

gbmGrid <- expand.grid(n.trees = as.numeric(nonlinearFit2.gbm$bestTune[1]), interaction.depth = as.numeric(nonlinearFit2.gbm$bestTune[2]), 
	shrinkage = as.numeric(nonlinearFit2.gbm$bestTune[3]), n.minobsinnode = as.numeric(nonlinearFit2.gbm$bestTune[4]))
set.seed(4)
nonlinearFit2.gbm.tuned <- train(df2X, df2Y, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, 
	trControl = fitControl)

gbmGrid <- expand.grid(interaction.depth = seq(10,16,by = 2), n.trees = seq(1100,1500,by = 100), 
  shrinkage = c(0.03),n.minobsinnode = c(9,11,13))
set.seed(4) 
nonlinearFit.gbm <- train(df3X, df3Y, method = 'gbm', verbose = FALSE, 
	trControl = fitControl)

gbmGrid <- expand.grid(n.trees = as.numeric(nonlinearFit.gbm$bestTune[1]), interaction.depth = as.numeric(nonlinearFit.gbm$bestTune[2]), 
	shrinkage = as.numeric(nonlinearFit.gbm$bestTune[3]), n.minobsinnode = as.numeric(nonlinearFit.gbm$bestTune[4]))
set.seed(4)
nonlinearFit.gbm.tuned <- train(df3X, df3Y, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, 
	trControl = fitControl)


#Validation Results

val.results <- data.frame(Model = c(rep(c('M1','M2'),4)), Method = c(rep('ENet',2), rep('SVM',2),rep('RF',2),rep('GBM',2)), RMSE = c(linearFit1.enet.tuned$results$RMSE,
	linearFit2.enet.tuned$results$RMSE,nonlinearFit1.svm.tuned$results$RMSE, nonlinearFit2.svm.tuned$results$RMSE,
	nonlinearFit1.rf.tuned$results$RMSE, nonlinearFit2.rf.tuned$results$RMSE, 
	nonlinearFit1.gbm.tuned$results$RMSE, nonlinearFit2.gbm.tuned$results$RMSE), RMSESD = c(linearFit1.enet.tuned$results$RMSESD,
	linearFit2.enet.tuned$results$RMSESD,nonlinearFit1.svm.tuned$results$RMSESD, nonlinearFit2.svm.tuned$results$RMSESD, nonlinearFit1.rf.tuned$results$RMSESD, nonlinearFit2.rf.tuned$results$RMSESD, 
	nonlinearFit1.gbm.tuned$results$RMSESD, nonlinearFit2.gbm.tuned$results$RMSESD))

p1 <- ggplot(val.results, aes(x = Method, y = RMSE, fill = Model)) + geom_bar(stat = 'identity', color = 'black', position = 
	position_dodge()) + scale_fill_manual(values = c('#56B4E9','#E69F00')) + 
geom_errorbar(aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD), width=.2,position=position_dodge(.9)) + theme_bw(base_size = 20)
ggsave('val-results.png')
#,"#56B4E9","#4f6548"#E69F00,#999999


val2.results <- data.frame(Model = c(rep(c('M1','M2'),4)), Method = c(rep('ENet',2), rep('SVM',2),rep('RF',2),rep('GBM',2)), MAE = c(linearFit1.enet.tuned$results$MAE,
	linearFit2.enet.tuned$results$MAE,nonlinearFit1.svm.tuned$results$MAE, nonlinearFit2.svm.tuned$results$MAE,
	nonlinearFit1.rf.tuned$results$MAE, nonlinearFit2.rf.tuned$results$MAE, 
	nonlinearFit1.gbm.tuned$results$MAE, nonlinearFit2.gbm.tuned$results$MAE), MAESD = c(linearFit1.enet.tuned$results$MAESD,
	linearFit2.enet.tuned$results$MAESD,nonlinearFit1.svm.tuned$results$MAESD, nonlinearFit2.svm.tuned$results$MAESD, nonlinearFit1.rf.tuned$results$MAESD, nonlinearFit2.rf.tuned$results$MAESD, 
	nonlinearFit1.gbm.tuned$results$MAESD, nonlinearFit2.gbm.tuned$results$MAESD))

p2 <- ggplot(val2.results, aes(x = Method, y = MAE, fill = Model)) + geom_bar(stat = 'identity', color = 'black', position = 
	position_dodge()) + scale_fill_manual(values = c('#999999','#E69F00')) + 
geom_errorbar(aes(ymin = MAE - MAESD, ymax = MAE + MAESD), width=.2,position=position_dodge(.9)) + theme_bw(base_size = 20)
ggsave('MAE-results12.png')



val3.results <- data.frame(Model = rep('M3',4), Method = c('Enet', 'SVM', 'RF', 'GBM'), 
	RMSE  = c(linearFit.enet.tuned$results$RMSE, nonlinearFit.svm.tuned$results$RMSE, nonlinearFit.rf.tuned$results$RMSE,
		nonlinearFit.gbm.tuned$results$RMSE), RMSESD = c(linearFit.enet.tuned$results$RMSESD, nonlinearFit.svm.tuned$results$RMSESD, nonlinearFit.rf.tuned$results$RMSESD,
	nonlinearFit.gbm.tuned$results$RMSESD))

p3 <- ggplot(val3.results, aes(x = Method, y = RMSE, fill = Model)) + geom_bar(stat = 'identity', color = 'black', position = 
	position_dodge()) + scale_fill_manual(values = c('#999999')) + 
geom_errorbar(aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD), width=.2,position=position_dodge(.9)) + 
geom_hline(yintercept = linearFit0$results$RMSE, color = '#BB0000', linetype = 'dashed', lwd = 1) + theme_bw(base_size = 20)
ggsave('valM3-results.png')
ggsave('valM3-results.pdf')


val4.results <- data.frame(Model = rep('M3',4), Method = c('Enet', 'SVM', 'RF', 'GBM'), 
	MAE  = c(linearFit.enet.tuned$results$MAE, nonlinearFit.svm.tuned$results$MAE, nonlinearFit.rf.tuned$results$MAE,
		nonlinearFit.gbm.tuned$results$MAE), MAESD = c(linearFit.enet.tuned$results$MAESD, nonlinearFit.svm.tuned$results$MAESD, nonlinearFit.rf.tuned$results$MAESD,
	nonlinearFit.gbm.tuned$results$MAESD))

p4 <- ggplot(val4.results, aes(x = Method, y = MAE, fill = Model)) + geom_bar(stat = 'identity', color = 'black', position = 
	position_dodge()) + scale_fill_manual(values = c('#999999')) + 
geom_errorbar(aes(ymin = MAE - MAESD, ymax = MAE + MAESD), width=.2,position=position_dodge(.9)) + 
geom_hline(yintercept = linearFit0$results$MAE, color = '#BB0000', linetype = 'dashed', lwd = 1) + theme_bw(base_size = 20)
ggsave('MAE-results34.png')

p1 <- ggplot(varImp(linearFit1.enet.tuned))
p2 <- ggplot(varImp(nonlinearFit1.svm.tuned))
p3 <- ggplot(varImp(nonlinearFit1.rf.tuned))
p4 <- ggplot(varImp(nonlinearFit1.gbm.tuned))