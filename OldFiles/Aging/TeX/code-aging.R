rm(list = ls())
libraries <- c('glmnet','leaps','caret','e1071','cowplot')
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
names(aging)[names(aging) == 'Overall.Score'] <- 'score'
names(aging)[names(aging) == 'Age.at.Test'] <- 'TestAge'
names(aging)[names(aging) == 'Body.Weight'] <- 'Weight'
names(aging)[names(aging) == 'Collected.By'] <- 'Tester'
aging$Tester <- factor(aging$Tester, levels = c('Amanda','Gaven','Hannah','Mackenzie'))
aging$Sex <- factor(aging$Sex, levels = c('-1','1'))
#levels(aging$Sex) <- c('Male','Female')

df <- aging[,names(aging) %in% c('score',animal_features,
	OFA_measures,engineered_features_mean, avg_gait_measures_linear, engineered_features_mean, engineered_features_stdev)]


corr.mat <- cor(df[,-which(names(df) %in% c('score','Sex','Tester'))])[upper.tri(cor(df[,-which(names(df) %in% c('score','Sex','Tester'))]), diag = FALSE)]
A <- ggplot(data.frame(corr.mat), aes(x = corr.mat)) + geom_histogram(bins = 10, color = 'black') + labs(x = 'Correlation', y =' Frequency') + 
ggtitle('Summary of Correlations')

corr.mat <- cor(df[,sapply(df,is.numeric)])
summary(corr.mat[upper.tri(corr.mat,diag = FALSE)])

pp_df <- preProcess(df[,-which(names(df) %in% c('score','Sex','Tester'))], method = c('center','scale'))
df.X <- predict(pp_df, newdata = df[,-which(names(df) %in% c('score','Sex','Tester'))])
X.cov <- cov(df.X[,sapply(df.X, is.numeric)])
X.eigen <- eigen(X.cov)
B <- ggplot(data.frame(X.eigen$values), aes(y = X.eigen.values/sum(X.eigen.values), x = seq(length(X.eigen.values)))) + 
geom_point() + geom_line() + labs(x = 'PC', y = '% Variance Explained')

pp_df <- preProcess(df[,-which(names(df) %in% c('score','Sex','Tester'))], method = c('center','scale'))
df.X <- predict(pp_df, newdata = df[,-which(names(df) %in% c('score','Sex','Tester'))])
df.X.ica <- fastICA(df.X, 3)
X.ica <- df.X.ica$S
X.pca <- as.matrix(df.X.ica$X) %*% as.matrix(df.X.ica$K) 

df.pca <- cbind(score = df$score,data.frame(X.pca))
C1 <- ggplot(df.pca, aes(y = score, x = X1)) + geom_point() + stat_smooth(method = 'loess') + labs(x = 'PC1')
C2 <- ggplot(df.pca, aes(y = score, x = X2)) + geom_point() + stat_smooth(method = 'loess') + labs(x = 'PC2')
C3 <- ggplot(df.pca, aes(y = score, x = X3)) + geom_point() + stat_smooth(method = 'loess') + labs(x = 'PC3')

df.ica <- cbind(score = df$score, data.frame(X.ica))
D1 <- ggplot(df.ica, aes(y = score, x = X1)) + geom_point() + stat_smooth(method = 'loess') + labs(x = 'IC1')
D2 <- ggplot(df.ica, aes(y = score, x = X2)) + geom_point() + stat_smooth(method = 'loess') + labs(x = 'IC2')
D3 <- ggplot(df.ica, aes(y = score, x = X3)) + geom_point() + stat_smooth(method = 'loess') + labs(x = 'IC3')

plot_grid(A,B,arrangeGrob(D1,D2,D3), nrow = 2, ncol = 2, labels = c('A','B','C'))
ggsave('/Users/sabnig/Documents/KLMeetings/Lab Meetings/Meeting3/Plots/PCvsIC.pdf')


#Data preprocess
set.seed(100)
trainIndex <- createDataPartition(df$score, p = 0.7, times = 1, list = FALSE)
dfTrain <- df[trainIndex,]
names(dfTrain) <- colnames(dfTrain)
dfTrainY <- dfTrain$score
dfTrainX <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
#dfTrainXcat <- model.matrix(score ~ Sex, data = dfTrain)
#dfTrainX <- cbind(dfTrainXnum,dfTrainXcat)
dfTest <- df[-trainIndex,]
dfTestX <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
names(dfTest) <- colnames(dfTest)
dfTestY <- dfTest$score

#Table 1
tmp <- table(df$Sex, df$score)
chisq.test(tmp)


#Figure 1
g1 <- ggplot(df, aes(x = score)) + geom_histogram(fill = '#08306b', alpha = 0.5, binwidth = 1) + 
labs(x = 'Score', y = 'Frequency') + ggtitle('Original Data') + theme_minimal(base_size = 18)
g2 <- ggplot(data.frame(dfTrainY), aes(x = dfTrainY)) + geom_histogram(fill = '#08306b', alpha = 0.5, 
	binwidth = 1) + labs(x = 'Score', y = 'Frequency') + ggtitle('Training Data') + theme_minimal(base_size = 18)
g3 <- ggplot(data.frame(dfTestY), aes(x = dfTestY)) + geom_histogram(fill = '#08306b', alpha = 0.5, 
	binwidth = 1) + labs(x = 'Score', y = 'Frequency') + ggtitle('Testing Data') + theme_minimal(base_size = 18)
plot_grid(g1,g2,g3, nrow = 1, align = 'h')
ggsave('../TeX/TeXPlots/figure1.pdf')




fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10)

#Data preprocess
set.seed(100)
trainIndex <- createDataPartition(df$score, p = 0.8, times = 1, list = FALSE)
dfTrain <- df[trainIndex,]
names(dfTrain) <- colnames(dfTrain)
dfTrainY <- dfTrain$score
dfTrainX <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
#dfTrainXcat <- model.matrix(score ~ Sex, data = dfTrain)
#dfTrainX <- cbind(dfTrainXnum,dfTrainXcat)
dfTest <- df[-trainIndex,]
names(dfTest) <- colnames(dfTest)
dfTestX <- dfTest[,-which(names(dfTest) %in% c('score'))]
dfTestY <- dfTest$score

idGrid <- expand.grid(.n.comp = seq(1,10,1)) 
set.seed(4)
linearFit.IC <- train(dfTrainX,dfTrainY, method = 'icr', trControl = fitControl, 
  preProc = c('center','scale'), tuneGrid = idGrid)
A <- ggplot(linearFit.IC)

idGrid <- expand.grid(.ncomp = seq(1,10,1)) 
set.seed(4)
linearFit.pcr <- train(dfTrainX,dfTrainY, method = 'pcr', trControl = fitControl, 
  preProc = c('center','scale'), tuneGrid = idGrid)
B <- ggplot(linearFit.pcr)

perf.pcr <- data.frame(true = dfTestY, pred = predict(linearFit.pcr, dfTestX))

set.seed(4)
linearFit.pls <- train(dfTrainX, dfTrainY, method = 'pls', tuneLength = 20, trControl = fitControl,
  preProc = c('center','scale'))
C <- ggplot(linearFit.pls)

perf.pls <- data.frame(true = dfTestY, pred = predict(linearFit.pls, dfTestX))

plot_grid(B + ggtitle('PCR') ,C + ggtitle('PLS'),nrow = 1, align = 'h')
ggsave('/Users/sabnig/Documents/KLMeetings/Lab Meetings/Meeting3/Plots/IC-PC-PLS.pdf')


glmn_grid <- expand.grid(alpha = seq(0, 1, by = .25), lambda = 10^seq(-3, -.5, length = 20))
set.seed(4)
linearFit.enet <- train(dfTrainX, dfTrainY, method = 'glmnet', tuneGrid = glmn_grid, 
  trControl = fitControl, preProc = c('center','scale'))
A <- ggplot(linearFit.enet) + theme(legend.position = 'top')
B <- ggplot(varImp(linearFit.enet))
plot_grid(A,B,nrow = 1, align = 'h',labels = c('A','B'),axis = 0)
ggsave('/Users/sabnig/Documents/KLMeetings/Lab Meetings/Meeting3/Plots/linearFits.pdf')

perf.enet <- data.frame(true = dfTestY, pred = predict(linearFit.enet, dfTestX))



set.seed(4)
nonlinearFit.svm <- train(dfTrainX, dfTrainY, method = 'svmRadial', preProc = c('center','scale'), tuneLength = 14, 
  trControl = fitControl)
perf.svm <- data.frame(true = dfTestY, pred = predict(nonlinearFit.svm, dfTestX))
set.seed(4)
nonlinearFit.rf <- train(score ~ ., data = dfTrain, method = 'rf', preProc = c('center','scale'),
	trControl = fitControl)
perf.rf <- data.frame(true = dfTestY, pred = predict(nonlinearFit.rf, dfTestX))

gbmGrid <- expand.grid(interaction.depth = seq(1,11,by = 2), n.trees = seq(100,1000,by = 50), 
  shrinkage = c(0.01,0.02),n.minobsinnode = 10)
set.seed(4)
nonlinearFit.gbm <- train(dfTrainX, dfTrainY, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, trControl = fitControl)
perf.gbm <- data.frame(true = dfTestY, pred = predict(nonlinearFit.gbm, dfTestX))

A <- ggplot(varImp(nonlinearFit.svm))
B <- ggplot(varImp(nonlinearFit.rf))
C <- ggplot(nonlinearFit.gbm)

plot_grid(A + ggtitle('SVM'), B + ggtitle('Random Forest'), C + ggtitle('GBM'), nrow = 1, align = 'h', axis = 0)
ggsave('/Users/sabnig/Documents/KLMeetings/Lab Meetings/Meeting3/Plots/nonlinearFits.pdf')


test.perf <- data.frame(rmse = c(rmse(perf.pcr, truth = true, estimate = pred)$.estimate,
	rmse(perf.pls, truth = true, estimate = pred)$.estimate,
	rmse(perf.enet, truth = true, estimate = pred)$.estimate,
	rmse(perf.svm, truth = true, estimate = pred)$.estimate,
	rmse(perf.rf, truth = true, estimate = pred)$.estimate,
	rmse(perf.gbm, truth = true, estimate = pred)$.estimate), 
	model = c('PCR','PLS','E-Net','SVM','RF','GBM'))
ggplot(test.perf, aes(x = model, y = rmse)) + geom_bar(stat = 'identity') + labs(x = 'Models', y = 'RMSE') + 
ggtitle('Test Set Performance')
ggsave('/Users/sabnig/Documents/KLMeetings/Lab Meetings/Meeting3/Plots/Test-Perf.pdf')


#Classification

glmnGrid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),lambda = seq(.01, .2, length = 40))
set.seed(4)
linearFit.glmnet <- train(dfTrainX, dfTrainY, method = 'glmnet',tuneGrid = glmnGrid, preProc = c("center", "scale"),
	metric = "ROC",trControl = fitControl)
A <- ggplot(linearFit.glmnet) + theme(legend.position = 'top')
B <- ggplot(varImp(linearFit.glmnet))


plsGrid <- expand.grid(ncomp = 1:10)
set.seed(4)
linearFit.pls <- train(dfTrainX, dfTrainY, method = 'pls', tuneGrid = plsGrid, metric = 'ROC', trControl = fitControl, preProc = c("center","scale")) 


plot_grid(A,B,nrow = 1, align = 'h', axis = 0)
ggsave('/Users/sabnig/Documents/KLMeetings/Lab Meetings/Meeting3/Plots/classification1.pdf')

set.seed(4)
sigmaRangeFull <- sigest(as.matrix(dfTrainX))
svmGrid <- expand.grid(sigma = as.vector(sigmaRangeFull)[1], C = 2^(-3:4))
set.seed(4)
nonlinearFit.svm <- train(dfTrainX, dfTrainY, method = 'svmRadial', metric = 'ROC', preProc = c('center','scale'),
tuneGrid = svmGrid, trControl = fitControl)

set.seed(4)
nonlinearFit.rf <- train(score ~ ., data = dfTrain, method = 'rf', preProc = c('center','scale'),
	trControl = fitControl, tuneGrid = expand.grid(mtry = seq(2,ncol(dfTrainX), by = 10), node_size = seq(3,9,by = 2), 
	sampe_size = c(.55,.632,.70,.80))

gbmGrid <- expand.grid(interaction.depth = seq(1,11,by = 2), n.trees = seq(100,1000,by = 50), 
  shrinkage = c(0.01,0.02),n.minobsinnode = 10)
set.seed(4)
nonlinearFit.gbm <- train(dfTrainX, dfTrainY, method = 'gbm', tuneGrid = gbmGrid, verbose = FALSE, trControl = fitControl)

A <- ggplot(varImp(nonlinearFit.svm))
B <- ggplot(varImp(nonlinearFit.rf))
C <- ggplot(nonlinearFit.gbm)
plot_grid(A + ggtitle('SVM'),B+ggtitle('RF'),C, nrow = 1, align = 'h', axis = 0)
ggsave('/Users/sabnig/Documents/KLMeetings/Lab Meetings/Meeting3/Plots/classification2.pdf')


lda.roc <- roc(response = dfTestY, predictor = predict(linearFit.LDA, dfTestX, type = 'prob')[,1])
pls.roc <- roc(response = dfTestY, predictor = predict(linearFit.pls, dfTestX, type = 'prob')[,1])