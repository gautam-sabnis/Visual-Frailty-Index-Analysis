Phenos.lin.Nomen <- c('Angular Velocity',"Base Tail LD","Limb Duty Factor","Nose LD",
	"Speed","Step Length1","Step Length2","Step Width","Stride Length","Tip Tail LD")
frailty_parameters <- c('Alopecia','Loss.of.fur.colour','Dermatitis','Loss.of.whiskers','Coat.condition',
	'Piloerection','Cataracts','Eye.discharge.swelling','Microphthalmia','Corneal.opacity','Nasal.discharge',
	'Rectal.prolapse','Vaginal.uterine.','Diarrhea','Vestibular.disturbance','Vision.loss..Visual.Placing.',
	'Menace.reflex','Tail.stiffening','Gait.disorders','Tremor','Tumours','Distended.abdomen','Kyphosis',
	'Body.condition','Breathing.rate.depth','Malocclusions','Righting.Reflex')
avg_gait_measures_linear <- c('avg_angular_velocity','avg_base_tail_lateral_displacement',
	'avg_limb_duty_factor','avg_nose_lateral_displacement','avg_speed_cm_per_sec',
	'avg_step_length1','avg_step_length2','avg_step_width','avg_stride_length',
	'avg_tip_tail_lateral_displacement') #'avg_temporal_symmetry' is dropped
median_gait_measures_linear <- c('median_angular_velocity','median_base_tail_lateral_displacement',
	'median_limb_duty_factor','median_nose_lateral_displacement','median_speed_cm_per_sec',
	'median_step_length1','median_step_length2','median_step_width','median_stride_length',
	'median_tip_tail_lateral_displacement') #'median_temporal_symmetry' is dropped
var_gait_measures_linear <- c('angular_velocity_var','base_tail_lateral_displacement_var',
	'limb_duty_factor_var','nose_lateral_displacement_var','speed_cm_per_sec_var',
	'step_length1_var','step_length2_var','step_width_var','stride_length_var',
	'tip_tail_lateral_displacement_var')
std_gait_measures_linear <- c('angular_velocity_stdev','base_tail_lateral_displacement_stdev',
	'limb_duty_factor_stdev','nose_lateral_displacement_stdev','speed_cm_per_sec_stdev',
	'step_length1_stdev','step_length2_stdev','step_width_stdev','stride_length_stdev',
	'tip_tail_lateral_displacement_stdev')
iqr_gait_measures_linear <- c('angular_velocity_iqr','base_tail_lateral_displacement_iqr',
	'limb_duty_factor_iqr','nose_lateral_displacement_iqr','speed_cm_per_sec_iqr',
	'step_length1_iqr','step_length2_iqr','step_width_iqr','stride_length_iqr',
	'tip_tail_lateral_displacement_iqr')
OFA_measures <- c('stride_count','Distance.cm.sc','center_time_secs','periphery_time_secs','corner_time_secs',
	'center_distance_cm','periphery_distance_cm','corner_distance_cm','grooming_number_bouts',
	'grooming_duration_secs')

rearpaw_pose_measures <- c('median_rearpaw')
rears_measures <- c('rear_count','rears_0_5','rears_0_10')
ellipsefit_measures <- c('median_width', 'median_length')
engineered_features_mean <- c('dAC_mean','dB_mean','aABC_mean')
engineered_features_stdev <- c('dAC_stdev','dB_stdev','aABC_stdev')
engineered_features_min <- c('dAC_min','dB_min','aABC_min')
engineered_features_max <- c('dAC_max','dB_max','aABC_max')
engineered_features_median <- c('dAC_median','dB_median','aABC_median')

#setwd('/Users/sabnig/Lab-Projects/Analysis/Visual-Frailty-Index')
setwd('/projects/kumar-lab/sabnig/Projects/Visual-Frailty') 
require(lme4)
require(caret) 
#require(reshape)
require(kernlab)
require(elasticnet)
require(grf)
require(xgboost)

#Old - Batch 1
masterdfgaitfix <- read.csv('Data/New/masterdfgaitfix.csv', header=TRUE, stringsAsFactors=FALSE)
fixedflex <- read.csv('Data/New/fixedflex.csv', header=TRUE, stringsAsFactors=FALSE)
ellipsefit <- read.csv('Data/New/ellipsefit_all.csv', header=TRUE, stringsAsFactors=FALSE)
fixedflex$NetworkFilename <- ellipsefit$NetworkFilename
rearpaw <- read.csv('Data/New/rearpaw.csv', header=TRUE, stringsAsFactors=FALSE)
rear4all <- read.csv('Data/New/rear4all.csv', header=TRUE, stringsAsFactors=FALSE)
rear4bins <- read.csv('Data/New/rear4bins.csv', header=TRUE, stringsAsFactors=FALSE)
df_old <- Reduce(function(...) merge(...,by='NetworkFilename'), list(masterdfgaitfix,fixedflex,ellipsefit,rearpaw,rear4all,rear4bins))
names(df_old)[names(df_old) == 'MouseID.x'] <- 'MouseID'

masterdf <- read.csv('Data/New/masterdf.csv',header=TRUE, stringsAsFactors=FALSE)
rearpaw_new <- read.csv('Data/New/rearpaw_new.csv',header=TRUE, stringsAsFactors=FALSE)
df_new <- merge(masterdf,rearpaw_new, by='NetworkFilename')

common_cols <- intersect(colnames(df_old), colnames(df_new))
df0 <- rbind(df_old[,names(df_old) %in% common_cols], df_new[,names(df_new) %in% common_cols])

df <- df0[,names(df0) %in% c('NetworkFilename','Overall.Score','MouseID','Collected.By','Body.Weight','Sex','Age.at.Test',
	avg_gait_measures_linear, median_gait_measures_linear, std_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_mean,engineered_features_median, ellipsefit_measures, 
	rears_measures, rearpaw_pose_measures)]
df <- df[complete.cases(df),] #remove 9 animals with missing values
names(df)[names(df) == 'Overall.Score'] <- 'score' 
names(df)[names(df) == 'Age.at.Test'] <- 'TestAge' 
names(df)[names(df) == 'Body.Weight'] <- 'Weight' 
names(df)[names(df) == 'Collected.By'] <- 'Tester' 
df$Tester <- factor(df$Tester, levels = c('Amanda','Gaven','Hannah','Mackenzie'))
df$Sex <- as.factor(df$Sex) 

#Old - Batch 2
gaven_data <- read.csv('Data/gaven.csv', header = TRUE)
gaven_data$Tester <- as.factor(gaven_data$Collected.By)
gaven_data$score <- gaven_data$CFI.norm
gaven_data$TestAge <- gaven_data$Age.at.Test
gaven_data$Sex <- as.factor(gaven_data$Sex)
names(gaven_data)[names(gaven_data) == 'Body.Weight'] <- 'Weight'

#New data - Batch 3
newdata <- read.csv('Data/newestdf.csv', header=TRUE)
newdata$MouseID <- sapply(seq(nrow(newdata)), function(x) as.factor(gsub(".avi","",gsub(".*/","",(newdata$NetworkFilename)[x]))))
newdata$Tester <- newdata$CollectedBy
newdata$Tester <- factor(newdata$Tester, levels = c('Gaven','Mackenzie'))

newdata <- newdata[,names(newdata) %in% c('NetworkFilename','Overall.Score','MouseID','CollectedBy','Body.Weight','Sex','Age.at.Test',
	avg_gait_measures_linear, median_gait_measures_linear, std_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_mean,engineered_features_median, ellipsefit_measures, 
	rears_measures, rearpaw_pose_measures)]
newdata <- newdata[complete.cases(newdata),] #remove 9 animals with missing values
names(newdata)[names(newdata) == 'Overall.Score'] <- 'score' 
names(newdata)[names(newdata) == 'Age.at.Test'] <- 'TestAge' 
names(newdata)[names(newdata) == 'Body.Weight'] <- 'Weight' 
names(newdata)[names(newdata) == 'CollectedBy'] <- 'Tester' 
newdata$Tester <- factor(newdata$Tester, levels = c('Gaven','Mackenzie'))

#Old - Batch 1 + 2
common_cols <- intersect(colnames(df), colnames(gaven_data))
DF <- data.frame()
DF <- rbind(subset(df, select = common_cols),subset(gaven_data, select = common_cols))
DF$Batch <- as.factor(rep(c('Batch1','Batch2'), c(nrow(df),nrow(gaven_data))))



#Old + New - Batch 1 + 3
#common_cols <- intersect(colnames(df),colnames(newdata))
#DF <- data.frame()
#DF <- rbind(subset(df, select = common_cols),subset(newdata, select = common_cols))
#DF$Batch <- as.factor(rep(c('Batch1','Batch3'), c(nrow(df),nrow(newdata))))

#Old + New - Batch 1+2+3
#common_cols <- intersect(intersect(colnames(df),colnames(newdata)), colnames(gaven_data))
#DF <- data.frame()
#DF <- rbind(subset(df,select = common_cols), subset(gaven_data, select = common_cols), subset(newdata, select = common_cols))
#DF$Batch <- as.factor(rep(c('Batch1','Batch2','Batch3'), c(nrow(df),nrow(gaven_data),nrow(newdata))))

#Adjust for the tester and batch effects
mod.lmm <- lmer(score ~ Weight + Sex + (1|Tester) + (1|Batch), data = DF[!duplicated(DF$MouseID),])
df <- DF
df$Tester <- factor(df$Tester, levels = c('Amanda','Gaven','Hannah','Mackenzie'))
levels(df$Tester) <- vapply(1:4, function(x) paste0('Scorer',x), character(1)) 
df$score <- ifelse(df$Tester == 'Scorer1', df$score - ranef(mod.lmm)$Tester[1,],
	ifelse(df$Tester == 'Scorer2', df$score - ranef(mod.lmm)$Tester[2,],
	ifelse(df$Tester == 'Scorer3', df$score - ranef(mod.lmm)$Tester[3,], 
	df$score - ranef(mod.lmm)$Tester[4,])))
df$score <- ifelse(df$Batch == 'Batch1', df$score - ranef(mod.lmm)$Batch[1,], df$score - ranef(mod.lmm)$Batch[2,])
#df$score <- ifelse(df$Batch == 'Batch1', df$score - ranef(mod.lmm)$Batch[1,], ifelse(df$Batch == 'Batch2', df$score - ranef(mod.lmm)$Batch[2,], df$score - ranef(mod.lmm)$Batch[3,]))
df <- df[,-which(names(df) %in% c('Tester'))]

df <- df[,names(df) %in% c('MouseID','score','TestAge','Sex',median_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures, rears_measures)]
df <- df[,c('MouseID', setdiff(colnames(df),'MouseID'))]
df <- df[complete.cases(df),]

set.seed(1234)
nsim <- 50
quantiles <- c(0.5) #seq(0.1,0.9,length=9)
help <- rep(1:3,c(505,100,200)) #since length(unique(df$MouseID)) = 805
splits <- lapply(seq(nsim), function(x) sample(help))
#metrics <- data.frame(matrix(0,nrow=nsim,ncol=6))
MAE_age <- data.frame(matrix(0,nrow=nsim,ncol=4))
RMSE_age <- data.frame(matrix(0,nrow=nsim,ncol=4))
R2_age <- data.frame(matrix(0,nrow=nsim,ncol=4))
MAE_frailty <- data.frame(matrix(0,nrow=nsim,ncol=4))
RMSE_frailty <- data.frame(matrix(0,nrow=nsim,ncol=4))
R2_frailty <- data.frame(matrix(0,nrow=nsim,ncol=4))


lapply(seq(nsim), function(i){

cat("Simulation Run = ", i, "\n")
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])


######################Predict Age
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==2],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'TestAge']
Ytest <- dfTest[,'TestAge']

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

#Elastic Net
enetfit <- caret::train(y=Ytrain,x=Xtrain, method='enet',
	trControl=trainControl(method='repeatedcv',repeats=1),tuneLength=10)
enethat <- predict(enetfit,newdata=Xtest)

#SVM 
svmfit <- caret::train(y=Ytrain, x=Xtrain, method = 'svmLinear', 
	trControl = trainControl(method='repeatedcv',repeats=1))
svmhat <- predict(svmfit,newdata=Xtest)

#Mean and median random forests 
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
rfhat <- predict(model,Xtest)$predictions


#Extreme Gradient Boosting:Boosted Regression Trees 
xgbfit <- caret::train(y=Ytrain, x=Xtrain, method='xgbTree',objective='reg:squarederror',
	trControl=trainControl(method='repeatedcv',repeats=1))
xgbhat <- predict(xgbfit,newdata=Xtest)

MAE_age[i,] <<- c(mean(abs(Ytest-enethat)),mean(abs(Ytest-svmhat)),mean(abs(Ytest-rfhat)),mean(abs(Ytest-xgbhat)))
RMSE_age[i,] <<- c(sqrt(mean((Ytest-enethat)^2)),sqrt(mean((Ytest-svmhat)^2)),sqrt(mean((Ytest-rfhat)^2)),sqrt(mean((Ytest-xgbhat)^2)))
R2_age[i,] <<- c(cor(Ytest,enethat)^2,cor(Ytest,svmhat)^2,cor(Ytest,rfhat)^2,cor(Ytest,xgbhat)^2)

######################Predict Frailty
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

#Elastic Net
enetfit <- caret::train(y=Ytrain,x=Xtrain, method='enet',
	trControl=trainControl(method='repeatedcv',repeats=1),tuneLength=10)
enethat <- predict(enetfit,newdata=Xtest)

#SVM 
svmfit <- caret::train(y=Ytrain, x=Xtrain, method = 'svmLinear', 
	trControl = trainControl(method='repeatedcv',repeats=1))
svmhat <- predict(svmfit,newdata=Xtest)

#Mean and median random forests 
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
rfhat <- predict(model,Xtest)$predictions


#Extreme Gradient Boosting:Boosted Regression Trees 
xgbfit <- caret::train(y=Ytrain, x=Xtrain, method='xgbTree',objective='reg:squarederror',
	trControl=trainControl(method='repeatedcv',repeats=1))
xgbhat <- predict(xgbfit,newdata=Xtest)

MAE_frailty[i,] <<- c(mean(abs(Ytest-enethat)),mean(abs(Ytest-svmhat)),mean(abs(Ytest-rfhat)),mean(abs(Ytest-xgbhat)))
RMSE_frailty[i,] <<- c(sqrt(mean((Ytest-enethat)^2)),sqrt(mean((Ytest-svmhat)^2)),sqrt(mean((Ytest-rfhat)^2)),sqrt(mean((Ytest-xgbhat)^2)))
R2_frailty[i,] <<- c(cor(Ytest,enethat)^2,cor(Ytest,svmhat)^2,cor(Ytest,rfhat)^2,cor(Ytest,xgbhat)^2)



})

colnames(MAE_age) <- c("LR-Enet","SVM","RF","XGB")
colnames(RMSE_age) <- c("LR-Enet","SVM","RF","XGB")
colnames(R2_age) <- c("LR-Enet","SVM","RF","XGB")

write.csv(x=MAE_age,file='Results/Age-MAE-new.csv')
write.csv(x=RMSE_age,file='Results/Age-RMSE-new.csv') 
write.csv(x=R2_age,file='Results/Age-R2-new.csv')

colnames(MAE_frailty) <- c("LR-Enet","SVM","RF","XGB")
colnames(RMSE_frailty) <- c("LR-Enet","SVM","RF","XGB")
colnames(R2_frailty) <- c("LR-Enet","SVM","RF","XGB")

write.csv(x=MAE_frailty,file='Results/FI-MAE-new.csv')
write.csv(x=RMSE_frailty,file='Results/FI-RMSE-new.csv') 
write.csv(x=R2_frailty,file='Results/FI-R2-new.csv')


