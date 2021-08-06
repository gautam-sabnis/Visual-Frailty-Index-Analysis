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

setwd('/Users/sabnig/Lab-Projects/Analysis/Visual-Frailty-Index')

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

df <- df0[,names(df0) %in% c('Overall.Score','MouseID','Collected.By','Body.Weight','Sex','Age.at.Test',
	avg_gait_measures_linear, median_gait_measures_linear, std_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_mean,engineered_features_median, ellipsefit_measures, 
	rears_measures, rearpaw_pose_measures)]
df <- df[complete.cases(df),] #remove 9 animals with missing values
names(df)[names(df) == 'Overall.Score'] <- 'score' 
names(df)[names(df) == 'Age.at.Test'] <- 'TestAge' 
names(df)[names(df) == 'Body.Weight'] <- 'Weight' 
names(df)[names(df) == 'Collected.By'] <- 'Tester' 
df$Tester <- factor(df$Tester, levels = c('Amanda','Gaven','Hannah','Mackenzie'))
levels(df$Tester) <- vapply(1:4, function(x) paste0('Scorer',x), character(1)) 
df$Sex <- as.factor(ifelse(df$Sex == 'Male', -1, 1)) 

#Adjust for the tester effect
mod.lmm <- lmer(score ~ (1|Tester), data = df) 
df$score <- ifelse(df$Tester == 'Scorer1', df$score - ranef(mod.lmm)$Tester[1,],
	ifelse(df$Tester == 'Scorer2', df$score - ranef(mod.lmm)$Tester[2,],
	ifelse(df$Tester == 'Scorer3', df$score - ranef(mod.lmm)$Tester[3,], 
	df$score - ranef(mod.lmm)$Tester[4,])))
df <- df[,-which(names(df) %in% c('Tester'))]

df <- df[,names(df) %in% c('MouseID','score','TestAge','Sex',median_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures, rears_measures)]


#Reviewer 2: Comparison between video features versus age alone in predicting visual FI 
#Approach 1: Age alone vs Video
#Approach 2: Regress out age and then create a vFI from the residuals. The residual vFI will focus on biological aging parameters
#Approach 3: Video vs Video + Age

set.seed(123)
nsim <- 10
quantiles <- c(0.025,0.5,0.975) #seq(0.1,0.9,length=9)
help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 403
splits <- lapply(seq(nsim), function(x) sample(help))
#metrics <- data.frame(matrix(0,nrow=nsim,ncol=6))
MAE <- data.frame(matrix(0,nrow=nsim,ncol=4))
RMSE <- data.frame(matrix(0,nrow=nsim,ncol=4))
R2 <- data.frame(matrix(0,nrow=nsim,ncol=4))


######################Predict Age
lapply(seq(nsim), function(i){

cat("Simulation Run = ", i, "\n")
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])


# mins
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

#Use Age to predict Frailty
#Age alone (Linear Model)
mod <- lm(score ~ TestAge, data = dfTrain)
yhat <- as.numeric(predict(mod, newdata = dfTest))

#Test Set Performance
MAE[i,1] <<- mean(abs(Ytest-yhat))
RMSE[i,1] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,1] <<- cor(Ytest,yhat)^2

#Age alone (Generalized Additive Model)
mod <- mgcv::gam(score ~ s(TestAge), data = dfTrain)
yhat <- as.numeric(predict(mod, newdata = dfTest))

#Test Set Performance
MAE[i,2] <<- mean(abs(Ytest-yhat))
RMSE[i,2] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,2] <<- cor(Ytest,yhat)^2


#Video
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
rfhat <- predict(model,Xtest)$predictions

#Test Set Performance
MAE[i,2] <<- mean(abs(Ytest-rfhat))
RMSE[i,2] <<- sqrt(mean((Ytest - rfhat)^2))
R2[i,2] <<- cor(Ytest,rfhat)^2

#Age + Video
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
rfhat <- predict(model,Xtest)$predictions

#Test Set Performance
MAE[i,3] <<- mean(abs(Ytest-rfhat))
RMSE[i,3] <<- sqrt(mean((Ytest - rfhat)^2))
R2[i,3] <<- cor(Ytest,rfhat)^2


#Approach 2: Residual vFI
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- resid(lm(score ~ TestAge, data = dfTrain))
Ytest <- resid(lm(score ~ TestAge, data = dfTest))

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
rfhat <- predict(model,Xtest)$predictions

#Test Set Performance
MAE[i,4] <<- mean(abs(Ytest-rfhat))
RMSE[i,4] <<- sqrt(mean((Ytest - rfhat)^2))
R2[i,4] <<- cor(Ytest,rfhat)^2


})

colnames(MAE) <- c("Age", "Video", "Age+Video","Residual FI")
colnames(RMSE) <- c("Age", "Video","Age+Video","Residual FI")
colnames(R2) <- c("Age", "Video","Age+Video","Residual FI")

MAE.melt <- reshape::melt(MAE)
RMSE.melt <- reshape::melt(RMSE)
R2.melt <- reshape::melt(R2)

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=40)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
ggplot(df.metrics,aes(x = variable, y = value)) + geom_point() + geom_boxplot() + 
labs(x='Model',y='Metric') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed)

dev.print(pdf,'Revision/Plots/Temp/tmp1.pdf')

tmp1 <- read.csv('Revision/Results/FI-MAE-Age-Video.csv',header=TRUE)
tmp2 <- read.csv('Revision/Results/FI-RMSE-Age-Video.csv',header=TRUE)
tmp3 <- read.csv('Revision/Results/FI-R2-Age-Video.csv',header=TRUE)

MAE.melt <- reshape::melt(tmp1[,-1])
RMSE.melt <- reshape::melt(tmp2[,-1])
R2.melt <- reshape::melt(tmp3[,-1])

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=200)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
levels(df.metrics$variable) <- c('Age(L)','Age(G)','Video','All')
ggplot(df.metrics,aes(x = variable, y = value)) + geom_boxplot() + 
geom_point(alpha = 0.5) + 
labs(x='Feature',y='Metric') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14), axis.title.x = element_text(size = 14), axis.title.y=element_text(size=14))

dev.print(pdf,'Revision/Plots/Age-Video-ResidualFI2.pdf')

#Feature importance plots for video vs all
set.seed(1234)
nsim <- 50
i <- which.min(tmp1[,3])
help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 403
splits <- lapply(seq(nsim), function(x) sample(help))
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])



# mins
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

#Use video features to predict frailty
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

model1 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)


#Age + Video
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

model2 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)



#Feature importances: Video vs Age+Video
Features <- c('Age','Center Time','Periphery Time','Corner Time','Center Distance','Periphery Distance','Corner Distance','Grooming Bouts','Grooming Duration','Angular Velocity','Base Tail LD','Limb Duty Factor','Nose LD','Speed','Step Length1','Step Length2','Step Width','Stride Length','Tip Tail LD','Stride Count','Distance','Angular Velocity IQR','Base Tail LD IQR','Limb Duty Factor IQR','Nose LD IQR','Speed IQR','Step Length1 IQR','Step Length2 IQR','Step Width IQR','Stride Length IQR','Tip Tail LD IQR','dAC','dB','aABC','Width','Length','Rearpaw','Rearcount','Rears_0_5')

set.seed(1234)
nsim <- 50
i <- which.min(tmp1[,3])
help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 403
splits <- lapply(seq(nsim), function(x) sample(help))
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])


# mins
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

#Video
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain[,sapply(Xtrain,is.numeric)] <- scale(Xtrain[,sapply(Xtrain,is.numeric)], center=TRUE, scale=TRUE)
Xtest[,sapply(Xtest,is.numeric)] <- scale(Xtest[,sapply(Xtest,is.numeric)], center=TRUE, scale=TRUE)

model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
imp_video <- grf::variable_importance(model)



#Age + Video
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
imp_agevideo <- grf::variable_importance(model)

df_imp <- data.frame(Feature = Features, Video = c(0,imp_video), 'Age+Video' = imp_agevideo)
df_imp_melt <- reshape2::melt(df_imp)
ggplot(df_imp_melt, aes(x=Feature,y=value,fill=variable)) + 
geom_bar(stat='identity',position='dodge') +  
theme_bw(base_size=18) + labs(x = 'Feature', y = 'Importance') + 
scale_fill_manual(name = "Features", values = c('#377eb8','#e41a1c'),
	labels = c('Video','Age + Video')) + 
theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1)) + theme(axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), axis.title.x = element_text(size = 18), axis.title.y=element_text(size=18))

dev.print(pdf,'Revision/Plots/Temp/imp-age-vs-video.pdf')



######
set.seed(123)
nsim <- 10
quantiles <- c(0.025,0.50,0.975) #seq(0.1,0.9,length=9)
#help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 403
#splits <- lapply(seq(nsim), function(x) sample(help))
#metrics <- data.frame(matrix(0,nrow=nsim,ncol=6))
MAE <- data.frame(matrix(0,nrow=nsim,ncol=4))
RMSE <- data.frame(matrix(0,nrow=nsim,ncol=4))
R2 <- data.frame(matrix(0,nrow=nsim,ncol=4))

tmp <- data.frame(matrix(0,nrow=nsim,ncol=4))

lapply(seq(nsim), function(i){

cat("Simulation Run = ", i, "\n")
#trIndex <- createDataPartition(df[!duplicated(df$MouseID),'TestAge'], p = 0.7, list = FALSE)
#teIndex <- setdiff(seq(length(unique(df$MouseID))),trIndex)

#Old animals
#trIndex <- union(which(df$TestAge < 100), sample(which(df$TestAge > 100), 50))
#teIndex <- setdiff(seq(nrow(df)), trIndex)

#Young animals
trIndex <- union(which(df$TestAge > 40), sample(which(df$TestAge < 40), 91))
teIndex <- setdiff(seq(nrow(df)), trIndex)

# mins
#dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
#dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- df[trIndex,]
dfTest <- df[teIndex,]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID


#Approach 1: Age alone vs Video

#Age alone
Xtrain <- dfTrain[,c("TestAge","Sex")]
Xtest <- dfTest[,c("TestAge","Sex")]
Ytrain <- dfTrain[,c("score")]
Ytest <- dfTest[,c("score")]
tmp[i,1] <<- var(Ytrain)
tmp[i,2] <<- var(Ytest)

Xtrain[,"TestAge"] <- scale(Xtrain[,"TestAge"], center=TRUE, scale=TRUE)
Xtest[,"TestAge"] <- scale(Xtest[,"TestAge"], center=TRUE, scale=TRUE)

#Xtrain <- model.matrix(~ 0 + ., Xtrain)
#Xtest <- model.matrix(~ 0 + ., Xtest)

#model <- caret::train(y = Ytrain, x = Xtrain, method = 'lm',trControl=trainControl(method='repeatedcv',repeats=1))
#yhat <- as.numeric(predict(model,newdata = Xtest))
df_train <- data.frame(score = dfTrain$score, TestAge = dfTrain$TestAge) 
df_test <- data.frame(score = dfTest$score, TestAge = dfTest$TestAge) 

model <- quantreg::rq(score ~ TestAge, tau = quantiles[1], data = df_train)
new <- data.frame(TestAge = (dfTest$TestAge))
yhat <- as.numeric(predict(model,newdata = new))

#Test Set Performance
MAE[i,1] <<- mean(abs(Ytest-yhat))
RMSE[i,1] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,1] <<- cor(Ytest,yhat)^2


#Video
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain[,-which(names(Xtrain) %in% c("Sex"))] <- scale(Xtrain[,-which(names(Xtrain) %in% c("Sex"))], center=TRUE, scale=TRUE)
Xtest[,-which(names(Xtest) %in% c("Sex"))] <- scale(Xtest[,-which(names(Xtest) %in% c("Sex"))], center=TRUE, scale=TRUE)

Xtrain <- model.matrix(~ 0 + ., Xtrain)
Xtest <- model.matrix(~ 0 + ., Xtest)


#model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
#rfhat <- predict(model,Xtest)$predictions

model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[1], honesty=FALSE)
rfhat <- predict(model,Xtest)

#Test Set Performance
MAE[i,2] <<- mean(abs(Ytest-rfhat))
RMSE[i,2] <<- sqrt(mean((Ytest - rfhat)^2))
R2[i,2] <<- cor(Ytest,rfhat)^2

tmp[i,1] <<- var(Ytrain)
tmp[i,2] <<- var(Ytest)

#Age + Video
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain[,-which(names(Xtrain) %in% c("Sex"))]<- scale(Xtrain[,-which(names(Xtrain) %in% c("Sex"))], center=TRUE, scale=TRUE)
Xtest[,-which(names(Xtest) %in% c("Sex"))] <- scale(Xtest[,-which(names(Xtest) %in% c("Sex"))], center=TRUE, scale=TRUE)


Xtrain <- model.matrix(~ 0 + ., Xtrain)
Xtest <- model.matrix(~ 0 + ., Xtest)
#model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
#rfhat <- predict(model,Xtest)$predictions
model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[1], honesty=FALSE)
rfhat <- predict(model,Xtest)


#Test Set Performance
MAE[i,3] <<- mean(abs(Ytest-rfhat))
RMSE[i,3] <<- sqrt(mean((Ytest - rfhat)^2))
R2[i,3] <<- cor(Ytest,rfhat)^2


#Approach 2: Residual vFI
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','TestAge'))]
Ytrain <- resid(lm(score ~ TestAge, data = dfTrain))
Ytest <- resid(lm(score ~ TestAge, data = dfTest))
tmp[i,3] <<- var(Ytrain)
tmp[i,4] <<- var(Ytest)

Xtrain[,-which(names(Xtrain) %in% c("Sex"))] <- scale(Xtrain[,-which(names(Xtrain) %in% c("Sex"))], center=TRUE, scale=TRUE)
Xtest[,-which(names(Xtest) %in% c("Sex"))] <- scale(Xtest[,-which(names(Xtest) %in% c("Sex"))], center=TRUE, scale=TRUE)

Xtrain <- model.matrix(~ 0 + ., Xtrain)
Xtest <- model.matrix(~ 0 + ., Xtest)
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
rfhat <- predict(model,Xtest)$predictions

#Test Set Performance
MAE[i,4] <<- mean(abs(Ytest-rfhat))
RMSE[i,4] <<- sqrt(mean((Ytest - rfhat)^2))
R2[i,4] <<- cor(Ytest,rfhat)^2


})

colnames(MAE) <- c("Age", "Video", "Age+Video","Residual FI")
colnames(RMSE) <- c("Age", "Video","Age+Video","Residual FI")
colnames(R2) <- c("Age", "Video","Age+Video","Residual FI")

MAE.melt <- reshape::melt(MAE)
RMSE.melt <- reshape::melt(RMSE)
R2.melt <- reshape::melt(R2)

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=40)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
ggplot(df.metrics,aes(x = variable, y = value)) + geom_point() + geom_boxplot() + 
labs(x='Features',y='Metric') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed)

dev.print(pdf,'Revision/Plots/Temp/Q025-prediction.pdf')


######
set.seed(123)
nsim <- 10
#quantiles <- c(0.85,0.95,0.975) 
quantiles <- c(0.025,0.05,0.15)
#help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 403
#splits <- lapply(seq(nsim), function(x) sample(help))
#metrics <- data.frame(matrix(0,nrow=nsim,ncol=6))
MAE <- data.frame(matrix(0,nrow=nsim,ncol=6))
RMSE <- data.frame(matrix(0,nrow=nsim,ncol=6))
R2 <- data.frame(matrix(0,nrow=nsim,ncol=6))


lapply(seq(nsim), function(i){

cat("Simulation Run = ", i, "\n")
#trIndex <- createDataPartition(df[!duplicated(df$MouseID),'TestAge'], p = 0.7, list = FALSE)
#teIndex <- setdiff(seq(length(unique(df$MouseID))),trIndex)

#Old animals
#trIndex <- union(which(df$TestAge < 100), sample(which(df$TestAge > 100), 50))
#teIndex <- setdiff(seq(nrow(df)), trIndex)

#Young animals
trIndex <- union(which(df$TestAge > 40), sample(which(df$TestAge < 40), 91))
teIndex <- setdiff(seq(nrow(df)), trIndex)

# mins
#dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
#dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- df[trIndex,]
dfTest <- df[teIndex,]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID


#Approach 1: Age alone vs Video

#Age alone
Xtrain <- dfTrain[,c("TestAge","Sex")]
Xtest <- dfTest[,c("TestAge","Sex")]
Ytrain <- dfTrain[,c("score")]
Ytest <- dfTest[,c("score")]

Xtrain[,"TestAge"] <- scale(Xtrain[,"TestAge"], center=TRUE, scale=TRUE)
Xtest[,"TestAge"] <- scale(Xtest[,"TestAge"], center=TRUE, scale=TRUE)

#Xtrain <- model.matrix(~ 0 + ., Xtrain)
#Xtest <- model.matrix(~ 0 + ., Xtest)

#model <- caret::train(y = Ytrain, x = Xtrain, method = 'lm',trControl=trainControl(method='repeatedcv',repeats=1))
#yhat <- as.numeric(predict(model,newdata = Xtest))
df_train <- data.frame(score = dfTrain$score, TestAge = dfTrain$TestAge) 
df_test <- data.frame(score = dfTest$score, TestAge = dfTest$TestAge) 

#Q1
model <- quantreg::rq(score ~ TestAge, tau = quantiles[1], data = df_train)
new <- data.frame(TestAge = (dfTest$TestAge))
yhat <- as.numeric(predict(model,newdata = new))

#Test Set Performance
MAE[i,1] <<- mean(abs(Ytest-yhat))
RMSE[i,1] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,1] <<- cor(Ytest,yhat)^2

#Q2
model <- quantreg::rq(score ~ TestAge, tau = quantiles[2], data = df_train)
new <- data.frame(TestAge = (dfTest$TestAge))
yhat <- as.numeric(predict(model,newdata = new))

#Test Set Performance
MAE[i,2] <<- mean(abs(Ytest-yhat))
RMSE[i,2] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,2] <<- cor(Ytest,yhat)^2

#Q3
model <- quantreg::rq(score ~ TestAge, tau = quantiles[3], data = df_train)
new <- data.frame(TestAge = (dfTest$TestAge))
yhat <- as.numeric(predict(model,newdata = new))

#Test Set Performance
MAE[i,3] <<- mean(abs(Ytest-yhat))
RMSE[i,3] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,3] <<- cor(Ytest,yhat)^2




#Video
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain[,-which(names(Xtrain) %in% c("Sex"))] <- scale(Xtrain[,-which(names(Xtrain) %in% c("Sex"))], center=TRUE, scale=TRUE)
Xtest[,-which(names(Xtest) %in% c("Sex"))] <- scale(Xtest[,-which(names(Xtest) %in% c("Sex"))], center=TRUE, scale=TRUE)

Xtrain <- model.matrix(~ 0 + ., Xtrain)
Xtest <- model.matrix(~ 0 + ., Xtest)


#model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
#rfhat <- predict(model,Xtest)$predictions

#Q1
model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[1], honesty=FALSE)
rfhat <- predict(model,Xtest)

#Test Set Performance
MAE[i,4] <<- mean(abs(Ytest-rfhat))
RMSE[i,4] <<- sqrt(mean((Ytest - rfhat)^2))
R2[i,4] <<- cor(Ytest,rfhat)^2


#Q2
model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[2], honesty=FALSE)
rfhat <- predict(model,Xtest)

#Test Set Performance
MAE[i,5] <<- mean(abs(Ytest-rfhat))
RMSE[i,5] <<- sqrt(mean((Ytest - rfhat)^2))
R2[i,5] <<- cor(Ytest,rfhat)^2

#Q3
model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[3], honesty=FALSE)
rfhat <- predict(model,Xtest)

#Test Set Performance
MAE[i,6] <<- mean(abs(Ytest-rfhat))
RMSE[i,6] <<- sqrt(mean((Ytest - rfhat)^2))
R2[i,6] <<- cor(Ytest,rfhat)^2



})


colnames(MAE) <- c("Age_Q1", "Age_Q2", "Age_Q3","Video_Q1","Video_Q2","Video_Q3")
colnames(RMSE) <- c("Age_Q1", "Age_Q2", "Age_Q3","Video_Q1","Video_Q2","Video_Q3")
colnames(R2) <- c("Age_Q1", "Age_Q2", "Age_Q3","Video_Q1","Video_Q2","Video_Q3")

MAE.melt <- reshape::melt(MAE)
RMSE.melt <- reshape::melt(RMSE)
R2.melt <- reshape::melt(R2)

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=60)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
ggplot(df.metrics,aes(x = variable, y = value)) + geom_point() + geom_boxplot() + 
labs(x='Features',y='Metric') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed)


dev.print(pdf,'Revision/Plots/Temp/predicting-old-animals.pdf')




######Comparing Age vs video at different quantiles (young, middle, old?)
set.seed(123)
nsim <- 10
#quantiles <- c(0.85,0.95,0.975) 
quantiles <- c(0.05,0.95)
#help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 403
#splits <- lapply(seq(nsim), function(x) sample(help))
#metrics <- data.frame(matrix(0,nrow=nsim,ncol=6))
MAE <- data.frame(matrix(0,nrow=nsim,ncol=6))
RMSE <- data.frame(matrix(0,nrow=nsim,ncol=6))
R2 <- data.frame(matrix(0,nrow=nsim,ncol=6))


lapply(seq(nsim), function(i){

cat("Simulation Run = ", i, "\n")

df0 <- df[!duplicated(df$MouseID), c("MouseID","TestAge")]

#Young animals
trIndex <- df0[union(which(df0$TestAge > 30), sample(which(df0$TestAge < 40), 91)),'MouseID']
teIndex <- setdiff(unique(df0$MouseID), trIndex)

dfTrain <- df[df$MouseID %in% trIndex,]
dfTest <- df[df$MouseID %in% teIndex,]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

#Age alone
Xtrain <- dfTrain[,c("TestAge")]
Xtest <- dfTest[,c("TestAge")]
Ytrain <- dfTrain[,c("score")]
Ytest <- dfTest[,c("score")]

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

df_train <- data.frame(score = dfTrain$score, TestAge = Xtrain) 
df_test <- data.frame(score = dfTest$score, TestAge = Xtest) 


model <- quantreg::rq(score ~ TestAge, tau = quantiles[1], data = df_train)
new <- data.frame(TestAge = (Xtest))
yhat <- as.numeric(predict(model,newdata = new))

MAE[i,1] <<- mean(abs(Ytest-yhat))
RMSE[i,1] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,1] <<- cor(Ytest,yhat)^2


#Video
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','TestAge','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','TestAge','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[1], honesty=FALSE)
yhat <- predict(model,Xtest)

MAE[i,2] <<- mean(abs(Ytest-yhat))
RMSE[i,2] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,2] <<- cor(Ytest,yhat)^2



#Stratified Split
trIndex <- df0[createDataPartition(df[!duplicated(df$MouseID),'TestAge'], p = 0.81, list = FALSE),'MouseID']
teIndex <- setdiff(unique(df0$MouseID),trIndex)

dfTrain <- df[df$MouseID %in% trIndex,]
dfTest <- df[df$MouseID %in% teIndex,]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

#Age alone
Xtrain <- dfTrain[,which(names(dfTrain) %in% c("TestAge"))]
Xtest <- dfTest[,which(names(dfTest) %in% c("TestAge"))]
Ytrain <- dfTrain[,c("score")]
Ytest <- dfTest[,c("score")]

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

df_train <- data.frame(score = Ytrain, TestAge = Xtrain)
df_test <- data.frame(score = Ytest, TestAge = Xtest)

#model <- caret::train(y = Ytrain, x = data.frame(Xtrain), method = 'lm',trControl=trainControl(method='repeatedcv',repeats=1))
model <- lm(score ~ TestAge, data = df_train)
new <- data.frame(TestAge = df_test$TestAge)
yhat <- as.numeric(predict(model,newdata = new))


MAE[i,3] <<- mean(abs(Ytest-yhat))
RMSE[i,3] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,3] <<- cor(Ytest,yhat)^2


#Video
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','TestAge','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','TestAge','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
yhat <- predict(model,Xtest)$predictions

MAE[i,4] <<- mean(abs(Ytest-yhat))
RMSE[i,4] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,4] <<- cor(Ytest,yhat)^2


#Old animals
trIndex <- df0[union(which(df$TestAge < 100), sample(which(df$TestAge > 100), 55)),'MouseID']
teIndex <- setdiff(unique(df0$MouseID), trIndex)

dfTrain <- df[df$MouseID %in% trIndex,]
dfTest <- df[df$MouseID %in% teIndex,]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID


#Age alone
Xtrain <- dfTrain[,c("TestAge")]
Xtest <- dfTest[,c("TestAge")]
Ytrain <- dfTrain[,c("score")]
Ytest <- dfTest[,c("score")]

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

df_train <- data.frame(score = dfTrain$score, TestAge = Xtrain) 
df_test <- data.frame(score = dfTest$score, TestAge = Xtest) 


model <- quantreg::rq(score ~ TestAge, tau = quantiles[2], data = df_train)
new <- data.frame(TestAge = (Xtest))
yhat <- as.numeric(predict(model,newdata = new))

MAE[i,5] <<- mean(abs(Ytest-yhat))
RMSE[i,5] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,5] <<- cor(Ytest,yhat)^2




#Video
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','TestAge','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','TestAge','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[2], honesty=FALSE)
yhat <- predict(model,Xtest)

#Test Set Performance
MAE[i,6] <<- mean(abs(Ytest-yhat))
RMSE[i,6] <<- sqrt(mean((Ytest - yhat)^2))
R2[i,6] <<- cor(Ytest,yhat)^2



})


colnames(MAE) <- c("A_1", "V_1", "A_2","V_2","A_3","V_3")
colnames(RMSE) <- c("A_1", "V_1", "A_2","V_2","A_3","V_3")
colnames(R2) <- c("A_1", "V_1", "A_2","V_2","A_3","V_3")

MAE.melt <- reshape::melt(MAE)
RMSE.melt <- reshape::melt(RMSE)
R2.melt <- reshape::melt(R2)

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=60)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
ggplot(df.metrics,aes(x = variable, y = value)) + geom_point() + geom_boxplot() + 
labs(x='Features',y='Metric') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed)


dev.print(pdf,'Revision/Plots/Temp/predicting-old-animals.pdf')

#Reviewer 2:Make a feature importance plot for the model that predicts Age. Compare this with Figure 5A

tmp1 <- read.csv('Data/Results/Revision/FI-MAE.csv',header=TRUE)
tmp2 <- read.csv('Data/Results/Revision/FI-RMSE.csv',header=TRUE)
tmp3 <- read.csv('Data/Results/Revision/FI-R2.csv',header=TRUE)

Features <- c('Center Time','Periphery Time','Corner Time','Center Distance','Periphery Distance','Corner Distance','Grooming Bouts','Grooming Duration','Angular Velocity','Base Tail LD','Limb Duty Factor','Nose LD','Speed','Step Length1','Step Length2','Step Width','Stride Length','Tip Tail LD','Stride Count','Distance','Angular Velocity IQR','Base Tail LD IQR','Limb Duty Factor IQR','Nose LD IQR','Speed IQR','Step Length1 IQR','Step Length2 IQR','Step Width IQR','Stride Length IQR','Tip Tail LD IQR','dAC','dB','aABC','Width','Length','Rearpaw','Rearcount','Rears_0_5')

set.seed(1234)
nsim <- 50
quantiles <- c(0.025,0.5,0.975) #seq(0.1,0.9,length=9)
help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 403
splits <- lapply(seq(nsim), function(x) sample(help))
i <- which(tmp1[,4] == sort(tmp1[,4])[26])
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)

quantiles <- c(0.025,0.50,0.975)
vi_list <- list()
modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[2], 
	honesty=FALSE)
vi_list[[1]] <- grf::variable_importance(modelGRF60)

Ytrain <- dfTrain[,'TestAge']
Ytest <- dfTest[,'TestAge']

modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[2], 
	honesty=FALSE)
vi_list[[2]] <- grf::variable_importance(modelGRF60)


df.tmp <- data.frame(importance = do.call(rbind,vi_list)) 
#df.tmp$feature <- colnames(Xtest)
df.tmp$feature <- rep(Features,2)
df.tmp$Value <- factor(rep(c('Predicted FI', 'Predicted Age'), each=38)) 
df.tmp$Value <- factor(df.tmp$Value, levels = c('Predicted FI', 'Predicted Age'))

ggplot(df.tmp, aes(x=feature,y=importance,fill=Value)) + 
geom_bar(stat='identity',position='dodge') +  
theme_bw(base_size=18) + labs(x = 'Feature', y = 'Importance') + 
scale_fill_manual(name = "Value", values = c('#377eb8','#e41a1c')) + theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1)) + theme(axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), axis.title.x = element_text(size = 18), axis.title.y=element_text(size=18))
dev.print(pdf,'Revision/Plots/feature-importance-age-vs-FI2.pdf', width=16,height=5)


#Further exploring the predictions with shapley values 
require(shapr)

Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
dfTrain <- data.frame(score = Ytrain, Xtrain)
model <- ranger::ranger(score ~ ., data = dfTrain)

explainer <- shapr::shapr(Xtrain, model, n_combinations = 2^12)

