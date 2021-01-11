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

p1 <- ggplot(df,aes(x=TestAge)) + geom_density(color='#e41a1c') + labs(x = 'Age', y = 'Density') + theme_bw(base_size=18)
p2 <- ggplot(df,aes(x=score)) + geom_density(color='#377eb8') + labs(x = 'Score', y = 'Density') + theme_bw(base_size=18)

df1 <- df[,names(df) %in% c('TestAge',median_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures, rears_measures)]
corr.df <- data.frame(feature = names(df1[,-1]), corr = as.numeric(apply(df1[,-which(names(df1) %in% c('TestAge'))], 
	2, function(x) cor(df1$TestAge,x, use="complete.obs"))))
corr.df$Type <- ifelse(corr.df$feature %in% c(median_gait_measures_linear, iqr_gait_measures_linear), 'Gait',
	ifelse(corr.df$feature %in% c(OFA_measures), 'OFA', 'Engineered'))
corr.df$Type <- factor(corr.df$Type,levels=c('Gait','OFA','Engineered'))
p4 <- ggplot(corr.df, aes(x = seq(1,38), y = abs(corr), color = Type)) + geom_point(size = 3, stroke = 1, alpha = 0.8) + 
labs(x = 'Feature', y = 'Absolute Correlation') + scale_color_manual(values=c('#4daf4a','#984ea3','#ff7f00')) + 
theme_bw(base_size = 18) + theme(legend.position = 'none')

set.seed(1234)
nsim <- 20
quantiles <- c(0.5) #seq(0.1,0.9,length=9)
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
#model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
#		honesty=FALSE)
#yhat <- predict(model,Xtest)

#Extreme Gradient Boosting:Boosted Regression Trees 
xgbfit <- caret::train(y=Ytrain, x=Xtrain, method='xgbTree',objective = 'reg:squarederror',
	trControl=trainControl(method='repeatedcv',repeats=1))
xgbhat <- predict(xgbfit,newdata=Xtest)

#Test Set Performance
MAE[i,] <<- c(mean(abs(Ytest-enethat)),mean(abs(Ytest-svmhat)),mean(abs(Ytest-rfhat)),mean(abs(Ytest-xgbhat)))
RMSE[i,] <<- c(sqrt(mean((Ytest-enethat)^2)),sqrt(mean((Ytest-svmhat)^2)),sqrt(mean((Ytest-rfhat)^2)),sqrt(mean((Ytest-xgbhat)^2)))
R2[i,] <<- c(cor(Ytest,enethat)^2,cor(Ytest,svmhat)^2,cor(Ytest,rfhat)^2,cor(Ytest,xgbhat)^2)

})

colnames(MAE) <- c("LR-Enet","SVM","RF","XGB")
colnames(RMSE) <- c("LR-Enet","SVM","RF","XGB")
colnames(R2) <- c("LR-Enet","SVM","RF","XGB")

MAE.melt <- reshape::melt(MAE)
RMSE.melt <- reshape::melt(RMSE)
R2.melt <- reshape::melt(R2)

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=80)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
p1 <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_point() + geom_boxplot() + 
labs(x='Model',y='Metric') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed)



#######################Predict Frailty
set.seed(1234)
nsim <- 20
quantiles <- c(0.5) #seq(0.1,0.9,length=9)
help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 403
splits <- lapply(seq(nsim), function(x) sample(help))
#metrics <- data.frame(matrix(0,nrow=nsim,ncol=6))
MAE <- data.frame(matrix(0,nrow=nsim,ncol=4))
RMSE <- data.frame(matrix(0,nrow=nsim,ncol=4))
R2 <- data.frame(matrix(0,nrow=nsim,ncol=4))

lapply(seq(nsim), function(i){

cat("Simulation Run = ", i, "\n")
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])


# mins
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

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
#model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
#		honesty=FALSE)
#yhat <- predict(model,Xtest)

#Extreme Gradient Boosting:Boosted Regression Trees 
xgbfit <- caret::train(y=Ytrain, x=Xtrain, method='xgbTree',
	trControl=trainControl(method='repeatedcv',repeats=1))
xgbhat <- predict(xgbfit,newdata=Xtest)

#Test Set Performance
MAE[i,] <<- c(mean(abs(Ytest-enethat)),mean(abs(Ytest-svmhat)),mean(abs(Ytest-rfhat)),mean(abs(Ytest-xgbhat)))
RMSE[i,] <<- c(sqrt(mean((Ytest-enethat)^2)),sqrt(mean((Ytest-svmhat)^2)),sqrt(mean((Ytest-rfhat)^2)),sqrt(mean((Ytest-xgbhat)^2)))
R2[i,] <<- c(cor(Ytest,enethat)^2,cor(Ytest,svmhat)^2,cor(Ytest,rfhat)^2,cor(Ytest,xgbhat)^2)

})

colnames(MAE) <- c("LR-Enet","SVM","RF","XGB")
colnames(RMSE) <- c("LR-Enet","SVM","RF","XGB")
colnames(R2) <- c("LR-Enet","SVM","RF","XGB")

MAE.melt <- reshape::melt(MAE)
RMSE.melt <- reshape::melt(RMSE)
R2.melt <- reshape::melt(R2)

df <- rbind(MAE.melt,RMSE.melt,R2.melt)
df$Metric <- rep(c('MAE','RMSE','R2'), each=80)
df$Metric <- factor(df$Metric, levels = c('MAE','RMSE','R2'))
levels(df$Metric) <- c('MAE','RMSE','R^2')
p2 <- ggplot(df,aes(x = variable, y = value)) + geom_point() + geom_boxplot() + 
labs(x='Model',y='Metric') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed)


############
tmp1 <- read.csv('Data/Results/Age-MAE.csv',header=TRUE)
tmp2 <- read.csv('Data/Results/Age-RMSE.csv',header=TRUE)
tmp3 <- read.csv('Data/Results/Age-R2.csv',header=TRUE)

MAE.melt <- reshape::melt(tmp1[,-1])
RMSE.melt <- reshape::melt(tmp2[,-1])
R2.melt <- reshape::melt(tmp3[,-1])

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=200)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
levels(df.metrics$variable) <- c('LR*','SVM','RF','XGB')
p1 <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_point(alpha = 0.5) + geom_boxplot(fill = '#e41a1c', alpha = 0.7) + 
labs(x='Model',y='Metric (weeks)') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed)

#aov <- aov(value ~ variable, data=MAE.melt)
#summary(aov) 
#summary(glht(aov, linfct = mcp(variable='Dunnett')))

#aov <- aov(value ~ variable, data=RMSE.melt)
#summary(aov) 
#summary(glht(aov, linfct = mcp(variable='Dunnett')))

#aov <- aov(value ~ variable, data=R2.melt)
#summary(aov) 
#summary(glht(aov, linfct = mcp(variable='Dunnett')))

MAE.melt$sim <- rep(1:50,4)
tmp <- lmer(value ~ variable + (1|sim), data = MAE.melt)
anova(tmp) 

RMSE.melt$sim <- rep(1:50,4)
tmp <- lmer(value ~ variable + (1|sim), data = RMSE.melt)
anova(tmp)

R2.melt$sim <- rep(1:50,4)
tmp <- lmer(value ~ variable + (1|sim), data = R2.melt)
anova(tmp)

tmp1 <- read.csv('Data/Results/FI-MAE.csv',header=TRUE)
tmp2 <- read.csv('Data/Results/FI-RMSE.csv',header=TRUE)
tmp3 <- read.csv('Data/Results/FI-R2.csv',header=TRUE)

MAE.melt <- reshape::melt(tmp1[,-1])
RMSE.melt <- reshape::melt(tmp2[,-1])
R2.melt <- reshape::melt(tmp3[,-1])

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=200)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
levels(df.metrics$variable) <- c('LR*','SVM','RF','XGB')
p2 <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_point(alpha = 0.5) + geom_boxplot(fill = '#377eb8', alpha = 0.7) + 
labs(x='Model',y='Metric') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed)

#aov <- aov(value ~ variable, data=MAE.melt)
#summary(aov) 
#summary(glht(aov, linfct = mcp(variable='Dunnett')))

#aov <- aov(value ~ variable, data=RMSE.melt)
#summary(aov) 
#summary(glht(aov, linfct = mcp(variable='Dunnett')))

#aov <- aov(value ~ variable, data=R2.melt)
#summary(aov) 
#summary(glht(aov, linfct = mcp(variable='Dunnett')))

#Can't do regular anova as above since there is a dependence between test samples 

MAE.melt$sim <- rep(1:50,4)
tmp <- lmer(value ~ variable + (1|sim), data = MAE.melt)
anova(tmp) 

RMSE.melt$sim <- rep(1:50,4)
tmp <- lmer(value ~ variable + (1|sim), data = RMSE.melt)
anova(tmp)

R2.melt$sim <- rep(1:50,4)
tmp <- lmer(value ~ variable + (1|sim), data = R2.melt)
anova(tmp)

p1|p2
dev.print(pdf,'Temp/TestResults.pdf',width=14,height = 3)

#####
tmp1a <- read.csv('Data/Results/YesAge-MAE.csv',header=TRUE)
tmp2a <- read.csv('Data/Results/YesAge-RMSE.csv',header=TRUE)
tmp3a <- read.csv('Data/Results/YesAge-R2.csv',header=TRUE)
tmp1n <- read.csv('Data/Results/NoAge-MAE.csv',header=TRUE)
tmp2n <- read.csv('Data/Results/NoAge-RMSE.csv',header=TRUE)
tmp3n <- read.csv('Data/Results/NoAge-R2.csv',header=TRUE)

tmp1a <- tmp1a[,4]
tmp2a <- tmp2a[,4]
tmp3a <- tmp3a[,4]
tmp1n <- tmp1n[,4]
tmp2n <- tmp2n[,4]
tmp3n <- tmp3n[,4]

df.mae <- data.frame(Error = c(tmp1a,tmp1n), Type = rep(c('Age','-Age'), each=50))
df.rmse <- data.frame(Error = c(tmp2a,tmp2n), Type = rep(c('Age','-Age'), each=50))
df.R2 <- data.frame(Error = c(tmp3a,tmp3n), Type = rep(c('Age','-Age'), each=50)) 

df.metrics <- rbind(df.mae,df.rmse,df.R2)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=100)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
df.metrics$Model <- rep('RF',300)
p1 <- ggplot(df.metrics,aes(x = Model, y = Error, fill = Type)) + 
geom_point(alpha = 0.5, position = position_dodge(0.75), aes(group = Type)) + 
geom_boxplot(alpha = 0.5) + scale_fill_manual(values=c('#deebf7','#377eb8')) + 
labs(x=NULL,y='Metric') + theme_bw(base_size=18) + theme(legend.title=element_blank(),legend.position='none') + 
facet_wrap(~Metric, scales='free',labeller=label_parsed)
 

dev.print(pdf,'Temp/AgevsnoAge.pdf',width=8,height=3)
legend <- cowplot::get_legend(p1)
grid.newpage()
grid.draw(legend)

#######
tmp1 <- read.csv('Data/Results/FI-MAE.csv',header=TRUE)
set.seed(1234)
nsim <- 20
quantiles <- c(0.025,0.5,0.975) #seq(0.1,0.9,length=9)
help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 403
splits <- lapply(seq(nsim), function(x) sample(help))
i <- which(tmp1[,4] == sort(tmp1[,4])[26])
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
Xtrain <- scale(Xtrain, center=TRUE, scale=TRUE)
Xtest <- scale(Xtest, center=TRUE, scale=TRUE)
modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
yhat <- predict(modelGRF60,Xtest)
modelRF60 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
yhat.mean <- predict(modelRF60,Xtest)$predictions

df.pred60 <- data.frame(Age = dfTest$TestAge, True = Ytest, Mean = yhat.mean, yhat)
colnames(df.pred60) <- c('Age','True','Mean','Q025','Median','Q975')
df.pred60$width <- df.pred60$Q975 - df.pred60$Q025
df.pred60$AgeGroup <- ifelse(df.pred60$Age < as.numeric(summary(df.pred60$Age)[2]), 'Q1', 
	ifelse(df.pred60$Age > as.numeric(summary(df.pred60$Age)[3]), 'Q3', 'M'))
df.pred60$AgeGroup <- factor(df.pred60$AgeGroup,levels = c('Q1','M','Q3'))
df.pred60$Prediction <- ifelse(df.pred60$Mean > df.pred60$True, '1','-1')
df.pred60 <- df.pred60[with(df.pred60,order(-Age)),]

df.tmp <- data.frame(Age = unique(df.pred60$Age), Under = table(df.pred60$Age,df.pred60$Prediction)[,1],
	Over = table(df.pred60$Age,df.pred60$Prediction)[,2])
df.tmp <- df.tmp[with(df.tmp,order(Age)),]
df.tmp.melt <- reshape::melt(df.tmp[,-1])


IW60 <- ggplot(df.pred60,aes(x=Age,y=width)) + geom_point() + stat_smooth(method='loess',color = "#D55E00") + 
labs(x = 'Age', y = 'Prediction interval width') + theme_bw(base_size=18) 

ID60 <- ggplot(df.pred60,aes(x = width, color = AgeGroup)) + geom_density(lwd=1.2) + geom_rug() + 
scale_color_manual(values=c('#1f78b4','#e7298a','#525252')) + theme_bw(base_size=18) + labs(x='Prediction interval width', y='Density') + 
theme(legend.position='top')

dev.print(pdf,'Temp/I-2.pdf', width=7.5,height=7.5)
Pr60 <- ggplot(df.pred60, aes(x = seq(nrow(df.pred60)), y = Median, ymin=Q025, ymax=Q975)) + 
	geom_point(aes(y=True), size = 1.5, color='#377eb8') + geom_point(color = '#969696') + geom_pointrange(color = '#969696',alpha = 0.8) + theme_bw(base_size=18) + 
	labs(x='Test Set Index',y='Score') 
	#scale_y_discrete(breaks=c(2,4,6,8,10,12), labels=c(2,4,6,8,10,12),drop=FALSE)
dev.print(pdf,'Temp/pred-int.pdf', width=12, height = 3)




#######
tmp1 <- read.csv('Data/Results/metrics60.csv',header=TRUE)
tmp2 <- read.csv('Data/Results/metrics20.csv',header=TRUE)
tmp3 <- read.csv('Data/Results/metrics5.csv',header=TRUE)


#Extract results for the random forest model that models the average not the median
tmp1 <- tmp1[,-c(1,2,3,4)]
colnames(tmp1) <- c('MAE','RMSE','R2')
tmp2 <- tmp2[,-c(1,2,3,4)]
colnames(tmp2) <- c('MAE','RMSE','R2')
tmp3 <- tmp3[,-c(1,2,3,4)]
colnames(tmp3) <- c('MAE','RMSE','R2')

MAE.melt <- data.frame(value = c(tmp1[,1],tmp2[,1],tmp3[,1]))
RMSE.melt <- data.frame(value = c(tmp1[,2],tmp2[,2],tmp3[,2]))
R2.melt <- data.frame(value = c(tmp1[,3],tmp2[,3],tmp3[,3]))

MAE.melt$variable <- as.factor(rep(c('60mins','20mins','5mins'),each=50)) 
RMSE.melt$variable <- as.factor(rep(c('60mins','20mins','5mins'),each=50)) 
R2.melt$variable <- as.factor(rep(c('60mins','20mins','5mins'),each=50)) 

MAE.melt$sim <- rep(1:50,3)
tmp <- lmer(value ~ variable + (1|sim), data = MAE.melt)
anova(tmp) 

RMSE.melt$sim <- rep(1:50,3)
tmp <- lmer(value ~ variable + (1|sim), data =RMSE.melt)
anova(tmp) 

R2.melt$sim <- rep(1:50,3)
tmp <- lmer(value ~ variable + (1|sim), data = R2.melt)
anova(tmp) 


tmp1 <- read.csv('Data/Results/metrics60.csv',header=TRUE)
tmp2 <- read.csv('Data/Results/metrics20.csv',header=TRUE)
tmp3 <- read.csv('Data/Results/metrics5.csv',header=TRUE)

tmp1 <- tmp1[,-c(1,2,3,4)]
colnames(tmp1) <- c('MAE','RMSE','R2')
tmp2 <- tmp2[,-c(1,2,3,4)]
colnames(tmp2) <- c('MAE','RMSE','R2')
tmp3 <- tmp3[,-c(1,2,3,4)]
colnames(tmp3) <- c('MAE','RMSE','R2')

MAE.melt <- reshape::melt(tmp1)
RMSE.melt <- reshape::melt(tmp2)
R2.melt <- reshape::melt(tmp3)

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
colnames(df.metrics) <- c('Metric','value')
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
df.metrics$variable <- as.factor(rep(c('60mins','20mins','5mins'), each = 150)) 
df.metrics$variable <- factor(df.metrics$variable, levels = c('5mins','20mins','60mins'))
ggplot(df.metrics,aes(x = variable, y = value, fill= variable)) + geom_boxplot() + geom_point(alpha = 0.5) + 
labs(x='Model',y='Test Set Metric') + theme_bw(base_size=18) + theme(legend.position='none') + 
scale_fill_manual(values = c("#009E73", "gold3", "#D55E00")) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed) 





#####Legends#####
df.tmp <- data.frame(value = c(-1,1), Type = c('Age','Frailty'))
p0 <- ggplot(df.tmp,aes(y=value,x=Type,color=Type)) + geom_point() + 
scale_color_manual(values=c('#e41a1c','#377eb8')) + theme_bw(base_size=18) + 
theme(legend.title=element_blank(),legend.position='top') + 
guides(color = guide_legend(override.aes = list(size = 5))) 
legend <- cowplot::get_legend(p0)
grid.newpage()
grid.draw(legend)

df.tmp <- data.frame(value = c(-1,0,1), Type = c('Gait','OFA','Engineered'))
df.tmp$Type <- factor(df.tmp$Type, values = c('Gait','OFA','Engineered'))
levels(df.tmp$Type) <- c('Gait','OFA','Engineered')
p0 <- ggplot(df.tmp,aes(y=value,x=Type,color=Type)) + geom_point() + 
scale_color_manual(values=c('#4daf4a','#984ea3','#ff7f00')) + theme_bw(base_size=18) + 
theme(legend.title=element_blank(),legend.position='top') + 
guides(color = guide_legend(override.aes = list(size = 5))) 
legend <- cowplot::get_legend(p0)
grid.newpage()
grid.draw(legend)
dev.print(pdf,'Temp/legend-A.pdf',width=5,height=1)

df.tmp <- data.frame(value = c(-1,1), Type = c('- Age','Age'))
p0 <- ggplot(df.tmp,aes(y=value,x=Type,color=Type)) + geom_point() + 
scale_color_manual(values=c('#deebf7','#377eb8')) + theme_bw(base_size=18) + 
theme(legend.title=element_blank(),legend.position='top') + 
guides(color = guide_legend(override.aes = list(size = 5))) 
legend <- cowplot::get_legend(p0)
grid.newpage()
grid.draw(legend)
dev.print(pdf,'Temp/legend-C.pdf',width=2,height=1)

####Exploring different types of prediction intervals 
dfTrain <- dfTrain[,-3] #remove Sex
dfTest <- dfTest[,-3] #remove Sex
y <- dfTest$score
method_vec <- c("quantile", "Zhang", "Tung", "Romano", "Roy", "HDI", "Ghosal")
res <- rfint(score ~ ., train_data=dfTrain, test_data=dfTest, method=method_vec)
mean(res$int$quantile[,1] < y & res$int$quantile[,2] > y)
mean(res$int$Zhang[,1] < y & res$int$Zhang[,2] > y)
mean(res$int$Tung[,1] < y & res$int$Tung[,2] > y)
mean(res$int$Romano[,1] < y & res$int$Romano[,2] > y)
mean(res$int$Roy[,1] < y & res$int$Roy[,2] > y)
mean(res$int$HDI[,1] < y & res$int$HDI[,2] > y)
mean(res$int$Ghosal[,1] < y & res$int$Ghosal[,2] > y)

res <- rfinterval::rfinterval(score ~ ., train_data=dfTrain, test_data=dfTest, 
	method = c("oob","quantreg","split-conformal"), symmetry=TRUE)
mean(output$oob_interval$lo < y & output$oob_interval$up > y)
mean(output$sc_interval$lo < y & output$sc_interval$up > y)
mean(output$quantreg_interval$lo < y & output$quantreg_interval$up > y)