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

p1 <- ggplot(df,aes(x=TestAge)) + geom_density(color='#e41a1c',lwd=2) + labs(x = 'Age (weeks)', y = 'Density') + theme_bw(base_size=18) + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))
p3 <- ggplot(df,aes(x=score)) + geom_density(color='#377eb8',lwd=2) + labs(x = 'FI Score', y = 'Density') + theme_bw(base_size=18) + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

df1 <- df[,names(df) %in% c('score',median_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures, rears_measures)]
corr.df <- data.frame(feature = names(df1[,-1]), corr = as.numeric(apply(df1[,-which(names(df1) %in% c('score'))], 
	2, function(x) cor(df1$score,x, use="complete.obs"))))
corr.df$Type <- ifelse(corr.df$feature %in% c(median_gait_measures_linear, iqr_gait_measures_linear), 'Gait',
	ifelse(corr.df$feature %in% c(OFA_measures), 'OFA', 'Engineered'))
corr.df$Type <- factor(corr.df$Type,levels=c('Gait','OFA','Engineered'))
p4 <- ggplot(corr.df, aes(x = seq(1,38), y = abs(corr), color = Type)) + geom_point(size = 3, stroke = 1, alpha = 0.8) + 
labs(x = 'Feature', y = 'Absolute Correlation') + scale_color_manual(values=c('#4daf4a','#984ea3','#ff7f00')) + theme_bw(base_size = 18) + theme(legend.position = 'none') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

df1 <- df[,names(df) %in% c('TestAge',median_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures, rears_measures)]
corr.df <- data.frame(feature = names(df1[,-1]), corr = as.numeric(apply(df1[,-which(names(df1) %in% c('TestAge'))], 
	2, function(x) cor(df1$TestAge,x, use="complete.obs"))))
corr.df$Type <- ifelse(corr.df$feature %in% c(median_gait_measures_linear, iqr_gait_measures_linear), 'Gait',
	ifelse(corr.df$feature %in% c(OFA_measures), 'OFA', 'Engineered'))
corr.df$Type <- factor(corr.df$Type,levels=c('Gait','OFA','Engineered'))
p2 <- ggplot(corr.df, aes(x = seq(1,38), y = abs(corr), color = Type)) + geom_point(size = 3, stroke = 1, alpha = 0.8) + 
labs(x = 'Feature', y = 'Absolute Correlation') + scale_color_manual(values=c('#4daf4a','#984ea3','#ff7f00')) + theme_bw(base_size = 18) + theme(legend.position = 'none') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

p1|p2|p3|p4
dev.print(pdf,'Temp3/1A.pdf',width=14.4,height=3.3) 


###1B####

tmp1 <- read.csv('Data/Age-MAE.csv',header=TRUE)
tmp2 <- read.csv('Data/Age-RMSE.csv',header=TRUE)
tmp3 <- read.csv('Data/Age-R2.csv',header=TRUE)



MAE.melt <- reshape::melt(tmp1[,-1])
RMSE.melt <- reshape::melt(tmp2[,-1])
R2.melt <- reshape::melt(tmp3[,-1])

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=200)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
levels(df.metrics$variable) <- c('LR*','SVM','RF','XGB')
p1 <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_boxplot(fill = '#e41a1c', alpha = 0.7) + geom_point(alpha = 0.5) + 
labs(x='Model',y='Metric (weeks)') + theme_bw(base_size=28) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=21), axis.text.y = element_text(size=21), axis.title.x = element_text(size = 21), axis.title.y=element_text(size=21))

tmp1 <- read.csv('Data/FI-MAE.csv',header=TRUE)
tmp2 <- read.csv('Data/FI-RMSE.csv',header=TRUE)
tmp3 <- read.csv('Data/FI-R2.csv',header=TRUE)

MAE.melt <- reshape::melt(tmp1[,-1])
RMSE.melt <- reshape::melt(tmp2[,-1])
R2.melt <- reshape::melt(tmp3[,-1])

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=200)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
levels(df.metrics$variable) <- c('LR*','SVM','RF','XGB')
p2 <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_boxplot(fill = '#377eb8', alpha = 0.7) + 
geom_point(alpha = 0.5) + 
labs(x='Model',y='Metric') + theme_bw(base_size=28) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=21), axis.text.y = element_text(size=21), axis.title.x = element_text(size = 21), axis.title.y=element_text(size=21))

p1|p2
dev.print(pdf,'Temp3/1B.pdf',width=19.45,height=3.67)


####1C&D#####

tmp1 <- read.csv('Data/Age-MAE.csv',header=TRUE)
tmp2 <- read.csv('Data/Age-RMSE.csv',header=TRUE)
tmp3 <- read.csv('Data/Age-R2.csv',header=TRUE)

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
Ytrain <- dfTrain[,'TestAge']
Ytest <- dfTest[,'TestAge']
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
df.pred60$Group <- ifelse(df.pred60$Age < as.numeric(summary(df.pred60$Age)[2]), 'Q1', 
	ifelse(df.pred60$Age > as.numeric(summary(df.pred60$Age)[3]), 'Q3', 'M'))
df.pred60$Group <- factor(df.pred60$Group,levels = c('Q1','M','Q3'))
df.pred60$Prediction <- ifelse(df.pred60$Mean > df.pred60$True, '1','-1')
df.pred60 <- df.pred60[with(df.pred60,order(-Age)),]

df.tmp <- data.frame(Age = unique(df.pred60$Age), Under = table(df.pred60$Age,df.pred60$Prediction)[,1],
	Over = table(df.pred60$Age,df.pred60$Prediction)[,2])
df.tmp <- df.tmp[with(df.tmp,order(Age)),]
df.tmp.melt <- reshape::melt(df.tmp[,-1])


C1 <- ggplot(df.pred60,aes(x=Age,y=width)) + geom_point(color = '#e41a1c',size=3,alpha=0.8) + stat_smooth(method='loess',color="#000000") + labs(x = 'Age (weeks)', y = 'PI width (Age)') + theme_bw(base_size=22) + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

C2 <- ggplot(df.pred60,aes(x = width, color = Group)) + geom_density(lwd=1.2) + geom_rug() + 
scale_color_manual(values=c('#006d2c','#e7298a','#525252')) + theme_bw(base_size=22) + labs(x='PI width', y='Density') + theme(legend.position='top') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

dev.print(pdf,'Temp/I-2.pdf', width=7.5,height=7.5)
D1 <- ggplot(df.pred60, aes(x = seq(nrow(df.pred60)), y = Median, ymin=Q025, ymax=Q975)) + 
	geom_point(aes(y=True), size = 1.5, color='#e41a1c') + geom_point(color = '#969696') + geom_pointrange(color = '#969696',alpha = 0.8) + theme_bw(base_size=12) + 
	labs(x='Test Set Index',y='Age (weeks)') + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12), axis.title.x = element_text(size = 12), axis.title.y=element_text(size=12))
dev.print(pdf,'Temp3/1D1.pdf', width=8.5,height=2.2)

tmp1 <- read.csv('Data/FI-MAE.csv',header=TRUE)
tmp2 <- read.csv('Data/FI-RMSE.csv',header=TRUE)
tmp3 <- read.csv('Data/FI-R2.csv',header=TRUE)

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
modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
yhat <- predict(modelGRF60,Xtest)
modelRF60 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
yhat.mean <- predict(modelRF60,Xtest)$predictions

df.pred60 <- data.frame(Age = dfTest$TestAge, True = Ytest, Mean = yhat.mean, yhat)
colnames(df.pred60) <- c('Age','True','Mean','Q025','Median','Q975')
df.pred60$width <- df.pred60$Q975 - df.pred60$Q025
df.pred60$Group <- ifelse(df.pred60$Age < as.numeric(summary(df.pred60$Age)[2]), 'Q1', 
	ifelse(df.pred60$Age > as.numeric(summary(df.pred60$Age)[3]), 'Q3', 'M'))
df.pred60$Group <- factor(df.pred60$Group,levels = c('Q1','M','Q3'))
df.pred60$Prediction <- ifelse(df.pred60$Mean > df.pred60$True, '1','-1')
df.pred60 <- df.pred60[with(df.pred60,order(-Age)),]

df.tmp <- data.frame(Age = unique(df.pred60$Age), Under = table(df.pred60$Age,df.pred60$Prediction)[,1],
	Over = table(df.pred60$Age,df.pred60$Prediction)[,2])
df.tmp <- df.tmp[with(df.tmp,order(Age)),]
df.tmp.melt <- reshape::melt(df.tmp[,-1])


C3 <- ggplot(df.pred60,aes(x=Age,y=width)) + geom_point(color = '#377eb8',size=3,alpha=0.8) + stat_smooth(method='loess',color = "#000000") + labs(x = 'Age (weeks)', y = 'PI width (FI Score)') + theme_bw(base_size=22) + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

C4 <- ggplot(df.pred60,aes(x = width, color = Group)) + geom_density(lwd=1.2) + geom_rug() + 
scale_color_manual(values=c('#006d2c','#e7298a','#525252')) + theme_bw(base_size=22) + labs(x='PI width', y='Density') + theme(legend.position='top') + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

C1|C2|C3|C4
dev.print(pdf,'Temp3/1C.pdf',width=17.0,height=4.56)

D2 <- ggplot(df.pred60, aes(x = seq(nrow(df.pred60)), y = Median, ymin=Q025, ymax=Q975)) + 
	geom_point(aes(y=True), size = 1.5, color='#377eb8') + geom_point(color = '#969696') + geom_pointrange(color = '#969696',alpha = 0.8) + theme_bw(base_size=14) + 
	labs(x='Test Set Index',y='FI Score') + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12), axis.title.x = element_text(size = 12), axis.title.y=element_text(size=12))

dev.print(pdf,'Temp3/1D2.pdf', width=8.5,height=2.2)


#####################Figure2
tmp1 <- read.csv('Data/FI-MAE.csv',header=TRUE)
tmp2 <- read.csv('Data/FI-RMSE.csv',header=TRUE)
tmp3 <- read.csv('Data/FI-R2.csv',header=TRUE)

#Features <- c('Stride Count','Weight','Center Time','Periphery Time','Corner Time','Center Distance',
#	'Periphery Distance','Corner Distance','Grooming Bouts','Grooming Duration','Angular Velocity',
#	'Base Tail LD','Limb Duty Factor','Nose LD','Speed','Step Length1','Step Length2','Step Width',
#	'Stride Length','Tip Tail LD','Distance','Angular Velocity IQR','Base Tail LD IQR',
#	'Limb Duty Factor IQR','Nose LD IQR','Speed IQR','Step Length1 IQR','Step Length2 IQR','Step Width IQR',
#	'Stride Length IQR','Tip Tail LD IQR','dAC','dB','aABC','Width','Length','Rearpaw','Rearcount') 

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
modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[1], 
	honesty=FALSE)
vi_list[[1]] <- grf::variable_importance(modelGRF60)
modelGRF60 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
vi_list[[2]] <- grf::variable_importance(modelGRF60)
modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[3], 
	honesty=FALSE)
vi_list[[3]] <- grf::variable_importance(modelGRF60)

df.tmp <- data.frame(importance = do.call(rbind,vi_list)) 
#df.tmp$feature <- colnames(Xtest)
df.tmp$feature <- rep(Features,3)
df.tmp$Quantile <- factor(rep(c('Q025','M','Q975'), each=38)) 
df.tmp$Quantile <- factor(df.tmp$Quantile, levels = c('Q025','M','Q975'))

ggplot(df.tmp, aes(x=feature,y=importance,fill=Quantile)) + 
geom_bar(stat='identity',position='dodge') +  
theme_bw(base_size=18) + labs(x = 'Feature', y = 'Importance') + 
scale_fill_manual(name = "Quantile", values = c('#e41a1c','#377eb8','#4daf4a'),
	labels = expression(Q[.025], Q[.50], Q[.975])) + 
theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1)) + theme(axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), axis.title.x = element_text(size = 18), axis.title.y=element_text(size=18))
dev.print(pdf,'Temp3/2A.pdf', width=16,height=5)

ftrs <- c(30,36,34,16,32)
k <- 50 #A numeric scalar that specifies the number of intervals into which the predictor range is 
		#divided when calculating the ALE plot effects.
pb <- txtProgressBar(min = 0, max = length(ftrs), style = 3)
lapply(seq(ftrs), function(f) {
	ff <- ftrs[f]
	feature <- Features[ftrs[f]]
	modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
	modelRF60 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
	Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata,type='raw')$predictions)
	ALERF <- ALEPlot::ALEPlot(Xtrain,modelRF60,pred.fun=Yhat,J=ff,K=k)
	ALEGRF <- lapply(seq(quantiles), function(x) {Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)[,x]);
	ALEPlot::ALEPlot(Xtrain,modelGRF60,pred.fun=Yhat,J=ff,K=k)$f.values})
	df.ALE60 <- data.frame(x = ALERF$x.values)
	df.ALE60 <- cbind(df.ALE60, do.call(cbind,ALEGRF))
	colnames(df.ALE60) <- c('x','Q025','Median','Q975')

	df.ALE.melt60 <- invisible(reshape::melt(df.ALE60[,-1]))
	df.ALE.melt60$x <- rep(df.ALE60$x,ncol(df.ALE60[,-1]))
	df.ALE.melt60$variable <- factor(df.ALE.melt60$variable, levels=c("Q025","Median","Q975"))

	assign(paste0("p",f), ggplot(df.ALE.melt60, aes(x = x, y = value, col = variable)) + geom_point() + geom_line() + 
	theme_bw(base_size=18) + scale_color_manual(name = "Quantile", values = c('#e41a1c','#377eb8','#4daf4a'),
	labels = expression(Q[.025], Q[.50], Q[.975])) + labs(x = paste0(feature), y = 'ALE') + 
	theme(legend.position='none') + theme(axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), axis.title.x = element_text(size = 18), axis.title.y=element_text(size=18)), 
	inherits=TRUE);
	setTxtProgressBar(pb, f)
	close(pb)
})

p1|p2|p3|p4|p5
dev.print(pdf,'Temp3/2B.pdf', width=15,height=2.9)

modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[2], 
		honesty=FALSE)
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
predictor <- iml::Predictor$new(
  model = modelGRF60, 
  data = as.data.frame(Xtrain),
  y = Ytrain, 
  predict.fun = Yhat
  )
interact <- iml::Interaction$new(predictor)
tmp <- data.frame(interact$results)
colnames(tmp) <- c('Feature','Interaction')
tmp$Feature <- as.factor(Features) 
tmp <- tmp[with(tmp, order(Interaction)),]
tmp$Feature <- factor(tmp$Feature, levels = tmp$Feature)
ggplot(tmp, aes(y = Feature, x = Interaction)) +
geom_point(color = '#377eb8') + theme_bw(base_size=18) + 
geom_segment(aes(yend = Feature, x = 0, xend = Interaction),color='#377eb8') +
scale_x_continuous("Overall interaction strength") + scale_y_discrete("Features") + theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16), axis.title.x = element_text(size = 16), axis.title.y=element_text(size=16))
dev.print(pdf,'Temp3/2C.pdf', width=6,height=10)


f <- c(34,36) #c(35,34) #c(14,16) #(30,34), (1,17), |||Agemodel c(1,18) (1,16)
k <- 50 #A numeric scalar that specifies the number of intervals into which the predictor range is 
		#divided when calculating the ALE plot effects.
j <- 2
names(Xtrain) <- Features
modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[j], 
		honesty=FALSE)
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
ALEGRF <- ALEPlot::ALEPlot(Xtrain,modelGRF60,pred.fun=Yhat,J=f,K=k)$f.values
dev.print(pdf,"Temp3/2E.pdf",width=5,height=5)

require(viridis)
pred.fi <- iml::Predictor$new(modelGRF60, data = as.data.frame(Xtrain), y = Ytrain)
iml::FeatureEffect$new(pred.fi, feature = c("median_step_width", "median_step_length1"), method = "ale", grid.size = 50)$plot() + theme_bw(base_size=18) + 
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Step Width") + 
  scale_y_continuous("Step Length1")+
  scale_fill_viridis(option = "D") + theme(axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), axis.title.x = element_text(size = 18), axis.title.y=element_text(size=18))
dev.print(pdf,"Temp3/2D.pdf",width=7.1,height=5.3)

Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
ale_list <- list()
ice_list <- list()
for (j in 1:3){
	modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[j], 
		honesty=FALSE)
	predictor <- Predictor$new(modelGRF60, data = as.data.frame(Xtrain), y = Ytrain, predict.fun = Yhat)
	ale_list[[j]] <-  FeatureEffect$new(predictor, "TestAge", method="ale")
	ice_list[[j]] <-  FeatureEffect$new(predictor, "TestAge", method="pdp+ice")
}

df.tmp <- rbind(ice_list[[1]]$results,ice_list[[2]]$results,ice_list[[3]]$results)
df.tmp$Quartile <- rep(c('Q025','M','Q975'), each=20)
ggplot(df.tmp,aes(x=TestAge,y=.value,color=Quartile)) + geom_line() + 
scale_fill_brewer(palette='Set1')


#####################Figure3
tmp1 <- read.csv('Data/Results/metrics60-FI.csv',header=TRUE)
tmp2 <- read.csv('Data/Results/metrics20-FI.csv',header=TRUE)
tmp3 <- read.csv('Data/Results/metrics5-FI.csv',header=TRUE)

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
df.metrics$variable <- as.factor(rep(c('60','20','5'), each = 150)) 
df.metrics$variable <- factor(df.metrics$variable, levels = c('5','20','60'))
p2 <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_boxplot(fill = '#377eb8',alpha = 0.7) + geom_point(alpha = 0.5) +
labs(x='Duration (mins) ',y='Test Set Metric (FI)') + theme_bw(base_size=22) + theme(legend.position='none') +  
facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=22), axis.text.y = element_text(size=22), axis.title.x = element_text(size = 22), axis.title.y=element_text(size=22))

tmp1 <- read.csv('Data/Results/metrics60-Age.csv',header=TRUE)
tmp2 <- read.csv('Data/Results/metrics20-Age.csv',header=TRUE)
tmp3 <- read.csv('Data/Results/metrics5-Age.csv',header=TRUE)

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
df.metrics$variable <- as.factor(rep(c('60','20','5'), each = 150)) 
df.metrics$variable <- factor(df.metrics$variable, levels = c('5','20','60'))
p1 <- ggplot(df.metrics,aes(x = variable, y = value)) + geom_boxplot(fill = '#e41a1c',alpha = 0.7) + geom_point(alpha = 0.5) +
labs(x='Duration (mins)',y='Test Set Metric (weeks)') + theme_bw(base_size=22) + theme(legend.position='none') +  
facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=22), axis.text.y = element_text(size=22), axis.title.x = element_text(size = 22), axis.title.y=element_text(size=22))

p1|p2
dev.print(pdf,'Temp3/3A.pdf',width=19, height=3.5)


tmp1 <- read.csv('Data/Results/metrics60-FI.csv',header=TRUE)
tmp2 <- read.csv('Data/Results/metrics20-FI.csv',header=TRUE)
tmp3 <- read.csv('Data/Results/metrics5-FI.csv',header=TRUE)

tmp1 <- tmp1[,-c(1,2,3,4)]
colnames(tmp1) <- c('MAE','RMSE','R2')
tmp2 <- tmp2[,-c(1,2,3,4)]
colnames(tmp2) <- c('MAE','RMSE','R2')
tmp3 <- tmp3[,-c(1,2,3,4)]
colnames(tmp3) <- c('MAE','RMSE','R2')

#60mins
vi_list <- list()
i <- which.min(tmp1$MAE)
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)

vi_list[[1]] <- grf::variable_importance(model)

i <- which.min(tmp2$MAE)
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)

vi_list[[2]] <- grf::variable_importance(model)

i <- which.min(tmp3$MAE)
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)

vi_list[[3]] <- grf::variable_importance(model)

df.tmp <- data.frame(importance = do.call(rbind,vi_list)) 
#df.tmp <- df.tmp[-c(1,2,39,40,77,78),]
df.tmp <- data.frame(importance = df.tmp)
df.tmp$feature <- Features
df.tmp$Duration <- factor(rep(c('60mins','20mins','5mins'), each=38)) 
df.tmp$Duration <- factor(df.tmp$Duration, levels = c('5mins','20mins','60mins'))
ggplot(df.tmp, aes(x=feature,y=importance,fill=Duration)) + 
geom_bar(stat='identity',position='dodge') +  
theme_bw(base_size=22) + labs(x = 'Feature', y = 'Importance (Predict FI)') + 
scale_fill_manual(name = "Duration", values = c('#1f78b4','#e7298a','#525252')) + 
theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.position='top') + 
theme(axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), axis.title.x = element_text(size = 18), axis.title.y=element_text(size=18))
dev.print(pdf,'Temp3/3C.pdf',width=16,height=5)

tmp1 <- read.csv('Data/Results/metrics60-Age.csv',header=TRUE)
tmp2 <- read.csv('Data/Results/metrics20-Age.csv',header=TRUE)
tmp3 <- read.csv('Data/Results/metrics5-Age.csv',header=TRUE)

tmp1 <- tmp1[,-c(1,2,3,4)]
colnames(tmp1) <- c('MAE','RMSE','R2')
tmp2 <- tmp2[,-c(1,2,3,4)]
colnames(tmp2) <- c('MAE','RMSE','R2')
tmp3 <- tmp3[,-c(1,2,3,4)]
colnames(tmp3) <- c('MAE','RMSE','R2')

#60mins
vi_list <- list()
i <- which.min(tmp1$MAE)
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'TestAge']
Ytest <- dfTest[,'TestAge']
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)

vi_list[[1]] <- grf::variable_importance(model)

i <- which.min(tmp2$MAE)
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'TestAge']
Ytest <- dfTest[,'TestAge']
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)

vi_list[[2]] <- grf::variable_importance(model)

i <- which.min(tmp3$MAE)
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'TestAge']
Ytest <- dfTest[,'TestAge']
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)

vi_list[[3]] <- grf::variable_importance(model)

df.tmp <- data.frame(importance = do.call(rbind,vi_list)) 
#df.tmp <- df.tmp[-c(1,2,39,40,77,78),]
df.tmp <- data.frame(importance = df.tmp)
df.tmp$feature <- Features
df.tmp$Duration <- factor(rep(c('60mins','20mins','5mins'), each=38)) 
df.tmp$Duration <- factor(df.tmp$Duration, levels = c('5mins','20mins','60mins'))
ggplot(df.tmp, aes(x=feature,y=importance,fill=Duration)) + 
geom_bar(stat='identity',position='dodge') +  
theme_bw(base_size=22) + labs(x = 'Feature', y = 'Importance (Predict Age)') + 
scale_fill_manual(name = "Duration", values = c('#1f78b4','#e7298a','#525252')) + 
theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),legend.position='top') + 
theme(axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), axis.title.x = element_text(size = 18), axis.title.y=element_text(size=18))
dev.print(pdf,'Temp3/3B.pdf',width=16,height=5)


