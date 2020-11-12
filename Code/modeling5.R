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
median_gait_measures_linear_20 <- c('med_angular_velocity_20','med_base_tail_lateral_displacement_20',
	'med_limb_duty_factor_20','med_nose_lateral_displacement_20','med_speed_cm_per_sec_20',
	'med_step_length1_20','med_step_length2_20','med_step_width_20','med_stride_length_20',
	'med_tip_tail_lateral_displacement_20')
median_gait_measures_linear_5 <- c('med_angular_velocity_5','med_base_tail_lateral_displacement_5',
	'med_limb_duty_factor_5','med_nose_lateral_displacement_5','med_speed_cm_per_sec_5',
	'med_step_length1_5','med_step_length2_5','med_step_width_5','med_stride_length_5',
	'med_tip_tail_lateral_displacement_5')
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
iqr_gait_measures_linear_20 <- c('angular_velocity_iqr_20','base_tail_lateral_displacement_iqr_20',
	'limb_duty_factor_iqr_20','nose_lateral_displacement_iqr_20','speed_cm_per_sec_iqr_20',
	'step_length1_iqr_20','step_length2_iqr_20','step_width_iqr_20','stride_length_iqr_20',
	'tip_tail_lateral_displacement_iqr_20')
iqr_gait_measures_linear_5 <- c('angular_velocity_iqr_5','base_tail_lateral_displacement_iqr_5',
	'limb_duty_factor_iqr_5','nose_lateral_displacement_iqr_5','speed_cm_per_sec_iqr_5',
	'step_length1_iqr_5','step_length2_iqr_5','step_width_iqr_5','stride_length_iqr_5',
	'tip_tail_lateral_displacement_iqr_5')
OFA_measures <- c('stride_count','Distance.cm.sc','center_time_secs','periphery_time_secs','corner_time_secs',
	'center_distance_cm','periphery_distance_cm','corner_distance_cm','grooming_number_bouts',
	'grooming_duration_secs')
OFA_measures_20 <- c('distance_cm','center_time_secs','periphery_time_secs','corner_time_secs',
	'center_distance_cm','periphery_distance_cm','corner_distance_cm','grooming_number_bouts',
	'grooming_duration_secs') #'stride_count' missing
OFA_measures_5 <- c('distance_cm','center_time_secs','periphery_time_secs','corner_time_secs',
	'center_distance_cm','periphery_distance_cm','corner_distance_cm','grooming_number_bouts',
	'grooming_duration_secs') #'stride_count' missing

rearpaw_pose_measures <- c('median_rearpaw')
rears_measures <- c('rear_count','rears_0_5','rears_0_10')
ellipsefit_measures <- c('median_width', 'median_length')
engineered_features_mean <- c('dAC_mean','dB_mean','aABC_mean')
engineered_features_stdev <- c('dAC_stdev','dB_stdev','aABC_stdev')
engineered_features_min <- c('dAC_min','dB_min','aABC_min')
engineered_features_max <- c('dAC_max','dB_max','aABC_max')
engineered_features_median <- c('dAC_median','dB_median','aABC_median')
engineered_features_median_20 <- c('dAC_median_20','dB_median_20','aABC_median_20')
engineered_features_median_5 <- c('dAC_median_5','dB_median_5','aABC_median_5')
ellipsefit_measures_20 <- c('median_width_20', 'median_length_20')
ellipsefit_measures_5 <- c('median_width_5', 'median_length_5')
rearpaw_pose_measures_20 <- c('rearpaw_median_20')
rears_measures_20 <- c('rear_count_20')
rearpaw_pose_measures_5 <- c('rearpaw_median_5')
rears_measures_5 <- c('rear_count_5')

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
df <- df0[,names(df0) %in% c('NetworkFilename','Overall.Score','MouseID','Collected.By','Body.Weight','Sex','Age.at.Test',
	median_gait_measures_linear, iqr_gait_measures_linear, OFA_measures, engineered_features_median, 
	ellipsefit_measures, rearpaw_pose_measures,rears_measures)]

df <- df[complete.cases(df),] #remove 9 animals with missing values
df <- df[,-which(names(df) %in% c('rears_0_5','stride_count'))]

df20 <- read.csv('Data/df20.csv',header=TRUE,stringsAsFactors=FALSE)
colnames(df20)[2] <- 'MouseID'
colnames(df20)[41] <- 'Overall.Score'
colnames(df20)[108] <- 'rear_count_20'
colnames(df20)[104:105] <- ellipsefit_measures_20
df.20 <- df20[,names(df20) %in% c('NetworkFilename','Overall.Score','MouseID','Collected.By','Body.Weight','Sex','Age.at.Test',
	median_gait_measures_linear_20, iqr_gait_measures_linear_20, OFA_measures_20, engineered_features_median_20, 
	ellipsefit_measures_20, rearpaw_pose_measures_20, rears_measures_20)] #stride_counts and rears_0_5 missing


df5 <- read.csv('Data/df5.csv', header=TRUE, stringsAsFactors=FALSE)
colnames(df5)[2] <- 'MouseID'
colnames(df5)[41] <- 'Overall.Score'
colnames(df5)[104:105] <- ellipsefit_measures_5
colnames(df5)[108] <- rears_measures_5
df.5 <- df5[,names(df5) %in% c('NetworkFilename','Overall.Score','MouseID','Collected.By','Body.Weight','Sex','Age.at.Test',
	median_gait_measures_linear_5, iqr_gait_measures_linear_5, OFA_measures_5, engineered_features_median_5, 
	ellipsefit_measures_5, rearpaw_pose_measures_5, rears_measures_5)] #stride_counts and rears_0_5 missing


common_animals <- intersect(intersect(df$MouseID,df.20$MouseID),df.5$MouseID)
df <- df[df$MouseID %in% common_animals,]
df.5 <- df.5[df.5$MouseID %in% common_animals,]
df.20 <- df.20[df.20$MouseID %in% common_animals,]
df <- df[,-1] #Remove Networkfilename
df.5 <- df.5[,-1] #Remove Networkfilename
df.20 <- df.20[,-1] #Remove Networkfilename


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

df.20 <- df.20[complete.cases(df.20),] #remove 9 animals with missing values
names(df.20)[names(df.20) == 'Overall.Score'] <- 'score' 
names(df.20)[names(df.20) == 'Age.at.Test'] <- 'TestAge' 
names(df.20)[names(df.20) == 'Body.Weight'] <- 'Weight' 
names(df.20)[names(df.20) == 'Collected.By'] <- 'Tester' 
df.20$Tester <- factor(df.20$Tester, levels = c('Amanda','Gaven','Hannah','Mackenzie'))
levels(df.20$Tester) <- vapply(1:4, function(x) paste0('Scorer',x), character(1)) 
df.20$Sex <- as.factor(ifelse(df.20$Sex == 'Male', -1, 1)) 
df.20 <- df.20[,-which(names(df.20) %in% c('Tester'))]

#Adjust for the tester effect
#mod.lmm <- lmer(score ~ (1|Tester), data = df.20) 
#df.20$score <- ifelse(df.20$Tester == 'Scorer1', df.20$score - ranef(mod.lmm)$Tester[1,],
#	ifelse(df.20$Tester == 'Scorer2', df.20$score - ranef(mod.lmm)$Tester[2,],
#	ifelse(df.20$Tester == 'Scorer3', df.20$score - ranef(mod.lmm)$Tester[3,], 
#	df.20$score - ranef(mod.lmm)$Tester[4,])))
#df.20 <- df.20[,-which(names(df.20) %in% c('Tester'))]

df.5 <- df.5[complete.cases(df.5),] #remove 9 animals with missing values
names(df.5)[names(df.5) == 'Overall.Score'] <- 'score' 
names(df.5)[names(df.5) == 'Age.at.Test'] <- 'TestAge' 
names(df.5)[names(df.5) == 'Body.Weight'] <- 'Weight' 
names(df.5)[names(df.5) == 'Collected.By'] <- 'Tester' 
df.5$Tester <- factor(df.5$Tester, levels = c('Amanda','Gaven','Hannah','Mackenzie'))
levels(df.5$Tester) <- vapply(1:4, function(x) paste0('Scorer',x), character(1)) 
df.5$Sex <- as.factor(ifelse(df.5$Sex == 'Male', -1, 1)) 
df.5 <- df.5[,-which(names(df.5) %in% c('Tester'))]

#Adjust for the tester effect
#mod.lmm <- lmer(score ~ (1|Tester), data = df.5) 
#df.5$score <- ifelse(df.5$Tester == 'Scorer1', df.5$score - ranef(mod.lmm)$Tester[1,],
#	ifelse(df.5$Tester == 'Scorer2', df.5$score - ranef(mod.lmm)$Tester[2,],
#	ifelse(df.5$Tester == 'Scorer3', df.5$score - ranef(mod.lmm)$Tester[3,], 
#	df.5$score - ranef(mod.lmm)$Tester[4,])))


#For a meaningful comparison, we need df$score,df.20$score,df.5$score to be equal
invisible(sapply(seq(length(unique(df.20$MouseID))), function(x) {
	if (length(df.20[df.20$MouseID %in% unique(df.20$MouseID)[x],'score'])==1){
			df.20[df.20$MouseID %in% unique(df.20$MouseID)[x],'score'] <<- 
		df[df$MouseID %in% unique(df.20$MouseID)[x], 'score'][1];
	} else{
			df.20[df.20$MouseID %in% unique(df.20$MouseID)[x],'score'] <<- 
		df[df$MouseID %in% unique(df.20$MouseID)[x], 'score'];
	}
	}))

invisible(sapply(seq(length(unique(df.5$MouseID))), function(x) {
	if (length(df.5[df.5$MouseID %in% unique(df.5$MouseID)[x],'score'])==1){
			df.5[df.5$MouseID %in% unique(df.5$MouseID)[x],'score'] <<- 
		df[df$MouseID %in% unique(df.5$MouseID)[x], 'score'][1];
	} else{
			df.5[df.5$MouseID %in% unique(df.5$MouseID)[x],'score'] <<- 
		df[df$MouseID %in% unique(df.5$MouseID)[x], 'score'];
	}
	}))

#df <- merge(df[,names(df)%in%c('MouseID','TestAge','Weight','score','Sex',
#	median_gait_measures_linear, iqr_gait_measures_linear, OFA_measures, engineered_features_median, 
#	ellipsefit_measures, rearpaw_pose_measures,rears_measures)],
#	df.5[,names(df.5) %in% c('MouseID','TestAge')],by=c('MouseID','TestAge'),all.y=TRUE)

#df.20 <- merge(df.20[,names(df.20)%in%c('MouseID','TestAge','Weight','score','Sex',
#	median_gait_measures_linear_20, iqr_gait_measures_linear_20, OFA_measures_20, engineered_features_median_20, 
#	ellipsefit_measures_20, rearpaw_pose_measures_20,rears_measures_20)],
#	df.5[,names(df.5) %in% c('MouseID','TestAge')],by=c('MouseID','TestAge'),all.y=TRUE)

ggplot(df,aes(x=score)) + geom_density(lwd=1.2) + labs(x='Score',y='Density') + theme_bw(base_size=18)
dev.print(pdf,'Temp2/score-density.pdf',width=8,height=3)

tmp <- data.frame(Score = c(df$score, df.20$score, df.5$score), 
	Duration = rep(c('60mins','20mins','5mins'), c(nrow(df),nrow(df.20),nrow(df.5))))
ggplot(tmp, aes(x=Score, color=Duration)) + geom_density() + scale_color_brewer(palette='Set1')

#Some exploratory analysis for figures
df1 <- df[,-which(names(df) %in% c('MouseID','TestAge','Weight','Sex'))]
corr.df60 <- data.frame(feature = names(df1[,-1]), corr = as.numeric(apply(df1[,-which(names(df1) %in% c('score'))], 
	2, function(x) cor(df1$score,x, use="complete.obs"))))
corr.df60$Type <- ifelse(corr.df60$feature %in% c(median_gait_measures_linear, iqr_gait_measures_linear), 'Gait',
	ifelse(corr.df60$feature %in% c(OFA_measures), 'OFA', 'Engineered'))

df1 <- df.20[,-which(names(df.20) %in% c('MouseID','TestAge','Weight','Sex'))]
corr.df20 <- data.frame(feature = names(df1[,-1]), corr = as.numeric(apply(df1[,-which(names(df1) %in% c('score'))], 
	2, function(x) cor(df1$score,x, use="complete.obs"))))
corr.df20$Type <- ifelse(corr.df20$feature %in% c(median_gait_measures_linear_20, 
	iqr_gait_measures_linear_20), 'Gait',
	ifelse(corr.df20$feature %in% c(OFA_measures_20), 'OFA', 'Engineered'))

df1 <- df.5[,-which(names(df.5) %in% c('MouseID','TestAge','Weight','Sex'))]
corr.df5 <- data.frame(feature = names(df1[,-1]), corr = as.numeric(apply(df1[,-which(names(df1) %in% c('score'))], 
	2, function(x) cor(df1$score,x, use="complete.obs"))))
corr.df5$Type <- ifelse(corr.df5$feature %in% c(median_gait_measures_linear_5,
	iqr_gait_measures_linear_5), 'Gait',
	ifelse(corr.df5$feature %in% c(OFA_measures_5), 'OFA', 'Engineered'))

df.tmp <- rbind(corr.df5,corr.df20,corr.df60)
df.tmp$Duration <- rep(c('5mins','20mins','60mins'), each = nrow(corr.df60))
df.tmp$Duration <- factor(df.tmp$Duration, levels = c('5mins','20mins','60mins')) 
df.tmp$Type <- factor(df.tmp$Type, levels = c('Gait','Engineered','OFA')) 
ggplot(df.tmp, aes(x = seq(1,nrow(df.tmp)), y= abs(corr), color=Duration, shape = Type)) + 
geom_point(size=3, stroke=1, alpha = 0.8) + scale_color_manual(values = c("#009E73", "gold3", "#D55E00")) + 
geom_hline(yintercept = 0.5, color = 'black', linetype = 'dashed') + labs(x = 'Feature', y = 'Correlation') + 
theme_bw(base_size=18) + theme(legend.position='top') 

ggplot(df.tmp, aes(x = abs(corr), color = Duration)) + geom_density(alpha=0.8,lwd=1.2) + 
facet_wrap(~Type, scales = 'free') + scale_color_manual(values = c("#009E73", "gold3", "#D55E00")) + 
labs(x = 'Absolute Correlation', y = 'Density') + theme_bw(base_size=18) + theme(legend.position='none')
dev.print(pdf,'Temp2/abs-corr-density.pdf',width=8,height=3)

ggplot(df, aes(x = score)) + geom_density(lwd = 1.2) + labs(x = 'Score', y ='Density') + theme_bw(base_size=18)

set.seed(1234)
nsim <- 20
quantiles <- c(0.025,0.5,0.975) #seq(0.1,0.9,length=9)
help <- rep(1:3,c(203,100,100)) #since length(unique(df$MouseID)) = 403
splits <- lapply(seq(nsim), function(x) sample(help))
metrics60 <- data.frame(matrix(0,nrow=nsim,ncol=6))
metrics20 <- data.frame(matrix(0,nrow=nsim,ncol=6))
metrics5 <- data.frame(matrix(0,nrow=nsim,ncol=6))

lapply(seq(nsim), function(i){

cat("Simulation Run = ", i, "\n")
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])


#60 mins
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


model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
yhat <- predict(model,Xtest)
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
yhat.mean <- predict(model,Xtest)$predictions

metrics60[i,] <<- c(mean(abs(Ytest-yhat[,2])), sqrt(mean((Ytest-yhat[,2])^2)), cor(Ytest,yhat[,2])^2,
		mean(abs(Ytest-yhat.mean)), sqrt(mean((Ytest-yhat.mean)^2)), cor(Ytest,yhat.mean)^2)


#20 mins
dfTrain <- df.20[df.20$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df.20[df.20$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
yhat <- predict(model,Xtest)
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
yhat.mean <- predict(model,Xtest)$predictions

metrics20[i,] <<- c(mean(abs(Ytest-yhat[,2])), sqrt(mean((Ytest-yhat[,2])^2)), cor(Ytest,yhat[,2])^2,
		mean(abs(Ytest-yhat.mean)), sqrt(mean((Ytest-yhat.mean)^2)), cor(Ytest,yhat.mean)^2)


#5 mins
dfTrain <- df.5[df.5$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df.5[df.5$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
yhat <- predict(model,Xtest)
model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
yhat.mean <- predict(model,Xtest)$predictions

metrics5[i,] <<- c(mean(abs(Ytest-yhat[,2])), sqrt(mean((Ytest-yhat[,2])^2)), cor(Ytest,yhat[,2])^2,
		mean(abs(Ytest-yhat.mean)), sqrt(mean((Ytest-yhat.mean)^2)), cor(Ytest,yhat.mean)^2)
})

colnames(metrics60) <- c("Median MAE","Median RMSE","Median R2","Mean MAE","Mean RMSE","Mean R2")
colnames(metrics20) <- c("Median MAE","Median RMSE","Median R2","Mean MAE","Mean RMSE","Mean R2")
colnames(metrics5) <- c("Median MAE","Median RMSE","Median R2","Mean MAE","Mean RMSE","Mean R2")

df.melt60 <- reshape::melt(metrics60)
df.melt20 <- reshape::melt(metrics20)
df.melt5 <- reshape::melt(metrics5)

df.melt <- rbind(df.melt60,df.melt20,df.melt5)
df.melt$Duration <- as.factor(rep(c('60mins','20min','5mins'), each=120)) 
levels(df.melt$Duration) <- c('5mins','20mins','60mins')
df.melt$index <- as.factor(rep(1:120,3)) 

ggplot(df.melt, aes(x = Duration, y = value)) + geom_point() + geom_boxplot(aes(fill = Duration),alpha=0.8) + 
facet_wrap(~variable,scales='free') + scale_fill_manual(values = c("#009E73", "gold3", "#D55E00")) + 
labs(y = 'Metric')

df.tmp <- df.melt[df.melt$variable %in% c('Median MAE','Median RMSE','Median R2'),]
df.tmp$variable <- droplevels(df.tmp$variable)
levels(df.tmp$variable) <- c('MAE','RMSE','R^2')
ggplot(df.tmp, aes(x = Duration, y = value)) + geom_point() + geom_boxplot(aes(fill = Duration),alpha=0.8) + 
facet_wrap(~variable,scales='free',labeller=label_parsed) + scale_fill_manual(values = c("#009E73", "gold3", "#D55E00")) + theme_bw(base_size=18) + 
labs(y = 'Test set metric') + theme(legend.position = 'none')
dev.print(pdf,'Temp/test-set-results.pdf',width=6,height=3)
#60mins
i <- which.min(metrics60$'Median MAE')
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex','TestAge'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex','TestAge'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
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

df.tmp <- data.frame(Age = unique(df.pred60$Age), Under = table(df.pred60$Age,df.pred60$Prediction)[,1],
	Over = table(df.pred60$Age,df.pred60$Prediction)[,2])
df.tmp <- df.tmp[with(df.tmp,order(Age)),]
df.tmp.melt <- reshape::melt(df.tmp[,-1])


IW60 <- ggplot(df.pred60,aes(x=Age,y=width)) + geom_point() + stat_smooth(method='loess',color = "#D55E00") + 
labs(x = 'Age', y = 'Prediction interval width') + theme_bw(base_size=18) 

ID60 <- ggplot(df.pred60,aes(x = width, color = AgeGroup)) + geom_density(lwd=1.2) + 
scale_color_manual(values=c('#1f78b4','#e7298a','#525252')) + theme_bw(base_size=18) + labs(x='Prediction interval width', y='Density') + 
theme(legend.position='top')

Pr60 <- ggplot(df.pred60, aes(x = seq(nrow(df.pred60)), y = Median, ymin=Q025, ymax=Q975)) + 
	geom_point(aes(y=True), size = 2) + geom_point(cex = 0.5) + geom_pointrange(color = "#D55E00") + theme_bw(base_size=18) + 
	labs(x='Animals',y='Score') + scale_x_discrete(breaks=c(50,100,length(Ytest)), labels=c(50,100,length(Ytest)))
#geom_point(aes(y=Mean), size = 1, color = 'blue')
dev.print(pdf,'Temp/pred-int.pdf', width=12, height = 3)
ggplot(df.tmp.melt, aes(x=value,color=variable)) + geom_histogram(bins=6)

#20mins
#i <- which.min(metrics20$'Median MAE') #i should be equal for 60,20,5 for valid comparison 
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
modelGRF20 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
yhat <- predict(modelGRF20,Xtest)
modelRF20 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
yhat.mean <- predict(modelRF20,Xtest)$predictions

df.pred20 <- data.frame(Age = Xtest$TestAge, True = Ytest, Mean = yhat.mean, yhat)
colnames(df.pred20) <- c('Age','True','Mean','Q025','Median','Q975')
df.pred20$width <- df.pred20$Q975 - df.pred20$Q025
df.pred20$AgeGroup <- ifelse(df.pred20$Age < as.numeric(summary(df.pred20$Age)[2]), 'Q1', 
	ifelse(df.pred20$Age > as.numeric(summary(df.pred20$Age)[3]), 'Q3', 'M'))

IW20 <- ggplot(df.pred20,aes(x=Age,y=width)) + geom_point() + stat_smooth(method='loess') + 
labs(x = 'Age', y = 'Prediction interval width') + theme_bw(base_size=22) 

ID20 <- ggplot(df.pred20,aes(x = width, color = AgeGroup)) + geom_density(lwd=1.2)

Pr20 <- ggplot(df.pred20, aes(x = seq(nrow(df.pred20)), y = Median, ymin=Q025, ymax=Q975)) + 
	geom_point(aes(y=True), size = 2) + geom_point(cex = 0.5) + geom_pointrange(color = "gold3") + theme_bw(base_size=18) + 
	labs(x=NULL,y='Score') + scale_x_discrete(breaks=c(50,100,length(Ytest)), labels=c(50,100,length(Ytest)))
#geom_point(aes(y=Mean), size = 1, color = 'blue')

#5mins
#i <- which.min(metrics5$'Median MAE') #i should be equal for 60,20,5 for valid comparison
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
modelGRF5 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
yhat <- predict(modelGRF5,Xtest)
modelRF5 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
yhat.mean <- predict(modelRF5,Xtest)$predictions

df.pred5 <- data.frame(Age = Xtest$TestAge, True = Ytest, Mean = yhat.mean, yhat)
colnames(df.pred5) <- c('Age','True','Mean','Q025','Median','Q975')
df.pred5$width <- df.pred5$Q975 - df.pred5$Q025
df.pred5$AgeGroup <- ifelse(df.pred5$Age < as.numeric(summary(df.pred5$Age)[2]), 'Q1', 
	ifelse(df.pred5$Age > as.numeric(summary(df.pred5$Age)[3]), 'Q3', 'M'))

IW5 <- ggplot(df.pred5,aes(x=Age,y=width)) + geom_point() + stat_smooth(method='loess') + 
labs(x = 'Age', y = 'Prediction interval width') + theme_bw(base_size=22) 

ID5 <- ggplot(df.pred5,aes(x = width, color = AgeGroup)) + geom_density(lwd=1.2)

Pr5 <- ggplot(df.pred5, aes(x = seq(nrow(df.pred5)), y = Median, ymin=Q025, ymax=Q975)) + 
	geom_point(aes(y=True), size = 2) + geom_point(cex = 0.5) + geom_pointrange(color = "#009E73") + theme_bw(base_size=18) + 
	labs(x=NULL,y='Score') + scale_x_discrete(breaks=c(50,100,length(Ytest)), labels=c(50,100,length(Ytest)))
#geom_point(aes(y=Mean), size = 1, color = 'blue')

IW60|IW20|IW5
ID60|ID20|ID5
Pr5/Pr20/Pr60



#60mins
f <- 1 #Choose feature,from names(df),to construct ALE plots for the fitted model
k <- 50 #A numeric scalar that specifies the number of intervals into which the predictor range is 
		#divided when calculating the ALE plot effects.

Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
ALERF <- ALEPlot::PDPlot(Xtrain,modelRF60,pred.fun=Yhat,J=f,K=k)
ALEGRF <- lapply(seq(quantiles), function(x) {Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)[,x]);
	ALEPlot::PDPlot(Xtrain,modelGRF60,pred.fun=Yhat,J=f,K=k)$f.values})
df.ALE60 <- data.frame(x = ALERF$x.values, Mean = ALERF$f.values)
df.ALE60 <- cbind(df.ALE60, do.call(cbind,ALEGRF))
colnames(df.ALE60) <- c('x','Mean','Q025','Median','Q975')

df.ALE.melt60 <- reshape::melt(df.ALE60[,-1])
df.ALE.melt60$x <- rep(df.ALE60$x,ncol(df.ALE60[,-1]))

ALE60 <- ggplot(df.ALE.melt60, aes(x = x, y = value, col = variable)) + geom_point() + geom_line()

#20mins
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
ALERF <- ALEPlot::PDPlot(Xtrain,modelRF20,pred.fun=Yhat,J=f,K=k)
ALEGRF <- lapply(seq(quantiles), function(x) {Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)[,x]);
	ALEPlot::PDPlot(Xtrain,modelGRF20,pred.fun=Yhat,J=f,K=k)$f.values})
df.ALE20 <- data.frame(x = ALERF$x.values, Mean = ALERF$f.values)
df.ALE20 <- cbind(df.ALE20, do.call(cbind,ALEGRF))
colnames(df.ALE20) <- c('x','Mean','Q025','Median','Q975')

df.ALE.melt20 <- reshape::melt(df.ALE20[,-1])
df.ALE.melt20$x <- rep(df.ALE20$x,ncol(df.ALE20[,-1]))

ALE20 <- ggplot(df.ALE.melt20, aes(x = x, y = value, col = variable)) + geom_point() + geom_line()

#5mins
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
ALERF <- ALEPlot::PDPlot(Xtrain,modelRF5,pred.fun=Yhat,J=f,K=k)
ALEGRF <- lapply(seq(quantiles), function(x) {Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)[,x]);
	ALEPlot::PDPlot(Xtrain,modelGRF5,pred.fun=Yhat,J=f,K=k)$f.values})
df.ALE5 <- data.frame(x = ALERF$x.values, Mean = ALERF$f.values)
df.ALE5 <- cbind(df.ALE5, do.call(cbind,ALEGRF))
colnames(df.ALE5) <- c('x','Mean','Q025','Median','Q975')

df.ALE.melt5 <- reshape::melt(df.ALE5[,-1])
df.ALE.melt5$x <- rep(df.ALE5$x,ncol(df.ALE5[,-1]))

ALE5 <- ggplot(df.ALE.melt5, aes(x = x, y = value, col = variable)) + geom_point() + geom_line()


ALE60|ALE20|ALE5


