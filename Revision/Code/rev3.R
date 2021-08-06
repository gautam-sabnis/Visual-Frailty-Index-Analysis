setwd('/Users/sabnig/Lab-Projects/Analysis/Visual-Frailty-Index')

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

df <- df[,names(df) %in% c('MouseID','score','TestAge','Sex','Tester','Weight',median_gait_measures_linear, iqr_gait_measures_linear,	OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures, rears_measures)]


newdata <- read.csv('Data/oldagenew2.csv', header = TRUE)
newdata$MouseID <- sapply(seq(nrow(newdata)), function(x) as.factor(gsub(".avi","",gsub(".*/","",(newdata$NetworkFilename)[x]))))
names(newdata)[names(newdata) == 'Body.Weight'] <- 'Weight'
newdata$Tester <- newdata$CollectedBy
newdata$Tester <- factor(newdata$Tester, levels = c('Gaven','Mackenzie'))
levels(newdata$Tester) <- vapply(c(2,4), function(x) paste0('Scorer',x), character(1)) 
newdata$Sex <- as.factor(ifelse(newdata$Sex == 'Male', -1, 1)) 
newdata$score <- newdata$CFI_norm
newdata$TestAge <- newdata$Age.at.Test
df_newdata <- newdata[,names(newdata) %in% c(intersect(colnames(df),colnames(newdata)))]

df <- df[,-which(names(df) %in% c('angular_velocity_iqr'))]
df2 <- rbind(df,df_newdata)
df2 <- df2[complete.cases(df2),]

#Adjust for the tester effect
mod.lmm <- lme4::lmer(score ~ Sex + Weight + (1|Tester), data = df2) 
df2$score <- ifelse(df2$Tester == 'Scorer1', df2$score - lme4::ranef(mod.lmm)$Tester[1,],
	ifelse(df2$Tester == 'Scorer2', df2$score - lme4::ranef(mod.lmm)$Tester[2,],
	ifelse(df2$Tester == 'Scorer3', df2$score - lme4::ranef(mod.lmm)$Tester[3,], 
	df2$score - lme4::ranef(mod.lmm)$Tester[4,])))
df2 <- df2[,-which(names(df2) %in% c('Tester'))]

df2 <- df2[,names(df2) %in% c('MouseID','score','TestAge','Sex',median_gait_measures_linear, iqr_gait_measures_linear,	OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures, rears_measures)]

df2$Data <- as.factor(ifelse(df2$MouseID %in% unique(newdata$MouseID), 'New', 'Old')) 

#Some plots comparing the new and old datasets 
p1 <- ggplot(df2, aes(x = score, color = Data)) + geom_density() + theme_bw(base_size = 14) + scale_color_brewer(palette = 'Set1') + labs(x = 'Score') + theme(legend.position = 'none')

p2 <- ggplot(df2, aes(x = TestAge, color = Data)) + geom_density() + theme_bw(base_size = 14) + scale_color_brewer(palette = 'Set1')

p1|p2 

df_tmp <- data.frame(corr = c(as.numeric(cor(df2[,5:41])[lower.tri(cor(df2[df2$Data == 'Old',5:41]), diag=FALSE)]),as.numeric(cor(df2[,5:41])[lower.tri(cor(df2[df2$Data == 'New',5:41]), diag=FALSE)])), Data = as.factor(rep(c('Old','New'), each = 666)))
ggplot(df_tmp, aes(x = corr, fill = Data)) + geom_histogram(alpha = 0.7, color = 'black') + scale_fill_brewer(palette = 'Set1') + labs(x = 'Correlation', y = 'Count')



#Predictive modeling results
tmp1 <- read.csv('Data/Results/Age-MAE-new.csv',header=TRUE)
tmp2 <- read.csv('Data/Results/Age-RMSE-new.csv',header=TRUE)
tmp3 <- read.csv('Data/Results/Age-R2-new.csv',header=TRUE)



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


tmp1 <- read.csv('Data/Results/FI-MAE-new.csv',header=TRUE)
tmp2 <- read.csv('Data/Results/FI-RMSE-new.csv',header=TRUE)
tmp3 <- read.csv('Data/Results/FI-R2-new.csv',header=TRUE)

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
labs(x='Model',y='Metric') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed) + theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20), axis.title.x = element_text(size = 20), axis.title.y=element_text(size=20))

p1|p2
dev.print(pdf,'Revision/Plots/Temp2/4EF.pdf',width=19.45,height = 4)


######uniform sampling
sp <- split(df, list(df$TestAge))
samples <- lapply(sp, function(x) {
	if (nrow(x) > 10) {
		x[sample(1:nrow(x), 10, FALSE),]
	} else {
		x
	}
})
df_us <- do.call(rbind,samples)