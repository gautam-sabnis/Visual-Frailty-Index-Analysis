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

newdata <- read.csv('Data/newestdf.csv', header=TRUE)
newdata$MouseID <- sapply(seq(nrow(newdata)), function(x) as.factor(gsub(".avi","",gsub(".*/","",(newdata$NetworkFilename)[x]))))
newdata$Tester <- newdata$CollectedBy
newdata$Tester <- factor(newdata$Tester, levels = c('Gaven','Mackenzie'))
#levels(newdata$Tester) <- vapply(c(2,4), function(x) paste0('Scorer',x), character(1)) 
#newdata$Sex <- as.factor(ifelse(newdata$Sex == 'Male', -1, 1)) 
#newdata$score <- newdata$CFI
#newdata$TestAge <- newdata$Age.at.Test
#newdata$Weight <- newdata$Body.Weight

df <- newdata[,names(newdata) %in% c('NetworkFilename','Overall.Score','MouseID','CollectedBy','Body.Weight','Sex','Age.at.Test',
	avg_gait_measures_linear, median_gait_measures_linear, std_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_mean,engineered_features_median, ellipsefit_measures, 
	rears_measures, rearpaw_pose_measures)]
df <- df[complete.cases(df),] #remove 9 animals with missing values
df <- df[,c('MouseID', setdiff(colnames(df),'MouseID'))]

names(df)[names(df) == 'CollectedBy'] <- 'Collected.By'
names(df)[names(df) == 'Overall.Score'] <- 'score' 
names(df)[names(df) == 'Age.at.Test'] <- 'TestAge' 
names(df)[names(df) == 'Body.Weight'] <- 'Weight' 
names(df)[names(df) == 'Collected.By'] <- 'Tester'

df$Tester <- factor(df$Tester, levels = c('Gaven','Mackenzie'))
levels(df$Tester) <- vapply(1:2, function(x) paste0('Scorer',x), character(1)) 
df$Sex <- as.factor(ifelse(df$Sex == 'Male', -1, 1)) 

#Adjust for the tester effect
mod.lmm <- lmer(score ~ Sex + Weight + (1|Tester), data = df) 
df$score <- ifelse(df$Tester == 'Scorer1', df$score - ranef(mod.lmm)$Tester[1,],
	df$score - ranef(mod.lmm)$Tester[2,])
df <- df[,-which(names(df) %in% c('Tester'))]

df <- df[,names(df) %in% c('MouseID','score','TestAge','Sex',median_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures, rears_measures)]
df <- df[complete.cases(df),]


tmp1 <- read.csv('Revision/Results/Age-MAE-new80.csv',header=TRUE)
tmp2 <- read.csv('Revision/Results/Age-RMSE-new80.csv',header=TRUE)
tmp3 <- read.csv('Revision/Results/Age-R2-new80.csv',header=TRUE)



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

tmp1 <- read.csv('Revision/Results/FI-MAE-new80.csv',header=TRUE)
tmp2 <- read.csv('Revision/Results/FI-RMSE-new80.csv',header=TRUE)
tmp3 <- read.csv('Revision/Results/FI-R2-new80.csv',header=TRUE)

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
dev.print(pdf,'Revision/Plots/new_data/1B_new80.pdf',width=19.45,height=4.0)





