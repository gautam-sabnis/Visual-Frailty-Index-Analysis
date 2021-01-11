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

invisible(sapply(seq(ncol(df[,-which(names(df) %in% c('MouseID','Tester','Sex','score'))])), function(x) {
	vals <- df[,-which(names(df) %in% c('MouseID','Tester','Sex','score'))][x][[1]];
	outliers <- boxplot.stats(vals)$out
	ids <- match(outliers, vals) 
	df[,-which(names(df) %in% c('MouseID','Tester','Sex','score'))][x][[1]][ids] <<- NA
}))

dfX <- df[,-which(names(df) %in% c('MouseID','Tester','Sex','score'))]
dfX <- cbind(id = 1:dim(dfX)[1], dfX)
dfX.melt <- reshape::melt(dfX, id.vars = 'id')
dfX.melt <- cbind(dfX.melt, score = rep(df$score, dim(dfX)[2]-1), Sex = rep(df$Sex, dim(dfX)[2]-1))
ggplot(dfX.melt, aes(y = score, x = value)) + geom_point(alpha = 0.8,aes(color = Sex)) + 
facet_wrap(~ variable, scales = 'free') + stat_smooth(method='loess',color='black') + 
scale_color_manual(values = c('#ffc100','#006291')) + labs(y = NULL, x = NULL) + theme_bw(base_size = 15) + 
theme(legend.position='none') + labs(x = 'Feature', y = 'Score')

df.tmp <- df[,names(df) %in% c('score',median_gait_measures_linear, avg_gait_measures_linear)]
df.tmp.melt <- reshape::melt(df.tmp[,-1])
df.tmp.melt <- cbind(df.tmp.melt[1:(nrow(df.tmp)*length(Phenos.lin.Nomen)),], 
	df.tmp.melt[(nrow(df.tmp)*length(Phenos.lin.Nomen)+1):nrow(df.tmp.melt),])
df.tmp.melt <- cbind(Phenotype = rep(Phenos.lin.Nomen, each = nrow(df.tmp)),df.tmp.melt)
df.tmp.melt <- df.tmp.melt[,-c(2,4)]
df.tmp.melt <- cbind(df.tmp.melt, Score = rep(df.tmp$score, length(Phenos.lin.Nomen)))
colnames(df.tmp.melt) <- c('Phenotype','Average','Median','Score')
ggplot(df.tmp.melt, aes(x = Average, y = Median, color = Score)) + geom_point(alpha = 0.7) + 
geom_abline(intercept = 0, slope = 1) + scale_color_viridis_c() + theme_bw(base_size=16) + 
facet_wrap(~Phenotype, scales = 'free')
dev.print(pdf,'Temp/avg-median-gait.pdf',width=14,height=10)

df.tmp <- df[,names(df) %in% c('score',engineered_features_median,engineered_features_mean)]
p1 <- ggplot(df.tmp, aes(x = dAC_mean, y = dAC_median, color = score)) + geom_point(alpha = 0.7) + 
geom_abline(intercept = 0, slope = 1) + scale_color_viridis_c() + labs(x = 'Average', y = 'Median') + 
theme_bw(base_size=16) + ggtitle('dAC')
p2 <- ggplot(df.tmp, aes(x = dB_mean, y = dB_median, color = score)) + geom_point(alpha = 0.7) + 
geom_abline(intercept = 0, slope = 1) + scale_color_viridis_c() + labs(x = 'Average', NULL) + 
theme(legend.position = 'none') + theme_bw(base_size=16) + ggtitle('dB')
p3 <- ggplot(df.tmp, aes(x = aABC_mean, y = aABC_median, color = score)) + geom_point(alpha = 0.7) + 
geom_abline(intercept = 0, slope = 1) + scale_color_viridis_c() + labs(x = 'Average', y = NULL) + 
theme(legend.position = 'none') + theme_bw(base_size=16) + ggtitle('aABC')
p1|p2|p3
dev.print(pdf,'Temp/avg-median-engineered.pdf',width=16.4,height=4.9)

df.tmp <- df[,names(df) %in% c(std_gait_measures_linear, iqr_gait_measures_linear)]
names(df.tmp) <- c('std_angular_velocity','iqr_angular_velocity',
	'std_base_tail_lateral_displacement','iqr_base_tail_lateral_displacement',
	'std_limb_duty_factor','iqr_limb_duty_factor','std_nose_lateral_displacement',
	'iqr_nose_lateral_displacement','std_speed_cm_per_sec','iqr_speed_cm_per_sec',
	'std_step_length1','iqr_step_length1','std_step_length2','iqr_step_length2',
	'std_step_width','iqr_step_width','std_stride_length','iqr_stride_length',
	'std_tip_tail_lateral_displacement','iqr_tip_tail_lateral_displacement')
df.tmp <- df.tmp[,order(colnames(df.tmp))]
df.tmp.melt <- reshape::melt(df.tmp)
df.tmp.melt <- cbind(df.tmp.melt[1:(nrow(df.tmp)*length(Phenos.lin.Nomen)),], 
	df.tmp.melt[(nrow(df.tmp)*length(Phenos.lin.Nomen)+1):nrow(df.tmp.melt),])
df.tmp.melt <- cbind(Phenotype = rep(Phenos.lin.Nomen, each = nrow(df.tmp)),df.tmp.melt)
df.tmp.melt <- df.tmp.melt[,-c(2,4)]
df.tmp.melt <- cbind(df.tmp.melt, Score = rep(df$score, length(Phenos.lin.Nomen)))
colnames(df.tmp.melt) <- c('Phenotype','IQR','Stdev','Score')
ggplot(df.tmp.melt, aes(x = IQR, y = Stdev, color = Score)) + geom_point(alpha = 0.7) + 
geom_abline(intercept = 0, slope = 1) + scale_color_viridis_c() + theme_bw(base_size=16) + 
facet_wrap(~Phenotype, scales = 'free')
dev.print(pdf,'Temp/std-iqr-gait.pdf',width=14,height=10)



df <- df0[,names(df0) %in% c('Overall.Score','MouseID','Collected.By','Body.Weight','Sex','Age.at.Test',
	median_gait_measures_linear, std_gait_measures_linear, OFA_measures, engineered_features_median, 
	ellipsefit_measures, rearpaw_pose_measures,rears_measures)]
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
df <- df[,names(df) %in% c('score','Weight','TestAge',
	median_gait_measures_linear, std_gait_measures_linear, OFA_measures, engineered_features_median, 
	ellipsefit_measures, rearpaw_pose_measures,rears_measures)]

invisible(sapply(seq(ncol(df[,-which(names(df) %in% c('MouseID','Tester','Sex','score'))])), function(x) {
	vals <- df[,-which(names(df) %in% c('MouseID','Tester','Sex','score'))][x][[1]];
	outliers <- boxplot.stats(vals)$out
	ids <- match(outliers, vals) 
	df[,-which(names(df) %in% c('MouseID','Tester','Sex','score'))][x][[1]][ids] <<- NA
}))
df <- df[complete.cases(df),]
df <- df[-266,]
df.melt <- reshape::melt(df[,-3]) #remove score
df.melt <- cbind(df.melt, Score = rep(df$score,ncol(df)-1))
ggplot(df.melt, aes(x = value, y = Score)) + geom_point() + stat_smooth(method = 'loess') + 
facet_wrap(~variable, scales = 'free')

