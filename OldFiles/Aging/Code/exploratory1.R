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
std_gait_measures_linear <- c('angular_velocity_stdev','base_tail_lateral_displacement_stdev',
	'limb_duty_factor_stdev','nose_lateral_displacement_stdev','speed_cm_per_sec_stdev',
	'step_length1_stdev','step_length2_stdev','step_width_stdev','stride_length_stdev',
	'tip_tail_lateral_displacement_stdev')
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

aging <- read.table("~/Documents/Projects/Aging/Data/completeagedb6.csv", header=TRUE, sep = ',', stringsAsFactors = FALSE) 
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
#aging$Tester <- factor(aging$Tester, levels = c('Scorer1', 'Scorer2', 'Scorer3', 'Scorer4'))
aging$Sex <- factor(aging$Sex, levels = c('-1','1'))
#levels(aging$Tester)[1] <- 'Scorer1'
#levels(aging$Tester)[2] <- 'Scorer2'
#levels(aging$Tester)[3] <- 'Scorer3'
#levels(aging$Tester)[4] <- 'Scorer4'
aging <- data.frame(aging)



ggplot(data.frame(aging), aes(x = TestAge, y = score, color=Tester)) + geom_point(alpha = 0.5) + labs(x = 'TestAge', y = 'CFI')
ggplot(aging, aes(x = as.factor(TestAge), y = score)) + geom_boxplot() + labs(x = 'TestAge', y = 'CFI')

p1 <- ggplot(aging, aes(x = (TestAge), y = score, color = Tester)) + 
geom_point(alpha = 0.8, size = 3) + 
scale_color_manual(values = c('#ff6600', '#008000', '#407294','#ffd700')) + 
labs(x = 'TestAge', y = 'CFI') 



model1 <- lmer(score ~ 1 + (1|Tester), data = aging)
model2 <- lmer(score ~ TestAge + (1|Tester), data = aging)
model3 <- lmer(score ~ TestAge + Sex + Weight + (1|Tester), data = aging)
model4 <- lmer(score ~ Sex + Weight + (1|Tester), data = aging)
model5 <- lmer(score ~ TestAge + Sex + (1|Tester), data = aging)

tmp <- data.frame(score = predict(model4, type = 'response', re.form = ~0, random.only = FALSE), TestAge = aging$TestAge, Tester = aging$Tester)
p2 <- ggplot(tmp, aes(x = (TestAge), y = score, color = Tester)) + geom_point(alpha = 0.5) + labs(x = 'TestAge', y = 'CFI') 
model1 <- lmer(score ~ 1 + (1|Tester), data = tmp)

cowplot::plot_grid(p1,p2,nrow = 1, align = 'h')

aging$score <- predict(model4, type = 'response', re.form =~ 0, random.only = FALSE)

tmp <- data.frame(resids = resid(model4), Tester = aging$Tester, TestAge = aging$TestAge)
S(lmer(resids ~ TestAge + (1|Tester), data = tmp))



df <- aging[,names(aging) %in% c('score',
	OFA_measures, median_gait_measures_linear, avg_gait_measures_linear,
	std_gait_measures_linear, iqr_gait_measures_linear,engineered_features_mean, engineered_features_stdev, 'median_width', 'median_length', 
	'median_rearpaw')]
p2 <- ggplot(aging, aes(x = (TestAge), y = score, color = Tester)) + geom_point(alpha = 0.5) + labs(x = 'TestAge', y = 'CFI') 

dfY <- df$score
df <- df[,-1]
df <- cbind(id = 1:dim(df)[1], df)
df_melt <- melt(df, id.vars = 'id')
df_melt <- cbind(df_melt, score = rep(dfY, 61))
ggplot(df_melt, aes(y = score, x = value)) + geom_point() + geom_smooth(method = 'loess') + facet_wrap(.~variable, scales = 'free')

p2 <- ggplot(aging, aes(x = (TestAge), y = score, color = Tester)) + geom_point(alpha = 0.5) + labs(x = 'TestAge', y = 'CFI') 




Age <- as.numeric(names(table(aging$TestAge)))
age_counts <- as.numeric(table(aging$TestAge))
frailty_comp <- data.frame(matrix(nrow = length(Age), ncol = length(frailty_parameters)))
a <- 0 
for (age in Age){
	a <- a + 1
	frailty_comp[a,] <- sapply(seq_along(frailty_parameters), function(f) {vec <- aging[aging$TestAge == age, 
		names(aging) %in% frailty_parameters[f]]; mean(vec)})
}
colnames(frailty_comp) <- frailty_parameters 
frailty_comp <- cbind(id = 1:dim(frailty_comp)[1], frailty_comp)
fcomp_melt <- melt(frailty_comp, id.vars = 'id')
fcomp_melt <- cbind(fcomp_melt, Age = rep(Age, dim(frailty_comp)[2]-1))


ggplot(fcomp_melt, aes(y = value, x = Age)) + geom_point() + geom_line() + 
facet_wrap(. ~ variable, scales = 'free')


Age <- as.numeric(names(table(aging$TestAge)))
age_counts <- as.numeric(table(aging$TestAge))
frailty_comp <- data.frame(matrix(nrow = length(Age), ncol = length(frailty_parameters)))
a <- 0 
for (age in Age){
	a <- a + 1
	frailty_comp[a,] <- sapply(seq_along(frailty_parameters), function(f) {vec <- aging[aging$TestAge == age, 
		names(aging) %in% frailty_parameters[f]]; sum(vec!=0)})
}
colnames(frailty_comp) <- frailty_parameters 
frailty_comp <- data.frame(apply(frailty_comp,2, function(x) cumsum(x))) 
frailty_comp <- cbind(id = 1:dim(frailty_comp)[1], frailty_comp)
fcomp_melt <- melt(frailty_comp, id.vars = 'id')
fcomp_melt <- cbind(fcomp_melt, Age = rep(Age, dim(frailty_comp)[2]-1))

ggplot(fcomp_melt, aes(y = value, x = Age)) + geom_point() + geom_line() + 
facet_wrap(. ~ variable, scales = 'free_x') + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
scale_x_continuous("Age", labels = as.character(c(20,40,60,80,100,120,140)), breaks = c(20,40,60,80,100,120,140)) + 
labs(y = 'Number of Animals')
ggsave('kD-figure.pdf')


Age <- as.numeric(names(table(aging$TestAge)))
age_counts <- as.numeric(table(aging$TestAge))
tmp <- array(dim = c(length(Age), 3, length(frailty_parameters)))
colnames(tmp) <- c("0","0.5","1")
aging$Coat.condition[aging$Coat.condition == 0.6] <- 0.5
a <- 0 
for (f in 1:length(frailty_parameters)){
	for (a in 1:length(Age)){
				vec <- aging[aging$TestAge == Age[a], names(aging) %in% frailty_parameters[f]]
				tmp[a,names(table(vec)),f] <- as.numeric(table(vec))
	}
}
tmp[is.na(tmp)] <- 0 	

data <- do.call(rbind,lapply(1:length(frailty_parameters), function(f) melt(t(tmp[,,f]))))
data <- cbind(Age = rep(rep(Age,each = 3), 27), data)
data <- data[, -which(names(data) %in% c("X2"))]
data <- cbind(Frailty_parameters = rep(frailty_parameters, each = 96), data)
names(data)[names(data) == "X1"] <- "Frequency"
data$Age <- factor(data$Age)
data$Frequency <- factor(data$Frequency)

ggplot(data, aes(fill = Frequency, y = value, x = Age)) + geom_bar(color = 'black',position = 'stack', stat = 'identity') + 
scale_fill_manual(values = c('white','grey','black')) + theme(axis.text.x = element_text(angle = 90)) + 
labs(y = 'Number of Animals') + facet_wrap(. ~ Frailty_parameters, scales = 'free')

ggsave('kD-figure2.pdf')


lapply(seq_along(frailty_parameters), function(f) ggplot(data[data$Frailty_parameters == frailty_parameters[f],], 
	aes(fill = Frequency, y = value, x = Age)) + geom_bar(color = 'black',position = 'fill', stat = 'identity') + 
scale_fill_manual(values = c('white','grey','black'))) 


ggplot(data[1:96,], 
	aes(fill = Frequency, y = value, x = Age)) + geom_bar(color = 'black',position = 'stack', stat = 'identity') + 
scale_fill_manual(values = c('white','grey','black'))