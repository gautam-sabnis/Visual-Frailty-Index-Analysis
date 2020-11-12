install.packages("viridis")
libraries <- c("ggplot2", "lme4", "cowplot","reshape","viridis")
lapply(libraries, require, character.only = TRUE)

aging <- read.table(snakemake@input[[1]], header = TRUE, sep = ',', stringsAsFactors = FALSE) 
ellipsefit <- read.csv(snakemake@input[[2]], header = TRUE, stringsAsFactors = FALSE) 
rearpaw <- read.csv(snakemake@input[[3]], header = TRUE, stringsAsFactors = FALSE) 
var_agingall <- read.csv(snakemake@input[[4]], header = TRUE, stringsAsFactors = FALSE) 

#aging <- read.csv('Data/completeagedb6.csv',header = TRUE, stringsAsFactors = FALSE)
#ellipsefit <- read.csv('Data/ellipsefit_all.csv',header = TRUE, stringsAsFactors = FALSE) 
#rearpaw <- read.csv('Data/rearpaw.csv',header = TRUE, stringsAsFactors = FALSE)
#var_agingall <- read.csv('Data/var_agingall.csv',header = TRUE, stringsAsFactors = FALSE)
#master <- read.csv('Data/masterdf.csv',header = TRUE, stringsAsFactors = FALSE)
#masterdf <- master[, !(names(master) %in% c('Animal_x','Animal_x.1','Animal_y','Unnamed..6',
#'Unnamed..7')]
#names(masterdf)[names(masterdf) == 'Overall.Score'] <- 'score'
#names(masterdf)[names(masterdf) == 'Animal'] <- 'MouseID'
#names(masterdf)[names(masterdf) == 'Age.at.Test'] <- 'TestAge' 
#names(masterdf)[names(masterdf) == 'Body.Weight'] <- 'Weight' 
#names(masterdf)[names(masterdf) == 'Collected.By'] <- 'Tester' 
#names(masterdf)[names(masterdf) == 'dAC_standard.deviation'] <- 'dAC_stdev'
#names(masterdf)[names(masterdf) == 'dB_standard_deviation'] <- 'dB_stdev'
#names(masterdf)[names(masterdf) == 'aABC_standard_deviation'] <- 'aABC_stdev'

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

frailty_parameters <- c('Alopecia','Loss.of.fur.colour','Dermatitis','Loss.of.whiskers','Coat.condition',
	'Piloerection','Cataracts','Eye.discharge.swelling','Microphthalmia','Corneal.opacity','Nasal.discharge',
	'Rectal.prolapse','Vaginal.uterine.','Diarrhea','Vestibular.disturbance','Vision.loss..Visual.Placing.',
	'Menace.reflex','Tail.stiffening','Gait.disorders','Tremor','Tumours','Distended.abdomen','Kyphosis',
	'Body.condition','Breathing.rate.depth','Malocclusions','Righting.Reflex')
avg_gait_measures_linear <- c('avg_angular_velocity','avg_base_tail_lateral_displacement',
	'avg_limb_duty_factor','avg_nose_lateral_displacement','avg_speed_cm_per_sec',
	'avg_step_length1','avg_step_length2','avg_step_width','avg_stride_length','avg_temporal_symmetry',
	'avg_tip_tail_lateral_displacement')
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


df <- df0[,names(df0) %in% c('Overall.Score',avg_gait_measures_linear, median_gait_measures_linear, std_gait_measures_linear,
iqr_gait_measures_linear, OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures,
rears_measures)]
df <- df[complete.cases(df),]



#aging <- cbind(aging, ellipsefit[1:nrow(aging),],rearpaw[1:nrow(aging),],var_agingall) 
aging <- df0
names(aging)[names(aging) == 'Overall.Score'] <- 'score' 
names(aging)[names(aging) == 'Age.at.Test'] <- 'TestAge' 
names(aging)[names(aging) == 'Body.Weight'] <- 'Weight' 
names(aging)[names(aging) == 'Collected.By'] <- 'Tester' 
aging$Tester <- factor(aging$Tester, levels = c('Amanda','Gaven','Hannah','Mackenzie'))
levels(aging$Tester) <- vapply(1:4, function(x) paste0('Scorer',x), character(1)) 
aging$Sex <- factor(aging$Sex, levels = c('-1','1')) 
aging <- aging[, !duplicated(colnames(aging))] 

#Plots
#p1: Scatterplot of CFI vs Age color coded by Tester. 
#p2: Boxplot of CFIs at each age value.
#p3: Scatterplot of CFI (adjusted for Tester effect) vs Age. 
#p4: Boxplot of adjusted CFIs at each age value. 

p1 <- ggplot(aging, aes(x = (TestAge), y = score)) + geom_jitter(aes(color = Sex), alpha = 0.7, size = 2, stroke=1) + 
scale_color_manual(values=c("#ff6600", "#377EB8")) + geom_smooth(method = 'lm', se = TRUE, color='black',
	linetype='dashed') + labs(x = 'TestAge', y = 'CFI') +  scale_x_continuous("Age", labels = as.character(c(20,40,60,80,100,120,140)), 
	breaks = c(20,40,60,80,100,120,140)) + theme_bw(base_size = 18) + theme(legend.position=c(.13,.9),
	legend.background = element_blank()) + 
	ggtitle(paste0('r = ',round(cor(aging$score, aging$TestAge),2)))

p1 <- ggplot(aging, aes(x = (TestAge), y = score, color = Tester)) + geom_point(alpha = 0.7, size = 2) +  
stat_summary(fun="mean", geom="line", size=1) + 
scale_color_manual(values = c('#ff6600','#008000', '#407294','#ffd700')) + labs(x = 'TestAge', y = 'CFI') +  
scale_x_continuous("Age", labels = as.character(c(20,40,60,80,100,120,140)), 
	breaks = c(20,40,60,80,100,120,140)) + theme_bw(base_size = 18)


p2 <- ggplot(aging, aes(x = as.factor(TestAge), y = score)) + geom_boxplot() + 
geom_point(aes(color = Tester), alpha = 0.8, size = 3) + 
scale_color_manual(values = c('#ff6600','#008000', '#407294','#ffd700')) + 
labs(x = 'TestAge', y = 'CFI') + theme_bw(base_size = 18) + 
theme(legend.position = 'top',axis.text.x = element_text(angle = 45, vjust = 0.5))

mod.lmm <- lmer(score ~ TestAge + Sex + Weight + (1|Tester), data = aging) 

aging$scoreGC <- ifelse(aging$Tester == 'Scorer1', aging$score - ranef(mod.lmm)$Tester[1,],
	ifelse(aging$Tester == 'Scorer2', aging$score - ranef(mod.lmm)$Tester[2,],
	ifelse(aging$Tester == 'Scorer3', aging$score - ranef(mod.lmm)$Tester[3,], 
	aging$score - ranef(mod.lmm)$Tester[4,])))

#aging$scoreGS <- predict(mod.lmm, type = 'response', re.form =~ 0, random.only = FALSE)

score.df <- data.frame(score = aging$score, GC = aging$scoreGC,Tester = aging$Tester, TestAge = aging$TestAge)

ggplot(score.df, aes(x = TestAge, y = GC)) + geom_jitter(size = 3, width = 0.5, stroke = 1,alpha=0.6,aes(color=Tester)) + 
stat_smooth(method = 'lm',color='black',linetype = 'dashed') + 
scale_color_brewer(palette="Dark2") + labs(x = 'TestAge', y = TeX('$CFI_{adj}$')) + theme_bw(base_size = 22) + 
theme(legend.position = 'top')
ggsave('Plots3/Fig1.jpg', width = 9, height = 9)


df1 <- aging[,names(aging) %in% c(avg_gait_measures_linear, median_gait_measures_linear, std_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures, rears_measures)]
df1 <- cbind(score = score.df$GC, df1)
corr.df <- data.frame(feature = names(df1[,-1]), corr = as.numeric(apply(df1[,-which(names(df1) %in% c('score'))], 
	2, function(x) cor(df1$score,x, use="complete.obs"))))
corr.df$Type <- ifelse(corr.df$feature %in% c(avg_gait_measures_linear, median_gait_measures_linear, std_gait_measures_linear, 
	iqr_gait_measures_linear), 'Gait',
	ifelse(corr.df$feature %in% c(OFA_measures), 'OFA', 'Engineered'))
ggplot(corr.df, aes(x = seq(1,60), y = abs(corr), color = Type)) + geom_point(size = 3, stroke = 1, alpha = 0.8) + 
labs(x = 'Feature', y = 'Correlation') + scale_color_brewer(palette="Dark2") + theme_bw(base_size = 22) + 
theme(legend.position = 'top')
ggsave('Plots3/Fig2.jpg', width = 9, height = 9)

tmp <- c(as.character(corr.df[abs(corr.df$corr) >= 0.4,'feature']))
corrplot::corrplot(cor(df1[,names(df1) %in% tmp],use="complete.obs"), order = "hclust", 
	tl.col = 'black',type = 'upper', tl.cex = 0.7)
dev.print(pdf,'Plots3/Fig3.pdf', width = 9, height = 9)





p3 <- ggplot(aging, aes(x = (TestAge), y = score)) + geom_point(alpha = 0.8, size = 3) +
geom_smooth(aes(color = Tester), method = 'lm', se = FALSE) +
scale_color_manual(values = c('#ff6600', '#008000', '#407294','#ffd700')) +
labs(x = 'TestAge', y = TeX('$CFI_{adj}$')) + 
scale_x_continuous("Age", labels = as.character(c(20,40,60,80,100,120,140)), 
	breaks = c(20,40,60,80,100,120,140)) + theme_bw(base_size = 18)

p4 <- ggplot(aging, aes(x = as.factor(TestAge), y = score)) + geom_boxplot() + 
geom_point(aes(color = Tester), alpha = 0.8, size = 3) + 
scale_color_manual(values = c('#ff6600','#008000', '#407294','#ffd700')) + 
labs(x = 'TestAge', y = TeX('$CFI_{adj}$')) + theme_bw(base_size = 18) + 
theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

cowplot::plot_grid(p1,p3, nrow = 1, align = 'h') 
#ggsave('Plots/CFIvsCFIadj.pdf', width = 14, height = 6)
ggsave(paste0(snakemake@output[[1]]), width = 14, height = 6)

cowplot::plot_grid(p2,p4, nrow = 1, align = 'h')
#ggsave('Plots/CFIvsCFIadj-box.pdf', width = 14, height = 6)
ggsave((snakemake@output[[2]]), width = 14, height = 6)

#This plot shows the loess fit of the relationship between CFI and each individual predictor. 
df <- aging[,names(aging) %in% c('score',
	OFA_measures, median_gait_measures_linear, avg_gait_measures_linear,
	std_gait_measures_linear, iqr_gait_measures_linear,engineered_features_mean, engineered_features_stdev, 
	'median_width', 'median_length','median_rearpaw')]
dfY <- df$score
df <- df[,-1]
df <- cbind(id = 1:dim(df)[1], df)
df_melt <- reshape::melt(df, id.vars = 'id')
df_melt <- cbind(df_melt, score = rep(dfY, dim(df)[2]-1))

ggplot(df_melt, aes(y = score, x = value)) + geom_point(alpha = 0.6) + geom_smooth(method = 'loess',color='red') + 
labs(y = 'CFI') + theme_bw(base_size = 12) + facet_wrap(.~variable, scales = 'free')
#ggsave('Plots/CFIvsX.pdf', width = 21, height = 12)
ggsave((snakemake@output[[3]]), width = 21, height = 12)

#This plot shows the cumulative distribution function of each frailty parameters versus age. 
Age <- as.numeric(names(table(aging$TestAge)))
age_counts <- as.numeric(table(aging$TestAge))
tmp <- sapply(seq_along(frailty_parameters), function(x) mapply(function(x,y) {vec <- aging[aging$TestAge == Age[y], 
	names(aging) %in% frailty_parameters[x]]; sum(vec!=0)}, x,seq_along(Age)))
colnames(tmp) <- frailty_parameters
tmp <- data.frame(apply(tmp,2, function(x) cumsum(x))) 
Q1 <- sapply(seq_along(frailty_parameters), function(x) {tmp2 <- which(tmp[,x] >= dim(df)[1]/4)[1]; Age[tmp2]})
Q2 <- sapply(seq_along(frailty_parameters), function(x) {tmp2 <- which(tmp[,x] >= dim(df)[1]/2)[1]; Age[tmp2]})  
Q3 <- sapply(seq_along(frailty_parameters), function(x) {tmp2 <- which(tmp[,x] >= dim(df)[1]*3/4)[1]; Age[tmp2]})  

tmp.max <- sapply(seq_along(frailty_parameters), function(x) max(tmp[,x])/dim(df)[1])
tmp <- cbind(id = 1:dim(tmp)[1], tmp)
tmp_melt <- reshape::melt(tmp, id.vars = 'id')
tmp_melt <- cbind(tmp_melt, Age = rep(Age, dim(tmp)[2]-1))

ggplot(tmp_melt, aes(y = value, x = Age)) + geom_point() + geom_line() + 
facet_wrap(. ~ variable, scales = 'free_x') + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
scale_x_continuous("Age", labels = as.character(c(20,40,60,80,100,120,140)), breaks = c(20,40,60,80,100,120,140)) + 
labs(y = 'Number of Animals') + theme_bw(base_size = 14)
#ggsave('Plots/kD-figure.pdf', width=15, height=10)
ggsave((snakemake@output[[4]]), width = 15, height = 10)

#This plot shows counts of categories (0,0.5,1) for each frailty parameters at all ages in form of stacked bar plots. 
tmp <- array(dim = c(length(Age), 3, length(frailty_parameters)))
colnames(tmp) <- c("0","0.5","1")
aging$Coat.condition[aging$Coat.condition == 0.6] <- 0.5 #Correction
for (f in 1:length(frailty_parameters)){
	for (a in 1:length(Age)){
				vec <- aging[aging$TestAge == Age[a], names(aging) %in% frailty_parameters[f]]
				tmp[a,names(table(vec)),f] <- as.numeric(table(vec))
	}
}
tmp[is.na(tmp)] <- 0 	
data <- do.call(rbind,lapply(1:length(frailty_parameters), function(f) reshape::melt(t(tmp[,,f]))))
data <- cbind(Age = rep(rep(Age,each = 3), 27), data)
data <- data[, -which(names(data) %in% c("X2"))]
data <- cbind(Frailty_parameters = rep(frailty_parameters, each = 96), data)
names(data)[names(data) == "X1"] <- "Frequency"
data$Age <- factor(data$Age)
data$Frequency <- factor(data$Frequency)
ggplot(data, aes(fill = Frequency, y = value, x = Age)) + geom_bar(color = 'black',position = 'stack', stat = 'identity') + 
scale_fill_viridis(discrete=TRUE) + theme(axis.text.x = element_text(angle = 90)) + 
labs(y = 'Number of Animals') + facet_wrap(. ~ Frailty_parameters, scales = 'free')
#ggsave('Plots/kD-figure2.pdf', width=20, height=12)
ggsave((snakemake@output[[5]]), width = 20, height = 12)

