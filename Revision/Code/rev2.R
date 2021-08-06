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
#levels(df$Tester) <- vapply(1:4, function(x) paste0('Scorer',x), character(1)) 
#df$Sex <- as.factor(ifelse(df$Sex == 'Male', -1, 1))
df$Sex <- as.factor(df$Sex) 

df_summary <- data.frame(Tester = names(table(df$Tester)), Count = as.numeric(table(df$Tester)))
df_summary <- cbind(Batch = rep(1,nrow(df_summary)), df_summary)

gaven_data <- read.csv('Data/gaven.csv', header = TRUE)
gaven_data$Tester <- as.factor(gaven_data$Collected.By)
gaven_data$score <- gaven_data$CFI.norm
gaven_data$TestAge <- gaven_data$Age.at.Test
gaven_data$Sex <- as.factor(gaven_data$Sex)
names(gaven_data)[names(gaven_data) == 'Body.Weight'] <- 'Weight'

df_summary[5,] <- c(2, 'Gaven', nrow(gaven_data))

laura_data <- readxl::read_excel('Data/FI_Round 4_mating summary.xlsx')

newdata <- read.csv('Data/newestdf.csv', header=TRUE)
newdata$MouseID <- sapply(seq(nrow(newdata)), function(x) as.factor(gsub(".avi","",gsub(".*/","",(newdata$NetworkFilename)[x]))))
newdata$Tester <- newdata$CollectedBy
newdata$Tester <- factor(newdata$Tester, levels = c('Gaven','Mackenzie'))
#levels(newdata$Tester) <- vapply(c(2,4), function(x) paste0('Scorer',x), character(1)) 
#newdata <- newdata[newdata$Tester == 'Gaven',]
#newdata$Tester <- droplevels(newdata$Tester)
#newdata$Sex <- as.factor(ifelse(newdata$Sex == 'Male', -1, 1)) 
newdata$Sex <- as.factor(newdata$Sex)
newdata$score <- newdata$CFI
newdata$TestAge <- newdata$Age.at.Test
newdata$Weight <- newdata$Body.Weight

newdf_summary <- data.frame(Batch = rep(3,2), Tester = names(table(newdata$Tester)), Count = as.numeric(table(newdata$Tester)))

df_summary <- rbind(df_summary,newdf_summary)
df_summary$Count <- as.numeric(df_summary$Count)

ggplot(df_summary, aes(x = Tester, y = Count, fill = Batch)) + geom_bar(stat = 'identity', position = 'dodge') + scale_fill_brewer(palette = 'Dark2') + theme_bw(base_size = 16)
#dev.print(pdf,'Revision/Plots/VFI-datasets.pdf')





#Old - Batch 1
table(df$Tester)
mod.lmm <- lmer(score ~ Weight + Sex + TestAge + (1|Tester), data = df[!duplicated(df$MouseID),]) #62% 

#Old - Batch 1 + 2
common_cols <- intersect(colnames(df), colnames(gaven_data))
DF <- data.frame()
DF <- rbind(subset(df, select = common_cols),subset(gaven_data, select = common_cols))
DF$Batch <- as.factor(rep(c('Batch1','Batch2'), c(nrow(df),nrow(gaven_data))))
table(DF$Tester)
table(DF$Batch)
mod.lmm <- lmer(score ~ Weight + Sex + TestAge + (1|Tester) + (1|Batch), data = DF[!duplicated(DF$MouseID),]) 
#Tester effect - 0.08% (50% if batch random effect is excluded)
#Batch effect - 87%

#New - Batch 3
table(newdata$Tester) 
mod.lmm <- lmer(score ~ Weight + Sex + TestAge + (1|Tester), data = newdata[!duplicated(newdata$MouseID),]) 

#Old + New - Batch 1 + 3
common_cols <- intersect(colnames(df),colnames(newdata))
DF <- data.frame()
DF <- rbind(subset(df, select = common_cols),subset(newdata, select = common_cols))
DF$Batch <- as.factor(rep(c('Batch1','Batch3'), c(nrow(df),nrow(newdata))))
table(DF$Tester)
table(DF$Batch)
mod.lmm <- lmer(score ~ Weight + Sex + TestAge + (1|Tester) + (1|Batch), data = DF[!duplicated(DF$MouseID),])
#Tester effect - 26% (54% if you ignore the batch effect)
#Batch effect - 24.5%

#Old + New - Batch 1+2+3
common_cols <- intersect(intersect(colnames(df),colnames(newdata)), colnames(gaven_data))
DF <- data.frame()
DF <- rbind(subset(df,select = common_cols), subset(gaven_data, select = common_cols), subset(newdata, select = common_cols))
DF$Batch <- as.factor(rep(c('Batch1','Batch2','Batch3'), c(nrow(df),nrow(gaven_data),nrow(newdata))))
table(DF$Tester)
table(DF$Batch)
mod.lmm <- lmer(score ~ Weight + Sex + TestAge + (1|Tester) + (1|Batch), data = DF[!duplicated(DF$MouseID),])

#Tester effect - 0.08% (51.5% if batch effect is ignored)
#Batch effect - 77%


#Adjust for the tester effect
mod.lmm <- lmer(score ~ Weight + Sex + TestAge + (1|Tester), data = DF[!duplicated(DF$MouseID),]) 
mod.lmm2 <- update(mod.lmm, . ~ . - (1|Batch))
mod.lmm3 <- update(mod.lmm, . ~ . - (1|Tester))
RLRsim::exactRLRT(mod.lmm,mod.lmm2,mod.lmm3)

df$score <- ifelse(df$Tester == 'Scorer1', df$score - ranef(mod.lmm)$Tester[1,],
	ifelse(df$Tester == 'Scorer2', df$score - ranef(mod.lmm)$Tester[2,],
	ifelse(df$Tester == 'Scorer3', df$score - ranef(mod.lmm)$Tester[3,], 
	df$score - ranef(mod.lmm)$Tester[4,])))
df <- df[,-which(names(df) %in% c('Tester'))]

df <- df[,names(df) %in% c('MouseID','score','TestAge','Sex',median_gait_measures_linear, iqr_gait_measures_linear,	OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures, rears_measures)]

df1 <- df[,names(df) %in% c('TestAge',median_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_median, ellipsefit_measures, rearpaw_pose_measures, rears_measures)]
corr.df <- data.frame(feature = names(df1[,-1]), corr = as.numeric(apply(df1[,-which(names(df1) %in% c('TestAge'))], 
	2, function(x) cor(df1$TestAge,x, use="complete.obs"))))
corr.df$Type <- ifelse(corr.df$feature %in% c(median_gait_measures_linear, iqr_gait_measures_linear), 'Gait',
	ifelse(corr.df$feature %in% c(OFA_measures), 'OFA', 'Engineered'))
corr.df$Type <- factor(corr.df$Type,levels=c('Gait','OFA','Engineered'))
p4 <- ggplot(corr.df, aes(x = seq(1,38), y = (corr), color = Type)) + geom_point(size = 3, stroke = 1, alpha = 0.8) + 
labs(x = 'Feature', y = 'Absolute Correlation') + scale_color_manual(values=c('#4daf4a','#984ea3','#ff7f00')) + 
theme_bw(base_size = 18) + theme(legend.position = 'none')


tmp <- cbind(corr.df1, corr.df2[,2],corr.df[,2])
colnames(tmp) <- c('feature','Young','Old','Both')
tmp_melt <- reshape2::melt(tmp)

ggplot(tmp_melt, aes(x = feature, y = value)) + geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + scale_fill_brewer(palette = 'Set1') + labs(y = 'Correlation')


#####Testers' scores' performances in predicting Age
set.seed(1234)
nsim <- 50
#quantiles <- c(0.025,0.5,0.975) #seq(0.1,0.9,length=9)
#help <- rep(1:3,c(547,100,200)) #since length(unique(df$MouseID)) = 847
#splits <- lapply(seq(nsim), function(x) sample(help))
MAE <- data.frame(matrix(0,nrow=nsim,ncol=3))
RMSE <- data.frame(matrix(0,nrow=nsim,ncol=3))
R2 <- data.frame(matrix(0,nrow=nsim,ncol=3))

invisible(lapply(seq(nsim), function(i){

cat("Simulation Run = ", i, "\n")
#tmp <- data.frame(MouseID = unique(DF$MouseID), id = splits[[i]])

#Tester 1
DF_Tester <- DF[DF$Tester == 'Gaven',]
DF_unique <- DF_Tester[!duplicated(DF_Tester$MouseID),]
train_index <- createDataPartition(DF_unique[,'TestAge'],p = 0.8, list = FALSE)
train_IDs <- DF_unique[train_index,'MouseID']
# mins
DFTrain <- DF_Tester[DF_Tester$MouseID %in% train_IDs,]
DFTest <- DF_Tester[!DF_Tester$MouseID %in% train_IDs,]

mod_gaven <- lm(TestAge ~ score, data = DFTrain)
ytest_gaven <- DFTest[,'TestAge']
tmp2 <- data.frame(score = DFTest[,'score'])
yhat_gaven <- as.numeric(predict(mod_gaven, newdata = tmp2))

#Tester 2
DF_Tester <- DF[DF$Tester == 'Hannah',]
DF_unique <- DF_Tester[!duplicated(DF_Tester$MouseID),]
train_index <- createDataPartition(DF_unique[,'TestAge'],p = 0.8, list = FALSE)
train_IDs <- DF_unique[train_index,'MouseID']
# mins
DFTrain <- DF_Tester[DF_Tester$MouseID %in% train_IDs,]
DFTest <- DF_Tester[!DF_Tester$MouseID %in% train_IDs,]

mod_hannah <- lm(TestAge ~ score, data = DFTrain)
ytest_hannah <- DFTest[,'TestAge']
tmp2 <- data.frame(score = DFTest[,'score'])
yhat_hannah <- as.numeric(predict(mod_hannah, newdata = tmp2))

#Tester 3
DF_Tester <- DF[DF$Tester == 'Mackenzie',]
DF_unique <- DF_Tester[!duplicated(DF_Tester$MouseID),]
train_index <- createDataPartition(DF_unique[,'TestAge'],p = 0.8, list = FALSE)
train_IDs <- DF_unique[train_index,'MouseID']
# mins
DFTrain <- DF_Tester[DF_Tester$MouseID %in% train_IDs,]
DFTest <- DF_Tester[!DF_Tester$MouseID %in% train_IDs,]

mod_Mackenzie <- lm(TestAge ~ score, data = DFTrain)
ytest_Mackenzie <- DFTest[,'TestAge']
tmp2 <- data.frame(score = DFTest[,'score'])
yhat_Mackenzie <- as.numeric(predict(mod_Mackenzie, newdata = tmp2))


MAE[i,1] <<- mean(abs(ytest_hannah - yhat_hannah))
RMSE[i,1] <<- sqrt(mean((ytest_hannah - yhat_hannah)^2))
R2[i,1] <<- cor(ytest_hannah,yhat_hannah)^2

MAE[i,2] <<- mean(abs(ytest_gaven - yhat_gaven))
RMSE[i,2] <<- sqrt(mean((ytest_gaven - yhat_gaven)^2))
R2[i,2] <<- cor(ytest_gaven,yhat_gaven)^2

MAE[i,3] <<- mean(abs(ytest_Mackenzie - yhat_Mackenzie))
RMSE[i,3] <<- sqrt(mean((ytest_Mackenzie - yhat_Mackenzie)^2))
R2[i,3] <<- cor(ytest_Mackenzie,yhat_Mackenzie)^2

}))

colnames(MAE) <- c("Hannah","Gaven","Mackenzie")
colnames(RMSE) <- c("Hannah","Gaven","Mackenzie")
colnames(R2) <- c("Hannah","Gaven","Mackenzie")


MAE.melt <- reshape::melt(MAE)
RMSE.melt <- reshape::melt(RMSE)
R2.melt <- reshape::melt(R2)

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=150)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
ggplot(df.metrics,aes(x = variable, y = value)) + geom_point() + geom_boxplot() + 
labs(x='Testers',y='Metric') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed)

dev.print(pdf,'Revision/Plots/Tester-performance-predict-FI-from-Age-entire-dataset.pdf')


#Batches performance in predicting ages 
set.seed(1234)
nsim <- 50
#help <- rep(1:3,c(550,100,200)) #since length(unique(df$MouseID)) = 850
#splits <- lapply(seq(nsim), function(x) sample(help))
MAE <- data.frame(matrix(0,nrow=nsim,ncol=4))
RMSE <- data.frame(matrix(0,nrow=nsim,ncol=4))
R2 <- data.frame(matrix(0,nrow=nsim,ncol=4))

invisible(lapply(seq(nsim), function(i){

cat("Simulation Run = ", i, "\n")
#tmp <- data.frame(MouseID = unique(DF$MouseID), id = splits[[i]])


##Old data set - Batch1
DF_batch <- DF[DF$Batch == 'Batch1',]
DF_unique <- DF_batch[!duplicated(DF_batch$MouseID),]
train_index <- createDataPartition(DF_unique[,'TestAge'],p = 0.8, list = FALSE)
train_IDs <- DF_unique[train_index,'MouseID']
# mins
DFTrain <- DF_batch[DF_batch$MouseID %in% train_IDs,]
DFTest <- DF_batch[!DF_batch$MouseID %in% train_IDs,]

mod_batch1 <- lm(TestAge ~ score, data = DFTrain)
ytest_batch1 <- DFTest[,'TestAge']
tmp2 <- data.frame(score = DFTest[,'score'])
yhat_batch1 <- as.numeric(predict(mod_batch1, newdata = tmp2))

##Old Data set - Batch 1 + Batch2
DF_batch <- DF[DF$Batch %in% c('Batch1','Batch2'),]
DF_unique <- DF_batch[!duplicated(DF_batch$MouseID),]
train_index <- createDataPartition(DF_unique[,'TestAge'],p = 0.8, list = FALSE)
train_IDs <- DF_unique[train_index,'MouseID']
# mins
DFTrain <- DF_batch[DF_batch$MouseID %in% train_IDs,]
DFTest <- DF_batch[!DF_batch$MouseID %in% train_IDs,]

mod_batch2 <- lm(TestAge ~ score, data = DFTrain)
ytest_batch2 <- DFTest[,'TestAge']
tmp2 <- data.frame(score = DFTest[,'score'])
yhat_batch2 <- as.numeric(predict(mod_batch2, newdata = tmp2))

#New data set - Batch3
DF_batch <- DF[DF$Batch == 'Batch3',]
DF_unique <- DF_batch[!duplicated(DF_batch$MouseID),]
train_index <- createDataPartition(DF_unique[,'TestAge'],p = 0.8, list = FALSE)
train_IDs <- DF_unique[train_index,'MouseID']
# mins
DFTrain <- DF_batch[DF_batch$MouseID %in% train_IDs,]
DFTest <- DF_batch[!DF_batch$MouseID %in% train_IDs,]

mod_batch3 <- lm(TestAge ~ score, data = DFTrain)
ytest_batch3 <- DFTest[,'TestAge']
tmp2 <- data.frame(score = DFTest[,'score'])
yhat_batch3 <- as.numeric(predict(mod_batch3, newdata = tmp2))

#Old + New data set - Batch 1 + Batch 3
DF_batch <- DF[DF$Batch %in% c('Batch1','Batch3'),]
DF_unique <- DF_batch[!duplicated(DF_batch$MouseID),]
train_index <- createDataPartition(DF_unique[,'TestAge'],p = 0.8, list = FALSE)
train_IDs <- DF_unique[train_index,'MouseID']
# mins
DFTrain <- DF_batch[DF_batch$MouseID %in% train_IDs,]
DFTest <- DF_batch[!DF_batch$MouseID %in% train_IDs,]

mod_batch4 <- lm(TestAge ~ score, data = DFTrain)
ytest_batch4 <- DFTest[,'TestAge']
tmp2 <- data.frame(score = DFTest[,'score'])
yhat_batch4 <- as.numeric(predict(mod_batch2, newdata = tmp2))



MAE[i,1] <<- mean(abs(ytest_batch1 - yhat_batch1))
RMSE[i,1] <<- sqrt(mean((ytest_batch1 - yhat_batch1)^2))
R2[i,1] <<- cor(ytest_batch1,yhat_batch1)^2


MAE[i,2] <<- mean(abs(ytest_batch2 - yhat_batch2))
RMSE[i,2] <<- sqrt(mean((ytest_batch2 - yhat_batch2)^2))
R2[i,2] <<- cor(ytest_batch2,yhat_batch2)^2

MAE[i,3] <<- mean(abs(ytest_batch3 - yhat_batch3))
RMSE[i,3] <<- sqrt(mean((ytest_batch3 - yhat_batch3)^2))
R2[i,3] <<- cor(ytest_batch3,yhat_batch3)^2

MAE[i,4] <<- mean(abs(ytest_batch4 - yhat_batch4))
RMSE[i,4] <<- sqrt(mean((ytest_batch4 - yhat_batch4)^2))
R2[i,4] <<- cor(ytest_batch4,yhat_batch4)^2

}))

colnames(MAE) <- c("B1","B1+B2","B3","B1+B3")
colnames(RMSE) <- c("B1","B1+B2","B3","B1+B3")
colnames(R2) <- c("B1","B1+B2","B3","B1+B3")


MAE.melt <- reshape::melt(MAE)
RMSE.melt <- reshape::melt(RMSE)
R2.melt <- reshape::melt(R2)

df.metrics <- rbind(MAE.melt,RMSE.melt,R2.melt)
df.metrics$Metric <- rep(c('MAE','RMSE','R2'), each=200)
df.metrics$Metric <- factor(df.metrics$Metric, levels = c('MAE','RMSE','R2'))
levels(df.metrics$Metric) <- c('MAE','RMSE','R^2')
ggplot(df.metrics,aes(x = variable, y = value)) + geom_point() + geom_boxplot() + 
labs(x='Batches',y='Metric') + theme_bw(base_size=18) + 
facet_wrap(~Metric, scales='free',labeller=label_parsed)

dev.print(pdf,'Revision/Plots/Batch-performance-predict-FI-from-Age.pdf')




#Cluster Results from different batches 

#Old + New - Batch 1 + 3
tmp1 <- read.csv('Revision/Results/Batches/1/FI-MAE-new.csv',header=TRUE)
tmp2 <- read.csv('Revision/Results/Batches/1/Age-RMSE-new.csv',header=TRUE)

tmp1 <- tmp1[,-c(1,2,3,5)]
tmp2 <- tmp2[,-c(1,2,3,5)]
c(mean(tmp1), sd(tmp1))
c(mean(tmp2), sd(tmp2))

