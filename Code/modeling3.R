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
rears_measures <- c('rear_count','rears_0_5','rears_5_10')
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

dfall <- df0[,names(df0) %in% c('Overall.Score','MouseID','Collected.By','Body.Weight','Sex','Age.at.Test',
	avg_gait_measures_linear, median_gait_measures_linear, std_gait_measures_linear, iqr_gait_measures_linear,
	OFA_measures, engineered_features_median, ellipsefit_measures, rears_measures, rearpaw_pose_measures)]

df <- df0[,names(df0) %in% c('Overall.Score','MouseID','Collected.By','Body.Weight','Sex','Age.at.Test',
	median_gait_measures_linear, std_gait_measures_linear, iqr_gait_measures_linear,OFA_measures, engineered_features_median, 
	ellipsefit_measures, rearpaw_pose_measures,rears_measures)]

tmp <- data.frame(Features = names(apply(df[,-which(names(df) %in% c('Overall.Score','Collected.By'))],2,function(x) sum(is.na(x)))), 
	missing = as.numeric(apply(df[,-which(names(df) %in% c('Overall.Score','Collected.By'))],2,function(x) sum(is.na(x)))))
ggplot(tmp, aes(x = Features, y = missing)) + geom_bar(stat = 'identity') +  
theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1)) + labs(y = '# of MouseIDs with missing values')
#ggsave('Temp/missing-data.pdf')

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

#Remove outliers 
invisible(sapply(seq(ncol(df[,-which(names(df) %in% c('MouseID','Tester','Sex','score'))])), function(x) {
	vals <- df[,-which(names(df) %in% c('MouseID','Tester','Sex','score'))][x][[1]];
	outliers <- boxplot.stats(vals)$out
	ids <- match(outliers, vals) 
	df[,-which(names(df) %in% c('MouseID','Tester','Sex','score'))][x][[1]][ids] <<- NA
}))

df.tmp <- df[,-which(names(df) %in% c('MouseID','Tester','Sex','score'))]
#Remove outliers 

df.tmp.melt <- reshape::melt(df.tmp)
df.tmp.melt$score <- rep(df$score, ncol(df.tmp))
df.tmp.melt$Sex <- rep(df$Sex, ncol(df.tmp))
ggplot(df.tmp.melt, aes(x = value, y = score)) + geom_point(aes(color = Sex)) + 
stat_smooth(method = 'loess', color = 'black', linetype = 'dashed') + scale_color_brewer(palette = 'Set1') + 
facet_wrap(~variable, scales = 'free')

tmp <- data.frame(MouseID = unique(df$MouseID), 
	TestAge = sapply(seq(length(unique(df$MouseID))), function(x) df[df$MouseID == unique(df$MouseID)[x], 'TestAge'][1]))
tmp2 <- data.frame(TestAge = as.factor(names(table(tmp$TestAge))), Count = as.numeric(table(tmp$TestAge)))
tmp2$TestAge <- factor(tmp2$TestAge, levels = sort(as.numeric(levels(tmp2$TestAge))))
ggplot(tmp2, aes(x = TestAge, y = Count)) + geom_bar(stat = 'identity') + labs(y = 'Number of mice')


df_unique <- df[!duplicated(df$MouseID),]
trainIndex <- createDataPartition(df_unique$TestAge, p=0.80, list=FALSE)
dfTrain <- df[df$MouseID %in% df_unique$MouseID[trainIndex],]
dfTest <- df[df$MouseID %in% df_unique$MouseID[-trainIndex],]
df.tmp <- data.frame(Age = (unique(df$TestAge))) 
tmp1 <- unlist(sapply(seq(nrow(df.tmp)), function(x) {tmp <- data.frame(table(dfTrain$TestAge));
	ifelse(df.tmp$Age[x] %in% tmp$Var1, tmp[tmp$Var1 == df.tmp$Age[x],'Freq'],0)}))
tmp2 <- unlist(sapply(seq(nrow(df.tmp)), function(x) {tmp <- data.frame(table(dfTest$TestAge));
	ifelse(df.tmp$Age[x] %in% tmp$Var1, tmp[tmp$Var1 == df.tmp$Age[x],'Freq'],0)}))
df.tmp <- cbind(df.tmp, Train = tmp1, Test = tmp2)
df.tmp$Age <- as.factor(df.tmp$Age)
df.tmp$Age <- factor(df.tmp$Age, levels = sort(as.numeric(levels(df.tmp$Age))))
df.tmp.melt <- reshape::melt(df.tmp)
colnames(df.tmp.melt) <- c('TestAge','Data','value')
ggplot(df.tmp.melt,aes(x = TestAge, y = value, fill = Data)) + geom_bar(stat = 'identity', alpha = 0.8) + 
scale_fill_brewer(palette="Dark2") + labs(y = 'Number of mice')
#ggsave('Temp/train_test_split.pdf')
Xtrain <- dfTrain[,names(dfTrain) %in% c('TestAge','Weight','Sex',median_gait_measures_linear,iqr_gait_measures_linear,OFA_measures,
	engineered_features_median, ellipsefit_measures, rearpaw_pose_measures,rears_measures)]
Xtest <- dfTest[,names(dfTest) %in% c('TestAge','Weight','Sex',median_gait_measures_linear,iqr_gait_measures_linear,OFA_measures,
	engineered_features_median, ellipsefit_measures, rearpaw_pose_measures,rears_measures)]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain[,sapply(Xtrain,is.numeric)] <- apply(Xtrain[,sapply(Xtrain,is.numeric)], 2, function(x) (x - mean(x))/sd(x))
df.tmp <- data.frame(score = Ytrain, Xtrain)
mlr <- lm(score ~ ., data = df.tmp) #'mean' linear regression
df.tmp.mlr <- data.frame(Feature = names(summary(mlr)$coefficients[,1]), LSE = as.numeric(summary(mlr)$coefficients[,1]),
	LB = as.numeric(summary(mlr)$coefficients[,1]) - as.numeric(summary(mlr)$coefficients[,2]), 
	UB = as.numeric(summary(mlr)$coefficients[,1]) + as.numeric(summary(mlr)$coefficients[,2]))
qr <- quantreg::rq(score ~ ., data = df.tmp, tau = 1:9/10) #quantile regression
#colnames(Xtrain)[3] <- 'Sex1'
#sapply(seq(nrow(Xtrain)), function(x) plot(summary(qr), parm = paste0(colnames(Xtrain)[x])))
df.tmp <- data.frame(t(coef(qr))) 
df.tmp.melt <- reshape::melt(df.tmp)
df.tmp.melt <- cbind(df.tmp.melt,rep(c(1:9/10),ncol(df.tmp)))
colnames(df.tmp.melt) <- c('Feature','Coefficient','Quantile')
levels(df.tmp.melt$Feature)[levels(df.tmp.melt$Feature) == 'X.Intercept.'] <- '(Intercept)'
ggplot(df.tmp.melt, aes(x = Quantile, y = Coefficient, group = 1)) + geom_line() + geom_point() + 
geom_hline(data = df.tmp.mlr, aes(yintercept = LSE), color = 'red') + 
geom_hline(data = df.tmp.mlr, aes(yintercept = LB), color = 'red', linetype='dashed') + 
geom_hline(data = df.tmp.mlr, aes(yintercept = UB), color = 'red',linetype='dashed') + 
facet_wrap(~Feature, scales = 'free') + labs(y = 'QR Coefficient')
#dev.print(pdf,'Temp/qr-coefs.pdf', width = 16, height = 12)

Xtrain_noSex <- Xtrain[,-3] #remove Sex
lqr <- rqPen::rq.lasso.fit.mult(as.matrix(Xtrain_noSex), Ytrain, lambda = 0.01, tau_seq = 1:9/10)
df.tmp <- do.call(cbind,lapply(seq(9), function(x) lqr[[x]]$coefficients))
colnames(df.tmp) <- as.factor(seq(0.1,0.9,by=0.1))
df.tmp <- t(df.tmp)
df.tmp.melt <- reshape::melt(df.tmp)
colnames(df.tmp.melt) <- c('Quantile','Feature','Coefficient')
levels(df.tmp.melt$Feature)[levels(df.tmp.melt$Feature) == 'intercept'] <- '(Intercept)'
ggplot(df.tmp.melt, aes(x = Quantile, y = Coefficient, group = 1)) + geom_line() + geom_point() + 
geom_hline(yintercept=0, color = 'red') +  facet_wrap(~Feature, scales = 'free') + 
labs(y = 'Lasso QR Coefficient')
#dev.print(pdf,'Temp/lasso-qr-coefs.pdf', width = 16, height = 12)

#Repeated 10-fold CV
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 1, savePredictions = 'final')
grid <- expand.grid(lambda = seq(0, .1, length = 5))
lqr_fit <- train(Xtrain_noSex, Ytrain, method = 'rqlasso', tuneGrid = grid, trControl = fitControl, 
	preProc = c('center','scale'))


#####
quantiles <- c(0.5) #seq(0.1,0.9,length=9)
features <- colnames(Xtrain_noSex)
lmX <- paste(paste0("bols(",features,")"), collapse = " + ")
formula <- as.formula(paste("score", lmX, sep=" ~ "))
help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 451
splits <- lapply(seq(50), function(x) sample(help))

for (i in 1:50){
	df.tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
	dfTrain <- df[df$MouseID %in% df.tmp$MouseID[df.tmp$id < 3],]
	dfTest <- df[df$MouseID %in% df.tmp$MouseID[df.tmp$id == 3],]
	wfit <- as.numeric(dfTrain$MouseID %in% df.tmp$MouseID[df.tmp$id == 1])

	for (j in 1:length(quantiles)){
		it <- 10000
		inc <- 10000
		maxit <- 20000
		bc <- mboost::boost_control(mstop=it, nu=0.25, trace=TRUE, risk='oob')

		model <- mboost::gamboost(formula, data = dfTrain, control=bc, weights=wfit, 
			family=QuantReg(tau = quantiles[j]))
		model <- mboost::blackboost(Ytrain ~ ., data = DF, control=boost_control(mstop=100))

	}

}

###Generalized Random Forest
df <- df0[,names(df0) %in% c('Overall.Score','MouseID','Collected.By','Body.Weight','Sex','Age.at.Test',
	median_gait_measures_linear, std_gait_measures_linear, OFA_measures, engineered_features_median, 
	ellipsefit_measures, rearpaw_pose_measures,rears_measures)]

#df <- df[complete.cases(df),] #remove 9 animals with missing values
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

#df <- df[-266,]
df_unique <- df[!duplicated(df$MouseID),]
trainIndex <- createDataPartition(df_unique$TestAge, p=0.80, list=FALSE)
dfTrain <- df[df$MouseID %in% df_unique$MouseID[trainIndex],]
dfTest <- df[df$MouseID %in% df_unique$MouseID[-trainIndex],]

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','MouseID','Tester','TestAge','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','MouseID','Tester','TestAge','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

Xtrain[,sapply(Xtrain,is.numeric)] <- apply(Xtrain[,sapply(Xtrain,is.numeric)], 2, function(x) (x - mean(x))/sd(x))
Xtest[,sapply(Xtest,is.numeric)] <- apply(Xtest[,sapply(Xtest,is.numeric)], 2, function(x) (x - mean(x))/sd(x))

#Xtrain_noSex <- Xtrain[,-3] #remove Sex

model <- grf::regression_forest(Xtrain, Ytrain, 
	tune.parameters = 'all')
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
yhat <- predict(model,Xtest)
mean(abs(yhat - Ytest)[[1]])
ALEPlot::ALEPlot(Xtrain,model,pred.fun=Yhat,J=1,K=50)

q <- c(0.1,0.5,0.9)
model <- grf::quantile_forest(Xtrain, Ytrain,
	quantiles = q)
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
yhat <- predict(model,Xtest)
sapply(seq(q), function(x) mean(abs(yhat - Ytest)[[x]]))
ALEPlot::ALEPlot(Xtrain,model,pred.fun=Yhat,J=1,K=50)

df.tmp <- data.frame(Feature = names(Xtrain[,-which(names(Xtrain) %in% c('Sex','Weight','TestAge'))]), 
	Importance = grf::variable_importance(model))
df.tmp <- with(df.tmp, sort(df.tmp$Importance))
df.tmp$Feature <- with(df.tmp,reorder(Feature,Importance))
ggplot(df.tmp, aes(x = Feature, y = Importance)) + geom_bar(stat = 'identity') + coord_flip()

quantiles <- c(0.5) #seq(0.1,0.9,length=9)
