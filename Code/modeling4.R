df <- df0[,names(df0) %in% c('Overall.Score','MouseID','Collected.By','Body.Weight','Sex','Age.at.Test',
	median_gait_measures_linear, iqr_gait_measures_linear, OFA_measures, engineered_features_median, 
	ellipsefit_measures, rearpaw_pose_measures,rears_measures)]
#df <- df[-268,]
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

features <- colnames(df)[-c(1,2,4)] #Remove MouseID,TestAge,score
XDesign <- paste(features, collapse = " + ")
formula <- as.formula(paste("score", XDesign, sep=" ~ "))

nsim <- 10
quantiles <- c(0.025,0.5,0.975) #seq(0.1,0.9,length=9)
help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 451
splits <- lapply(seq(nsim), function(x) sample(help))
err <- data.frame(matrix(0,nrow=nsim,ncol=2*(1+length(quantiles))+1)) 
colnames(err) <- c('RF-Mean','RF-UQ1','RF-UQ2','RF-UQ3','Boosting-Mean',
	'Boosting-UQ1','Boosting-UQ2','Boosting-UQ3','Boosted-RF')
yhats_list<- list()
RF_Mean <- list()
RF_UQ <- list()
Boosted_Mean <- list()
Boosted_UQ1 <- list()
Boosted_UQ2 <- list()
Boosted_UQ3 <- list()
Boosted_RF <- list() 
for (i in 1:nsim){

	cat("Sim =",i,"\n")
	tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
	#wfit <- as.numeric(dfTrain$MouseID %in% df.tmp$MouseID[df.tmp$id == 1])
	dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
	dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
	dfTrain <- dfTrain[,-1] #Remove MouseID
	dfTest <- dfTest[,-1] #Remove MouseID
	yhats <- data.frame(matrix(0,nrow = nrow(dfTest), ncol = ncol(err)))

	Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
	Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
	Ytrain <- dfTrain[,'score']
	Ytest <- dfTest[,'score']

	#Gradient Boosting 
	model <- blackboost(score ~ ., data = dfTrain)
	yhat <- predict(model,dfTest[,-2])
	err[i,5] <- mean(abs(yhat - dfTest[,2]))
	yhats[,5] <- yhat
	Boosted_Mean[[i]] <- model

	model <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
	yhat <- predict(model,Xtest)
	err[i,1] <- mean(abs(yhat - Ytest)[[1]])
	yhats[,1] <- yhat
	RF_Mean[[i]] <- model

	for (j in 1:length(quantiles)){
		#Quantile Gradient Boosting
		model <- blackboost(score ~ .,data = dfTrain,family = QuantReg(tau = quantiles[j]))
		yhat <- predict(model,dfTest[,-3])
		err[i,5+j] <- mean(abs(yhat - dfTest[,3]))
		yhats[,5+j] <- yhat
		#assign(paste0("Boosted_UQ",j), model)[[i]]
	}

	#Generalized (Quantile) Random Forest
	model <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
	yhat <- predict(model,Xtest)
	err[i,2:4] <- sapply(seq(quantiles), function(x) mean(abs(yhat - Ytest)[[x]]))
	yhats[,2:4] <- yhat
	RF_UQ[[i]] <- model

	#Boosted Regression Forest
	model <- grf::boosted_regression_forest(Xtrain,Ytrain,tune.parameter='all',honesty=FALSE)
	yhat <- predict(model,Xtest)
	err[i,9] <- mean(abs(yhat - Ytest)[[1]])
	yhats[,9] <- yhat
	Boosted_RF[[i]] <- model

	yhats_list[[i]] <- yhats

}

#Comparing results I 
err <- cbind(err, count = sapply(seq(20), function(x) {
	tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[x]]);
	dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],];
	dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],];
	count <- sum(as.numeric((dfTest$score <= quantile(dfTest$score,0.1)) | (dfTest$score >= quantile(dfTest$score,0.9))))

}))
err.melt <- reshape::melt(err[,-c(2,3,4)])
#err.melt <- err.melt[,-1]
names(err.melt) <- c('Method','MAE')
ggplot(err.melt, aes(x = Method, y = MAE)) + geom_point() + geom_boxplot() + 
theme(axis.text.x=element_text(angle=90, vjust=0.5,hjust=1))

#Comparing results II 
x <- 14
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[x]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
count <- sum(as.numeric((dfTest$score <= quantile(dfTest$score,0.1)) | (dfTest$score >= quantile(dfTest$score,0.9))))
df.tmp <- data.frame(True = dfTest$score, Q1 = yhats_list[[x]][,2], Q5 = yhats_list[[x]][,3],
	Q9 = yhats_list[[x]][,4], Mean = yhats_list[[x]][,1])
rownames(df.tmp) <- 1:nrow(df.tmp)
df.tmp.melt <- reshape::melt(df.tmp)
df.tmp.melt <- cbind(index = rep(seq(nrow(df.tmp)), ncol(df.tmp)), df.tmp.melt)

ggplot(df.tmp.melt[df.tmp.melt$variable %in% 'True',], aes(x = index, y = value)) + geom_line() + 
geom_point() + geom_hline(yintercept = quantile(df$score,probs=c(0.9)),color='#31a354') + 
geom_hline(yintercept = quantile(df$score,probs=c(0.1)),color='#31a354')

ggplot(df.tmp.melt[df.tmp.melt$variable %in% c('Q1','True','Mean'),], aes(x = index, y = value, color = variable)) + 
geom_point() + geom_line() + geom_hline(yintercept = quantile(df$score,probs=c(0.1)),color='#31a354') + 
geom_hline(yintercept = quantile(df$score,probs=c(0.9)),color='#31a354') + 
scale_color_manual(values=c('Q1' = 'red','True' = 'black','Mean' = 'blue')) 

ggplot(df.tmp.melt[df.tmp.melt$variable %in% c('Q9','True','Mean'),], aes(x = index, y = value, color = variable)) + 
geom_point() + geom_line() + geom_hline(yintercept = quantile(df$score,probs=c(0.9)),color='#31a354') + 
geom_hline(yintercept = quantile(df$score,probs=c(0.1)),color='#31a354') + 
scale_color_manual(values=c('Q9'='red','True'='black','Mean'='blue'))



#Comparing train and test sets 
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[x]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain$score <- as.factor(round(dfTrain$score))
dfTest$score <- as.factor(round(dfTest$score))
df.tmp <- data.frame(score = c(1,unique(as.factor(round(df$score)))))

tmp1 <- unlist(sapply(seq(nrow(df.tmp)), function(x) {tmp <- data.frame(table(dfTrain$score));
	ifelse(df.tmp$score[x] %in% tmp$Var1, tmp[tmp$Var1 == df.tmp$score[x],'Freq'],0)}))
tmp2 <- unlist(sapply(seq(nrow(df.tmp)), function(x) {tmp <- data.frame(table(dfTest$score));
	ifelse(df.tmp$score[x] %in% tmp$Var1, tmp[tmp$Var1 == df.tmp$score[x],'Freq'],0)}))
df.tmp <- cbind(df.tmp, Train = tmp1, Test = tmp2)
df.tmp <- df.tmp[-9,]
df.tmp$score <- as.factor(df.tmp$score)
df.tmp$score <- factor(df.tmp$score, levels = sort(as.numeric(levels(df.tmp$score))))
df.tmp.melt <- reshape::melt(df.tmp[,-1])
df.tmp.melt <- cbind(score = rep(df.tmp[,1],2),df.tmp.melt)
colnames(df.tmp.melt) <- c('Score','Data','Count')
ggplot(df.tmp.melt,aes(x = Score, y = Count, fill = Data)) + geom_bar(stat = 'identity', alpha = 0.8) + 
scale_fill_brewer(palette="Dark2") + labs(y = 'Number of mice')


#Interpretable ML 
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
f <- 38 #Choose feature,from names(df),to construct ALE plots for the fitted model
k <- 50 #A numeric scalar that specifies the number of intervals into which the predictor range is 
		#divided when calculating the ALE plot effects.
ALE.list <- list()

m <- which(err[,"RF-Mean"] == sort(err[,"RF-Mean"])[10])
model <- RF_Mean[[m]]
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[m]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('MouseID','score','Sex'))]
ALE1 <- ALEPlot::PDPlot(Xtrain,model,pred.fun=Yhat,J=f,K=k)
df.ALE1 <- data.frame('rear_count' = ALE1$x.values, Score = ALE1$f.values)




#####
nsim <- 20
quantiles <- c(0.025,0.5,0.975) #seq(0.1,0.9,length=9)
help <- rep(1:3,c(251,100,100)) #since length(unique(df$MouseID)) = 451
splits <- lapply(seq(nsim), function(x) sample(help))
metrics <- data.frame(matrix(0,nrow=nsim,ncol=6))
lapply(seq(nsim), function(i){
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
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

df.pred <- data.frame(Age = Xtest$TestAge, True = Ytest, Mean = yhat.mean, yhat)
print(ggplot(df.pred, aes(x = seq(nrow(df.pred)), y = X2, ymin=X1, ymax=X3)) + geom_point(aes(y=True), size = 3) + 
geom_point(cex = 0.5) + geom_pointrange(color = "#D55E00") + geom_point(aes(y=Mean), size = 3, color = 'blue'))
cat("Quantile (MAE,RMSE,Corr), Average(MAE,RMSE,Corr) = ", 
	c(mean(abs(Ytest-yhat[,2])), sqrt(mean((Ytest-yhat[,2])^2)), cor(Ytest,yhat[,2])^2,
		mean(abs(Ytest-yhat.mean)), sqrt(mean((Ytest-yhat.mean)^2)), cor(Ytest,yhat.mean)^2), 
	"\n")
metrics[i,] <<- c(mean(abs(Ytest-yhat[,2])), sqrt(mean((Ytest-yhat[,2])^2)), cor(Ytest,yhat[,2])^2,
		mean(abs(Ytest-yhat.mean)), sqrt(mean((Ytest-yhat.mean)^2)), cor(Ytest,yhat.mean)^2)
})
colnames(metrics) <- c("Median MAE","Median RMSE","Median R2","Mean MAE","Mean RMSE","Mean R2")

metrics.melt <- reshape::melt(metrics)
ggplot(metrics.melt, aes(x = variable, y = value)) + geom_point() + geom_boxplot()



i <- which.min(metrics$'Median MAE')
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
modelGRF <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
yhat <- predict(modelGRF,Xtest)
modelRF <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
yhat.mean <- predict(modelRF,Xtest)$predictions

df.pred <- data.frame(Age = Xtest$TestAge, True = Ytest, Mean = yhat.mean, yhat)
colnames(df.pred) <- c('Age','True','Mean','Q025','Median','Q975')
df.pred$width <- df.pred$Q975 - df.pred$Q025
df.pred$AgeGroup <- ifelse(df.pred$Age < as.numeric(summary(df.pred$Age)[2]), 'Q1', 
	ifelse(df.pred$Age > as.numeric(summary(df.pred$Age)[3]), 'Q3', 'M'))

ggplot(df.pred,aes(x=Age,y=width)) + geom_point() + stat_smooth(method='loess') + 
labs(x = 'Age', y = 'Prediction interval width') + theme_bw(base_size=22) 

ggplot(df.pred,aes(x = width, color = AgeGroup)) + geom_density(lwd=1.2)

ggplot(df.pred, aes(x = seq(nrow(df.pred)), y = Median, ymin=Q025, ymax=Q975)) + 
	geom_point(aes(y=True), size = 3) + geom_point(cex = 0.5) + geom_pointrange(color = "#D55E00") + 
geom_point(aes(y=Mean), size = 3, color = 'blue')


ggplot(df.pred, aes(x = seq(nrow(df.pred)), y = Median)) + geom_ribbon(aes(ymin=Q025, ymax=Q975),alpha = 0.5,
	fill="gold3") + 
	geom_point(aes(y=True), size = 3) + 
geom_point(aes(y=Median), size = 3, color = 'red') 

#Interpretable ML 
f <- 38 #Choose feature,from names(df),to construct ALE plots for the fitted model
k <- 50 #A numeric scalar that specifies the number of intervals into which the predictor range is 
		#divided when calculating the ALE plot effects.

Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
ALERF <- ALEPlot::PDPlot(Xtrain,modelRF,pred.fun=Yhat,J=f,K=k)
ALEGRF <- lapply(seq(quantiles), function(x) {Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)[,x]);
	ALEPlot::PDPlot(Xtrain,modelGRF,pred.fun=Yhat,J=f,K=k)$f.values})
df.ALE <- data.frame(x = ALERF$x.values, Mean = ALERF$f.values)
df.ALE <- cbind(df.ALE, do.call(cbind,ALEGRF))
colnames(df.ALE) <- c('x','Mean','Q025','Median','Q975')

df.ALE.melt <- reshape::melt(df.ALE[,-1])
df.ALE.melt$x <- rep(df.ALE$x,ncol(df.ALE[,-1]))

ggplot(df.ALE.melt, aes(x = x, y = value, col = variable)) + geom_point() + geom_line()

df.tmp <- data.frame(diff = Ytest - yhat[,2], Pred = yhat[,2])



######

features <- colnames(df)[-c(1,2,4)] #Remove MouseID,TestAge,score
fixed_effects <- paste(features, collapse = " + ")
formula <- as.formula(paste("score", fixed_effects, sep=" ~ "))

additive_model <- score ~ bols(TestAge) + 
						bbs(Weight, center=TRUE, df=1, knots=5) + 
						bols(center_time_secs) + bols(periphery_time_secs) + 
						bols(corner_time_secs) + bols(center_distance_cm) + 
						bols(periphery_distance_cm) + 
						bols(corner_distance_cm) + 
						bbs(grooming_number_bouts,center=TRUE,df=1,knots=5) + 
						bbs(median_angular_velocity, center=TRUE, df=1,knots=5) + 
						bbs(median_base_tail_lateral_displacement, center=TRUE, df=1,knots=5) + 
						bols(median_limb_duty_factor) + bols(median_nose_lateral_displacement) + 
						bols(median_speed_cm_per_sec) + bols(median_step_length1) + 
						bols(median_step_length2) + 
						bbs(median_step_width,center=TRUE,df=1,knots=5) + 
						bbs(median_stride_length,center=TRUE,df=1,knots=5) + 
						bbs(median_tip_tail_lateral_displacement,center=TRUE,df=1,knots=5) + 
						bols(stride_count) + 
						bols(Distance.cm.sc) + bols(angular_velocity_iqr) + 
						bbs(base_tail_lateral_displacement_iqr,center=TRUE,df=1,knots=5) + 
						bols(dAC_median) + bols(dB_median) + bols(aABC_median) + 
						bbs(median_width,center=TRUE,df=1,knots=5) + bols(median_length) + 
						bbs(median_rearpaw,center=TRUE,df=1,knots=5) + 
						bbs(rear_count,center=TRUE,df=1,knots=5) + bols(rears_0_5) 

fixed_effects <- paste0("bols(",features,")",collapse="+")
formula <- as.formula(paste("score", fixed_effects, sep="~"))

for(i in 1:1)
{	

	tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
	df.tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
	wfit <- as.numeric(dfTrain$MouseID %in% df.tmp$MouseID[df.tmp$id == 1])
	dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
	dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
	dfTrain <- dfTrain[,-1] #Remove MouseID
	dfTest <- dfTest[,-1] #Remove MouseID

    
    for(j in 1:length(quantiles))
    {
        it <- 10000
        inc <- 10000
        maxit <- 200000
        bc <- boost_control(mstop = it, nu=0.25, trace = TRUE, risk = "oob")
        
        model <- gamboost(formula,
                          data = dfTrain,
                          control = bc,
                          weights = wfit,	
                          family = QuantReg(tau = quantiles[j])
        )
        
        risk <- model$risk()                    
        while( ((risk[it-inc+1] - risk[it])>=0.05) &
               (it<=maxit) ) 
        {
            it <- it + inc
            model[it]
            risk <- model$risk()                    
        }
	}
}

####### Figure ###### 

ggplot(df, aes(x = score)) + geom_density(lwd=1.2) + labs(x = 'Score', y = 'Density') + 
theme_light(base_size = 22) + 
scale_x_continuous("Score", labels = seq(round(min(unique(df$score))), round(max(unique(df$score)))), 
	breaks = seq(round(min(unique(df$score))), round(max(unique(df$score)))))


ggplot(df,aes(x = TestAge, y = score)) + geom_point() + geom_quantile(quantiles = c(0.025,0.975), lwd = 1.5) + 
geom_smooth(method="lm", color = 'black')