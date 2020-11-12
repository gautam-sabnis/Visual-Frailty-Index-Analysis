i <- which.min(metrics60$'Median MAE')
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]
Xtest <- dfTest[,-which(names(dfTest) %in% c('score','Sex'))]
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
variable_importances <- list()
for (j in 1:3){
	modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[j], 
		honesty=FALSE)
	variable_importances[[j]] <- grf::variable_importance(modelGRF60)
}

lapply(seq(quantiles), function(x) {
	df.tmp <- data.frame(Feature = names(Xtrain),Importance = variable_importances[[x]]);
	df.tmp <- df.tmp[with(df.tmp,order(Importance)),];
	df.tmp$Feature <- factor(df.tmp$Feature, levels = df.tmp$Feature);
	assign(paste0("p",x),
		ggplot(tail(df.tmp,10),aes(x=Feature,y=Importance)) + geom_bar(stat='identity') + coord_flip(),
		inherits=TRUE);
})


vi_list <- list()
for (j in 1:3){
	modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[j], 
		honesty=FALSE)
	predictor <- Predictor$new(modelGRF60, data = as.data.frame(Xtrain), y = Ytrain)
	vi_list[[j]] <- FeatureImp$new(predictor, loss = "mae")
}

lapply(seq(quantiles), function(x) assign(paste0("p",x), plot(vi_list[[x]]), inherits=TRUE))

features_imp <- unique(c(vi_list[[1]]$results[1:10,1],vi_list[[2]]$results[1:10,1],vi_list[[3]]$results[1:10,1]))

Xtrain_imp <- Xtrain[,which(names(Xtrain) %in% features_imp)]



ggcorrplot(round(cor(Xtrain_imp),1)[,ncol(Xtrain_imp):1],
	lab=TRUE,type='upper',ggtheme=theme_bw(base_size=18), tl.cex=18)

lapply(seq(quantiles), function(x) {
	df.tmp <- data.frame(vi_list[[x]]$results);
	df.tmp <- df.tmp[with(df.tmp, order(importance)),];
	df.tmp$feature <- factor(df.tmp$feature, levels = df.tmp$feature);
	col <- ifelse(x==1,'#e41a1c',ifelse(x==2, '#4d4d4d','#377eb8'))
	assign(paste0("p",x),ggplot(df.tmp, aes(x = feature, y=importance, ymin=importance.05,ymax=importance.95)) + 
	geom_point(color=col) + geom_pointrange(color=col) + coord_flip() + theme_bw(base_size=18) + 
	labs(x = 'Importance', y = 'Feature'),inherits=TRUE);
})

df.tmp <- data.frame(vi_list[[1]]$results)
df.tmp <- df.tmp[with(df.tmp, order(importance)),]
df.tmp$feature <- factor(df.tmp$feature, levels = df.tmp$feature)
ggplot(df.tmp, aes(x = feature, y=importance, ymin=importance.05,ymax=importance.95)) + 
geom_point(color='#e41a1c') + geom_pointrange(color='#e41a1c') + coord_flip() + theme_bw(base_size=18) + 
labs(x = 'Importance', y = 'Feature')



features <- names(Xtrain)
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)


ice_curves <- lapply(features, FUN = function(feature) {
  ice <- partial(Yhat, pred.var = feature, ice = TRUE)
  autoplot(ice, alpha = 0.1) + 
    ylim(range(Ytrain)) +
    theme_light()
})
grid.arrange(grobs = ice_curves, ncol = 5)


features <- Xtrain
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
predictor.rf <- Predictor$new(
  model = modelGRF60, 
  data = features, 
  y = Ytrain, 
  predict.fun = Yhat
  )

rf.age <- FeatureEffect$new(predictor.rf, "TestAge", method="pdp+ice")
rf.age$center(min(features$TestAge))
p2 <- plot(rf.age) + ggtitle("RF")


tmp <- Interaction$new(predictor.rf, c('TestAge','median_width'))




###################################
Features <- c('TestAge','Weight','Center Time','Periphery Time','Corner Time','Center Distance',
	'Periphery Distance','Corner Distance','Grooming Bouts','Grooming Duration','Angular Velocity',
	'Base Tail LD','Limb Duty Factor','Nose LD','Speed','Step Length1','Step Length2','Step Width',
	'Stride Length','Tip Tail LD','Distance','Angular Velocity IQR','Base Tail LD IQR','Limb Duty Factor IQR',
	'Nose LD IQR','Speed IQR','Step Length1 IQR','Step Length2 IQR','Step Width IQR','Stride Length IQR',
	'Tip Tail LD IQR','dAC','dB','aABC','Width','Length','Rearpaw','Rearcount') 

Features <- Features[-1]

i <- which.min(metrics60$'Median MAE')
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- data.frame(dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]) 
Xtest <- data.frame(dfTest[,-which(names(dfTest) %in% c('score','Sex'))]) 
Xtrain <- data.frame(scale(Xtrain, center=TRUE, scale=TRUE))
Xtest <- data.frame(scale(Xtest, center=TRUE, scale=TRUE))
names(Xtrain) <- Features
names(Xtest) <- Features
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

#vi_list <- list()
#for (j in 1:3){
#	modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[j], 
#		honesty=FALSE)
#	predictor <- iml::Predictor$new(modelGRF60, data = as.data.frame(Xtrain), y = Ytrain)
#	vi_list[[j]] <- iml::FeatureImp$new(predictor, loss = "mae")
#}

quantiles <- c(0.025,0.50,0.975)
vi_list <- list()
for (j in 1:3){
	modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[j], 
		honesty=FALSE)
	vi_list[[j]] <- grf::variable_importance(modelGRF60)
}

A <- ggcorrplot(round(cor(Xtrain),1)[,ncol(Xtrain):1],
	lab=TRUE,type='upper',ggtheme=theme_bw(base_size=18), tl.cex=18)

#df.tmp <- rbind(vi_list[[1]]$results[,c(1,3)],vi_list[[2]]$results[,c(1,3)],vi_list[[3]]$results[,c(1,3)])
df.tmp <- data.frame(importance = do.call(rbind,vi_list)) 
df.tmp$feature <- names(Xtest)
df.tmp$Quantile <- factor(rep(c('Q025','M','Q975'), each=38)) 
df.tmp$Quantile <- factor(df.tmp$Quantile, levels = c('Q025','M','Q975'))


B <- ggplot(df.tmp, aes(x=feature,y=importance,fill=Quantile)) + 
geom_bar(stat='identity',position='dodge') +  
theme_bw(base_size=18) + labs(x = 'Feature', y = 'Importance') + 
scale_fill_manual(name = "Quantile", values = c('#e41a1c','#377eb8','#4daf4a'),
	labels = expression(Q[.025], Q[.50], Q[.975])) + 
theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))
dev.print(pdf,'Temp/var-imp.pdf',width=16,height=5)
#scale_fill_manual(values = c('#e41a1c','#4d4d4d','#377eb8'))


Quantiles <- c('Q025','M','Q975')
lapply(seq(Quantiles), function(x){
	f <- which.max(df.tmp[df.tmp$Quantile %in% paste0(Quantiles[x]),'importance']); #Choose feature,from names(df),to construct ALE plots for the fitted model
	feature <- Features[f];
	k <- 50; #A numeric scalar that specifies the number of intervals into which the predictor range is 
		#divided when calculating the ALE plot effects.

	i <- which.min(metrics60$'Median MAE');
	tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]]);
	dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],];
	dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],];
	dfTrain <- dfTrain[,-1]; #Remove MouseID
	dfTest <- dfTest[,-1]; #Remove MouseID

	Xtrain <- data.frame(dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]);
	Xtest <- data.frame(dfTest[,-which(names(dfTest) %in% c('score','Sex'))]);
	names(Xtrain) <- Features;
	names(Xtest) <- Features;
	Ytrain <- dfTrain[,'score'];
	Ytest <- dfTest[,'score'];

	modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE);
	modelRF60 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE);
	Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions);
	ALERF <- ALEPlot::ALEPlot(Xtrain,modelRF60,pred.fun=Yhat,J=f,K=k);
	ALEGRF <- lapply(seq(quantiles), function(x) {Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)[,x]);
	ALEPlot::ALEPlot(Xtrain,modelGRF60,pred.fun=Yhat,J=f,K=k)$f.values});
	df.ALE60 <- data.frame(x = ALERF$x.values);
	df.ALE60 <- cbind(df.ALE60, do.call(cbind,ALEGRF));
	colnames(df.ALE60) <- c('x','Q025','Median','Q975');

	df.ALE.melt60 <- reshape::melt(df.ALE60[,-1]);
	df.ALE.melt60$x <- rep(df.ALE60$x,ncol(df.ALE60[,-1]));
	df.ALE.melt60$variable <- factor(df.ALE.melt60$variable, levels=c("Q025","Median","Q975"));

	assign(paste0("p",x),ggplot(df.ALE.melt60, aes(x = x, y = value, col = variable)) + geom_point() + 
		geom_line() + theme_bw(base_size=18) + 
		scale_color_manual(name = "Quantile", values = c('#e41a1c','#377eb8','#4daf4a'),
		labels = expression(Q[.025], Q[.50], Q[.975])) + labs(x = paste0(feature), y = 'ALE') + 
		theme(legend.position='none'), inherits=TRUE);
})

f <- 38
feature <- Features[f]
k <- 50 #A numeric scalar that specifies the number of intervals into which the predictor range is 
		#divided when calculating the ALE plot effects.

i <- which.min(metrics60$'Median MAE')
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID

Xtrain <- data.frame(dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]) 
Xtest <- data.frame(dfTest[,-which(names(dfTest) %in% c('score','Sex'))]) 
names(Xtrain) <- Features
names(Xtest) <- Features
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
modelRF60 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
ALERF <- ALEPlot::ALEPlot(Xtrain,modelRF60,pred.fun=Yhat,J=f,K=k)
ALEGRF <- lapply(seq(quantiles), function(x) {Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)[,x]);
	ALEPlot::ALEPlot(Xtrain,modelGRF60,pred.fun=Yhat,J=f,K=k)$f.values})
df.ALE60 <- data.frame(x = ALERF$x.values)
df.ALE60 <- cbind(df.ALE60, do.call(cbind,ALEGRF))
colnames(df.ALE60) <- c('x','Q025','Median','Q975')

df.ALE.melt60 <- reshape::melt(df.ALE60[,-1])
df.ALE.melt60$x <- rep(df.ALE60$x,ncol(df.ALE60[,-1]))
df.ALE.melt60$variable <- factor(df.ALE.melt60$variable, levels=c("Q025","Median","Q975"))

p2 <- ggplot(df.ALE.melt60, aes(x = x, y = value, col = variable)) + geom_point() + geom_line() + theme_bw(base_size=18) + 
scale_color_manual(name = "Quantile", values = c('#e41a1c','#377eb8','#4daf4a'),
	labels = expression(Q[.025], Q[.50], Q[.975])) + labs(x = paste0(feature), y = 'ALE') + 
theme(legend.position='none')

f <- 32
feature <- Features[f]
k <- 50 #A numeric scalar that specifies the number of intervals into which the predictor range is 
		#divided when calculating the ALE plot effects.

i <- which.min(metrics60$'Median MAE')
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID
names(dfTrain)[-c(1,3,4)] <- Features
names(dfTrain)[-c(1,3,4)] <- Features

Xtrain <- data.frame(dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]) 
Xtest <- data.frame(dfTest[,-which(names(dfTest) %in% c('score','Sex'))]) 
#names(Xtrain) <- Features
#names(Xtest) <- Features
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']

modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles, 
		honesty=FALSE)
modelRF60 <- grf::regression_forest(Xtrain, Ytrain, tune.parameters = 'all',honesty=FALSE)
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
ALERF <- ALEPlot::ALEPlot(Xtrain,modelRF60,pred.fun=Yhat,J=f,K=k)
ALEGRF <- lapply(seq(quantiles), function(x) {Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)[,x]);
	ALEPlot::ALEPlot(Xtrain,modelGRF60,pred.fun=Yhat,J=f,K=k)$f.values})
df.ALE60 <- data.frame(x = ALERF$x.values)
df.ALE60 <- cbind(df.ALE60, do.call(cbind,ALEGRF))
colnames(df.ALE60) <- c('x','Q025','Median','Q975')

df.ALE.melt60 <- reshape::melt(df.ALE60[,-1])
df.ALE.melt60$x <- rep(df.ALE60$x,ncol(df.ALE60[,-1]))
df.ALE.melt60$variable <- factor(df.ALE.melt60$variable, levels=c("Q025","Median","Q975"))

p5 <- ggplot(df.ALE.melt60, aes(x = x, y = value, col = variable)) + geom_point() + geom_line() + theme_bw(base_size=18) + 
scale_color_manual(name = "Quantile", values = c('#e41a1c','#377eb8','#4daf4a'),
	labels = expression(Q[.025], Q[.50], Q[.975])) + labs(x = paste0(feature), y = 'ALE') + 
theme(legend.position='none')


Features <- c('TestAge','Weight','CenterTime','PeripheryTime','CornerTime','CenterDistance',
	'PeripheryDistance','CornerDistance','GroomingBouts','GroomingDuration','AngularVelocity',
	'BaseTailLD','LimbDutyFactor','NoseLD','Speed','StepLength1','StepLength2','StepWidth',
	'StrideLength','TipTailLD','Distance','AngularVelocityIQR','BaseTailLDIQR','LimbDutyFactorIQR',
	'NoseLDIQR','SpeedIQR','StepLength1IQR','StepLength2IQR','StepWidthIQR','StrideLengthIQR',
	'TipTailLDIQR','dAC','dB','aABC','Width','Length','Rearpaw','Rearcount') 
Features <- Features[-1]
i <- which.min(metrics60$'Median MAE')
tmp <- data.frame(MouseID = unique(df$MouseID), id = splits[[i]])
dfTrain <- df[df$MouseID %in% tmp$MouseID[tmp$id<3],]
dfTest <- df[df$MouseID %in% tmp$MouseID[tmp$id==3],]
dfTrain <- dfTrain[,-1] #Remove MouseID
dfTest <- dfTest[,-1] #Remove MouseID
#names(dfTrain)[-c(1,3,4)] <- Features
#names(dfTest)[-c(1,3,4)] <- Features

Xtrain <- data.frame(dfTrain[,-which(names(dfTrain) %in% c('score','Sex'))]) 
Xtest <- data.frame(dfTest[,-which(names(dfTest) %in% c('score','Sex'))]) 
names(Xtrain) <- Features
names(Xtest) <- Features
Ytrain <- dfTrain[,'score']
Ytest <- dfTest[,'score']
modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[2], 
		honesty=FALSE)
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
predictor <- iml::Predictor$new(
  model = modelGRF60, 
  data = as.data.frame(Xtrain),
  y = Ytrain, 
  predict.fun = Yhat
  )
interact <- Interaction$new(predictor)
tmp <- data.frame(interact$results)
colnames(tmp) <- c('Feature','Interaction')
tmp$Feature <- as.factor(tmp$Feature) 
tmp <- tmp[with(tmp, order(Interaction)),]
tmp$Feature <- factor(tmp$Feature, levels = tmp$Feature)
ggplot(tmp, aes(y = Feature, x = Interaction)) +
        geom_point(color = '#377eb8') + theme_bw(base_size=18) + 
        geom_segment(aes(yend = Feature, x = 0, xend = Interaction),color='#377eb8') +
        scale_x_continuous("Overall interaction strength") +
        scale_y_discrete("Features")

f <- c(1,16) #(30,34), (1,17), |||Agemodel c(1,18) (1,16)
k <- 50 #A numeric scalar that specifies the number of intervals into which the predictor range is 
		#divided when calculating the ALE plot effects.
j <- 2
modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[j], 
		honesty=FALSE)
Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
ALEGRF <- ALEPlot::ALEPlot(Xtrain,modelGRF60,pred.fun=Yhat,J=f,K=k)$f.values



Yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
ale_list <- list()
ice_list <- list()
for (j in 1:3){
	modelGRF60 <- grf::quantile_forest(Xtrain, Ytrain, quantiles = quantiles[j], 
		honesty=FALSE)
	predictor <- Predictor$new(modelGRF60, data = as.data.frame(Xtrain), y = Ytrain, predict.fun = Yhat)
	ale_list[[j]] <-  FeatureEffect$new(predictor.rf, "TestAge", method="ale")
	ice_list[[j]] <-  FeatureEffect$new(predictor.rf, "TestAge", method="pdp+ice")
}

df.tmp <- rbind(ice_list[[1]]$results,ice_list[[2]]$results,ice_list[[3]]$results)
df.tmp$Quartile <- rep(c('Q025','M','Q975'), each=20)
ggplot(df.tmp,aes(x=TestAge,y=.value,color=Quartile)) + geom_line() + 
scale_fill_brewer(palette='Set1')

