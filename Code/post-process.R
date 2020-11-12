libraries <- c("ggplot2","cowplot","reshape")
lapply(libraries, require, character.only = TRUE)

nestedcv <- read.table(snakemake@input[[1]], header = TRUE, sep = ',', stringsAsFactors = FALSE) 
nestedcv <- -1*nestedcv[,-1]
colnames(nestedcv) <- c('ElasticNet','GAM','XGBoost','RF','SVM')
nestedcv <- cbind(nestedcv, id=1:dim(nestedcv)[1])
nestedcv_melt <- melt(nestedcv, id.vars='id')

ggplot(nestedcv_melt, aes(y = value, x = variable)) + geom_boxplot() + geom_point(alpha=0.7) + 
labs(x='Model', y='RMSE') + theme_bw(base_size=16) 
ggsave(paste0(snakemake@output[[1]]), width = 6, height = 6)








enet_mae <- c(-0.44340383, -0.41517411, -0.36909268, -0.37506004, -0.42915067, -0.40334989,
 -0.47453688, -0.47050348, -0.46510421, -0.56382753)
 xgboost_mae <- c(-0.3717584,  -0.44079061, -0.32978578, -0.31479888, -0.31764704, -0.36723099,
 -0.46498399, -0.4148297,  -0.35113978, -0.4399182)
 rf_mae <- c(-0.41641092, -0.41465422, -0.33133032, -0.38253849, -0.37981953, -0.34337045,
 -0.43629496, -0.40559008, -0.39880145, -0.39296445)
 svm_mae <- c(-0.45234715, -0.41279412, -0.33600995, -0.39075628, -0.41664808, -0.3703913,
 -0.50515417, -0.47767574, -0.46973098, -0.55351331)

 enet_mse <- c(-0.24775026, -0.2562085,  -0.19088096, -0.18939187, -0.25727381, -0.21551635,
 -0.30297684, -0.36589817, -0.32031539, -0.44084461)
 xgboost_mse <- c(-0.19926562, -0.37429913, -0.15019232, -0.1362785,  -0.16659921, -0.19375639,
 -0.36993251, -0.26890565, -0.20615484, -0.28652619)
 rf_mse <- c(-0.26634176, -0.32486491, -0.14597477, -0.19784239, -0.23732691, -0.17455572,
 -0.32326674, -0.28881607, -0.27779248, -0.22934658)
 svm_mse <- c(-0.26859802, -0.26576627, -0.16087592, -0.21853902, -0.24055699, -0.18885067,
 -0.33967261, -0.37602322, -0.34323942, -0.46349045)

 results_mae <- data.frame(enet_mae, xgboost_mae, rf_mae, svm_mae)
 colnames(results_mae) <- c('ElasticNet','XGBoost','RF','SVM')
 results_mae <- -1*results_mae
 nestedcv <- cbind(results_mae, id=1:dim(results_mae)[1])
 nestedcv_melt <- reshape::melt(nestedcv, id.vars='id')

ggplot(nestedcv_melt, aes(y = value, x = variable)) + geom_boxplot() + geom_point(alpha=0.7) + 
labs(x='Model', y='MAE') + theme_bw(base_size=16) 
ggsave('Plots2/results-nestedcv-mae.pdf', width=7, height=7)

results_mse <- data.frame(enet_mse, xgb_mse, rf_mse, svm_mse)
colnames(results_mse) <- c('ElasticNet','XGBoost','RF','SVM')
results_rmse <- sqrt(results_mse)
nestedcv <- cbind(results_rmse, id=1:dim(results_rmse)[1])
 nestedcv_melt <- reshape::melt(nestedcv, id.vars='id')

ggplot(nestedcv_melt, aes(y = value, x = variable)) + geom_boxplot() + geom_point(alpha=0.7) + 
labs(x='Model', y='RMSE') + theme_bw(base_size=22) 
ggsave('Plots3/train-rmse.pdf', width=7, height=7)


nestedcv <- results_mae
 [0.18169623515132413, 0.4768484196094757]


enet_mae <- c(-0.66410118, -0.54757529, -0.63688896, -0.68852744, -0.58926031, -0.56003796,
 -0.57112085, -0.5730912,  -0.65466129, -0.59747271)
xgboost_mae <- c(-0.58786841, -0.47553572, -0.51172925, -0.57719832, -0.4578486,  -0.53921481,
 -0.54593362, -0.49100408, -0.55761349, -0.50285009)
rf_mae <- c(-0.6220241,  -0.45939015, -0.5506644,  -0.657711,   -0.45110882, -0.52005551,
 -0.52559048, -0.49737148, -0.56427407, -0.47110826)
svm_mae <- c(-0.63556457, -0.53253518, -0.62779763, -0.70305999, -0.55698054, -0.58566048,
 -0.55299904, -0.55954651, -0.6456659,  -0.55981473)

enet_mse <- c(-0.68580263, -0.47658859, -0.58214196, -0.73608489, -0.58602809, -0.55432609,
 -0.47874075, -0.57721823, -0.71436764, -0.66364686)
xgboost_mse <- c(-0.62454027, -0.36260988, -0.43525204, -0.63270852, -0.41045791, -0.54661555,
 -0.52488367, -0.39380134, -0.5031503,  -0.39203206)
rf_mse <- c(-0.707635,   -0.39276591, -0.56453189, -0.95235644, -0.49031497, -0.60695494,
 -0.53187028, -0.44778251, -0.57404914, -0.37487514)
svm_mse <- c(-0.64128954, -0.45297921, -0.57412481, -0.75804124, -0.57206526, -0.6191409,
 -0.47953723, -0.59108751, -0.69469849, -0.62218424)




enet_mse <- c(0.79923702, 0.87392144, 1.14380037, 0.87156476, 1.16529838,
       1.01664974, 0.99841453, 0.89936428, 1.1500324 , 0.98925848)
svm_mse <- c(0.80849632, 0.92111026, 1.23658688, 0.81586516, 1.21359344,
       1.06848874, 0.98206573, 0.90357475, 1.14196455, 1.05751752)
rf_mse <- c(0.84890162, 0.8312366 , 1.36250474, 0.80762749, 1.1883813 ,
       1.1057616 , 0.97369264, 1.05071448, 1.13040291, 1.03757764)
xgb_mse <- c(0.91245866, 0.78095655, 1.31749552, 0.98697399, 1.14818607,
       1.10881952, 0.96554736, 1.0348661 , 1.14401114, 1.09479357)





df <- data.frame(Model = c('Enet','SVM','RF','XGBoost'), Training = c(0.93,0.91,0.49,0.39), Test = c(0.99,1.03,0.98,0.99))
df1 <- reshape::melt(df,id.vars = 'Model')
names(df1)[names(df1) == 'variable'] <- 'Error'
ggplot(df1, aes(x = Model, y = value, fill = Error)) + geom_bar(stat = 'identity', position = 'dodge')  + 
scale_fill_brewer(palette = 'Set1') + labs(y = 'MAE') + theme_bw(base_size = 22) + theme(legend.position = 'top') 
ggsave('Lab-Projects/Analysis/Visual-Frailty-Index/Plots3/results.pdf', width = 9, height = 9)



