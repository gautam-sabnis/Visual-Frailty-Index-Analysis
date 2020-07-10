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

 results_mae <- data.frame(enet_mae, gam_mae, xgboost_mae, rf_mae, svm_mae)
 colnames(results_mae) <- c('ElasticNet','GAM','XGBoost','RF','SVM')
 results_mae <- -1*results_mae

 results_mse <- data.frame(enet_mse, gam_mse, xgboost_mse, rf_mse, svm_mse)
 colnames(results_mse) <- c('ElasticNet','GAM','XGBoost','RF','SVM')
 results_mse <- -1*results_mse

nestedcv <- results_mae
 [0.18169623515132413, 0.4768484196094757]
