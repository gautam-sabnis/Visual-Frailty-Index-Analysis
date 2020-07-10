import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import ElasticNet, ElasticNetCV
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split, RepeatedKFold, GridSearchCV, cross_val_score
from sklearn.pipeline import Pipeline, make_pipeline
from sklearn.metrics import mean_absolute_error as mae

from bayes_opt import BayesianOptimization
from skopt import BayesSearchCV
from skopt.space import Real,Categorical,Integer

import xgboost as xgb

score_abs['gam1'].append(np.mean(gam1.predict(dfX[valid_idx]) - dfY[valid_idx]**2))

df = pd.read_csv(snakemake.input[0])
dfX, dfY = df.drop(['score'], axis = 1), df['score']

#Data Splitting 
X_train, X_test, y_train, y_test = train_test_split(dfX,dfY, test_size = 0.2, shuffle = True, random_state = 123)

CV = RepeatedKFold(n_splits = 10, n_repeats = 1, random_state=1234)

#LinearModel with Elastic Net
#Tuning Elastic Net:Grid Search
pipe = make_pipeline(StandardScaler(), ElasticNet())
enet_grid = [{'elasticnet__alpha':np.linspace(0.008,0.009,20), 'elasticnet__l1_ratio':np.linspace(0.01,1,10)}]
enet_search = GridSearchCV(pipe, param_grid = enet_grid, cv = CV, 
	scoring = 'neg_mean_absolute_error', return_train_score = True, n_jobs= -1)
enet_search.fit(X_train, y_train)
best_params = enet_search.best_params_

#Tuning Elastic Net:Bayesian Optimization using BayesianOptimization
#def enet_cv(l1_ratio, alpha, data, target):
#	estimator = ElasticNet(l1_ratio=l1_ratio, alpha=alpha)
#	cval = cross_val_score(estimator,data,target,scoring='neg_mean_absolute_error',cv=CV)
#	return cval.mean()

#def optimize_enet(data,target):
#	def enet_crossval(param1,param2):
#		return enet_cv(l1_ratio=param1, alpha=param2, data=data, target=target)

#	optimizer=BayesianOptimization(f=enet_crossval, pbounds={"param1":(0.1,1),
#		"param2":(0.1,0.9)}, random_state=1234)
#	optimizer.maximize(n_iter=30, init_points=2)
#	print("Final result:", optimizer.max)
#optimize_enet(X_train,y_train)

#Tuning Elastic Net:BayesianOptimization using scikit-optimize
pipe = Pipeline([('model',ElasticNet())])
enet_search = {'model':Categorical([ElasticNet()]), 'model__l1_ratio':Real(0.01,0.02,prior = 'log-uniform'),
	'model__alpha':Real(0.1,1,prior = 'log-uniform')}
opt = BayesSearchCV(pipe, [(enet_search, 100)], cv=CV, scoring='neg_mean_absolute_error')
opt.fit(X_train,y_train)

#Train with best tuning parameters to obtain train errors and test error 
pipe = make_pipeline(StandardScaler(), ElasticNet(alpha = enet_search.best_params_['elasticnet__alpha'], 
	l1_ratio = enet_search.best_params_['elasticnet__l1_ratio']))
enet = cross_val_score(pipe, X_train, y_train, cv = CV, scoring = 'neg_mean_absolute_error', n_jobs= -1)
pipe.fit(X_train, y_train)
enet_train = np.abs(enet.mean())
enet_test = mean_absolute_error(y_test, pipe.predict(X_test))

#Train with entire data set
pipe =  make_pipeline(StandardScaler(), ElasticNet())
enet_grid = [{'elasticnet__alpha':[best_params['elasticnet__alpha']], 
	'elasticnet__l1_ratio':[best_params['elasticnet__l1_ratio']]}]
enet_search = GridSearchCV(pipe, param_grid = enet_grid, cv = CV, 
	scoring = 'neg_mean_absolute_error', return_train_score = True, n_jobs= -1)
enet_search.fit(dfX,dfY)
enet_train_all = np.abs(enet_search.cv_results_['mean_train_score'].mean())
enet_test_all = np.abs(enet_search.cv_results_['mean_test_score'].mean()) 

#Same as above
#pipe = make_pipeline(StandardScaler(), ElasticNet(alpha = enet_search.best_params_['elasticnet__alpha'], 
#	l1_ratio = enet_search.best_params_['elasticnet__l1_ratio']))
#cross_val_score(pipe, dfX,dfY, cv = CV, scoring = 'neg_mean_absolute_error', n_jobs = -1).mean()

#Support Vector Machine
#Tuning SVM:Grid Search
pipe = make_pipeline(StandardScaler(), SVR())
svm_grid = [{'svr__kernel':['linear','poly','rbf'], 'svr__degree':[2,3], 'svr__C':np.linspace(0.01,0.02,10)}]
svm_search = GridSearchCV(pipe, param_grid = svm_grid, cv = CV, scoring= 'neg_mean_absolute_error', 
	return_train_score=True, n_jobs=-1)
svm_search.fit(X_train,y_train)
best_params = svm_search.best_params_

#Tuning Elastic Net:Bayesian Optimization using Bayesian Optimization
#def svm_cv(C,gamma,data,target):
#	estimator = SVR(C=C, gamma=gamma)
#	cval = cross_val_score(estimator, data,target,scoring="neg_mean_absolute_error",cv=CV)
#	return cval.mean()
#def optimize_svm(data,target):
#	def svm_crossval(param1,param2):
#		return svm_cv(C=param1, gamma=param2, data=data, target=target)
#	optimizer=BayesianOptimization(f=svm_crossval, pbounds={"param1":(0.01,1),"param2":(0.001,0.01)},random_state=1234)
#	optimizer.maximize(n_iter=30,init_points=2)
#	print("Final Results:", optimizer.max)
#optimize_svm(X_train,y_train)

#Tuning SVM:BayesianOptimization using scikit-optimize
pipe = Pipeline([('model',SVR())])
svm_search = {'model':Categorical([SVR()]), 'model__C':Real(1e-6,1e+1,prior = 'log-uniform'),
	'model__gamma':Real(1e-6,1e+1, prior='log-uniform'),'model__degree':Integer(1, 8),
	'model__kernel':Categorical(['linear','poly','rbf'])}
opt = BayesSearchCV(pipe, [(svm_search, 20)], cv=CV, scoring='neg_mean_absolute_error')

#Train with best tuning parameters to obtain train error and test error 
pipe = make_pipeline(StandardScaler(), SVR(kernel = best_params['svr__kernel'], C = best_params['svr__C']))
svm = cross_val_score(pipe, X_train,y_train, cv=CV, scoring='neg_mean_absolute_error',n_jobs=-1)
pipe.fit(X_train,y_train)
svm_train = np.abs(svm.mean())
svm_test = mean_absolute_error(y_test,pipe.predict(X_test))

#Train with entire data 
pipe = make_pipeline(StandardScaler(), SVR())
svm_grid = [{'svr__kernel':[best_params['svr__kernel']], 'svr__C':[best_params['svr__C']]}]
svm_search = GridSearchCV(pipe, param_grid=svm_grid, cv=CV, scoring='neg_mean_absolute_error',
	return_train_score=True, n_jobs=-1)
svm_search.fit(dfX,dfY)
svm_train_all = np.abs(svm_search.cv_results_['mean_train_score'].mean())
svm_test_all = np.abs(svm_search.cv_results_['mean_test_score'].mean()) 


#RandomForest
#Tuning RF:Grid Search
pipe = make_pipeline(StandardScaler(), RandomForestRegressor())
rf_grid = {'randomforestregressor__n_estimators': np.linspace(100,200,2).astype('int32'),
'randomforestregressor__max_features': ['auto','sqrt','log2'], 
'randomforestregressor__max_depth': [int(x) for x in np.linspace(10,110,11)], 
'randomforestregressor__min_samples_split': [2,5,10], 
'randomforestregressor__min_samples_leaf': [1,2,4]}
rf_search = GridSearchCV(pipe, param_grid = rf_grid, cv = Splits, scoring = 'neg_mean_absolute_error',
	return_train_score = True, n_jobs = -1)
rf_search.fit(X_train,y_train)
best_params = rf_search.best_params_ 

#Tuning RF:Bayesian Optimization
def rf_cv(n_estimators,max_features,max_depth,min_samples_split,min_samples_leaf,data,target):
	estimator = RandomForestRegressor(n_estimators=n_estimators,max_features=max_features,max_depth=max_depth,
		min_samples_split=min_samples_split,min_samples_leaf=min_samples_leaf)
	cval = cross_val_score(estimator,data,target,scoring="neg_mean_absolute_error",cv=CV)
	return cval.mean()
def optimize_rf(data,target):
	def rf_crossval(param1,param2,param3,param4,param5):
		return rf_cv(n_estimators=int(param1), max_features=int(param2), max_depth=int(param3), min_samples_split=int(param4), 
			min_samples_leaf=int(param5), data=data, target=target)
	optimizer=BayesianOptimization(f=rf_crossval, pbounds={"param1":(10,200),"param2":(5,dfX.shape[1]),
		"param3":(40,100), "param4":(1,10), "param5":(1,4)}, random_state=1234)
	optimizer.maximize(n_iter=100)
	print("Final Results:",optimizer.max)

optimize_rf(X_train, y_train)

#Train with best tuning paramers to obtain train and test error 
pipe = make_pipeline(StandardScaler(), RandomForestRegressor('randomforestregressor__n_estimators':
	[best_params['randomforestregressor__n_estimators']],'randomforestregressor__max_features':
	[best_params['randomforestregressor__max_features']], 'randomforestregressor__max_depth':
	[best_params['randomforestregressor__max_depth']], 'randomforestregressor__min_samples_split':
	[best_params['randomforestregressor__min_samples_split']], 'randomforestregressor__min_samples_leaf':
	[best_params['randomforestregressor__min_samples_leaf']]))
rf = cross_val_score(pipe, X_train,y_train, cv = CV, scoring='neg_mean_absolute_error',n_jobs=-1)
pipe.fit(X_train,y_train)
rf_train = np.abs(rf.mean())
rf_test = neg_mean_absolute_error(y_test, pipe.predict(X_test))

#Train with entire data 
pipe = make_pipeline(StandardScaler(), RandomForestRegressor())
rf_grid = [{'randomforestregressor__n_estimators':
	[best_params['randomforestregressor__n_estimators']],'randomforestregressor__max_features':
	[best_params['randomforestregressor__max_features']], 'randomforestregressor__max_depth':
	[best_params['randomforestregressor__max_depth']], 'randomforestregressor__min_samples_split':
	[best_params['randomforestregressor__min_samples_split']], 'randomforestregressor__min_samples_leaf':
	[best_params['randomforestregressor__min_samples_leaf']]}]
rf_search = GridSearchCV(pipe, param_grid = rf_grid, cv=CV, scoring='neg_mean_absolute_error',
	return_train_score=True,n_jobs=-1)
rf_search.fit(dfX,dfY)
rf_train_all = np.abs(rf_search.cv_results_['mean_train_score'].mean())
rf_test_all = np.abs(rf_search.cv_results_['mean_test_score'].mean()) 


#XGBoost
dtrain,dtest = xgb.DMatrix(X_train, label=y_train), xgb.DMatrix(X_test, label=y_test)

xgb_grid = {"objective":["reg:squarederror"], "eta":[.3,.2,.1,.05,.01,.005], 'max_depth':list(range(2,8)), 
	"subsample":[i/10. for i in range(5,11)], "colsample_bytree": [i/10. for i in range(7,11)], "alpha":np.linspace(0.1,1,10)}
xgb_search = GridSearchCV(XGBRegressor(), param_grid = xgb_grid, cv = CV, scoring = 'neg_mean_absolute_error',
 return_train_score = True, n_jobs= -1)
xgb_search.fit(X_train,y_train)

print(np.mean(np.abs(xgb_search.predict(X_test) - y_test)))
print(xgb_search.best_params_)





data = {'Train error': [enet_train], 'Test error': [enet_test], 'Best parameters': [enet_search.best_params_]}
results = pd.DataFrame(data, index = ['Elastic Net'])
results.to_csv(snakemake.output[0])
#print("\nTraining error = {}\nTest error = {}".format(enet_search.cv_results_['mean_train_score'].mean(),
#	mean_absolute_error(y_test, enet_search.best_estimator_.predict(X_test))))