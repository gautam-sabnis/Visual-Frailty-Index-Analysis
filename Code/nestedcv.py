import pandas as pd
import numpy as np

from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split, RepeatedKFold, GridSearchCV, cross_val_score
from sklearn.linear_model import ElasticNet
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestRegressor
from xgboost.sklearn import XGBRegressor

df = pd.read_csv(snakemake.input[0])
dfX, dfY = df.drop(['score'], axis = 1), df['score']

X_train,X_test,y_train,y_test = train_test_split(dfX, dfY, test_size=0.2, shuffle=True, random_state=1234)
CV = RepeatedKFold(n_splits=10, n_repeats=2, random_state=1234)

#Pipelines
pipe_enet = make_pipeline(StandardScaler(),ElasticNet())
pipe_svm = make_pipeline(StandardScaler(),SVR())
pipe_rf = make_pipeline(StandardScaler(), RandomForestRegressor())
pipe_xgboost = make_pipeline(StandardScaler(), XGBRegressor())

#Grids
enet_grid = [{'elasticnet__alpha':np.linspace(0.01,0.1,20), 'elasticnet__l1_ratio':np.linspace(0.01,1,10)}]
svm_grid = [{'svr__kernel':['linear','poly','rbf'], 'svr__degree':[2,3], 'svr__C':np.linspace(0.01,0.02,10)}]
rf_grid = {'randomforestregressor__n_estimators': np.linspace(100,1000,10).astype('int32'),
'randomforestregressor__max_features': ['auto','sqrt','log2'],
'randomforestregressor__max_depth': [int(x) for x in np.linspace(10,110,10)],
'randomforestregressor__min_samples_split': [2,5,7,10],
'randomforestregressor__min_samples_leaf': [1,2,4,6]}
xgb_grid = {"objective":["reg:squarederror"], "eta":[.3,.2,.1,.05,.01,.005], 'max_depth':list(range(2,8)),
        "subsample":[i/10. for i in range(5,11)], "colsample_bytree": [i/10. for i in range(7,11)], 
        "alpha":np.linspace(0.1,1,10)}


#Implement Nested Cross-validation
gridcvs = {} 

#Inner loop
inner_cv = RepeatedKFold(n_splits=2, n_repeats=1, random_state=1234)

for pgrid, est, name in zip((enet_grid,svm_grid,rf_grid,xgb_grid),(pipe_enet,pipe_svm,pipe_rf,pipe_xgboost),
	('ElasticNet','SVM','RF','GBM')): 
	if name == 'GBM':
		gcv = GridSearchCV(estimator=XGBRegressor(), param_grid=xgb_grid,cv=inner_cv, 
			scoring='neg_mean_absolute_error', n_jobs=-1, refit=True)
		gridcvs[name] = gcv
	else:
		gcv = GridSearchCV(estimator=est, param_grid=pgrid, cv=inner_cv, scoring='neg_mean_absolute_error',
		n_jobs=-1, refit=True)
		gridcvs[name] = gcv

#Outer loop
outer_cv = RepeatedKFold(n_splits=10, n_repeats=1, random_state=1234)
overall_scores = {}

for name, gs_est in sorted(gridcvs.items()):
	if name == 'GBM':
		nested_score_abs = cross_val_score(gs_est, X=X_train, y=y_train, cv=outer_cv, n_jobs=-1, 
		scoring="neg_mean_absolute_error")
		nested_score_squ = cross_val_score(gs_est, X=X_train, y=y_train, cv=outer_cv, n_jobs=-1,
			scoring="neg_mean_squared_error")
		overall_scores[name] = [nested_score_abs,nested_score_squ]
		print('%s | outer MAE %.2f +/- %.2f' % (name, nested_score_abs.mean(), nested_score_abs.std()))
		print('%s | outer MSE %.2f +/- %.2f' % (name, nested_score_squ.mean(), nested_score_squ.std()))
	else:
		nested_score = cross_val_score(gs_est, X=X_train, y=y_train, cv=outer_cv, n_jobs=-1, 
			scoring="neg_mean_absolute_error")
		nested_score_squ = cross_val_score(gs_est, X=X_train, y=y_train, cv=outer_cv, n_jobs=-1,
			scoring="neg_mean_squared_error")
		overall_scores[name] = [nested_score_abs,nested_score_squ]
		print('%s | outer MAE %.2f +/- %.2f' % (name, nested_score_abs.mean(), nested_score_abs.std()))
		print('%s | outer MSE %.2f +/- %.2f' % (name, nested_score_squ.mean(), nested_score_squ.std()))

pd.DataFrame(overall_scores).to_csv('nested_cv_results.csv', header=True)


