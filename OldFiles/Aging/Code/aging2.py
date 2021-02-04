import os 
import numpy as np
import pandas as pd 
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
from plotnine import *
from pprint import pprint

from sklearn.metrics import mean_absolute_error
from sklearn.linear_model import LinearRegression, ElasticNet
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestRegressor
from xgboost import XGBRegressor
from sklearn.model_selection import train_test_split,GridSearchCV, RandomizedSearchCV, RepeatedKFold
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline, make_pipeline

os.chdir("/Users/sabnig/Documents/Projects/Aging")

aging = pd.read_csv('Data/completeagedb6.csv')
ellipsefit = pd.read_csv('Data/ellipsefit_all.csv')
rearpaw = pd.read_csv('Data/rearpaw.csv')
var_agingall = pd.read_csv('Data/var_agingall.csv')

frailty_parameters = ['Alopecia','Loss of fur colour','Dermatitis','Loss of whiskers','Coat condition',
	'Piloerection','Cataracts','Eye discharge/swelling','Microphthalmia','Corneal opacity','Nasal discharge',
	'Rectal prolapse','Vaginal/uterine/','Diarrhea','Vestibular disturbance','Vision loss (Visual Placing)',
	'Menace reflex','Tail stiffening','Gait disorders','Tremor','Tumours','Distended abdomen','Kyphosis',
	'Body condition','Breathing rate/depth','Malocclusions','Righting Reflex']
avg_gait_measures_linear = ['avg_angular_velocity','avg_base_tail_lateral_displacement',
	'avg_limb_duty_factor','avg_nose_lateral_displacement','avg_speed_cm_per_sec',
	'avg_step_length1','avg_step_length2','avg_step_width','avg_stride_length','avg_temporal_symmetry',
	'avg_tip_tail_lateral_displacement']
avg_gait_measures_circular = ['avg_base_tail_lateral_displacement_phase','avg_nose_lateral_displacement_phase',
'avg_tip_tail_lateral_displacement_phase']
median_gait_measures_linear = ['median_angular_velocity','median_base_tail_lateral_displacement',
	'median_limb_duty_factor','median_nose_lateral_displacement','median_speed_cm_per_sec',
	'median_step_length1','median_step_length2','median_step_width','median_stride_length','median_temporal_symmetry',
	'median_tip_tail_lateral_displacement']
var_gait_measures_linear = ['angular_velocity_var','base_tail_lateral_displacement_var',
	'limb_duty_factor_var','nose_lateral_displacement_var','speed_cm_per_sec_var',
	'step_length1_var','step_length2_var','step_width_var','stride_length_var',
	'tip_tail_lateral_displacement_var']
std_gait_measures_linear = ['angular_velocity_stdev','base_tail_lateral_displacement_stdev',
	'limb_duty_factor_stdev','nose_lateral_displacement_stdev','speed_cm_per_sec_stdev',
	'step_length1_stdev','step_length2_stdev','step_width_stdev','stride_length_stdev',
	'tip_tail_lateral_displacement_stdev']
iqr_gait_measures_linear = ['angular_velocity_iqr','base_tail_lateral_displacement_iqr',
	'limb_duty_factor_iqr','nose_lateral_displacement_iqr','speed_cm_per_sec_iqr',
	'step_length1_iqr','step_length2_iqr','step_width_iqr','stride_length_iqr',
	'tip_tail_lateral_displacement_iqr']
OFA_measures = ['stride_count','Distance','center_time_secs','periphery_time_secs','corner_time_secs',
	'center_distance_cm','periphery_distance_cm','corner_distance_cm','grooming_number_bouts',
	'grooming_duration_secs']
engineered_features_mean = ['dAC_mean','dB_mean','aABC_mean']
engineered_features_stdev = ['dAC_stdev','dB_stdev','aABC_stdev']
engineered_features_min = ['dAC_min','dB_min','aABC_min']
engineered_features_max = ['dAC_max','dB_max','aABC_max']
animal_features = ['Sex','Weight'] #TestAge, Weight were removed

data = pd.concat([aging, var_agingall, ellipsefit[:-1], rearpaw[:-1]], axis = 1)
data = data.drop(columns = ['Unnamed: 0'])

data = data.rename(columns = {'Overall Score':'score','Age at Test':'TestAge','Body Weight':'Weight','Distance cm/sc':'Distance',
	'Collected By':'Tester'})
data['Tester'] = data['Tester'].astype('category')

#Adjust for Tester random effect 
model = sm.MixedLM.from_formula('score ~ 1', data = data, groups = data['Tester'])
result = model.fit()
#print(result.summary())
data['score'] = result.resid #Tester adjusted residual values 
for key,value in result.random_effects.items():
	data.loc[data.Tester == str(key),'score'] -= np.array(value) 

df = data[['score'] + avg_gait_measures_linear + median_gait_measures_linear + std_gait_measures_linear + 
iqr_gait_measures_linear + OFA_measures + engineered_features_mean + engineered_features_stdev + ['median_rearpaw',  
'median_width', 'median_length']]
dfX, dfY = df.drop(['score'],axis = 1), df['score']

#Stratifying a continuous target variable
bins = np.linspace(dfY.min(),dfY.max(),3)
dfY_binned = np.digitize(dfY, bins)

#Data Splitting 
X_train, X_test, y_train, y_test = train_test_split(dfX,dfY, test_size = 0.2, shuffle = True, random_state = 123)

Splits = RepeatedKFold(n_splits = 10, n_repeats = 2, random_state=1234)

#LinearModel with Elastic Net
pipe = make_pipeline(StandardScaler(), ElasticNet())
enet_grid = [{'elasticnet__alpha':np.linspace(0.10,0.30,20), 'elasticnet__l1_ratio':np.linspace(0.1,1,10)}]
enet_search = GridSearchCV(pipe, param_grid = enet_grid, cv = Splits, 
	scoring = 'neg_mean_absolute_error', return_train_score = True, n_jobs= -1)
enet_search.fit(X_train, y_train)
#print(enet_search.best_score_)
#print(enet_search.best_params_)
#print(enet_search.best_estimator_.score(X_test,y_test))
print("\nTraining error = {}\nTest error = {}".format(enet_search.best_score_,
	mean_absolute_error(y_test, enet_search.best_estimator_.predict(X_test))))


#SupportVectorMachine
pipe = make_pipeline(StandardScaler(), SVR())
svm_grid = {'svr__kernel':['linear','poly','rbf'], 'svr__degree':[2,3], 'svr__C':np.linspace(0.01,0.02,10)}
svm_search = GridSearchCV(pipe, param_grid = svm_grid, cv = Splits, 
	scoring = 'neg_mean_absolute_error', n_jobs = -1)
svm_search.fit(X_train,y_train)
print(svm_search.best_score_)
print(svm_search.best_params_)
print(svm_search.best_estimator_.score(X_test,y_test))
print("\nTraining error = {}\nTest error = {}".format(svm_search.best_score_,
	mean_absolute_error(y_test, svm_search.best_estimator_.predict(X_test))))


#RandomForest
pipe = make_pipeline(StandardScaler(), RandomForestRegressor())
rf_grid = {'randomforestregressor__n_estimators': np.linspace(100,200,2).astype('int32'),
'randomforestregressor__max_features': ['auto','sqrt','log2'], 
'randomforestregressor__max_depth': [int(x) for x in np.linspace(10,110,11)], 
'randomforestregressor__min_samples_split': [2,5,10], 
'randomforestregressor__min_samples_leaf': [1,2,4]}

rf_search = GridSearchCV(pipe, param_grid = rf_grid, cv = Splits, scoring = 'neg_mean_absolute_error',
	return_train_score = True, n_jobs = -1)
rf_search.fit(X_train,y_train) 
print(rf_search.best_score_)
print(rf_search.best_params_)

#XGBoost
dTrain = xgb.DMatrix(X_train, label = y_train)
dTest = xgb.DMatrix(X_test, label = y_test)

#Start with a random grid and then tune
xgb_grid = {'max_depth':6, 'min_child_weight': 1, 'eta':.3, 'subsample': 1, 'colsample_bytree': 1,
    'objective':'reg:squarederror',}
cv_results = xgb.cv(xgb_grid,dTrain,num_boost_round=999,seed=42,nfold=10,metrics={'mae'},
    early_stopping_rounds=10)

gridsearch_params = [(max_depth, min_child_weight) for max_depth in range(2,7) 
for min_child_weight in range(1,5)]

min_mae = float("Inf")
best_params = []
for max_depth, min_child_weight in gridsearch_params:
	print("CV with max_depth={}, min_child_weight={}".format(max_depth,min_child_weight))
	xgb_grid['max_depth'] = max_depth
	xgb_grid['min_child_weight'] = min_child_weight

	cv_results = xgb.cv(xgb_grid, dTrain,num_boost_round = 999, seed = 42, nfold = 10, metrics = {'mae'}, 
		early_stopping_rounds = 10)
	mean_mae = cv_results['test-mae-mean'].min()
	boost_rounds = cv_results['test-mae-mean'].idxmin()
	if mean_mae < min_mae:
		min_mae = mean_mae
		best_params = (max_depth, min_child_weight)

print("Best params: {}, {}, MAE: {}".format(best_params[0], best_params[1], min_mae))

xgb_grid["max_depth"] = best_params[0]
xgb_grid["min_child_weight"] = best_params[1]

gridsearch_params = [(subsample,colsample) for subsample in [i/10. for i in range(7,11)] 
for colsample in [i/10. for i in range(7,11)]]

min_mae = float("Inf")
best_params = []
for subsample, colsample in gridsearch_params:
	print("CV with subsample={}, colsample={}".format(subsample,colsample))
	xgb_grid["subsample"] = subsample
	xgb_grid["colsample_bytree"] = colsample

	cv_results = xgb.cv(xgb_grid, dTrain,num_boost_round = 999, seed = 42, nfold = 10, metrics = {'mae'}, 
		early_stopping_rounds = 10)
	mean_mae = cv_results['test-mae-mean'].min()
	boost_rounds = cv_results['test-mae-mean'].idxmin()
	if mean_mae < min_mae:
		min_mae = mean_mae
		best_params = (subsample, colsample)

print("Best params: {}, {}, MAE: {}".format(best_params[0], best_params[1], min_mae))

xgb_grid["subsample"] = best_params[0]
xgb_grid["colsample_bytree"] = best_params[1]

min_mae = float("Inf")
best_params = []
for eta in [.3,.2,.1,.05,.01,.005]:
	print("CV with eta = {}".format(eta))
	xgb_grid["eta"] = eta

	cv_results = xgb.cv(xgb_grid, dTrain,num_boost_round = 999, seed = 42, nfold = 10, metrics = {'mae'}, 
		early_stopping_rounds = 10)
	mean_mae = cv_results['test-mae-mean'].min()
	boost_rounds = cv_results['test-mae-mean'].idxmin()
	print("\tMAE {} for {} rounds\n".format(mean_mae, boost_rounds))
	if mean_mae < min_mae:
		min_mae = mean_mae
		best_params = eta

print("Best params: {}, MAE: {}".format(best_params, min_mae))
xgb_grid["eta"] = best_params

xgbmodel = xgb.train(xgb_grid,dTrain,num_boost_round = 999, evals = [(dTest, "Test")], early_stopping_rounds = 10)