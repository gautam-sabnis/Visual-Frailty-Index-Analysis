#Step 3: Model and Algorithm Comparison
import os 
import numpy as np
import pandas as pd 
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
from plotnine import *
from pprint import pprint

from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import RepeatedKFold, GridSearchCV, cross_val_score
from sklearn.linear_model import LinearRegression, ElasticNet
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor

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
model = sm.MixedLM.from_formula('score ~ TestAge + Weight + Sex', data = data, groups = data['Tester'])
result = model.fit()
data['score'] = result.resid #Tester adjusted residual values 

df = data[['score'] + avg_gait_measures_linear + median_gait_measures_linear + std_gait_measures_linear + 
iqr_gait_measures_linear + OFA_measures + engineered_features_mean + engineered_features_stdev + ['median_rearpaw',  
'median_width', 'median_length']]
dfX, dfY = df.drop(['score'],axis = 1), df['score']


#Building pipelines
pipe0 = make_pipeline(StandardScaler(),LinearRegression())
pipe1 = make_pipeline(StandardScaler(),ElasticNet())
pipe2 = make_pipeline(StandardScaler(),SVR())
pipe3 = make_pipeline(StandardScaler(), RandomForestRegressor())
pipe4 = make_pipeline(StandardScaler(), GradientBoostingRegressor())

#Setting up hyper-parameter search grids
param_grid1 = [{'elasticnet__alpha':np.linspace(0.10,0.30,20), 'elasticnet__l1_ratio':np.linspace(0.1,1,10)}]
param_grid2 = [{'svr__kernel':['linear','poly','rbf'], 'svr__degree':[2,3], 'svr__C':np.linspace(0.01,0.02,10)}]
param_grid3 = [{'randomforestregressor__n_estimators': np.linspace(100,1000,10).astype('int32'),
'randomforestregressor__max_features': ['auto','sqrt','log2'], 
'randomforestregressor__max_depth': [int(x) for x in np.linspace(10,110,10)], 
'randomforestregressor__min_samples_split': [2,5,10], 
'randomforestregressor__min_samples_leaf': [1,2,4]}]
param_grid4 = [{'gradientboostingregressor__n_estimators':np.linspace(100,500,3).astype('int32'),
'gradientboostingregressor__learning_rate':[0.3,0.2,0.1,0.02,0.001],
'gradientboostingregressor__max_depth':[2,4,6,8], 'gradientboostingregressor__min_samples_leaf':[1,3,5,9],
'gradientboostingregressor__max_features':[1.0]}]

#Setting up multiple GridSearchCV objects, 1 for each algorithm 
gridcvs = {}
inner_cv = RepeatedKFold(n_splits = 2, n_repeats = 1, random_state=1234)

for pgrid, est, name in zip((param_grid1, param_grid2, param_grid3, param_grid4),
	(pipe1,pipe2,pipe3,pipe4),("ElasticNet","SVM","RF","GBM")):
	gcv = GridSearchCV(estimator = est, param_grid = pgrid, scoring = 'neg_mean_absolute_error', 
		cv = inner_cv, n_jobs = -1, verbose = 0, refit = True)
	gridcvs[name] = gcv

outer_cv = RepeatedKFold(n_splits = 5, n_repeats = 1, random_state = 1234)
nested_scores = {}

for name, gs_est in sorted(gridcvs.items()):
	nested_score = cross_val_score(gs_est, X = dfX, y = dfY, cv = outer_cv, n_jobs = -1, scoring = 'neg_mean_absolute_error')
	nested_scores[name] = nested_score
	print('%s | outer MAE %.2f%% +/- %.2f' % (name, nested_score.mean(), nested_score.std()))


