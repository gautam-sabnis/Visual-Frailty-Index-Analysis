import os 
import numpy as np
import pandas as pd 
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
from plotnine import *
from pprint import pprint

from sklearn.linear_model import LinearRegression, ElasticNet
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import cross_val_score, GridSearchCV, RandomizedSearchCV, RepeatedKFold
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline

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

#df1 = data[['TestAge'] + frailty_parameters]
#df1X, df1Y = df1.drop(['TestAge'], axis = 1), df1['TestAge']

#df2  = data[['TestAge'] + avg_gait_measures_linear + median_gait_measures_linear + std_gait_measures_linear + 
#iqr_gait_measures_linear + OFA_measures + engineered_features_mean + engineered_features_stdev + ['median_rearpaw',  
#'median_width', 'median_length']]
#df2X, df2Y = df2.drop(['TestAge'], axis = 1), df2['TestAge']

df = data[['score'] + avg_gait_measures_linear + median_gait_measures_linear + std_gait_measures_linear + 
iqr_gait_measures_linear + OFA_measures + engineered_features_mean + engineered_features_stdev + ['median_rearpaw',  
'median_width', 'median_length']]
dfX, dfY = df.drop(['score'],axis = 1), df['score']

#df4 = data[['score','TestAge']]
#df4X, df4Y = data[['TestAge']], data[['score']]

(ggplot(df4, aes(x = 'TestAge', y = 'score')) + geom_point(alpha = 0.5) + labs(x = 'TestAge (in weeks)', y = 'Score') + theme_538())

#M4:Predicting score from TestAge
#df4X -= df4X.mean() 
#df4X /= df4X.std()

#linreg_scores = cross_val_score(LinearRegression(), df4X, df4Y, scoring = 'neg_mean_squared_error', 
#	cv = RepeatedKFold(n_splits = 10, n_repeats = 1))


#M2:Predicting Age from video features
#scaler = StandardScaler()
#df2X = scaler.fit_transform(df2X)
#Enet_scores = cross_val_score(ElasticNet(), df2X, df2Y, scoring = 'neg_mean_absolute_error', 
#	cv = RepeatedKFold(n_splits = 10, n_repeats = 2))

#param_grid = [{'alpha': np.linspace(0.01,0.04,20), 'l1_ratio': np.linspace(1,1,1)}]
#grid_search = GridSearchCV(ElasticNet(max_iter = 100000), param_grid, cv = 5, scoring = 'neg_mean_absolute_error', return_train_score = True)
#grid_search.fit(df2X,df2Y)
#print("Best cross-validation score: {:.2f}".format(grid_search.best_score_))
#print("Best parameters: ", grid_search.best_params_)

#param_grid = np.array([(alpha, l) for alpha in np.linspace(0.01,0.06,20) for l in pd.Series(np.linspace(1,1,1), dtype = 'category')]) 
#train_scores_mean = -1*grid_search.cv_results_['mean_train_score']
#train_scores_std = grid_search.cv_results_['std_train_score']
#test_scores_mean = -1*grid_search.cv_results_['mean_test_score']
#test_scores_std = grid_search.cv_results_['std_test_score']
#Enet_results = pd.DataFrame({'alpha':param_grid[:,0], 'ratio': param_grid[:,1],'train_mean':train_scores_mean,'train_std':train_scores_std, 'test_mean':test_scores_mean,
#	'test_std':test_scores_std})
#Enet_results['ratio'] = Enet_results['ratio'].round(decimals = 1).astype('category')
#(ggplot(Enet_results, aes(x = 'alpha', y = 'test_mean', color = 'ratio')) + geom_line() + geom_point(aes(shape = 'ratio')) + 
#	theme_538() + labs(y = 'MAE', x = 'alpha'))

#M3:Predicting CFI from video features
scaler = StandardScaler()
df3X = scaler.fit_transform(df3X)

alphas = np.linspace(0.01,0.2,20)
ratios = np.linspace(1,1,1)
param_grid = [{'alpha':alphas, 'l1_ratio':ratios}]
grid_search = GridSearchCV(ElasticNet(max_iter = 100000), param_grid, cv = RepeatedKFold(n_splits = 10, n_repeats = 10),
	scoring = 'neg_mean_absolute_error',return_train_score = True)
grid_search.fit(df3X,df3Y)
print("Best cross-validation score: {:.2f}".format(grid_search.best_score_))
print("Best parameters: ", grid_search.best_params_)

param_grid = np.array([(alpha, l) for alpha in alphas for l in ratios]) 
train_scores_mean = -1*grid_search.cv_results_['mean_train_score']
train_scores_std = grid_search.cv_results_['std_train_score']
test_scores_mean = -1*grid_search.cv_results_['mean_test_score']
test_scores_std = grid_search.cv_results_['std_test_score']
Enet_results = pd.DataFrame({'alpha':param_grid[:,0], 'ratio': param_grid[:,1],'train_mean':train_scores_mean,'train_std':train_scores_std, 'test_mean':test_scores_mean,
	'test_std':test_scores_std})
Enet_results['ratio'] = Enet_results['ratio'].round(decimals = 1).astype('category')
(ggplot(Enet_results, aes(x = 'alpha', y = 'test_mean', color = 'ratio')) + geom_line() + geom_point(aes(shape = 'ratio')) + 
	theme_538() + labs(y = 'MAE', x = 'alpha'))

#Random Hyperparameter Search
n_estimators = np.linspace(100,200,2).astype('int32')
max_features = ['auto','sqrt','log2']
max_depth = [int(x) for x in np.linspace(10,110,11)]
min_samples_split = [2,5,10]
min_samples_leaf = [1, 2, 4]
random_grid = {'n_estimators':n_estimators, 'max_features':max_features, 'max_depth':max_depth,
'min_samples_split':min_samples_split, 'min_samples_leaf':min_samples_leaf}

random_search = RandomizedSearchCV(RandomForestRegressor(), random_grid, n_iter = 100, cv = 5, n_jobs=-1)
random_search.fit(df3X,df3Y)

param_grid = {'n_estimators': np.linspace(100,400,4).astype('int32'),'min_samples_split': [5,7,10],
'min_samples_leaf': [2,4,5],'max_features': ['sqrt'],'max_depth': np.linspace(80,120,4).astype('int32')}
grid_search = GridSearchCV(RandomForestRegressor(random_state= 12345), param_grid, cv = RepeatedKFold(n_splits = 10, n_repeats = 1, random_state=1234), 
	scoring = 'neg_mean_absolute_error', return_train_score = True, n_jobs= -1)
grid_search.fit(df3X,df3Y)

print("Best validation score: {:.2f}".format(-1*grid_search.best_score_))
print("Best parameters: ", grid_search.best_params_)




