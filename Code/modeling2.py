import os
import pandas as pd
import numpy as np
from functools import reduce
import statsmodels.api as sm
from sklearn.preprocessing import StandardScaler, RobustScaler
from sklearn.model_selection import train_test_split, RepeatedKFold, GridSearchCV, cross_val_score
from sklearn.pipeline import make_pipeline
from sklearn.linear_model import ElasticNet
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestRegressor
import xgboost as xgb
from xgboost.sklearn import XGBRegressor
from pygam import GAM
from sklearn.impute import SimpleImputer
from sklearn.metrics import mean_absolute_error
from scipy import stats
from scipy.stats import norm, skew
from scipy.special import boxcox1p
import shap
import matplotlib.pyplot as plt

os.chdir('/Users/sabnig/Lab-Projects/Analysis/Visual-Frailty-Index')

frailty_parameters = ['Alopecia','Loss of fur colour','Dermatitis','Loss of whiskers','Coat condition',
        'Piloerection','Cataracts','Eye discharge/swelling','Microphthalmia','Corneal opacity','Nasal discharge',
        'Rectal prolapse','Vaginal/uterine/','Diarrhea','Vestibular disturbance','Vision loss (Visual Placing)',
        'Menace reflex','Tail stiffening','Gait disorders','Tremor','Tumours','Distended abdomen','Kyphosis',
        'Body condition','Breathing rate/depth','Malocclusions','Righting Reflex']
avg_gait_measures_linear = ['avg_angular_velocity','avg_base_tail_lateral_displacement',
        'avg_limb_duty_factor','avg_nose_lateral_displacement','avg_speed_cm_per_sec',
        'avg_step_length1','avg_step_length2','avg_step_width','avg_stride_length',
        'avg_tip_tail_lateral_displacement']
median_gait_measures_linear = ['median_angular_velocity','median_base_tail_lateral_displacement',
        'median_limb_duty_factor','median_nose_lateral_displacement','median_speed_cm_per_sec',
        'median_step_length1','median_step_length2','median_step_width','median_stride_length',
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
rearpaw_pose_measures = ['median_rearpaw']
rears_measures = ['rear_count','rears_0_5','rears_5_10']
ellipsefit_measures = ['median_width', 'median_length']
engineered_features_mean = ['dAC_mean','dB_mean','aABC_mean']
engineered_features_median = ['dAC_median','dB_median','aABC_median']
engineered_features_stdev = ['dAC_stdev','dB_stdev','aABC_stdev']
engineered_features_min = ['dAC_min','dB_min','aABC_min']
engineered_features_max = ['dAC_max','dB_max','aABC_max']


masterdfgaitfix = pd.read_csv('Data/New/masterdfgaitfix.csv')
fixedflex = pd.read_csv('Data/New/fixedflex.csv')
ellipsefit = pd.read_csv('Data/New/ellipsefit_all.csv')
fixedflex['NetworkFilename'] = ellipsefit.NetworkFilename
rearpaw = pd.read_csv('Data/New/rearpaw.csv')
rear4all = pd.read_csv('Data/New/rear4all.csv')
rear4bins = pd.read_csv('Data/New/rear4bins.csv')

data_frames = [masterdfgaitfix,fixedflex,ellipsefit,rearpaw,rear4all,rear4bins]
df_old = reduce(lambda  left,right: pd.merge(left,right,on=['NetworkFilename'],how='outer'), data_frames)
df_old = df_old.rename(columns = {'MouseID_x':'MouseID'})

masterdf = pd.read_csv('Data/New/masterdf.csv')
rearpaw_new = pd.read_csv('Data/New/rearpaw_new.csv')
df_new = pd.merge(masterdf,rearpaw_new, on='NetworkFilename')

df0 = pd.concat([df_old,df_new], join = 'inner')
df0 = df0.rename(columns = {'Overall Score':'score', 'Age at Test':'TestAge',
        'Body Weight':'Weight','Distance cm/sc':'Distance','Collected By':'Tester', 'dAC_standard deviation':'dAC_stdev',
        'dB_standard_deviation':'dB_stdev','aABC_standard_deviation':'aABC_stdev'})
df0['Tester'] = df0['Tester'].astype('category')

df1 = df0[['score','MouseID','TestAge','Tester','Sex','Weight'] + avg_gait_measures_linear + median_gait_measures_linear + std_gait_measures_linear + 
iqr_gait_measures_linear + OFA_measures + engineered_features_median + ellipsefit_measures + rearpaw_pose_measures + rears_measures]
df1 = df1.dropna()

#Adjust for Tester random effect 
model = sm.MixedLM.from_formula('score ~ 1', data = df1, groups = df1['Tester'])
result = model.fit()
#print(result.summary())
#df1['score'] = result.predict()  
df1.loc[df1['Tester'] == 'Amanda','score'] = df1.loc[df1['Tester'] == 'Amanda','score'] - result.random_effects['Amanda']['Group'] 
df1.loc[df1['Tester'] == 'Gaven','score'] = df1.loc[df1['Tester'] == 'Gaven','score'] - result.random_effects['Gaven']['Group']
df1.loc[df1['Tester'] == 'Hannah','score'] = df1.loc[df1['Tester'] == 'Hannah','score'] - result.random_effects['Hannah']['Group']
df1.loc[df1['Tester'] == 'Mackenzie','score'] = df1.loc[df1['Tester'] == 'Mackenzie','score'] - result.random_effects['Mackenzie']['Group']

df = df1[['score','MouseID','Weight','Sex'] + avg_gait_measures_linear + median_gait_measures_linear + std_gait_measures_linear + 
iqr_gait_measures_linear + OFA_measures + engineered_features_median + ellipsefit_measures + rearpaw_pose_measures + rears_measures]

df = df1[['score','MouseID','Weight','Sex'] + median_gait_measures_linear + engineered_features_median + ellipsefit_measures + 
rears_measures + rearpaw_pose_measures + OFA_measures]

df = df.rename(columns = {'median_angular_velocity':'Angular Velocity', 'median_base_tail_lateral_displacement': 'Base Tail LD',
        'median_limb_duty_factor':'Limb Duty Factor','median_nose_lateral_displacement':'Nose LD',
        'median_speed_cm_per_sec':'Speed','median_step_length1':'Step Length1','median_step_length2':'Step Length2',
        'median_step_width':'Step Width','median_stride_length':'Stride Length','median_tip_tail_lateral_displacement':'Tip Tail LD',
        'dAC_median':'dAC', 'dB_median':'dB', 'aABC_median':'aABC', 'median_width':'Width',
       'median_length':'Length', 'rear_count':'Rear Count', 
       'median_rearpaw':'Rearpaw', 'stride_count':'Stride Count','center_time_secs':'Center Time (secs)',
       'periphery_time_secs':'Periphery Time (secs)', 'corner_time_secs':'Corner Time (secs)', 
       'center_distance_cm':'Center Distance (cm)',
       'periphery_distance_cm':'Periphery Distance (cm)', 'corner_distance_cm':'Corner Distance (cm)', 
       'grooming_number_bouts':'Grooming Bouts',
       'grooming_duration_secs':'Grooming Duration (secs)'})

df.Sex = df.Sex.replace(['Male','Female'],[-1,1])
df.Sex = df.Sex.astype('category')


#numeric_feats = df.dtypes[df.dtypes != "object"].index
#numeric_feats = np.delete(numeric_feats,0)

# Check the skew of all numerical features
#skewed_features = df[numeric_feats].apply(lambda x: skew(x.dropna())).sort_values(ascending=False)
#skewness = pd.DataFrame({'Skew' :skewed_features})
#skewness.head(10)

#skewness = skewness[abs(skewness) > 0.75]
#print("There are {} skewed numerical features to Box Cox transform".format(skewness.shape[0]))
#skewed_features = skewness.index
#lam = 0.15
#for feat in skewed_features:
#    df[feat] = boxcox1p(df[feat], lam)

#df_repeat = df[df['MouseID'].isin(df.MouseID.value_counts()[df.MouseID.value_counts() > 1].index)]
#df_nonrepeat = df[df['MouseID'].isin(df.MouseID.value_counts()[df.MouseID.value_counts() == 1]. index)]
#df_repeat = df_repeat.drop('MouseID',1)
#df_nonrepeat = df_nonrepeat.drop('MouseID',1)


MouseID = df.MouseID.unique()
np.random.shuffle(MouseID)
trainIDs, testIDs = MouseID[:np.round(.80*len(MouseID)).astype('int')], MouseID[np.round(.80*len(MouseID)).astype('int'):]

df_train, df_test = df[df['MouseID'].isin(trainIDs)], df[df['MouseID'].isin(testIDs)]
X_train, y_train, X_test, y_test =  df_train.drop(['score','MouseID'],axis = 1), df_train['score'], df_test.drop(['score','MouseID'],axis = 1), df_test['score']
X,y = df.drop(['score','MouseID'], axis=1), df['score']

#X_r,y_r = df_repeat.drop(['score'], axis = 1), df_repeat['score']
#X_nr,y_nr = df_nonrepeat.drop(['score'], axis = 1), df_nonrepeat['score']

#X_train, X_test, y_train, y_test = train_test_split(X_nr,y_nr, test_size = 0.2, shuffle=True, random_state = 123)
#X_train, y_train = X_train.append(X_r), y_train.append(y_r)


CV = RepeatedKFold(n_splits = 10, n_repeats = 2, random_state=1234)

#LinearModel with Elastic Net
#Tuning Elastic Net:Grid Search
pipe = make_pipeline(RobustScaler(), ElasticNet(max_iter = 10000))
enet_grid = [{'elasticnet__alpha':np.linspace(0.01,0.05,20), 'elasticnet__l1_ratio':np.linspace(0.01,1,10)}]
enet_search = GridSearchCV(pipe, param_grid = enet_grid, cv = CV, 
        scoring = 'neg_mean_absolute_error', return_train_score = True, n_jobs= -1)
enet_search.fit(X_train, y_train)
best_params = enet_search.best_params_

#Train with best tuning parameters to obtain train errors and test error 
pipe = make_pipeline(RobustScaler(), ElasticNet(alpha = enet_search.best_params_['elasticnet__alpha'], 
        l1_ratio = enet_search.best_params_['elasticnet__l1_ratio']))
enet = cross_val_score(pipe, X_train, y_train, cv = CV, scoring = 'neg_mean_absolute_error', n_jobs= -1)
pipe.fit(X_train, y_train)
enet_train = np.abs(enet.mean())
enet_test = mean_absolute_error(y_test, pipe.predict(X_test))

np.sqrt(mean_squared_error(pipe.predict(X_train),y_train))
np.sqrt(mean_squared_error(pipe.predict(X_test),y_test))

(mean_absolute_error(pipe.predict(X_train),y_train))
(mean_absolute_error(pipe.predict(X_test),y_test))

#Support Vector Machine
#Tuning SVM:Grid Search
pipe = make_pipeline(RobustScaler(), SVR())
svm_grid = [{'svr__kernel':['linear','poly','rbf'], 'svr__degree':[1,2,3], 'svr__C':np.linspace(0.01,0.1,10)}]
svm_search = GridSearchCV(pipe, param_grid = svm_grid, cv = CV, scoring= 'neg_mean_absolute_error', 
        return_train_score=True, n_jobs=-1)
svm_search.fit(X_train,y_train)
best_params = svm_search.best_params_

#Train with best tuning parameters to obtain train error and test error 
pipe = make_pipeline(RobustScaler(), SVR(kernel = best_params['svr__kernel'], C = best_params['svr__C']))
svm = cross_val_score(pipe, X_train,y_train, cv=CV, scoring='neg_mean_absolute_error',n_jobs=-1)
pipe.fit(X_train,y_train)
svm_train = np.abs(svm.mean())
svm_test = mean_absolute_error(y_test,pipe.predict(X_test))

np.sqrt(mean_squared_error(pipe.predict(X_train),y_train))
np.sqrt(mean_squared_error(pipe.predict(X_test),y_test))

(mean_absolute_error(pipe.predict(X_train),y_train))
(mean_absolute_error(pipe.predict(X_test),y_test))

#Random Forest
#Tuning SVM:Grid Search
#pipe = make_pipeline(RobustScaler(), RandomForestRegressor())
#rf_grid = {'randomforestregressor__n_estimators': np.linspace(100,1000,10).astype('int32'),
#'randomforestregressor__max_features': ['auto','sqrt','log2'],
#'randomforestregressor__max_depth': [int(x) for x in np.linspace(10,110,10)],
#'randomforestregressor__min_samples_split': [2,5,7,10],
#'randomforestregressor__min_samples_leaf': [1,2,4,6]}
#rf_search = GridSearchCV(pipe, param_grid=rf_grid, cv=CV, scoring='neg_mean_absolute_error',
#        return_train_score=True, n_jobs=-1, refit=True)
#rf_search.fit(X_train,y_train)
best_params = {'randomforestregressor__max_depth': 87, 'randomforestregressor__max_features': 'auto', 'randomforestregressor__min_samples_leaf': 4, 
'randomforestregressor__min_samples_split': 5, 'randomforestregressor__n_estimators': 100}
best_params = {'randomforestregressor__max_depth': 65, 'randomforestregressor__max_features': 'auto', 'randomforestregressor__min_samples_leaf': 4, 
'randomforestregressor__min_samples_split': 2, 'randomforestregressor__n_estimators': 100}
#This set of parameters is obtained by running the main grid search code on Winter cluster. 

#Train with best tuning paramers to obtain train and test error 
pipe = make_pipeline(RobustScaler(), RandomForestRegressor(n_estimators = 
        best_params['randomforestregressor__n_estimators'],max_features = 
        best_params['randomforestregressor__max_features'], max_depth = 
        best_params['randomforestregressor__max_depth'], min_samples_split = 
        best_params['randomforestregressor__min_samples_split'], min_samples_leaf = 
        best_params['randomforestregressor__min_samples_leaf']))
rf = cross_val_score(pipe, X_train,y_train, cv = CV, scoring='neg_mean_absolute_error',n_jobs=-1)
pipe.fit(X_train,y_train)
rf_train = np.abs(rf.mean())
rf_test = mean_absolute_error(y_test, pipe.predict(X_test))

np.sqrt(mean_squared_error(pipe.predict(X_train),y_train))
np.sqrt(mean_squared_error(pipe.predict(X_test),y_test))

(mean_absolute_error(pipe.predict(X_train),y_train))
(mean_absolute_error(pipe.predict(X_test),y_test))


#Interpretable ML 

rf = RandomForestRegressor(n_estimators=100,min_samples_split=5,min_samples_leaf=4,max_features="auto",
        max_depth=65)
rf.fit(X_train, y_train)

explainer = shap.TreeExplainer(rf)
shap_values = explainer.shap_values(X)

f = plt.figure()
shap.summary_plot(shap_values, X, plot_type="bar")
f.savefig("varImp_med.png", bbox_inches='tight', dpi=600)

plt.close('all')
f = plt.figure()
shap.summary_plot(shap_values, X)
f.savefig("varImp2_med.png", bbox_inches='tight', dpi=300)


shap.dependence_plot("Width", shap_values, X)
plt.savefig("width.png", bbox_inches='tight', dpi=600)

shap.dependence_plot("Stride Length", shap_values, X)
plt.savefig("stridelength.png", bbox_inches='tight', dpi=600)

shap.dependence_plot("Rearpaw", shap_values, X)
plt.savefig("rearpaw.png", bbox_inches='tight', dpi=600)

shap.dependence_plot("Weight", shap_values, X)
plt.savefig("weight.png", bbox_inches='tight', dpi=600)

plt.close('all')
shap.dependence_plot("dB", shap_values, X)
plt.savefig("dB.png", bbox_inches='tight', dpi=600)

plt.close('all')
shap.dependence_plot("median_stride_length", shap_values, X_train)
plt.savefig("sl-dep.pdf", bbox_inches='tight', dpi=600)

plt.close('all')
shap.force_plot(explainer.expected_value, shap_values[0,:], X_test.iloc[0,:], show=False, 
        matplotlib=True)
plt.savefig('tmp.pdf')
shap.force_plot(explainer.expected_value, shap_values, X_train)

shap.save_html('explainer.html', shap.force_plot(explainer.expected_value, shap_values, X_train))

#XGBoost
best_params = {'XGBRegressor__alpha': 0.8, 'XGBRegressor__colsample_bytree': 0.7, 'XGBRegressor__eta': 0.05, 
'XGBRegressor__max_depth': 5, 'XGBRegressor__objective': 'reg:squarederror', 'XGBRegressor__subsample': 0.5}
#This set of parameters is obtained by running the main grid search code on Winter cluster. 

pipe = make_pipeline(RobustScaler(), XGBRegressor(alpha = best_params['XGBRegressor__alpha'],
        colsample_bytree = best_params['XGBRegressor__colsample_bytree'], 
        eta = best_params['XGBRegressor__eta'],max_depth = best_params['XGBRegressor__max_depth'],
        objective = best_params['XGBRegressor__objective'], subsample = best_params['XGBRegressor__subsample']))
xgb = cross_val_score(pipe, X_train,y_train, cv = CV, scoring='neg_mean_absolute_error',n_jobs=-1)

pipe.fit(X_train,y_train)
xgb_train = np.abs(xgb.mean())
xgb_test = mean_absolute_error(y_test, pipe.predict(X_test))

np.sqrt(mean_squared_error(pipe.predict(X_train),y_train))
np.sqrt(mean_squared_error(pipe.predict(X_test),y_test))

(mean_absolute_error(pipe.predict(X_train),y_train))
(mean_absolute_error(pipe.predict(X_test),y_test))

{'alpha': 0.8, 'colsample_bytree': 0.9, 'eta': 0.05, 'max_depth': 4, 'objective': 'reg:squarederror', 'subsample': 0.7}



####Coming back to RF 



