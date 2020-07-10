import os 
import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt

from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor

import shap

os.chdir('/projects/kumar-lab/sabnig')

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
#print(result.summary())
data['score'] = result.predict()  

df = data[['score'] + avg_gait_measures_linear + median_gait_measures_linear + std_gait_measures_linear + 
iqr_gait_measures_linear + OFA_measures + engineered_features_mean + engineered_features_stdev + ['median_rearpaw',  
'median_width', 'median_length']]

dfX, dfY = df.drop(['score'], axis = 1), df['score']

#Data Splitting 
X_train, X_test, y_train, y_test = train_test_split(dfX,dfY, test_size = 0.2, shuffle = True, random_state = 123)

rf = RandomForestRegressor(n_estimators=400,min_samples_split=2,min_samples_leaf=1,max_features="auto",
	max_depth=65)
rf.fit(X_train, y_train)

explainer = shap.TreeExplainer(rf)
shap_values = explainer.shap_values(X_train)

f = plt.figure()
shap.summary_plot(shap_values, X_train, plot_type="bar")
f.savefig("varImp.pdf", bbox_inches='tight', dpi=600)

plt.close('all')
f = plt.figure()
shap.summary_plot(shap_values, X_train)
f.savefig("varImp2.pdf", bbox_inches='tight', dpi=600)

plt.close('all')
shap.dependence_plot("median_step_width", shap_values, X_train)
plt.savefig("sw-dep.pdf", bbox_inches='tight', dpi=600)

plt.close('all')
shap.dependence_plot("avg_stride_length", shap_values, X_train)
plt.savefig("sl-dep.pdf", bbox_inches='tight', dpi=600)

plt.close('all')
shap.force_plot(explainer.expected_value, shap_values[0,:], X_test.iloc[0,:], show=False, 
	matplotlib=True)
plt.savefig('tmp.pdf')
shap.force_plot(explainer.expected_value, shap_values, X_train)

shap.save_html('explainer.html', shap.force_plot(explainer.expected_value, shap_values, X_train))

#Variable Importance
tmp = pd.DataFrame({'feature':X_train.columns, 'importance':rf.feature_importances_})
tmp = tmp.sort_values('importance', ascending=False)

#Get the feature importances from each tree in the forest 
feature_imp = pd.DataFrame(data=[tree.feature_importances_ for tree in rf], columns=X_train.columns)



fig, axes, summary_df = info_plots.target_plot(df=df, feature='median_step_width', 
	feature_name='median_step_width', target=y_train.name, show_percentile=True)

#fig, axes, summary_df = info_plots.actual_plot(model=rf, X=df[X_train.columns], feature='median_step_width', 
#	feature_name='median_step_width', show_percentile=True)

#Partial Dependence plot for step width
pdp_medianstepwidth = pdp.pdp_isolate(model=rf, dataset=df, model_features=list(X_train.columns), 
	feature='median_step_width')
fig,axes = pdp.pdp_plot(pdp_medianstepwidth, 'median_step_width')
fig,axes = pdp.pdp_plot(pdp_medianstepwidth, 'median_step_width', plot_lines=True, frac_to_plot=100, 
	x_quantile=True, plot_pts_dist=True, show_percentile=True)

#Partial Dependence plot for stride length
pdp_avgstridelength = pdp.pdp_isolate(model=rf, dataset=df, model_features=list(X_train.columns), 
	feature='avg_stride_length')
fig,axes = pdp.pdp_plot(pdp_avgstridelength, 'avg_stride_length')
fig,axes = pdp.pdp_plot(pdp_avgstridelength, 'avg_stride_length', plot_lines=True, frac_to_plot=100, 
	x_quantile=True, plot_pts_dist=True, show_percentile=True)

#Interaction between step width and stride length
fig, axes, summary_df = info_plots.target_plot_interact(df=df, features=['median_step_width','avg_stride_length'],
	feature_names=['median_step_width','avg_stride_length'], target=y_train.name)
fig, axes, summary_df = info_plots.actual_plot_interact(model=rf, X=df[X_train.columns],
	features=['median_step_width','avg_stride_length'],feature_names=['median_step_width','avg_stride_length'])

inter_rf = pdp.pdp_interact(model=rf, dataset=df, model_features=list(X_train.columns),
	features=['median_step_width','avg_stride_length'])
fig, axes = pdp.pdp_interact_plot(inter_rf, ['median_step_width','avg_stride_length'], x_quantile=True,
	plot_type='contour',plot_pdp=False)




depths= [tree for tree in rf.estimators_ if tree.max_depth==5]
tree = depths[0]
export_graphviz(tree, out_file = 'tree10.dot', feature_names = X_train.columns, 
	rounded = True, precision = 1)
(graph, ) = pydot.graph_from_dot_file('tree10.dot')
graph.write_png('tree10.png');


from sklearn.tree import export_graphviz
# Export as dot file
export_graphviz(rf.estimators_[0], out_file='tree.dot', 
                feature_names = X_train.columns,
                rounded = True, proportion = False, 
                precision = 2, filled = True)

# Convert to png using system command (requires Graphviz)
from subprocess import call
call(['dot', '-Tpng', 'tree.dot', '-o', 'tree.png', '-Gdpi=600'])

# Display in jupyter notebook
from IPython.display import Image
Image(filename = 'tree.png')

# grab the first one
tree = depths[0]
# plot the tree
dot_data = StringIO()
export_graphviz(tree, out_file=dot_data, feature_names=X_train.columns, 
                filled=True, rounded=True, special_characters=True)
graph = pydotplus.graph_from_dot_data(dot_data.getvalue())  
Image(graph.create_png())