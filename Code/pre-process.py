import pandas as pd 
import statsmodels.api as sm

aging = pd.read_csv(snakemake.input[0])
ellipsefit = pd.read_csv(snakemake.input[1])
rearpaw = pd.read_csv(snakemake.input[2])
var_agingall = pd.read_csv(snakemake.input[3])

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

df.to_csv(snakemake.output[0], index = False)


master = master.rename(columns = {'Overall Score':'score','Age at Test':'TestAge','Body Weight':'Weight','Distance cm/sc':'Distance',
	'Collected By':'Tester'})
master['Tester'] = master['Tester'].astype('category')
master = master[['score'] + avg_gait_measures_linear + median_gait_measures_linear + std_gait_measures_linear + 
iqr_gait_measures_linear + OFA_measures + engineered_features_mean + engineered_features_stdev + ['median_rearpaw',  
'median_width', 'median_length']]