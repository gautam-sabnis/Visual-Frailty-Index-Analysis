plots_exp = ["CFIvsCFIadj","CFIvsCFIadj-box","CFIvsX","kD-figure","kD-figure2"]
rule all:
	input:expand("Plots/{plot}.pdf", plot = plots_exp)

rule pre_process:
	input:['Data/completeagedb6.csv','Data/ellipsefit_all.csv','Data/rearpaw.csv','Data/var_agingall.csv']
	output: 'Output/df.csv'
	script: 'Code/pre-process.py'

rule exploratory_analysis:
	input:["Data/completeagedb6.csv",'Data/ellipsefit_all.csv','Data/rearpaw.csv','Data/var_agingall.csv']
	output:expand("Plots/{plot}.pdf", plot = plots_exp) 
	script: 'Code/exploratory.R'
	
rule model:
	input:['Output/df.csv']
	output: 'Output/result.csv'
	script: 'Code/modeling.py'

rule post_process:
	input:["Data/nested_cv_results2.csv"]
	output: "Plots/nestedcv.pdf"
	script: "Code/post-process.R"

rule clean:
	shell: 'rm Output/*.csv Plots/*.pdf'
