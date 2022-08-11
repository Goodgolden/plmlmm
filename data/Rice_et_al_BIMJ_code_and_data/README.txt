README for "Application of gap time analysis with flexible hazards to pulmonary exacerbations in the EPIC Observational Study" by Rice et al.

FILE STRUCTURE

* Code_master.R: run this to reproduce all analyses reported in the manuscript: this sources the following three files:
	- FirstEvent_sims.R: code to run the simulations of a single event.
	- RecurrentEvent_sims.R: code to run the simulations for recurrent events. Also includes option to instead pull in
		+ frailty_recurrent_results_N200.RData and frailty_recurrent_results_N1000.RData: saved simulation data for recurrent events simulations
	- EPIC_data_analysis.R: code to run the EPIC data analysis, which reads in the data file
		+ EPIC_Obs_Study_Data.csv: fully de-identified data used in our analyses (not for public dissemination, only to be used for the audit).
	- supplement_covariate_effects.R: code sourced by EPIC_data_analysis.R to create Supplement Figures 3-7 
* smcp_functions.R: function definitions.


VERSION INFORMATION

> sessionInfo()R version 4.0.1 (2020-06-06)Platform: x86_64-apple-darwin17.0 (64-bit)Running under: OS X  12.2.1Matrix products: defaultLAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dyliblocale:[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8attached base packages:[1] stats     graphics  grDevices utils     datasets  methods   base     other attached packages:[1] plyr_1.8.6          xtable_1.8-4        knitr_1.30          sandwich_2.5-1      survival_3.2-3     [6] mvtnorm_1.1-1       numDeriv_2016.8-1.1 table1_1.4.2       loaded via a namespace (and not attached): [1] Rcpp_1.0.5      lattice_0.20-41 zoo_1.8-8       grid_4.0.1      Matrix_1.2-18   Formula_1.2-3   [7] splines_4.0.1   tools_4.0.1     xfun_0.18       compiler_4.0.1 