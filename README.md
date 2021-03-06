# Code for "Forecast comparison of volatility models and their combinations for the FTSE 100: a tied race"

This is the repository for my Master's thesis in Finance from the University of Minho.
I cannot share the data because it is proprietary, so I include all the data sources and specify 
how they are structured below. 

You can read the thesis [here](http://repositorium.sdum.uminho.pt/bitstream/1822/69725/1/Dissertacao%20David%20Mendonca%20Pinho.pdf), with additional information [here](http://repositorium.sdum.uminho.pt/handle/1822/69725).

Grade: 20/20.

## Abstract

> I first compare 74 models that include the main naïve, ARMA, GARCH, and HAR models from the volatility forecasting literature to assess their out-of-sample performance for day-ahead forecasts. For the FTSE100 index in the period of  2005-2010, all HAR and GARCH models and some ARMA and exponential smoothing models perform similarly to each other. I then test 176 model combinations (meaning that 250 models are compared in total) in the period of 2007-2010, and observe that the average performance has less variance and is always slightly improved. This tendency is not observed with complex weighting schemes, which are based on regularized regression (i.e., Lasso, Ridge and Elastic Net); and this tendency is marginally larger when excluding underperforming models and equal weighting the forecasts of the remaining models (i.e., trimming). But, overall, as observed in the relevant literature, all reasonably adequate models tend to have identical performance, so past research seems to have overstated the improvements generated by new models. An additional problem is that, according to the literature, even large performance gains with the loss functions used seem to rarely translate into improvements in economic applications, such as risk management and portfolio optimization. Because of this, I argue thatI first compare 74 models that include the main naïve, ARMA, GARCH, and HAR models from the volatility forecasting literature to assess their out-of-sample performance for day-ahead forecasts. For the FTSE100 index in the period of 2005-2010, all HAR and GARCH models and some ARMA and exponential smoothing models perform similarly to each other. I then test 176 model combinations (meaning that 250 models are compared in total) in the period of 2007-2010, and observe that the average performance has less variance and is always slightly improved. This tendency is not observed with complex weighting schemes, which are based on regularized regression (i.e., Lasso, Ridge and Elastic Net); and this tendency is marginally larger when excluding underperforming models and equal weighting the forecasts of the remaining models (i.e., trimming). But, overall, as observed in the relevant literature, all reasonably adequate models tend to have identical performance, so past research seems to have overstated the improvements generated by new models. An additional problem is that, according to the literature, even large performance gains with the loss functions used seem to rarely translate into improvements in economic applications, such as risk management and portfolio optimization. Because of this, I argue that subsequent research must use metrics directly related to these applications.

## How to run the code 

1. Install R version 3.6.3 (other versions should work fine, but no guarantees).
2. Install the packages in requirements.R. 
3. Run the scripts in the *R_exec* folder in the order of the file names -- first run
   *0_clean_raw_data.R*, then *1_create_volatility_measures.R*, and so on.  You can choose
   to skip some of these (for example, skip the first script if you already have
   the raw data cleaned). The only requirement is that your working folder is the
   repository folder (the one where this README file is in), whether you are running things
   in an IDE or through the terminal (with Rscript).
4. To see how the data was cleaned and generate the graphs/tables from the article,
   run the code in the */R_notebooks/* folder. 
   If you do not have the data, you can see the output of that code in the 
   */R_notebooks/html_output/* folder.

## Repository structure 

(The data folder is not included here for the reasons stated above. If you have the data, you
only need to create the */raw_data/* folder manually. Everything else is created automatically 
by the code.)  

```
.
├── data
│   └── ftse100
│       ├── clean_data
│       │   ├── all_data_cleaned.csv
│       │   └── vtse_cleaned.csv
│       ├── comb_reg_all
│       ├── comb_reg_predictions
│       ├── comb_results
│       ├── comb_weights_matrix
│       ├── ind_results
│       ├── ind_results_fit
│       ├── mcs
│       ├── measures
│       │   ├── realised_measures.csv
│       │   └── returns.csv
│       └── raw_data
│           ├── equity-index_FT100_2000.txt
│           ├── equity-index_FT100_2001.txt
│           ├── equity-index_FT100_2002.txt
│           ├── equity-index_FT100_2003.txt
│           ├── equity-index_FT100_2004.txt
│           ├── equity-index_FT100_2005.txt
│           ├── equity-index_FT100_2006.txt
│           ├── equity-index_FT100_2007.txt
│           ├── equity-index_FT100_2008.txt
│           ├── equity-index_FT100_2009.txt
│           ├── equity-index_FT100_2010.txt
│           ├── bst.csv
│           ├── oxfordmanrealizedvolatilityindices2.csv
│           ├── OxfordManRealizedVolatilityIndices.csv
│           ├── price_ftse100_datastream.csv
│           ├── ukbankholidays.xls
│           └── vftse.csv
├── README.md
├── requirements.R
├── R_exec
│   └── ftse100
│       ├── 0_clean_raw_data.R
│       ├── 1_create_volatility_measures.R
│       ├── 2_run_individual_models.R
│       ├── 3_run_model_combinations.R
│       ├── global_variables.R
│       └── import_volatility_measures.R
├── R_notebooks
│   ├── empirical_analysis
│   │   └── graphs_and_appendix.rmd
│   ├── html_output
│   │   ├── graphs_and_appendix.html
│   │   ├── methodology_diagram.html
│   │   └── sanity_checks.html
│   └── methodology
│       ├── data_cleaning_analysis.Rmd
│       ├── methodology_diagram.Rmd
│       └── sanity_checks.rmd
└── R_scripts
    ├── 0_har_model.R
    ├── 0_helper_functions.R
    ├── 1_generate_measures.R
    ├── 2_arma_models.R
    ├── 2_garch_models.R
    ├── 2_har_models.R
    ├── 2_naive_models.R
    ├── 3_averaging_comb.R
    ├── 3_regularization_comb.R
    └── 4_mcs_test.R
```

## Notes 

- A lot of my code is meant to simplify the analysis so that the data was formatted in a
  standardized way -- it cleans the raw data, formats it to be used with different APIs that 
  packages have, and then outputs it with labels. 
- The topic of my thesis gradually reduced in scope, so there is some code complexity that
  looks unnecessary, but it was not at the time I built it. This also happened 
  because some packages made to simplify the workflow (e.g., 
  [caret](http://caret.r-forge.r-project.org/) and [tidymodels](https://www.tidymodels.org/)) 
  or to run models (such as the Heterogeneous AutoRegressive model, HAR)
  were either incomplete or had a lot of specific functionality missing for my case. 
  All of this being said...
- ...I do not advise you to re-use the code related to model combinations. I did it in a way that
  creates some nasty off-by-one bugs that interact among themselves. Lesson learned: do not get 
  cute with indexing. 
- Lastly, one of the main conclusions of my study is that most models are very similar. This is 
  also often true in any other area where the literature is filled with very incremental model 
  iterations. Given that, I recommend that you compare models in light of some utility function
  that needs to be maximized (or something analogous to that).  

## Data structure for the raw data

### equity-index_FT100_20XX.txt

| Column description (no header) 	| Data type               	|
|-------------------------------- 	|-------------------------	|
| Date    	                        | Date (format: %d.%m.%Y) 	|
| Time                           	| Time (format: %H:%M:%S) 	|
| Price (FTSE100)                   | Numeric                	|

Source: OlsenData.

Notes: All 11 files (2000-2010) have the data in this format. 

### bst.csv

| Column description (no header) 	| Data type               	|
|-------------------------------- 	|-------------------------	|
| Start date of BST for the year  	| Date (format: %d-%m-%Y) 	|
| Ending date of BST for the year 	| Date (format: %d-%m-%Y) 	|

Source: https://greenwichmeantime.com/uk/time/british-summer-time/dates/  

Notes: Copy-pasted everything to an excel file from 2000 to 2010 and saved as a csv file.

### oxfordmanrealizedvolatilityindices2.csv

Source: https://realized.oxford-man.ox.ac.uk/images/oxfordmanrealizedvolatilityindices.zip

Notes: The data is publicly available. I only changed the date format to `%d-%m-%Y`. 

### OxfordManRealizedVolatilityIndices.csv

Source: https://realized.oxford-man.ox.ac.uk/images/oxfordmanrealizedvolatilityindices-0.2-final.zip

Notes: The data is unaltered and publicly available. 

### price_ftse100_datastream.csv

| Column name (header) 	| Data type               	|
|----------------------	|-------------------------	|
| date                 	| Date (format: %d-%m-%Y) 	|
| ftse_100_price_index  | Character               	|

Source: Thomson Reuters' Datastream. 

Notes: The data extracted includes dividends, is adjusted for splits, and only
includes the index's trading days. Note that the `ftse_100_price_index` column is the daily *price*, not 
the returns. It also uses commas to separate the decimals, which is why it initially gets converted to a character ("string") 
instead of numeric type. 

### ukbankholidays.csv

Source: https://www.dmo.gov.uk/media/15008/ukbankholidays.xls

Notes: Converted from xls to csv. 

### vftse.csv

| Column name (header) 	| Data type               	|
|----------------------	|-------------------------	|
| date                 	| Date (format: %d-%m-%Y) 	|
| vftse                	| Numeric                 	|

Source: Thomson Reuters' Datastream.

Notes: I removed all the other output that comes from Datastream data. Daily data.
