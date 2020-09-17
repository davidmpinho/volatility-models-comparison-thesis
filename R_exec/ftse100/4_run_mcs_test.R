library(foreach)
library(dplyr)
# Source files with functions and variables ------------------------------------
path_global_variables <- here::here('/R_exec/ftse100/global_variables.R')
invisible(source(path_global_variables))
script_names <- list.files(path=path_r_scripts, pattern="*.R$", full.names=TRUE)
invisible(lapply(script_names, FUN=source))

# Import data -----------------------------------------------------------------
invisible(source(path_import_vars))
data.table::setDTthreads(threads=5)  # Change this if you are running on another PC

rv_all_oos <- realised_measures$rv_5[(
    start_forecast+start_comb_forecast_cv+start_comb_forecast):NROW(realised_measures)]
data_all_forecasts <- join_and_adjust_all_data(path_ind=path_results, path_comb=path_comb_results,
                                               sum_to_date_int=start_forecast, regex='csv')
data_all_forecasts <- data_all_forecasts %>%
    dplyr::filter(date_int >= (start_forecast+start_comb_forecast_cv+start_comb_forecast)) %>%
    dplyr::select(date_int, model_name, vol) %>%
    tidyr::pivot_wider(names_from=model_name, values_from=vol) %>%  
    dplyr::select(-date_int)
data_all_forecasts <- apply(X=data_all_forecasts, FUN=replace_negative_predictions,
                            MARGIN=2)  

# Parameters
do_par_options$max_cores <- 10
mcs_alpha <- 0.20
data_all_forecasts_mse <- apply(X=data_all_forecasts, MARGIN=2, 
                                FUN= function (x) .error_squared(data_x=x, data_y=rv_all_oos))
data_all_forecasts_qlike <- apply(X=data_all_forecasts, MARGIN=2, 
                                FUN= function (x) .error_qlike2(data_x=x, data_y=rv_all_oos))

# Group names
set.seed(seed=1)
do_par_options <- list(export = ls(rlang::global_env()),
                       packages = parallel_packages,
                       max_cores = max_cores)


# MCS test
mcs_test(data=data_all_forecasts_mse, alpha=mcs_alpha, B=10000, statistic='Tmax',
         save_mcs_data=TRUE, path_folder_save=path_mcs, parallel=TRUE,
         par_options=do_par_options)
mcs_test(data=data_all_forecasts_qlike, alpha=mcs_alpha, B=10000, statistic='Tmax',
         save_mcs_data=TRUE, path_folder_save=path_mcs, parallel=TRUE,
         par_options=do_par_options)



