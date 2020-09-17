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

data_ind_models_full <- join_forecast_data(path=path_results, regex='csv')
data_ind_models_full <- data_ind_models_full %>%
    dplyr::filter(date_int >= start_forecast)  
data_ind_models <- data_ind_models_full %>%
    dplyr::select(date_int, model_name, vol) %>%
    tidyr::pivot_wider(names_from=model_name, values_from=vol) %>%  
    dplyr::select(-date_int)
data_ind_models <- apply(X=data_ind_models, FUN=replace_negative_predictions, MARGIN=2)  
rv_ind_oos <- realised_measures$rv_5[start_forecast:NROW(realised_measures)]
rv_cv_oos <- realised_measures$rv_5[(start_forecast+start_comb_forecast_cv):NROW(realised_measures)]
rv_all_oos <- realised_measures$rv_5[(
    start_forecast+start_comb_forecast_cv+start_comb_forecast):NROW(realised_measures)]

strategy_equalweight <- list(equal='')
trim_values <- (0:20) * 0.10 + 1
group_sizes <- 1:7
alpha_params <- seq(from=0, to=1, length.out=21)
lambda_params <- exp(seq(from=-12, -28, length.out=81))  # Has to be in descending order
# Group names
unique_model_names <- function (data) {
    return(data %>% dplyr::select('model_name') %>% unlist %>% unique)
}
model_name_groups <- list(
    garch = data_ind_models_full %>%
        dplyr::filter(family == 'garch') %>%
        unique_model_names(),
    har = data_ind_models_full %>%
        dplyr::filter(family == 'har') %>%
        unique_model_names(),
    `naive-rv5` = data_ind_models_full %>%
        dplyr::filter(family == 'naive-rv5') %>%
        unique_model_names(),
    `naive-squared_ret` = data_ind_models_full %>%
        dplyr::filter(family == 'naive-squared_ret') %>%
        unique_model_names(),
    `arima-rv` = data_ind_models_full %>%
        dplyr::filter(family == 'arima-rv') %>%
        unique_model_names(),
    `arima-squared_ret` = data_ind_models_full %>%
        dplyr::filter(family=='arima-squared_ret') %>%
        unique_model_names(),
    `naive-iv` = data_ind_models_full %>%
        dplyr::filter(family == 'naive-iv') %>%
        unique_model_names()
)
model_name_groups$`naive-iv`
set.seed(seed=1)
do_par_options <- list(export = ls(rlang::global_env()),
                       packages = parallel_packages,
                       max_cores = max_cores)

# TEST MODEL COMBINATIONS -----------------------------------------------------------------
# Equal weight
weights_matrix <- get_weights_matrix(strategy_list=strategy_equalweight,
                                     start_forecast=start_comb_forecast_cv,
                                     weights_matrix=NULL, data_x=data_ind_models,
                                     data_y=rv_ind_oos, save_matrix=TRUE,
                                     path_save_folder=path_comb_weights_matrix)
forecast_naive_combinations(data=data_ind_models, weights=weights_matrix,
                            return_results=FALSE, save_results=TRUE,
                            path_folder_save=path_comb_results,
                            strategy_list=strategy_equalweight,
                            pos_start=start_comb_forecast_cv)
# Trimming
for (k in trim_values) {
    # has 1 observation too many
    weights_matrix_mse <- get_weights_matrix(
        strategy_list=list(trim=k), start_forecast=start_comb_forecast_cv,
        weights_matrix=NULL, data_x=data_ind_models,
        data_y=rv_ind_oos, save_matrix=TRUE,
        path_save_folder=path_comb_weights_matrix,
        loss_function='mse')
    forecast_naive_combinations(data=data_ind_models, weights=weights_matrix_mse,
                                return_results=FALSE, save_results=TRUE,
                                path_folder_save=path_comb_results,
                                strategy_list=list(trim=k), loss_function='mse',
                                pos_start=start_comb_forecast_cv)
    weights_matrix_qlike <- get_weights_matrix(
        strategy_list=list(trim=k), start_forecast=start_comb_forecast_cv,
        weights_matrix=NULL, data_x=data_ind_models,
        data_y=rv_ind_oos, save_matrix=TRUE,
        path_save_folder=path_comb_weights_matrix,
        loss_function='qlike2')
    forecast_naive_combinations(data=data_ind_models, weights=weights_matrix_qlike,
                                return_results=FALSE, save_results=TRUE,
                                path_folder_save=path_comb_results,
                                strategy_list=list(trim=k), loss_function='qlike2',
                                pos_start=start_comb_forecast_cv)
}

# Grouping
for (size in group_sizes) {
    groups_of_size <- utils::combn(x=names(model_name_groups), m=size)
    for (col in 1:NCOL(groups_of_size)) {
        groups_to_use <- names(model_name_groups) %in% unlist(groups_of_size[ , col])
        groups_to_use <- model_name_groups[groups_to_use]
        weights_matrix <- get_weights_matrix(strategy_list=list(group=groups_to_use),
                                             start_forecast=start_comb_forecast_cv,
                                             weights_matrix=NULL, data_x=data_ind_models,
                                             data_y=rv_ind_oos, save_matrix=TRUE,
                                             path_save_folder=path_comb_weights_matrix)
        forecast_naive_combinations(data=data_ind_models, weights=weights_matrix,
                                    return_results=FALSE, save_results=TRUE,
                                    path_folder_save=path_comb_results,
                                    strategy_list=list(group=groups_to_use),
                                    pos_start=start_comb_forecast_cv)
    }
}

# Regularization -------------------------------------------------------------------
regularization_args <- list(
    data_y=list(as.matrix(rv_ind_oos)), data_x=list(as.matrix(data_ind_models)),
    family='gaussian', alpha=alpha_params, lambda_values=list(lambda_params),
    standardize_x=FALSE, standardize_y=FALSE)
regularization_args <- get_parameter_combinations(regularization_args)
roll_forecast(model_func=return_glmnet_forecasts,
              model_func_args=regularization_args,
              pos_start_forecast=start_comb_forecast_cv+1,  # this +1 is the result of a few hours debugging
              n_forecasts=n_forecasts_cv_comb, do_par=FALSE,
              do_par_options=do_par_options,
              path_folder_save=path_comb_reg_predictions,
              model_family='glmnet')
data_reg_predictions <- join_reg_forecasts(path=path_comb_reg_predictions,
                                           col_lambda_start=1, n_lambda=NROW(lambda_params))
all_regularization_forecasts(reg_predictions=data_reg_predictions, data_y=rv_cv_oos,
                             func_loss=.error_squared, func_loss_name='mse',
                             pos_start_oos=start_comb_forecast+1, save_errors=TRUE,
                             path_folder_errors=path_comb_reg_all, save_best_forecast=TRUE,
                             path_folder_best=path_comb_results)
all_regularization_forecasts(reg_predictions=data_reg_predictions, data_y=rv_cv_oos,
                             func_loss=.error_qlike2, func_loss_name='qlike2',
                             pos_start_oos=start_comb_forecast+1, save_errors=TRUE,
                             path_folder_errors=path_comb_reg_all, save_best_forecast=TRUE,
                             path_folder_best=path_comb_results)

