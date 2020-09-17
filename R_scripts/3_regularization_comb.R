# If you need to run this script alone:
# import save_output, lag_columns from 0_helper_functions.R
# import dplyr

return_glmnet_forecasts <- function (data_y, data_x, alpha, lambda_values,
                                     family='gaussian', standardize_x=TRUE,
                                     standardize_y=TRUE) {
    # Calls glmnet::glmnet, but returns the results in a format that facilitates
    # the analysis.
    #
    list_to_df(list_of_vars=list(data_x=data_x, data_y=data_y))
    if (is.list(lambda_values)) {
        lambda_values <- unlist(lambda_values)
    }

    data_x <- remove_duplicated_columns(data=data_x, fromLast=FALSE)
    fit <- glmnet::glmnet(x=data_x[1:(NROW(data_x)-1), ],
                          y=data_y[1:NROW(data_y), ], alpha=alpha, lambda=lambda_values,
                          family=family, relax=FALSE, standardize=standardize_x,
                          standardize.response=standardize_y, maxit=10^8)
    vector_pred <- glmnet::predict.glmnet(fit, newx=data_x[NROW(data_x), , drop=FALSE])
    vector_pred <- as.data.frame(vector_pred)
    colnames(vector_pred) <- lambda_values
    vector_pred[ , 'alpha'] <- alpha
    vector_pred[ , 'date_int'] <- NROW(data_y)
    return(vector_pred)
}

all_regularization_forecasts <- function (reg_predictions, data_y, func_loss,
                                          func_loss_name='', pos_start_oos,
                                          save_errors=FALSE, path_folder_errors='./',
                                          save_best_forecast=TRUE, path_folder_best='./') {
    # After using grid search with glmnet, pass the values to `reg_predictions` and
    # this function will output the errors (formatted to make the analysis easier), and
    # choose the best parameters values on a one-step-ahead basis, according to
    # some loss function (i.e., at time t, it selects the parameters that performed
    # best up to t-1). The results are also
    # formatted to make the analysis easier (hence the function size).
    #
    predictions_and_errors <- reg_predictions %>%
        dplyr::group_by(alpha, lambda) %>%
        dplyr::mutate(data_y = data_y) %>%
        dplyr::ungroup() %>%
        na.omit() %>% 
        dplyr::mutate(error = func_loss(prediction, data_y),
                      alpha = as.character(alpha), 
                      lambda = as.character(lambda))

    error <- predictions_and_errors %>%
        select(-data_y, -prediction) %>%
        tidyr::pivot_wider(names_from = c(alpha, lambda), values_from = error)
    c_error <- error %>% 
        dplyr::select(-date_int) %>%
        roll_matrix_mean(matrix=., na.rm=TRUE) %>%
        lag_columns(data=., lag_n=1, as_matrix=FALSE, na.omit=FALSE) %>%  
        dplyr::mutate(date_int = error$date_int) %>%
        tidyr::pivot_longer(-date_int, names_to='alpha_lambda', values_to='c_error') 
    alpha_lambda_vals <- unlist(c_error$alpha_lambda %>% stringr::str_split(pattern='_'))
    c_error <- c_error %>%
        dplyr::mutate(alpha = alpha_lambda_vals[seq(1, NROW(alpha_lambda_vals), by=2)], 
                      lambda = alpha_lambda_vals[seq(2, NROW(alpha_lambda_vals), by=2)]) %>% 
        dplyr::select(-alpha_lambda)    
    predictions_and_errors <- predictions_and_errors %>%
        dplyr::full_join(y=c_error, by=c('alpha', 'date_int', 'lambda')) %>%
        dplyr::group_by(date_int) %>% 
        dplyr::mutate(c_error_rank = rank(c_error, ties.method='random')) %>%
        dplyr::ungroup()

    n_insample <- pos_start_oos 
    n_outsample <- length(unique(predictions_and_errors$date_int)) - n_insample
    best_forecast_enet <- predictions_and_errors %>%
        dplyr::filter(c_error_rank == 1) %>%
        dplyr::arrange(date_int) %>%  # dplyr::filter messes up the order
        dplyr::mutate(period = c(rep('in-sample', length.out=n_insample),
                                 rep('out-of-sample', length.out=n_outsample)),
                      model_name = paste0('enet-', func_loss_name),
                      family = paste0('regularization-', func_loss_name)) %>%
        dplyr::select(-data_y, -c_error_rank, -c_error, -error) %>%
        dplyr::rename(vol = 'prediction')
    best_forecast_ridge <- predictions_and_errors %>%
        dplyr::filter(alpha == '0') %>%
        dplyr::arrange(date_int) %>%
        dplyr::group_by(date_int) %>% 
        dplyr::filter(c_error_rank == min(c_error_rank)) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(period = c(rep('in-sample', length.out=n_insample),
                                 rep('out-of-sample', length.out=n_outsample)),
                      model_name = paste0('ridge-', func_loss_name),
                      family = paste0('regularization-', func_loss_name)) %>% 
        dplyr::select(-data_y, -c_error_rank, -c_error, -error) %>%
        dplyr::rename(vol = 'prediction')
    best_forecast_lasso <- predictions_and_errors %>%
        dplyr::filter(alpha == '1') %>%
        dplyr::arrange(date_int) %>% 
        dplyr::group_by(date_int) %>%
        dplyr::filter(c_error_rank == min(c_error_rank)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(period = c(rep('in-sample', length.out=n_insample),
                                 rep('out-of-sample', length.out=n_outsample)),
                      model_name = paste0('lasso-', func_loss_name),
                      family = paste0('regularization-', func_loss_name)) %>%
        dplyr::select(-data_y, -c_error_rank, -c_error, -error) %>% 
        dplyr::rename(vol = 'prediction')
    if (save_errors) {
        save_output(data=predictions_and_errors, path_folder_save=path_folder_errors,
                    csv=TRUE, ext_file_name=paste0('regularization_errors_', func_loss_name))
    }
    if (save_best_forecast) {
        save_output(data=best_forecast_enet, path_folder_save=path_folder_best,
                    csv=TRUE, ext_file_name=paste0('enet-', func_loss_name))
        save_output(data=best_forecast_ridge, path_folder_save=path_folder_best,
                    csv=TRUE, ext_file_name=paste0('ridge-', func_loss_name))
        save_output(data=best_forecast_lasso, path_folder_save=path_folder_best,
                    csv=TRUE, ext_file_name=paste0('lasso-', func_loss_name))
    }
}

