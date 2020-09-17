# If you need to run this script alone:
# library(dplyr)  

har_model_fit <- function(data, parameters) {
    # Fit Heterogenous AutoRegressive (HAR) models.
    # Allows for HAR-RV, HAR-J, HARQ, HAR-C, and SHAR
    # models, and combinations of all of those.
    # The methodology in my thesis goes into more detail
    # on how these models are fit.
    #
    # --------------------------------------------------------------------------
    # Input
    # data (type: data.frame)
    #       These data should have the realised volatility measures
    #       that you want to use. The data should not be formatted
    #       (no need to include multiple measures with lags, or
    #       to include the positive and negative semi variance measures)
    #       since that is taken care of by the function.
    #       The dataframe's name of the columns need to correspond to the parameters
    #       you want to use ('rv', 'c', 'j', 'q', and/or 's').
    #
    # parameters (type: list)
    #       This is used to specify the type of model you want to use (rv/c/j/q/s),
    #       and how many observations you want to aggregate. For example, if you want
    #       to fit the typical HAR-RV model, you would use:
    #       > har_rv_params <- list(rv = c(1, 5, 22))
    #       > har_rv_fit <- har_model_fit(data=data_with_rv, parameters=har_rv_params)
    #
    # --------------------------------------------------------------------------
    # Output (type: list)
    #       List with two keys: a `lm` object (`lm_object`) and the data used
    #       for the regression (`data_predictors`).
    #
    parameters_allowed <- c('rv', 'j', 'q', 'c', 's')
    parameters <- parameters %>%
        dplyr::select_if(!is.na(.) & (colnames(.) %in% parameters_allowed))
    parameter_names <- names(parameters)
    if ('q' %in% parameter_names) {
        parameter_names <- .move_q_to_last(parameter_names)
    }
    data <- data %>%
        dplyr::select_if(colnames(.) %in% c(parameter_names, 'rv'))
    for (p_name in parameter_names) {
        data <- .add_predictors_to_data(data=data, parameter_name=p_name,
                                        parameter_list=parameters)
    }

    data <- na.omit(data)
    rv_response <- data[2:NROW(data), 'rv']
    data <- data[ , !(colnames(data) %in% parameters_allowed)]
    predictors <- as.matrix(apply(X=data, FUN=dplyr::lag, MARGIN=2))
    predictors <- na.omit(predictors)
    lm_object <- lm(rv_response ~ predictors)
    names(lm_object$coefficients) <- c('alpha', colnames(predictors))
    return(list(lm_object=lm_object, data_predictors=data))
}
har_model_forecast <- function (data, fit_obj, rv_log, period=1) {
    # Predict values for an `lm` object originated from the `har_model_fit`
    # function. Currently only allows step-ahead forecasts (set `period` equal to 1).
    #
    # --------------------------------------------------------------------------
    # Input
    # data (type: data.frame)
    #       The `data_predictors` output from the `har_model_fit` output, or data
    #       that is in the same format (needs to have column names than the `fit_obj`.
    #
    # fit_obj (type: lm)
    #       There needs to be concordance between the names in `data` and the
    #       names of these variables.
    #
    # rv_log (type: logical)
    #       This makes the adjustment when using the logarithm of
    #       realised volatility. This adjustment yields a forecast for the
    #       mean instead of the median.
    #
    # --------------------------------------------------------------------------
    # Output (type: matrix)
    #       Forecasts. The function produces one forecast for each row of
    #       the `data` input.
    #
    if (period == 1) {
        data <- merge(data.frame(alpha=1), data)  # 'alpha' must be the first column
        forecast <- as.matrix(data[NROW(data), , drop=FALSE]) %*% fit_obj$coefficients
        if (rv_log) {
            residual_var <- var(stats::residuals(fit_obj))
            forecast <- exp(forecast + residual_var/2)
        }
    } else {
        stop("Forecasts for more than one period are not implemented yet.")
    }
    return(forecast)
}

.add_predictors_to_data <- function (data, parameter_name, parameter_list) {
    parameter_periods <- parameter_list[[parameter_name]][[1]]
    if (parameter_name %in% c('c', 'j', 'rv')) {
        data <- .add_column_c_j_rv(data=data, parameter_name=parameter_name,
                                   parameter_periods=parameter_periods)
    } else if (parameter_name == 's') {
        data <- .add_column_s(data=data, parameter_periods=parameter_periods)
    } else if (parameter_name == 'q') {
        data <- .add_column_q(data=data, parameter_periods=parameter_periods)
    } else {
        stop(paste0("At least one of the parameters you used is not allowed.
                     The ones allowed are 'c', 'j', 'rv', or 'q'."))
    }
    return(data)
}
.add_column_q <- function(data, parameter_periods) {
    is_not_j <- !stringr::str_detect(string=colnames(data), pattern='j')
    is_parameter <- stringr::str_detect(string=colnames(data), pattern='_')
    for (period in parameter_periods) {
        is_in_q_period <- stringr::str_detect(string=colnames(data),
                                              pattern=paste0('_', period))
        parameters <- colnames(data)[is_in_q_period & is_not_j & is_parameter]
        parameters <- parameters %>% stringr::str_split(pattern='_') %>%
            lapply(X=., FUN=dplyr::first) %>% unlist() %>% unique()
        parameters <- parameters[parameters != 's-pos']
        for (p_name in parameters) {
            if (p_name == 's-neg') {
                q_data_neg <- data[ , 'q']^(1/2) * data[ , 's']
                q_data_pos <- data[ , 'q']^(1/2) * (data[ , 'rv'] - data[ , 's'])
                new_var_name_neg <- paste0('q.s-neg_', period)
                new_var_name_pos <- paste0('q.s-pos_', period)
                data[ , new_var_name_neg] <- zoo::rollmean(x=q_data_neg, k=period,
                                                           fill=NA, align='right')
                data[ , new_var_name_pos] <- zoo::rollmean(x=q_data_pos, k=period,
                                                           fill=NA, align='right')
            } else {
                q_data <- data[ , 'q']^(1/2) * data[ , p_name]
                new_var_name <- paste('q.', p_name, period, sep='_')
                data[ , new_var_name] <- zoo::rollmean(x=q_data, k=period,
                                                       fill=NA, align='right')
            }
        }
    }
    return(data)
}
.add_column_s <- function (data, parameter_periods) {
    for (period in parameter_periods) {
        neg_var_name <- paste('s-pos', period, sep='_')
        pos_var_name <- paste('s-neg', period, sep='_')
        rv_minus_data <- zoo::rollmean(x=data[ , 's'], k=period,
                                       fill=NA, align='right') 
        rv_plus_data <- zoo::rollmean(x=(data[ , 'rv'] - data[ , 's']),
                                      k=period, fill=NA, align='right') 
        data[ , neg_var_name] <- rv_minus_data
        data[ , pos_var_name] <- rv_plus_data
        return(data)
    }
}

.add_column_c_j_rv <- function(data, parameter_name, parameter_periods) {
    for (period in parameter_periods) {
        new_var_name <- paste(parameter_name, period, sep='_')
        new_data <- zoo::rollmean(x=data[ , parameter_name], k=period,
                                  fill=NA, align='right')
        data <- cbind(data, new_data) 
        colnames(data)[NCOL(data)] <- new_var_name
    }
    return(data)
}
.move_q_to_last <- function (parameter_names) {
    # The 'q' is applied to all parameters that only use one lagged observation,
    # so it needs to be the last observation.
    #
    parameter_names <- parameter_names[parameter_names != 'q']
    parameter_names <- c(parameter_names, 'q')
    return(parameter_names)
}

