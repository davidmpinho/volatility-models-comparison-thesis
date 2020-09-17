# If you need to run this script alone:
# library(dplyr)  

# GENERAL FUNCTIONS -----------------------------------------------------------
`%not_in%` <- Negate(`%in%`)

as_numeric_scientific <- function (char) {
    # Converts a 'character' to 'numeric', even if it is in the format that
    # R uses for scientific notation.
    # Slightly adapted from the genpwr package:
    # https://rdrr.io/cran/genpwr/src/R/plotting_function.R
    #
    if(grepl(pattern="e.", char)) {
        char <- gsub(pattern="e.", replacement="e-", char)
    }
    return(as.numeric(char))
}

get_int_no_remainder <- function(start, end, numerator) {
    list_ints <- (start:end)[((numerator %% (start:end)) == 0)]
    return(list_ints)
}

list_to_df <- function (list_of_vars) {
    # Convert the elements of a list to dataframes. Each element becomes a
    # dataframe whose name is equal to their respective key. For example,
    # the list `test`:
    # > test <- list(a = c(1, 2, 3), b = c(4, 5, 6)
    # gets converted into two dataframes, `a` and `b`, with their respestive
    # vectors.
    #
    for (list_name in names(list_of_vars)) {
        if (!is.data.frame(list_of_vars[[list_name]])) {
            assign(x=list_name, value=list_of_vars[[list_name]][[1]], envir=parent.frame(n=1))
        }
    }
}

save_output <- function (data, path_folder_save, csv=FALSE, ext_file_name='') {
    # If `path_folder_save` does not exist, this function first creates it.
    # `data` is then saved to this folder as a csv file (with `csv` set to TRUE)
    # or as an R object (with `csv` set to FALSE).
    # The date and time that the file was created on is written on the file name.
    # You should always add a distinct extention to the file name
    # (`ext_file_name`) so that you do not run the risk of saving 2+ identially-named
    # files in the same second.
    #
    ext_file_name <- paste(ext_file_name, collapse=' + ')
    file_name <- gsub(pattern='-|:', replacement='', Sys.time())
    file_name <- gsub(pattern=' ', replacement='_', file_name)
    file_name <- paste0(path_folder_save, ext_file_name, '_', file_name)

    if (!dir.exists(path_folder_save)) {
        dir.create(path_folder_save)
    }
    if (csv) {
        file_name <- paste0(file_name, '.csv')
        readr::write_csv(as.data.frame(data), file_name,
                         append=FALSE, col_names=TRUE)
    } else {
        save(data, file=file_name)
    }
}

lag_columns <- function (data, lag_n=1, na.omit=FALSE, as_matrix=TRUE) {
    # Lag the observations in each column of a data.frame.
    data <- apply(X=data, MARGIN=2, FUN=dplyr::lag, n=lag_n)
    if (na.omit) {
        data <- na.omit(data)
    }
    if (!as_matrix) {
        data <- as.data.frame(data)
    }
    return(data)
}

remove_duplicated_columns <- function (data, fromLast=FALSE) {
    # Eliminates all duplicated rows except the last one in the group of
    # duplicates.
    duplicated_cols <- as.vector(duplicated(t(data), fromLast=fromLast))
    data <- data[ , !duplicated_cols]
    return(data)
}

roll_forecast <- function (model_func, model_func_args, pos_start_forecast,
                           n_forecasts, do_par=FALSE, do_par_options=list(),
                           path_folder_save='./', model_family=NA) {
    # Saves step-ahead forecasts of a model to a csv file.
    # --------------------------------------------------------------------------
    # Input
    # model_func (type: function)
    #       The model/algorithm being applied.
    #
    # model_func_args (type: list)
    #       Arguments for that model/algorithm.
    #
    # model_func_args (type: list)
    #       Arguments for that model/algorithm. This includes the data.
    #
    # pos_start_forecast (type: integer)
    #       The position where this function starts making forecasts.
    #
    # n_forecasts (type: integer)
    #       How many forecasts will be generated.
    # do_par (type: logical)
    #       Run the function parallel (by setting `do_par` as TRUE) or not
    #       (by setting setting `do_par` as FALSE). This uses the 'foreach'
    #       package.
    #
    # do_par_options (type: list)
    #       Options for the `dopar` function from the 'foreach' package.
    #
    # path_folder_save (type: character)
    #       The folder where you want to save the step-ahead forecasts.
    #       This saves a csv file.
    #
    # model_family (type: character)
    #       This is used in the `.prepare_data_for_forecast` function
    #
    # --------------------------------------------------------------------------
    # Output (type: data.frame)
    #       It saves the forecast on a csv file.
    #
    number_models <- NROW(model_func_args)
    if (do_par) {
        cluster_in_use <- get_cluster(max_cores=do_par_options$max_cores)
        foreach::foreach(m=2:number_models, .inorder=FALSE, .packages=do_par_options$packages) %dopar% {
            # Note: Using .export gives me bugs on Windows ("already exporting variables (...)").
            # Using other ways of doing this gives me bugs on one computer using Linux.
            # This way works for everything.
            path_global_variables <- here::here('/R_exec/ftse100/global_variables.R')
            invisible(source(path_global_variables))
            script_names <- list.files(path=path_r_scripts, pattern="*.R$", full.names=TRUE)
            invisible(lapply(script_names, FUN=source))
            
            m_args <- model_func_args[m, ]
            forecasts <- .step_ahead_forecast(model_func=model_func, m_args=m_args,
                                             pos_start_forecast=pos_start_forecast,
                                             n_forecasts=n_forecasts, model_family=model_family)
            save_output(data=forecasts, csv=TRUE, path_folder_save=path_folder_save,
                        ext_file_name=.get_ext_file_name(dataframe=forecasts))
        }
        on.exit(close_cluster(cluster=cluster_in_use))
    } else {
        for (m in 1:number_models) {
            m_args <- model_func_args[m, ]
            forecasts <- .step_ahead_forecast(model_func=model_func, m_args=m_args,
                                             pos_start_forecast=pos_start_forecast,
                                             n_forecasts=n_forecasts, model_family=model_family)

            save_output(data=forecasts, csv=TRUE, path_folder_save=path_folder_save,
                        ext_file_name=.get_ext_file_name(dataframe=forecasts))
        }
    }
}
.step_ahead_forecast <- function (model_func, m_args, pos_start_forecast,
                                  model_family, n_forecasts) {
    # Creates the step-ahead forecasts. The arguments are the same ones from
    # the `roll_forecast` function.
    #
    forecasts <- vector(mode='list', length=n_forecasts)
    for (i in 1:n_forecasts) {
        m_args_forecast <- .prepare_data_for_forecast(i=i, m_args=m_args,
                                                     model_family=model_family,
                                                     pos_start_forecast=pos_start_forecast)
        forecasts[[i]] <- do.call(what=model_func, args=m_args_forecast)  # return the rows meant to be saved.
    }
    forecasts <- do.call(what='rbind', args=forecasts)
    return(forecasts)
}
.prepare_data_for_forecast <- function (i, m_args, model_family, pos_start_forecast) {
    # Changes the data in `model_func_args` from the `roll_forecast` function.
    # It only passes the observations of the data up to the `i`th position.
    #
    if (is.na(model_family)) {
        stop('Define the model_family in the roll_forecast function.')
    } else if (model_family == 'garch') {
        m_args <- replace(x=m_args, list=c('data_returns', 'data_rv'),
                          values=list(data_returns = list(m_args$data_returns[[1]][1:(pos_start_forecast-2+i)]),
                                      data_rv = list(m_args$data_rv[[1]][1:(pos_start_forecast-2+i)])))
    } else if (model_family == 'har') {
        m_args <- replace(x=m_args, list=c('data'),
                          values=list(list(m_args$data[[1]][1:(pos_start_forecast-2+i), ])))
    } else if (model_family == 'arima') {
        m_args <- replace(x=m_args, list=c('data_y'),
                          values=list(list(m_args$data_y[[1]][1:(pos_start_forecast-2+i)])))
    } else if (model_family == 'glmnet') {
        m_args <- replace(x=m_args, list=c('data_x', 'data_y'),
                          values=list(data_x = list(m_args$data_x[[1]][1:(pos_start_forecast-1+i), ]),
                                      data_y = list(m_args$data_y[[1]][1:(pos_start_forecast-2+i), , drop=FALSE])))
    } else {
        stop(paste0('There is no family_name called ', family_name, '.'))
    }
    return(m_args)
}
.get_ext_file_name <- function (dataframe) {
    # Sets the file name as either the 'model_name' or a random
    # number if that column does not exist in `dataframe`.
    #
    tryCatch(
        expr={
            ext_file_name <- unique(dataframe[ , 'model_name'])
            return(ext_file_name)
        }, error= function (error) {
            if (inherits(error, what='simpleError')) {
                ext_file_name <- runif(n=1, min=0, max=1e8)
            } else {
                stop("There is no 'model_name' in dataframe provided to 'save_output',
                      but the error was not a 'simpleError'. Find what is happening.")
            }
            return(ext_file_name)
        })
}

get_parameter_combinations <- function (list_parameters) {
    # Returns combinations of parameters. You input a list
    # with the parameter names and values, and you get a
    # data.table where each row represents one parameter.
    #
    all_models <- data.table::as.data.table(expand.grid(list_parameters,
                                                        stringsAsFactors=FALSE),
                                            stringsAsFactors=FALSE)
    return(all_models)
} 

get_cluster <- function (max_cores) {
    # Initializes a doParallel cluster.
    #
    detected_cores <- parallel::detectCores()
    cores_used <- min((detected_cores-1), max_cores)
    cluster <-  parallel::makeCluster(cores_used, outfile='')
    doParallel::registerDoParallel(cluster)
    return(cluster)
}
close_cluster <- function(cluster) {
    # Closes a doParallel and/or foreach cluster.
    #
    foreach::registerDoSEQ()
    parallel::stopCluster(cluster)
}
    
calculate_error <- function (data_x, data_y, loss_function='mse') {
    # Calculates the error for different loss functions (to get the
    # built-in loss functions, average the errors).
    #
    # --------------------------------------------------------------------------
    # Input
    # data_x (type: data.frame)
    #       Explanatory variable.
    #
    # data_y (type: data.frame)
    #       Response variable.
    #
    # loss_function (type: character)
    #       How many forecasts will be generated.
    #
    # -------------------------------------------------------------------------
    # Output (type: character)
    #       The calculated error.
    #
    error_matrix <- switch(loss_function,
                            'mse' = .error_squared(data_x=data_x,
                                                   data_y=data_y),
                            'mae' = .error_absolute(data_x=data_x,
                                                    data_y=data_y),
                            'r2_log' = .error_r2_log(data_x=data_x,
                                                     data_y=data_y),
                            'qlike' = .error_qlike(data_x=data_x,
                                                   data_y=data_y),
                            'qlike2' = .error_qlike2(data_x=data_x,
                                                     data_y=data_y))
    return(error_matrix)
}
.error_squared <- function (data_x, data_y){
    # Mean squared error
    mse <- (data_x - data_y)^2
    return(mse)
}
.error_absolute <- function (data_x, data_y){
    # Mean absolute error
    mae <- abs(data_x - data_y)
    return(mae)
}
.error_r2_log <- function (data_x, data_y) {
    # log R-squared
    r2_log <- log(data_x^2 / data_y^2)
    return(r2_log)
}
.error_qlike <- function (data_x, data_y) {
    # Quasi-likelihood
    qlike <- log(data_x) + (data_y / data_x)
    return(qlike)
}
.error_qlike2 <- function (data_x, data_y) {
    # Quasi-likelihood (alternative specification)
    qlike2 <- data_y / data_x - log(data_y / data_x) - 1
    return(qlike2)
}

mse <- function (data_x, data_y) {
    mse <- mean((data_x - data_y)^2)
    return(mse)
}
mae <- function (data_x, data_y) {
    mae <- mean(abs(data_x - data_y))
    return(mae)
}
r2_log <- function (data_x, data_y) {
    r2_log <- mean(log(data_x^2 / data_y^2))
    return(r2_log)
}
qlike <- function (data_x, data_y) {
    qlike <- mean(log(data_x) + (data_y / data_x))
    return(qlike)
}

# COMBINATIONS ----------------------------------------------------------------
join_forecast_data <- function (path, regex) {
    # Join all individual model forecast data.
    file_names <- list.files(path=path, pattern=regex)
    data <- lapply(X=paste(path, file_names, sep='/'),
                   FUN=read.csv, header=TRUE, stringsAsFactors=FALSE)
    data <- data.table::rbindlist(data, fill=TRUE)
    return(data)
}
join_and_adjust_comb_data <- function (path, regex, sum_to_date_int=0) {
    # Join all model combination forecast data.
    data <- join_forecast_data(path=path, regex=regex)
    data$date_int <- data$date_int + sum_to_date_int
    return(data)
}
join_and_adjust_all_data <- function (path_ind, path_comb,
                                      sum_to_date_int=1, regex) {
    # Aggregate both individual and combination model forecast data.
    data_ind <- join_forecast_data(path=path_ind, regex=regex)
    data_ind[ , 'ind_or_comb'] <- 'ind'
    data_comb <- join_and_adjust_comb_data(path=path_comb, regex=regex,
                                           sum_to_date_int=sum_to_date_int)
    data_comb[ , 'ind_or_comb'] <- 'comb'
    all_data <- rbind(data_ind, data_comb, fill=TRUE)
    return(all_data)
}

div_min_loss <- function (x, min_loss) {
    return(x/min_loss)
}
multiply_weight <- function (matrix, weights) {
    return(matrix * weights)
}

join_reg_forecasts <- function (path, col_lambda_start=1, n_lambda) {
    # Note: this replaces negative forecasts by X_{t-1}/2
    files <- list.files(path, full.names=TRUE)
    data <- lapply(X=files, FUN=read.csv)
    data <- data.table::rbindlist(data)
    data <- data %>%
        tidyr::pivot_longer(cols=tidyselect::all_of(col_lambda_start:(col_lambda_start + n_lambda-1)),
                            names_to='lambda', values_to='prediction') %>%
        dplyr::group_by(alpha, lambda) %>%
        dplyr::mutate(prediction = replace_negative_predictions(predictions=prediction)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(lambda = stringr::str_remove(string=lambda, pattern='X'),
                      lambda = unlist(lapply(X=lambda, FUN=as_numeric_scientific)))
    return(data)
}
replace_negative_predictions <- function (predictions) {
    if (predictions[1] < 0) {
        predictions[1] <- 0.0001
    }
    while (sum(predictions < 0) > 0) {
        previous_obs <- (which(predictions < 0)-1)
        predictions[which(predictions < 0)] <- predictions[previous_obs]/2
    }
    return(predictions)
}
roll_matrix_mean <- function (matrix, na.rm) {
    # Partial is always true and it always does everything to all the columns
    new_matrix <- matrix(data=NA, nrow=NROW(matrix), ncol=NCOL(matrix))
    colnames(new_matrix) <- colnames(matrix)
    for (row in 1:NROW(matrix)) {
        new_matrix[row, ] <- colMeans(matrix[1:row, ], na.rm=na.rm)
    }
    return(new_matrix)
}

