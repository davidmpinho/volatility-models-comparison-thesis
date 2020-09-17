# If you need to run this script alone:
# import save_output, calculate_error, lag_columns, div_min_loss, multiply_weight from 0_helper_functions.R

get_weights_matrix <- function (strategy_list, data_x, data_y,
                                weights_matrix=NULL, start_forecast=1,
                                loss_function='mse', save_matrix=FALSE,
                                path_save_folder='./') {
    # Generates the weights for each model combination scheme.
    #
    # --------------------------------------------------------------------------
    # Input
    # strategy_list (type: list)
    #       The model/algorithm being applied. The possible keys of your list are
    #       'group' (grouping), 'trim' (trimming), and 'equal' (equal weight).
    #       Only the first value in that list will be used.
    #       If you are using 'group', you need to specify a regex that selects the
    #       names of the columns that you want to use for each group;
    #       if you are using 'trim', you only need to specify a numeric value that
    #       represents the threshold  ('k' in my thesis); for 'equal', set the value
    #       to whatever because this function does not use it.
    #
    # weights_matrix (type: matrix)
    #       You can specify a matrix with its own weights, and the strategy you
    #       specify will act on those weights (e.g., you can trim and then group
    #       the variables). (I thought I was going to need this but it is unlikely
    #       to be needed).
    #
    # data_x (type: data.frame)
    #       Explanatory variables (in my case, the predictions made by models).
    #       Each column is a variable.
    #
    # data_y (type: data.frame)
    #       Outcome (in my case, the realised volatility).
    #
    # loss_function (type: data.frame)
    #       The loss function you want to use. The inputs are passed to the
    #       `calculate_error` function from the '0_helper_functions.R' script.
    #
    # save_matrix (type: logical)
    #       If set to TRUE, save the matrix in `path_save_folder`.
    #
    # path_save_folder (type: character)
    #       If the folder does not exist, it will be created.
    # --------------------------------------------------------------------------
    # Output (type: matrix)
    #       It saves the forecast on a csv file.
    #
    strategy <- names(strategy_list)[1]
    if (strategy == 'group') {
        ext_file_name <- paste('group',
                               paste(names(strategy_list[[1]]),
                                     collapse='-'),
                               sep='-')
    } else if (strategy == 'trim') {
        ext_file_name <- paste0('trim-', strategy_list[[1]], '-', loss_function)
    } else if (strategy == 'equal') {
        ext_file_name <- 'equal'
    } else {
        stop('The name of the strategy you used in strategy_list is not supported.')
    }
    if (is.null(weights_matrix)) {
        weights_matrix <- matrix(nrow=(NROW(data_x)-start_forecast),
                                 ncol=NCOL(data_x))
        colnames(weights_matrix) <- colnames(data_x)
    }
    if (!is.null(strategy_list)) {
        weights_matrix <- switch(
            strategy,
            'equal' = .get_equal_weights(weights_matrix=weights_matrix),
            'trim' = .get_trimmed_weigths(weights_matrix=weights_matrix, data_x=data_x,
                                          data_y=data_y, start_forecast=start_forecast,
                                          loss_function=loss_function, cutoff=strategy_list[[1]]),
            'group' = .get_group_weights(weights_matrix=weights_matrix,
                                         strategy_list=strategy_list$group))
    }
    if (save_matrix) {
        save_output(data=weights_matrix, path_folder_save=path_save_folder,
                    csv=TRUE, ext_file_name=ext_file_name)
    }
    return(weights_matrix)
}
.get_equal_weights <- function (weights_matrix) {
    equal_weight <- 1 / NCOL(weights_matrix)
    weights_matrix[ , ] <- equal_weight
    return(weights_matrix)
}
.get_trimmed_weigths <- function (weights_matrix, start_forecast, data_x,
                                  data_y, loss_function, cutoff) {
    data_loss <- calculate_error(data_x=data_x, data_y=data_y,
                                 loss_function=loss_function)
    data_loss <- lag_columns(data=data_loss, lag_n=1, na.omit=TRUE,
                             as_matrix=FALSE)
    data_mean_loss <- zoo::rollapply(data=data_loss, width=NROW(data_loss), 
                                     FUN=mean, fill=NA, align='right',
                                     partial=TRUE)
    
    min_loss <- apply(X=data_mean_loss, MARGIN=1, FUN=min)
    data_mean_loss <- apply(X=data_mean_loss, MARGIN=2, 
                            FUN=div_min_loss, min_loss=min_loss)   
    # It would need the +1 if there was no na.omit 
    data_mean_loss <- data_mean_loss[start_forecast:NROW(data_mean_loss), ]
    
    weights_matrix <- ifelse(data_mean_loss > cutoff, yes=0, no=1)
    row_weights <- 1/rowSums(weights_matrix)
    weights_matrix <- apply(X=weights_matrix, MARGIN=2,
                            FUN=multiply_weight, weights=row_weights)
    return(weights_matrix)
}
.get_group_weights <- function (weights_matrix, strategy_list) {
    weight_per_group <- 1 / length(strategy_list)
    for (group in names(strategy_list)) {
        models_to_use <- strategy_list[[group]]
        weights_matrix[ , models_to_use] <- weight_per_group / length(models_to_use)
    }
    weights_matrix[which(is.na(weights_matrix))] <- 0
    return(weights_matrix)
}

forecast_naive_combinations <- function (data, weights_matrix, strategy_list,
                                         save_results=FALSE, path_folder_save='./',
                                         return_results=TRUE, loss_function='',
                                         pos_start=1) {
    # Starting at the position `pos_start`, this generates a weighted average
    # forecast of other forecasts (which are in `data`) by
    # the specified weights (`weights_matrix`).
    # You can specify the strategy, as in the function get_weights_matrix.
    # This function is mostly used to output results that are then easy to analyze.
    #
    excess_rows <- NROW(data) - NROW(weights_matrix) + 1
    data <- data[excess_rows:NROW(data), ]
    results <- rowSums(data * weights_matrix)
    strategy <- names(strategy_list)[1]
    if (strategy == 'equal') {
        results <- data.frame(
            date_int = pos_start:(pos_start+NROW(data)-1),
            vol = results, period = 'out-of-sample',
            model_name = 'equal_weight',
            family = 'equal_weight')
    } else if (strategy == 'trim') {
        results <- data.frame(
            date_int = pos_start:(pos_start+NROW(data)-1),
            vol = results, period = 'out-of-sample',
            model_name = paste('trim', strategy_list$trim, loss_function, sep='-'),
            family = paste0('trim-', loss_function), 
            trim_k = strategy_list$trim,
            loss_function = loss_function)
    } else if (strategy == 'group') {
        results <- data.frame(
            date_int = pos_start:(pos_start+NROW(data)-1),
            vol = results, period = 'out-of-sample',
            model_name = paste0('group:', paste(names(strategy_list$group), collapse=' ')),
            group_models = paste(names(strategy_list$group), collapse=' '),
            group_n_models = length(names(strategy_list$group)),
            family = 'group')
    }
    
    if (strategy == 'group') {
        ext_file_name <- paste0('group-', paste(names(strategy_list$group), collapse='-'))
    } else {
        ext_file_name <- paste(strategy, strategy_list[[1]], loss_function, sep='-')     
    }
    if (save_results) {
        save_output(data=results, path_folder_save=path_folder_save, csv=TRUE, 
                    ext_file_name=ext_file_name)
    }
    if (return_results) {
        return(results)
    }
}












