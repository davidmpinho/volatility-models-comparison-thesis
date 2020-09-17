# If you need to run this script alone:
# import list_to_df, save_output from 0_helper_functions.R

rand_walk_forecast <- function (data, int_start_forecast,
                                path_folder_save='.', vol_proxy='rv',
                                save_output=FALSE, return_output=TRUE) {
    # Random walk forecast.
    # The output (type: data.frame) is meant to make the analysis easier.
    #
    data <- dplyr::lag(data, n=1)
    obs_insample <- int_start_forecast - 1
    obs_outsample <- NROW(data) - int_start_forecast+1
    data <- data.frame(
        date_int = seq_len(NROW(data)),
        vol = data, period = c(rep('in-sample', obs_insample),
                               rep('out-of-sample', obs_outsample)),
        model_name = paste0('random_walk-', vol_proxy),
        family = paste0('naive-', vol_proxy),
        vol_proxy = vol_proxy)
    if (save_output) {
       save_output(data=data, path_folder_save=path_folder_save, csv=TRUE,
                   ext_file_name=paste0('random_walk-', vol_proxy))
    }
    if (return_output) {
        return(data)
    }
}

roll_avg_forecast <- function (data, window, int_start_forecast,
                               path_folder_save='./', vol_proxy='rv',
                               save_output=FALSE, return_output=TRUE) {
    # Rolling average walk forecast.
    # The output (type: data.frame) is meant to make the analysis easier.
    #
    data <- zoo::rollapply(data, width=window, partial=TRUE,
                           FUN=mean, by.column=TRUE, fill=NA,
                           align='right')
    data <- dplyr::lag(data)
    obs_insample <- int_start_forecast - 1
    obs_outsample <- NROW(data) - int_start_forecast + 1
    data <- data.frame(
        date_int = seq_len(NROW(data)),
        vol = data, period = c(rep('in-sample', obs_insample),
                               rep('out-of-sample', obs_outsample)),
        model_name = paste('roll_avg', window, vol_proxy, sep='-'),
        family = paste('naive', vol_proxy, sep='-'),
        vol_proxy = vol_proxy, rollavg_window = as.character(window))
    if (save_output) {
        save_output(data=data, path_folder_save=path_folder_save, csv=TRUE,
                    ext_file_name=paste('roll_avg', window, vol_proxy, sep='-'))
    }
    if (return_output) {
        return(data)
    }
}

exp_smooth_forecast <- function (data, beta, int_start_forecast,
                                 path_folder_save='./', vol_proxy='rv',
                                 save_output=FALSE, return_output=TRUE) {
    # Exponential smoothing forecast.
    # The output (type: data.frame) is meant to make the analysis easier.
    #
    prev_smooth_value <- data[1]
    smooth_values <- prev_smooth_value
    for (value in data[2:length(data)]) {
        prev_smooth_value <- value * beta + prev_smooth_value * (1-beta)
        smooth_values <- c(smooth_values, prev_smooth_value)
    }
    smooth_values <- dplyr::lag(smooth_values)

    obs_insample <- int_start_forecast - 1
    obs_outsample <- NROW(data) - int_start_forecast + 1
    data <- data.frame(
        date_int = seq_len(NROW(data)),
        vol = smooth_values, period = c(rep('in-sample', obs_insample),
                                        rep('out-of-sample', obs_outsample)),
        model_name = paste('exp_smooth', beta, vol_proxy, sep='-'),
        family = paste('naive', vol_proxy, sep='-'),
        vol_proxy = vol_proxy, expsmooth_beta = as.character(beta))
    if (save_output) {
        save_output(data=data, path_folder_save=path_folder_save, csv=TRUE,
                    ext_file_name=paste('exp_smooth', beta, vol_proxy, sep='-'))
    }
    if (return_output) {
        return(data)
    }
}

