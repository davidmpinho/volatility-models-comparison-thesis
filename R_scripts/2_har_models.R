# If you need to run this script alone:
# import list_to_df, save_output from 0_helper_functions.R
# import 0_har_model.R

return_har_forecast <- function (data, parameter_list, save_fit_in_pos=NA,
                                 path_folder_save='./', rv_log=FALSE) {
    # Calls har_model_fit from the 0_har_model.R script, and saves the
    # fit object when save_fit_in_pos == NROW(data_y)+1.
    # The output (type: data.frame) is meant to make the analysis easier.
    #
    list_to_df(list_of_vars=list(data=data, parameter_list=parameter_list,
                                 rv_log=rv_log, save_fit_in_pos=save_fit_in_pos,
                                 path_folder_save=path_folder_save))

    fit <- har_model_fit(data=data, parameters=parameter_list)
    if ((NROW(data)+1) %in% save_fit_in_pos) {
        rv_par <-parameter_list[['rv']]
        q_par <-parameter_list[['q']]
        j_par <-parameter_list[['j']]
        c_par <-parameter_list[['c']]
        s_par <-parameter_list[['s']]

        save_output(data=fit[['lm_object']], path_folder_save=path_folder_save, csv=FALSE,
                    ext_file_name=paste('har', rv_par, q_par, j_par, c_par, s_par, sep='-'))
    }
    forecast <- har_model_forecast(data=fit[['data_predictors']],
                                   fit_obj=fit[['lm_object']], rv_log=rv_log)
    forecast <- data.frame(
        date_int = (NROW(data)+1), vol=forecast, period = 'out-of-sample',
        model_name = paste('har', parameter_list$c, parameter_list$j, parameter_list$q,
                           parameter_list$rv, parameter_list$s, rv_log, sep='-'),
        family = 'har', vol_proxy = 'rv',
        c=stringr::str_c(as.character(parameter_list$c[[1]]), collapse=';'),
        j=stringr::str_c(as.character(parameter_list$j[[1]]), collapse=';'),
        q=stringr::str_c(as.character(parameter_list$q[[1]]), collapse=';'),
        rv=stringr::str_c(as.character(parameter_list$rv[[1]]), collapse=';'),
        s=stringr::str_c(as.character(parameter_list$s[[1]]), collapse=';'),
        rv_log=rv_log)
    return(forecast)
}

clean_har_models <- function(models) {
    for (row in 1:NROW(models)) {
        models[row, ] <- .remove_incompatible_har(row=models[row, ])
    }
    return(models)
}
.remove_incompatible_har <- function(row) {
    # The priority for the parameters is: S > C > RV
    rv_periods <- row$rv[[1]] %not_in% c(row$s[[1]], row$c[[1]])
    rv_periods <- row$rv[[1]][rv_periods]
    row$rv[[1]] <- ifelse(test=(length(rv_periods) == 0),
                          yes=list(NA), no=list(rv_periods))

    c_periods <- row$c[[1]] %not_in% row$s[[1]]
    c_periods <- row$c[[1]][c_periods]
    row$c[[1]] <- ifelse(test=length(c_periods) == 0, yes=list(NA), no=list(c_periods))
    return(row)
}

