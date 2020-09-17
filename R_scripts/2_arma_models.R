# If you need to run this script alone:
# import list_to_df, save_output from 0_helper_functions.R

return_arima_forecast <- function (data_y, vol_proxy, n_ar, n_d, n_ma, method='ML',
                                   save_fit_in_pos=NA, path_folder_save='./') {
    # Calls forecast::Arima and saves the fit object when save_fit_in_pos == NROW(data_y)+1.
    # The output (type: data.frame) is meant to make the analysis easier.
    #
    list_to_df(list_of_vars=list(data_y=data_y))

    fit <- forecast::Arima(y=data_y, order=c(n_ar, n_d, n_ma), method=method)
    if ((NROW(data_y)+1) %in% save_fit_in_pos) {
        save_output(data=fit, path_folder_save=path_folder_save, csv=FALSE,
                    ext_file_name=paste('arima-', n_ar, n_d, n_ma, vol_proxy,
                                        method, sep='-'))
    }
    forecast <- forecast::forecast(object=fit, h=1)
    forecast <- data.frame(
        date_int = (NROW(data_y)+1), vol=forecast$mean[[1]], period = 'out-of-sample',
        model_name = paste('arima', n_ar, n_d, n_ma, vol_proxy, method, sep='-'),

        family = paste('arima', vol_proxy, sep='-'), vol_proxy = vol_proxy,
        arima_ar=n_ar, arima_d=n_d, arima_ma=n_ma, method=method)
    return(forecast)
}

clean_arima_models <- function (table_models) {
    # Makes a small correction to a dataframe with arima parameters.
    # This used to make correction for ARFIMA models, which has to use different
    # APIs, but that became simplified.
    has_zero_params <- (table_models$n_ar == 0) & (table_models$n_ma == 0)
    table_models <- table_models[!has_zero_params]
    return(table_models)
}

