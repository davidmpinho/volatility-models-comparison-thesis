# If you need to run this script alone:
# import list_to_df, save_output from 0_helper_functions.R

return_garch_forecast <- function(data_rv, data_returns, model, lag_p, lag_q,
                                  sub_model, arma_p, arma_q, include_cond_mean,
                                  include_arch_m, cond_dist, path_folder_save='./',
                                  save_fit_in_pos=NA) {
    # Calls rugarch::ugarchspec and rugarch::ugarchfit, and saves the
    # fit object when save_fit_in_pos == NROW(data_y)+1.
    # The output (type: data.frame) is meant to make the analysis easier.
    #
    # Note: this function does not work with the MC-GARCH
    # ('mcsGARCH' in the rugarch package).
    #
    list_to_df(list_of_vars=list(data_rv=data_rv, data_returns=data_returns))

    spec <- rugarch::ugarchspec(
        variance.model=list(model=model, garchOrder=c(lag_p, lag_q),
                            submodel=sub_model),
        mean.model=list(armaOrder=c(arma_p, arma_q), archm=include_arch_m,
                        include.mean=include_cond_mean),
        distribution.model=cond_dist)
    
    fit <- .get_garch_fit(spec=spec, data=data_returns, data_rv=data_rv, model=model) # Why is model here??
    if ((NROW(data_returns)+1) %in% save_fit_in_pos) {
        save_output(data=fit, path_folder_save=path_folder_save, csv=FALSE,
                    ext_file_name=paste(model, sub_model, lag_p, lag_q,
                                        include_cond_mean, include_arch_m,
                                        cond_dist, sep='-'))
    }

    forecast <- rugarch::ugarchforecast(fit=fit, data=data_returns,
                                        n.ahead=1, realVol=data_rv)
    if (model == 'realGARCH') {
        forecast <- data.frame(
            date_int = rep((NROW(data_rv)+1), 2),
            vol=c(forecast@forecast$realizedFor[[1]],
                  forecast@forecast$sigmaFor[[1]]^2),
            period = rep('out-of-sample', 2),
            model_name = c(
                paste('realGARCH_rv', lag_p, lag_q, arma_p, arma_q, include_cond_mean,
                      include_arch_m, cond_dist, sep='-'),
                paste('realGARCH_ret', lag_p, lag_q, arma_p, arma_q, include_cond_mean,
                      include_arch_m, cond_dist, sep='-')),
            family = rep('garch', 2), vol_proxy = c('rv', 'squared_ret'),
            garch_model = model, garch_sub_model = sub_model,
            garch_lag_p = rep(lag_p, 2), lag_q = rep(lag_q, 2),
            garch_arma_p = rep(arma_p, 2), arma_q = rep(arma_q, 2),
            garch_include_cond_mean = rep(include_cond_mean, 2),
            garch_include_arch_m = rep(include_arch_m, 2),
            garch_cond_dist = rep(cond_dist, 2))
    } else {
        forecast <- data.frame(
            date_int = (NROW(data_rv)+1), vol = forecast@forecast$sigmaFor[[1]]^2,
            period = 'out-of-sample', family = 'garch', vol_proxy = 'squared_ret',
            model_name = paste(model, sub_model, lag_p, lag_q, arma_p, arma_q,
                               include_cond_mean, include_arch_m, cond_dist, sep='-'),
            garch_model = model, garch_sub_model = sub_model, garch_lag_p = lag_p, lag_q = lag_q,
            garch_arma_p = arma_p, arma_q = arma_q, garch_include_cond_mean = include_cond_mean,
            garch_include_arch_m = include_arch_m, garch_cond_dist = cond_dist)
    }
    return(forecast)
}
.get_garch_fit <- function (spec, data_returns, data_rv, model) {
    n_restarts <- 1000
    n_sim <- 40000
    if (model == 'realGARCH') {
        fit <- rugarch::ugarchfit(spec=spec, data=data_returns, realizedVol=data_rv, solver = 'hybrid',
                                  solver.control = list(n.restarts=n_restarts, n.sim=n_sim),
                                  out.sample=0) 
    } else {
        fit <- rugarch::ugarchfit(spec=spec, data=data_returns, out.sample=0, solver = 'hybrid',
                                  solver.control = list(n.restarts=n_restarts, n.sim=n_sim))
    }
    return(fit)
}

clean_garch_models <- function (all_models) {
    # Takes the all_models dataframe and outputs another dataframe where 
    # every fGARCH model has the correct sub-model.
    all_models <- .make_fgarch_correction(all_models=all_models)
    all_models <- .make_arch_correction(all_models=all_models)
    return(all_models)
}
.make_fgarch_correction <- function(all_models) {
    # Set the correct submodel for fGARCH models
    # Note: the EGARCH isn't implemented in this way in rugarch,
    # even though that is a model from the family.
    f_garch_models <- c('GARCH', 'AVGARCH', 'GJRGARCH', 'TGARCH',
                        'NGARCH', 'NAGARCH', 'APARCH', 'ALLGARCH')  
    in_f_garch <- (all_models[ , 'model'][[1]] %in% f_garch_models)
    all_models[in_f_garch, sub_model := model] 
    all_models[in_f_garch, model := 'fGARCH'] 
    return(all_models)
}
.make_arch_correction <- function(all_models) {
    # There is no 'ARCH' model in rugarch, you have to use the 
    # GARCH specification with lag_p = 0.
    is_arch <- (all_models[ , 'model'][[1]] == 'ARCH')
    all_models[is_arch, lag_p := 0]
    all_models[is_arch, model := 'sGARCH'] 
    return(all_models)
}
