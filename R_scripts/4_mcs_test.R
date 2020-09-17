# If you need to run this script alone:
# import get_cluster, close_cluster, save_output, remove_duplicated_columns from 0_helper_functions.R

mcs_test <- function (data, alpha, B=5000, statistic='Tmax', save_mcs_data,
                      path_folder_save, parallel=FALSE, par_options) {
    # Mostly just calls the MCSprocedure function from the MCS package and saves
    # the output, but it also eliminates identical columns (i.e., models that
    # make identical predictions).
    #
    if (parallel) {
        cluster_in_use <- get_cluster(max_cores=par_options$max_cores)
    } else {
        cluster_in_use <- NULL
    }
    original_models <- colnames(data)
    data <- remove_duplicated_columns(data)
    duplicated_models <- original_models[which(original_models %not_in% colnames(data))]
    mcs_results <- MCS::MCSprocedure(Loss=data, alpha=alpha, B=B, statistic=statistic,
                                     cl=cluster_in_use)
    if (save_mcs_data) {
        save_output(data=mcs_results, path_folder_save=path_folder_save,
                    csv=FALSE, ext_file_name='results')
        save_output(data=duplicated_models, path_folder_save=path_folder_save,
                    csv=TRUE, ext_file_name='duplicated_models')
    }
    if (parallel) {
        on.exit(close_cluster(cluster=cluster_in_use))
    }
}
