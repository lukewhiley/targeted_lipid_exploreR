
# PROCESS: Welcome section and project set up --------------------------------------

#welcome messages
if(!exists("master_list")){
  dlg_message("Welcome to lipid qc exploreR! :-)", type = 'ok'); dlg_message("Please run lipid SkylineR notebook prior to running this notebook", type = 'ok'); dlg_message("Now open master_list.rda file produced by SkylineR", type = 'ok')
# load rda file
load(file = file.choose())
}

#source LGW github functions
#pca function
master_list$environment$user_functions$pca <- source(paste0(master_list$project_details$github_master_dir,
                                                            "/functions/FUNC_lipidExploreR_PCA.R"))
#missing value filter
master_list$environment$user_functions$miss_value_filter <- source(paste0(master_list$project_details$github_master_dir,
                                                                          "/functions/FUNC_lipidExploreR_missing_value_filter.R"))
#impute data
master_list$environment$user_functions$impute_data <- source(paste0(master_list$project_details$github_master_dir,
                                                                    "/functions/FUNC_lipidExploreR_impute_data.R"))
#concentration calculator
master_list$environment$user_functions$conc_calc <-  source(paste0(master_list$project_details$github_master_dir,
                                                                   "/functions/FUNC_lipidExploreR_conc_calculator.R"))
#pca qc filter
master_list$environment$user_functions$pca_filter <- source(paste0(master_list$project_details$github_master_dir,
                                                                   "/functions/FUNC_lipidExploreR_pca_filter.R"))
#signal/batch correction
master_list$environment$user_functions$signal_correct <- source(paste0(master_list$project_details$github_master_dir,
                                                                       "/functions/FUNC_lipidExploreR_signal_drift_correct.R"))

#run order vs PC plots
master_list$environment$user_functions$pc_run_plot <- source(paste0(master_list$project_details$github_master_dir,
                                                                    "/functions/FUNC_lipidExploreR_pc_runorder_plot.R"))


# PROCESS: transpose data to standard metabolomics structure (features in columns, samples in rows) -------------------------------------- 
# Chunk also creates a data summary
#   -> number of samples
#   -> number of features
#   -> missing values
#   -> NA values
#   -> NAN values

master_list$data$transposed <- list()
master_list$summary_tables$transposed_summary <- list()

for(idx_data in master_list$project_details$mzml_plate_list){
  master_list$data$transposed[[idx_data]] <- pivot_wider(
    data = master_list$data$skyline_reports$report_2 %>%
      filter(file_name %in% names(master_list$data$mzR[[idx_data]])),
    id_cols = file_name,
    names_from = molecule_name,
    values_from = area
  ) %>%
    rename(sample_name = file_name)
  
  #remove file extenstion (.mzML from sample_name)
  master_list$data$transposed[[idx_data]]$sample_name <- sub(".mzML", "", master_list$data$transposed[[idx_data]]$sample_name)
  
  
  #make numeric 
  master_list$data$transposed[[idx_data]][,-1] <-
    sapply(master_list$data$transposed[[idx_data]][,-1], 
           as.numeric) %>% 
    as_tibble()
  
  # TABLE: raw Skyline missing data summary ---------------------------------
  master_list$summary_tables$transposed_summary <- master_list$summary_tables$transposed_summary %>%   bind_rows(
    bind_cols(
      "batch" = idx_data,
      "total samples" = nrow(master_list$data$transposed[[idx_data]]),
      "LTR samples" = length(grep("LTR",  master_list$data$transposed[[idx_data]]$sample_name)), #report number of LTRs in dataset
      "PQC samples" = length(grep("PQC",  master_list$data$transposed[[idx_data]]$sample_name)), #report number of PQC in dataset
      "conditioning samples" = length(grep("cond", master_list$data$transposed[[idx_data]]$sample_name, ignore.case = TRUE)), #report number of conditioning runs in dataset
      "study samples"= nrow(master_list$data$transposed[[idx_data]])-
        length(grep("LTR", master_list$data$transposed[[idx_data]]$sample_name))- 
        length(grep("PQC", master_list$data$transposed[[idx_data]]$sample_name))-
        length(grep("cond", master_list$data$transposed[[idx_data]]$sample_name, ignore.case = TRUE)), #report number of study samples in dataset
      "total features" = ncol(master_list$data$transposed[[idx_data]] %>% 
                                select(-contains("sample"))),
      "zero values" = length(which(master_list$data$transposed[[idx_data]] %>% select(!contains("sample"))==0)),
      "NA values" = length(which(is.na(as.matrix(master_list$data$transposed[[idx_data]] %>% select(!contains("sample")))))),
      "NaN values" = length(which(is.nan(as.matrix(master_list$data$transposed[[idx_data]] %>% select(!contains("sample"))))))
    )
  )
}

master_list$summary_tables$transposed_summary <- rbind(master_list$summary_tables$transposed_summary,
                                                       c("total project",
                                                         sum(master_list$summary_tables$transposed_summary$`total samples`),
                                                         sum(master_list$summary_tables$transposed_summary$`LTR samples`),
                                                         sum(master_list$summary_tables$transposed_summary$`PQC samples`),
                                                         sum(master_list$summary_tables$transposed_summary$`conditioning samples`),
                                                         sum(master_list$summary_tables$transposed_summary$`study samples`),
                                                         max(master_list$summary_tables$transposed_summary$`total features`),
                                                         sum(master_list$summary_tables$transposed_summary$`zero values`),
                                                         sum(master_list$summary_tables$transposed_summary$`NA values`),
                                                         sum(master_list$summary_tables$transposed_summary$`NaN values`)
                                                       ))



#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))


# PROCESS: Sort by run order and add annotation data -------------------------------

#list for storing concentration data sorted by run order
master_list$project_details$run_orders <- list()
master_list$data$sorted <- list()
#run loop
for (idx_data in names(master_list$data$transposed)){
  master_list$project_details$run_orders[[idx_data]] <- sub(".mzML", "", names(master_list$data$mzR[[idx_data]])) %>% as_tibble() %>% rename(sample_name = value)
  temp_timestamp <- NULL
  for(idx_file in names(master_list$data$mzR[[idx_data]])){
    temp_timestamp <- c(temp_timestamp, master_list$data$mzR[[idx_data]][[idx_file]]$mzR_timestamp)
  }
  master_list$project_details$run_orders[[idx_data]] <-  master_list$project_details$run_orders[[idx_data]] %>%
    add_column(sample_timestamp = temp_timestamp) %>%
    arrange(sample_timestamp)
  
  #create metadata columns
  master_list$project_details$run_orders[[idx_data]]$sample_batch <- master_list$project_details$project_name # batch/project name
  master_list$project_details$run_orders[[idx_data]]$sample_plate_id <- idx_data #plate_id
  master_list$project_details$run_orders[[idx_data]]$sample_plate_order <- c(1:nrow(master_list$project_details$run_orders[[idx_data]])) #sample_plate_order
  master_list$project_details$run_orders[[idx_data]]$sample_type <- "sample"
  #set sample_type to qc for all samples with the qc_type tag in their filename
  master_list$project_details$run_orders[[idx_data]]$sample_type[grep(
    master_list$project_details$qc_type,
    master_list$project_details$run_orders[[idx_data]]$sample_name)] <- "qc"
  #set sample_type to conditioning for all samples with the COND tag in their filename
  master_list$project_details$run_orders[[idx_data]]$sample_type[grep(
    "cond", master_list$project_details$run_orders[[idx_data]]$sample_name,
    ignore.case = TRUE)] <- "conditioning"
  
  #sort transposed data by run order and then remove conditioning runs
  master_list$data$sorted[[idx_data]] <- master_list$project_details$run_orders[[idx_data]] %>%
    left_join(master_list$data$transposed[[idx_data]], by = "sample_name") %>% 
    filter(sample_type == "qc" | sample_type == "sample")
  
  #add_factor column for plotting
  master_list$data$sorted[[idx_data]] <- master_list$data$sorted[[idx_data]] %>%
    add_column(sample_type_factor =
                 master_list$data$sorted[[idx_data]]$sample_type %>% factor(levels = c("qc", "sample"), ordered = TRUE),
               sample_type_factor_rev = master_list$data$sorted[[idx_data]]$sample_type %>% factor(levels = c("sample", "qc"), ordered = TRUE),
               .after = "sample_type")
  
}

#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))

# PCA: raw Skyline imports ------------------------------------------------

#create empty list for results
master_list$pca_analysis$data_sorted <- list()
master_list$pca_analysis$data_sorted$sample_qc <- list()
master_list$pca_analysis$data_sorted$plate <- list()

#run pca loop color by QC/sample
master_list$pca_analysis$data_sorted$sample_qc <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$sorted %>% bind_rows(),
  FUNC_metabolite_list =  master_list$data$sorted %>% bind_rows() %>%
    select(-contains("sample")) %>% names(),
  FUNC_colour_by = "sample_type_factor",
  FUNC_plot_label = "sample_name", 
  FUNC_scaling = "Pareto",
  FUNC_title = paste0(master_list$project_details$project_name),
  FUNC_project_colours = c("steelblue2", "white"),
  FUNC_option_invert_y = FALSE,
  FUNC_option_invert_x = FALSE,
  FUNC_option_plot_qc = TRUE
)

#run pca loop color by plate
master_list$pca_analysis$data_sorted$plate <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$sorted %>% bind_rows(),
  FUNC_metabolite_list =  master_list$data$sorted %>% bind_rows() %>%
    select(-contains("sample")) %>% names(),
  FUNC_colour_by = "sample_plate_id",
  FUNC_plot_label = "sample_name", 
  FUNC_scaling = "Pareto",
  FUNC_title = paste0(master_list$project_details$project_name),
  FUNC_project_colours = c(viridisLite::magma(n = length(master_list$project_details$mzml_plate_list))),
  FUNC_option_invert_y = FALSE,
  FUNC_option_invert_x = FALSE,
  FUNC_option_plot_qc = TRUE
)

#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))


# PROCESS: data filter: missing values filter by flags --------------------

# * Step 1: Remove all samples that have > 50% missing values (removes any mis-injections etc that may be present in the data)
# * Step 2: Remove all metabolite features that have > 50% missing values (zero, NA, NaN etc)

#create empty lists for storing outputs
master_list$data$missing_value_filter <- list()
master_list$process_lists$missing_value_filter <- list()
master_list$process_lists$missing_value_filter$failed_SIL <- NULL
master_list$summary_tables$missing_value_filter_summary <- list()
master_list$summary_tables$missing_value_qc_fail <- list()


for(idx_data in names(master_list$data$sorted)){
  master_list$process_lists$missing_value_filter[[idx_data]] <- list()
  #master_list$process_lists$missing_value_filter[[idx_data]]$sample_fail_list <- list()
  #master_list$process_lists$missing_value_filter[[idx_data]]$feature_fail_list <- list()
  
  
  for(idx_sample_type in c("sample", "qc")){
    master_list$process_lists$missing_value_filter[[idx_data]][[idx_sample_type]] <- list()
    
    
    #run missing value filter function
    
    master_list$process_lists$missing_value_filter[[idx_data]][[idx_sample_type]] <- master_list$environment$user_functions$miss_value_filter$value(
      FUNC_data = master_list$data$sorted[[idx_data]] %>%
        filter(sample_type == idx_sample_type),
      FUNC_metabolite_list = master_list$data$sorted[[idx_data]] %>%
        select(!contains("sample")) %>% names(),
      FUNC_IS_tag = "SIL",
      FUNC_OPTION_missing_value_threshold_sample = 0.50, #decimal % of missing value threshold before sample is removed from dataset
      FUNC_OPTION_missing_value_threshold_feature = 0.50, #decimal % of missing value threshold before feature is removed from dataset
      FUNC_OPTION_intensity_threshold = 5000)
    
  }
  
  #bind fail sample list
  master_list$process_lists$missing_value_filter[[idx_data]]$sample_fail_list <- c(
    master_list$process_lists$missing_value_filter[[idx_data]]$sample$mv_samples_fail,
    master_list$process_lists$missing_value_filter[[idx_data]]$qc$mv_samples_fail) %>%
    unique()
  
  #bind fail feature list
  master_list$process_lists$missing_value_filter[[idx_data]]$feature_fail_list <- c(
    master_list$process_lists$missing_value_filter[[idx_data]]$sample$mv_features_fail,
    master_list$process_lists$missing_value_filter[[idx_data]]$qc$mv_features_fail) %>%
    unique()
  
  #create filtered dataset per plate for next phase
  master_list$data$missing_value_filter[[idx_data]] <- master_list$data$sorted[[idx_data]] %>%
    filter(!sample_name %in% master_list$process_lists$missing_value_filter[[idx_data]]$sample_fail_list) %>% #select only pass samples
    select(-all_of(master_list$process_lists$missing_value_filter[[idx_data]]$feature_fail_list)) #select only pass features
  
  
  
  
  # TABLE: summary table of missing value filter ----------------------------
  
  master_list$summary_tables$missing_value_filter_summary <- master_list$summary_tables$missing_value_filter_summary %>%
    bind_rows(
      bind_cols("batch" = idx_data,
                "samples pre-filter" = master_list$data$sorted[[idx_data]] %>% nrow(),
                "samples post-filter" = master_list$data$missing_value_filter[[idx_data]] %>% nrow(),
                "samples removed" = master_list$data$sorted[[idx_data]] %>% nrow() -
                  master_list$data$missing_value_filter[[idx_data]] %>% nrow(),
                "qc samples remaining" = master_list$data$missing_value_filter[[idx_data]] %>% filter(grepl('qc', sample_type)) %>% nrow(), #report number of QCs remaining in dataset
                "study samples remaining" = master_list$data$missing_value_filter[[idx_data]] %>% filter(grepl('sample', sample_type)) %>% nrow(), #report number of samples in dataset
                "features pre-filter" = master_list$data$sorted[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL")) %>% ncol(),
                "features post-filter" = master_list$data$missing_value_filter[[idx_data]] %>% select(-contains("sample"))  %>% select(-contains("SIL")) %>% ncol(),
                "features removed" = master_list$data$sorted[[idx_data]] %>% select(-contains("sample"))  %>% select(-contains("SIL")) %>% ncol() -
                  master_list$data$missing_value_filter[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL")) %>% ncol(),
                "internal standards pre-filter" = master_list$data$sorted[[idx_data]] %>% select(contains("SIL")) %>% ncol(),
                "internal standards post-filter" = master_list$data$missing_value_filter[[idx_data]]  %>% select(contains("SIL")) %>% ncol(),
                "internal standards removed" = master_list$data$sorted[[idx_data]]  %>% select(contains("SIL")) %>% ncol() -
                  master_list$data$missing_value_filter[[idx_data]] %>% select(contains("SIL")) %>% ncol(),
                "zero values remaining" = length(which(master_list$data$missing_value_filter[[idx_data]] %>% select(!contains("sample"))==0)), #find 0 values
                "NA values remaining" = length(which(is.na(as.matrix(master_list$data$missing_value_filter[[idx_data]] %>% select(!contains("sample")))))), #find NAs
                "NaN values remaining" = length(which(is.nan(as.matrix(master_list$data$missing_value_filter[[idx_data]] %>% select(!contains("sample")))))) # find NANs
      ))
  
  #create list of SIL that fail
  master_list$process_lists$missing_value_filter$failed_SIL <- c(master_list$process_lists$missing_value_filter$failed_SIL, 
                                                                 master_list$process_lists$missing_value_filter[[idx_data]]$feature_fail_list[grep("SIL", master_list$process_lists$missing_value_filter[[idx_data]]$feature_fail_list)]) %>%
    unique()
  
  #remove plate where > 1/3rds of QCs have failed the missing value filter
  #Step 1 - find qcs that pass plate:
  qc_pass <- master_list$data$missing_value_filter[[idx_data]] %>% filter(grepl('qc', sample_type)) %>% nrow()
  total_qc <- master_list$data$sorted[[idx_data]] %>% filter(grepl('qc', sample_type)) %>% nrow()
  
  
  if(qc_pass < floor(total_qc * 0.66)){
    #save fail plate for report
    master_list$summary_tables$missing_value_qc_fail <- master_list$summary_tables$missing_value_qc_fail %>%
      bind_rows(
        master_list$summary_tables$missing_value_filter_summary %>% filter(batch == idx_data)
      )
    
    #remove plate from future steps in process
    master_list$data$missing_value_filter[[idx_data]] <- NULL
  }
  
}

master_list$summary_tables$missing_value_filter_summary <- rbind(master_list$summary_tables$missing_value_filter_summary ,
                                                                 c("total project",
                                                                   sum(master_list$summary_tables$missing_value_filter_summary$`samples pre-filter`),
                                                                   sum(master_list$summary_tables$missing_value_filter_summary$`samples post-filter`),
                                                                   sum(master_list$summary_tables$missing_value_filter_summary$`samples removed`),
                                                                   sum(master_list$summary_tables$missing_value_filter_summary$`qc samples remaining`),
                                                                   sum(master_list$summary_tables$missing_value_filter_summary$`study samples remaining`),
                                                                   max(master_list$summary_tables$missing_value_filter_summary$`features pre-filter`),
                                                                   min(master_list$summary_tables$missing_value_filter_summary$`features post-filter`),
                                                                   max(master_list$summary_tables$missing_value_filter_summary$`features removed`),
                                                                   max(master_list$summary_tables$missing_value_filter_summary$`internal standards pre-filter`),
                                                                   min(master_list$summary_tables$missing_value_filter_summary$`internal standards post-filter`),
                                                                   max(master_list$summary_tables$missing_value_filter_summary$`internal standards removed`),
                                                                   sum(master_list$summary_tables$missing_value_filter_summary$`zero values remaining`),
                                                                   sum(master_list$summary_tables$missing_value_filter_summary$`NA values remaining`),
                                                                   sum(master_list$summary_tables$missing_value_filter_summary$`NaN values remaining`)))


#set empty table if all plates pass mising value qc threshold

if (length(master_list$summary_tables$missing_value_qc_fail) == 0){master_list$summary_tables$missing_value_qc_fail <- "No plates were removed"} 

# PROCESS: select data features that are common for all plates ------------
master_list$data$common_metabolite_filter <- list()

#step 1: find common features
temp_common_features <- names(master_list$data$missing_value_filter[[1]])
for(idx_data in names(master_list$data$missing_value_filter)){
  temp_common_features <- intersect(temp_common_features,
                                    names(master_list$data$missing_value_filter[[idx_data]]))
}

for(idx_data in names(master_list$data$missing_value_filter)){
  master_list$data$common_metabolite_filter[[idx_data]] <- master_list$data$missing_value_filter[[idx_data]] %>%
    select(all_of(temp_common_features))
}


# PCA: post-missing value filter ------------------------------------------

#create empty list for results
master_list$pca_analysis$missing_value_filter <- list()
#run pca loop color by QC/sample
master_list$pca_analysis$missing_value_filter$sample_qc <- master_list$environment$user_functions$pca$value(
  FUNC_data = master_list$data$common_metabolite_filter %>% bind_rows(),
  FUNC_metabolite_list = master_list$data$common_metabolite_filter %>% bind_rows() %>%
    select(-contains("sample")) %>% names(),
  FUNC_colour_by = "sample_type_factor",
  FUNC_plot_label = "sample_name", 
  FUNC_scaling = "Pareto",
  FUNC_title = paste0(master_list$project_details$project_name),
  FUNC_project_colours = c("steelblue2", "white"),
  FUNC_option_invert_y = FALSE,
  FUNC_option_invert_x = FALSE,
  FUNC_option_plot_qc = TRUE
)


#run pca loop color by plate
master_list$pca_analysis$missing_value_filter$plate <- master_list$environment$user_functions$pca$value(
  FUNC_data = master_list$data$common_metabolite_filter %>% bind_rows(),
  FUNC_metabolite_list = master_list$data$common_metabolite_filter %>% bind_rows() %>%
    select(-contains("sample")) %>% names(),
  FUNC_colour_by = "sample_plate_id",
  FUNC_plot_label = "sample_name", 
  FUNC_scaling = "Pareto",
  FUNC_title = paste0(master_list$project_details$project_name),
  FUNC_project_colours = c(viridisLite::magma(n = length(master_list$project_details$mzml_plate_list))),
  FUNC_option_invert_y = FALSE,
  FUNC_option_invert_x = FALSE,
  FUNC_option_plot_qc = TRUE
)

# PROCESS: imputation of remaining missing values -----------------------------------------------------

#Imputation of the remaining zero value and missing data 
#Imputation is completed using x/2, where x is minimum intensity of that feature in the batch

master_list$data$impute <- list()
master_list$summary_tables$impute_table <- list()

for(idx_data in names(master_list$data$common_metabolite_filter)){
  master_list$data$impute[[idx_data]] <- master_list$environment$user_functions$impute_data$value(
    FUNC_data = master_list$data$common_metabolite_filter[[idx_data]],
    FUNC_metabolite_list = master_list$data$common_metabolite_filter[[idx_data]] %>% 
      select(-contains("sample")) %>% names(),
    FUNC_option_impute_missing_data = TRUE)
  
  # TABLE: summary table of imputed data ----------------------------
  
  master_list$summary_tables$impute_table <- master_list$summary_tables$impute_table %>%
    bind_rows(
      bind_cols("batch" = idx_data,
                "zero vaules pre-imputation" = length(which(master_list$data$common_metabolite_filter[[idx_data]] %>% select(!contains("sample"))==0)),
                "NA values pre-imputation" = length(which(is.na(as.matrix(master_list$data$common_metabolite_filter[[idx_data]] %>% select(!contains("sample")))))),
                "NaN values pre-imputation" = length(which(is.nan(as.matrix(master_list$data$common_metabolite_filter[[idx_data]] %>% select(!contains("sample")))))), # find NANs
                "zero vaules post-imputation" = length(which(master_list$data$impute[[idx_data]] %>% select(!contains("sample"))==0)),
                "NA values post-imputation" = length(which(is.na(as.matrix(master_list$data$impute[[idx_data]] %>% select(!contains("sample")))))),
                "NaN values post-imputation" = length(which(is.nan(as.matrix(master_list$data$impute[[idx_data]] %>% select(!contains("sample")))))), 
                
      ))
}

master_list$summary_tables$impute_table <- rbind(master_list$summary_tables$impute_table ,
                                                 c("total project",
                                                   sum(master_list$summary_tables$impute_table$`zero vaules pre-imputation`),
                                                   sum(master_list$summary_tables$impute_table$`NA values pre-imputation`),
                                                   sum(master_list$summary_tables$impute_table$`NaN values pre-imputation`),
                                                   sum(master_list$summary_tables$impute_table$`zero vaules post-imputation`),
                                                   sum(master_list$summary_tables$impute_table$`NA values post-imputation`),
                                                   sum(master_list$summary_tables$impute_table$`NaN values post-imputation`)
                                                 ))



# PROCESS: Response  ratio and concentration value  ------------------------------------------------------
#* Calculation of target metabolite/stable isotope labelled (SIL) internal standard ratio, using predefined target metabolite/internal standard pairs
#* Conversion of response ratio to concentration values using single point calibration


#set empty list to store output data
master_list$data$concentration <- list()
master_list$summary_tables$concentration_summary <- list()
master_list$templates <- list()
master_list$templates$SIL_guide <- read_csv(
  file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_lipid_mrm_template.csv",
  show_col_types = FALSE) %>%
  clean_names()
master_list$templates$conc_guide <- read_csv(
  file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_SIL_batch_103.csv", 
  show_col_types = FALSE) %>% 
  clean_names()

for(idx_data in names(master_list$data$impute)){
  master_list$data$concentration[[idx_data]] <- master_list$environment$user_functions$conc_calc$value(
    FUNC_data = master_list$data$impute[[idx_data]],
    FUNC_metabolite_list = master_list$data$impute[[idx_data]] %>% 
      select(!contains("sample")) %>% names(),
    FUNC_SIL_guide = master_list$templates$SIL_guide,
    FUNC_conc_guide = master_list$templates$conc_guide) %>%
    #ensure no SIL metabolites remain in dataset
    select(!contains("SIL"))
  
  # TABLE: summary table of data following response ratio and concentration calculation ----------------------------
  
  master_list$summary_tables$concentration_summary <- master_list$summary_tables$concentration_summary %>%
    bind_rows(
      bind_cols("batch" = idx_data,
                "total samples" = master_list$data$concentration[[idx_data]] %>% nrow(),
                "qc samples" = master_list$data$concentration[[idx_data]] %>% filter(grepl('qc', sample_type)) %>% nrow(), #report number of QCs remaining in dataset
                "study samples" = master_list$data$concentration[[idx_data]] %>% filter(grepl('sample', sample_type)) %>% nrow(), #report number of samples in dataset
                "features" = master_list$data$concentration[[idx_data]] %>% select(!contains("sample")) %>% select(-contains("SIL")) %>% ncol(),
                "zero values remaining" = length(which(master_list$data$concentration[[idx_data]] %>% select(!contains("sample"))==0)), #find 0 values
                "NA values remaining" = length(which(is.na(as.matrix(master_list$data$concentration[[idx_data]] %>% select(!contains("sample")))))), #find NAs
                "NaN values remaining" = length(which(is.nan(as.matrix(master_list$data$concentration[[idx_data]] %>% select(!contains("sample")))))) # find NANs
      ))
  
}

master_list$summary_tables$concentration_summary <- rbind(master_list$summary_tables$concentration_summary,
                                                          c("total project",
                                                            sum(master_list$summary_tables$concentration_summary$`total samples`),
                                                            sum(master_list$summary_tables$concentration_summary$`qc samples`),
                                                            sum(master_list$summary_tables$concentration_summary$`study samples`),
                                                            max(master_list$summary_tables$concentration_summary$features),
                                                            sum(master_list$summary_tables$concentration_summary$`zero values remaining`),
                                                            sum(master_list$summary_tables$concentration_summary$`NA values remaining`),
                                                            sum(master_list$summary_tables$concentration_summary$`NaN values remaining`)
                                                          ))


#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))


# PCA: response ratio and concentration calculation -----------------------

#create empty list for results
master_list$pca_analysis$concentration <- list()
#run pca loop color by QC/sample
master_list$pca_analysis$concentration$sample_qc <- master_list$environment$user_functions$pca$value(
  FUNC_data = master_list$data$concentration %>% bind_rows(),
  FUNC_metabolite_list = master_list$data$concentration %>% bind_rows() %>%
    select(-contains("sample")) %>% names(),
  FUNC_colour_by = "sample_type_factor",
  FUNC_plot_label = "sample_name", 
  FUNC_scaling = "Pareto",
  FUNC_title = paste0(master_list$project_details$project_name),
  FUNC_project_colours = c("steelblue2", "white"),
  FUNC_option_invert_y = FALSE,
  FUNC_option_invert_x = FALSE,
  FUNC_option_plot_qc = TRUE
)


#run pca loop color by plate
master_list$pca_analysis$concentration$plate <- master_list$environment$user_functions$pca$value(
  FUNC_data = master_list$data$concentration %>% bind_rows(),
  FUNC_metabolite_list = master_list$data$concentration %>% bind_rows() %>%
    select(-contains("sample")) %>% names(),
  FUNC_colour_by = "sample_plate_id",
  FUNC_plot_label = "sample_name", 
  FUNC_scaling = "Pareto",
  FUNC_title = paste0(master_list$project_details$project_name),
  FUNC_project_colours = c(viridisLite::magma(n = length(master_list$project_details$mzml_plate_list))),
  FUNC_option_invert_y = FALSE,
  FUNC_option_invert_x = FALSE,
  FUNC_option_plot_qc = TRUE
)


# PLOT: pca filter runorder vs pc -----------------------------------------
#prepare data 

master_list$data$bind_plates$pre_statTarget <- master_list$data$concentration %>%
  bind_rows() %>% 
  add_column(sample_idx = seq(1:nrow(bind_rows(master_list$data$concentration))),
             sample_fail = (bind_rows(master_list$data$concentration))[["sample_type"]],
             .before = 1)

#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))

# TABLE: Pre-batch correction summary table -------------------------------

prelipid_stDev <- colSds(master_list$data$bind_plates$pre_statTarget %>% filter(sample_type == "qc") %>% select(!contains("sample")) %>% as.matrix())
prelipid_means <- colMeans2(master_list$data$bind_plates$pre_statTarget %>% filter(sample_type == "qc") %>% select(!contains("sample")) %>% as.matrix())
prelipid_RSD <- (100*prelipid_stDev)/prelipid_means

master_list$summary_tables$batch_correction_overview <- list()
master_list$summary_tables$batch_correction_overview <- bind_cols(
  "data" = "pre-statTarget signal correction",
  "total samples" = nrow(master_list$data$bind_plates$pre_statTarget),
  "qc samples" = nrow(master_list$data$bind_plates$pre_statTarget %>% filter(sample_type == "qc")),
  "study samples" = nrow(master_list$data$bind_plates$pre_statTarget %>% filter(sample_type == "sample")),
  "total features" = ncol(master_list$data$bind_plates$pre_statTarget %>% select(!contains("sample"))),
  "features < 30% qcRSD" = length(which(prelipid_RSD < 30)),
  "features < 20% qcRSD" = length(which(prelipid_RSD < 20)),
  "features < 10% qcRSD" = length(which(prelipid_RSD < 10)),
  "zero values" = length(which(master_list$data$bind_plates$pre_statTarget %>% select(!contains("sample"))==0)),
  "NA values remaining" = length(which(is.na(as.matrix(master_list$data$bind_plates$pre_statTarget %>% select(!contains("sample")))))), #find NAs
  "NaN values remaining" = length(which(is.nan(as.matrix(master_list$data$bind_plates$pre_statTarget %>% select(!contains("sample"))))))#find 0 values
)


# PROCESS: signal drift and batch correct ---------------------------------

#* Data from each individual batch undergoes signal drift correction using statTarget package (https://stattarget.github.io/)
#* This is performed within individual batches at this point to evaluate the performance of each batch
#* 
#list for storing signal drift corrected data (per project)
master_list$data$bind_plates$post_statTarget <- list()
master_list$summary_tables$statTarget_corrected <- list()

#create batch correction directory
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/batch_correction"))){
  dir.create(paste0(master_list$project_details$project_dir, "/data/batch_correction"))
}

#run batch correction PER PROJECT 
master_list$data$bind_plates$post_statTarget <- master_list$environment$user_functions$signal_correct$value(
  FUNC_project_directory = paste0(master_list$project_details$project_dir,
                                  "/data/batch_correction"),
  FUNC_data = master_list$data$bind_plates$pre_statTarget,
  FUNC_metabolite_list = master_list$data$bind_plates$pre_statTarget %>%
    select(-contains("sample")) %>% names(),
  FUNC_header_sample_id = "sample_name",
  FUNC_header_batch = "sample_plate_id",
  FUNC_header_sample_type = "sample_type",
  FUNC_header_run_order = "sample_idx",
  FUNC_option_method = "RF",
  FUNC_option_coCV = 30
)

# TABLE: Post-batch correction summary table -------------------------------

postlipid_stDev <- colSds(master_list$data$bind_plates$post_statTarget %>% filter(sample_type == "qc") %>% select(!contains("sample")) %>% as.matrix())
postlipid_means <- colMeans2(master_list$data$bind_plates$post_statTarget %>% filter(sample_type == "qc") %>% select(!contains("sample")) %>% as.matrix())
postlipid_RSD <- (100*postlipid_stDev)/postlipid_means

master_list$summary_tables$batch_correction_overview <- bind_rows(master_list$summary_tables$batch_correction_overview,
                                                                  bind_cols(
                                                                    "data" = "post-statTarget signal correction",
                                                                    "total samples" = nrow(master_list$data$bind_plates$post_statTarget),
                                                                    "qc samples" = nrow(master_list$data$bind_plates$post_statTarget %>% filter(sample_type == "qc")),
                                                                    "study samples" = nrow(master_list$data$bind_plates$post_statTarget %>% filter(sample_type == "sample")),
                                                                    "total features" = ncol(master_list$data$bind_plates$post_statTarget %>% select(!contains("sample"))),
                                                                    "features < 30% qcRSD" = length(which(postlipid_RSD < 30)),
                                                                    "features < 20% qcRSD" = length(which(postlipid_RSD < 20)),
                                                                    "features < 10% qcRSD" = length(which(postlipid_RSD < 10)),
                                                                    "zero values" = length(which(master_list$data$bind_plates$post_statTarget %>% select(!contains("sample"))==0)),
                                                                    "NA values remaining" = length(which(is.na(as.matrix(master_list$data$bind_plates$post_statTarget %>% select(!contains("sample")))))), #find NAs
                                                                    "NaN values remaining" = length(which(is.nan(as.matrix(master_list$data$bind_plates$post_statTarget %>% select(!contains("sample"))))))#find 0 values
                                                                  ))


#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))


# PCA: statTarget corrected data ------------------------------------------

#create empty list for results
master_list$pca_analysis$statTarget_corrected <- list()
#run pca loop color by QC/sample
master_list$pca_analysis$statTarget_corrected$sample_qc <- master_list$environment$user_functions$pca$value(
  FUNC_data = master_list$data$bind_plates$post_statTarget,
  FUNC_metabolite_list = master_list$data$bind_plates$post_statTarget %>%
    select(-contains("sample")) %>% names(),
  FUNC_colour_by = "sample_type_factor",
  FUNC_plot_label = "sample_name", 
  FUNC_scaling = "Pareto",
  FUNC_title = paste0(master_list$project_details$project_name),
  FUNC_project_colours = c("steelblue2", "white"),
  FUNC_option_invert_y = FALSE,
  FUNC_option_invert_x = FALSE,
  FUNC_option_plot_qc = TRUE
)

#run pca loop color by plate
master_list$pca_analysis$statTarget_corrected$plate <- master_list$environment$user_functions$pca$value(
  FUNC_data = master_list$data$bind_plates$post_statTarget,
  FUNC_metabolite_list = master_list$data$bind_plates$post_statTarget %>%
    select(-contains("sample")) %>% names(),
  FUNC_colour_by = "sample_plate_id",
  FUNC_plot_label = "sample_name", 
  FUNC_scaling = "Pareto",
  FUNC_title = paste0(master_list$project_details$project_name),
  FUNC_project_colours = c(viridisLite::magma(n = length(master_list$project_details$mzml_plate_list))),
  FUNC_option_invert_y = FALSE,
  FUNC_option_invert_x = FALSE,
  FUNC_option_plot_qc = TRUE
)



# PLOT: run order vs pc variation ----------------------------------------------------------

master_list$pc_runorder_plots$pre_statTarget <- list()
master_list$pc_runorder_plots$pre_statTarget <- master_list$environment$user_functions$pc_run_plot$value(
  FUNC_data = master_list$data$bind_plates$pre_statTarget,
  FUNC_metabolite_list = master_list$data$bind_plates$pre_statTarget %>%
    select(-contains("sample")) %>% names(),
  FUNC_colour_by = "sample_type_factor_rev",
  FUNC_plot_label = "sample_name",
  FUNC_scaling = "Pareto",
  FUNC_title = paste0(master_list$project_details$project_name),
  FUNC_project_colours = c("white", "steelblue2"),
  FUNC_option_point_size = 3,
  FUNC_option_plot_qc = TRUE
)


master_list$pc_runorder_plots$post_statTarget <- list()
master_list$pc_runorder_plots$post_statTarget <- master_list$environment$user_functions$pc_run_plot$value(
  FUNC_data = master_list$data$bind_plates$post_statTarget,
  FUNC_metabolite_list = master_list$data$bind_plates$post_statTarget %>%
    select(-contains("sample")) %>% names(),
  FUNC_colour_by = "sample_type_factor_rev",
  FUNC_plot_label = "sample_name",
  FUNC_scaling = "Pareto",
  FUNC_title = paste0(master_list$project_details$project_name),
  FUNC_project_colours = c("white", "steelblue2"),
  FUNC_option_point_size = 3,
  FUNC_option_plot_qc = TRUE
)



#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))



# PROCESS: save rda output ------------------------------------------------

save(master_list,
     file = paste0(
       master_list$project_details$project_dir,
       "/data/rda/", Sys.Date(), 
       "_qcCheckeR_", 
       master_list$project_details$project_name, 
       ".rda"))

# PROCESS: save final csv output ------------------------------------------------

write_csv(x = master_list$data$bind_plates$post_statTarget,
          file =paste0(
            master_list$project_details$project_dir,
            "/html_report/", Sys.Date(), 
            "_", 
            master_list$project_details$project_name, 
            "_qcCheckeRv3.21.csv"))

# PROCESS: render html report ---------------------------------------------

fileConn<-file(paste0(master_list$project_details$project_dir, "/html_report/lipid_exploreR_report_templatev3.21.R"))
writeLines(httr::GET(url = paste0(master_list$project_details$github_master_dir, "/templates/TEMPLATE_lipidExploreR_reportv3.21.R")) %>%
             httr::content(as = "text"), fileConn)
close(fileConn)


rmarkdown::render(input = paste0(master_list$project_details$project_dir, "/html_report/lipid_exploreR_report_templatev3.21.R"),
                  output_format = "html_document",
                  output_dir = paste0(master_list$project_details$project_dir, "/html_report"),
                  output_file = paste0(Sys.Date(), "_", master_list$project_details$project_name, "_lipidExploreR_qcCheckeR_reportv3.21.html")
)

browseURL(url = paste0(master_list$project_details$project_dir, 
                       "/html_report/",
                       Sys.Date(), "_", master_list$project_details$project_name, "_lipidExploreR_qcCheckeR_reportv3.21.html")
)
