# PROJECT SETUP: Welcome section and project set up --------------------------------------

#set version of lipidExploreR used
master_list$project_details$qcCheckR_version <- "3.3"
master_list$summary_tables$project_summary$value[which(master_list$summary_tables$project_summary$`Project detail` == "lipidExploreR version")] <- "3.3"

#welcome messages
if(!exists("master_list")){
  dlg_message("Welcome to lipid qc exploreR! :-)", type = 'ok'); dlg_message("Please run lipid SkylineR notebook prior to running this notebook", type = 'ok'); dlg_message("Now open master_list.rda file produced by SkylineR", type = 'ok')
  # load rda file
  load(file = file.choose())
}

# PROJECT SETUP: source LGW github functions --------------------------------------
master_list$project_details$github_master_dir = "~/Documents/GitHub/targeted_lipid_exploreR_v3/"
#pca scores plot function
master_list$environment$user_functions$pca <- source(paste0(master_list$project_details$github_master_dir,
                                                            "/functions/FUNC_lipidExploreR_PCA_ggplot.R"))
#missing value filter
master_list$environment$user_functions$miss_value_filter <- source(paste0(master_list$project_details$github_master_dir,
                                                                          "/functions/FUNC_lipidExploreR_missing_value_filter.R"))
#impute data
master_list$environment$user_functions$impute_data <- source(paste0(master_list$project_details$github_master_dir,
                                                                    "/functions/FUNC_lipidExploreR_impute_data.R"))
#concentration calculator
master_list$environment$user_functions$conc_calc <-  source(paste0(master_list$project_details$github_master_dir,
                                                                   "/functions/FUNC_lipidExploreR_conc_calculator_v3.3.R"))
#pca qc filter
master_list$environment$user_functions$pca_filter <- source(paste0(master_list$project_details$github_master_dir,
                                                                   "/functions/FUNC_lipidExploreR_pca_filter.R"))
#signal/batch correction
master_list$environment$user_functions$signal_correct <- source(paste0(master_list$project_details$github_master_dir,
                                                                       "/functions/FUNC_lipidExploreR_signal_drift_correct.R"))
#run order vs PC plots
master_list$environment$user_functions$pc_run_plot <- source(paste0(master_list$project_details$github_master_dir,
                                                                    "/functions/FUNC_lipidExploreR_PCA_runOrder_ggplot.R"))
#control chart check
master_list$environment$user_functions$control_chart <- source(paste0(master_list$project_details$github_master_dir,
                                                                    "/functions/FUNC_lipidExploreR_controlChart.R"))

# PROJECT SETUP: plot color/fill/shape/size --------------------------------------

#set plots colours
master_list$project_details$plot_fill <-  c("sample" = "white",
                                               "ltr" = "steelblue2",
                                               "pqc" = "darkorange",
                                               "sltr" = "purple1",
                                               "vltr" = "seagreen")
#set plots colours
master_list$project_details$plot_colour <-  c("sample" = "black",
                                              "ltr" = "black",
                                              "pqc" = "black",
                                              "sltr" = "black",
                                              "vltr" = "black")
#set preferred LTR type for subsequent QC filters
master_list$project_details$plot_colour[which(tolower(master_list$project_details$qc_type) == tolower(names(master_list$project_details$plot_colour)))] <- "red"

#set plot shapes
master_list$project_details$plot_shape <- c("sample" = 21,
                                             "ltr" = 21,
                                             "sltr" = 21,
                                             "vltr" = 21,
                                            "pqc" = 21)
#set preferred LTR type for subsequent QC filters
master_list$project_details$plot_shape[which(tolower(master_list$project_details$qc_type) == tolower(names(master_list$project_details$plot_colour)))] <- 23
#set plot size
master_list$project_details$plot_size <- c("sample" = 2,
                                            "ltr" = 2,
                                            "sltr" = 2,
                                            "vltr" = 2,
                                           "pqc" = 2)
#set preferred LTR type for subsequent QC filters
master_list$project_details$plot_size[which(tolower(master_list$project_details$qc_type) == tolower(names(master_list$project_details$plot_colour)))] <- 3

# PROJECT SETUP: templates/guides --------------------------------------
#prepare template/guides for concentration for calculation
master_list$templates <- list()
#if using sciex lipidizer internal standards
if(master_list$project_details$is_ver == "v1"){
  master_list$templates$SIL_guide <- read_csv(
    file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_lipid_mrm_template_v1.csv",
    show_col_types = FALSE) %>%
    clean_names()
  master_list$templates$conc_guide <- read_csv(
    file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_SIL_batch_103.csv", 
    show_col_types = FALSE) %>% 
    clean_names()
}

#if using ultra splash mix (ANPC method v2)
if(master_list$project_details$is_ver == "v2"){
  master_list$templates$SIL_guide <- read_csv(
    file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_lipid_mrm_template_v2.csv",
    show_col_types = FALSE) %>%
    clean_names()
  master_list$templates$conc_guide <- read_csv(
    file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_SIL_batch_Ultimate_2023-03-06.csv", 
    show_col_types = FALSE) %>% 
    clean_names()
}

# PHASE 1: NO FILTERING ------------------------------------

#DATA PROCESS: transpose data to standard metabolomics structure (features in columns, samples in rows) ---------------------------------------
master_list$data$area_transposed <- list()

for(idx_data in master_list$project_details$mzml_plate_list){
  master_list$data$area_transposed[[idx_data]] <- pivot_wider(
    data = master_list$data$skyline_report %>%
      filter(file_name %in% names(master_list$data$mzR[[idx_data]])),
    id_cols = file_name,
    names_from = molecule_name,
    values_from = area
  ) %>%
    rename(sample_name = file_name)
  
  #remove file extenstion (.mzML from sample_name)
  master_list$data$area_transposed[[idx_data]]$sample_name <- sub(".mzML", "", master_list$data$area_transposed[[idx_data]]$sample_name)
  
  
  #make numeric 
  master_list$data$area_transposed[[idx_data]][,-1] <-
    sapply(master_list$data$area_transposed[[idx_data]][,-1], 
           as.numeric) %>% 
    as_tibble()
  
}


#DATA PROCESS: Sort by run order and add annotation data ------------------------------- 
#list for storing concentration data area_sorted by run order
# Chunk also creates a data summary
#   -> number of samples
#   -> number of features
#   -> missing values
#   -> NA values
#   -> NAN values
master_list$summary_tables$area_summary <- list()
master_list$project_details$run_orders <- list()
master_list$data$area_sorted <- list()
#run loop
for (idx_data in names(master_list$data$area_transposed)){
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
  master_list$project_details$run_orders[[idx_data]]$sample_type <- NA
  
  # set sample_type using mutate
  master_list$project_details$run_orders[[idx_data]] <- master_list$project_details$run_orders[[idx_data]]  %>%
    mutate(
      sample_type = ifelse(str_detect(string = sample_name, pattern = "(?i)pqc"), "pqc",
                           ifelse(str_detect(string = sample_name, pattern = "(?i)vltr"), "vltr",
                                  ifelse(str_detect(string = sample_name, pattern = "(?i)sltr"), "sltr",
                                         ifelse(str_detect(string = sample_name, pattern = "(?i)ltr"), "ltr",
                                                ifelse(str_detect(string = sample_name, pattern = "(?i)blank"), "blank",
                                                       ifelse(str_detect(string = sample_name, pattern = "(?i)istds"), "istds",
                                                              ifelse(str_detect(string = sample_name, pattern = "(?i)cond"), "conditioning",
                                                                     "sample")
                                                         )))))))
    
    #sort area_transposed data by run order and then remove conditioning runs
    master_list$data$area_sorted[[idx_data]] <- master_list$project_details$run_orders[[idx_data]] %>%
      left_join(master_list$data$area_transposed[[idx_data]], by = "sample_name") %>% 
      filter(sample_type == "pqc" | sample_type == "sample" | sample_type == "ltr" | sample_type == "sltr" | sample_type == "vltr") %>%
      arrange(sample_timestamp) %>%
      add_column(sample_run_index = c(1:nrow(.)), .before = 1)
    
    #add_factor column for plotting
    master_list$data$area_sorted[[idx_data]] <- master_list$data$area_sorted[[idx_data]] %>%
      add_column(sample_type_factor = master_list$data$area_sorted[[idx_data]]$sample_type %>% 
                   factor(
                     levels = c("sample", master_list$project_details$qc_type, "ltr", "pqc", "vltr", "sltr")[!duplicated(c("sample", master_list$project_details$qc_type, "ltr", "pqc", "vltr", "sltr"))], 
                     ordered = TRUE),
                 .after = "sample_type") %>% 
      add_column(
        sample_type_factor_rev = factor(
          .$sample_type_factor,
          levels = rev(levels(.$sample_type_factor)), 
          ordered = TRUE),
        .after = "sample_type_factor")
    
    #convert sample type to "qc" for selected qc type and "sample" for everything else
    master_list$data$area_sorted[[idx_data]][["sample_type"]][-which(tolower(master_list$project_details$qc_type) == tolower(master_list$data$area_sorted[[idx_data]][["sample_type"]]))] <- "sample"
    master_list$data$area_sorted[[idx_data]][["sample_type"]][which(tolower(master_list$project_details$qc_type) == tolower(master_list$data$area_sorted[[idx_data]][["sample_type"]]))] <- "qc"
  
    # SUMMARY TABLE: skyline peak area missing data summary ---------------------------------
    master_list$summary_tables$area_summary <- master_list$summary_tables$area_summary %>%   
      bind_rows(
        bind_cols(
          "batch" = idx_data,
          "total samples" = nrow(master_list$data$area_sorted[[idx_data]]),
          "VLTR samples" = length(which(master_list$data$area_sorted[[idx_data]]$sample_type_factor == "vltr")), #report number of VLTRs in dataset
          "SLTR samples" = length(which(master_list$data$area_sorted[[idx_data]]$sample_type_factor == "sltr")), #report number of SLTRs in dataset
          "LTR samples" = length(which(master_list$data$area_sorted[[idx_data]]$sample_type_factor == "ltr")), #report number of LTRs in dataset
          "PQC samples" = length(which(master_list$data$area_sorted[[idx_data]]$sample_type_factor == "pqc")), #report number of PQC in dataset
          "study samples"= nrow(master_list$data$area_sorted[[idx_data]])-
            length(which(master_list$data$area_sorted[[idx_data]]$sample_type_factor == "vltr"))- 
            length(which(master_list$data$area_sorted[[idx_data]]$sample_type_factor == "sltr")) -
            length(which(master_list$data$area_sorted[[idx_data]]$sample_type_factor == "ltr")) -
            length(which(master_list$data$area_sorted[[idx_data]]$sample_type_factor == "pqc")),#report number of study samples in dataset
          "total lipid targets" = ncol(master_list$data$area_sorted[[idx_data]] %>% 
                                    select(-contains("sample")) %>%
                                    select(-contains("SIL"))
          ),
          "total SIL Int.Stds" = ncol(master_list$data$area_sorted[[idx_data]] %>% 
                                                  select(-contains("sample")) %>%
                                                  select(contains("SIL"))
          ),
          "zero values (lipid targets)" = length(which(
              master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL"))==0
              )),
          "zero values (SIL Int.Stds)" = length(which(
            master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample")) %>% select(contains("SIL"))==0
          )),
          "NA values (lipid targets)" = length(which(
            is.na(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL"))
                            )))),
          "NA values (SIL Int.Stds)" = length(which(
            is.na(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample")) %>% select(contains("SIL"))
            )))),
          "NaN values (lipid targets)" = length(which(
            is.nan(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL"))
            )))),
          "NaN values (SIL Int.Stds)" = length(which(
            is.nan(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample")) %>% select(contains("SIL"))
            )))),
          "Inf values (lipid targets)" = length(which(
            is.infinite(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL"))
            )))),
          "Inf values (SIL Int.Stds)" = length(which(
            is.infinite(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample")) %>% select(contains("SIL"))
            ))))
          )
      )
  }
  
  #add total row for project to summary table
  master_list$summary_tables$area_summary <- rbind(master_list$summary_tables$area_summary,
                                                              c("total project",
                                                                sum(master_list$summary_tables$area_summary$`total samples`),
                                                                sum(master_list$summary_tables$area_summary$`VLTR samples`),
                                                                sum(master_list$summary_tables$area_summary$`SLTR samples`),
                                                                sum(master_list$summary_tables$area_summary$`LTR samples`),
                                                                sum(master_list$summary_tables$area_summary$`PQC samples`),
                                                                sum(master_list$summary_tables$area_summary$`study samples`),
                                                                max(master_list$summary_tables$area_summary$`total lipid targets`),
                                                                max(master_list$summary_tables$area_summary$`total SIL Int.Stds`),
                                                                sum(master_list$summary_tables$area_summary$`zero values (lipid targets)`),
                                                                sum(master_list$summary_tables$area_summary$`zero values (SIL Int.Stds)`),
                                                                sum(master_list$summary_tables$area_summary$`NA values (lipid targets)`),
                                                                sum(master_list$summary_tables$area_summary$`NA values (SIL Int.Stds)`),
                                                                sum(master_list$summary_tables$area_summary$`NaN values (lipid targets)`),
                                                                sum(master_list$summary_tables$area_summary$`NaN values (SIL Int.Stds)`),
                                                                sum(master_list$summary_tables$area_summary$`Inf values (lipid targets)`),
                                                                sum(master_list$summary_tables$area_summary$`Inf values (SIL Int.Stds)`)
                                                          ))
  
  #clean environment
  #rm(list = c(ls()[which(ls() != "master_list")]))
  
  # DATA PROCESS: Response ratio and concentration calculations  ------------------------------------------------------
  #* Calculation of target metabolite/stable isotope labelled (SIL) internal standard ratio, using predefined target metabolite/internal standard pairs
  #* Conversion of response ratio to concentration values using single point calibration
  #* Performed on raw area data (non-filtered)
  
  #set empty list to store output data
  master_list$data$response <- list()
  master_list$data$concentration <- list()
  master_list$summary_tables$concentration_summary <- list()
  
  for(idx_data in names(master_list$data$area_sorted)){
    loop_data <- master_list$environment$user_functions$conc_calc$value(
        FUNC_data = master_list$data$area_sorted[[idx_data]],
        FUNC_SIL_list = master_list$data$area_sorted[[idx_data]] %>% 
          select(-contains("sample")) %>% select(contains("SIL")) %>% names(),
        FUNC_SIL_guide = master_list$templates$SIL_guide,
        FUNC_conc_guide = master_list$templates$conc_guide
      )
    master_list$data$response[[idx_data]] <- loop_data$response; master_list$data$concentration[[idx_data]] <- loop_data$concentration
  
  # SUMMARY TABLE: summary table of data following response ratio and concentration calculation ----------------------------
  
  master_list$summary_tables$concentration_summary <- bind_rows(
    master_list$summary_tables$concentration_summary,
      bind_cols(
        "batch" = idx_data,
        "total samples" = nrow(master_list$data$concentration[[idx_data]]),
        "VLTR samples" = length(which(master_list$data$concentration[[idx_data]]$sample_type_factor == "vltr")), #report number of VLTRs in dataset
        "SLTR samples" = length(which(master_list$data$concentration[[idx_data]]$sample_type_factor == "sltr")), #report number of SLTRs in dataset
        "LTR samples" = length(which(master_list$data$concentration[[idx_data]]$sample_type_factor == "ltr")), #report number of LTRs in dataset
        "PQC samples" = length(which(master_list$data$concentration[[idx_data]]$sample_type_factor == "pqc")), #report number of PQC in dataset
        "study samples"= nrow(master_list$data$concentration[[idx_data]])-
          length(which(master_list$data$concentration[[idx_data]]$sample_type_factor == "vltr"))- 
          length(which(master_list$data$concentration[[idx_data]]$sample_type_factor == "sltr")) -
          length(which(master_list$data$concentration[[idx_data]]$sample_type_factor == "ltr")) -
          length(which(master_list$data$concentration[[idx_data]]$sample_type_factor == "pqc")),#report number of study samples in dataset
        "total lipid targets" = ncol(master_list$data$concentration[[idx_data]] %>% 
                                       select(-contains("sample")) %>%
                                       select(-contains("SIL"))
        ),
        "total SIL Int.Stds" = ncol(master_list$data$concentration[[idx_data]] %>% 
                                      select(-contains("sample")) %>%
                                      select(contains("SIL"))
        ),
        "zero values (lipid targets)" = length(which(
          master_list$data$concentration[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL"))==0
        )),
        "zero values (SIL Int.Stds)" = length(which(
          master_list$data$concentration[[idx_data]] %>% select(-contains("sample")) %>% select(contains("SIL"))==0
        )),
        "NA values (lipid targets)" = length(which(
          is.na(as.matrix(master_list$data$concentration[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL"))
          )))),
        "NA values (SIL Int.Stds)" = length(which(
          is.na(as.matrix(master_list$data$concentration[[idx_data]] %>% select(-contains("sample")) %>% select(contains("SIL"))
          )))),
        "NaN values (lipid targets)" = length(which(
          is.nan(as.matrix(master_list$data$concentration[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL"))
          )))),
        "NaN values (SIL Int.Stds)" = length(which(
          is.nan(as.matrix(master_list$data$concentration[[idx_data]] %>% select(-contains("sample")) %>% select(contains("SIL"))
          )))),
        "Inf values (lipid targets)" = length(which(
          is.infinite(as.matrix(master_list$data$concentration[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL"))
          )))),
        "Inf values (SIL Int.Stds)" = length(which(
          is.infinite(as.matrix(master_list$data$concentration[[idx_data]] %>% select(-contains("sample")) %>% select(contains("SIL"))
          ))))
      ))
  }
  
  master_list$summary_tables$concentration_summary <- rbind(master_list$summary_tables$concentration_summary,
                                                            c("total project",
                                                              sum(master_list$summary_tables$concentration_summary$`total samples`),
                                                              sum(master_list$summary_tables$concentration_summary$`VLTR samples`),
                                                              sum(master_list$summary_tables$concentration_summary$`SLTR samples`),
                                                              sum(master_list$summary_tables$concentration_summary$`LTR samples`),
                                                              sum(master_list$summary_tables$concentration_summary$`PQC samples`),
                                                              sum(master_list$summary_tables$concentration_summary$`study samples`),
                                                              max(master_list$summary_tables$concentration_summary$`total lipid targets`),
                                                              max(master_list$summary_tables$concentration_summary$`total SIL Int.Stds`),
                                                              sum(master_list$summary_tables$concentration_summary$`zero values (lipid targets)`),
                                                              sum(master_list$summary_tables$concentration_summary$`zero values (SIL Int.Stds)`),
                                                              sum(master_list$summary_tables$concentration_summary$`NA values (lipid targets)`),
                                                              sum(master_list$summary_tables$concentration_summary$`NA values (SIL Int.Stds)`),
                                                              sum(master_list$summary_tables$concentration_summary$`NaN values (lipid targets)`),
                                                              sum(master_list$summary_tables$concentration_summary$`NaN values (SIL Int.Stds)`),
                                                              sum(master_list$summary_tables$concentration_summary$`Inf values (lipid targets)`),
                                                              sum(master_list$summary_tables$concentration_summary$`Inf values (SIL Int.Stds)`)
                                                            ))
  
  # DATA PREPROCESSING: data filter: missing values filter by flags --------------------
  # * Step 1: Remove all samples that have > 50% missing values in peak area (removes any mis-injections etc that may be present in the data)
  # * Step 2: Remove all metabolite features that have > 50% missing values (zero, NA, NaN etc)
  # * Step 3: Impute missing values
  # * Step 4: %RSD filter on user selected QC type
  
  #create empty lists for storing outputs
  master_list$data$area_missVal_filtered <- list()
  master_list$process_lists$area_missVal_filtered <- list()
  #master_list$process_lists$area_missVal_filtered$failed_SIL <- NULL
  # master_list$summary_tables$missing_value_filter_summary <- list()
  # master_list$summary_tables$missing_value_qc_fail <- list()
  
  
  for(idx_data in names(master_list$data$area_sorted)){
    master_list$process_lists$area_missVal_filtered[[idx_data]] <- list()
    
    for(idx_sample_type in c("sample", tolower(master_list$project_details$qc_type))){
      master_list$process_lists$area_missVal_filtered[[idx_data]][[idx_sample_type]] <- list()
      #run missing value filter function
      master_list$process_lists$area_missVal_filtered[[idx_data]][[idx_sample_type]] <- master_list$environment$user_functions$miss_value_filter$value(
        FUNC_data = master_list$data$area_sorted[[idx_data]] %>%
          filter(sample_type == idx_sample_type),
        FUNC_metabolite_list = master_list$data$area_sorted[[idx_data]] %>%
          select(-contains("sample")) %>% names(),
        FUNC_IS_tag = "SIL",
        FUNC_OPTION_missing_value_threshold_sample = 0.50, #decimal % of missing value threshold before sample is removed from dataset
        FUNC_OPTION_missing_value_threshold_feature = 0.50, #decimal % of missing value threshold before feature is removed from dataset
        FUNC_OPTION_intensity_threshold = 5000)
    }
    
    #bind fail sample list
    master_list$process_lists$area_missVal_filtered[[idx_data]]$sample_fail_list <- c(
      master_list$process_lists$area_missVal_filtered[[idx_data]]$sample$mv_samples_fail,
      master_list$process_lists$area_missVal_filtered[[idx_data]]$qc$mv_samples_fail) %>%
      unique()
    
    #bind fail feature list
    master_list$process_lists$area_missVal_filtered[[idx_data]]$feature_fail_list <- c(
      master_list$process_lists$area_missVal_filtered[[idx_data]]$sample$mv_features_fail,
      master_list$process_lists$area_missVal_filtered[[idx_data]]$qc$mv_features_fail) %>%
      unique()
    
    #create filtered dataset per plate for next phase
    master_list$data$area_missVal_filtered[[idx_data]] <- master_list$data$area_sorted[[idx_data]] %>%
      filter(!sample_name %in% master_list$process_lists$area_missVal_filtered[[idx_data]]$sample_fail_list) %>% #select only pass samples
      select(-any_of(master_list$process_lists$area_missVal_filtered[[idx_data]]$feature_fail_list)) #select only pass features
  }
  
  # DATA PREPROCESSING: select data features that are common for all plates ------------
  master_list$data$area_common_metabolite_filter <- list()
  
  #step 1: find common features
  temp_common_features <- names(master_list$data$area_missVal_filtered[[1]])
  for(idx_data in names(master_list$data$area_missVal_filtered)){
    temp_common_features <- intersect(temp_common_features,
                                      names(master_list$data$area_missVal_filtered[[idx_data]]))
  }
  
  for(idx_data in names(master_list$data$area_missVal_filtered)){
    master_list$data$area_common_metabolite_filter[[idx_data]] <- master_list$data$area_missVal_filtered[[idx_data]] %>%
      select(any_of(temp_common_features))
  }
  

  #DATA PREPROCESSING: impute missing values [min/2 imputation (missing assumed < LOD)] -----------------------------------------------------
  #Imputation of the remaining zero value and missing data 
  #Imputation is completed using x/2, where x is minimum intensity of that feature in the batch
  
  master_list$data$area_impute <- list()
  #master_list$summary_tables$impute_table <- list()
  
  for(idx_data in names(master_list$data$area_common_metabolite_filter)){
    master_list$data$area_impute[[idx_data]] <- master_list$environment$user_functions$impute_data$value(
      FUNC_data = master_list$data$area_common_metabolite_filter[[idx_data]],
      FUNC_metabolite_list = master_list$data$area_common_metabolite_filter[[idx_data]] %>% 
        select(-contains("sample")) %>% names(),
      FUNC_option_impute_missing_data = TRUE)
  }
  
  #DATA PREPROCESSING: response and concentration completed on imputed data
  #* Calculation of target metabolite/stable isotope labelled (SIL) internal standard ratio, using predefined target metabolite/internal standard pairs
  #* Conversion of response ratio to concentration values using single point calibration
  #* Performed on imputed and filtered data
  
  #set empty list to store output data
  master_list$data$response_impute <- list()
  master_list$data$concentration_impute <- list()
  
  for(idx_data in names(master_list$data$area_impute)){
    loop_data <- master_list$environment$user_functions$conc_calc$value(
      FUNC_data = master_list$data$area_impute[[idx_data]],
      FUNC_SIL_list = master_list$data$area_impute[[idx_data]] %>% 
        select(-contains("sample")) %>% select(contains("SIL")) %>% names(),
      FUNC_SIL_guide = master_list$templates$SIL_guide,
      FUNC_conc_guide = master_list$templates$conc_guide
    )
    master_list$data$response_impute[[idx_data]] <- loop_data$response; master_list$data$concentration_impute[[idx_data]] <- loop_data$concentration
  }
   
  
  #DATAPREPROCESSING: %RSD filter in user selected LTRs (PER PLATE)
  master_list$data$concentration_impute_rsd <- list()
  master_list$process_lists$rsd_filter <- tibble(
    lipid =  names(qc_data)
  )
  for(idx_data in names(master_list$data$concentration_impute)){
    qc_data <- master_list$data$concentration_impute[[idx_data]] %>%
      filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
      select(-contains("sample"))
    master_list$process_lists$rsd_filter <- left_join(
      master_list$process_lists$rsd_filter,
      tibble(
        lipid =  names(qc_data),
        !!idx_data := (colSds(as.matrix(qc_data))*100)/colMeans(as.matrix(qc_data))
      ), 
      by = "lipid")
    lipid_rsd_pass <- which((colSds(as.matrix(qc_data))*100)/colMeans(as.matrix(qc_data)) <30)
    lipid_rsd_fail <- which((colSds(as.matrix(qc_data))*100)/colMeans(as.matrix(qc_data)) >30)
    # create rsd filtered tibble
    master_list$data$concentration_impute_rsd[[idx_data]] <- master_list$data$concentration_impute[[idx_data]] %>%
      select(contains("sample"), all_of(lipid_rsd_pass))
    }
  
  
  
  #DATAPREPROCESSING: %RSD filter in user selected LTRs (ALL PLATES/BATCHES)
  qc_data <- bind_rows(master_list$data$concentration_impute) %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(qc_data),
      across_all_plates = (colSds(as.matrix(qc_data))*100)/colMeans(as.matrix(qc_data))
    ), 
    by = "lipid")
  #get list of lipids that pass
  lipid_rsd_pass <- which((colSds(as.matrix(qc_data))*100)/colMeans(as.matrix(qc_data)) <30)
  
  #create final dataset
  master_list$data$concentration_impute_rsd_all <- bind_rows(master_list$data$concentration_impute) %>%
    select(contains("sample"), all_of(lipid_rsd_pass))
  
  #EXPORT: Unfiltered ods tabbed file   -------------------------------
  write_ods(
    path = paste0(master_list$project_details$project_dir, "/html_report/test_File.ods"),
    list(
      "userGuide" = data.frame(
        tab = c(
          "lipidPeakArea",
          "silPeakArea",
          "responseRatio",
          "concentration",
          "preProcessedConcentration",
          "",
          "NOTE:"),
        descriptor = c(
          "lipid target peak area integrals (skylineMS)",
          "stable isotope labelled internal standard peak area integrals (skylineMS)",
          "lipid target/SIL internal standards peak area response ratios",
          "peak area response ratios normalised to SIL Int.Std concentration factor",
          "concentration data that has undergone pre-processing (missing value sample filter [>50%], missing value feature filter [>50%], imputation remaining missing values [min/2], relative standard deviation filtering [>30%]",
          "",
          "DATA IS RAW AREA, NO SIGNAL DRIFT OR BATCH CORRECTION HAS BEEN APPLIED")
      ),
      "areaSummary" = master_list$summary_tables$area_summary,
      "lipidPeakArea" = bind_rows(master_list$data$area_sorted) %>% select(-contains("SIL")),
      "silPeakArea" = bind_rows(master_list$data$area_sorted) %>% select(contains("sample") | contains("SIL")),
      "responseRatio" = bind_rows(master_list$data$response),
      "concentration" = bind_rows(master_list$data$concentration),
      "preProcessedConcentration" = master_list$data$concentration_impute_rsd_all
    )
  )
  
  
  
  
  
  
  
  
  
  
  #GAP  -------------------------------
  #GAP -------------------------------
  
  
  
  
  
  
  
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  




#clean environment
#rm(list = c(ls()[which(ls() != "master_list")]))




# PCA: raw Skyline imports ------------------------------------------------

#create empty list for results
master_list$pca_analysis$data_area_sorted <- list()
master_list$pca_analysis$data_area_sorted$sample_qc <- list()
master_list$pca_analysis$data_area_sorted$plate <- list()

#run pca loop color by QC/sample
master_list$pca_analysis$data_area_sorted$sample_qc <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$area_sorted %>% bind_rows(), 
  FUNC_metabolite_list = master_list$data$area_sorted %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_type_factor",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
  )

#run pca loop color by plateID
master_list$pca_analysis$data_area_sorted$plate <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$area_sorted %>% bind_rows(), 
  FUNC_metabolite_list = master_list$data$area_sorted %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_plate_id",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = (RColorBrewer::brewer.pal(name = "Set3", n= 12))[length(unique(bind_rows(master_list$data$area_sorted)[["sample_plate_id"]]))],
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape = master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_plate_id"
)

#clean environment
#rm(list = c(ls()[which(ls() != "master_list")]))




















# PROCESS: make concentration values from peak area (unfiltered data)









# PROCESS: data filter: missing values filter by flags --------------------

# * Step 1: Remove all samples that have > 50% missing values in peak area (removes any mis-injections etc that may be present in the data)
# * Step 2: Remove all metabolite features that have > 50% missing values (zero, NA, NaN etc)

#create empty lists for storing outputs
master_list$data$missing_value_filter <- list()
master_list$process_lists$missing_value_filter <- list()
master_list$process_lists$missing_value_filter$failed_SIL <- NULL
master_list$summary_tables$missing_value_filter_summary <- list()
master_list$summary_tables$missing_value_qc_fail <- list()


for(idx_data in names(master_list$data$area_sorted)){
  master_list$process_lists$missing_value_filter[[idx_data]] <- list()
  #master_list$process_lists$missing_value_filter[[idx_data]]$sample_fail_list <- list()
  #master_list$process_lists$missing_value_filter[[idx_data]]$feature_fail_list <- list()
  
  
  for(idx_sample_type in c("sample", "qc")){
    master_list$process_lists$missing_value_filter[[idx_data]][[idx_sample_type]] <- list()
    
    
    #run missing value filter function
    
    master_list$process_lists$missing_value_filter[[idx_data]][[idx_sample_type]] <- master_list$environment$user_functions$miss_value_filter$value(
      FUNC_data = master_list$data$area_sorted[[idx_data]] %>%
        filter(sample_type == idx_sample_type),
      FUNC_metabolite_list = master_list$data$area_sorted[[idx_data]] %>%
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
  master_list$data$missing_value_filter[[idx_data]] <- master_list$data$area_sorted[[idx_data]] %>%
    filter(!sample_name %in% master_list$process_lists$missing_value_filter[[idx_data]]$sample_fail_list) %>% #select only pass samples
    select(-all_of(master_list$process_lists$missing_value_filter[[idx_data]]$feature_fail_list)) #select only pass features
  
  
  
  
  # TABLE: summary table of missing value filter ----------------------------
  
  master_list$summary_tables$missing_value_filter_summary <- master_list$summary_tables$missing_value_filter_summary %>%
    bind_rows(
      bind_cols("batch" = idx_data,
                "samples pre-filter" = master_list$data$area_sorted[[idx_data]] %>% nrow(),
                "samples post-filter" = master_list$data$missing_value_filter[[idx_data]] %>% nrow(),
                "samples removed" = master_list$data$area_sorted[[idx_data]] %>% nrow() -
                  master_list$data$missing_value_filter[[idx_data]] %>% nrow(),
                "qc samples remaining" = master_list$data$missing_value_filter[[idx_data]] %>% filter(grepl('qc', sample_type)) %>% nrow(), #report number of QCs remaining in dataset
                "study samples remaining" = master_list$data$missing_value_filter[[idx_data]] %>% filter(grepl('sample', sample_type)) %>% nrow(), #report number of samples in dataset
                "features pre-filter" = master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL")) %>% ncol(),
                "features post-filter" = master_list$data$missing_value_filter[[idx_data]] %>% select(-contains("sample"))  %>% select(-contains("SIL")) %>% ncol(),
                "features removed" = master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample"))  %>% select(-contains("SIL")) %>% ncol() -
                  master_list$data$missing_value_filter[[idx_data]] %>% select(-contains("sample")) %>% select(-contains("SIL")) %>% ncol(),
                "internal standards pre-filter" = master_list$data$area_sorted[[idx_data]] %>% select(contains("SIL")) %>% ncol(),
                "internal standards post-filter" = master_list$data$missing_value_filter[[idx_data]]  %>% select(contains("SIL")) %>% ncol(),
                "internal standards removed" = master_list$data$area_sorted[[idx_data]]  %>% select(contains("SIL")) %>% ncol() -
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
  total_qc <- master_list$data$area_sorted[[idx_data]] %>% filter(grepl('qc', sample_type)) %>% nrow()
  
  
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


#set empty table if all plates pass missing value qc threshold

if (length(master_list$summary_tables$missing_value_qc_fail) == 0){master_list$summary_tables$missing_value_qc_fail <- "No plates were removed"} 

# PROCESS: select data features that are common for all plates ------------
master_list$data$area_common_metabolite_filter <- list()

#step 1: find common features
temp_common_features <- names(master_list$data$missing_value_filter[[1]])
for(idx_data in names(master_list$data$missing_value_filter)){
  temp_common_features <- intersect(temp_common_features,
                                    names(master_list$data$missing_value_filter[[idx_data]]))
}

for(idx_data in names(master_list$data$missing_value_filter)){
  master_list$data$area_common_metabolite_filter[[idx_data]] <- master_list$data$missing_value_filter[[idx_data]] %>%
    select(all_of(temp_common_features))
}


# PCA: post-missing value filter ------------------------------------------

#create empty list for results
master_list$pca_analysis$missing_value_filter <- list()
#run pca loop color by QC/sample
master_list$pca_analysis$missing_value_filter$sample_qc <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$area_common_metabolite_filter %>% bind_rows(), 
  FUNC_metabolite_list = master_list$data$area_common_metabolite_filter %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_type_factor",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
)

#run pca loop color by plate
master_list$pca_analysis$missing_value_filter$plate <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$area_common_metabolite_filter %>% bind_rows(), 
  FUNC_metabolite_list = master_list$data$area_common_metabolite_filter %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_plate_id",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = (RColorBrewer::brewer.pal(name = "Set3", n= 12))[length(unique(bind_rows(master_list$data$area_sorted)[["sample_plate_id"]]))],
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape = master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_plate_id"
)

# PROCESS: imputation of remaining missing values -----------------------------------------------------

#Imputation of the remaining zero value and missing data 
#Imputation is completed using x/2, where x is minimum intensity of that feature in the batch

master_list$data$area_impute <- list()
master_list$summary_tables$impute_table <- list()

for(idx_data in names(master_list$data$area_common_metabolite_filter)){
  master_list$data$area_impute[[idx_data]] <- master_list$environment$user_functions$impute_data$value(
    FUNC_data = master_list$data$area_common_metabolite_filter[[idx_data]],
    FUNC_metabolite_list = master_list$data$area_common_metabolite_filter[[idx_data]] %>% 
      select(-contains("sample")) %>% names(),
    FUNC_option_impute_missing_data = TRUE)
  
  # TABLE: summary table of imputed data ----------------------------
  
  master_list$summary_tables$impute_table <- master_list$summary_tables$impute_table %>%
    bind_rows(
      bind_cols("batch" = idx_data,
                "zero vaules pre-imputation" = length(which(master_list$data$area_common_metabolite_filter[[idx_data]] %>% select(!contains("sample"))==0)),
                "NA values pre-imputation" = length(which(is.na(as.matrix(master_list$data$area_common_metabolite_filter[[idx_data]] %>% select(!contains("sample")))))),
                "NaN values pre-imputation" = length(which(is.nan(as.matrix(master_list$data$area_common_metabolite_filter[[idx_data]] %>% select(!contains("sample")))))), # find NANs
                "zero vaules post-imputation" = length(which(master_list$data$area_impute[[idx_data]] %>% select(!contains("sample"))==0)),
                "NA values post-imputation" = length(which(is.na(as.matrix(master_list$data$area_impute[[idx_data]] %>% select(!contains("sample")))))),
                "NaN values post-imputation" = length(which(is.nan(as.matrix(master_list$data$area_impute[[idx_data]] %>% select(!contains("sample")))))), 
                
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

# PROCESS: statTarget signal drift correction on imputed raw data -----------------------------------------------------

#* Raw data for each individual mrm transition undergoes signal drift correction using statTarget package (https://stattarget.github.io/)
#* This is performed within individual batches at this point to evaluate the performance of each batch
# 
#list for storing signal drift corrected data (per project)
master_list$data$statTarget <- list()
master_list$summary_tables$statTarget_table <- list()

#create batch correction directory
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/batch_correction"))){
  dir.create(paste0(master_list$project_details$project_dir, "/data/batch_correction"))
}

#run batch correction 
statTarget_data <- master_list$environment$user_functions$signal_correct$value(
  FUNC_project_directory = paste0(master_list$project_details$project_dir,
                                  "/data/batch_correction"),
  FUNC_data = master_list$data$area_impute %>% bind_rows(), 
  FUNC_metabolite_list = master_list$data$area_impute %>% bind_rows() %>%
    select(!contains("sample")) %>% names(),
  FUNC_header_sample_id = "sample_name",
  FUNC_header_batch = "sample_plate_id",
  FUNC_header_sample_type = "sample_type",
  FUNC_header_run_order = "sample_run_index",
  FUNC_option_method = "RF",
  FUNC_option_coCV = 1000 # set at 1000% as filter out manually later
)

#assign data to plate list
for(idx_data in names(master_list$data$area_impute)){
  master_list$data$statTarget[[idx_data]] <- statTarget_data %>% filter(sample_plate_id == idx_data) #%>% select(-sample_run_index)
  
  # TABLE: summary table of statTarget data ----------------------------
  
  master_list$summary_tables$statTarget_table <- master_list$summary_tables$statTarget_table %>%
    bind_rows(
      bind_cols("batch" = idx_data,
                "zero vaules pre-statTarget" = length(which(master_list$data$area_impute[[idx_data]] %>% select(!contains("sample"))==0)),
                "NA values pre-statTarget" = length(which(is.na(as.matrix(master_list$data$area_impute[[idx_data]] %>% select(!contains("sample")))))),
                "NaN values pre-statTarget" = length(which(is.nan(as.matrix(master_list$data$area_impute[[idx_data]] %>% select(!contains("sample")))))), # find NANs
                "zero vaules post-statTarget" = length(which(master_list$data$statTarget[[idx_data]] %>% select(!contains("sample"))==0)),
                "NA values post-statTarget" = length(which(is.na(as.matrix(master_list$data$statTarget[[idx_data]] %>% select(!contains("sample")))))),
                "NaN values post-statTarget" = length(which(is.nan(as.matrix(master_list$data$statTarget[[idx_data]] %>% select(!contains("sample")))))), 
                
      ))
}

master_list$summary_tables$statTarget_table <- rbind(master_list$summary_tables$statTarget_table,
                                                     c("total project",
                                                       sum(master_list$summary_tables$statTarget_table$`zero vaules pre-statTarget`),
                                                       sum(master_list$summary_tables$statTarget_table$`NA values pre-statTarget`),
                                                       sum(master_list$summary_tables$statTarget_table$`NaN values pre-statTarget`),
                                                       sum(master_list$summary_tables$statTarget_table$`zero vaules post-statTarget`),
                                                       sum(master_list$summary_tables$statTarget_table$`NA values post-statTarget`),
                                                       sum(master_list$summary_tables$statTarget_table$`NaN values post-statTarget`)
                                                     ))



# PROCESS: Response ratio and concentration calculations  ------------------------------------------------------
#* Calculation of target metabolite/stable isotope labelled (SIL) internal standard ratio, using predefined target metabolite/internal standard pairs
#* Conversion of response ratio to concentration values using single point calibration
#* Performed on filtered data

#prepare template/guides for concentration for calculation
master_list$templates <- list()
#if using sciex lipidizer internal standards
if(master_list$project_details$is_ver == "v1"){
master_list$templates$SIL_guide <- read_csv(
  file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_lipid_mrm_template_v1.csv",
  show_col_types = FALSE) %>%
  clean_names()
master_list$templates$conc_guide <- read_csv(
  file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_SIL_batch_103.csv", 
  show_col_types = FALSE) %>% 
  clean_names()
}

#if using ultra splash mix (ANPC method v2)
if(master_list$project_details$is_ver == "v2"){
  master_list$templates$SIL_guide <- read_csv(
    file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_lipid_mrm_template_v2.csv",
    show_col_types = FALSE) %>%
    clean_names()
  master_list$templates$conc_guide <- read_csv(
    file = "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main/templates/LGW_SIL_batch_Ultimate_2023-03-06.csv", 
    show_col_types = FALSE) %>% 
    clean_names()
}


# PROCESS: Response ratio and concentration calculations for raw imputed data  ------------------------------------------------------
#set empty list to store output data
master_list$data$concentration <- list()
master_list$summary_tables$concentration_summary <- list()

for(idx_data in names(master_list$data$area_impute)){
  master_list$data$concentration[[idx_data]] <- master_list$environment$user_functions$conc_calc$value(
    FUNC_data = master_list$data$area_impute[[idx_data]],
    FUNC_metabolite_list = master_list$data$area_impute[[idx_data]] %>% 
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

# PROCESS: Response ratio and concentration calculations for statTarget data  ------------------------------------------------------
#set empty list to store output data
master_list$data$concentration_statTarget <- list()
master_list$summary_tables$concentration_statTarget_summary <- list()

for(idx_data in names(master_list$data$statTarget)){
  master_list$data$concentration_statTarget[[idx_data]] <- master_list$environment$user_functions$conc_calc$value(
    FUNC_data = master_list$data$statTarget[[idx_data]],
    FUNC_metabolite_list = master_list$data$statTarget[[idx_data]] %>% 
      select(!contains("sample")) %>% names(),
    FUNC_SIL_guide = master_list$templates$SIL_guide,
    FUNC_conc_guide = master_list$templates$conc_guide) %>%
    #ensure no SIL metabolites remain in dataset
    select(!contains("SIL"))
  
  # TABLE: summary table of data following response ratio and concentration_statTarget calculation ----------------------------
  
  master_list$summary_tables$concentration_statTarget_summary <- master_list$summary_tables$concentration_statTarget_summary %>%
    bind_rows(
      bind_cols("batch" = idx_data,
                "total samples" = master_list$data$concentration_statTarget[[idx_data]] %>% nrow(),
                "qc samples" = master_list$data$concentration_statTarget[[idx_data]] %>% filter(grepl('qc', sample_type)) %>% nrow(), #report number of QCs remaining in dataset
                "study samples" = master_list$data$concentration_statTarget[[idx_data]] %>% filter(grepl('sample', sample_type)) %>% nrow(), #report number of samples in dataset
                "features" = master_list$data$concentration_statTarget[[idx_data]] %>% select(!contains("sample")) %>% select(-contains("SIL")) %>% ncol(),
                "zero values remaining" = length(which(master_list$data$concentration_statTarget[[idx_data]] %>% select(!contains("sample"))==0)), #find 0 values
                "NA values remaining" = length(which(is.na(as.matrix(master_list$data$concentration_statTarget[[idx_data]] %>% select(!contains("sample")))))), #find NAs
                "NaN values remaining" = length(which(is.nan(as.matrix(master_list$data$concentration_statTarget[[idx_data]] %>% select(!contains("sample")))))) # find NANs
      ))
  
}

master_list$summary_tables$concentration_statTarget_summary <- rbind(master_list$summary_tables$concentration_statTarget_summary,
                                                                     c("total project",
                                                                       sum(master_list$summary_tables$concentration_statTarget_summary$`total samples`),
                                                                       sum(master_list$summary_tables$concentration_statTarget_summary$`qc samples`),
                                                                       sum(master_list$summary_tables$concentration_statTarget_summary$`study samples`),
                                                                       max(master_list$summary_tables$concentration_statTarget_summary$features),
                                                                       sum(master_list$summary_tables$concentration_statTarget_summary$`zero values remaining`),
                                                                       sum(master_list$summary_tables$concentration_statTarget_summary$`NA values remaining`),
                                                                       sum(master_list$summary_tables$concentration_statTarget_summary$`NaN values remaining`)
                                                                     ))
#bind plates into single tibble

#raw imputed data
master_list$data$concentration_bind_plates <- master_list$data$concentration %>%
  bind_rows() 

#statTarget data
master_list$data$concentration_statTarget_bind_plates <- master_list$data$concentration_statTarget %>%
  bind_rows() 

#clean environment
#rm(list = c(ls()[which(ls() != "master_list")]))


# PCA: response ratio and concentration -----------------------
# raw imputed data

#create empty list for results
master_list$pca_analysis$concentration <- list()
#run pca loop color by QC/sample
master_list$pca_analysis$concentration$sample_qc <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$concentration_bind_plates,
  FUNC_metabolite_list = master_list$data$concentration_bind_plates %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_type_factor",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
)

#run pca loop color by plate
master_list$pca_analysis$concentration$plate <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$concentration_bind_plates,
  FUNC_metabolite_list = master_list$data$concentration_bind_plates %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_plate_id",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = (RColorBrewer::brewer.pal(name = "Set3", n= 12))[length(unique(bind_rows(master_list$data$area_sorted)[["sample_plate_id"]]))],
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape = master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_plate_id"
)


# statTarget data

#create empty list for results
master_list$pca_analysis$concentration_statTarget <- list()
#run pca loop color by QC/sample
master_list$pca_analysis$concentration_statTarget$sample_qc <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$concentration_statTarget_bind_plates,
  FUNC_metabolite_list = master_list$data$concentration_statTarget_bind_plates %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_type_factor",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
)

#run pca loop color by plate
master_list$pca_analysis$concentration_statTarget$plate <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$concentration_statTarget_bind_plates,
  FUNC_metabolite_list = master_list$data$concentration_statTarget_bind_plates %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_plate_id",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = (RColorBrewer::brewer.pal(name = "Set3", n= 12))[length(unique(bind_rows(master_list$data$area_sorted)[["sample_plate_id"]]))],
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape = master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_plate_id"
)

# PROCESS: %RSD filter and summary -----------------------------------------

# TABLE: statTarget signal drift correction summary table -------------------------------

# %RSD from raw imputed data
prelipid_stDev <- colSds(master_list$data$concentration_bind_plates %>% filter(sample_type == "qc") %>% select(!contains("sample")) %>% as.matrix())
prelipid_means <- colMeans2(master_list$data$concentration_bind_plates %>% filter(sample_type == "qc") %>% select(!contains("sample")) %>% as.matrix())
prelipid_RSD <- (100*prelipid_stDev)/prelipid_means
prelipid_RSD_fail <- (master_list$data$concentration_bind_plates %>% select(!contains("sample")) %>% names())[which(prelipid_RSD > 30)]

# %RSD from statTarget data
postlipid_stDev <- colSds(master_list$data$concentration_statTarget_bind_plates %>% filter(sample_type == "qc") %>% select(!contains("sample")) %>% as.matrix())
postlipid_means <- colMeans2(master_list$data$concentration_statTarget_bind_plates %>% filter(sample_type == "qc") %>% select(!contains("sample")) %>% as.matrix())
postlipid_RSD <- (100*postlipid_stDev)/postlipid_means
postlipid_RSD_fail <- (master_list$data$concentration_statTarget_bind_plates %>% select(!contains("sample")) %>% names())[which(postlipid_RSD > 30)]

#create summary table
master_list$summary_tables$batch_correction_overview <- list()
master_list$summary_tables$batch_correction_overview <- bind_rows(
  bind_cols(
    "data" = "pre-statTarget signal correction",
    "total samples" = nrow(master_list$data$concentration_bind_plates),
    "qc samples" = nrow(master_list$data$concentration_bind_plates %>% filter(sample_type == "qc")),
    "study samples" = nrow(master_list$data$concentration_bind_plates %>% filter(sample_type == "sample")),
    "total features" = ncol(master_list$data$concentration_bind_plates %>% select(!contains("sample"))),
    "features < 30% qcRSD" = length(which(prelipid_RSD < 30)),
    "features < 20% qcRSD" = length(which(prelipid_RSD < 20)),
    "features < 10% qcRSD" = length(which(prelipid_RSD < 10)),
    "zero values" = length(which(master_list$data$concentration_bind_plates %>% select(!contains("sample"))==0)),
    "NA values remaining" = length(which(is.na(as.matrix(master_list$data$concentration_bind_plates %>% select(!contains("sample")))))), #find NAs
    "NaN values remaining" = length(which(is.nan(as.matrix(master_list$data$concentration_bind_plates %>% select(!contains("sample"))))))#find 0 values
  ),
  bind_cols(
    "data" = "post-statTarget signal correction",
    "total samples" = nrow(master_list$data$concentration_statTarget_bind_plates),
    "qc samples" = nrow(master_list$data$concentration_statTarget_bind_plates %>% filter(sample_type == "qc")),
    "study samples" = nrow(master_list$data$concentration_statTarget_bind_plates %>% filter(sample_type == "sample")),
    "total features" = ncol(master_list$data$concentration_statTarget_bind_plates %>% select(!contains("sample"))),
    "features < 30% qcRSD" = length(which(postlipid_RSD < 30)),
    "features < 20% qcRSD" = length(which(postlipid_RSD < 20)),
    "features < 10% qcRSD" = length(which(postlipid_RSD < 10)),
    "zero values" = length(which(master_list$data$concentration_statTarget_bind_plates %>% select(!contains("sample"))==0)),
    "NA values remaining" = length(which(is.na(as.matrix(master_list$data$concentration_statTarget_bind_plates %>% select(!contains("sample")))))), #find NAs
    "NaN values remaining" = length(which(is.nan(as.matrix(master_list$data$concentration_statTarget_bind_plates %>% select(!contains("sample"))))))#find 0 values
  )
)

# PROCESS: create final datasets (remove lipids > 30% RSD) -----------------------------------------
#raw imputed data
master_list$data$concentration_rsd_filter <- master_list$data$concentration_bind_plates %>% select(!contains(prelipid_RSD_fail))
#statTarget data
master_list$data$concentration_statTarget_rsd_filter <- master_list$data$concentration_statTarget_bind_plates %>% select(!contains(postlipid_RSD_fail))


#clean environment
#rm(list = c(ls()[which(ls() != "master_list")]))


# PCA: RSD filtered data -----------------------
# imputed data

#create empty list for results
master_list$pca_analysis$concentration_rsd_filtered <- list()
#run pca loop color by QC/sample
master_list$pca_analysis$concentration_rsd_filtered$sample_qc <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$concentration_rsd_filter,
  FUNC_metabolite_list = master_list$data$concentration_rsd_filter %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_type_factor",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
)

#run pca loop color by plate
master_list$pca_analysis$concentration_rsd_filtered$plate <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$concentration_rsd_filter,
  FUNC_metabolite_list = master_list$data$concentration_rsd_filter %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_plate_id",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = (RColorBrewer::brewer.pal(name = "Set3", n= 12))[length(unique(bind_rows(master_list$data$area_sorted)[["sample_plate_id"]]))],
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape = master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_plate_id"
)

# statTarget data

#create empty list for results
master_list$pca_analysis$concentration_statTarget_rsd_filtered <- list()
#run pca loop color by QC/sample
master_list$pca_analysis$concentration_statTarget_rsd_filtered$sample_qc <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$concentration_statTarget_rsd_filter,
  FUNC_metabolite_list = master_list$data$concentration_statTarget_rsd_filter %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_type_factor",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
)

#run pca loop color by plate
master_list$pca_analysis$concentration_statTarget_rsd_filtered$plate <- master_list$environment$user_functions$pca$value(
  FUNC_data =  master_list$data$concentration_statTarget_rsd_filter,
  FUNC_metabolite_list = master_list$data$concentration_statTarget_rsd_filter %>% 
    bind_rows() %>%
    select(-contains("sample")) %>% 
    select(-contains("SIL")) %>%
    names(), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_plate_id",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = (RColorBrewer::brewer.pal(name = "Set3", n= 12))[length(unique(bind_rows(master_list$data$area_sorted)[["sample_plate_id"]]))],
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape = master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_plate_id"
)

#clean environment
#rm(list = c(ls()[which(ls() != "master_list")]))

# PLOT: PCA scores vs run order chart ------------------------------------------------
#run order vs PC plots
# master_list$environment$user_functions$pc_run_plot <- source(paste0(master_list$project_details$github_master_dir,
#                                                                     "/functions/FUNC_lipidExploreR_PCA_runOrder_ggplot.R"))

master_list$pc_runorder_plots <- master_list$environment$user_functions$pc_run_plot$value(
  FUNC_data_concentration = master_list$pca_analysis$concentration_rsd_filtered$sample_qc$plot_Val %>% add_column(sample_data_type = "concentration", .before=1),
  FUNC_data_concentration_corrected = master_list$pca_analysis$concentration_statTarget_rsd_filtered$sample_qc$plot_Val %>% add_column(sample_data_type = "concentration_corrected", .before =1),
  FUNC_HEADER_run_order = "run_index",
  FUNC_HEADER_plate_id = "plate_id",
  FUNC_HEADER_colour_by = "qc",
  FUNC_HEADER_highlight_by = "qc",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
)



# PLOT: individual analyte control chart ------------------------------------------------
master_list$control_chart <- master_list$environment$user_functions$control_chart$value(
  FUNC_data_area = bind_rows(master_list$data$area_sorted) %>% add_column(sample_data_type = "area", .after=1),
  FUNC_data_response = bind_rows(master_list$data$concentration) %>% add_column(sample_data_type = "concentration_response", .after=1),
  FUNC_data_area_corrected = bind_rows(master_list$data$statTarget) %>% add_column(sample_data_type = "area_corrected", .after=1),
  FUNC_data_response_corrected = bind_rows(master_list$data$concentration_statTarget) %>% add_column(sample_data_type = "concentration_response_corrected", .after=1),
  FUNC_metabolite_list = filter(master_list$templates$SIL_guide, control_chart == TRUE),
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_type_factor",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_OPTION_title = master_list$project_details$project_name,
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
)




# PROCESS: save rda output ------------------------------------------------

# save(master_list,
#      file = paste0(
#        master_list$project_details$project_dir,
#        "/data/rda/", Sys.Date(), 
#        "_qcCheckeR_", 
#        master_list$project_details$project_name, 
#        ".rda"))

# PROCESS: save final csv output ------------------------------------------------

#csv of peak area data
write_csv(x = bind_rows(master_list$data$area_sorted),
          file =paste0(
            master_list$project_details$project_dir,
            "/html_report/", Sys.Date(),
            "_",
            master_list$project_details$project_name,
            "_PEAK_AREA_qcCheckeR_v3.3.csv"))

#csv of concentration data (no filters)
write_csv(x = master_list$data$concentration_bind_plates,
          file =paste0(
            master_list$project_details$project_dir,
            "/html_report/", Sys.Date(),
            "_",
            master_list$project_details$project_name,
            "CONCENTRATION_qcCheckeR_v3.3.csv"))

#csv of concentration data (no filters)

write_csv(x = master_list$data$concentration_statTarget_rsd_filter,
          file =paste0(
            master_list$project_details$project_dir,
            "/html_report/", Sys.Date(),
            "_",
            master_list$project_details$project_name,
            "_qcCheckeR_v3.3.csv"))

# PROCESS: render html report ---------------------------------------------

# fileConn<-file(paste0(master_list$project_details$project_dir, "/html_report/lipid_exploreR_report_templatev3.3.R"))
# writeLines(httr::GET(url = paste0(master_list$project_details$github_master_dir, "/templates/TEMPLATE_lipidExploreR_report_v3.3.R")) %>%
#              httr::content(as = "text"), fileConn)
# close(fileConn)


rmarkdown::render(input = paste0(master_list$project_details$project_dir, "/html_report/lipid_exploreR_report_templatev3.3.R"),
                  output_format = "html_document",
                  output_dir = paste0(master_list$project_details$project_dir, "/html_report"),
                  output_file = paste0(Sys.Date(), "_", master_list$project_details$project_name, "_lipidExploreR_qcCheckeR_report_v3.3.html")
)

browseURL(url = paste0(master_list$project_details$project_dir, 
                       "/html_report/",
                       Sys.Date(), "_", master_list$project_details$project_name, "_lipidExploreR_qcCheckeR_report_v3.3.html")
)

