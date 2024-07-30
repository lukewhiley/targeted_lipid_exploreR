# . ------------------------------------------------------------------------------------------------------------------------------  
# PROJECT SETUP: --------------------------------------
## 1. Welcome section/project set upload skylineR --------------------------------------

#welcome messages
if(!exists("master_list")){
  dlg_message("Welcome to lipid qc exploreR! :-)", type = 'ok'); dlg_message("Please run lipid SkylineR notebook prior to running this notebook", type = 'ok'); dlg_message("Now open master_list.rda file produced by SkylineR", type = 'ok')
  # load rda file
  load(file = file.choose())
}

#set version of lipidExploreR used
master_list$project_details$lipidExploreR_version <- "3.35"
master_list$project_details$qcCheckR_version <- "3.35"
master_list$summary_tables$project_summary$value[which(master_list$summary_tables$project_summary$`Project detail` == "lipidExploreR version")] <- "3.35"
master_list$summary_tables$area_project_summary$value[which(master_list$summary_tables$area_project_summary$`Project detail` == "lipidExploreR version")] <- "3.3"


## 2. Source LGW github functions --------------------------------------
master_list$project_details$github_master_dir = "~/Documents/GitHub/targeted_lipid_exploreR_v3/"
#pca scores plot function
master_list$environment$user_functions$pca <- source(paste0(master_list$project_details$github_master_dir,
                                                            "/functions/FUNC_lipidExploreR_PCA_ggplotv3.35.R"))
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
                                                                       "/functions/FUNC_lipidExploreR_signal_drift_correct_v3.3.R"))
#run order vs PC plots
master_list$environment$user_functions$pc_run_plot <- source(paste0(master_list$project_details$github_master_dir,
                                                                    "/functions/FUNC_lipidExploreR_PCA_runOrder_ggplot.R"))
#control chart check
master_list$environment$user_functions$control_chart <- source(paste0(master_list$project_details$github_master_dir,
                                                                      "/functions/FUNC_lipidExploreR_controlChart.R"))

## 3. Set plot color/fill/shape/size --------------------------------------

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

## 4. Load templates/guides --------------------------------------
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

# . ------------------------------------------------------------------------------------------------------------------------------  

# PHASE 1: RAW PEAK AREA (NO SIGNAL DRIFT OR BATCH CORRECTION) ------------------------------------

## Data processing -------------------

### 1. Transpose data to standard metabolomics structure (features in columns, samples in rows) ---------------------------------------
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
  
  #add sample_data_source column
  master_list$data$area_transposed[[idx_data]] <- master_list$data$area_transposed[[idx_data]] %>%
    add_column(
      sample_data_source = factor("uncorrectedPeakArea",
                                  levels = c("uncorrectedPeakArea",
                                             "statTargetCorrectedData"),
                                  ordered = TRUE) ,
      .after ="sample_name"
    )
}


### 2. Sort by run order and add annotation data ------------------------------- 
#list for storing concentration data area_sorted by run order
#master_list$summary_tables$area_summary <- list()
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
}

### 3. Data filter: missing values filter by flags --------------------
#### a. Remove all samples that have > 50% missing values in peak area (removes any mis-injections etc that may be present in the data) ----

master_list$process_lists$mvSamples <-
  master_list$data$area_sorted %>%
  bind_rows() %>%
  select(
    sample_run_index,
    sample_name,
    sample_batch,
    sample_plate_id,
    sample_type_factor
  )

#zero values [samples]
master_list$process_lists$mvSamples[["zeroValues[lipidTarget]"]] <- rowSums(
  x = (master_list$data$area_sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix()) == 0, na.rm =T)

#zero values [SIL Int.Stds]
master_list$process_lists$mvSamples[["zeroValues[SIL.Int.Stds]"]] <- rowSums(
  x = (master_list$data$area_sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix()) == 0, na.rm =T)

#na values [samples]
master_list$process_lists$mvSamples[["naValues[lipidTarget]"]] <- rowSums(
  x = (master_list$data$area_sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix() %>%
         is.na()), na.rm =T)

#na values [SIL Int.Stds]
master_list$process_lists$mvSamples[["naValues[SIL.Int.Stds]"]] <- rowSums(
  x = (master_list$data$area_sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix() %>% 
         is.na()), na.rm =T)

#nan values [samples]
master_list$process_lists$mvSamples[["nanValues[lipidTarget]"]] <- rowSums(
  x = (master_list$data$area_sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix() %>%
         is.nan()), na.rm =T)

#nan values [SIL Int.Stds]
master_list$process_lists$mvSamples[["nanValues[SIL.Int.Stds]"]] <- rowSums(
  x = (master_list$data$area_sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix() %>% 
         is.nan()), na.rm =T)

#inf values [samples]
master_list$process_lists$mvSamples[["infValues[lipidTarget]"]] <- rowSums(
  x = (master_list$data$area_sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix() %>%
         is.infinite()), na.rm =T)

#inf values [SIL Int.Stds]
master_list$process_lists$mvSamples[["infValues[SIL.Int.Stds]"]] <- rowSums(
  x = (master_list$data$area_sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix() %>% 
         is.infinite()), na.rm =T)

#total missing values [samples]
master_list$process_lists$mvSamples[["totalMissingValues[lipidTarget]"]] <- master_list$process_lists$mvSamples %>%
  select(-contains("sample_")) %>%
  select(-contains("SIL")) %>%
  as.matrix() %>%
  rowSums()

#total missing values[SIL.Int.stds]
master_list$process_lists$mvSamples[["totalMissingValues[SIL.Int.Stds]"]] <- master_list$process_lists$mvSamples %>%
  select(-contains("sample_")) %>%
  select(contains("SIL")) %>%
  as.matrix() %>%
  rowSums()

#sample removed?
master_list$process_lists$mvSamples$sampleRemoved <- FALSE
#remove where missing data >50% total lipidTargets
master_list$process_lists$mvSamples$sampleRemoved[which(
  master_list$process_lists$mvSamples$`totalMissingValues[lipidTarget]` > ((
    bind_rows(master_list$data$area_sorted) %>%
      select(-contains("sample")) %>%
      select(-contains("SIL")) %>%
      ncol())*0.5)
)] <- TRUE
#remove where missing data >50% total SIL.int.Stds
master_list$process_lists$mvSamples$sampleRemoved[which(
  master_list$process_lists$mvSamples$`totalMissingValues[SIL.Int.Stds]` > ((bind_rows(
    master_list$data$area_sorted) %>%
      select(-contains("sample")) %>%
      select(contains("SIL")) %>%
      ncol())*0.5)
)] <- TRUE

#create filtered dataset
master_list$data$area_missVal_filtered <- master_list$data$area_sorted

for(idx_data in names(master_list$data$area_sorted)){
  master_list$data$area_missVal_filtered[[idx_data]] <- master_list$data$area_missVal_filtered[[idx_data]] %>%
    filter(sample_name %in% master_list$process_lists$mvSamples$sample_name[which(
      master_list$process_lists$mvSamples$sampleRemoved == FALSE)
    ])
}

#### b. Remove all lipidTargets that have > 50% missing values in peak area (removes any bad lipidt MRM ransitions in dataSet) ----

master_list$process_lists$mvLipids <- tibble(
  lipid = 
    master_list$data$area_missVal_filtered  %>%
    bind_rows() %>%
    select(-contains("sample")) %>%
    names()
)

for(idx_data in names(master_list$data$area_missVal_filtered)){
  master_list$process_lists$mvLipids[[paste0(idx_data,".zeroValues")]] <- colSums(
    x= (master_list$data$area_missVal_filtered[[idx_data]] %>%
          select(-contains("sample")) %>%
          as.matrix()) == 0, na.rm = T)
  
  #na values
  master_list$process_lists$mvLipids[[paste0(idx_data,".naValues")]] <- colSums(
    x= (master_list$data$area_missVal_filtered[[idx_data]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.na(), na.rm = T)
  
  #nan values
  master_list$process_lists$mvLipids[[paste0(idx_data,".nanValues")]] <- colSums(
    x= (master_list$data$area_missVal_filtered[[idx_data]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.nan(), na.rm = T)
  
  #inf values
  master_list$process_lists$mvLipids[[paste0(idx_data,".infValues")]] <- colSums(
    x= (master_list$data$area_missVal_filtered[[idx_data]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.infinite(), na.rm = T)
  
  #total missing values for plate
  master_list$process_lists$mvLipids[[paste0(idx_data,".totalMissingValues")]] <-  rowSums(x= (master_list$process_lists$mvLipids %>%
                                                                                                 select(-lipid, -contains("failPlate"))%>%
                                                                                                 as.matrix()),
                                                                                           na.rm = T)
  
  #does lipid fail for the plate?
  master_list$process_lists$mvLipids[[paste0(idx_data, ".failPlate")]] <- FALSE
  master_list$process_lists$mvLipids[[paste0(idx_data, ".failPlate")]][which(
    master_list$process_lists$mvLipids[[paste0(idx_data,".totalMissingValues")]] > (nrow(master_list$data$area_missVal_filtered[[idx_data]]) * 0.5)
  )] <- TRUE
  
}

#for all plates
master_list$process_lists$mvLipids[[paste0("allPlates.zeroValues")]] <- colSums(
  x= (master_list$data$area_missVal_filtered %>%
        bind_rows() %>%
        select(-contains("sample")) %>%
        as.matrix()) == 0, na.rm = T)

#na values
master_list$process_lists$mvLipids[[paste0("allPlates.naValues")]] <- colSums(
  x= (master_list$data$area_missVal_filtered %>%
        bind_rows() %>%
        select(-contains("sample")) %>%
        as.matrix()) %>%
    is.na(), na.rm = T)

#nan values
master_list$process_lists$mvLipids[[paste0("allPlates.nanValues")]] <- colSums(
  x= (master_list$data$area_missVal_filtered %>%
        bind_rows() %>%
        select(-contains("sample")) %>%
        as.matrix()) %>%
    is.nan(), na.rm = T)

#inf values
master_list$process_lists$mvLipids[[paste0("allPlates.infValues")]] <- colSums(
  x= (master_list$data$area_missVal_filtered %>%
        bind_rows() %>%
        select(-contains("sample")) %>%
        as.matrix()) %>%
    is.infinite(), na.rm = T)

#total missing values for plate
master_list$process_lists$mvLipids[[paste0("allPlates.totalMissingValues")]] <-  rowSums(x= (master_list$process_lists$mvLipids %>%
                                                                                               select(contains("allPlates"))%>%
                                                                                               as.matrix()),
                                                                                         na.rm = T)

#does lipid fail for across all plates?
master_list$process_lists$mvLipids[[paste0("allPlates.failPlate")]] <- FALSE
master_list$process_lists$mvLipids[[paste0("allPlates.failPlate")]] [which(
  master_list$process_lists$mvLipids[[paste0("allPlates.totalMissingValues")]] > (nrow(bind_rows(master_list$data$area_missVal_filtered)) * 0.5)
)] <- TRUE

# add fail.filter column if lipid failed for a plate or for all plates
master_list$process_lists$mvLipids[["lipidRemoved"]] <- FALSE
master_list$process_lists$mvLipids[["lipidRemoved"]][which(
  rowSums(x= (master_list$process_lists$mvLipids %>%
                select(contains("failPlate")) %>%
                as.matrix()) == TRUE, na.rm = T) > 0
)] <- TRUE


#create filtered dataset
for(idx_data in names(master_list$data$area_missVal_filtered)){
  master_list$data$area_missVal_filtered[[idx_data]] <- master_list$data$area_missVal_filtered[[idx_data]] %>%
    select(-any_of(
      master_list$process_lists$mvLipids$lipid[which(master_list$process_lists$mvLipids$lipidRemoved == TRUE)]
    ))
}

### 4. Impute missing values [min/2 imputation (missing assumed < LOD)] -----------------------------------------------------
#Imputation of the remaining zero value and missing data 
#Imputation is completed using x/2, where x is minimum intensity of that feature in the batch

master_list$data$area_impute <- list()

for(idx_data in names(master_list$data$area_missVal_filtered)){
  master_list$data$area_impute[[idx_data]] <- master_list$environment$user_functions$impute_data$value(
    FUNC_data = master_list$data$area_missVal_filtered[[idx_data]],
    FUNC_metabolite_list = master_list$data$area_missVal_filtered[[idx_data]] %>% 
      select(-contains("sample")) %>% names(),
    FUNC_option_impute_missing_data = TRUE)
}

### 5. Calculate response ratio and concentration calculations  ------------------------------------------------------

#* Calculation of target metabolite/stable isotope labelled (SIL) internal standard ratio, using predefined target metabolite/internal standard pairs
#* Conversion of response ratio to concentration values using single point calibration


#### a. Performed on raw area data (non-filtered) --------------------------------

#set empty list to store output data
master_list$data$area_response <- list()
master_list$data$area_concentration <- list()

for(idx_data in names(master_list$data$area_sorted)){
  loop_data <- master_list$environment$user_functions$conc_calc$value(
    FUNC_data = master_list$data$area_sorted[[idx_data]],
    FUNC_SIL_list = master_list$data$area_sorted[[idx_data]] %>% 
      select(-contains("sample")) %>% select(contains("SIL")) %>% names(),
    FUNC_SIL_guide = master_list$templates$SIL_guide,
    FUNC_conc_guide = master_list$templates$conc_guide
  )
  master_list$data$area_response[[idx_data]] <- loop_data$response; master_list$data$area_concentration[[idx_data]] <- loop_data$concentration
}

#### b. Performed on missing value filtered and imputed data -------------------------------------------
#* Calculation of target metabolite/stable isotope labelled (SIL) internal standard ratio, using predefined target metabolite/internal standard pairs
#* Conversion of response ratio to concentration values using single point calibration
#* Performed on imputed and filtered data

#set empty list to store output data
master_list$data$area_impute_response <- list()
master_list$data$area_impute_concentration <- list()

for(idx_data in names(master_list$data$area_impute)){
  loop_data <- master_list$environment$user_functions$conc_calc$value(
    FUNC_data = master_list$data$area_impute[[idx_data]],
    FUNC_SIL_list = master_list$data$area_impute[[idx_data]] %>% 
      select(-contains("sample")) %>% select(contains("SIL")) %>% names(),
    FUNC_SIL_guide = master_list$templates$SIL_guide,
    FUNC_conc_guide = master_list$templates$conc_guide
  )
  master_list$data$area_impute_response[[idx_data]] <- loop_data$response; master_list$data$area_impute_concentration[[idx_data]] <- loop_data$concentration
}

#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))

#.----

# PHASE 2: STATTARGET - SIGNAL DRIFT AND/OR BATCH CORRECTED DATA ------------------------------------

#create batch correction directory
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/batch_correction"))){
  dir.create(paste0(master_list$project_details$project_dir, "/data/batch_correction"))
}

## Data processing ---------------------------------------------------------
### 1. statTarget signal drift | batch correction ---------------------------------------------------------

#list for storing signal drift corrected data (per project)
master_list$data$statTarget_sorted <- list()

#run batch correction 
statTarget_data <- master_list$environment$user_functions$signal_correct$value(
  FUNC_project_directory = paste0(master_list$project_details$project_dir,
                                  "/data/batch_correction"),
  FUNC_data = master_list$data$area_sorted %>% bind_rows(), 
  FUNC_metabolite_list = master_list$data$area_sorted %>% bind_rows() %>%
    select(!contains("sample")) %>% names(),
  FUNC_header_sample_id = "sample_name",
  FUNC_header_batch = "sample_plate_id",
  FUNC_header_sample_type = "sample_type",
  FUNC_header_run_order = "sample_run_index",
  FUNC_option_qc_type = "vltr"
)

for(idx_data in unique(statTarget_data$sample_plate_id)){
  master_list$data$statTarget_sorted[[idx_data]] <- statTarget_data %>%
    filter(sample_plate_id == idx_data)
  
  master_list$data$statTarget_sorted[[idx_data]]$sample_data_source <- factor("statTargetCorrectedData",
                                                                              levels = c("uncorrectedPeakArea",
                                                                                         "statTargetCorrectedData"),
                                                                              ordered = TRUE) 
}

### 2. Data filter: missing values filter by flags --------------------
# * Step 1: Remove all samples that have > 50% missing values in peak area (removes any mis-injections etc that may be present in the data)
# * Step 2: Remove all metabolite features that have > 50% missing values (zero, NA, NaN etc)

#create empty lists for storing outputs
master_list$data$statTarget_missVal_filtered <- list()
master_list$process_lists$statTarget_missVal_filtered <- list()

for(idx_data in names(master_list$data$statTarget_sorted)){
  master_list$process_lists$statTarget_missVal_filtered[[idx_data]] <- list()
  
  for(idx_sample_type in c("sample", tolower(master_list$project_details$qc_type))){
    master_list$process_lists$statTarget_missVal_filtered[[idx_data]][[idx_sample_type]] <- list()
    #run missing value filter function
    master_list$process_lists$statTarget_missVal_filtered[[idx_data]][[idx_sample_type]] <- master_list$environment$user_functions$miss_value_filter$value(
      FUNC_data = master_list$data$statTarget_sorted[[idx_data]] %>%
        filter(sample_type == idx_sample_type),
      FUNC_metabolite_list = master_list$data$statTarget_sorted[[idx_data]] %>%
        select(-contains("sample")) %>% names(),
      FUNC_IS_tag = "SIL",
      FUNC_OPTION_missing_value_threshold_sample = 0.50, #decimal % of missing value threshold before sample is removed from dataset
      FUNC_OPTION_missing_value_threshold_feature = 0.50, #decimal % of missing value threshold before feature is removed from dataset
      FUNC_OPTION_intensity_threshold = 5000)
  }
  
  #bind fail sample list
  master_list$process_lists$statTarget_missVal_filtered[[idx_data]]$sample_fail_list <- c(
    master_list$process_lists$statTarget_missVal_filtered[[idx_data]]$sample$mv_samples_fail,
    master_list$process_lists$statTarget_missVal_filtered[[idx_data]]$qc$mv_samples_fail) %>%
    unique()
  
  #bind fail feature list
  master_list$process_lists$statTarget_missVal_filtered[[idx_data]]$feature_fail_list <- c(
    master_list$process_lists$statTarget_missVal_filtered[[idx_data]]$sample$mv_features_fail,
    master_list$process_lists$statTarget_missVal_filtered[[idx_data]]$qc$mv_features_fail) %>%
    unique()
  
  #create filtered dataset per plate for next phase
  master_list$data$statTarget_missVal_filtered[[idx_data]] <- master_list$data$statTarget_sorted[[idx_data]] %>%
    filter(!sample_name %in% master_list$process_lists$statTarget_missVal_filtered[[idx_data]]$sample_fail_list) %>% #select only pass samples
    select(-any_of(master_list$process_lists$statTarget_missVal_filtered[[idx_data]]$feature_fail_list)) #select only pass features
}

### 3. Select data features that are common for all plates ------------
master_list$data$statTarget_common_metabolite_filter <- list()

#step 1: find common features
temp_common_features <- names(master_list$data$statTarget_missVal_filtered[[1]])
for(idx_data in names(master_list$data$statTarget_missVal_filtered)){
  temp_common_features <- intersect(temp_common_features,
                                    names(master_list$data$statTarget_missVal_filtered[[idx_data]]))
}

for(idx_data in names(master_list$data$statTarget_missVal_filtered)){
  master_list$data$statTarget_common_metabolite_filter[[idx_data]] <- master_list$data$statTarget_missVal_filtered[[idx_data]] %>%
    select(any_of(temp_common_features))
}

### 4. Impute missing values [min/2 imputation (missing assumed < LOD)] -----------------------------------------------------
#Imputation of the remaining zero value and missing data 
#Imputation is completed using x/2, where x is minimum intensity of that feature in the batch

master_list$data$statTarget_impute <- list()

for(idx_data in names(master_list$data$statTarget_common_metabolite_filter)){
  master_list$data$statTarget_impute[[idx_data]] <- master_list$environment$user_functions$impute_data$value(
    FUNC_data = master_list$data$statTarget_common_metabolite_filter[[idx_data]],
    FUNC_metabolite_list = master_list$data$statTarget_common_metabolite_filter[[idx_data]] %>% 
      select(-contains("sample")) %>% names(),
    FUNC_option_impute_missing_data = TRUE)
}

### 5. Calculate response ratio and concentration calculations  ------------------------------------------------------
#* Calculation of target metabolite/stable isotope labelled (SIL) internal standard ratio, using predefined target metabolite/internal standard pairs
#* Conversion of response ratio to concentration values using single point calibration

#### a: Performed on raw area data (non-filtered - but is imputed already by statTarget (min/2)) --------------------------------

#set empty list to store output data
master_list$data$statTarget_response <- list()
master_list$data$statTarget_concentration <- list()

for(idx_data in names(master_list$data$statTarget_sorted)){
  loop_data <- master_list$environment$user_functions$conc_calc$value(
    FUNC_data = master_list$data$statTarget_sorted[[idx_data]],
    FUNC_SIL_list = master_list$data$statTarget_sorted[[idx_data]] %>% 
      select(-contains("sample")) %>% select(contains("SIL")) %>% names(),
    FUNC_SIL_guide = master_list$templates$SIL_guide,
    FUNC_conc_guide = master_list$templates$conc_guide
  )
  master_list$data$statTarget_response[[idx_data]] <- loop_data$response; master_list$data$statTarget_concentration[[idx_data]] <- loop_data$concentration
}

#### b: Performed on missing value filtered and imputed data -------------------------------------------
#* Calculation of target metabolite/stable isotope labelled (SIL) internal standard ratio, using predefined target metabolite/internal standard pairs
#* Conversion of response ratio to concentration values using single point calibration
#* Performed on imputed and filtered data

#set empty list to store output data
master_list$data$statTarget_impute_response <- list()
master_list$data$statTarget_impute_concentration <- list()

for(idx_data in names(master_list$data$statTarget_impute)){
  loop_data <- master_list$environment$user_functions$conc_calc$value(
    FUNC_data = master_list$data$statTarget_impute[[idx_data]],
    FUNC_SIL_list = master_list$data$statTarget_impute[[idx_data]] %>% 
      select(-contains("sample")) %>% select(contains("SIL")) %>% names(),
    FUNC_SIL_guide = master_list$templates$SIL_guide,
    FUNC_conc_guide = master_list$templates$conc_guide
  )
  master_list$data$statTarget_impute_response[[idx_data]] <- loop_data$response; master_list$data$statTarget_impute_concentration[[idx_data]] <- loop_data$concentration
}

#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))  
#.----

#PHASE 3. FILTER BY %RSD TO CREATE FINAL DATASETS ----

### 1. Data filter: %RSD filter across user selected LTRs -------------------------
#### a. intraPlate ------
#first create a table of rsd performance
master_list$process_lists$rsd_filter <- tibble(
  lipid =  names(master_list$data$area_sorted[[1]] %>%
                   filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
                   select(-contains("sample"))))

for(idx_data in names(master_list$data$area_sorted)){
  
  peakArea_qc_data <- master_list$data$area_sorted[[idx_data]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  
  concentration_qc_data <- master_list$data$area_concentration[[idx_data]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  
  preProcessedConcentration_qc_data <- master_list$data$area_impute_concentration[[idx_data]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  
  statTargetArea_qc_data <- master_list$data$statTarget_sorted[[idx_data]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  
  statTargetConcentration_qc_data <- master_list$data$statTarget_concentration[[idx_data]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  
  preProcessedStatTargetConcentration_qc_data <- master_list$data$statTarget_impute_concentration[[idx_data]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  
  #extract RSD performance in peakAreaData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(peakArea_qc_data),
      !! paste0("peakArea.", idx_data) := (colSds(as.matrix(peakArea_qc_data))*100)/colMeans(as.matrix(peakArea_qc_data))
    ), 
    by = "lipid")
  
  #extract RSD performance in concentrationData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(concentration_qc_data),
      !! paste0("concentration.", idx_data) := (colSds(as.matrix(concentration_qc_data))*100)/colMeans(as.matrix(concentration_qc_data))
    ), 
    by = "lipid")
  
  #extract RSD performance in concentrationData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(preProcessedConcentration_qc_data),
      !! paste0("preProcessedConcentration.", idx_data) := (colSds(as.matrix(preProcessedConcentration_qc_data))*100)/colMeans(as.matrix(preProcessedConcentration_qc_data))
    ), 
    by = "lipid")
  
  
  #extract RSD performance in statTargetAreaData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(statTargetArea_qc_data),
      !! paste0("statTargetArea.", idx_data) := (colSds(as.matrix(statTargetArea_qc_data))*100)/colMeans(as.matrix(statTargetArea_qc_data))
    ), 
    by = "lipid")
  
  #extract RSD performance in statTargetConcentrationData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(statTargetConcentration_qc_data),
      !! paste0("statTargetConcentration.", idx_data) := (colSds(as.matrix(statTargetConcentration_qc_data))*100)/colMeans(as.matrix(statTargetConcentration_qc_data))
    ), 
    by = "lipid")
  
  #extract RSD performance in preProcessedStatTargetConcentrationData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(preProcessedStatTargetConcentration_qc_data),
      !! paste0("preProcessedStatTargetConcentration.", idx_data) := (colSds(as.matrix(preProcessedStatTargetConcentration_qc_data))*100)/colMeans(as.matrix(preProcessedStatTargetConcentration_qc_data))
    ), 
    by = "lipid")
}

#### b. interPlate -----------------------------
# make rsd values for all plates
peakArea_qc_data <- bind_rows(master_list$data$area_sorted) %>%
  filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
  select(-contains("sample"))

concentration_qc_data <- bind_rows(master_list$data$area_concentration) %>%
  filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
  select(-contains("sample"))

preProcessedConcentration_qc_data <- bind_rows(master_list$data$area_impute_concentration) %>%
  filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
  select(-contains("sample"))

#statTargetData
statTargetArea_qc_data <- bind_rows(master_list$data$statTarget_sorted) %>%
  filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
  select(-contains("sample"))

statTargetConcentration_qc_data <- bind_rows(master_list$data$statTarget_concentration) %>%
  filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
  select(-contains("sample"))

statTargetConcentration_qc_data <- bind_rows(master_list$data$statTarget_impute_concentration) %>%
  filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
  select(-contains("sample"))

#extract RSD performance in peakAreaData
master_list$process_lists$rsd_filter <- left_join(
  master_list$process_lists$rsd_filter,
  tibble(
    lipid =  names(peakArea_qc_data),
    !! paste0("peakArea.allPlates") := (colSds(as.matrix(peakArea_qc_data))*100)/colMeans(as.matrix(peakArea_qc_data))
  ), 
  by = "lipid")

#extract RSD performance in concentrationData
master_list$process_lists$rsd_filter <- left_join(
  master_list$process_lists$rsd_filter,
  tibble(
    lipid =  names(concentration_qc_data),
    !! paste0("concentration.allPlates") := (colSds(as.matrix(concentration_qc_data))*100)/colMeans(as.matrix(concentration_qc_data))
  ), 
  by = "lipid")

#extract RSD performance in preProcessedConcentrationData
master_list$process_lists$rsd_filter <- left_join(
  master_list$process_lists$rsd_filter,
  tibble(
    lipid =  names(preProcessedConcentration_qc_data),
    !! paste0("preProcessedConcentration.allPlates") := (colSds(as.matrix(preProcessedConcentration_qc_data))*100)/colMeans(as.matrix(preProcessedConcentration_qc_data))
  ), 
  by = "lipid")


#extract RSD performance in statTargetAreaData
master_list$process_lists$rsd_filter <- left_join(
  master_list$process_lists$rsd_filter,
  tibble(
    lipid =  names(statTargetArea_qc_data),
    !! paste0("statTargetArea.allPlates") := (colSds(as.matrix(statTargetArea_qc_data))*100)/colMeans(as.matrix(statTargetArea_qc_data))
  ), 
  by = "lipid")

#extract RSD performance in statTargetConcentrationData
master_list$process_lists$rsd_filter <- left_join(
  master_list$process_lists$rsd_filter,
  tibble(
    lipid =  names(statTargetConcentration_qc_data),
    !! paste0("statTargetConcentration.allPlates") := (colSds(as.matrix(statTargetConcentration_qc_data))*100)/colMeans(as.matrix(statTargetConcentration_qc_data))
  ), 
  by = "lipid")

master_list$process_lists$rsd_filter <- left_join(
  master_list$process_lists$rsd_filter,
  tibble(
    lipid =  names(preProcessedStatTargetConcentration_qc_data),
    !! paste0("preProcessedStatTargetConcentration.allPlates") := (colSds(as.matrix(preProcessedStatTargetConcentration_qc_data))*100)/colMeans(as.matrix(preProcessedStatTargetConcentration_qc_data))
  ), 
  by = "lipid")

### 2. Make final datasets ----
#create keep list for rsd filter (areaConcentration)
master_list$process_lists$rsd_filter_area_keepList <- (
  master_list$process_lists$rsd_filter %>%
    filter(preProcessedConcentration.allPlates <30))[["lipid"]]
#create keep list for rsd filter (areaConcentration)
master_list$process_lists$rsd_filter_statTarget_keepList <- (
  master_list$process_lists$rsd_filter %>%
    filter(preProcessedStatTargetConcentration.allPlates<30))[["lipid"]]

#bind plateRows and filter results to get final dataSet
#uncorrectedPeakAre
master_list$data$area_preProcessed <- master_list$data$area_impute_concentration %>%
  bind_rows() %>%
  select(contains("sample"), any_of(master_list$process_lists$rsd_filter_area_keepList))

#statTargetCorrectedArea
master_list$data$statTarget_preProcessed <- master_list$data$statTarget_impute_concentration %>%
  bind_rows() %>%
  select(contains("sample"), any_of(master_list$process_lists$rsd_filter_statTarget_keepList))

#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))

#. ----

# PHASE 4. SummaryReports ------------------------ 
## peakArea dataSetSummary ---------------------------------------
metric =  c("totalSamples", "studySamples", "ltrSamples", "vltrSamples", "sltrSamples", "pqcSamples", 
            "lipidTargets", "SIL.IntStds", 
            "zeroValues[lipidTargets]", "zeroValues[SIL.IntStds]", 
            "naValues[lipidTargets]", "naValues[SIL.IntStds]", 
            "infValues[lipidTargets]", "infValues[SIL.IntStds]", 
            "nanValues[lipidTargets]", "nanValues[SIL.IntStds]",
            "rsdQc<30%", "rsdQc<20%", "rsdQc<10%")
### 1. intraPlate ----
#### a. peakArea ---------------------------
master_list$summary_tables$peakAreaOverview <- tibble(metric = metric)
for(idx_data in names(master_list$data$area_sorted)){
  
  master_list$summary_tables$peakAreaOverview <- left_join(
    master_list$summary_tables$peakAreaOverview,
    rbind(
      c("totalSamples", nrow(master_list$data$area_sorted[[idx_data]])),
      c("studySamples", nrow(master_list$data$area_sorted[[idx_data]] %>% filter(sample_type_factor == "sample"))),
      c("ltrSamples", nrow(master_list$data$area_sorted[[idx_data]] %>% filter(sample_type_factor == "ltr"))),
      c("vltrSamples", nrow(master_list$data$area_sorted[[idx_data]] %>% filter(sample_type_factor == "vltr"))),
      c("sltrSamples", nrow(master_list$data$area_sorted[[idx_data]] %>% filter(sample_type_factor == "sltr"))),
      c("pqcSamples", nrow(master_list$data$area_sorted[[idx_data]] %>% filter(sample_type_factor == "pqc"))),
      c("lipidTargets", ncol(master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample"), - contains("SIL")))),
      c("SIL.IntStds", ncol(master_list$data$area_sorted[[idx_data]] %>% select(contains("SIL")))),
      c("zeroValues[lipidTargets]", length(which(master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample"), -contains("SIL")) ==0))),
      c("zeroValues[SIL.IntStds]", length(which(master_list$data$area_sorted[[idx_data]] %>% select(contains("SIL")) ==0))),
      c("naValues[lipidTargets]", length(which(is.na(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("naValues[SIL.IntStds]", length(which(is.na(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(contains("SIL"))))))),
      c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("infValues[SIL.IntStds]", length(which(is.infinite(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(contains("SIL"))))))),
      c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("nanValues[SIL.IntStds]", length(which(is.nan(as.matrix(master_list$data$area_sorted[[idx_data]] %>% select(contains("SIL"))))))),
      c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("peakArea.", idx_data)]] <30))),
      c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("peakArea.", idx_data)]] <20))),
      c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("peakArea.", idx_data)]] <10)))
    ) %>%
      as_tibble() %>%
      rename(metric = V1,
             !! paste0("peakArea.", idx_data) := V2),
    by = "metric"
  )
  #change column to numeric
  master_list$summary_tables$peakAreaOverview[[paste0("peakArea.", idx_data)]] <- as.numeric(master_list$summary_tables$peakAreaOverview[[paste0("peakArea.", idx_data)]])
  
  
  #### b. peakConcentration -----------------
  
  master_list$summary_tables$peakAreaOverview <- left_join(
    master_list$summary_tables$peakAreaOverview,
    rbind(
      c("totalSamples", nrow(master_list$data$area_concentration[[idx_data]])),
      c("studySamples", nrow(master_list$data$area_concentration[[idx_data]] %>% filter(sample_type_factor == "sample"))),
      c("ltrSamples", nrow(master_list$data$area_concentration[[idx_data]] %>% filter(sample_type_factor == "ltr"))),
      c("vltrSamples", nrow(master_list$data$area_concentration[[idx_data]] %>% filter(sample_type_factor == "vltr"))),
      c("sltrSamples", nrow(master_list$data$area_concentration[[idx_data]] %>% filter(sample_type_factor == "sltr"))),
      c("pqcSamples", nrow(master_list$data$area_concentration[[idx_data]] %>% filter(sample_type_factor == "pqc"))),
      c("lipidTargets", ncol(master_list$data$area_concentration[[idx_data]] %>% select(-contains("sample"), - contains("SIL")))),
      c("SIL.IntStds", NA),
      c("zeroValues[lipidTargets]", length(which(master_list$data$area_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL")) ==0))),
      c("zeroValues[SIL.IntStds]", NA),
      c("naValues[lipidTargets]", length(which(is.na(as.matrix(master_list$data$area_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("naValues[SIL.IntStds]", NA),
      c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(master_list$data$area_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("infValues[SIL.IntStds]", NA),
      c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(master_list$data$area_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("nanValues[SIL.IntStds]", NA),
      c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("concentration.", idx_data)]] <30))),
      c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("concentration.", idx_data)]] <20))),
      c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("concentration.", idx_data)]] <10)))
    ) %>%
      as_tibble() %>%
      rename(metric = V1,
             !! paste0("concentration.", idx_data) := V2),
    by = "metric") 
  #change column to numeric
  master_list$summary_tables$peakAreaOverview[[paste0("concentration.", idx_data)]] <- as.numeric(master_list$summary_tables$peakAreaOverview[[paste0("concentration.", idx_data)]])
  
  
  #### c. concentration (preProcessed) --------------
  master_list$summary_tables$peakAreaOverview <- left_join(
    master_list$summary_tables$peakAreaOverview,
    rbind(
      c("totalSamples", nrow(master_list$data$area_impute_concentration[[idx_data]])),
      c("studySamples", nrow(master_list$data$area_impute_concentration[[idx_data]] %>% filter(sample_type_factor == "sample"))),
      c("ltrSamples", nrow(master_list$data$area_impute_concentration[[idx_data]] %>% filter(sample_type_factor == "ltr"))),
      c("vltrSamples", nrow(master_list$data$area_impute_concentration[[idx_data]] %>% filter(sample_type_factor == "vltr"))),
      c("sltrSamples", nrow(master_list$data$area_impute_concentration[[idx_data]] %>% filter(sample_type_factor == "sltr"))),
      c("pqcSamples", nrow(master_list$data$area_impute_concentration[[idx_data]] %>% filter(sample_type_factor == "pqc"))),
      c("lipidTargets", ncol(master_list$data$area_impute_concentration[[idx_data]] %>% select(-contains("sample"), - contains("SIL")))),
      c("SIL.IntStds", NA),
      c("zeroValues[lipidTargets]", length(which(master_list$data$area_impute_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL")) ==0))),
      c("zeroValues[SIL.IntStds]", NA),
      c("naValues[lipidTargets]", length(which(is.na(as.matrix(master_list$data$area_impute_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("naValues[SIL.IntStds]", NA),
      c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(master_list$data$area_impute_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("infValues[SIL.IntStds]", NA),
      c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(master_list$data$area_impute_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("nanValues[SIL.IntStds]", NA),
      c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedConcentration.", idx_data)]] <30))),
      c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedConcentration.", idx_data)]] <20))),
      c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedConcentration.", idx_data)]] <10)))
    ) %>%
      as_tibble() %>%
      rename(metric = V1,
             !! paste0("preProcessedConcentration.", idx_data) := V2),
    by = "metric") 
  #change column to numeric
  master_list$summary_tables$peakAreaOverview[[paste0("preProcessedConcentration.", idx_data)]] <- as.numeric(master_list$summary_tables$peakAreaOverview[[paste0("preProcessedConcentration.", idx_data)]])
  
}

### 2. interPlate ----

#### a.  peakArea ---------------------------

master_list$summary_tables$peakAreaOverview <- left_join(
  master_list$summary_tables$peakAreaOverview,
  rbind(
    c("totalSamples", nrow(bind_rows(master_list$data$area_sorted))),
    c("studySamples", nrow(bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "sample"))),
    c("ltrSamples", nrow(bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "ltr"))),
    c("vltrSamples", nrow(bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "vltr"))),
    c("sltrSamples", nrow(bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "sltr"))),
    c("pqcSamples", nrow(bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "pqc"))),
    c("lipidTargets", ncol(bind_rows(master_list$data$area_sorted) %>% select(-contains("sample"), - contains("SIL")))),
    c("SIL.IntStds", ncol(bind_rows(master_list$data$area_sorted) %>% select(contains("SIL")))),
    c("zeroValues[lipidTargets]", length(which(bind_rows(master_list$data$area_sorted) %>% select(-contains("sample"), -contains("SIL")) ==0))),
    c("zeroValues[SIL.IntStds]", length(which(bind_rows(master_list$data$area_sorted) %>% select(contains("SIL")) ==0))),
    c("naValues[lipidTargets]", length(which(is.na(as.matrix(bind_rows(master_list$data$area_sorted) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("naValues[SIL.IntStds]", length(which(is.na(as.matrix(bind_rows(master_list$data$area_sorted) %>% select(contains("SIL"))))))),
    c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$area_sorted) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("infValues[SIL.IntStds]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$area_sorted) %>% select(contains("SIL"))))))),
    c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(bind_rows(master_list$data$area_sorted) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("nanValues[SIL.IntStds]", length(which(is.nan(as.matrix(bind_rows(master_list$data$area_sorted) %>% select(contains("SIL"))))))),
    c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("peakArea.allPlates")]] <30))),
    c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("peakArea.allPlates")]] <20))),
    c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("peakArea.allPlates")]] <10)))
  ) %>%
    as_tibble() %>%
    rename(metric = V1,
           !! paste0("peakArea.allPlates") := V2),
  by = "metric"
)
#change column to numeric
master_list$summary_tables$peakAreaOverview[[paste0("peakArea.allPlates")]] <- as.numeric(master_list$summary_tables$peakAreaOverview[[paste0("peakArea.allPlates")]])


#### b. peakConcentration -----------------

master_list$summary_tables$peakAreaOverview <- left_join(
  master_list$summary_tables$peakAreaOverview,
  rbind(
    c("totalSamples", nrow(bind_rows(master_list$data$area_concentration))),
    c("studySamples", nrow(bind_rows(master_list$data$area_concentration) %>% filter(sample_type_factor == "sample"))),
    c("ltrSamples", nrow(bind_rows(master_list$data$area_concentration) %>% filter(sample_type_factor == "ltr"))),
    c("vltrSamples", nrow(bind_rows(master_list$data$area_concentration) %>% filter(sample_type_factor == "vltr"))),
    c("sltrSamples", nrow(bind_rows(master_list$data$area_concentration) %>% filter(sample_type_factor == "sltr"))),
    c("pqcSamples", nrow(bind_rows(master_list$data$area_concentration) %>% filter(sample_type_factor == "pqc"))),
    c("lipidTargets", ncol(bind_rows(master_list$data$area_concentration) %>% select(-contains("sample"), - contains("SIL")))),
    c("SIL.IntStds", ncol(bind_rows(master_list$data$area_concentration) %>% select(contains("SIL")))),
    c("zeroValues[lipidTargets]", length(which(bind_rows(master_list$data$area_concentration) %>% select(-contains("sample"), -contains("SIL")) ==0))),
    c("zeroValues[SIL.IntStds]", length(which(bind_rows(master_list$data$area_concentration) %>% select(contains("SIL")) ==0))),
    c("naValues[lipidTargets]", length(which(is.na(as.matrix(bind_rows(master_list$data$area_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("naValues[SIL.IntStds]", length(which(is.na(as.matrix(bind_rows(master_list$data$area_concentration) %>% select(contains("SIL"))))))),
    c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$area_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("infValues[SIL.IntStds]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$area_concentration) %>% select(contains("SIL"))))))),
    c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(bind_rows(master_list$data$area_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("nanValues[SIL.IntStds]", length(which(is.nan(as.matrix(bind_rows(master_list$data$area_concentration) %>% select(contains("SIL"))))))),
    c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("concentration.allPlates")]] <30))),
    c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("concentration.allPlates")]] <20))),
    c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("concentration.allPlates")]] <10)))
  ) %>%
    as_tibble() %>%
    rename(metric = V1,
           !! paste0("concentration.allPlates") := V2),
  by = "metric"
)
#change column to numeric
master_list$summary_tables$peakAreaOverview[[paste0("concentration.allPlates")]] <- as.numeric(master_list$summary_tables$peakAreaOverview[[paste0("concentration.allPlates")]])

#### c. concentration (preProcessed) --------------
master_list$summary_tables$peakAreaOverview <- left_join(
  master_list$summary_tables$peakAreaOverview,
  rbind(
    c("totalSamples", nrow(bind_rows(master_list$data$area_impute_concentration))),
    c("studySamples", nrow(bind_rows(master_list$data$area_impute_concentration) %>% filter(sample_type_factor == "sample"))),
    c("ltrSamples", nrow(bind_rows(master_list$data$area_impute_concentration) %>% filter(sample_type_factor == "ltr"))),
    c("vltrSamples", nrow(bind_rows(master_list$data$area_impute_concentration) %>% filter(sample_type_factor == "vltr"))),
    c("sltrSamples", nrow(bind_rows(master_list$data$area_impute_concentration) %>% filter(sample_type_factor == "sltr"))),
    c("pqcSamples", nrow(bind_rows(master_list$data$area_impute_concentration) %>% filter(sample_type_factor == "pqc"))),
    c("lipidTargets", ncol(bind_rows(master_list$data$area_impute_concentration) %>% select(-contains("sample"), - contains("SIL")))),
    c("SIL.IntStds", ncol(bind_rows(master_list$data$area_impute_concentration) %>% select(contains("SIL")))),
    c("zeroValues[lipidTargets]", length(which(bind_rows(master_list$data$area_impute_concentration) %>% select(-contains("sample"), -contains("SIL")) ==0))),
    c("zeroValues[SIL.IntStds]", length(which(bind_rows(master_list$data$area_impute_concentration) %>% select(contains("SIL")) ==0))),
    c("naValues[lipidTargets]", length(which(is.na(as.matrix(bind_rows(master_list$data$area_impute_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("naValues[SIL.IntStds]", length(which(is.na(as.matrix(bind_rows(master_list$data$area_impute_concentration) %>% select(contains("SIL"))))))),
    c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$area_impute_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("infValues[SIL.IntStds]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$area_impute_concentration) %>% select(contains("SIL"))))))),
    c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(bind_rows(master_list$data$area_impute_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("nanValues[SIL.IntStds]", length(which(is.nan(as.matrix(bind_rows(master_list$data$area_impute_concentration) %>% select(contains("SIL"))))))),
    c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedConcentration.allPlates")]] <30))),
    c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedConcentration.allPlates")]] <20))),
    c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedConcentration.allPlates")]] <10)))
  ) %>%
    as_tibble() %>%
    rename(metric = V1,
           !! paste0("preProcessedConcentration.allPlates") := V2),
  by = "metric"
)
#change column to numeric
master_list$summary_tables$peakAreaOverview[[paste0("preProcessedConcentration.allPlates")]] <- as.numeric(master_list$summary_tables$peakAreaOverview[[paste0("preProcessedConcentration.allPlates")]])


## statTarget dataSetSummary ---------------------------------------
metric =  c("totalSamples", "studySamples", "ltrSamples", "vltrSamples", "sltrSamples", "pqcSamples", 
            "lipidTargets", "SIL.IntStds", 
            "zeroValues[lipidTargets]", "zeroValues[SIL.IntStds]", 
            "naValues[lipidTargets]", "naValues[SIL.IntStds]", 
            "infValues[lipidTargets]", "infValues[SIL.IntStds]", 
            "nanValues[lipidTargets]", "nanValues[SIL.IntStds]",
            "rsdQc<30%", "rsdQc<20%", "rsdQc<10%")
### 1. intraPlate ----
#### a. peakArea ---------------------------
master_list$summary_tables$statTargetOverview <- tibble(metric = metric)
for(idx_data in names(master_list$data$statTarget_sorted)){
  
  master_list$summary_tables$statTargetOverview <- left_join(
    master_list$summary_tables$statTargetOverview,
    rbind(
      c("totalSamples", nrow(master_list$data$statTarget_sorted[[idx_data]])),
      c("studySamples", nrow(master_list$data$statTarget_sorted[[idx_data]] %>% filter(sample_type_factor == "sample"))),
      c("ltrSamples", nrow(master_list$data$statTarget_sorted[[idx_data]] %>% filter(sample_type_factor == "ltr"))),
      c("vltrSamples", nrow(master_list$data$statTarget_sorted[[idx_data]] %>% filter(sample_type_factor == "vltr"))),
      c("sltrSamples", nrow(master_list$data$statTarget_sorted[[idx_data]] %>% filter(sample_type_factor == "sltr"))),
      c("pqcSamples", nrow(master_list$data$statTarget_sorted[[idx_data]] %>% filter(sample_type_factor == "pqc"))),
      c("lipidTargets", ncol(master_list$data$statTarget_sorted[[idx_data]] %>% select(-contains("sample"), - contains("SIL")))),
      c("SIL.IntStds", ncol(master_list$data$statTarget_sorted[[idx_data]] %>% select(contains("SIL")))),
      c("zeroValues[lipidTargets]", length(which(master_list$data$statTarget_sorted[[idx_data]] %>% select(-contains("sample"), -contains("SIL")) ==0))),
      c("zeroValues[SIL.IntStds]", length(which(master_list$data$statTarget_sorted[[idx_data]] %>% select(contains("SIL")) ==0))),
      c("naValues[lipidTargets]", length(which(is.na(as.matrix(master_list$data$statTarget_sorted[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("naValues[SIL.IntStds]", length(which(is.na(as.matrix(master_list$data$statTarget_sorted[[idx_data]] %>% select(contains("SIL"))))))),
      c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(master_list$data$statTarget_sorted[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("infValues[SIL.IntStds]", length(which(is.infinite(as.matrix(master_list$data$statTarget_sorted[[idx_data]] %>% select(contains("SIL"))))))),
      c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(master_list$data$statTarget_sorted[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("nanValues[SIL.IntStds]", length(which(is.nan(as.matrix(master_list$data$statTarget_sorted[[idx_data]] %>% select(contains("SIL"))))))),
      c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetArea.", idx_data)]] <30))),
      c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetArea.", idx_data)]] <20))),
      c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetArea.", idx_data)]] <10)))
    ) %>%
      as_tibble() %>%
      rename(metric = V1,
             !! paste0("statTargetArea.", idx_data) := V2),
    by = "metric"
  )
  #change column to numeric
  master_list$summary_tables$statTargetOverview[[paste0("statTargetArea.", idx_data)]] <- as.numeric(master_list$summary_tables$statTargetOverview[[paste0("statTargetArea.", idx_data)]])
  
  
  #### b. peakConcentration -----------------
  
  master_list$summary_tables$statTargetOverview <- left_join(
    master_list$summary_tables$statTargetOverview,
    rbind(
      c("totalSamples", nrow(master_list$data$statTarget_concentration[[idx_data]])),
      c("studySamples", nrow(master_list$data$statTarget_concentration[[idx_data]] %>% filter(sample_type_factor == "sample"))),
      c("ltrSamples", nrow(master_list$data$statTarget_concentration[[idx_data]] %>% filter(sample_type_factor == "ltr"))),
      c("vltrSamples", nrow(master_list$data$statTarget_concentration[[idx_data]] %>% filter(sample_type_factor == "vltr"))),
      c("sltrSamples", nrow(master_list$data$statTarget_concentration[[idx_data]] %>% filter(sample_type_factor == "sltr"))),
      c("pqcSamples", nrow(master_list$data$statTarget_concentration[[idx_data]] %>% filter(sample_type_factor == "pqc"))),
      c("lipidTargets", ncol(master_list$data$statTarget_concentration[[idx_data]] %>% select(-contains("sample"), - contains("SIL")))),
      c("SIL.IntStds", NA),
      c("zeroValues[lipidTargets]", length(which(master_list$data$statTarget_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL")) ==0))),
      c("zeroValues[SIL.IntStds]", NA),
      c("naValues[lipidTargets]", length(which(is.na(as.matrix(master_list$data$statTarget_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("naValues[SIL.IntStds]", NA),
      c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(master_list$data$statTarget_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("infValues[SIL.IntStds]", NA),
      c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(master_list$data$statTarget_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("nanValues[SIL.IntStds]", NA),
      c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetConcentration.", idx_data)]] <30))),
      c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetConcentration.", idx_data)]] <20))),
      c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetConcentration.", idx_data)]] <10)))
    ) %>%
      as_tibble() %>%
      rename(metric = V1,
             !! paste0("statTargetConcentration.", idx_data) := V2),
    by = "metric") 
  #change column to numeric
  master_list$summary_tables$statTargetOverview[[paste0("statTargetConcentration.", idx_data)]] <- as.numeric(master_list$summary_tables$statTargetOverview[[paste0("statTargetConcentration.", idx_data)]])
  
  
  #### c. concentration (preProcessed) --------------
  master_list$summary_tables$statTargetOverview <- left_join(
    master_list$summary_tables$statTargetOverview,
    rbind(
      c("totalSamples", nrow(master_list$data$statTarget_impute_concentration[[idx_data]])),
      c("studySamples", nrow(master_list$data$statTarget_impute_concentration[[idx_data]] %>% filter(sample_type_factor == "sample"))),
      c("ltrSamples", nrow(master_list$data$statTarget_impute_concentration[[idx_data]] %>% filter(sample_type_factor == "ltr"))),
      c("vltrSamples", nrow(master_list$data$statTarget_impute_concentration[[idx_data]] %>% filter(sample_type_factor == "vltr"))),
      c("sltrSamples", nrow(master_list$data$statTarget_impute_concentration[[idx_data]] %>% filter(sample_type_factor == "sltr"))),
      c("pqcSamples", nrow(master_list$data$statTarget_impute_concentration[[idx_data]] %>% filter(sample_type_factor == "pqc"))),
      c("lipidTargets", ncol(master_list$data$statTarget_impute_concentration[[idx_data]] %>% select(-contains("sample"), - contains("SIL")))),
      c("SIL.IntStds", NA),
      c("zeroValues[lipidTargets]", length(which(master_list$data$statTarget_impute_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL")) ==0))),
      c("zeroValues[SIL.IntStds]", NA),
      c("naValues[lipidTargets]", length(which(is.na(as.matrix(master_list$data$statTarget_impute_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("naValues[SIL.IntStds]", NA),
      c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(master_list$data$statTarget_impute_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("infValues[SIL.IntStds]", NA),
      c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(master_list$data$statTarget_impute_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("nanValues[SIL.IntStds]", NA),
      c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedStatTargetConcentration.", idx_data)]] <30))),
      c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedStatTargetConcentration.", idx_data)]] <20))),
      c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedStatTargetConcentration.", idx_data)]] <10)))
    ) %>%
      as_tibble() %>%
      rename(metric = V1,
             !! paste0("preProcessedStatTargetConcentration.", idx_data) := V2),
    by = "metric") 
  #change column to numeric
  master_list$summary_tables$statTargetOverview[[paste0("preProcessedStatTargetConcentration.", idx_data)]] <- as.numeric(master_list$summary_tables$statTargetOverview[[paste0("preProcessedStatTargetConcentration.", idx_data)]])
  
}

### 2. interPlate ----

#### a.  peakArea ---------------------------

master_list$summary_tables$statTargetOverview <- left_join(
  master_list$summary_tables$statTargetOverview,
  rbind(
    c("totalSamples", nrow(bind_rows(master_list$data$statTarget_sorted))),
    c("studySamples", nrow(bind_rows(master_list$data$statTarget_sorted) %>% filter(sample_type_factor == "sample"))),
    c("ltrSamples", nrow(bind_rows(master_list$data$statTarget_sorted) %>% filter(sample_type_factor == "ltr"))),
    c("vltrSamples", nrow(bind_rows(master_list$data$statTarget_sorted) %>% filter(sample_type_factor == "vltr"))),
    c("sltrSamples", nrow(bind_rows(master_list$data$statTarget_sorted) %>% filter(sample_type_factor == "sltr"))),
    c("pqcSamples", nrow(bind_rows(master_list$data$statTarget_sorted) %>% filter(sample_type_factor == "pqc"))),
    c("lipidTargets", ncol(bind_rows(master_list$data$statTarget_sorted) %>% select(-contains("sample"), - contains("SIL")))),
    c("SIL.IntStds", ncol(bind_rows(master_list$data$statTarget_sorted) %>% select(contains("SIL")))),
    c("zeroValues[lipidTargets]", length(which(bind_rows(master_list$data$statTarget_sorted) %>% select(-contains("sample"), -contains("SIL")) ==0))),
    c("zeroValues[SIL.IntStds]", length(which(bind_rows(master_list$data$statTarget_sorted) %>% select(contains("SIL")) ==0))),
    c("naValues[lipidTargets]", length(which(is.na(as.matrix(bind_rows(master_list$data$statTarget_sorted) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("naValues[SIL.IntStds]", length(which(is.na(as.matrix(bind_rows(master_list$data$statTarget_sorted) %>% select(contains("SIL"))))))),
    c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$statTarget_sorted) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("infValues[SIL.IntStds]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$statTarget_sorted) %>% select(contains("SIL"))))))),
    c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(bind_rows(master_list$data$statTarget_sorted) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("nanValues[SIL.IntStds]", length(which(is.nan(as.matrix(bind_rows(master_list$data$statTarget_sorted) %>% select(contains("SIL"))))))),
    c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetArea.allPlates")]] <30))),
    c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetArea.allPlates")]] <20))),
    c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetArea.allPlates")]] <10)))
  ) %>%
    as_tibble() %>%
    rename(metric = V1,
           !! paste0("statTargetArea.allPlates") := V2),
  by = "metric"
)
#change column to numeric
master_list$summary_tables$statTargetOverview[[paste0("statTargetArea.allPlates")]] <- as.numeric(master_list$summary_tables$statTargetOverview[[paste0("statTargetArea.allPlates")]])


#### b. peakConcentration -----------------

master_list$summary_tables$statTargetOverview <- left_join(
  master_list$summary_tables$statTargetOverview,
  rbind(
    c("totalSamples", nrow(bind_rows(master_list$data$statTarget_concentration))),
    c("studySamples", nrow(bind_rows(master_list$data$statTarget_concentration) %>% filter(sample_type_factor == "sample"))),
    c("ltrSamples", nrow(bind_rows(master_list$data$statTarget_concentration) %>% filter(sample_type_factor == "ltr"))),
    c("vltrSamples", nrow(bind_rows(master_list$data$statTarget_concentration) %>% filter(sample_type_factor == "vltr"))),
    c("sltrSamples", nrow(bind_rows(master_list$data$statTarget_concentration) %>% filter(sample_type_factor == "sltr"))),
    c("pqcSamples", nrow(bind_rows(master_list$data$statTarget_concentration) %>% filter(sample_type_factor == "pqc"))),
    c("lipidTargets", ncol(bind_rows(master_list$data$statTarget_concentration) %>% select(-contains("sample"), - contains("SIL")))),
    c("SIL.IntStds", ncol(bind_rows(master_list$data$statTarget_concentration) %>% select(contains("SIL")))),
    c("zeroValues[lipidTargets]", length(which(bind_rows(master_list$data$statTarget_concentration) %>% select(-contains("sample"), -contains("SIL")) ==0))),
    c("zeroValues[SIL.IntStds]", length(which(bind_rows(master_list$data$statTarget_concentration) %>% select(contains("SIL")) ==0))),
    c("naValues[lipidTargets]", length(which(is.na(as.matrix(bind_rows(master_list$data$statTarget_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("naValues[SIL.IntStds]", length(which(is.na(as.matrix(bind_rows(master_list$data$statTarget_concentration) %>% select(contains("SIL"))))))),
    c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$statTarget_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("infValues[SIL.IntStds]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$statTarget_concentration) %>% select(contains("SIL"))))))),
    c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(bind_rows(master_list$data$statTarget_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("nanValues[SIL.IntStds]", length(which(is.nan(as.matrix(bind_rows(master_list$data$statTarget_concentration) %>% select(contains("SIL"))))))),
    c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetConcentration.allPlates")]] <30))),
    c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetConcentration.allPlates")]] <20))),
    c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("statTargetConcentration.allPlates")]] <10)))
  ) %>%
    as_tibble() %>%
    rename(metric = V1,
           !! paste0("statTargetConcentration.allPlates") := V2),
  by = "metric"
)
#change column to numeric
master_list$summary_tables$statTargetOverview[[paste0("statTargetConcentration.allPlates")]] <- as.numeric(master_list$summary_tables$statTargetOverview[[paste0("statTargetConcentration.allPlates")]])

#### c. concentration (preProcessed) --------------
master_list$summary_tables$statTargetOverview <- left_join(
  master_list$summary_tables$statTargetOverview,
  rbind(
    c("totalSamples", nrow(bind_rows(master_list$data$statTarget_impute_concentration))),
    c("studySamples", nrow(bind_rows(master_list$data$statTarget_impute_concentration) %>% filter(sample_type_factor == "sample"))),
    c("ltrSamples", nrow(bind_rows(master_list$data$statTarget_impute_concentration) %>% filter(sample_type_factor == "ltr"))),
    c("vltrSamples", nrow(bind_rows(master_list$data$statTarget_impute_concentration) %>% filter(sample_type_factor == "vltr"))),
    c("sltrSamples", nrow(bind_rows(master_list$data$statTarget_impute_concentration) %>% filter(sample_type_factor == "sltr"))),
    c("pqcSamples", nrow(bind_rows(master_list$data$statTarget_impute_concentration) %>% filter(sample_type_factor == "pqc"))),
    c("lipidTargets", ncol(bind_rows(master_list$data$statTarget_impute_concentration) %>% select(-contains("sample"), - contains("SIL")))),
    c("SIL.IntStds", ncol(bind_rows(master_list$data$statTarget_impute_concentration) %>% select(contains("SIL")))),
    c("zeroValues[lipidTargets]", length(which(bind_rows(master_list$data$statTarget_impute_concentration) %>% select(-contains("sample"), -contains("SIL")) ==0))),
    c("zeroValues[SIL.IntStds]", length(which(bind_rows(master_list$data$statTarget_impute_concentration) %>% select(contains("SIL")) ==0))),
    c("naValues[lipidTargets]", length(which(is.na(as.matrix(bind_rows(master_list$data$statTarget_impute_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("naValues[SIL.IntStds]", length(which(is.na(as.matrix(bind_rows(master_list$data$statTarget_impute_concentration) %>% select(contains("SIL"))))))),
    c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$statTarget_impute_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("infValues[SIL.IntStds]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$statTarget_impute_concentration) %>% select(contains("SIL"))))))),
    c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(bind_rows(master_list$data$statTarget_impute_concentration) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("nanValues[SIL.IntStds]", length(which(is.nan(as.matrix(bind_rows(master_list$data$statTarget_impute_concentration) %>% select(contains("SIL"))))))),
    c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedStatTargetConcentration.allPlates")]] <30))),
    c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedStatTargetConcentration.allPlates")]] <20))),
    c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("preProcessedStatTargetConcentration.allPlates")]] <10)))
  ) %>%
    as_tibble() %>%
    rename(metric = V1,
           !! paste0("preProcessedStatTargetConcentration.allPlates") := V2),
  by = "metric"
)
#change column to numeric
master_list$summary_tables$statTargetOverview[[paste0("preProcessedStatTargetConcentration.allPlates")]] <- as.numeric(master_list$summary_tables$statTargetOverview[[paste0("preProcessedStatTargetConcentration.allPlates")]])

#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))

# . ------------------------------------------------------------------------------------------------------------------------------  
# PHASE 4: PLOTS ---------------------
## PCA -----------
#pca scores plot function
master_list$environment$user_functions$pca <- source(paste0(master_list$project_details$github_master_dir,
                                                            "/functions/FUNC_lipidExploreR_PCA_ggplotv3.35.R"))
#setList
master_list$pca <- list()
### 1. peakArea ----
master_list$pca$peakArea <- list()


#### a. color by sample_type ----
#run function
master_list$pca$peakArea$sample_type <- master_list$environment$user_functions$pca$value(
  FUNC_data =  bind_rows(bind_rows(master_list$data$area_sorted), bind_rows(master_list$data$statTarget_sorted)) %>%
    select(-contains("SIL")), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_type_factor",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_HEADER_facet = "sample_data_source",
  FUNC_OPTION_title = paste0("peakArea; ", master_list$project_details$project_name),
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
)

#### b. color by plateID ----
#run function
master_list$pca$peakArea$plateID <- master_list$environment$user_functions$pca$value(
  FUNC_data =  bind_rows(bind_rows(master_list$data$area_sorted), bind_rows(master_list$data$statTarget_sorted)) %>%
    select(-contains("SIL")), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_plate_id",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_HEADER_facet = "sample_data_source",
  FUNC_OPTION_title = paste0("peakArea; ", master_list$project_details$project_name),
  FUNC_OPTION_colours =  master_list$project_details$plot_colour,
  FUNC_OPTION_fill = RColorBrewer::brewer.pal(n = 12, name = "Set3"),
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape = master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_plate"
)


### 2. concentration ----
master_list$pca$concentration <- list()

#### a. color by sample_type ----
#run function
master_list$pca$concentration$sample_type <- master_list$environment$user_functions$pca$value(
  FUNC_data =  bind_rows(bind_rows(master_list$data$area_concentration), bind_rows(master_list$data$statTarget_concentration)) %>%
    select(-contains("SIL")), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_type_factor",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_HEADER_facet = "sample_data_source",
  FUNC_OPTION_title = paste0("concentration; ", master_list$project_details$project_name),
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
)

#### b. color by plateID ----
#run function
master_list$pca$concentration$plateID <- master_list$environment$user_functions$pca$value(
  FUNC_data =  bind_rows(bind_rows(master_list$data$area_sorted), bind_rows(master_list$data$statTarget_sorted)) %>%
    select(-contains("SIL")), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_plate_id",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_HEADER_facet = "sample_data_source",
  FUNC_OPTION_title = paste0("concentration; ", master_list$project_details$project_name),
  FUNC_OPTION_colours =  master_list$project_details$plot_colour,
  FUNC_OPTION_fill = RColorBrewer::brewer.pal(n = 12, name = "Set3"),
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape = master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_plate"
)


### 3. preProcessed and filtered (missing value filter | impute | rsd filter) ----
master_list$pca$preProcessed <- list()

#### a. color by sample_type ----

#run function
master_list$pca$preProcessed$sample_type <- master_list$environment$user_functions$pca$value(
  FUNC_data =  bind_rows(master_list$data$area_preProcessed, master_list$data$statTarget_preProcessed) %>%
    select(-contains("SIL")), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_type_factor",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_HEADER_facet = "sample_data_source",
  FUNC_OPTION_title = paste0("concentration; ", master_list$project_details$project_name),
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
)

#### b. color by plateID ----
#run function
master_list$pca$preProcessed$plateID <- master_list$environment$user_functions$pca$value(
  FUNC_data =  bind_rows(master_list$data$area_preProcessed, master_list$data$statTarget_preProcessed) %>%
    select(-contains("SIL")), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_plate_id",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_HEADER_facet = "sample_data_source",
  FUNC_OPTION_title = paste0("concentration; ", master_list$project_details$project_name),
  FUNC_OPTION_colours =  master_list$project_details$plot_colour,
  FUNC_OPTION_fill = RColorBrewer::brewer.pal(n = 12, name = "Set3"),
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape = master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_plate"
)

## targetControlCharts ------------------

#control chart check
master_list$environment$user_functions$control_chart <- source(paste0(master_list$project_details$github_master_dir,
                                                                      "/functions/FUNC_lipidExploreR_controlChart.R"))

master_list$control_chart <- master_list$environment$user_functions$control_chart$value(
  FUNC_data_area = bind_rows(master_list$data$area_sorted) %>% add_column(sample_data_type = "uncorrectedPeakArea", .after=1),
  FUNC_data_area_concentration = bind_rows(master_list$data$area_concentration) %>% add_column(sample_data_type = "uncorrectedConcentration", .after=1),
  FUNC_data_statTarget = bind_rows(master_list$data$statTarget_sorted) %>% add_column(sample_data_type = "statTargetArea", .after=1),
  FUNC_data_statTarget_concentration = bind_rows(master_list$data$statTarget_concentration) %>% add_column(sample_data_type = "statTargetConcentration", .after=1),
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


# . ------------------------------------------------------------------------------------------------------------------------------  
#PHASE 4: EXPORTS ------------------

## ODS FILES --------------------
### 1. peakArea ----
# create peakArea user guide
master_list$summary_tables$odsAreaOverview <- rbind(
  c("projectName", master_list$project_details$project_name),
  c("user", master_list$project_details$user_name),
  c("lipidExploreR_version", master_list$project_details$lipidExploreR_version),
  c("qcType_preProcessing|filtering", master_list$project_details$qc_type),
  c("SIL_Int.Std_version", master_list$project_details$is_ver),
  c("totalStudyPlates", bind_rows(master_list$data$area_sorted)[["sample_plate_id"]] %>% unique() %>% length()),
  c("totalSamples", bind_rows(master_list$data$area_sorted) %>% nrow()),
  c("studySamples", bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "sample") %>% nrow()),
  c("ltr", bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "ltr") %>% nrow()),
  c("vltr", bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "vltr") %>% nrow()),
  c("sltr", bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "sltr") %>% nrow()),
  c("pqc", bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "pqc") %>% nrow()),
  c("totalLipidFeatures", bind_rows(master_list$data$area_sorted) %>% select(-contains("sample")) %>% select(-contains("SIL")) %>% ncol()),
  c("totalSilInternalStandards", bind_rows(master_list$data$area_sorted) %>% select(contains("SIL")) %>% ncol()),
  c("", ""),
  c("TAB_DESCRIPTION:", ""),
  c("DATA_lipidPeakArea", "lipid target peak area integrals (skylineMS)"),
  c("DATA_silPeakArea", "stable isotope labelled internal standard peak area integrals (skylineMS)"),
  c("DATA_responseRatio", "lipid target/SIL internal standards peak area response ratios"),
  c("DATA_concentration", "peak area response ratios normalised to SIL Int.Std concentration factor"),
  c("DATA_preProcessedConcentration", "concentration data that has undergone pre-processing missing value sample filter [>50%], missing value feature filter [>50%], imputation remaining missing values [min/2] relative standard deviation filtering [>30%]"),      
  c("SUMMARY_PeakArea", "sample|feature overview in peakArea dataset"),
  c("SUMMARY_response|concentration", "sample|feature overview in response|concentration dataset"),
  c("SUMMARY_preProcessedConcentration", "sample|feature overview in preProcessedConcentration dataset"),
  c("FILTER_missingValueSamples", "sample performance in missing value filter [samples with >50% values <5000 counts removed]"),
  c("FILTER_missingValueFeatures", "feature performance in missing value filter [features with >50% values <5000 counts removed]"),
  c("FILTER_RSDinQC", "feature performance in rsd filter [features with > 30% RSD across user selected qc are excluded]"),
  c("", ""),
  c("NOTE:", "DATA IN THIS SPREADSHEET IS PRODUCED FROM RAW AREA, NO SIGNAL DRIFT OR BATCH CORRECTION HAS BEEN APPLIED")
) %>%
  as_tibble() 

## write .ods tabbed file 
write_ods(
  path = paste0(master_list$project_details$project_dir, 
                "/html_report/",
                Sys.Date(),
                "_",
                master_list$project_details$project_name,
                "_lipidData_qcCeheckeR_v3.5_[peakArea].ods"),
  list(
    "userGuide" = master_list$summary_tables$odsAreaOverview,
    #summary
    "platePerformance" = master_list$summary_tables$peakAreaOverview,
    #uncorrected data
    "data_peakArea" = bind_rows(master_list$data$area_sorted) %>% select(-contains("SIL")),
    "data_silPeakArea" = bind_rows(master_list$data$area_sorted) %>% select(contains("sample") | contains("SIL")),
    "data_concentration" = bind_rows(master_list$data$area_concentration),
    "data_preProcessedConcentration" = master_list$data$area_impute_concentration_rsd_all,
    #samplePerformance
    master_list$process_lists$area_missVal_filtered$MNAp013$sample
    #lipidPerformance
    "lipidQcRsdPerformance" = master_list$process_lists$rsd_filter %>% select(lipid, !contains("statTarget")),
  )
)



# . ------------------------------------------------------------------------------------------------------------------------------  





# PHASE 5: EXPORT HTML REPORT FILES -------------------------

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


# . ------------------------------------------------------------------------------------------------------------------------------  


