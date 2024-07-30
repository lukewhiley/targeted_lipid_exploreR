# . ------------------------------------------------------------------------------------------------------------------------------  
# PROJECT SETUP: --------------------------------------
## 1. Welcome section/project set upload skylineR --------------------------------------

#welcome messages
#if(!exists("master_list")){
dlg_message("Welcome to lipid qc exploreR! :-)", type = 'ok'); dlg_message("please run the skylineR script (perPlate) before running this script", type = 'ok'); dlg_message("if combining multiple plates/batches select parent master folder. e.g. ~parent/skylineR_plate01; ~parent/skylineR_plate02", type = 'ok');       
dlg_message("select parent master folder with skylineR subfolders", type = 'ok');       

# choose directory
skylineR_directory <- rstudioapi::selectDirectory()
#setup project sub-directories
#data
if(!dir.exists(paste0(skylineR_directory, "/data"))){dir.create(paste0(skylineR_directory, "/data"))}
#rda
if(!dir.exists(paste0(skylineR_directory, "/data/rda"))){dir.create(paste0(skylineR_directory, "/data/rda"))}
#batch_correct
if(!dir.exists(paste0(skylineR_directory, "/data/batch_correction"))){dir.create(paste0(skylineR_directory, "/data/batch_correction"))}
#html_reports
if(!dir.exists(paste0(skylineR_directory, "/html_report"))){dir.create(paste0(skylineR_directory, "/html_report"))}
#xlsx_reports
if(!dir.exists(paste0(skylineR_directory, "/xlsx_report"))){dir.create(paste0(skylineR_directory, "/xlsx_report"))}

#list .rda files
rda_fileList <- list.files(skylineR_directory, pattern = "_skylineR_", recursive = TRUE)
#ensure only .rda files are in filelist
rda_fileList <- rda_fileList[grepl(".rda", rda_fileList, ignore.case = T)]
#load 1st rda
load(paste(skylineR_directory, rda_fileList[1], sep ="/"))
#set user
master_list$project_details$user_name <- dlgInput("user", "example_initials")$res
#set project name
master_list$project_details$project_name <- dlgInput("project", basename(paste0(master_list$project_details$project_dir)))$res
#set qc-type
master_list$project_details$qc_type <- dlgInput("qc type used - tag MUST be in filename of mzML files (matched case)", "LTR/SR/PQC")$res
#set qc-type
master_list$project_details$is_ver <- dlgInput("SIL internal standard version used (v1 = pre-2023, v2 = post-2023)", "v1/v2")$res
#reset parent directory
master_list$summary_tables$project_summary$value[which(master_list$summary_tables$project_summary$`Project detail` == "local directory")] <- skylineR_directory
master_list$project_details$project_dir <- skylineR_directory

#reassign master_list[1] and remove original master_list object
masterListBatch <- master_list; rm(master_list)



if(length(rda_fileList > 1)){
  #run loop to combine .rda files
  for(idx_rda in rda_fileList[2:length(rda_fileList)]){
    
    #load loop .rda
    load(paste(skylineR_directory, idx_rda, sep ="/"))
    
    #combine mzml
    for(idx_mzml in names(master_list$data$mzR)){
      masterListBatch$data$mzR[[idx_mzml]] <- master_list$data$mzR[[idx_mzml]]
    }
    
    #combine plate list
    masterListBatch$project_details$mzml_plate_list <- c(
      masterListBatch$project_details$mzml_plate_list,
      master_list$project_details$mzml_plate_list
    )
    
    #combine mzml_sample_list
    masterListBatch$project_details$mzml_sample_list <- c(
      masterListBatch$project_details$mzml_sample_list,
      master_list$project_details$mzml_plate_list
    )
    
    #combine skylineR data
    masterListBatch$data$skyline_report <- bind_rows(
      masterListBatch$data$skyline_report,
      master_list$data$skyline_report
    )
    
    #remove master_list for next iteration of loop
    rm(master_list)
  } #close for idx_ra
}#close if length(rda_filelist)>1 statement

#reset to master_list for remainder of script and removal of masterlistBatchObject
master_list <- masterListBatch; rm(masterListBatch)

#set version of lipidExploreR used
master_list$project_details$lipidExploreR_version <- "3.5"
master_list$project_details$qcCheckR_version <- "3.5"
master_list$summary_tables$project_summary$value[which(master_list$summary_tables$project_summary$`Project detail` == "lipidExploreR version")] <- "3.5"
master_list$summary_tables$area_project_summary$value[which(master_list$summary_tables$area_project_summary$`Project detail` == "lipidExploreR version")] <- "3.5"

## 2. Source LGW github functions --------------------------------------
master_list$project_details$github_master_dir = "~/Documents/GitHub/targeted_lipid_exploreR_v3/"
#pca scores plot function
master_list$environment$user_functions$pca <- source(paste0(master_list$project_details$github_master_dir,
                                                            "/functions/FUNC_lipidExploreR_PCA_ggplot_v3.5.R"))
#missing value filter
# master_list$environment$user_functions$miss_value_filter <- source(paste0(master_list$project_details$github_master_dir,
#                                                                           "/functions/FUNC_lipidExploreR_missing_value_filter.R"))
#impute data
master_list$environment$user_functions$impute_data <- source(paste0(master_list$project_details$github_master_dir,
                                                                    "/functions/FUNC_lipidExploreR_impute_data_v3.5.R"))
#concentration calculator
master_list$environment$user_functions$conc_calc <-  source(paste0(master_list$project_details$github_master_dir,
                                                                   "/functions/FUNC_lipidExploreR_conc_calculator_v3.5.R"))
#pca qc filter
# master_list$environment$user_functions$pca_filter <- source(paste0(master_list$project_details$github_master_dir,
#                                                                    "/functions/FUNC_lipidExploreR_pca_filter.R"))
#signal/batch correction
master_list$environment$user_functions$signal_correct <- source(paste0(master_list$project_details$github_master_dir,
                                                                       "/functions/FUNC_lipidExploreR_signal_drift_correct_v3.5.R"))
#run order vs PC plots
# master_list$environment$user_functions$pc_run_plot <- source(paste0(master_list$project_details$github_master_dir,
#                                                                     "/functions/FUNC_lipidExploreR_PCA_runOrder_ggplot.R"))
#control chart check
master_list$environment$user_functions$control_chart <- source(paste0(master_list$project_details$github_master_dir,
                                                                      "/functions/FUNC_lipidExploreR_controlChart_v3.5.R"))
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

# PHASE 1: STOCK DATA [no preProcessing, no signalDrift or batch correction] -----------------
# data has no preProcessing
# note: no signal drift or batch correction

## 1. Transpose data to standard metabolomics structure (features in columns, samples in rows) ---------------------------------------
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

## 2. Sort by run order and add annotation data ------------------------------- 
#list for storing concentration data area_sorted by run order

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

  #add sample_data_source column
  master_list$data$area_sorted[[idx_data]] <- master_list$data$area_sorted[[idx_data]] %>%
    add_column(sample_data_source = "peakArea",
               .after = "sample_type_factor_rev")
}

#add a global runorder based on mzML sample timestamp
tempAllSamples <- master_list$data$area_sorted %>%
  bind_rows() %>%
  arrange(sample_timestamp)
#set project master run index
tempAllSamples$sample_run_index <- c(1:nrow(tempAllSamples))
#reset sorted list
master_list$data$area_sorted <-list()
for(idx_data in unique(tempAllSamples$sample_plate_id)){
  master_list$data$area_sorted[[idx_data]] <- tempAllSamples %>%
    filter(sample_plate_id == idx_data)
}


## 3. Calculate response ratio and concentration calculations  ------------------------------------------------------
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

  master_list$data$area_response[[idx_data]]$sample_data_source <- "response"; master_list$data$area_concentration[[idx_data]]$sample_data_source <- "concentration"
  }

#.------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# PHASE 2: PREPROCESSING | FILTERING | IMPUTATION: -------------------------------------------

## no signalDrift or batch correction -----------------

### 1. missing value sampleFilter ----

master_list$process_lists$mvSamples <- master_list$data$area_sorted %>%
  bind_rows() %>%
  select(
    sample_run_index,
    sample_name,
    sample_batch,
    sample_plate_id,
    sample_type_factor
  )

#zero values [samples]
master_list$process_lists$mvSamples[["missing.lipid[<LOD.peakArea<5000]"]] <- rowSums(
  x = (master_list$data$area_sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix()) <5000, na.rm =T)

#zero values [SIL Int.Stds]
master_list$process_lists$mvSamples[["missing.SIL.Int.Std[<LOD.peakArea<5000]"]] <- rowSums(
  x = (master_list$data$area_sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix()) <5000, na.rm =T)

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
master_list$process_lists$mvSamples$sampleKeep <- 1
#remove where missing data >50% total lipidTargets
master_list$process_lists$mvSamples$sampleKeep[which(
  master_list$process_lists$mvSamples$`totalMissingValues[lipidTarget]` > ((
    bind_rows(master_list$data$area_sorted) %>%
      select(-contains("sample")) %>%
      select(-contains("SIL")) %>%
      ncol())*0.5)
)] <- 0

#remove where missing data >50% total SIL.int.Stds
master_list$process_lists$mvSamples$sampleKeep[which(
  master_list$process_lists$mvSamples$`totalMissingValues[SIL.Int.Stds]` > ((bind_rows(
    master_list$data$area_sorted) %>%
      select(-contains("sample")) %>%
      select(contains("SIL")) %>%
      ncol())*0.5)
)] <- 0

#create filtered dataset
master_list$data$pp_mvFilter_s <- master_list$data$area_sorted

for(idx_data in names(master_list$data$area_sorted)){
  master_list$data$pp_mvFilter_s[[idx_data]] <- master_list$data$pp_mvFilter_s[[idx_data]] %>%
    filter(sample_name %in% master_list$process_lists$mvSamples$sample_name[which(
      master_list$process_lists$mvSamples$sampleKeep == 1)
    ])
}

### 2. missing value lipidFilter ----

master_list$process_lists$mvLipids <- tibble(
  lipid = master_list$data$area_sorted  %>%
    bind_rows() %>%
    select(-contains("sample")) %>%
    names()
)

for(idx_data in names(master_list$data$pp_mvFilter_s)){
  master_list$process_lists$mvLipids[[paste0(idx_data,".peakArea<5000[LOD]")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_data]] %>%
          select(-contains("sample")) %>%
          as.matrix()) <5000, na.rm = T)
  
  #na values
  master_list$process_lists$mvLipids[[paste0(idx_data,".naValues")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_data]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.na(), na.rm = T)
  
  #nan values
  master_list$process_lists$mvLipids[[paste0(idx_data,".nanValues")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_data]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.nan(), na.rm = T)
  
  #inf values
  master_list$process_lists$mvLipids[[paste0(idx_data,".infValues")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_data]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.infinite(), na.rm = T)
  
  #total missing values for plate
  master_list$process_lists$mvLipids[[paste0(idx_data,".totalMissingValues")]] <-  rowSums(x= (master_list$process_lists$mvLipids %>%
                                                                                                 select(contains(idx_data))%>%
                                                                                                 as.matrix()),
                                                                                           na.rm = T)
  
  #does lipid fail for the plate?
  master_list$process_lists$mvLipids[[paste0(idx_data, ".keepLipid[Plate]")]] <- 1
  master_list$process_lists$mvLipids[[paste0(idx_data, ".keepLipid[Plate]")]][which(
    master_list$process_lists$mvLipids[[paste0(idx_data,".totalMissingValues")]] > (nrow(master_list$data$pp_mvFilter_s[[idx_data]]) * 0.5)
  )] <- 0
  
}

#for all plates
master_list$process_lists$mvLipids[[paste0("allPlates.peakArea<5000[LOD]")]] <- colSums(
  x= (master_list$data$pp_mvFilter_s %>%
        bind_rows() %>%
        select(-contains("sample")) %>%
        as.matrix()) <5000, na.rm = T)

#na values
master_list$process_lists$mvLipids[[paste0("allPlates.naValues")]] <- colSums(
  x= (master_list$data$pp_mvFilter_s %>%
        bind_rows() %>%
        select(-contains("sample")) %>%
        as.matrix()) %>%
    is.na(), na.rm = T)

#nan values
master_list$process_lists$mvLipids[[paste0("allPlates.nanValues")]] <- colSums(
  x= (master_list$data$pp_mvFilter_s %>%
        bind_rows() %>%
        select(-contains("sample")) %>%
        as.matrix()) %>%
    is.nan(), na.rm = T)

#inf values
master_list$process_lists$mvLipids[[paste0("allPlates.infValues")]] <- colSums(
  x= (master_list$data$pp_mvFilter_s %>%
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
# master_list$process_lists$mvLipids[[paste0("allPlates.keepLipid[all]")]] <- 1
# master_list$process_lists$mvLipids[[paste0("allPlates.keepLipid[all]")]] [which(
#   master_list$process_lists$mvLipids[[paste0("allPlates.totalMissingValues")]] > (nrow(bind_rows(master_list$data$pp_mvFilter_s)) * 0.5)
# )] <- 0

# add fail.filter column if lipid failed for a plate or for all plates
master_list$process_lists$mvLipids[["PROJECT.keepLipid"]] <- 1
master_list$process_lists$mvLipids[["PROJECT.keepLipid"]][which(
  rowSums(x= (master_list$process_lists$mvLipids %>%
                select(contains(".keepLipid[Plate]")) %>%
                as.matrix()) == 0, 
          na.rm = T) > 0
)] <- 0


#create filtered dataset
master_list$data$pp_mvFilter_s_l <- master_list$data$pp_mvFilter_s
for(idx_data in names(master_list$data$pp_mvFilter_s_l)){
  master_list$data$pp_mvFilter_s_l[[idx_data]] <- master_list$data$pp_mvFilter_s_l[[idx_data]] %>%
    select(-any_of(
      (master_list$process_lists$mvLipids %>% filter(PROJECT.keepLipid == 0))[["lipid"]]
    ))
  
  master_list$data$pp_mvFilter_s_l[[idx_data]]$sample_data_source <- "pp_mvFiltered"
}

### 3. impute missing values [min/2 imputation (missing assumed < LOD)] -----------------------------------------------------
#Imputation of the remaining zero value and missing data 
#Imputation is completed using x/2, where x is minimum intensity of that feature in the batch

master_list$data$pp_impute <- list()

for(idx_data in names(master_list$data$pp_mvFilter_s_l)){
  master_list$data$pp_impute[[idx_data]] <- master_list$environment$user_functions$impute_data$value(
    FUNC_data = master_list$data$pp_mvFilter_s_l[[idx_data]],
    FUNC_metabolite_list = master_list$data$pp_mvFilter_s_l[[idx_data]] %>% 
      select(-contains("sample")) %>% names(),
    FUNC_option_impute_missing_data = TRUE)
  
  master_list$data$pp_impute[[idx_data]]$sample_data_source <- "pp_impute"
}

### 4. calculate response ratio and concentration calculations  ------------------------------------------------------

#* Calculation of target metabolite/stable isotope labelled (SIL) internal standard ratio, using predefined target metabolite/internal standard pairs
#* Conversion of response ratio to concentration values using single point calibration
#* Performed on imputed and filtered data

#set empty list to store output data
master_list$data$pp_response <- list()
master_list$data$pp_concentration <- list()

for(idx_data in names(master_list$data$pp_impute)){
  loop_data <- master_list$environment$user_functions$conc_calc$value(
    FUNC_data = master_list$data$pp_impute[[idx_data]],
    FUNC_SIL_list = master_list$data$pp_impute[[idx_data]] %>% 
      select(contains("SIL")) %>% names(),
    FUNC_SIL_guide = master_list$templates$SIL_guide,
    FUNC_conc_guide = master_list$templates$conc_guide
  )
  master_list$data$pp_response[[idx_data]] <- loop_data$response; master_list$data$pp_concentration[[idx_data]] <- loop_data$concentration
  
  master_list$data$pp_response[[idx_data]]$sample_data_source <- "filteredResponse"; master_list$data$pp_concentration[[idx_data]]$sample_data_source <- "postFilterConcentration"
  
}

## statTarget signalDrift | batch correction ----------------------------

### 1. apply statTarget signal drift | batch correction algorithm -----

#create batch correction directory
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/batch_correction"))){
  dir.create(paste0(master_list$project_details$project_dir, "/data/batch_correction"))
}

#only apply on peakArea data
#run batch correction 
statTarget_data <- master_list$environment$user_functions$signal_correct$value(
  FUNC_project_directory = paste0(master_list$project_details$project_dir,
                                  "/data/batch_correction"),
  FUNC_data = master_list$data$area_sorted %>% 
    bind_rows(), 
  FUNC_metabolite_list = master_list$data$area_sorted %>%
    bind_rows() %>%
    select(-contains("sample")) %>%
    names(),
  FUNC_header_sample_id = "sample_name",
  FUNC_header_batch = "sample_plate_id",
  FUNC_header_sample_type = "sample_type",
  FUNC_header_run_order = "sample_run_index",
  FUNC_option_qc_type = "vltr"
) 


#list for storing signal drift corrected data (per project)
master_list$data$statTarget <- list()
#loop to back into plate elements of list
for(idx_data in unique(statTarget_data$sample_plate_id)){
  master_list$data$statTarget[[idx_data]] <- statTarget_data %>%
    filter(sample_plate_id == idx_data)
  #set dataSource
  master_list$data$statTarget[[idx_data]]$sample_data_source <- "statTarget"
}


#list for storing signal drift corrected data (per project)
master_list$data$statTarget_mvFilter_s <- list()
#loop to back into plate elements of list
for(idx_data in unique(statTarget_data$sample_plate_id)){
  master_list$data$statTarget_mvFilter_s[[idx_data]] <- statTarget_data %>%
    filter(sample_plate_id == idx_data) %>%
    filter(sample_name %in% filter(master_list$process_lists$mvSamples, sampleKeep==1)[["sample_name"]])

  master_list$data$statTarget_mvFilter_s[[idx_data]]$sample_data_source <- "statTarget_mvFilter_s"
}

master_list$data$statTarget_mvFilter_s_l <- list()
#loop to back into plate elements of list
for(idx_data in names(master_list$data$statTarget_mvFilter_s)){
  master_list$data$statTarget_mvFilter_s_l[[idx_data]] <- master_list$data$statTarget_mvFilter_s[[idx_data]] %>%
    select(
      contains("sample"),
      any_of(filter(master_list$process_lists$mvLipids, get(paste0(idx_data, ".keepLipid[Plate]")) == 1)[["lipid"]])
    )
  
  master_list$data$statTarget_mvFilter_s_l[[idx_data]]$sample_data_source <- "statTarget_mvFilter_s_l"
}

rm(list = c(ls()[which(ls() != "master_list")]))

### 2. calculate response and concentration ----
#set empty list to store output data
master_list$data$statTarget_response <- list()
master_list$data$statTarget_concentration <- list()

for(idx_data in names(master_list$data$statTarget)){
  loop_data <- master_list$environment$user_functions$conc_calc$value(
    FUNC_data = master_list$data$statTarget[[idx_data]],
    FUNC_SIL_list = master_list$data$statTarget[[idx_data]] %>% 
      select(-contains("sample")) %>% select(contains("SIL")) %>% names(),
    FUNC_SIL_guide = master_list$templates$SIL_guide,
    FUNC_conc_guide = master_list$templates$conc_guide
  )
  master_list$data$statTarget_response[[idx_data]] <- loop_data$response; master_list$data$statTarget_concentration[[idx_data]] <- loop_data$concentration
  
  master_list$data$statTarget_response[[idx_data]]$sample_data_source <- "statTargetResponse"; master_list$data$statTarget_concentration[[idx_data]]$sample_data_source <- "statTargetConcentration"
}

#make statTargetconcentration mv filtered
master_list$data$statTarget_concentration_mv <- list()

for(idx_data in names(master_list$data$statTarget_mvFilter_s_l)){
  loop_data <- master_list$environment$user_functions$conc_calc$value(
    FUNC_data = master_list$data$statTarget_mvFilter_s_l[[idx_data]],
    FUNC_SIL_list = master_list$data$statTarget_mvFilter_s_l[[idx_data]] %>% 
      select(-contains("sample")) %>% select(contains("SIL")) %>% names(),
    FUNC_SIL_guide = master_list$templates$SIL_guide,
    FUNC_conc_guide = master_list$templates$conc_guide
  )
  master_list$data$statTarget_concentration_mv[[idx_data]] <- loop_data$concentration
  
  master_list$data$statTarget_concentration_mv[[idx_data]]$sample_data_source <- "statTargetConcentration"
}


## apply QC %RSD filters to concentration data ---------------------------

### 1. Data filter: QC %RSD filter across user selected LTRs -------------------------
#### a. intraPlate ------
#first create a table of rsd performance
master_list$process_lists$rsd_filter <- tibble(
  lipid =  names(master_list$data$area_sorted[[1]] %>%
                   filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
                   select(-contains("sample"))))

#run loop for each dataType: rawArea; concentration; preProcessedConcentration; statTargetConcentration
for(idx_data in names(master_list$data$area_sorted)){
  
  peakArea_qc_data <- master_list$data$area_sorted[[idx_data]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  
  concentration_qc_data <- master_list$data$area_concentration[[idx_data]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  # 
  preProcessedConcentration_qc_data <- master_list$data$pp_concentration[[idx_data]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  
  statTargetConcentration_qc_data <- master_list$data$statTarget_concentration_mv[[idx_data]] %>%
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
  
  #extract RSD performance in preProcessedConcentrationData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(preProcessedConcentration_qc_data),
      !! paste0("postFilterConcentration.", idx_data) := (colSds(as.matrix(preProcessedConcentration_qc_data))*100)/colMeans(as.matrix(preProcessedConcentration_qc_data))
    ),
    by = "lipid")
  
  #extract RSD performance in statTargetConcentrationData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(statTargetConcentration_qc_data),
      !! paste0("postFilterStatTarget.", idx_data) := (colSds(as.matrix(statTargetConcentration_qc_data))*100)/colMeans(as.matrix(statTargetConcentration_qc_data))
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

preProcessedConcentration_qc_data <- bind_rows(master_list$data$pp_concentration) %>%
  filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
  select(-contains("sample"))

#statTargetData
statTargetConcentration_qc_data <- bind_rows(master_list$data$statTarget_concentration_mv) %>%
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

#extract RSD performance in concentration
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
    !! paste0("postFilterConcentration.allPlates") := (colSds(as.matrix(preProcessedConcentration_qc_data))*100)/colMeans(as.matrix(preProcessedConcentration_qc_data))
  ),
  by = "lipid")

#extract RSD performance in statTargetConcentrationData
master_list$process_lists$rsd_filter <- left_join(
  master_list$process_lists$rsd_filter,
  tibble(
    lipid =  names(statTargetConcentration_qc_data),
    !! paste0("postFilterStatTarget.allPlates") := (colSds(as.matrix(statTargetConcentration_qc_data))*100)/colMeans(as.matrix(statTargetConcentration_qc_data))
  ), 
  by = "lipid")

### 2. Make QC %RSD filtered datasets ----
#### preProcessedConcentration ----
master_list$data$pp_final <- list()
for(idx_data in names(master_list$data$pp_concentration)){
master_list$data$pp_final[[idx_data]] <- master_list$data$pp_concentration[[idx_data]] %>%
  select(
    contains("sample"),
    any_of(names(which(master_list$process_lists$rsd_filter[[paste0("concentration.", idx_data)]] < 30)))
  )

master_list$data$pp_final[[idx_data]]$sample_data_source <- "postFilterConcentration"
}
#### statTargetConcentration ----
master_list$data$statTarget_final <- list()
for(idx_data in names(master_list$data$statTarget_concentration_mv)){
  master_list$data$statTarget_final[[idx_data]] <- master_list$data$statTarget_concentration_mv[[idx_data]] %>%
    select(
      contains("sample"),
      any_of(names(which(master_list$process_lists$rsd_filter[[paste0("postFilterStatTarget.", idx_data)]] < 30)))
    )
  
  master_list$data$statTarget_final[[idx_data]]$sample_data_source <- "postFilterStatTarget"
}
#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))

##3. make combined plates dataset for export ----

#concentration data
#create a combined plate filtered by RSD across all plates
tempMatrix <- bind_rows(master_list$data$area_concentration) %>%
  select(contains("sample_name"), 
         all_of(bind_rows(master_list$data$area_concentration) %>% 
                  select(-contains("sample")) %>%
                  names()
                )) %>%
  column_to_rownames("sample_name") 
#replace values < 1 with values with 2 sig figs.
tempMatrix[is.na(tempMatrix)] <- 0
tempMatrix[tempMatrix <1] <- signif(tempMatrix[tempMatrix <1], 2)
tempMatrix[tempMatrix >1] <- round(tempMatrix[tempMatrix >1], 2)
tempMatrix[tempMatrix == 0] <- NA 

#rebind with sample_name
master_list$data$area_concentration_final_all_plates <- list(
  bind_rows(master_list$data$area_concentration) %>%
    select(contains("sample")) %>%
    left_join(.,
              tempMatrix %>% 
                rownames_to_column("sample_name"),
              by = "sample_name"))


#postFilterConcentration data
#create a combined plate filtered by RSD across all plates
tempMatrix <- bind_rows(master_list$data$pp_concentration) %>%
  select("sample_name", any_of(names(which(master_list$process_lists$rsd_filter$concentration.allPlates < 30)))) %>%
  column_to_rownames("sample_name") 
#replace values < 1 with values with 2 sig figs.
tempMatrix[tempMatrix <1] <- signif(tempMatrix[tempMatrix <1], 2)
tempMatrix[tempMatrix >1] <- round(tempMatrix[tempMatrix >1], 2)

#rebind with sample_name
master_list$data$pp_final_allPlates <- list(
  bind_rows(master_list$data$pp_concentration) %>%
    select(contains("sample")) %>%
    left_join(.,
              tempMatrix %>% 
                rownames_to_column("sample_name"),
              by = "sample_name"))

  
#and for statTarget data
tempMatrix <- bind_rows(master_list$data$statTarget_concentration_mv) %>%
  select("sample_name", any_of(names(which(master_list$process_lists$rsd_filter$postFilterStatTarget.allPlates < 30)))) %>%
  column_to_rownames("sample_name") 
#replace values < 1 with values with 2 sig figs.
tempMatrix[tempMatrix <1] <- signif(tempMatrix[tempMatrix <1], 2)
tempMatrix[tempMatrix >1] <- round(tempMatrix[tempMatrix >1], 2)

#rebind with sample_name
master_list$data$statTarget_final_allPlates <- list(
  bind_rows(master_list$data$statTarget_concentration_mv) %>%
    select(contains("sample")) %>%
    left_join(.,
              tempMatrix %>% 
                rownames_to_column("sample_name"),
              by = "sample_name"))

#. ------------------------------------------------------------------------------------------------------------------------------------------------
# PHASE 3. SummaryReports ------------------------ 
## peakArea dataSetSummary -----
metric =  c("totalSamples", "studySamples", "ltrSamples", "vltrSamples", "sltrSamples", "pqcSamples", 
            "lipidTargets", "SIL.IntStds", 
            "zeroValues[lipidTargets]", "zeroValues[SIL.IntStds]", 
            "naValues[lipidTargets]", "naValues[SIL.IntStds]", 
            "infValues[lipidTargets]", "infValues[SIL.IntStds]", 
            "nanValues[lipidTargets]", "nanValues[SIL.IntStds]",
            "rsdQc<30%", "rsdQc<20%", "rsdQc<10%")
### 1. intraPlate ----
#### a. peakArea ---------------------------
master_list$summary_tables$projectOverview <- tibble(metric = metric)
for(idx_data in names(master_list$data$area_sorted)){
  
  master_list$summary_tables$projectOverview <- left_join(
    master_list$summary_tables$projectOverview,
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
  master_list$summary_tables$projectOverview[[paste0("peakArea.", idx_data)]] <- as.numeric(master_list$summary_tables$projectOverview[[paste0("peakArea.", idx_data)]])
  
  
  #### b. concentration -----------------
  
  master_list$summary_tables$projectOverview <- left_join(
    master_list$summary_tables$projectOverview,
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
  master_list$summary_tables$projectOverview[[paste0("concentration.", idx_data)]] <- as.numeric(master_list$summary_tables$projectOverview[[paste0("concentration.", idx_data)]])
  
  
  #### c. preProcessedConcentration (preProcessed) --------------
  master_list$summary_tables$projectOverview <- left_join(
    master_list$summary_tables$projectOverview,
    rbind(
      c("totalSamples", nrow(master_list$data$pp_concentration[[idx_data]])),
      c("studySamples", nrow(master_list$data$pp_concentration[[idx_data]] %>% filter(sample_type_factor == "sample"))),
      c("ltrSamples", nrow(master_list$data$pp_concentration[[idx_data]] %>% filter(sample_type_factor == "ltr"))),
      c("vltrSamples", nrow(master_list$data$pp_concentration[[idx_data]] %>% filter(sample_type_factor == "vltr"))),
      c("sltrSamples", nrow(master_list$data$pp_concentration[[idx_data]] %>% filter(sample_type_factor == "sltr"))),
      c("pqcSamples", nrow(master_list$data$pp_concentration[[idx_data]] %>% filter(sample_type_factor == "pqc"))),
      c("lipidTargets", ncol(master_list$data$pp_concentration[[idx_data]] %>% select(-contains("sample"), - contains("SIL")))),
      c("SIL.IntStds", NA),
      c("zeroValues[lipidTargets]", length(which(master_list$data$pp_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL")) ==0))),
      c("zeroValues[SIL.IntStds]", NA),
      c("naValues[lipidTargets]", length(which(is.na(as.matrix(master_list$data$pp_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("naValues[SIL.IntStds]", NA),
      c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(master_list$data$pp_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("infValues[SIL.IntStds]", NA),
      c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(master_list$data$pp_concentration[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("nanValues[SIL.IntStds]", NA),
      c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterConcentration.", idx_data)]] <30))),
      c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterConcentration.", idx_data)]] <20))),
      c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterConcentration.", idx_data)]] <10)))
    ) %>%
      as_tibble() %>%
      rename(metric = V1,
             !! paste0("postFilterConcentration.", idx_data) := V2),
    by = "metric")
  #change column to numeric
  master_list$summary_tables$projectOverview[[paste0("postFilterConcentration.", idx_data)]] <- as.numeric(master_list$summary_tables$projectOverview[[paste0("postFilterConcentration.", idx_data)]])

  #### d. statTargetConcentration --------------
  master_list$summary_tables$projectOverview <- left_join(
    master_list$summary_tables$projectOverview,
    rbind(
      c("totalSamples", nrow(master_list$data$statTarget_concentration_mv[[idx_data]])),
      c("studySamples", nrow(master_list$data$statTarget_concentration_mv[[idx_data]] %>% filter(sample_type_factor == "sample"))),
      c("ltrSamples", nrow(master_list$data$statTarget_concentration_mv[[idx_data]] %>% filter(sample_type_factor == "ltr"))),
      c("vltrSamples", nrow(master_list$data$statTarget_concentration_mv[[idx_data]] %>% filter(sample_type_factor == "vltr"))),
      c("sltrSamples", nrow(master_list$data$statTarget_concentration_mv[[idx_data]] %>% filter(sample_type_factor == "sltr"))),
      c("pqcSamples", nrow(master_list$data$statTarget_concentration_mv[[idx_data]] %>% filter(sample_type_factor == "pqc"))),
      c("lipidTargets", ncol(master_list$data$statTarget_concentration_mv[[idx_data]] %>% select(-contains("sample"), - contains("SIL")))),
      c("SIL.IntStds", NA),
      c("zeroValues[lipidTargets]", length(which(master_list$data$statTarget_concentration_mv[[idx_data]] %>% select(-contains("sample"), -contains("SIL")) ==0))),
      c("zeroValues[SIL.IntStds]", NA),
      c("naValues[lipidTargets]", length(which(is.na(as.matrix(master_list$data$statTarget_concentration_mv[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("naValues[SIL.IntStds]", NA),
      c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(master_list$data$statTarget_concentration_mv[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("infValues[SIL.IntStds]", NA),
      c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(master_list$data$statTarget_concentration_mv[[idx_data]] %>% select(-contains("sample"), -contains("SIL"))))))),
      c("nanValues[SIL.IntStds]", NA),
      c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterStatTarget.", idx_data)]] <30))),
      c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterStatTarget.", idx_data)]] <20))),
      c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterStatTarget.", idx_data)]] <10)))
    ) %>%
      as_tibble() %>%
      rename(metric = V1,
             !! paste0("postFilterStatTarget.", idx_data) := V2),
    by = "metric") 
  #change column to numeric
  master_list$summary_tables$projectOverview[[paste0("postFilterStatTarget.", idx_data)]] <- as.numeric(master_list$summary_tables$projectOverview[[paste0("postFilterStatTarget.", idx_data)]])
}

### 2. interPlate ----

#### a.  peakArea ---------------------------
master_list$summary_tables$projectOverview <- left_join(
  master_list$summary_tables$projectOverview,
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
master_list$summary_tables$projectOverview[[paste0("peakArea.allPlates")]] <- as.numeric(master_list$summary_tables$projectOverview[[paste0("peakArea.allPlates")]])
#### b. peakConcentration -----------------
master_list$summary_tables$projectOverview <- left_join(
  master_list$summary_tables$projectOverview,
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
master_list$summary_tables$projectOverview[[paste0("concentration.allPlates")]] <- as.numeric(master_list$summary_tables$projectOverview[[paste0("concentration.allPlates")]])

### c. preProcessedConcentration --------------
master_list$summary_tables$projectOverview <- left_join(
  master_list$summary_tables$projectOverview,
  rbind(
    c("totalSamples", nrow(bind_rows(master_list$data$pp_final_allPlates))),
    c("studySamples", nrow(bind_rows(master_list$data$pp_final_allPlates) %>% filter(sample_type_factor == "sample"))),
    c("ltrSamples", nrow(bind_rows(master_list$data$pp_final_allPlates) %>% filter(sample_type_factor == "ltr"))),
    c("vltrSamples", nrow(bind_rows(master_list$data$pp_final_allPlates) %>% filter(sample_type_factor == "vltr"))),
    c("sltrSamples", nrow(bind_rows(master_list$data$pp_final_allPlates) %>% filter(sample_type_factor == "sltr"))),
    c("pqcSamples", nrow(bind_rows(master_list$data$pp_final_allPlates) %>% filter(sample_type_factor == "pqc"))),
    c("lipidTargets", ncol(bind_rows(master_list$data$pp_final_allPlates) %>% select(-contains("sample"), - contains("SIL")))),
    c("SIL.IntStds", ncol(bind_rows(master_list$data$pp_final_allPlates) %>% select(contains("SIL")))),
    c("zeroValues[lipidTargets]", length(which(bind_rows(master_list$data$pp_final_allPlates) %>% select(-contains("sample"), -contains("SIL")) ==0))),
    c("zeroValues[SIL.IntStds]", length(which(bind_rows(master_list$data$pp_final_allPlates) %>% select(contains("SIL")) ==0))),
    c("naValues[lipidTargets]", length(which(is.na(as.matrix(bind_rows(master_list$data$pp_final_allPlates) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("naValues[SIL.IntStds]", length(which(is.na(as.matrix(bind_rows(master_list$data$pp_final_allPlates) %>% select(contains("SIL"))))))),
    c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$pp_final_allPlates) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("infValues[SIL.IntStds]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$pp_final_allPlates) %>% select(contains("SIL"))))))),
    c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(bind_rows(master_list$data$pp_final_allPlates) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("nanValues[SIL.IntStds]", length(which(is.nan(as.matrix(bind_rows(master_list$data$pp_final_allPlates) %>% select(contains("SIL"))))))),
    c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterConcentration.allPlates")]] <30))),
    c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterConcentration.allPlates")]] <20))),
    c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterConcentration.allPlates")]] <10)))
  ) %>%
    as_tibble() %>%
    rename(metric = V1,
           !! paste0("postFilterConcentration.allPlates") := V2),
  by = "metric"
)
#change column to numeric
master_list$summary_tables$projectOverview[[paste0("postFilterConcentration.allPlates")]] <- as.numeric(master_list$summary_tables$projectOverview[[paste0("postFilterConcentration.allPlates")]])

#### d. statTargetConcentration --------------
master_list$summary_tables$projectOverview <- left_join(
  master_list$summary_tables$projectOverview,
  rbind(
    c("totalSamples", nrow(bind_rows(master_list$data$statTarget_final_allPlates))),
    c("studySamples", nrow(bind_rows(master_list$data$statTarget_final_allPlates) %>% filter(sample_type_factor == "sample"))),
    c("ltrSamples", nrow(bind_rows(master_list$data$statTarget_final_allPlates) %>% filter(sample_type_factor == "ltr"))),
    c("vltrSamples", nrow(bind_rows(master_list$data$statTarget_final_allPlates) %>% filter(sample_type_factor == "vltr"))),
    c("sltrSamples", nrow(bind_rows(master_list$data$statTarget_final_allPlates) %>% filter(sample_type_factor == "sltr"))),
    c("pqcSamples", nrow(bind_rows(master_list$data$statTarget_final_allPlates) %>% filter(sample_type_factor == "pqc"))),
    c("lipidTargets", ncol(bind_rows(master_list$data$statTarget_final_allPlates) %>% select(-contains("sample"), - contains("SIL")))),
    c("SIL.IntStds", ncol(bind_rows(master_list$data$statTarget_final_allPlates) %>% select(contains("SIL")))),
    c("zeroValues[lipidTargets]", length(which(bind_rows(master_list$data$statTarget_final_allPlates) %>% select(-contains("sample"), -contains("SIL")) ==0))),
    c("zeroValues[SIL.IntStds]", length(which(bind_rows(master_list$data$statTarget_final_allPlates) %>% select(contains("SIL")) ==0))),
    c("naValues[lipidTargets]", length(which(is.na(as.matrix(bind_rows(master_list$data$statTarget_final_allPlates) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("naValues[SIL.IntStds]", length(which(is.na(as.matrix(bind_rows(master_list$data$statTarget_final_allPlates) %>% select(contains("SIL"))))))),
    c("infValues[lipidTargets]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$statTarget_final_allPlates) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("infValues[SIL.IntStds]", length(which(is.infinite(as.matrix(bind_rows(master_list$data$statTarget_final_allPlates) %>% select(contains("SIL"))))))),
    c("nanValues[lipidTargets]", length(which(is.nan(as.matrix(bind_rows(master_list$data$statTarget_final_allPlates) %>% select(-contains("sample"), -contains("SIL"))))))),
    c("nanValues[SIL.IntStds]", length(which(is.nan(as.matrix(bind_rows(master_list$data$statTarget_final_allPlates) %>% select(contains("SIL"))))))),
    c("rsdQc<30%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterStatTarget.allPlates")]] <30))),
    c("rsdQc<20%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterStatTarget.allPlates")]] <20))),
    c("rsdQc<10%", length(which(master_list$process_lists$rsd_filter[[paste0("postFilterStatTarget.allPlates")]] <10)))
  ) %>%
    as_tibble() %>%
    rename(metric = V1,
           !! paste0("postFilterStatTarget.allPlates") := V2),
  by = "metric"
)
#change column to numeric
master_list$summary_tables$projectOverview[[paste0("postFilterStatTarget.allPlates")]] <- as.numeric(master_list$summary_tables$projectOverview[[paste0("postFilterStatTarget.allPlates")]])

# . ------------------------------------------------------------------------------------------------------------------------------  
# PHASE 4: PLOTS ---------------------
## PCA -----------
#pca scores plot function
# master_list$environment$user_functions$pca <- source(paste0(master_list$project_details$github_master_dir,
#                                                             "/functions/FUNC_lipidExploreR_PCA_ggplotv3.5.R"))
#setList
master_list$pca <- list()
### 1. peakArea ----
master_list$pca$peakArea <- list()

#### a. color by sample_type ----
#run function
master_list$pca$peakArea$sample_type <- master_list$environment$user_functions$pca$value(
  FUNC_data =  bind_rows(
    bind_rows(master_list$data$area_sorted),
    bind_rows(master_list$data$area_concentration_final_all_plates),
    bind_rows(master_list$data$pp_final_allPlates) %>%
      mutate(sample_data_source = str_replace(sample_data_source, "postFilterConcentration", "concentration[postFilter]")),
    bind_rows(master_list$data$statTarget_final_allPlates) %>%
      mutate(sample_data_source = str_replace(sample_data_source, "statTargetConcentration", "statTarget[postFilter]")),
  ) %>% 
    mutate(sample_data_source = factor(
      sample_data_source, 
      levels = c("peakArea",
                 "concentration",
                 "concentration[postFilter]",
                 "statTarget[postFilter]"),
      ordered = TRUE
    )) %>%
    select(-contains("SIL")), 
  FUNC_HEADER_run_order = "sample_run_index",
  FUNC_HEADER_plate_id = "sample_plate_id",
  FUNC_HEADER_colour_by = "sample_type_factor",
  FUNC_HEADER_highlight_by = "sample_type_factor",
  FUNC_HEADER_label_by = "sample_name",
  FUNC_HEADER_facet = "sample_data_source",
  FUNC_OPTION_title = paste0("uncorrectedPeakArea; ", master_list$project_details$project_name),
  FUNC_OPTION_colours = master_list$project_details$plot_colour,
  FUNC_OPTION_fill = master_list$project_details$plot_fill,
  FUNC_OPTION_size = master_list$project_details$plot_size,
  FUNC_OPTION_shape =master_list$project_details$plot_shape,
  FUNC_OPTION_legend_title = "sample_type"
)

#### b. color by plateID ----
#run function
master_list$pca$peakArea$plateID <- master_list$environment$user_functions$pca$value(
  FUNC_data =  bind_rows(
    bind_rows(master_list$data$area_sorted),
    bind_rows(master_list$data$area_concentration_final_all_plates),
    bind_rows(master_list$data$pp_final_allPlates) %>%
      mutate(sample_data_source = str_replace(sample_data_source, "postFilterConcentration", "concentration[postFilter]")),
    bind_rows(master_list$data$statTarget_final_allPlates) %>%
      mutate(sample_data_source = str_replace(sample_data_source, "statTargetConcentration", "statTarget[postFilter]")),
  ) %>% 
    mutate(sample_data_source = factor(
      sample_data_source, 
      levels = c("peakArea",
                 "concentration",
                 "concentration[postFilter]",
                 "statTarget[postFilter]"),
      ordered = TRUE
    )) %>%
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

## targetControlCharts ------------------

#control chart check
# master_list$environment$user_functions$control_chart <- source(paste0(master_list$project_details$github_master_dir,
#                                                                       "/functions/FUNC_lipidExploreR_controlChart.R"))

master_list$control_chart <- master_list$environment$user_functions$control_chart$value(
  FUNC_data_area = bind_rows(master_list$data$area_sorted),
  FUNC_data_area_concentration = bind_rows(master_list$data$area_concentration) %>%
    filter(sample_name %in% filter(master_list$process_lists$mvSample, sampleKeep == 1)[["sample_name"]]) %>%
    mutate(sample_data_source = str_replace(sample_data_source, "concentration", "concentration[postFilter]")),
  FUNC_data_statTarget_concentration = bind_rows(master_list$data$statTarget_concentration) %>%
    filter(sample_name %in% filter(master_list$process_lists$mvSample, sampleKeep == 1)[["sample_name"]]) %>%
    mutate(sample_data_source = str_replace(sample_data_source, "statTargetConcentration", "statTarget[postFilter]")),
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
#PHASE 5: EXPORTS ------------------------------------
## XLSX FILE -------------------------------
### 1. create peakArea user guide ----
master_list$summary_tables$odsAreaOverview <- rbind(
  c("projectName", master_list$project_details$project_name),
  c("user", master_list$project_details$user_name),
  c("lipidExploreR_version", master_list$project_details$lipidExploreR_version),
  c("qcType_preProcessing|filtering", master_list$project_details$qc_type),
  c("SIL_Int.Std_version", master_list$project_details$is_ver),
  c("total.StudyPlates", bind_rows(master_list$data$area_sorted)[["sample_plate_id"]] %>% unique() %>% length()),
  c("total.Samples", bind_rows(master_list$data$area_sorted) %>% nrow()),
  c("studySamples", bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "sample") %>% nrow()),
  c("ltr", bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "ltr") %>% nrow()),
  c("vltr", bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "vltr") %>% nrow()),
  c("sltr", bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "sltr") %>% nrow()),
  c("pqc", bind_rows(master_list$data$area_sorted) %>% filter(sample_type_factor == "pqc") %>% nrow()),
  c("total.LipidFeatures", bind_rows(master_list$data$area_sorted) %>% select(-contains("sample")) %>% select(-contains("SIL")) %>% ncol()),
  c("total.SIL.Int.Stds", bind_rows(master_list$data$area_sorted) %>% select(contains("SIL")) %>% ncol()),
  c("", ""),
  c("TAB_DESCRIPTION:", ""),
  c("QC.platePerformance", "overview of project quality performance (per plate)"),
  c("QC.samplesMV", "detailed overview of sample quality (missing values)"),
  c("QC.lipidsMV", "detailed overview of lipid quality (missing values)"),
  c("QC.lipidQcRsd", "detailed overview of lipid quality (% RSD in QC samples)"),
  c("DATA.lipidPeakArea", "lipid target peak area integrals (skylineMS)"),
  c("DATA.silPeakArea", "stable isotope labelled internal standard peak area integrals (skylineMS)"),
  c("DATA.allConcentration", "concentration values calculated from single point concentration factor adjustment of lipid target/SIL.int.std peak area response ratios"),
  c("DATA.preProcessedConcentration", "concentration data that has undergone data pre-processing: missing value sample filter [>50%]; missing value feature filter [>50%]; imputation remaining missing values [min/2]; relative standard deviation filtering [>30%])"),      
  c("DATA.statTargetConcentration", "concentration data that has undergone pre-processing including signalDrift | batch correction using the statTarget r package;  missing value sample filter [>50%]; missing value feature filter [>50%]; imputation remaining missing values [min/2]; relative standard deviation filtering [>30%])")
  ) %>%
  as_tibble() 

### 2. write .xlsx tabbed file ----
openxlsx::write.xlsx(
  file = paste0(master_list$project_details$project_dir, 
                "/xlsx_report/",
                Sys.Date(),
                "_",
                master_list$project_details$project_name,
                "_LGW_lipidData_qcCheckeR_v3.5.xlsx"),
  overwrite = TRUE,
  x = list(
    "userGuide" = master_list$summary_tables$odsAreaOverview,
    #summary
    "QC.platePerformance" = (master_list$summary_tables$projectOverview %>% t() %>% as.data.frame() %>% rownames_to_column() %>% setNames(nm = .[1,]))[-1,] %>% 
      mutate(across(-metric, as.numeric)) %>%
      separate_wider_delim(
        cols = metric,
        delim = ".",
        names = c("dataSource", "samplePlate")
      ) %>%
      replace(is.na(.), 0),
    "QC.sampleMV" = master_list$process_lists$mvSamples,
    "QC.lipidsMV" = (master_list$process_lists$mvLipids %>% t() %>% as.data.frame() %>% rownames_to_column() %>% setNames(nm = .[1,]))[-1,] %>% 
      mutate(across(-lipid, as.numeric)) %>%
      separate_wider_delim(
        cols = lipid,
        delim = ".",
        names = c("samplePlate", "filterMetric"),
        too_few = "align_end"
      ),
    "QC.lipidQcRsd" = (
      master_list$process_lists$rsd_filter %>% 
        mutate(across(-lipid, round, 2)) %>%
        t() %>% as.data.frame() %>% rownames_to_column() %>% setNames(nm = .[1,]))[-1,] %>% 
      mutate(across(-lipid, as.numeric)) %>%
      separate_wider_delim(
        cols = lipid,
        delim = ".",
        names = c("dataSource", "samplePlate")
      ),
    #data
    "DATA.peakArea" = bind_rows(master_list$data$area_sorted) %>% select(-contains("SIL")),
    "DATA.silPeakArea" = bind_rows(master_list$data$area_sorted) %>% select(contains("sample") | contains("SIL")),
    "DATA.concentration" = bind_rows(master_list$data$area_concentration),
    "DATA.preProcessedConcentration" = master_list$data$pp_final_allPlates[[1]],
    "DATA.statTargetConcentration" = master_list$data$statTarget_final_allPlates[[1]]
  )
)

# . ------------------------------------------------------------------------------------------------------------------------------  

## HTML EXPORT -------------------------

### 1. download template ----
# fileConn<-file(paste0(master_list$project_details$project_dir, "/html_report/lipid_exploreR_report_templatev3.3.R"))
# writeLines(httr::GET(url = paste0(master_list$project_details$github_master_dir, "/templates/TEMPLATE_lipidExploreR_report_v3.3.R")) %>%
#              httr::content(as = "text"), fileConn)
# close(fileConn)


### 2. render template ----
rmarkdown::render(input = paste0(master_list$project_details$project_dir, "/html_report/lipid_exploreR_report_templatev3.5.R"),
                  output_format = "html_document",
                  output_dir = paste0(master_list$project_details$project_dir, "/html_report"),
                  output_file = paste0(Sys.Date(), "_", master_list$project_details$project_name, "_LGW_lipidExploreR_qcCheckeR_report_v3.35.html")
)

### 3. browse template ----
browseURL(url = paste0(master_list$project_details$project_dir, 
                       "/html_report/",
                       Sys.Date(), "_", master_list$project_details$project_name, "_LGW_lipidExploreR_qcCheckeR_report_v3.35.html")
)


# . ------------------------------------------------------------------------------------------------------------------------------  

## RDA EXPORT -------

#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))
### 1. export rda of master_list ----

save(master_list,
     file = paste0(
       master_list$project_details$project_dir,
       "/data/rda/", Sys.Date(), 
       "_LGW_qcCheckeR_v3.5_", 
       master_list$project_details$project_name, 
       ".rda"))

#. ----
