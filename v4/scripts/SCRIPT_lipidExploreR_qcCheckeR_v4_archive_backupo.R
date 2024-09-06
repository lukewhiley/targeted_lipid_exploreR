# . ------------------------------------------------------------------------------------------------------------------------------  
# PROJECT SETUP: --------------------------------------
## 1. load packages: --------------------------------------
package_list <- c('svDialogs', 'plyr', 'tidyverse', 'plotly', 'statTarget', 'janitor', 'ropls', 'knitr', 'mzR', 'httr', 'matrixStats', 'openxlsx')

for(idx_package in package_list){
  if(length(which(row.names(installed.packages()) == idx_package)) > 0){
    suppressMessages(require(package = idx_package,
                             character.only = TRUE))
  } else {
    dlg_message(
      paste0("the package ", idx_package, " is not installed. Please install ", idx_package, " before continuing.")
    )
  }
}

## 2. Welcome section/project set upload skylineR --------------------------------------

#welcome messages
#if(!exists("master_list")){
dlg_message("Welcome to lipid qc exploreR! :-)", type = 'ok'); dlg_message("please run the skylineR script (perPlate) before running this script", type = 'ok'); dlg_message("if combining multiple plates/batches select parent master folder. e.g. ~parent/skylineR_plate01; ~parent/skylineR_plate02", type = 'ok');       
# choose directory
dlg_message("select parent master folder with skylineR subfolders", type = 'ok');skylineR_directory <- rstudioapi::selectDirectory()
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

#load and combine plate RDA files from skylineR
if(length(rda_fileList > 1)){
  #run loop to combine .rda files
  for(idx_rda in rda_fileList[2:length(rda_fileList)]){
    
    #load loop .rda
    load(paste(skylineR_directory, idx_rda, sep ="/"))
    
    #combine mzml
    for(idx_mzml in names(master_list$data$mzR)){masterListBatch$data$mzR[[idx_mzml]] <- master_list$data$mzR[[idx_mzml]]}
    
    #combine plate list
    masterListBatch$project_details$mzml_plate_list <- c(
      masterListBatch$project_details$mzml_plate_list,
      master_list$project_details$mzml_plate_list)
    
    #combine mzml_sample_list
    masterListBatch$project_details$mzml_sample_list <- c(
      masterListBatch$project_details$mzml_sample_list,
      master_list$project_details$mzml_plate_list)
    
    #combine skylineR data
    masterListBatch$data$skyline_report <- bind_rows(
      masterListBatch$data$skyline_report,
      master_list$data$skyline_report)
    
    #remove master_list for next iteration of loop
    rm(master_list)
  } #close for idx_ra
}#close if length(rda_filelist)>1 statement

#reset to master_list for remainder of script and removal of masterlistBatchObject
master_list <- masterListBatch; rm(masterListBatch)

#set version of lipidExploreR used
master_list$project_details$lipidExploreR_version <- "4.0"
master_list$project_details$qcCheckR_version <- "4.0"
master_list$summary_tables$project_summary$value[which(master_list$summary_tables$project_summary$`Project detail` == "lipidExploreR version")] <- "4.0"
master_list$summary_tables$area_project_summary$value[which(master_list$summary_tables$area_project_summary$`Project detail` == "lipidExploreR version")] <- "4.0"

## 3. Load templates/guides --------------------------------------
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

# PHASE 1: PREPARE DATA  -----------------
master_list$data$peakArea <- list()
## 1.1. move skyline data and nest under peakArea ------
master_list$data$peakArea$skylineReport <- master_list$data$skyline_report
#remove skyline report
master_list$data$skyline_report <- NULL
## 1.2. transpose data to standard metabolomics structure (features in columns, samples in rows) ---------------------------------------
master_list$data$peakArea$transposed <- list()
#run loop for each data plate/batch
for(idx_batch in master_list$project_details$mzml_plate_list){
  master_list$data$peakArea$transposed[[idx_batch]] <- pivot_wider(
    data = master_list$data$peakArea$skylineReport %>%
      filter(file_name %in% names(master_list$data$mzR[[idx_batch]])),
    id_cols = file_name,
    names_from = molecule_name,
    values_from = area
  ) %>%
    rename(sample_name = file_name) 
  
  #remove file extenstion (.mzML from sample_name)
  master_list$data$peakArea$transposed[[idx_batch]]$sample_name <- sub(".mzML", "", master_list$data$peakArea$transposed[[idx_batch]]$sample_name)
  
  
  #make numeric 
  master_list$data$peakArea$transposed[[idx_batch]][,-1] <-
    sapply(master_list$data$peakArea$transposed[[idx_batch]][,-1], 
           as.numeric) %>% 
    as_tibble() 
}

## 1.3. Sort by run order and add annotation data ------------------------------- 
#list for storing concentration data area_sorted by run order
master_list$project_details$run_orders <- list()
#set up sorted area list
master_list$data$peakArea$sorted <- list()

#run loop
for (idx_batch in names(master_list$data$peakArea$transposed)){
  master_list$project_details$run_orders[[idx_batch]] <- sub(".mzML", "", names(master_list$data$mzR[[idx_batch]])) %>% as_tibble() %>% rename(sample_name = value)
  temp_timestamp <- NULL
  for(idx_file in names(master_list$data$mzR[[idx_batch]])){
    temp_timestamp <- c(temp_timestamp, master_list$data$mzR[[idx_batch]][[idx_file]]$mzR_timestamp)
  }
  master_list$project_details$run_orders[[idx_batch]] <-  master_list$project_details$run_orders[[idx_batch]] %>%
    add_column(sample_timestamp = temp_timestamp) %>%
    arrange(sample_timestamp)
  
  #create metadata columns
  master_list$project_details$run_orders[[idx_batch]]$sample_batch <- master_list$project_details$project_name # batch/project name
  master_list$project_details$run_orders[[idx_batch]]$sample_plate_id <- idx_batch #plate_id
  master_list$project_details$run_orders[[idx_batch]]$sample_plate_order <- c(1:nrow(master_list$project_details$run_orders[[idx_batch]])) #sample_plate_order
  master_list$project_details$run_orders[[idx_batch]]$sample_type <- NA
  
  # set sample_type using mutate
  master_list$project_details$run_orders[[idx_batch]] <- master_list$project_details$run_orders[[idx_batch]]  %>%
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
  master_list$data$peakArea$sorted[[idx_batch]] <- master_list$project_details$run_orders[[idx_batch]] %>%
    left_join(master_list$data$peakArea$transposed[[idx_batch]], by = "sample_name") %>% 
    filter(sample_type == "pqc" | sample_type == "sample" | sample_type == "ltr" | sample_type == "sltr" | sample_type == "vltr") %>%
    arrange(sample_timestamp) %>%
    add_column(sample_run_index = c(1:nrow(.)), .before = 1)
  
  #add_factor column for plotting
  master_list$data$peakArea$sorted[[idx_batch]] <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    add_column(sample_type_factor = master_list$data$peakArea$sorted[[idx_batch]]$sample_type %>% 
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
  master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]][-which(tolower(master_list$project_details$qc_type) == tolower(master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]]))] <- "sample"
  master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]][which(tolower(master_list$project_details$qc_type) == tolower(master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]]))] <- "qc"

  #add sample_data_source column
  master_list$data$peakArea$sorted[[idx_batch]] <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    add_column(sample_data_source = ".peakArea",
               .after = "sample_type_factor_rev")
}

#add a global runorder based on mzML sample timestamp
tempAllSamples <- master_list$data$peakArea$sorted %>%
  bind_rows() %>%
  arrange(sample_timestamp)
#set project master run index
tempAllSamples$sample_run_index <- c(1:nrow(tempAllSamples))
#reset sorted list
master_list$data$peakArea$sorted <-list()
for(idx_batch in unique(tempAllSamples$sample_plate_id)){
  master_list$data$peakArea$sorted[[idx_batch]] <- tempAllSamples %>%
    filter(sample_plate_id == idx_batch)
}

## 1.4. impute missing values [min/2 imputation (missing assumed < LOD)] -----------------------------------------------------
#Imputation of the all zero value and missing data 
#Imputation is completed using x/2, where x is minimum intensity of that feature in the batch

#impute function
#add LGW impute function
lgw_impute <- function(x){
  map(.x = x, .f = ~ (min(.x[.x > 0], na.rm = TRUE))/2) %>%
    #use replace_na to replace NAs with min/2 value
    replace_na(
      data = x %>% mutate_all(~ replace(., . == 0, NA)), #note - replace zeros with NA to make compatible with replace_na()
      replace = .) #note - replace with list of min/2 values generated from map function in pipe (.)
}

#set data list
master_list$data$peakArea$imputed <- list()
#run loop in all plates
for(idx_batch in names(master_list$data$peakArea$sorted)){
  #set all 0, NaN, is.infinate to a NA value for consistency
  #create matrix 
  master_list$data$peakArea$imputed[[idx_batch]] <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    column_to_rownames("sample_name") %>%
    select(-contains("sample")) %>%
    as.matrix()
  
  #replace 0, NaN and Inf with NA for imputation (min/2)
  master_list$data$peakArea$imputed[[idx_batch]][master_list$data$peakArea$imputed[[idx_batch]] ==0] <- NA
  master_list$data$peakArea$imputed[[idx_batch]][is.infinite(master_list$data$peakArea$imputed[[idx_batch]])] <- NA
  master_list$data$peakArea$imputed[[idx_batch]][is.nan(master_list$data$peakArea$imputed[[idx_batch]])] <- NA
  
  
  #run lgw_impute function
  master_list$data$peakArea$imputed[[idx_batch]] <- master_list$data$peakArea$imputed[[idx_batch]] %>% 
    as.data.frame() %>%
    lgw_impute() %>%
    rownames_to_column("sample_name") %>%
    as_tibble() %>%
    left_join(
      select(master_list$data$peakArea$sorted[[idx_batch]], contains("sample")),
      .,
      by = "sample_name"
    )

  master_list$data$peakArea$imputed[[idx_batch]]$sample_data_source <- ".peakAreaImputed"
  
  }


## 1.5. statTarget signalDrift | batch correction ----------------------------
#create batch correction directory
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/batch_correction"))){
  dir.create(paste0(master_list$project_details$project_dir, "/data/batch_correction"))
}

#create data list 
FUNC_list <- list()
FUNC_list$project_dir <- paste0(master_list$project_details$project_dir,
                                "/data/batch_correction")

#run batch correction 
if(!dir.exists(paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results"))){
  dir.create(paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results"))
}

setwd(paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results")) 

#apply on peakArea data (post-impute)
#set master data for function
FUNC_list$master_data <- bind_rows(master_list$data$peakArea$imputed)
#set_qc type used for signal drift correction
FUNC_list$master_data[["sample_type"]] <- "sample"
FUNC_list$master_data[["sample_type"]][which(FUNC_list$master_data[["sample_type_factor"]] == "vltr")] <- "qc"

#set metabolite list
FUNC_list$metabolite_list <- master_list$data$peakArea$imputed %>%
  bind_rows() %>%
  select(-contains("sample")) %>%
  names()

#### 1.5.a. create the required metadata file (PhenoFile) for statTarget::shiftCor---------------------------------------------------------------
FUNC_list$PhenoFile <- list()
# build PhenoFile file template
FUNC_list$PhenoFile$template <- FUNC_list$master_data %>% 
  select(all_of("sample_name")) %>%
  rename(sample = all_of("sample_name")) %>%
  add_column(FUNC_list$master_data %>% 
               select(all_of("sample_name"))) %>%
  add_column(FUNC_list$master_data %>%
               select(all_of("sample_plate_id"))) %>%
  add_column(FUNC_list$master_data %>%
               select(all_of("sample_type"))) %>%
  rename(class = all_of("sample_type")) %>% 
  add_column(FUNC_list$master_data %>%
               select(all_of("sample_type"))) %>%
  add_column(FUNC_list$master_data %>%
               select(all_of("sample_run_index")))

#### arrange by run order
FUNC_list$PhenoFile$template <- FUNC_list$PhenoFile$template %>%
  arrange_at("sample_run_index")

#### QC placement
#stat target needs a qc at postion 1 and last position in the run. Default run order takes this into account, but some datasets this is not the case
#in this instance this section of code artificially moves the first and last qc into position. It is completed for each batch.

FUNC_list$PhenoFile$template_qc_order <- NULL
qc_idx <- NULL
for(idx_batch in FUNC_list$PhenoFile$template %>% 
    select(all_of("sample_plate_id")) %>%
    unique() %>%
    as.matrix() %>%
    c()
){
  
  #create a temp tibble batch specific
  loop_temp_data <- FUNC_list$PhenoFile$template %>%
    filter(!!as.symbol("sample_plate_id") == idx_batch)
  
  #ensure a sample_type "qc" is "first" and "last" in the worklist order. Required for statTarget::shiftCor
  loop_qc_idx <- which(loop_temp_data %>% 
                         select(all_of("sample_type")) 
                       == "qc")
  # browser()
  #if qc is not run before the samples - artificially move first qc to run order position 1. This is required for statTarget
  if(loop_qc_idx[1] > 1){
    loop_temp_data <- loop_temp_data %>%
      slice(loop_qc_idx[1],1:nrow(loop_temp_data)) %>%
      slice(-(loop_qc_idx[1]+1))
  }
  
  #create last qc
  if(loop_qc_idx[length(loop_qc_idx)] < nrow(loop_temp_data)){
    loop_temp_data <- loop_temp_data %>%
      slice(1:nrow(loop_temp_data), loop_qc_idx[length(loop_qc_idx)]) %>%
      slice(-loop_qc_idx[length(loop_qc_idx)])
  }
  
  #create total qc_idx for use later
  qc_idx <- c(qc_idx,
              loop_qc_idx)
  
  FUNC_list$PhenoFile$template_qc_order <- bind_rows(FUNC_list$PhenoFile$template_qc_order,
                                                     loop_temp_data)
  
}

#set sample column for statTarget requires "QC" in QC rows, and sample name in sample rows
FUNC_list$PhenoFile$template_qc_order$sample[which(FUNC_list$PhenoFile$template_qc_order %>% 
                                                     select(all_of("sample_type")) == "qc")] <- paste0("QC", rep(1:length(qc_idx)))
FUNC_list$PhenoFile$template_qc_order$sample[which(FUNC_list$PhenoFile$template_qc_order %>% 
                                                     select(all_of("sample_type")) == "sample")] <- paste0("sample", 
                                                                                                           rep(1:(nrow(FUNC_list$PhenoFile$template_qc_order)-length(qc_idx))))
#set NA for class column in rows that are NA
FUNC_list$PhenoFile$template_qc_order$class[which(FUNC_list$PhenoFile$template_qc_order %>% 
                                                    select(all_of("sample_type")) == "qc")] <- NA

#rename column header for statTarget template
FUNC_list$PhenoFile$template_sample_id <- FUNC_list$PhenoFile$template_qc_order %>% 
  rename(sample_id = all_of("sample_name"),
         batch = all_of("sample_plate_id"),
         order = all_of("sample_run_index")) %>%
  select(sample, batch, class, order, sample_id)

#confirm order columnn is continuous
FUNC_list$PhenoFile$template_sample_id$order <- c(1:nrow(FUNC_list$PhenoFile$template_sample_id))

#set batch/plate - numeric value starting at 1 - max number of plates/batch
temp_batch <- 1
for(idx_batch_set in unique(FUNC_list$PhenoFile$template_sample_id$batch)){
  FUNC_list$PhenoFile$template_sample_id$batch[which(FUNC_list$PhenoFile$template_sample_id$batch == idx_batch_set)] <- temp_batch
  temp_batch <- temp_batch + 1
}

FUNC_list$PhenoFile$template_sample_id$batch <- FUNC_list$PhenoFile$template_sample_id$batch %>%
  as.numeric()

#final Phenofile
FUNC_list$PhenoFile$PhenoFileOut <- FUNC_list$PhenoFile$template_sample_id %>%
  select(-sample_id)

# write out as csv (requirement for statTarget::shiftCor)
write_csv(x = FUNC_list$PhenoFile$PhenoFileOut,
          file = paste(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results", "/PhenoFile.csv", sep="")
)

#### 1.5.b. create data for statTarget::shiftCor  -----------------------------------
FUNC_list$ProfileFile <- list()

#must have samples in columns and metabolites in rows
FUNC_list$ProfileFile$template  <- FUNC_list$master_data %>%
  select(all_of("sample_name"),
         all_of(FUNC_list$metabolite_list)) %>%
  rename(sample_id = !!"sample_name")

#match run order to PhenoFile
FUNC_list$ProfileFile$template_qc_order <- FUNC_list$PhenoFile$template_sample_id %>%
  select(sample, sample_id) %>%
  left_join(FUNC_list$ProfileFile$template, by = "sample_id") %>%
  select(-sample_id)

#transpose tibble for statTarget
FUNC_list$ProfileFile$ProfileFile <- as_tibble(
  cbind(nms = names(FUNC_list$ProfileFile$template_qc_order), 
        t(FUNC_list$ProfileFile$template_qc_order))
) %>%
  setNames(.[1,]) %>%
  rename(name = sample) %>%
  filter(name != "sample") %>%
  mutate(across(!contains("name", ignore.case = FALSE), as.numeric))

#create a metabolite list and create metabolite code
FUNC_list$ProfileFile$metabolite_list <- FUNC_list$ProfileFile$ProfileFile %>%
  select(name) %>%
  add_column(metabolite_code = paste0("M", rep(1:nrow(FUNC_list$ProfileFile$ProfileFile))))

#add metabolite code to data
FUNC_list$ProfileFile$ProfileFile <- left_join(
  FUNC_list$ProfileFile$metabolite_list,
  FUNC_list$ProfileFile$ProfileFile,
  by = "name") %>%
  select(-name) %>%
  rename(name = metabolite_code)

# write out as csv (requirement for statTarget::shiftCor)
write_csv(x = FUNC_list$ProfileFile$ProfileFile, 
          file = paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results/ProfileFile.csv")
)

#script files
samPeno <- paste(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results", "/PhenoFile.csv", sep="")
samFile <- paste(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results",  "/ProfileFile.csv", sep="")

#run statTarget
statTarget::shiftCor(samPeno = samPeno,
                     samFile =  samFile,
                     Frule = 0,
                     ntree = 500,
                     MLmethod = 'QCRFSC',
                     imputeM = "minHalf",
                     plot = FALSE,
                     coCV = 10000
)

#re-import data produced by statTarget and return it to format for LGW lipid pipeline
FUNC_list$corrected_data$data <- read_csv(
  paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results", "/statTarget/shiftCor/After_shiftCor/shift_all_cor.csv"),
  show_col_types = FALSE) %>%
  filter(sample != "class") %>%
  rename(name = sample) %>%
  mutate(across(!contains("name", ignore.case = FALSE), as.numeric))

#recombine with sample filenames and lipid names
FUNC_list$corrected_data$data_transposed <- right_join(
  FUNC_list$ProfileFile$metabolite_list %>% rename(lipid = metabolite_code),
  FUNC_list$corrected_data$data %>% rename(lipid = name),
  by = "lipid"
) %>%
  select(-lipid) %>%
  as.matrix() %>%
  t() %>% data.frame() %>%
  rownames_to_column() %>% 
  as_tibble() %>%
  setNames(.[1,]) %>%
  filter(name!="name") %>%
  mutate(across(!contains("name", ignore.case = FALSE), as.numeric)) %>%
  rename(sample = name) %>%
  left_join(x = FUNC_list$PhenoFile$template_sample_id,
            y = .,
            by = "sample") %>%
  rename(!!"sample_name" := sample_id) %>%
  left_join(
    x = FUNC_list$master_data %>%
      select(contains("sample")),
    y = .,
    by = "sample_name"
  ) %>%
  select(-all_of(c("sample", "batch", "class", "order")))

#### 1.5.c. post-statTarget peak area mean adjustment ------------------------------------
#because the StatTarget correction changes the output signal area of the lipids, this next section re-scales the values based on the change (ratio) between pre and post corrected signal mean in the QCs
#step one - get mean value for each metabolite in the QC samples - pre-single drift corrected data 
FUNC_list$corrected_data$qc_means <- FUNC_list$master_data %>%
  filter(!!as.symbol("sample_type") == "qc") %>%
  select(-contains("sample")) %>%
  colMeans() %>%
  as_tibble() %>%
  rename(original_mean = value) %>%
  add_column(metabolite = FUNC_list$master_data %>%
               select(-contains("sample")) %>%
               names(), 
             .before = "original_mean") %>%
  #step two - get mean value for each metabolite in the QC samples - post-single drift corrected data 
  left_join(.,
            FUNC_list$corrected_data$data_transposed %>%
              filter(!!as.symbol("sample_type") == "qc") %>%
              select(-contains("sample")) %>%
              colMeans() %>%
              data.frame %>%
              rownames_to_column() %>%
              as_tibble() %>%
              setNames(c("metabolite", "corrected_mean")),
            by = "metabolite") %>%
  #step three - create ratio factor for concentration adjustment
  add_column(correction_ratio = .$corrected_mean/.$original_mean)

#step 4 - adjust data concentrations
FUNC_list$corrected_data$data_qc_mean_adjusted <- FUNC_list$corrected_data$data_transposed
#run loop
for(idx_metabolite in FUNC_list$corrected_data$qc_means$metabolite){
  FUNC_list$corrected_data$data_qc_mean_adjusted[[idx_metabolite]] <- FUNC_list$corrected_data$data_qc_mean_adjusted[[idx_metabolite]]/
    FUNC_list$corrected_data$qc_means[["correction_ratio"]][which(FUNC_list$corrected_data$qc_means[["metabolite"]]==idx_metabolite)]
}

#return QC type to ltr data
FUNC_DATA_qc_type <- unique(bind_rows(master_list$data$peakArea$sorted)$sample_type_factor[which(bind_rows(master_list$data$peakArea$sorted)$sample_type =="qc")]) %>% as.character()
FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type<- "sample"
FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type[which(FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type_factor == FUNC_DATA_qc_type)] <- "qc"

#list for storing signal drift corrected data (per project)
master_list$data$peakArea$statTargetProcessed <- list()
#loop to back into plate elements of list
for(idx_batch in unique(FUNC_list$corrected_data$data_qc_mean_adjusted$sample_plate_id)){
  master_list$data$peakArea$statTargetProcessed[[idx_batch]] <- FUNC_list$corrected_data$data_qc_mean_adjusted %>%
    filter(sample_plate_id == idx_batch) %>%
    mutate(across(where(is.numeric), round, 0))
  #set dataSource
  master_list$data$peakArea$statTargetProcessed[[idx_batch]]$sample_data_source <- "peakArea.statTarget"
}

#reset wd to project dir
setwd(master_list$project_details$project_dir)

#remove extra files
rm(list = c(ls()[which(ls() != "master_list")]))

## 1.6. calculate peakResponse and concentration values -------------

#setlist
master_list$data$response <- list()
master_list$data$concentration <- list()

#complete for both peakArea sorted and statTarget
for (idx_dataType in c("sorted", "imputed", "statTargetProcessed")){

#### 1.6.a. calculate peakResponse for sorted area and statTarget -----------
  master_list$data$response[[idx_dataType]] <- list()
  
  #for loop for each data plate
  for(idx_batch in names(master_list$data$peakArea[[idx_dataType]])){
    #set empty list to store output data for response
    master_list$data$response[[idx_dataType]][[idx_batch]] <- master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>% select(contains("sample"))
    #set empty list to store output data for concentration
    master_list$data$concentration[[idx_dataType]][[idx_batch]] <- master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>% select(contains("sample"))
    #run loop for each SIL IS.
    for(idx_SIL in master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>% 
        select(contains("SIL")) %>% 
        names()){
      if(length(master_list$templates$SIL_guide$precursor_name[which(master_list$templates$SIL_guide$note == idx_SIL)])>0){
        #find which SIL is used from the template
        target_lipids <- select(master_list$data$peakArea[[idx_dataType]][[idx_batch]], 
                                sample_name,
                                any_of(master_list$templates$SIL_guide$precursor_name[which(master_list$templates$SIL_guide$note == idx_SIL)])) %>%
          column_to_rownames("sample_name")
        if(ncol(target_lipids)>0){
          #calculate response ratio
          target_lipids_response <- as.matrix(target_lipids/master_list$data$peakArea[[idx_dataType]][[idx_batch]][[idx_SIL]])
          #standardise data set to remove infinates and NAs
          target_lipids_response[is.na(target_lipids_response)] <- 0
          target_lipids_response[is.infinite(target_lipids_response)] <- 0
          #round and improve decimal places (2dp for >1; 3 sf for < 1)
          target_lipids_response[target_lipids_response <1] <- signif(target_lipids_response[target_lipids_response <1], 2)
          target_lipids_response[target_lipids_response >1] <- round(target_lipids_response[target_lipids_response >1], 2)
          #rejoin and make master
          master_list$data$response[[idx_dataType]][[idx_batch]] <- left_join(by = "sample_name",
            master_list$data$response[[idx_dataType]][[idx_batch]], 
            as.data.frame(target_lipids_response) %>% 
              rownames_to_column("sample_name")
          )
        } #if ncol()
        
        ## 1.6.b. convert response to single point concentration factor ---------
        #Find the concentration factor of SIL
        sil_conc_factor <- master_list$templates$conc_guide$concentration_factor[which(master_list$templates$conc_guide$sil_name == idx_SIL)]
        if(length(sil_conc_factor) == 1){
          #select response data
          target_lipids <- master_list$data$response[[idx_dataType]][[idx_batch]] %>%
            select(sample_name,
                   any_of(master_list$templates$SIL_guide$precursor_name[which(master_list$templates$SIL_guide$note == idx_SIL)])) %>%
            column_to_rownames("sample_name")
          #calculate concentration
          target_lipids_concentration <- as.matrix(target_lipids*sil_conc_factor)
          #standardise data set to remove infinates and NAs
          target_lipids_concentration[is.na(target_lipids_concentration)] <- 0
          target_lipids_concentration[is.infinite(target_lipids_concentration)] <- 0
          #round and improve decimal places (2dp for >1; 3 sf for < 1)
          target_lipids_concentration[target_lipids_concentration <1] <- signif(target_lipids_concentration[target_lipids_concentration <1], 2)
          target_lipids_concentration[target_lipids_concentration >1] <- round(target_lipids_concentration[target_lipids_concentration >1], 2)
          #rejoin and make master
          master_list$data$concentration[[idx_dataType]][[idx_batch]] <- left_join(by = "sample_name",
                                                                                   master_list$data$concentration[[idx_dataType]][[idx_batch]], 
                                                                                   as.data.frame(target_lipids_concentration) %>% 
                                                                                     rownames_to_column("sample_name")
                                                                                   )
        } #close if(length(sil_conc_factor) == 1)
      } #if length()
    } #idx_sil
    master_list$data$response[[idx_dataType]][[idx_batch]]$sample_data_source <- paste0(".response.", idx_dataType)
    master_list$data$concentration[[idx_dataType]][[idx_batch]]$sample_data_source <- paste0("concentration.", idx_dataType)
  } #idx_batch
} #idx_dataType

rm(list = c(ls()[which(ls() != "master_list")]))


# data is now prepared for data preProcessing

#.-----------

# PHASE 2: DATA FILTERING ------------------
# mising value data filtering is performed on peakArea data from the skyLine export.
# First the filtering is performed per sample to identify failed samples (e.g. injection, extraction, preparation errors, or if sample is missing from well) it will return a high % of missing values
# Once failed samples have been identified - the filtering then identifies lipids that have >50% missing values
# note: missing also refers to <limit of detection [<LOD]. This refers to instances of peak areas that are <5000 counts, as skyline will sometimes integrate noise giving a small value.

master_list$filters <- list()

## 2.1. sample filter [missing value] -------
master_list$filters$samples.missingValues <- list()
# complete on raw uncorrected peakArea data
master_list$filters$samples.missingValues <- master_list$data$peakArea$sorted %>%
  bind_rows() %>%
  select(
    sample_run_index,
    sample_name,
    sample_batch,
    sample_plate_id,
    sample_type_factor
  )

#zero values [samples] (values with a peakArea of <5000 are considered <LOD and are noise)
master_list$filters$samples.missingValues[["missing.lipid[<LOD.peakArea<5000]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix()) <5000, na.rm =T)

#zero values [SIL Int.Stds]
master_list$filters$samples.missingValues[["missing.SIL.Int.Std[<LOD.peakArea<5000]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix()) <5000, na.rm =T)

#na values [samples]
master_list$filters$samples.missingValues[["naValues[lipidTarget]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix() %>%
         is.na()), na.rm =T)

#na values [SIL Int.Stds]
master_list$filters$samples.missingValues[["naValues[SIL.Int.Stds]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix() %>% 
         is.na()), na.rm =T)

#nan values [samples]
master_list$filters$samples.missingValues[["nanValues[lipidTarget]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix() %>%
         is.nan()), na.rm =T)

#nan values [SIL Int.Stds]
master_list$filters$samples.missingValues[["nanValues[SIL.Int.Stds]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix() %>% 
         is.nan()), na.rm =T)

#inf values [samples]
master_list$filters$samples.missingValues[["infValues[lipidTarget]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix() %>%
         is.infinite()), na.rm =T)

#inf values [SIL Int.Stds]
master_list$filters$samples.missingValues[["infValues[SIL.Int.Stds]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix() %>% 
         is.infinite()), na.rm =T)

#total missing values [samples]
master_list$filters$samples.missingValues[["totalMissingValues[lipidTarget]"]] <- master_list$filters$samples.missingValues %>%
  select(-contains("sample_")) %>%
  select(-contains("SIL")) %>%
  as.matrix() %>%
  rowSums()

#total missing values[SIL.Int.stds]
master_list$filters$samples.missingValues[["totalMissingValues[SIL.Int.Stds]"]] <- master_list$filters$samples.missingValues %>%
  select(-contains("sample_")) %>%
  select(contains("SIL")) %>%
  as.matrix() %>%
  rowSums()

#sample removed?
master_list$filters$samples.missingValues$sampleKeep <- 1
#remove where missing data >50% total lipidTargets
master_list$filters$samples.missingValues$sampleKeep[which(
  master_list$filters$samples.missingValues$`totalMissingValues[lipidTarget]` > ((
    bind_rows(master_list$data$peakArea$sorted) %>%
      select(-contains("sample")) %>%
      select(-contains("SIL")) %>%
      ncol())*0.5)
)] <- 0

#remove where missing (<LOD) data >50% total SIL.int.Stds
master_list$filters$samples.missingValues$sampleKeep[which(
  master_list$filters$samples.missingValues$`totalMissingValues[SIL.Int.Stds]` > ((bind_rows(
    master_list$data$peakArea$sorted) %>%
      select(-contains("sample")) %>%
      select(contains("SIL")) %>%
      ncol())*0.5)
)] <- 0

#create failed sample list
master_list$filters$failed_samples <- filter(master_list$filters$samples.missingValues, sampleKeep==0)[["sample_name"]]

## 2.2. sil.IntStd filter [missing value] -----
#create SIL list
master_list$filters$sil.intStd.missingValues <- tibble(
  lipid = master_list$data$peakArea$sorted  %>%
    bind_rows() %>%
    select(contains("SIL")) %>%
    names())

#loop for every batch
for(idx_batch in names(master_list$data$peakArea$sorted)){
  master_list$filters$sil.intStd.missingValues[[paste0(idx_batch,".peakArea<5000[LOD]")]] <- colSums(
    x= (master_list$data$peakArea$sorted[[idx_batch]] %>%
          #only check samples that passed mv filter in previous step
          filter(!sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
          select(contains("SIL")) %>%
          as.matrix()) <5000, na.rm = T)
  
  #na values
  master_list$filters$sil.intStd.missingValues[[paste0(idx_batch,".naValues")]] <- colSums(
    x= (master_list$data$peakArea$sorted[[idx_batch]] %>%
          #only check samples that passed mv filter in previous step
          filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
          select(contains("SIL")) %>%
          as.matrix()) %>%
      is.na(), na.rm = T)
  
  #nan values
  master_list$filters$sil.intStd.missingValues[[paste0(idx_batch,".nanValues")]] <- colSums(
    x= (master_list$data$peakArea$sorted[[idx_batch]] %>%
          #only check samples that passed mv filter in previous step
          filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
          select(contains("SIL")) %>%
          as.matrix()) %>%
      is.nan(), na.rm = T)
  
  #inf values
  master_list$filters$sil.intStd.missingValues[[paste0(idx_batch,".infValues")]] <- colSums(
    x= (master_list$data$peakArea$sorted[[idx_batch]] %>%
          #only check samples that passed mv filter in previous step
          filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
          select(contains("SIL")) %>%
          as.matrix()) %>%
      is.infinite(), na.rm = T)
  
  #total missing values for plate
  master_list$filters$sil.intStd.missingValues[[paste0(idx_batch,".totalMissingValues")]] <-  rowSums(x= (master_list$filters$sil.intStd.missingValues %>%
                                                                                                  select(contains(idx_batch))%>%
                                                                                                  as.matrix()),
                                                                                            na.rm = T)
  
  #Keep SIL intStd if has <80% missing values
  master_list$filters$sil.intStd.missingValues[[paste0(idx_batch, ".keepSIL.intStd[Plate]")]] <- 1
  master_list$filters$sil.intStd.missingValues[[paste0(idx_batch, ".keepSIL.intStd[Plate]")]][which(
  master_list$filters$sil.intStd.missingValues[[paste0(idx_batch,".totalMissingValues")]] > (nrow(
    (master_list$data$peakArea$sorted[[idx_batch]] %>%
      #only check samples that passed mv filter in previous step
      filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]))) * 0.8)
  )] <- 0
}

#for all plates
master_list$filters$sil.intStd.missingValues[[paste0("allPlates.peakArea<5000[LOD]")]] <- colSums(
  x= (master_list$data$peakArea$sorted %>%
        bind_rows() %>%
        #only check samples that passed mv filter in previous step
        filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
        select(contains("SIL")) %>%
        as.matrix()) <5000, na.rm = T)

#na values
master_list$filters$sil.intStd.missingValues[[paste0("allPlates.naValues")]] <- colSums(
  x= (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         #only check samples that passed mv filter in previous step
         filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
         select(contains("SIL")) %>%
         as.matrix()) %>%
    is.na(), na.rm = T)

#nan values
master_list$filters$sil.intStd.missingValues[[paste0("allPlates.nanValues")]] <- colSums(
  x=  (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         #only check samples that passed mv filter in previous step
         filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
         select(contains("SIL")) %>%
        as.matrix()) %>%
    is.nan(), na.rm = T)

#inf values
master_list$filters$sil.intStd.missingValues[[paste0("allPlates.infValues")]] <- colSums(
  x=  (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         #only check samples that passed mv filter in previous step
         filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
         select(contains("SIL")) %>%
        as.matrix()) %>%
    is.infinite(), na.rm = T)

#total missing values for plate
master_list$filters$sil.intStd.missingValues[[paste0("allPlates.totalMissingValues")]] <-  rowSums(x= (master_list$filters$sil.intStd.missingValues %>%
                                                                                                         select(contains("allPlates"))%>%
                                                                                                         as.matrix()),
                                                                                                   na.rm = T)

#does SIL.int.std fail across all plates?
# add fail.filter column if lipid failed for a plate or for all plates
master_list$filters$sil.intStd.missingValues[["PROJECT.keepSIL.intStd"]] <- 1
master_list$filters$sil.intStd.missingValues[["PROJECT.keepSIL.intStd"]][which(
  rowSums(x= (master_list$filters$sil.intStd.missingValues %>%
                select(contains("keepSIL.intStd")) %>%
                as.matrix()) == 0, 
          na.rm = T) > 0
)] <- 0

#create a failed internal standard list
master_list$filters$failed_sil.intStds <- filter(master_list$filters$sil.intStd.missingValues, PROJECT.keepSIL.intStd == 0)[["lipid"]]

## 2.3. lipid filter (missing value) --------
#create SIL list
master_list$filters$lipid.missingValues <- tibble(
  lipid = master_list$data$peakArea$sorted  %>%
    bind_rows() %>%
    select(!contains("sample") & !contains("SIL")) %>%
    names())

#tag lipid if its sil.int.std failed
master_list$filters$lipid.missingValues[["silFilter.keepLipid"]] <- 1 
master_list$filters$lipid.missingValues[["silFilter.keepLipid"]][which(master_list$filters$lipid.missingValues$lipid %in% filter(master_list$templates$SIL_guide, note %in% master_list$filters$failed_sil.intStds)[["precursor_name"]])] <- 0

#loop for every batch
for(idx_batch in names(master_list$data$peakArea$sorted)){
  master_list$filters$lipid.missingValues[[paste0(idx_batch,".peakArea<5000[LOD]")]] <- colSums(
    x= (master_list$data$peakArea$sorted[[idx_batch]] %>%
          #only check samples that passed mv filter in previous step
          filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
          select(!contains("sample") & !contains("SIL")) %>%
          as.matrix()) <5000, na.rm = T)
  
  #na values
  master_list$filters$lipid.missingValues[[paste0(idx_batch,".naValues")]] <- colSums(
    x= (master_list$data$peakArea$sorted[[idx_batch]] %>%
          #only check samples that passed mv filter in previous step
          filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
          select(!contains("sample") & !contains("SIL")) %>%
          as.matrix()) %>%
      is.na(), na.rm = T)
  
  #nan values
  master_list$filters$lipid.missingValues[[paste0(idx_batch,".nanValues")]] <- colSums(
    x= (master_list$data$peakArea$sorted[[idx_batch]] %>%
          #only check samples that passed mv filter in previous step
          filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
          select(!contains("sample") & !contains("SIL")) %>%
          as.matrix()) %>%
      is.nan(), na.rm = T)
  
  #inf values
  master_list$filters$lipid.missingValues[[paste0(idx_batch,".infValues")]] <- colSums(
    x= (master_list$data$peakArea$sorted[[idx_batch]] %>%
          #only check samples that passed mv filter in previous step
          filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
          select(!contains("sample") & !contains("SIL")) %>%
          as.matrix()) %>%
      is.infinite(), na.rm = T)
  
  #total missing values for plate
  master_list$filters$lipid.missingValues[[paste0(idx_batch,".totalMissingValues")]] <-  rowSums(x= (master_list$filters$lipid.missingValues %>%
                                                                                                            select(contains(idx_batch))%>%
                                                                                                            as.matrix()),
                                                                                                      na.rm = T)
  #only keep lipid has <50% missing values
  master_list$filters$lipid.missingValues[[paste0(idx_batch, ".keepLipid[Plate]")]] <- 1
  master_list$filters$lipid.missingValues[[paste0(idx_batch, ".keepLipid[Plate]")]][which(
    master_list$filters$lipid.missingValues[[paste0(idx_batch,".totalMissingValues")]] > (nrow(
      (master_list$data$peakArea$sorted[[idx_batch]] %>%
         #only check samples that passed mv filter in previous step
         filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]))) * 0.5)
  )] <- 0
}

#for all plates
master_list$filters$lipid.missingValues[[paste0("allPlates.peakArea<5000[LOD]")]] <- colSums(
  x= (master_list$data$peakArea$sorted %>%
        bind_rows() %>%
        #only check samples that passed mv filter in previous step
        filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
        select(!contains("sample") & !contains("SIL")) %>%
        as.matrix()) <5000, na.rm = T)

#na values
master_list$filters$lipid.missingValues[[paste0("allPlates.naValues")]] <- colSums(
  x= (master_list$data$peakArea$sorted %>%
        bind_rows() %>%
        #only check samples that passed mv filter in previous step
        filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
        select(!contains("sample") & !contains("SIL")) %>%
        as.matrix()) %>%
    is.na(), na.rm = T)

#nan values
master_list$filters$lipid.missingValues[[paste0("allPlates.nanValues")]] <- colSums(
  x=  (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         #only check samples that passed mv filter in previous step
         filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
         select(!contains("sample") & !contains("SIL")) %>%
         as.matrix()) %>%
    is.nan(), na.rm = T)

#inf values
master_list$filters$lipid.missingValues[[paste0("allPlates.infValues")]] <- colSums(
  x=  (master_list$data$peakArea$sorted %>%
         bind_rows() %>%
         #only check samples that passed mv filter in previous step
         filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]]) %>%
         select(!contains("sample") & !contains("SIL")) %>%
         as.matrix()) %>%
    is.infinite(), na.rm = T)

#total missing values for plate
master_list$filters$lipid.missingValues[[paste0("allPlates.totalMissingValues")]] <-  rowSums(x= (master_list$filters$lipid.missingValues %>%
                                                                                                         select(contains("allPlates"))%>%
                                                                                                         as.matrix()),
                                                                                                   na.rm = T)

#does lipid fail across all plates?
# add fail.filter column if lipid failed for a plate or for all plates
master_list$filters$lipid.missingValues[["PROJECT.keepLipid"]] <- 1
master_list$filters$lipid.missingValues[["PROJECT.keepLipid"]][which(
  rowSums(x= (master_list$filters$lipid.missingValues %>%
                select(contains("keepLipid")) %>%
                as.matrix()) == 0, 
          na.rm = T) > 0
)] <- 0

#create a failed internal standard list
master_list$filters$failed_lipids <- filter(master_list$filters$lipid.missingValues, PROJECT.keepLipid == 0)[["lipid"]]


## 2.4. RSD filter ------------
# following mising value filtering, replicate QC samples are evaluated to see the precision of measurement (represented by % relative standard deviation)
# features with a %RSD >30% are flagged for removal

### 2.4.a. peakArea ---------
master_list$filters$rsd <- NULL

#per batch/plate
for(idx_batch in names(master_list$data$peakArea$imputed)){
  loopData <- master_list$data$peakArea$imputed[[idx_batch]] %>%
    filter(sample_type == "qc") %>%
    select(!contains("sample")) %>%
    select(!contains("SIL"))
  loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100
  #export tibble
  master_list$filters$rsd <- rbind(master_list$filters$rsd,
                  c("peakArea",idx_batch, loopRSD))
}

#per project
loopData <- master_list$data$peakArea$imputed %>%
  bind_rows() %>%
  filter(sample_type == "qc") %>%
  select(!contains("sample")) %>%
  select(!contains("SIL"))
loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100
#export tibble
master_list$filters$rsd <- rbind(master_list$filters$rsd,
                c("peakArea","allBatches", loopRSD))

### 2.4.b. concentration ---------
#per batch/plate
for(idx_batch in names(master_list$data$concentration$imputed)){
  loopData <- master_list$data$concentration$imputed[[idx_batch]] %>%
    filter(sample_type == "qc") %>%
    select(!contains("sample")) %>%
    select(!contains("SIL"))
  loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100
  #export tibble
  master_list$filters$rsd <- rbind(master_list$filters$rsd,
                  c("concentration",idx_batch, loopRSD))
}

#per project
loopData <- master_list$data$concentration$imputed %>%
  bind_rows() %>%
  filter(sample_type == "qc") %>%
  select(!contains("sample")) %>%
  select(!contains("SIL"))
loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100
#export tibble
master_list$filters$rsd <- rbind(master_list$filters$rsd,
                c("concentration","allBatches", loopRSD))

### 2.4.c. statTarget concentration ---------
#per batch/plate
for(idx_batch in names(master_list$data$concentration$statTargetProcessed)){
  loopData <- master_list$data$concentration$statTargetProcessed[[idx_batch]] %>%
    filter(sample_type == "qc") %>%
    select(!contains("sample")) %>%
    select(!contains("SIL"))
  loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100
  #export tibble
  master_list$filters$rsd <- rbind(master_list$filters$rsd,
                  c("concentration[StatTarget]",idx_batch, loopRSD))
}

#per project
loopData <- master_list$data$concentration$statTargetProcessed %>%
  bind_rows() %>%
  filter(sample_type == "qc") %>%
  select(!contains("sample")) %>%
  select(!contains("SIL"))
loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100
#export tibble
master_list$filters$rsd <- rbind(master_list$filters$rsd,
                c("concentration[statTarget]","allBatches", loopRSD))

#tidy rsd table
master_list$filters$rsd <- master_list$filters$rsd %>%
  as_tibble() %>%
  rename(dataSource = V1,
         dataBatch = V2) %>%
  mutate(across(!contains("data"), as.numeric)) %>%
  mutate(across(!contains("data"), round, 2))



#. ------------------------------------------------------------------------------------------------------------------------------------------------
# PHASE 3. SUMMARY REPORT ------------------------ 
## 3.1. dataSetSummary -----
metric =  c("totalSamples", "studySamples", "ltrSamples", "vltrSamples", "sltrSamples", "pqcSamples", 
            "lipidTargets", "SIL.IntStds", 
            "missingValueFilterFlags[samples]", "missingValueFilterFlags[SIL.IS]", "missingValueFilterFlags[lipidTargets]", 
            "rsd<30%[peakArea]", "rsd<20%[peakArea]", "rsd<10%[peakArea]",
            "rsd<30%[concentration]", "rsd<20%[concentration]", "rsd<10%[concentration]",
            "rsd<30%[concentration.statTarget]", "rsd<20%[concentration.statTarget]", "rsd<10%[concentration.statTarget]")

# create tibble
master_list$summary_tables$projectOverview <- tibble(metric = metric)

#fill info for each plate/batch
for(idx_batch in names(master_list$data$peakArea$sorted)){
  
  master_list$summary_tables$projectOverview <- left_join(
    master_list$summary_tables$projectOverview,
    rbind(
      c("totalSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]])),
      c("studySamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "sample"))),
      c("ltrSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "ltr"))),
      c("vltrSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "vltr"))),
      c("sltrSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "sltr"))),
      c("pqcSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "pqc"))),
      c("lipidTargets", ncol(master_list$data$peakArea$sorted[[idx_batch]] %>% select(-contains("sample"), - contains("SIL")))),
      c("SIL.IntStds", ncol(master_list$data$peakArea$sorted[[idx_batch]] %>% select(contains("SIL")))),
      c("missingValueFilterFlags[samples]", master_list$filters$samples.missingValues %>% filter(sample_plate_id == idx_batch & sampleKeep == 0) %>% nrow()),
      c("missingValueFilterFlags[SIL.IS]", which(as.matrix(master_list$filters$sil.intStd.missingValues %>% select(contains(idx_batch)) %>% select(contains("keep")))[,1] == 0) %>% length()),
      c("missingValueFilterFlags[lipidTargets]", which(as.matrix(master_list$filters$lipid.missingValues %>% select(contains(idx_batch)) %>% select(contains("keep")))[,1] == 0) %>% length()),
      c("rsd<30%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "peakArea") %>% select(!contains("data")) <30) %>% length()),
      c("rsd<20%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "peakArea") %>% select(!contains("data")) <20) %>% length()),
      c("rsd<10%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "peakArea") %>% select(!contains("data")) <10) %>% length()),
      c("rsd<30%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration") %>% select(!contains("data")) <30) %>% length()),
      c("rsd<20%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration") %>% select(!contains("data")) <20) %>% length()),
      c("rsd<10%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration") %>% select(!contains("data")) <10) %>% length()),
      c("rsd<30%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration[StatTarget]") %>% select(!contains("data")) <30) %>% length()),
      c("rsd<20%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration[StatTarget]") %>% select(!contains("data")) <20) %>% length()),
      c("rsd<10%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration[StatTarget]") %>% select(!contains("data")) <10) %>% length())
    ) %>%
      as_tibble() %>%
      rename(metric = V1,
             !! paste0(idx_batch) := V2),
    by = "metric"
  )
  #change column to numeric
  master_list$summary_tables$projectOverview[[paste0(idx_batch)]] <- as.numeric(master_list$summary_tables$projectOverview[[paste0(idx_batch)]])
}
  
 ## 3.2. interPlate ----

master_list$summary_tables$projectOverview <- left_join(
  master_list$summary_tables$projectOverview,
  rbind(
    c("totalSamples", nrow(bind_rows(master_list$data$peakArea$sorted))),
    c("studySamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sample"))),
    c("ltrSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "ltr"))),
    c("vltrSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "vltr"))),
    c("sltrSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sltr"))),
    c("pqcSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "pqc"))),
    c("lipidTargets", ncol(bind_rows(master_list$data$peakArea$sorted) %>% select(-contains("sample"), - contains("SIL")))),
    c("SIL.IntStds", ncol(bind_rows(master_list$data$peakArea$sorted) %>% select(contains("SIL")))),
    c("missingValueFilterFlags[samples]", master_list$filters$samples.missingValues %>% filter(sampleKeep == 0) %>% nrow()),
    c("missingValueFilterFlags[SIL.IS]", which(as.matrix(master_list$filters$sil.intStd.missingValues %>% select(contains("PROJECT.keep")))[,1] == 0) %>% length()),
    c("missingValueFilterFlags[lipidTargets]", which(as.matrix(master_list$filters$lipid.missingValues %>% select(contains("PROJECT.keep")))[,1] == 0) %>% length()),
    c("rsd<30%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "peakArea") %>% select(!contains("data")) <30) %>% length()),
    c("rsd<20%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "peakArea") %>% select(!contains("data")) <20) %>% length()),
    c("rsd<10%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "peakArea") %>% select(!contains("data")) <10) %>% length()),
    c("rsd<30%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration") %>% select(!contains("data")) <30) %>% length()),
    c("rsd<20%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration") %>% select(!contains("data")) <20) %>% length()),
    c("rsd<10%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration") %>% select(!contains("data")) <10) %>% length()),
    c("rsd<30%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration[statTarget]") %>% select(!contains("data")) <30) %>% length()),
    c("rsd<20%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration[statTarget]") %>% select(!contains("data")) <20) %>% length()),
    c("rsd<10%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration[statTarget]") %>% select(!contains("data")) <10) %>% length())
  ) %>%
    as_tibble() %>%
    rename(metric = V1,
           !! paste0("allBatches") := V2),
  by = "metric"
)

master_list$summary_tables$projectOverview$allBatches <- master_list$summary_tables$projectOverview$allBatches %>% as.numeric()

# . ------------------------------------------------------------------------------------------------------------------------------  
# PHASE 4: PLOTS ---------------------

## 4.1. Set plot color/fill/shape/size --------------------------------------
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

## 4.2. PCA  -----------
### 4.2.a. create models and extract scores ---------
#setLists
master_list$pca <- list()
master_list$pca$models <- list()
master_list$pca$scores <- list()
for(idx_pca in c("peakArea", "concentration")){
  pca_x <- bind_rows(master_list$data[[idx_pca]]$imputed) %>%
    column_to_rownames("sample_name") %>%
    select(-contains("sample"), -contains("SIL")) %>%
    as.matrix()
  
  #build PCA
  master_list$pca$models[[idx_pca]] <- ropls::opls(
    x = pca_x,
    y=NULL,
    crossvalI = 1,
    predI = 3,
    algoC = "nipals",
    log10L = TRUE,
    scale = "pareto",
    plotSubC = NA, 
    fig.pdfC = "none",
    subset = NULL)
  
  #extract scores data
  master_list$pca$scores[[idx_pca]] <- master_list$pca$models[[idx_pca]]@scoreMN %>% 
    as_tibble() %>%
    rename(PC1 = p1, PC2 = p2, PC3 = p3) %>%
    add_column(.before = 1, sample_name = rownames(pca_x)) %>%
    left_join(by = "sample_name",
              .,
              bind_rows(master_list$data[[idx_pca]]$imputed) %>%
                select(contains("sample"))
    )
}

#statTarget model
pca_x <- bind_rows(master_list$data$concentration$statTargetProcessed) %>%
  column_to_rownames("sample_name") %>%
  select(-contains("sample"), -contains("SIL")) %>%
  as.matrix()

#build PCA
master_list$pca$models[["concentration.statTarget"]] <- ropls::opls(
  x = pca_x,
  y=NULL,
  crossvalI = 1,
  predI = 3,
  algoC = "nipals",
  log10L = TRUE,
  scale = "pareto",
  plotSubC = NA, 
  fig.pdfC = "none",
  subset = NULL)

#extract scores data
master_list$pca$scores[["concentration.statTarget"]] <- master_list$pca$models[["concentration.statTarget"]]@scoreMN %>% 
  as_tibble() %>%
  rename(PC1 = p1, PC2 = p2, PC3 = p3) %>%
  add_column(.before = 1, sample_name = rownames(pca_x)) %>%
  left_join(by = "sample_name",
            .,
            bind_rows(master_list$data$concentration$statTargetProcessed) %>%
              select(contains("sample"))
  )

#statTarget processed and filtered model
pca_x <- bind_rows(master_list$data$concentration$statTargetProcessed) %>%
  filter(!sample_name %in% master_list$filters$failed_samples) %>%
  select(!any_of(master_list$filters$failed_lipids)) %>%
  select(!any_of(
    (master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data")))[which(master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data"))>30)] %>% names()
  )) %>%
  column_to_rownames("sample_name") %>%
  select(-contains("sample"), -contains("SIL")) %>%
  as.matrix()

#build PCA
master_list$pca$models[["concentration.statTarget.filtered"]] <- ropls::opls(
  x = pca_x,
  y=NULL,
  crossvalI = 1,
  predI = 3,
  algoC = "nipals",
  log10L = TRUE,
  scale = "pareto",
  plotSubC = NA, 
  fig.pdfC = "none",
  subset = NULL)

#extract scores data
master_list$pca$scores[["concentration.statTarget.filtered"]] <- master_list$pca$models[["concentration.statTarget.filtered"]]@scoreMN %>% 
  as_tibble() %>%
  rename(PC1 = p1, PC2 = p2, PC3 = p3) %>%
  add_column(.before = 1, sample_name = rownames(pca_x)) %>%
  left_join(by = "sample_name",
            .,
            bind_rows(master_list$data$concentration$statTargetProcessed) %>%
              select(contains("sample"))
  )

master_list$pca$scores[["concentration.statTarget.filtered"]]$sample_data_source <- "concentration.statTargetProcessed.filtered"


### 4.2.b. create plotly scores by class ----------------------------
#make plotly scores
master_list$pca$plot <- list()
for(idx_fill in c("sample_type_factor", "sample_plate_id")){
  #ggplotly
  master_list$pca$plot[[idx_fill]] <- ggplotly(
    ggplot(
      data = bind_rows(master_list$pca$scores),
      aes(x = PC1, y = PC2,
          group  = sample_name,
          fill = get(idx_fill),
          color = sample_type_factor,
          shape = sample_type_factor,
          size = sample_type_factor)) +
      geom_vline(xintercept = 0, colour = "darkgrey") +
      geom_hline(yintercept = 0, color = "darkgrey")+
      geom_point() +
      theme_bw() +
      scale_shape_manual(values = master_list$project_details$plot_shape) +
      (if(idx_fill == "sample_type_factor"){scale_fill_manual(values = master_list$project_details$plot_fill)}) +
      scale_color_manual(values = master_list$project_details$plot_colour) +
      scale_size_manual(values = master_list$project_details$plot_size) +
      guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=paste0(idx_fill))) +
      facet_wrap(facets = "sample_data_source", scales = "free", ncol = 2, nrow = 2)
  ) %>% layout(
    title = list(
      text = paste0("PCA scores; coloured by ", idx_fill, "; ", master_list$project_details$project_name),
      y = 1.1, 
      x=0.05),
    margin = list(
      l = 10, r = 10, b=65, t=85),
    legend = list(
      orientation = "v",   # show entries horizontally
      xanchor = "center",  # use center of legend as anchor
      y = 0.5,
      x= 1.075)           # put legend in center of x-axis
  )}

### runOrder plots ------
#### find plateBoundary ------
#find plate boundaries for vline
plate_boundary = 0.5
annotate_coordinate = NULL
boundary_finder <- bind_rows(master_list$pca$scores) %>%
  select(sample_run_index, sample_plate_id) %>%
  distinct()

for(idx_plate in unique(boundary_finder$sample_plate_id)){
  plate_boundary <- c(
    plate_boundary,
    c(min((boundary_finder %>% filter(sample_plate_id == idx_plate))[["sample_run_index"]]) - 0.5,
      max((boundary_finder %>% filter(sample_plate_id == idx_plate))[["sample_run_index"]]) + 0.5)
  )
  
  annotate_coordinate <- c(
    annotate_coordinate,
    median((boundary_finder %>% filter(sample_plate_id == idx_plate))[["sample_run_index"]])
  )
}
#remove duplicates
plate_boundary <- unique(plate_boundary)
#finalise annotation co-ordinate
annotate_coordinate <- setNames(annotate_coordinate, unique(boundary_finder$sample_plate_id))

#make data.frame
annotate_label <- tibble(
  sample_data_source = rep("x.plateID", length(names(annotate_coordinate))),
  sample_plate_id = names(annotate_coordinate),
  sample_run_index = annotate_coordinate,
  PC1=0,PC2 =0, PC3=0, value =0
)

#### pca score vs run order -----
#plot PCA scores vs run order on x
#make plotly
master_list$pca$scoresRunOrder <- list()
for(idx_pca in c("PC1", "PC2", "PC3")){
  master_list$pca$scoresRunOrder[[idx_pca]] <- ggplotly(
    ggplot(
      data = bind_rows(master_list$pca$scores),
      aes(x = sample_run_index, y = get(idx_pca),
          group = sample_name,
          fill = sample_type_factor,
          color = sample_type_factor,
          shape = sample_type_factor,
          size = sample_type_factor,
      )) +
      geom_vline(xintercept = plate_boundary, linetype = "dashed") +
      geom_point() +
      theme_bw() +
      scale_shape_manual(values = master_list$project_details$plot_shape) +
      scale_fill_manual(values = master_list$project_details$plot_fill) +
      scale_color_manual(values = master_list$project_details$plot_colour) +
      scale_size_manual(values = master_list$project_details$plot_size) +
      ylab(paste0(idx_pca)) +
      guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=paste0(idx_fill))) +
      geom_text(inherit.aes = FALSE, data = annotate_label, aes(x = sample_run_index,  y = get(idx_pca), label = sample_plate_id), col = "black") +
      facet_wrap(facets = "sample_data_source", ncol = 1, scales = "free_y") 
  ) %>% layout(
    title = list(
      text = paste0(idx_pca, "; run order (x) vs PCA scores (y) ; ", master_list$project_details$project_name),
      y = 1.1, 
      x=0.05),
    margin = list(
      l = 10, r = 10, b=65, t=85),
    legend = list(
      orientation = "v",   # show entries horizontally
      xanchor = "center",  # use center of legend as anchor
      x = 1.1,
      y= 0.5
    )           # put legend in center of x-axis
  )
}

#### targetControlCharts ------------------
#setLists
master_list$control_charts <- list()
for(idx_metabolite in filter(master_list$templates$SIL_guide, control_chart == TRUE)[["precursor_name"]]){
  #extract SIL int.std
  idx_sil <- filter(master_list$templates$SIL_guide, control_chart == TRUE)[["note"]][which(filter(master_list$templates$SIL_guide, control_chart == TRUE)[["precursor_name"]] == idx_metabolite)]
  #make plot
  master_list$control_charts[[idx_metabolite]] <- ggplotly(
    ggplot(
      data = bind_rows(
        bind_rows(master_list$data$peakArea$imputed) %>% select(contains("sample"), any_of(idx_metabolite)) %>% filter(!sample_name %in% master_list$filters$failed_samples) %>% 
          mutate(sample_data_source=".peakArea"),
        #add SIL data
        bind_rows(master_list$data$peakArea$imputed) %>% select(contains("sample"), any_of(idx_sil)) %>% filter(!sample_name %in% master_list$filters$failed_samples) %>%
          rename(!! paste0(idx_metabolite) := !!(paste0(idx_sil))) %>%
          mutate(sample_data_source=".SIL.peakArea"),
        bind_rows(master_list$data$concentration$imputed) %>% select(contains("sample"), any_of(idx_metabolite)) %>% filter(!sample_name %in% master_list$filters$failed_samples) %>%
          mutate(sample_data_source="concentration"),
        bind_rows(master_list$data$concentration$statTargetProcessed) %>% select(contains("sample"), any_of(idx_metabolite)) %>% filter(!sample_name %in% master_list$filters$failed_samples) %>%
          mutate(sample_data_source="statTargetConcentration")
      ) %>% rename(!!(paste0("value")) := !! paste0(idx_metabolite)),
      aes(x = sample_run_index, y = value,
          group = sample_name,
          fill = sample_type_factor,
          color = sample_type_factor,
          shape = sample_type_factor,
          size = sample_type_factor)) +
      geom_vline(xintercept = plate_boundary, linetype = "dashed") +
      geom_point() +
      theme_bw() +
      scale_shape_manual(values = master_list$project_details$plot_shape) +
      scale_fill_manual(values = master_list$project_details$plot_fill) +
      scale_color_manual(values = master_list$project_details$plot_colour) +
      scale_size_manual(values = master_list$project_details$plot_size) +
      ylab(paste0(idx_metabolite)) +
      guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=paste0("sample_type"))) +
      geom_text(inherit.aes = FALSE, data = annotate_label, aes(x = sample_run_index,  y = value, label = sample_plate_id), col = "black") +
      facet_wrap(facets = "sample_data_source", ncol = 1, scales = "free_y") 
  ) %>% layout(
    title = list(
      text = paste0(idx_metabolite, "; control chart; ", master_list$project_details$project_name),
      y = 1.1, 
      x=0.05),
    margin = list(
      l = 10, r = 10, b=65, t=85),
    legend = list(
      orientation = "v",   # show entries horizontally
      xanchor = "center",  # use center of legend as anchor
      x = 1.1,
      y= 0.5
    )           # put legend in center of x-axis
  )
  }

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
  c("total.StudyPlates", bind_rows(master_list$data$peakArea$sorted)[["sample_plate_id"]] %>% unique() %>% length()),
  c("total.Samples", bind_rows(master_list$data$peakArea$sorted) %>% nrow()),
  c("studySamples", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sample") %>% nrow()),
  c("ltr", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "ltr") %>% nrow()),
  c("vltr", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "vltr") %>% nrow()),
  c("sltr", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sltr") %>% nrow()),
  c("pqc", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "pqc") %>% nrow()),
  c("total.LipidFeatures", bind_rows(master_list$data$peakArea$sorted) %>% select(-contains("sample")) %>% select(-contains("SIL")) %>% ncol()),
  c("total.SIL.Int.Stds", bind_rows(master_list$data$peakArea$sorted) %>% select(contains("SIL")) %>% ncol()),
  c("", ""),
  c("TAB_DESCRIPTION:", ""),
  c("QC.platePerformance", "overview of project quality performance (per plate)"),
  c("QC.samplesMV", "detailed overview of sample quality (missing values)"),
  c("QC.lipidsMV", "detailed overview of lipid quality (missing values)"),
  c("QC.lipidQcRsd", "detailed overview of lipid quality (% RSD in QC samples)"),
  c("DATA.lipidPeakArea", "lipid target peak area integrals (skylineMS)"),
  c("DATA.silPeakArea", "stable isotope labelled internal standard peak area integrals (skylineMS)"),
  c("DATA.allConcentration", "peakArea >> ratio with SIL IS >> single point concentration factor adjusted"),
  c("DATA.concentration.preProcessed", "peakArea >> ratio with SIL IS >> single point concentration factor adjusted >> imputed[min/2] >> filtered"),
  c("DATA.statTarget.preProcessed", "peakArea >> ratio with SIL IS >> single point concentration factor adjusted >> imputed[min/2] >> filtered >> signalDrift|batch correction using the statTarget r package")
) %>%
  as_tibble() 

### 2. write .xlsx tabbed file ----
openxlsx::write.xlsx(
  file = paste0(master_list$project_details$project_dir, 
                "/xlsx_report/",
                Sys.Date(),
                "_",
                master_list$project_details$project_name,
                "_LGW_lipidData_qcCheckeR_v4.xlsx"),
  overwrite = TRUE,
  x = list(
    "userGuide" = master_list$summary_tables$odsAreaOverview,
    #summary
    "QC.platePerformance" = master_list$summary_tables$projectOverview,
    "QC.sampleMV" = master_list$filters$samples.missingValues,
    "QC.lipidsMV" = master_list$filters$lipid.missingValues %>% relocate(PROJECT.keepLipid, .after = 1),
    "QC.lipidQcRsd" = master_list$filters$rsd %>% mutate(across(!contains("data"), round, 2)) %>%
      add_column(data = paste0(.$dataSource, ".", .$dataBatch), .before = 1) %>%
      select(-dataSource, - dataBatch) %>% t() %>% as.data.frame() %>% rownames_to_column() %>% setNames(.[1,]) %>% filter(data != "data")%>% as_tibble() %>% mutate(across(!contains("data"), as.numeric)),
    #data
    "DATA.peakArea" = bind_rows(master_list$data$peakArea$sorted) %>% select(-contains("SIL")),
    "DATA.silPeakArea" = bind_rows(master_list$data$peakArea$sorted) %>% select(contains("sample") | contains("SIL")),
    "DATA.all.concentration" = bind_rows(master_list$data$concentration$sorted),
    "DATA.concentration.preProcessed" = bind_rows(master_list$data$concentration$imputed) %>% 
      filter(!sample_name %in% master_list$filters$failed_samples) %>%
      select(!any_of(master_list$filters$failed_lipids)) %>%
      select(!any_of(
        (master_list$filters$rsd %>% filter(dataSource == "concentration" & dataBatch == "allBatches") %>% select(!contains("data")))[which(master_list$filters$rsd %>% filter(dataSource == "concentration" & dataBatch == "allBatches") %>% select(!contains("data"))>30)] %>% names()
      )),
    "DATA.statTarget.preProcessed" = bind_rows(master_list$data$concentration$statTargetProcessed) %>%
      filter(!sample_name %in% master_list$filters$failed_samples) %>%
      select(!any_of(master_list$filters$failed_lipids)) %>%
      select(!any_of(
        (master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data")))[which(master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data"))>30)] %>% names()
      ))
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
rmarkdown::render(input = paste0(master_list$project_details$project_dir, "/html_report/lipid_exploreR_report_templatev4.R"),
                  output_format = "html_document",
                  output_dir = paste0(master_list$project_details$project_dir, "/html_report"),
                  output_file = paste0(Sys.Date(), "_", master_list$project_details$project_name, "_LGW_lipidExploreR_qcCheckeR_report_v4.html")
)

### 3. browse template ----
browseURL(url = paste0(master_list$project_details$project_dir, 
                       "/html_report/",
                       Sys.Date(), "_", master_list$project_details$project_name, "_LGW_lipidExploreR_qcCheckeR_report_v4.html")
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
# ARCHIVE BELOW THIS LINE ----------
master_list$process_lists$mvLipids <- tibble(
  lipid = master_list$data$pp_mvFilter_s  %>%
    bind_rows() %>%
    select(-contains("sample")) %>%
    names()
)

for(idx_batch in names(master_list$data$pp_mvFilter_s)){
  master_list$process_lists$mvLipids[[paste0(idx_batch,".peakArea<5000[LOD]")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_batch]] %>%
          select(-contains("sample")) %>%
          as.matrix()) <5000, na.rm = T)
  
  #na values
  master_list$process_lists$mvLipids[[paste0(idx_batch,".naValues")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_batch]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.na(), na.rm = T)
  
  #nan values
  master_list$process_lists$mvLipids[[paste0(idx_batch,".nanValues")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_batch]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.nan(), na.rm = T)
  
  #inf values
  master_list$process_lists$mvLipids[[paste0(idx_batch,".infValues")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_batch]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.infinite(), na.rm = T)
  
  #total missing values for plate
  master_list$process_lists$mvLipids[[paste0(idx_batch,".totalMissingValues")]] <-  rowSums(x= (master_list$process_lists$mvLipids %>%
                                                                                                  select(contains(idx_batch))%>%
                                                                                                  as.matrix()),
                                                                                            na.rm = T)
  
  #does lipid fail for the plate?
  master_list$process_lists$mvLipids[[paste0(idx_batch, ".keepLipid[Plate]")]] <- 1
  master_list$process_lists$mvLipids[[paste0(idx_batch, ".keepLipid[Plate]")]][which(
    master_list$process_lists$mvLipids[[paste0(idx_batch,".totalMissingValues")]] > (nrow(master_list$data$pp_mvFilter_s[[idx_batch]]) * 0.5)
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
# add fail.filter column if lipid failed for a plate or for all plates
master_list$process_lists$mvLipids[["PROJECT.keepLipid"]] <- 1
master_list$process_lists$mvLipids[["PROJECT.keepLipid"]][which(
  rowSums(x= (master_list$process_lists$mvLipids %>%
                select(contains(".keepLipid[Plate]")) %>%
                as.matrix()) == 0, 
          na.rm = T) > 0
)] <- 0







































#. --------------


## 3. Calculate response ratio and concentration calculations  ------------------------------------------------------
master_list$data$area_response <- list(); master_list$data$area_concentration <- list()

#run loop per plate|batch data

        
        #calculate concentration
        #Find the concentration factor of SIL
        sil_conc_factor <- master_list$templates$conc_guide$concentration_factor[which(master_list$templates$conc_guide$sil_name == idx_SIL)]
        if(length(sil_conc_factor) == 1){
          #select response data
          target_lipids_conc <- select(master_list$data$area_response[[idx_batch]], any_of(master_list$templates$SIL_guide$precursor_name[which(master_list$templates$SIL_guide$note == idx_SIL)])) 
          #apply concentration factor to lipd values
          master_list$data$area_concentration[[idx_batch]] <- bind_cols(
            master_list$data$area_concentration[[idx_batch]],
            as_tibble(target_lipids_conc*sil_conc_factor)
          )
        } #close if(length(sil_conc_factor) == 1)
      } # close if(ncol(target_lipids)>0)
    } #close if(length(FUNC_SIL_guide$precursor_name[which(FUNC_SIL_guide$note == idx_SIL)])>0){
  }
  
  master_list$data$area_response[[idx_batch]]$sample_data_source <- "response"; 
  master_list$data$area_concentration[[idx_batch]]$sample_data_source <- "concentration"
  
}



rm(list = c(ls()[which(ls() != "master_list")]))

#.------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



## no signalDrift or batch correction -----------------



#### b. apply and create filtered dataset -----
#first set data list
master_list$data$pp_mvFilter_s <- list()
#run loop to filter data
for(idx_batch in names(master_list$data$peakArea$sorted)){
  master_list$data$pp_mvFilter_s[[idx_batch]] <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    filter(sample_name %in% master_list$filters$samples.missingValues$sample_name[which(
      master_list$filters$samples.missingValues$sampleKeep == 1)
    ])
}

### 2. missing value lipidFilter ----
####a. find and count missing values for lipids -----
master_list$process_lists$mvLipids <- tibble(
  lipid = master_list$data$pp_mvFilter_s  %>%
    bind_rows() %>%
    select(-contains("sample")) %>%
    names()
)

for(idx_batch in names(master_list$data$pp_mvFilter_s)){
  master_list$process_lists$mvLipids[[paste0(idx_batch,".peakArea<5000[LOD]")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_batch]] %>%
          select(-contains("sample")) %>%
          as.matrix()) <5000, na.rm = T)
  
  #na values
  master_list$process_lists$mvLipids[[paste0(idx_batch,".naValues")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_batch]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.na(), na.rm = T)
  
  #nan values
  master_list$process_lists$mvLipids[[paste0(idx_batch,".nanValues")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_batch]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.nan(), na.rm = T)
  
  #inf values
  master_list$process_lists$mvLipids[[paste0(idx_batch,".infValues")]] <- colSums(
    x= (master_list$data$pp_mvFilter_s[[idx_batch]] %>%
          select(-contains("sample")) %>%
          as.matrix()) %>%
      is.infinite(), na.rm = T)
  
  #total missing values for plate
  master_list$process_lists$mvLipids[[paste0(idx_batch,".totalMissingValues")]] <-  rowSums(x= (master_list$process_lists$mvLipids %>%
                                                                                                 select(contains(idx_batch))%>%
                                                                                                 as.matrix()),
                                                                                           na.rm = T)
  
  #does lipid fail for the plate?
  master_list$process_lists$mvLipids[[paste0(idx_batch, ".keepLipid[Plate]")]] <- 1
  master_list$process_lists$mvLipids[[paste0(idx_batch, ".keepLipid[Plate]")]][which(
    master_list$process_lists$mvLipids[[paste0(idx_batch,".totalMissingValues")]] > (nrow(master_list$data$pp_mvFilter_s[[idx_batch]]) * 0.5)
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
# add fail.filter column if lipid failed for a plate or for all plates
master_list$process_lists$mvLipids[["PROJECT.keepLipid"]] <- 1
master_list$process_lists$mvLipids[["PROJECT.keepLipid"]][which(
  rowSums(x= (master_list$process_lists$mvLipids %>%
                select(contains(".keepLipid[Plate]")) %>%
                as.matrix()) == 0, 
          na.rm = T) > 0
)] <- 0

#### b. apply filter and create filtered dataset -----
#create empty list
master_list$data$pp_mvFilter_s_l <- list()
for(idx_batch in names(master_list$data$pp_mvFilter_s)){
  master_list$data$pp_mvFilter_s_l[[idx_batch]] <- master_list$data$pp_mvFilter_s[[idx_batch]] %>%
    select(-any_of(
      (master_list$process_lists$mvLipids %>% filter(get(paste0(idx_batch, ".keepLipid[Plate]")) == 0))[["lipid"]]
    ))
  
  master_list$data$pp_mvFilter_s_l[[idx_batch]]$sample_data_source <- "pp_mvFiltered"
}

### 3. use missing value filters to filter imputed dataset 
master_list$data$pp_impute$ <- list()
for(idx_batch in names(master_list$data$peakArea$imputed)){
  master_list$data$pp_impute[[idx_batch]] <- master_list$data$peakArea$imputed[[idx_batch]] %>%
    filter(sample_name %in% master_list$data$pp_mvFilter_s_l[[idx_batch]][["sample_name"]]) %>%
    select(any_of(names(master_list$data$pp_mvFilter_s_l[[idx_batch]])))
  
  master_list$data$pp_impute[[idx_batch]]$sample_data_source <- "pp_impute"
}

### 3. calculate response ratio and concentration calculations using pp data (post-impute and filter)  ------------------------------------------------------

#* Calculation of target metabolite/stable isotope labelled (SIL) internal standard ratio, using predefined target metabolite/internal standard pairs
#* Conversion of response ratio to concentration values using single point calibration
#* Performed on imputed and filtered data
  master_list$data$pp_response <- list(); master_list$data$pp_concentration <- list()
  
  #run loop per plate|batch data
  for(idx_batch in names(master_list$data$pp_impute)){
    #set empty list to store output data
    master_list$data$pp_response[[idx_batch]] <- master_list$data$pp_impute[[idx_batch]] %>% select(contains("sample"))
    master_list$data$pp_concentration[[idx_batch]] <- master_list$data$pp_impute[[idx_batch]] %>% select(contains("sample"))
  
    #run loop for each SIL IS.
    for(idx_SIL in master_list$data$pp_impute[[idx_batch]] %>% 
        select(contains("SIL")) %>% 
        names()){
      if(length(master_list$templates$SIL_guide$precursor_name[which(master_list$templates$SIL_guide$note == idx_SIL)])>0){
        #find which SIL is used from the template
        target_lipids <- select(
          master_list$data$pp_impute[[idx_batch]], 
          any_of(master_list$templates$SIL_guide$precursor_name[which(master_list$templates$SIL_guide$note == idx_SIL)])) 
        if(ncol(target_lipids)>0){
          #calculate response ratio
          master_list$data$pp_response[[idx_batch]] <- bind_cols(
            master_list$data$pp_response[[idx_batch]], 
            as_tibble(target_lipids/master_list$data$pp_impute[[idx_batch]][[idx_SIL]])
          )
          
          #calculate concentration
          #Find the concentration factor of SIL
          sil_conc_factor <- master_list$templates$conc_guide$concentration_factor[which(master_list$templates$conc_guide$sil_name == idx_SIL)]
          if(length(sil_conc_factor) == 1){
            #select response data
            target_lipids_conc <- select(
              master_list$data$pp_response[[idx_batch]], 
              any_of(master_list$templates$SIL_guide$precursor_name[which(master_list$templates$SIL_guide$note == idx_SIL)])) 
            #apply concentration factor to lipd values
            master_list$data$pp_concentration[[idx_batch]] <- bind_cols(
              master_list$data$pp_concentration[[idx_batch]],
              as_tibble(target_lipids_conc*sil_conc_factor)
            )
          } #close if(length(sil_conc_factor) == 1)
        } # close if(ncol(target_lipids)>0)
      } #close if(length(FUNC_SIL_guide$precursor_name[which(FUNC_SIL_guide$note == idx_SIL)])>0){
    }
    
    
    master_list$data$pp_response[[idx_batch]]$sample_data_source <- "response.[filtered]"
    master_list$data$pp_concentration[[idx_batch]]$sample_data_source <- "concentration.[filtered]"
    
  }



#### d. filter statTarget data ----------------
##### i. for excluded samples ---------
master_list$data$peakArea$statTargetProcessed_mvFilter_s <- list()
#loop to back into plate elements of list
for(idx_batch in names(master_list$data$peakArea$statTargetProcessed)){
  master_list$data$peakArea$statTargetProcessed_mvFilter_s[[idx_batch]] <- master_list$data$peakArea$statTargetProcessed[[idx_batch]] %>%
    filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]])

  master_list$data$peakArea$statTargetProcessed_mvFilter_s[[idx_batch]]$sample_data_source <- "statTarget_mvFilter_s"
}

##### ii. filter on excluded lipids -------
master_list$data$peakArea$statTargetProcessed_mvFilter_s_l <- list()
#loop to back into plate elements of list
for(idx_batch in names(master_list$data$peakArea$statTargetProcessed_mvFilter_s)){
  master_list$data$peakArea$statTargetProcessed_mvFilter_s_l[[idx_batch]] <- master_list$data$peakArea$statTargetProcessed_mvFilter_s[[idx_batch]] %>%
    select(
      contains("sample"),
      any_of(filter(master_list$process_lists$mvLipids, get(paste0(idx_batch, ".keepLipid[Plate]")) == 1)[["lipid"]])
    )
  
  master_list$data$peakArea$statTargetProcessed_mvFilter_s_l[[idx_batch]]$sample_data_source <- "statTarget_mvFilter_s_l"
}

rm(list = c(ls()[which(ls() != "master_list")]))

### 2. calculate response and concentration [pp_filtered_statTarget data] ----------
#set empty list to store output data
master_list$data$peakArea$statTargetProcessed_response <- list(); master_list$data$peakArea$statTargetProcessed_concentration <- list()

#run loop per plate|batch data
for(idx_batch in names(master_list$data$peakArea$statTargetProcessed_mvFilter_s_l)){
  #set empty list to store output data
  master_list$data$peakArea$statTargetProcessed_response[[idx_batch]] <- master_list$data$peakArea$statTargetProcessed_mvFilter_s_l[[idx_batch]] %>% 
    select(contains("sample"))
  master_list$data$peakArea$statTargetProcessed_concentration[[idx_batch]] <- master_list$data$peakArea$statTargetProcessed_mvFilter_s_l[[idx_batch]] %>% 
    select(contains("sample"))
  #run loop for each SIL IS.
  for(idx_SIL in master_list$data$peakArea$statTargetProcessed_mvFilter_s_l[[idx_batch]] %>% 
      select(contains("SIL")) %>% 
      names()){
    if(length(master_list$templates$SIL_guide$precursor_name[which(master_list$templates$SIL_guide$note == idx_SIL)])>0){
      #find which SIL is used from the template
      target_lipids <- select(
        master_list$data$peakArea$statTargetProcessed_mvFilter_s_l[[idx_batch]], 
        any_of(master_list$templates$SIL_guide$precursor_name[which(master_list$templates$SIL_guide$note == idx_SIL)])) 
      if(ncol(target_lipids)>0){
        #calculate response ratio
        master_list$data$peakArea$statTargetProcessed_response[[idx_batch]] <- bind_cols(
          master_list$data$peakArea$statTargetProcessed_response[[idx_batch]], 
          as_tibble(target_lipids/master_list$data$peakArea$statTargetProcessed_mvFilter_s_l[[idx_batch]][[idx_SIL]])
        )
        
        #calculate concentration
        #Find the concentration factor of SIL
        sil_conc_factor <- master_list$templates$conc_guide$concentration_factor[which(master_list$templates$conc_guide$sil_name == idx_SIL)]
        if(length(sil_conc_factor) == 1){
          #select response data
          target_lipids_conc <- select(
            master_list$data$peakArea$statTargetProcessed_response[[idx_batch]], 
            any_of(master_list$templates$SIL_guide$precursor_name[which(master_list$templates$SIL_guide$note == idx_SIL)])) 
          #apply concentration factor to lipid values
          master_list$data$peakArea$statTargetProcessed_concentration[[idx_batch]] <- bind_cols(
            master_list$data$peakArea$statTargetProcessed_concentration[[idx_batch]],
            as_tibble(target_lipids_conc*sil_conc_factor)
          )
        } #close if(length(sil_conc_factor) == 1)
      } # close if(ncol(target_lipids)>0)
    } #close if(length(FUNC_SIL_guide$precursor_name[which(FUNC_SIL_guide$note == idx_SIL)])>0){
  }
  
  master_list$data$peakArea$statTargetProcessed_response[[idx_batch]]$sample_data_source <- "statTargetResponse.[filtered]"; master_list$data$peakArea$statTargetProcessed_concentration[[idx_batch]]$sample_data_source <- "statTargetConcentration.[filterd]"
}

#make statTargetconcentration mv filtered
#### d. filter statTarget concentration data ----------------
##### i. for excluded samples ---------
master_list$data$peakArea$statTargetProcessed_concentration_mvFilter_s <- list()
#loop to back into plate elements of list
for(idx_batch in names(master_list$data$peakArea$statTargetProcessed_concentration)){
  master_list$data$peakArea$statTargetProcessed_concentration_mvFilter_s[[idx_batch]] <- master_list$data$peakArea$statTargetProcessed_concentration[[idx_batch]] %>%
    filter(sample_name %in% filter(master_list$filters$samples.missingValues, sampleKeep==1)[["sample_name"]])
  
  master_list$data$peakArea$statTargetProcessed_concentration_mvFilter_s[[idx_batch]]$sample_data_source <- "statTarget_concentration_mvFilter_s"
}

##### ii. filter on excluded lipids -------
master_list$data$peakArea$statTargetProcessed_concentration_mvFilter_s_l <- list()
#loop to back into plate elements of list
for(idx_batch in names(master_list$data$peakArea$statTargetProcessed_concentration_mvFilter_s)){
  master_list$data$peakArea$statTargetProcessed_concentration_mvFilter_s_l[[idx_batch]] <- master_list$data$peakArea$statTargetProcessed_concentration_mvFilter_s[[idx_batch]] %>%
    select(
      contains("sample"),
      any_of(filter(master_list$process_lists$mvLipids, get(paste0(idx_batch, ".keepLipid[Plate]")) == 1)[["lipid"]])
    )
  
  master_list$data$peakArea$statTargetProcessed_concentration_mvFilter_s_l[[idx_batch]]$sample_data_source <- "statTarget_concentration_mvFilter_s_l"
}

rm(list = c(ls()[which(ls() != "master_list")]))


## apply QC %RSD filters to concentration data ---------------------------

### 1. Data filter: QC %RSD filter across user selected LTRs -------------------------
#### a. intraPlate ------
#first create a table of rsd performance
master_list$process_lists$rsd_filter <- tibble(
  lipid =  names(master_list$data$peakArea$sorted[[1]] %>%
                   filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
                   select(-contains("sample"))))

#run loop for each dataType: rawArea; concentration; preProcessedConcentration; statTargetConcentration
for(idx_batch in names(master_list$data$peakArea$sorted)){
  
  peakArea_qc_data <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  
  concentration_qc_data <- master_list$data$area_concentration[[idx_batch]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  # 
  preProcessedConcentration_qc_data <- master_list$data$pp_concentration[[idx_batch]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  
  statTargetConcentration_qc_data <- master_list$data$peakArea$statTargetProcessed_concentration[[idx_batch]] %>%
    filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
    select(-contains("sample"))
  
  #extract RSD performance in peakAreaData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(peakArea_qc_data),
      !! paste0("peakArea.", idx_batch) := (colSds(as.matrix(peakArea_qc_data))*100)/colMeans(as.matrix(peakArea_qc_data))
    ), 
    by = "lipid")
  
  #extract RSD performance in concentrationData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(concentration_qc_data),
      !! paste0("concentration.", idx_batch) := (colSds(as.matrix(concentration_qc_data))*100)/colMeans(as.matrix(concentration_qc_data))
    ), 
    by = "lipid")
  
  #extract RSD performance in preProcessedConcentrationData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(preProcessedConcentration_qc_data),
      !! paste0("postFilterConcentration.", idx_batch) := (colSds(as.matrix(preProcessedConcentration_qc_data))*100)/colMeans(as.matrix(preProcessedConcentration_qc_data))
    ),
    by = "lipid")
  
  #extract RSD performance in statTargetConcentrationData
  master_list$process_lists$rsd_filter <- left_join(
    master_list$process_lists$rsd_filter,
    tibble(
      lipid =  names(statTargetConcentration_qc_data),
      !! paste0("postFilterStatTarget.", idx_batch) := (colSds(as.matrix(statTargetConcentration_qc_data))*100)/colMeans(as.matrix(statTargetConcentration_qc_data))
    ), 
    by = "lipid")
  }

#### b. interPlate -----------------------------
# make rsd values for all plates
peakArea_qc_data <- bind_rows(master_list$data$peakArea$sorted) %>%
  filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
  select(-contains("sample"))

concentration_qc_data <- bind_rows(master_list$data$area_concentration) %>%
  filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
  select(-contains("sample"))

preProcessedConcentration_qc_data <- bind_rows(master_list$data$pp_concentration) %>%
  filter(sample_type_factor == tolower(master_list$project_details$qc_type)) %>%
  select(-contains("sample"))

#statTargetData
statTargetConcentration_qc_data <- bind_rows(master_list$data$peakArea$statTargetProcessed_concentration) %>%
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

### 2. Make QC %RSD filtered datasets perPlate ----
#### preProcessedConcentration [perPlate] ----
master_list$data$pp_rsdPlates <- list()
for(idx_batch in names(master_list$data$pp_concentration)){
master_list$data$pp_rsdPlates[[idx_batch]] <- master_list$data$pp_concentration[[idx_batch]] %>%
  select(
    contains("sample"),
    any_of(names(which(master_list$process_lists$rsd_filter[[paste0("concentration.", idx_batch)]] < 30)))
  )

master_list$data$pp_rsdPlates[[idx_batch]]$sample_data_source <- "postFilterConcentration"
}
#### statTargetConcentration ----
master_list$data$peakArea$statTargetProcessed_rsdPlates <- list()
for(idx_batch in names(master_list$data$peakArea$statTargetProcessed_concentration)){
  master_list$data$peakArea$statTargetProcessed_rsdPlates[[idx_batch]] <- master_list$data$peakArea$statTargetProcessed_concentration[[idx_batch]] %>%
    select(
      contains("sample"),
      any_of(names(which(master_list$process_lists$rsd_filter[[paste0("postFilterStatTarget.", idx_batch)]] < 30)))
    )
  
  master_list$data$peakArea$statTargetProcessed_rsdPlates[[idx_batch]]$sample_data_source <- "postFilterStatTarget"
}
#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))


### RSD filtered across total dataSet
master_list$data$pp_final <- list()
for(idx_batch in names(master_list$data$pp_concentration)){
  master_list$data$pp_final[[idx_batch]] <- master_list$data$pp_concentration[[idx_batch]] %>%
    select(
      contains("sample"),
      any_of(names(which(master_list$process_lists$rsd_filter$postFilterConcentration.allPlates < 30)))
    )
  
  master_list$data$pp_final[[idx_batch]]$sample_data_source <- "concentration.[postFilter]"
}

### RSD filtered across total dataSet
master_list$data$peakArea$statTargetProcessed_final <- list()
for(idx_batch in names(master_list$data$peakArea$statTargetProcessed_concentration)){
  master_list$data$peakArea$statTargetProcessed_final[[idx_batch]] <- master_list$data$peakArea$statTargetProcessed_concentration[[idx_batch]] %>%
    select(
      contains("sample"),
      any_of(names(which(master_list$process_lists$rsd_filter$postFilterStatTarget.allPlates < 30)))
    )
  
  master_list$data$peakArea$statTargetProcessed_final[[idx_batch]]$sample_data_source <- "statTargetConcentration.[postFilter]"
}

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
tempMatrix <- bind_rows(master_list$data$pp_final) %>%
  select("sample_name", any_of(names(master_list$data$pp_final[[1]] %>% select(-contains("sample"))))) %>%
  column_to_rownames("sample_name") 
#replace values < 1 with values with 2 sig figs.
tempMatrix[tempMatrix <1] <- signif(tempMatrix[tempMatrix <1], 2)
tempMatrix[tempMatrix >1] <- round(tempMatrix[tempMatrix >1], 2)

#rebind with sample_name
master_list$data$pp_final <- list(
  bind_rows(master_list$data$pp_final[[1]]) %>%
    select(contains("sample")) %>%
    left_join(.,
              tempMatrix %>% 
                rownames_to_column("sample_name"),
              by = "sample_name"))

  
#and for statTarget data
tempMatrix <- bind_rows(master_list$data$peakArea$statTargetProcessed_final) %>%
  select("sample_name", any_of(names(master_list$data$peakArea$statTargetProcessed_final[[1]] %>% select(-contains("sample"))))) %>%
  column_to_rownames("sample_name") 
#replace values < 1 with values with 2 sig figs.
tempMatrix[tempMatrix <1] <- signif(tempMatrix[tempMatrix <1], 2)
tempMatrix[tempMatrix >1] <- round(tempMatrix[tempMatrix >1], 2)

#rebind with sample_name
master_list$data$peakArea$statTargetProcessed_final <- list(
  bind_rows(master_list$data$peakArea$statTargetProcessed_final[[1]]) %>%
    select(contains("sample")) %>%
    left_join(.,
              tempMatrix %>% 
                rownames_to_column("sample_name"),
              by = "sample_name"))

