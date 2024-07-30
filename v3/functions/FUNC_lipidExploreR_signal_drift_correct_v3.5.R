#ANPC signal batch correction

## REQUIRED PACKAGES

# -> metabom8 (github.com/tkimhofer)
# -> tidyverse
# -> RColorBrewer
# -> plotly
# -> statTarget


## REQUIRED ARGUMENTS

# -> FUNC_project_directory: directory to create output files
# -> FUNC_data: data to be batch corrected
# -> FUNC_metabolite_list: list of metabolite targets
# -> FUNC_header_sample_id: string header of column name containing sample_id info
# -> FUNC_header_batch: string header of column name containing batch ID
# -> FUNC_header_sample_type: string header of column name containing sample_type (sample or qc)
# -> FUNC_header_run_order: string header of column name containing sample run order idx
# -> FUNC_option_method: string; RF or loess
# -> FUNC_option_coCV: %RSD in qc cut off. default is 30

lgw_signal_correction <- function(FUNC_project_directory,
                                  FUNC_data,
                                  FUNC_metabolite_list,
                                  FUNC_header_sample_id,
                                  FUNC_header_batch,
                                  FUNC_header_sample_type,
                                  FUNC_header_run_order,
                                  FUNC_option_qc_type
){

  #create directories for use later

  if(!dir.exists(paste0(FUNC_project_directory, "/", Sys.Date(), "_signal_correction_results"))){
    dir.create(paste(FUNC_project_directory, "/", Sys.Date(), "_signal_correction_results", sep=""))
  }
    setwd(paste(FUNC_project_directory, "/", Sys.Date(), "_signal_correction_results", sep="")) 
  
  
  #create data list 
  FUNC_list <- list()
  #set master data for function
  FUNC_list$master_data <- FUNC_data %>% as_tibble()
  #standardise zero values, NA, NaN, inf to NAs
  FUNC_list$standard_data <- FUNC_list$master_data
  FUNC_list$standard_data[FUNC_list$standard_data ==0] <- 0
  FUNC_list$standard_data[is.nan(as.matrix(FUNC_list$standard_data))] <- 0
  FUNC_list$standard_data[is.na(as.matrix(FUNC_list$standard_data))] <- 0
  FUNC_list$standard_data[is.infinite(as.matrix(FUNC_list$standard_data))] <- 0
  
  #set_qc type used for signal drift correction
  FUNC_list$standard_data[["sample_type"]] <- "sample"
  FUNC_list$standard_data[["sample_type"]][which(FUNC_list$standard_data[["sample_type_factor"]] == FUNC_option_qc_type)] <- "qc"
  
  
  # SECTION 1 ---------------------------------------------------------------
  #create the required metadata file (PhenoFile) for statTarget::shiftCor
  
  FUNC_list$PhenoFile <- list()
  
  # build PhenoFile file template
  FUNC_list$PhenoFile$template <- FUNC_list$standard_data %>% 
    select(all_of(FUNC_header_sample_id)) %>%
    rename(sample = all_of(FUNC_header_sample_id)) %>%
    add_column(FUNC_list$standard_data %>% 
                 select(all_of(FUNC_header_sample_id))) %>%
    add_column(FUNC_list$standard_data %>%
                 select(all_of(FUNC_header_batch))) %>%
    add_column(FUNC_list$standard_data %>%
                 select(all_of(FUNC_header_sample_type))) %>%
    rename(class = all_of(FUNC_header_sample_type)) %>% 
    add_column(FUNC_list$standard_data %>%
                 select(all_of(FUNC_header_sample_type))) %>%
    add_column(FUNC_list$standard_data %>%
                 select(all_of(FUNC_header_run_order)))
  
  #### arrange by run order
  FUNC_list$PhenoFile$template <- FUNC_list$PhenoFile$template %>%
    arrange_at(FUNC_header_run_order)
  
  #### QC placement
  #stat target needs a qc at postion 1 and last position in the run. Default run order takes this into account, but some datasets this is not the case
  #in this instance this section of code artificially moves the first and last qc into position. It is completed for each batch.
  
  FUNC_list$PhenoFile$template_qc_order <- NULL
  qc_idx <- NULL
  for(idx_batch in FUNC_list$PhenoFile$template %>% 
      select(all_of(FUNC_header_batch)) %>%
      unique() %>%
      as.matrix() %>%
      c()
  ){
    
    #create a temp tibble batch specific
    loop_temp_data <- FUNC_list$PhenoFile$template %>%
      filter(!!as.symbol(FUNC_header_batch) == idx_batch)
    
    #ensure a sample_type "qc" is "first" and "last" in the worklist order. Required for statTarget::shiftCor
    loop_qc_idx <- which(loop_temp_data %>% 
                           select(all_of(FUNC_header_sample_type)) 
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
                                                       select(all_of(FUNC_header_sample_type)) == "qc")] <- paste0("QC", rep(1:length(qc_idx)))
  FUNC_list$PhenoFile$template_qc_order$sample[which(FUNC_list$PhenoFile$template_qc_order %>% 
                                                       select(all_of(FUNC_header_sample_type)) == "sample")] <- paste0("sample", 
                                                                                                                       rep(1:(nrow(FUNC_list$PhenoFile$template_qc_order)-length(qc_idx))))
  #set NA for class column in rows that are NA
  FUNC_list$PhenoFile$template_qc_order$class[which(FUNC_list$PhenoFile$template_qc_order %>% 
                                                      select(all_of(FUNC_header_sample_type)) == "qc")] <- NA
  
  #rename column header for statTarget template
  FUNC_list$PhenoFile$template_sample_id <- FUNC_list$PhenoFile$template_qc_order %>% 
    rename(sample_id = all_of(FUNC_header_sample_id),
           batch = all_of(FUNC_header_batch),
           order = all_of(FUNC_header_run_order)) %>%
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
            file = paste(FUNC_project_directory, "/", Sys.Date(), "_signal_correction_results", "/PhenoFile.csv", sep="")
  )
  
  
  # SECTION 2 - create data for statTarget::shiftCor  -----------------------------------
  
  FUNC_list$ProfileFile <- list()
  
  #must have samples in columns and metabolites in rows
  FUNC_list$ProfileFile$template  <- FUNC_list$standard_data %>%
    select(all_of(FUNC_header_sample_id),
           all_of(FUNC_metabolite_list)) %>%
    rename(sample_id = !!FUNC_header_sample_id)
  
  #match run order to PhenoFile
  FUNC_list$ProfileFile$template_qc_order <- FUNC_list$PhenoFile$template_sample_id %>%
    select(sample, sample_id) %>%
    left_join(FUNC_list$ProfileFile$template, by = "sample_id") %>%
    select(-sample_id)
  
  #transpose tibble for statTarget
  FUNC_list$ProfileFile$ProfileFile <- as_tibble(cbind(nms = names(FUNC_list$ProfileFile$template_qc_order), t(FUNC_list$ProfileFile$template_qc_order))) %>%
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
            file = paste(FUNC_project_directory, "/", Sys.Date(), "_signal_correction_results", "/ProfileFile.csv", sep="")
  )
  
  
  #script files
  samPeno <- paste(FUNC_project_directory, "/", Sys.Date(), "_signal_correction_results", "/PhenoFile.csv", sep="")
  samFile <- paste(FUNC_project_directory, "/", Sys.Date(), "_signal_correction_results",  "/ProfileFile.csv", sep="")
  
  #browser()
    capture.output(
      shiftCor(samPeno = samPeno,
               samFile =  samFile,
               Frule = 0,
               ntree = 500,
               MLmethod = 'QCRFSC',
               imputeM = "minHalf",
               plot = FALSE,
               coCV = 10000
      )
    )
  
  #browser()
    #re-import data produced by statTarget and return it to format for LGW lipid pipeline
  FUNC_list$corrected_data$data <- read_csv(paste(FUNC_project_directory, "/", Sys.Date(), "_signal_correction_results", "/statTarget/shiftCor/After_shiftCor/shift_all_cor.csv", sep=""),
                                            show_col_types = FALSE) %>%
    filter(sample != "class") %>%
    rename(name = sample) %>%
    mutate(across(!contains("name", ignore.case = FALSE), as.numeric))
    
  
  # #create new list of metabolite codes (statTarget might throw some away based on cut off CV values (e.g >30% variation))
  # FUNC_list$ProfileFile$metabolite_list_update <- FUNC_list$ProfileFile$metabolite_list %>% 
  #   filter(metabolite_code %in% FUNC_list$corrected_data$data$name)
  
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
    rename(!!FUNC_header_sample_id := sample_id) %>%
    left_join(
      x = FUNC_list$standard_data %>%
        select(contains("sample")),
      y = .,
      by = FUNC_header_sample_id
    ) %>%
    select(-all_of(c("sample", "batch", "class", "order")))
      
  
  # SECTION 3 - concentration adjustment ------------------------------------
  #because the StatTarget correction changes the output concentrations of the lipids, this next section re-scales the values based on the change (ratio) between pre and post corrected signal mean in the QCs
  
  # #create empty list to store results
  # FUNC_list$corrected_data$data_ratio_adjusted <- list()
  
  #step one - get mean value for each metabolite in the QC samples - pre-single drift corrected data 
  FUNC_list$corrected_data$qc_means <- FUNC_list$standard_data %>%
    filter(!!as.symbol(FUNC_header_sample_type) == "qc") %>%
    select(-contains("sample")) %>%
    colMeans() %>%
    as_tibble() %>%
    rename(original_mean = value) %>%
    add_column(metabolite = FUNC_list$standard_data %>%
                 select(-contains("sample")) %>%
                 names(), 
               .before = "original_mean") %>%
    #step two - get mean value for each metabolite in the QC samples - post-single drift corrected data 
    left_join(.,
              FUNC_list$corrected_data$data_transposed %>%
                filter(!!as.symbol(FUNC_header_sample_type) == "qc") %>%
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
  #browser()
  #return QC type to FUNC data
  FUNC_DATA_qc_type <- unique(FUNC_data$sample_type_factor[which(FUNC_data$sample_type =="qc")]) %>% as.character()
    
  FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type<- "sample"
  FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type[which(FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type_factor == FUNC_DATA_qc_type)] <- "qc"

      
  FUNC_list$corrected_data$data_qc_mean_adjusted
}




