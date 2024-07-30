#ANPC PCA quality control visualisation

# FUNC_data = data containing individual lipid data - MUST CONTAIN sampleID column 
# FUNC_colour_by = how to colour the plot (e.g. sample class, is_ltr or cohort)
# FUNC_plot label = what to label the scores plot with (e.g. sampleID)
# FUNC_scaling = UV or Pareto

lgw_pca_filter <- function(FUNC_data, 
                           FUNC_metabolite_list, 
                           FUNC_scaling,
                           FUNC_option_iqr_filter_samples,
                           FUNC_option_iqr_filter_qc
){
  require(metabom8)
  require(tidyverse)
  
  pca_output <- list()
  
  qc_idx <- which(FUNC_data[["sample_type"]] == "qc")
  
  #create data matrix for PCA
  pca_x <- FUNC_data %>%  select(all_of(FUNC_metabolite_list)) %>% as.matrix()
  pca_x[pca_x == 0] <- NA #remove all 0 values (above adds 1 to all values therefore anything that = 1 was a 0)
  pca_x[is.infinite(pca_x)] <- NA #remove all infinite values
  min_value <- min(pca_x, na.rm = TRUE) # find the lowest value in the matrix
  pca_x[is.na(pca_x)] <- min_value/100 # replace all NA, Inf, and 0 values with the lowest value in the matrix/100 to represent a value below limit of detection
  
  pca_x <- log(pca_x+1) #log values for plotting
  
  #create PCA model using metabom8
  pca_output$pca_model <- metabom8::pca(pca_x, pc=3, scale = paste(FUNC_scaling), center = TRUE)
  
  #create tibble of data
  pca_output$plot_Val <- bind_cols(
    "sample_idx" = seq(1:length(pca_output$pca_model@t[,1])),
    "PC1" = as.numeric(as.matrix(pca_output$pca_model@t[,1])),
    "PC2" = as.numeric(as.matrix(pca_output$pca_model@t[,2])),
    "PC3" = as.numeric(as.matrix(pca_output$pca_model@t[,3])),
    "sample_name" = FUNC_data$sample_name,
    "sample_type" = FUNC_data$sample_type,
    "sample_plate_id" = FUNC_data$sample_plate_id
  )
  
  
  pca_output$fail_list <- NULL
  
  #loop for each component
  for(idx_PC in c("PC1", "PC2", "PC3")){
    #for samples
    sample_median <- (pca_output$plot_Val %>% filter(sample_type == "sample"))[[idx_PC]] %>% median()
    sample_sd <- (pca_output$plot_Val %>% filter(sample_type == "sample"))[[idx_PC]] %>% sd()
    sample_iq1 <- (pca_output$plot_Val %>% filter(sample_type == "sample"))[[idx_PC]] %>% quantile(0.25) %>% as.numeric()
    sample_iq3 <- (pca_output$plot_Val %>% filter(sample_type == "sample"))[[idx_PC]] %>% quantile(0.75) %>% as.numeric()
    sample_iqr <- (pca_output$plot_Val %>% filter(sample_type == "sample"))[[idx_PC]] %>% IQR()
    sample_threshold_low <- sample_iq1 - (sample_iqr*FUNC_option_iqr_filter_samples)
    sample_threshold_high <- sample_iq3 + (sample_iqr*FUNC_option_iqr_filter_samples)
    
    pca_output$fail_list <- c(pca_output$fail_list,
                              (pca_output$plot_Val %>% filter(sample_type == "sample"))[["sample_name"]][
                                which(
                                  (pca_output$plot_Val %>% filter(sample_type == "sample"))[[idx_PC]] < sample_threshold_low | 
                                    (pca_output$plot_Val %>% filter(sample_type == "sample"))[[idx_PC]] > sample_threshold_high)]
    )
    
    #for qc
    qc_median <- (pca_output$plot_Val %>% filter(sample_type == "qc"))[[idx_PC]] %>% median()
    qc_sd <- (pca_output$plot_Val %>% filter(sample_type == "qc"))[[idx_PC]] %>% sd()
    qc_iq1 <- (pca_output$plot_Val %>% filter(sample_type == "qc"))[[idx_PC]] %>% quantile(0.25) %>% as.numeric()
    qc_iq3 <- (pca_output$plot_Val %>% filter(sample_type == "qc"))[[idx_PC]] %>% quantile(0.75) %>% as.numeric()
    qc_iqr <- (pca_output$plot_Val %>% filter(sample_type == "qc"))[[idx_PC]] %>% IQR()
    qc_threshold_low <- qc_iq1 - (qc_iqr*FUNC_option_iqr_filter_qc)
    qc_threshold_high <- qc_iq3 + (qc_iqr*FUNC_option_iqr_filter_qc)
    
    pca_output$fail_list <- c(pca_output$fail_list,
                              (pca_output$plot_Val %>% filter(sample_type == "qc"))[["sample_name"]][
                                which(
                                  (pca_output$plot_Val %>% filter(sample_type == "qc"))[[idx_PC]] < qc_threshold_low | 
                                    (pca_output$plot_Val %>% filter(sample_type == "qc"))[[idx_PC]] > qc_threshold_high)]
    )
  }
  
  #metabolites must fail in >1 PC to be classed an failed outlier sample
  if(nrow(table(pca_output$fail_list)) > 0){
  pca_output$fail_list <- (table(pca_output$fail_list) %>% as.matrix() %>% as_tibble(rownames = "sample_name") %>% rename(frequency = V1) %>% filter(frequency > 1))[["sample_name"]]
  }
  pca_output
  
}