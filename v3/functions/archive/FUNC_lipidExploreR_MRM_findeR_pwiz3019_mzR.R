# RT findeR
mzR_mrm_findR <- function(FUNC_mzR, #list from master_list containing $mzR object for each sample; $mzR_header; $mzR_chromatogram
                          FUNC_mrm_guide, #tibble of mrm details
                          FUNC_OPTION_qc_type #qc_type used in the experiment LTR; PQC; none
                          
){
  
  #list mzR objects
  mzML_filelist <- NULL
  for(idx_plate in names(FUNC_mzR)){
    mzML_filelist <- c(mzML_filelist, names(FUNC_mzR[[idx_plate]]))
  }
  
  #list mzR objects matching qc tyope
  mzML_filelist_qc <- mzML_filelist[grep(FUNC_OPTION_qc_type, mzML_filelist)]
  
  mzML_filelist_crop <- mzML_filelist_qc
  
  
  #browser()    
  rt_find <- NULL
  #for each mrm transtion in the transition data
  for (idx_mrm in 1:nrow(FUNC_mrm_guide)){
    # find precursor reference
    precursor_mz <- FUNC_mrm_guide$precursor_mz[idx_mrm]
    #find product ion reference
    product_mz <- FUNC_mrm_guide$product_mz[idx_mrm]
    
    #find transition in each mzML file and find median peak apex
    mzml_rt_apex_out <- NULL
    for(idx_mzML in mzML_filelist_crop){
      #find the mzR mzML file from list
      for(idx_plate in names(FUNC_mzR)){
        if(length(grep(idx_mzML, names(FUNC_mzR[[idx_plate]]))) == 1){
          #find the data channel in the mzml file containing the data
          idx_mrm_channel <- which(
            FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$precursorIsolationWindowTargetMZ == precursor_mz &
              FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$productIsolationWindowTargetMZ == product_mz
          )
          #only complete the below if idx_mrm_channel finds a single unique match
          if(length(idx_mrm_channel) ==1){
            #find scan index of max intensity within mrm channel
            mzml_max_intensity <- which.max(
              FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2]
            )
            
            #find rt_peak_apex
            mzml_rt_apex <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]]$time[mzml_max_intensity] %>% round(2)
            #c() mzml rt apex from all mzml
            mzml_rt_apex_out <- c(mzml_rt_apex_out, mzml_rt_apex)
          }
        }
      }
      if(length(mzml_rt_apex_out) > 0){
        mzml_median_rt <- median(mzml_rt_apex_out)
        FUNC_mrm_guide$explicit_retention_time[idx_mrm] <- round(mzml_median_rt, 2)
      }
    }
  }
  #output final table
  FUNC_mrm_guide
}
