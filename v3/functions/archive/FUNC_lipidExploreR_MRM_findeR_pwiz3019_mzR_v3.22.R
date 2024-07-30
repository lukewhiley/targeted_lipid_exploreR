# RT findeR
mzR_mrm_findR <- function(FUNC_mzR, #list from master_list containing $mzR object for each sample; $mzR_header; $mzR_chromatogram
                          FUNC_mrm_guide, #tibble of mrm details
                          FUNC_OPTION_qc_type #qc_type used in the experiment LTR; PQC; none
                          
){
  
  #create output list
  FUNC_output <- list()
  FUNC_output$mrm_guide_updated <- FUNC_mrm_guide
  FUNC_output$peak_boundary_update <- list()
  
  
  
  #list mzR objects
  mzML_filelist <- NULL
  for(idx_plate in names(FUNC_mzR)){
    mzML_filelist <- c(mzML_filelist, names(FUNC_mzR[[idx_plate]]))
  }
  
  #list mzR objects matching qc type
  mzML_filelist_qc <- mzML_filelist[grep(FUNC_OPTION_qc_type, mzML_filelist)]
  
  # PROCESS: Find peak apex and boundaries using QC mzR (mzML) data ------------
  
  #this section finds peak start, end and apex using mzR data for the QC samples. Will be applied later to skyline.
  
  #for each mrm transtion in the transition data
  for (idx_mrm in 1:nrow(FUNC_mrm_guide)){
    # find precursor reference
    precursor_mz <- FUNC_mrm_guide$precursor_mz[idx_mrm] 
    #find product ion reference
    product_mz <- FUNC_mrm_guide$product_mz[idx_mrm]
    #find template retention time reference
    template_rt <- FUNC_mrm_guide$explicit_retention_time[idx_mrm]
    
    
    #find transition in each mzML file and find median peak apex
    mzml_rt_apex_out <- NULL
    mzml_rt_start_out <- NULL
    mzml_rt_end_out <- NULL
    for(idx_mzML in mzML_filelist_qc){
      #print(idx_mzML)
      #find the mzR mzML file from list
      for(idx_plate in names(FUNC_mzR)){
        if(length(grep(idx_mzML, names(FUNC_mzR[[idx_plate]]))) == 1){
          
          #find the data channel in the mzml file containing the data
          idx_mrm_channel <- which(
            FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$precursorIsolationWindowTargetMZ > (precursor_mz - 0.25) &
              FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$precursorIsolationWindowTargetMZ < (precursor_mz + 0.25) &
              FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$productIsolationWindowTargetMZ > (product_mz - 0.25) &
              FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$productIsolationWindowTargetMZ < (product_mz + 0.25)
            
          )
          
          #remove multiple matches (using retention time window match)
          if(length(idx_mrm_channel) == 0 &
             idx_mzML == mzML_filelist_qc[1]){
            print(paste("precursor = ", precursor_mz, "; product = ",product_mz, "; no MRM matches", sep = ""))
          } 
          
          if(length(idx_mrm_channel ) > 1 &
             #only print for 1 file per project loop
             idx_mzML == mzML_filelist_qc[1]){
            print(paste("precursor = ", precursor_mz, "; product = ",product_mz, "; multiple MRM matches -  channels = ", paste(idx_mrm_channel, collapse = " "), sep = ""))
          }
          
          if(length(idx_mrm_channel) > 1){
            for(idx_rt in idx_mrm_channel){
              if(template_rt > (FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_rt]]$time %>% min()) &
                 template_rt < (FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_rt]]$time %>% max())){
                idx_mrm_channel <- idx_rt
              }
              
            }
            
            #only print for 1 file per project loop
            if(idx_mzML == mzML_filelist_qc[1]){print(paste("channel selected based on RT estimate = ", idx_mrm_channel, sep = ""))
            }
            
          }
          
          
          
          # complete the below once the idx_mrm_channel has a single unique match
          if(length(idx_mrm_channel) ==1){
            
            # find baseline of transition window
            baseline_value <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2] %>% 
              median()
            
            if(baseline_value < 1){
              baseline_value <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2] %>%
                mean()
            }
            
            #find scan indeces
            
            #1. find scan index of max intensity within mrm channel
            peak_apex_idx <- which.max(
              FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2]
            )
            
            #re-calculate and re-centre if in wings of window
            if(peak_apex_idx < 5 |
               peak_apex_idx > (length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2])-5)){
              peak_apex_idx <-  which.max(
                (FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2])[5:(length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2])-5)]
              )+4
            }
            
            #2. find scan index of peak start time
            peak_start <- which(
              (FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2]) < baseline_value
            )
            
            # identify scan index where peak returns to baseline after peak apex
            peak_start_idx <- peak_start[which(peak_start < peak_apex_idx)][(length(which(peak_start < peak_apex_idx)))-3]
            
            #if peak_start_idx is beyond idx range set at 1.
            if(length(peak_start_idx) == 0){
              peak_start_idx <- 1
            }
            if(peak_start_idx < 1){
              peak_start_idx <- 1
            }
            
            
            #3. find scan index of peak end time (first point peak reaches baseline after peak apex)
            peak_end <- which(
              (FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2]) < baseline_value
            )
            # identify scan index where peak returns to baseline after peak apex
            peak_end_idx <- peak_end[which(peak_end > peak_apex_idx)][3]
            
            #browser()
            #if peak_start_idx is beyond idx range set at max length of scan index
            if(length(peak_end_idx) == 0){
              peak_end_idx <- length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2])
            }
            if(peak_end_idx > length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2]) |
               is.na(peak_end_idx)==TRUE){
              peak_end_idx <- length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]][,2])
            }
            
            
            #find retention times according to scan indices
            #1. find rt of peak_apex
            mzml_rt_apex <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]]$time[peak_apex_idx] %>% round(2)
            
            #2. find rt of peak start time
            mzml_rt_start <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]]$time[peak_start_idx] %>% round(2)
            
            #3. find rt of peak end time
            mzml_rt_end <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm_channel]]$time[peak_end_idx] %>% round(2)
            
            
            #bind retention times for all QCs
            #1.c() apex rt
            mzml_rt_apex_out <- c(mzml_rt_apex_out, mzml_rt_apex)
            
            #2. c() start rt
            mzml_rt_start_out <- c(mzml_rt_start_out, mzml_rt_start)
            
            #3. c() end rt
            mzml_rt_end_out <- c(mzml_rt_end_out, mzml_rt_end)
            
          }
        }
      }
    }
    
    #update RT for the import guide
    if(length(mzml_rt_apex_out) > 0){
      
      
      #print(paste0(FUNC_mrm_guide$precursor_name[[idx_mrm]], ": peak apex = ", median(mzml_rt_apex_out)))
      
      
      mzml_median_rt <- median(mzml_rt_apex_out)
      FUNC_mrm_guide$explicit_retention_time[idx_mrm] <- round(mzml_median_rt, 2)
    }
    
    
    #create peak boundary table
    if(length(mzml_rt_start_out) > 0){
      
      #print(paste0(FUNC_mrm_guide$precursor_name[[idx_mrm]], ": peak start = ", min(mzml_rt_start_out)))
      #print(paste0(FUNC_mrm_guide$precursor_name[[idx_mrm]], ": peak end = ", max(mzml_rt_end_out)))
      
      
      FUNC_output$peak_boundary_update[[FUNC_output$mrm_guide_updated$precursor_name[[idx_mrm]]]] <- mzML_filelist %>% 
        as_tibble() %>% 
        dplyr::rename(FileName = value) %>%
        add_column("FullPeptideName" = rep(FUNC_output$mrm_guide_updated$precursor_name[[idx_mrm]],
                                           length(mzML_filelist))
        ) %>%
        add_column("MinStartTime" = rep((mzml_rt_start_out %>% summary)[["1st Qu."]],
                                        length(mzML_filelist))
        )  %>% 
        add_column("MaxEndTime" = rep((mzml_rt_end_out %>% summary)[["3rd Qu."]],
                                      length(mzML_filelist))
        )
      
    }
    
    #browser()
    
  }
  #prepare output final table
  FUNC_output$mrm_guide_updated <- FUNC_mrm_guide
  FUNC_output$peak_boundary_update <- bind_rows(FUNC_output$peak_boundary_update)
  
  #output
  FUNC_output
}

