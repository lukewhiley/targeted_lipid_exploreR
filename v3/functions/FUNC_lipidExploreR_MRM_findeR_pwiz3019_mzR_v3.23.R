
# RT findeR
mzR_mrm_findR <- function(FUNC_mzR, #list from master_list containing $mzR object for each sample; $mzR_header; $mzR_chromatogram
                          FUNC_mrm_guide, #tibble of mrm details
                          FUNC_OPTION_qc_type #qc_type used in the experiment LTR; PQC; none
                          
){
  
  #create output list
  FUNC_output <- list()
  
  #list mzR objects
  mzML_filelist <- NULL
  for(idx_plate in names(FUNC_mzR)){
    mzML_filelist <- c(mzML_filelist, names(FUNC_mzR[[idx_plate]]))
  }
  
  #list mzR objects matching qc type
  mzML_filelist_qc <- mzML_filelist[grep(FUNC_OPTION_qc_type, mzML_filelist)]
  
  # PROCESS: Find peak apex and boundaries using QC mzR (mzML) data ------------
  
  #set output files 
  FUNC_tibble <- list()
  
  #loop to perform individually for each file
  for(idx_mzML in mzML_filelist_qc){
    FUNC_tibble[[idx_mzML]] <- tibble()
    
    #find file (data in master list stored in plate sublists)
    for(idx_plate in names(FUNC_mzR)){
      if(length(grep(idx_mzML, names(FUNC_mzR[[idx_plate]]))) == 1){
        
        #loop to perform individually for each mrm transition
        for(idx_mrm in 3:nrow(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header)){
          
          #run if statement - only perform on transitions containing data
          if(nrow(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]) > 0){
          
          #store precursor and product information
          precursor_mz <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$precursorIsolationWindowTargetMZ[idx_mrm]
          product_mz <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header$productIsolationWindowTargetMZ[idx_mrm]
          
          
          # find baseline of transition window
          baseline_value <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2] %>% 
            median()
          
          #if mrm transition contains lots of 0 median will = 0 so use mean 
          if(baseline_value < 1){
            baseline_value <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2] %>%
              mean()
          }  
          
          #find scan indeces
          
          #find scan index of max intensity within mrm channel
          peak_apex_idx <- which.max(
            FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2]
          )
          
          #re-calculate and re-centre if in wings of window
          if(peak_apex_idx < 5 |
             peak_apex_idx > (length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])-5)){
            peak_apex_idx <-  which.max(
              (FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])[5:(length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])-5)]
            )+4
          }
          
          # find scan index of scans below baseline noise
          baseline_idx <- which(
            (FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2]) < baseline_value
          )
          
          # identify scan index where peak returns to baseline after peak apex
          peak_start_idx <- baseline_idx[which(baseline_idx < peak_apex_idx)][(length(which(baseline_idx < peak_apex_idx)))-3]
          
          #if peak_start_idx is beyond idx range set at 1.
          if(length(peak_start_idx) == 0){
            peak_start_idx <- 1
          }
          if(peak_start_idx < 1){
            peak_start_idx <- 1
          }
          
          # identify scan index where peak returns to baseline after peak apex
          peak_end_idx <- baseline_idx[which(baseline_idx > peak_apex_idx)][3]
          
          #if peak_start_idx is beyond idx range set at max length of scan index
          if(length(peak_end_idx) == 0){
            peak_end_idx <- length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])
          }
          if(peak_end_idx > length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2]) |
             is.na(peak_end_idx)==TRUE){
            peak_end_idx <- length(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][,2])
          }
          
          
          #find retention times according to scan indices
          #1. find rt of peak_apex
          mzml_rt_apex <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$time[peak_apex_idx] %>% round(2)
          
          #2. find rt of peak start time
          mzml_rt_start <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$time[peak_start_idx] %>% round(2)
          
          #3. find rt of peak end time
          mzml_rt_end <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$time[peak_end_idx] %>% round(2)
          
          
          #find target lipid name and family from mrm_guide
          lipid_idx <- which(
            FUNC_mrm_guide$precursor_mz == precursor_mz &
              FUNC_mrm_guide$product_mz == product_mz
            )
          
          #relax tolerances if first match fails or gets multiple hits - e.g. isomer with same MRM transition, introduces RT thresholds
          if(length(lipid_idx) != 1){
          lipid_idx <- which(
            FUNC_mrm_guide$precursor_mz > (precursor_mz - 0.25) &
              FUNC_mrm_guide$precursor_mz < (precursor_mz + 0.25) &
              FUNC_mrm_guide$product_mz > (product_mz - 0.25) &
              FUNC_mrm_guide$product_mz < (product_mz + 0.25) &
              FUNC_mrm_guide$explicit_retention_time > (min(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$time)-0.1)&
              FUNC_mrm_guide$explicit_retention_time < (max(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$time)+0.1)
          )
          }
          
          if(length(lipid_idx) > 1){
            #only print multiple matches for first file
            if(idx_mzML == mzML_filelist_qc[1]){print(paste("multiple matches for precursor =", precursor_mz, "; product =", product_mz, "; retention time =", mzml_rt_apex, "; transitions", paste(lipid_idx, collapse = " ")))}
            lipid_class <- "multiple match"
            lipid <- "multiple match"
          }
          
          if(length(lipid_idx) == 0){
            #only print multiple matches for first file
            if(idx_mzML == mzML_filelist_qc[1]){print(paste("no match for precursor =", precursor_mz, "; product =", product_mz, "; retention time =", mzml_rt_apex, "; transitions"))}
            lipid_class <- "no match"
            lipid <- "no match"
          }
          
          if(length(lipid_idx) == 1){
            lipid_class <- FUNC_mrm_guide$molecule_list_name[lipid_idx]
            lipid <- FUNC_mrm_guide$precursor_name[lipid_idx]
          }
          
          
          
          FUNC_tibble[[idx_mzML]] <- FUNC_tibble[[idx_mzML]] %>% 
            bind_rows(
              bind_cols("mzml" = idx_mzML,
                        "lipid_class" = lipid_class,
                        "lipid" = lipid,
                        "precursor_mz" = precursor_mz, 
                        "product_mz" = product_mz, 
                        "peak_apex" = mzml_rt_apex, 
                        "peak_start" = mzml_rt_start,
                        "peak_end" = mzml_rt_end)
            )
        }
        }
      }
    }
    }
  
  #create master list
  FUNC_tibble <- bind_rows(FUNC_tibble) %>%
    filter(lipid_class != "no match") %>%
    filter(lipid_class != "multiple match")
  
  
  FUNC_output$mrm_guide_updated <- tibble()
  FUNC_output$peak_boundary_update <- tibble()
  
  for(idx_lipid in unique(FUNC_tibble$lipid)){
    
    FUNC_output$mrm_guide_updated <- bind_rows(FUNC_output$mrm_guide_updated,
                                               bind_cols(
                                                 "Molecule List Name" = (FUNC_tibble %>% filter(lipid == idx_lipid))[["lipid_class"]] %>% unique(),
                                                 "Precursor Name" = idx_lipid,
                                                 "Precursor Mz" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["precursor_mz"]],
                                                 "Precursor Charge" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["precursor_charge"]],
                                                 "Product Mz" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["product_mz"]],
                                                 "Product Charge" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["product_charge"]],
                                                 "Explicit Retention Time" = (FUNC_tibble %>% filter(lipid == idx_lipid))[["peak_apex"]] %>% median(),
                                                 "Explicit Retention Time Window" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["explicit_retention_time_window"]],
                                                 "Note" = (FUNC_mrm_guide %>% filter(precursor_name == idx_lipid))[["note"]]
                                               ))
    
    
    FUNC_output$peak_boundary_update <- bind_rows(FUNC_output$peak_boundary_update,
                                                  bind_cols(
                                                    "FileName" = mzML_filelist,
                                                    "FullPeptideName" =  rep(idx_lipid, length(mzML_filelist)),
                                                    "MinStartTime" = rep(
                                                      ((FUNC_tibble %>% filter(lipid == idx_lipid))[["peak_start"]] %>% summary())[["1st Qu."]], 
                                                      length(mzML_filelist)
                                                    ),
                                                    "MaxEndTime" =  rep(
                                                      ((FUNC_tibble %>% filter(lipid == idx_lipid))[["peak_end"]] %>% summary())[["3rd Qu."]], 
                                                      length(mzML_filelist)
                                                    )
                                                  )
    )
                                                  
                                                  
  }
  
  FUNC_output$mrm_guide_updated <-  FUNC_output$mrm_guide_updated %>% dplyr::arrange(`Precursor Name`)
  
  FUNC_output$peak_boundary_update <- FUNC_output$peak_boundary_update %>% arrange(`FullPeptideName`)
  
  FUNC_output    
  
}



