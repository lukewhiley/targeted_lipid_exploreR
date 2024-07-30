#impute function

lgw_lipid_conc_calc <- function(FUNC_data,
                                FUNC_SIL_list,
                                FUNC_SIL_guide,
                                FUNC_conc_guide)
{
  
  #set dummy data table for export
  FUNC_out <- list()
  FUNC_out$response <- FUNC_data %>% select(contains("sample"))
  FUNC_out$concentration <- FUNC_data %>% select(contains("sample"))
  
  
  for(idx_SIL in FUNC_SIL_list){
    if(length(FUNC_SIL_guide$precursor_name[which(FUNC_SIL_guide$note == idx_SIL)])>0){
      #find which SIL is used from the template
      target_lipids <- select(FUNC_data, any_of(FUNC_SIL_guide$precursor_name[which(FUNC_SIL_guide$note == idx_SIL)])) 
      if(ncol(target_lipids)>0){
        #calculate response ratio
        FUNC_out$response <- bind_cols(
          FUNC_out$response, 
          as_tibble(target_lipids/FUNC_data[[idx_SIL]])
        )
        
        #calculate concentration
        #Find the concentration factor of SIL
        sil_conc_factor <- FUNC_conc_guide$concentration_factor[which(FUNC_conc_guide$sil_name == idx_SIL)]
        if(length(sil_conc_factor) == 1){
          #select response data
          target_lipids_conc <- select(FUNC_out$response, any_of(FUNC_SIL_guide$precursor_name[which(FUNC_SIL_guide$note == idx_SIL)])) 
          #apply concentration factor to lipd values
          FUNC_out$concentration <- bind_cols(
            FUNC_out$concentration,
            as_tibble(target_lipids_conc*sil_conc_factor)
          )
        } #close if(length(sil_conc_factor) == 1)
      } # close if(ncol(target_lipids)>0)
    } #close if(length(FUNC_SIL_guide$precursor_name[which(FUNC_SIL_guide$note == idx_SIL)])>0){
  }# close for loop
  
  FUNC_out
  
}#close function
  