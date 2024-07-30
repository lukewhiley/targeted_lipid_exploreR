#impute function

lgw_lipid_conc_calc <- function(FUNC_data,
                                FUNC_metabolite_list,
                                FUNC_SIL_guide,
                                FUNC_conc_guide)
{
  
  FUNC_out <- FUNC_data %>% select(contains("sample"))
  
  FUNC_sil_list <- unique(FUNC_conc_guide$sil_name)
  
  for(idx_sil in FUNC_sil_list){
    if(length(which(names(FUNC_data) == idx_sil))==1){ #only perform the below if SIL is present in data (sometimes may be filtered out)
    target_lipids <- FUNC_data %>% select(any_of((FUNC_SIL_guide %>% filter(note == idx_sil) %>% select(precursor_name))[[1]])) #find target lipids that use defined SIL IS
    if(ncol(target_lipids) > 0){
    response_ratio <- (target_lipids/FUNC_data[[idx_sil]]) #calculate response ratio
    #calculate concentration from diltution factor
    concentration <- (response_ratio * (FUNC_conc_guide %>% 
      filter(sil_name == all_of(idx_sil)) %>%
      select(concentration_factor) %>% as.numeric())) %>% as_tibble()
    FUNC_out <- bind_cols(FUNC_out, concentration)
    }}}

  FUNC_out
}
