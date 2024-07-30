#welcome message
dlg_message("Welcome to skylineR! :-)", type = 'ok')

#set up project master list
master_list <- list(); master_list$environment <- list(); master_list$environment$user_functions <- list(); master_list$templates <- list(); master_list$templates$mrm_guides <- list(); master_list$project_details <- list();    master_list$data <- list(); master_list$data$mzR <- list(); master_list$data$skyline_reports <- list(); master_list$summary_tables <- list(); master_list$process_lists <- list()

#store environment details
master_list$environment$r_version <- sessionInfo()$R.version$version.string
master_list$environment$base_packages <- sessionInfo()$basePkgs
master_list$environment$user_packages <- paste0(names(sessionInfo()$otherPkgs), ": ", paste0(installed.packages()[names(sessionInfo()$otherPkgs), "Version"]))

##USER INPUT##
#set project details
dlg_message("select project directory", type = 'ok');master_list$project_details$project_dir <- rstudioapi::selectDirectory()
#set lipidExploreR version
master_list$project_details$lipidExploreR_version <- "3.2"
#set user
master_list$project_details$user_name <- dlgInput("user", "example_initials")$res
#set project name
master_list$project_details$project_name <- dlgInput("project", basename(paste0(master_list$project_details$project_dir)))$res
#set qc-type
master_list$project_details$qc_type <- dlgInput("qc type used - tag MUST be in filename of mzML files (matched case)", "LTR/SR/PQC")$res
#create summary table for report
master_list$summary_tables$project_summary <- tibble(unlist(master_list$project_details)) %>%
  add_column("Project detail" = c("local directory", "lipidExploreR version", "user initials", "project name", "project qc type"),
             .before = 1)
master_list$summary_tables$project_summary <- setNames(master_list$summary_tables$project_summary, c("Project detail", "value"))

#github master directory
master_list$project_details$github_master_dir <- "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR_v3/main"

#setup project directories
#data
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data"))){dir.create(paste0(master_list$project_details$project_dir, "/data"))}
#mzml
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/mzml"))){dir.create(paste0(master_list$project_details$project_dir, "/data/mzml"))}
#rda
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/rda"))){dir.create(paste0(master_list$project_details$project_dir, "/data/rda"))}
#skyline
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/skyline"))){dir.create(paste0(master_list$project_details$project_dir, "/data/skyline"))}
#sciex
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/sciex_raw"))){dir.create(paste0(master_list$project_details$project_dir, "/data/sciex_raw"))}
#batch_correct
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/batch_correction"))){dir.create(paste0(master_list$project_details$project_dir, "/data/batch_correction"))}
#html_reports
if(!dir.exists(paste0(master_list$project_details$project_dir, "/html_report"))){dir.create(paste0(master_list$project_details$project_dir, "/html_report"))}

#read in mrm_guide from github
master_list$templates$mrm_guides$mrm_guide <- read_csv(
  paste0(master_list$project_details$github_master_dir,
         "/templates/LGW_lipid_mrm_template.csv"),
  show_col_types = FALSE) 

#source functions
#RT finder
master_list$environment$user_functions$mrm_RT_findeR_mzR <- source(paste0(
  master_list$project_details$github_master_dir , 
  "/functions/FUNC_lipidExploreR_MRM_findeR_pwiz3019_mzR.R"))

#peak_boundary findeR
master_list$environment$user_functions$mrm_pb_findeR <- source(paste0(
  master_list$project_details$github_master_dir, 
  "/functions/FUNC_lipidExploreR_peak_boundary_findeR.R"))

dlg_message("convert SCIEX files to mzML", type = 'ok'); dlg_message(paste0("mzML directory: [", paste0(master_list$project_details$project_dir, "/data/mzml"), "]"), type = 'ok'); dlg_message("put mzML files in sub folders per plate [/data/mzml/plate_1; /data/mzml/plate_2] etc")

master_list$project_details$mzml_plate_list <- list.dirs(paste0(master_list$project_details$project_dir, 
                                                                "/data/mzml"),
                                                         recursive = FALSE,
                                                         full.names = FALSE)

dlg_message(paste0("there are ", length(master_list$project_details$mzml_plate_list), " plates of samples"), type = 'ok')

# PROCESS: IMPORT mzML FILES USING mzR ------------------------------------
mzml_filelist <- list() 
plate_list <- NULL
for(idx_plate in master_list$project_details$mzml_plate_list){
  mzml_filelist[[idx_plate]] <- list.files(paste0(master_list$project_details$project_dir, 
                                                  "/data/mzml/",
                                                  idx_plate),
                                           pattern = ".mzML",
                                           full.names = FALSE)
  
  plate_list <- c(plate_list, paste0(idx_plate," = ", length(mzml_filelist[[idx_plate]]), " samples; "))
}

dlg_message(plate_list, type = 'ok')
master_list$project_details$mzml_sample_list <- NULL
for(idx_plate in master_list$project_details$mzml_plate_list){
  master_list$data$mzR[[idx_plate]] <- list()
  #read in mzML files using mzR
  for(idx_mzML in mzml_filelist[[idx_plate]]){
    master_list$data$mzR[[idx_plate]][[idx_mzML]] <- list()
    master_list$data$mzR[[idx_plate]][[idx_mzML]]$mzR_object <- mzR::openMSfile(
      filename = paste0(master_list$project_details$project_dir, 
                        "/data/mzml/",
                        idx_plate,"/", idx_mzML))
    master_list$data$mzR[[idx_plate]][[idx_mzML]]$mzR_header <- mzR::chromatogramHeader(master_list$data$mzR[[idx_plate]][[idx_mzML]]$mzR_object)
    master_list$data$mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram <- mzR::chromatograms(master_list$data$mzR[[idx_plate]][[idx_mzML]]$mzR_object)
    master_list$data$mzR[[idx_plate]][[idx_mzML]]$mzR_timestamp <- master_list$data$mzR[[idx_plate]][[idx_mzML]]$mzR_object@backend$getRunStartTimeStamp()
  }
  master_list$project_details$mzml_sample_list <- c(master_list$project_details$mzml_sample_list, names(master_list$data$mzR[[idx_plate]]))
}

#######
# Retention time optimiser
master_list$templates$mrm_guides$mrm_guide_rt_update <- tibble()

#run function
master_list$templates$mrm_guides$mrm_guide_rt_update <- master_list$environment$user_functions$mrm_RT_findeR_mzR$value(
  FUNC_mzR = master_list$data$mzR, #list for each sample containing $mzR_object; $mzR_header; $mzR_chromatogram
  FUNC_mrm_guide = master_list$templates$mrm_guides$mrm_guide %>% clean_names(),
  FUNC_OPTION_qc_type = master_list$project_details$qc_type)

#set names so that skyline recognises columns
master_list$templates$mrm_guides$mrm_guide_rt_update <- setNames(master_list$templates$mrm_guides$mrm_guide_rt_update,
                                                       names(master_list$templates$mrm_guides$mrm_guide))

rm(temp_mzR_list)

#export updated optimised RT times
write_csv(x = master_list$templates$mrm_guides$mrm_guide_rt_update,
          file = paste0(master_list$project_details$project_dir, "/data/skyline/", Sys.Date(), "_RT_update_", 
                        master_list$project_details$project_name, ".csv"))

#create directory for storing skyline exports
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/skyline"))){
  dir.create(paste0(master_list$project_details$project_dir, "/data/skyline"))
}

#interact with skyline
dlg_message("1. Please open skylineMS software", type = 'ok');dlg_message("2. Create new small molecule file", type = 'ok'); dlg_message("3. Import the [skylineR_RT_update.csv] transition list located in [~project_directory/data/skyline] folder. In Skyline navigate to File -> import -> transition list", type = 'ok'); dlg_message("4. Save project", type = 'ok'); dlg_message("5. Import mzml data files for processing by navigating to File -> import -> results", type = 'ok'); dlg_message("6. Let skyline process.  Export results to [~project_directory/data/skyline] folder with the tag xskylineR_1.  Export reports must have the following headings: File Name, Molecule List Name, Molecule Name, Area, Retention Time, Start Time and End Time", type = 'ok'); 

#re_import skyline file
master_list$data$skyline_reports$report_1 <- read_csv(file = paste0(list.files(
  paste0(master_list$project_details$project_dir, "/data/skyline"),
  pattern = "xskylineR_1", full.names = TRUE)), show_col_types = FALSE) %>% mutate_at(
    vars("Precursor Mz", "Product Mz", "Retention Time", "Start Time", "End Time", "Area", "Height"), 
    as.numeric) 

#perform peak boundary update
master_list$templates$mrm_guides$mrm_guide_pb_update <- list()
for(idx_plate in master_list$project_details$mzml_plate_list){
  master_list$templates$mrm_guides$mrm_guide_pb_update[[idx_plate]] <- master_list$environment$user_functions$mrm_pb_findeR$value(
    FUNC_data = master_list$data$skyline_reports$report_1 %>% 
      clean_names() %>%
      filter(file_name %in% names(master_list$data$mzR[[idx_plate]])),
    FUNC_OPTION_qc_type = master_list$project_details$qc_type
  )
}
#bind all rows into master pb list
master_list$templates$mrm_guides$mrm_guide_pb_update_all_plates <- bind_rows(master_list$templates$mrm_guides$mrm_guide_pb_update)

#write peak boundary output
write_csv(x = master_list$templates$mrm_guides$mrm_guide_pb_update_all_plates,
          file = paste0(master_list$project_details$project_dir, "/data/skyline/", Sys.Date(), "_peak_boundary_update_", 
                        master_list$project_details$project_name, ".csv"))

dlg_message("1. Please return to skylineMS software", type = 'ok'); dlg_message("2. Import the new skylineR_boundary_update.csv transition list from csv file by navigating to File -> import -> peak boundaries", type = 'ok'); dlg_message("4. Save project", type = 'ok');dlg_message("5. Export results to [~project_directory/data/skyline] folder with the tag xskylineR_2.  Export reports must have the following headings: File Name, Molecule List Name, Molecule Name, Area, Retention Time, Start Time and End Time", type = 'ok'); dlg_message("7. Now return to R Studio and run the lipid_exploreR to QC check data", type = 'ok')

#import processed skyline_data
#re_import skyline file
master_list$data$skyline_reports$report_2 <- read_csv(file = paste0(list.files(
  paste0(master_list$project_details$project_dir, "/data/skyline"),
  pattern = "xskylineR_2", full.names = TRUE)), show_col_types = FALSE) %>% mutate_at(
    vars("Precursor Mz", "Product Mz", "Retention Time", "Start Time", "End Time", "Area", "Height"), 
    as.numeric) %>%
  clean_names()

#create directory for exporting rda files
if(!dir.exists(paste0(master_list$project_details$project_dir, "/data/rda"))){
  dir.create(paste0(master_list$project_details$project_dir, "/data/rda"))
}

save(master_list, file = paste0(master_list$project_details$project_dir,"/data/rda/", Sys.Date(), "_skylineR_", master_list$project_details$project_name, ".rda"))

rm(list = c(ls()[which(ls() != "master_list")]))