#ANPC PCA quality control visualisation

# c("uncorrectedPeakArea",
#   "statTargetCorrectedData"))

#test
#pca scores plot function
#master_list$environment$user_functions$pca <- source(paste0(master_list$project_details$github_master_dir,"/functions/FUNC_lipidExploreR_PCA_ggplot.R"))

lgw_control_chart <- function(
    FUNC_data_area,
    FUNC_data_area_concentration,
    FUNC_data_statTarget_concentration,
    FUNC_metabolite_list,
    FUNC_HEADER_run_order,
    FUNC_HEADER_plate_id,
    FUNC_HEADER_colour_by,
    FUNC_HEADER_highlight_by,
    FUNC_HEADER_label_by,
    FUNC_OPTION_title,
    FUNC_OPTION_colours,
    FUNC_OPTION_fill,
    FUNC_OPTION_size,
    FUNC_OPTION_shape,
    FUNC_OPTION_legend_title){

  #produce plot_ly PCA scores plot
  control_chart_out <- list()
  
  #get metabolite list
  
  # make long data table consisting of area, 
  FUNC_long_data <- bind_rows(
    FUNC_data_area,
    FUNC_data_area_concentration,
    FUNC_data_statTarget_concentration
  ) 

  
  #for testing 
  idx_metabolite = FUNC_metabolite_list$precursor_name[1]
  for(idx_metabolite in FUNC_metabolite_list$precursor_name){
    idx_SIL = FUNC_metabolite_list$note[which(FUNC_metabolite_list$precursor_name == idx_metabolite)]

    
    plot_Val <- bind_rows(
      #extract metabolite data from long table
      FUNC_long_data %>%
        rename(sample_plot_metabolite = all_of(idx_metabolite)) %>%
        select(contains("sample")),
      FUNC_long_data %>%
        rename(sample_plot_metabolite = all_of(idx_SIL)) %>%
        select(contains("sample")) %>%
        add_column(.sample_data_source = paste0("SIL_", .$sample_data_source), .after = "sample_data_source") %>%
        select(-sample_data_source) %>%
        rename(sample_data_source = .sample_data_source) %>%
        filter(!is.na(sample_plot_metabolite))
    ) %>%
      mutate(sample_data_source = factor(
       sample_data_source,
       levels =  c("peakArea",
              "SIL_peakArea",
               "concentration[postFilter]", "statTarget[postFilter]"),
      ordered = TRUE
      ))

    
    #plot_Val$sample_box_index <- -10
    #plot_Val$sample_box_index[which(plot_Val$sample_type_factor == "vltr")] <- -8
    #plot_Val$sample_box_index[which(plot_Val$sample_type_factor == "sltr")] <- -6
    #plot_Val$sample_box_index[which(plot_Val$sample_type_factor == "ltr")] <- -4
    #plot_Val$sample_box_index[which(plot_Val$sample_type_factor == "pqc")] <- -2
    
    #find plate boundaries for vline
    plate_boundary = 0.5
    annotate_coordinate = NULL
    boundary_finder <- plot_Val %>%
      select(sample_run_index, sample_plate_id) %>%
      distinct()
    
    for(idx_plate in unique(boundary_finder$sample_plate_id)){
      plate_boundary <- c(
        plate_boundary,
        c(
          min(
            (boundary_finder %>% 
               filter(sample_plate_id == idx_plate))[["sample_run_index"]]
          ) - 0.5,
          max(
            (boundary_finder %>% 
               filter(sample_plate_id == idx_plate))[["sample_run_index"]]
          ) + 0.5
        )
      )
      
      annotate_coordinate <- c(
        annotate_coordinate,
        median(
          (boundary_finder %>% 
             filter(sample_plate_id == idx_plate))[["sample_run_index"]]
        )
      )
    }
    
    plate_boundary <- unique(plate_boundary)
    
    
    # #finalise anotation co-ordinates for geom_text
    annotate_coordinate <- setNames(annotate_coordinate, unique(plot_Val$sample_plate_id))
    
    #make data.frame
    annotate_label <- tibble(
      sample_data_source = rep(unique(plot_Val$sample_data_source), length(names(annotate_coordinate))) %>% sort(),
      plate_id = rep(names(annotate_coordinate), length(unique(plot_Val$sample_data_source))),
      run_index = rep(annotate_coordinate, length(unique(plot_Val$sample_data_source))),
      sample_plot_metabolite = NA
    )
    
    
    for(idx_facet in unique(annotate_label$sample_data_source)){
      annotate_label[["sample_plot_metabolite"]][which(annotate_label$sample_data_source == idx_facet)] <- plot_Val %>% filter(sample_data_source == idx_facet) %>% .[["sample_plot_metabolite"]] %>% min(na.rm = TRUE)    
    }
    
    
    #make control chart plot
    control_chart_out[[idx_metabolite]] <- ggplotly(
      ggplot(
        data = plot_Val,
        aes(x = sample_run_index, y = sample_plot_metabolite,
            group  = sample_name,
            fill = sample_type_factor,
            color = sample_type_factor,
            shape = sample_type_factor,
            size = sample_type_factor,
        )) +
        geom_vline(xintercept = plate_boundary, linetype = "dashed") +
        geom_point() +
       #geom_boxplot(inherit.aes = FALSE, data = plot_Val, 
        #             aes(x = sample_box_index, 
         #                y = sample_plot_metabolite, 
          #               group = sample_type_factor,
           #              fill = sample_type_factor)) +
        theme_bw() +
        theme(axis.line = element_line(color='black'))+
        scale_shape_manual(values = FUNC_OPTION_shape) +
        scale_fill_manual(values = FUNC_OPTION_fill) +
        scale_color_manual(values = FUNC_OPTION_colours) +
        scale_size_manual(values = FUNC_OPTION_size) +
        scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
        #ggtitle(paste0(FUNC_OPTION_title, "; Control Chart; ", idx_metabolite)) +
        ylab("") +
        xlab("") +
        guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=FUNC_OPTION_legend_title)) +
        facet_wrap(facets = "sample_data_source", ncol = 1, scales = "free_y") +
        geom_text(inherit.aes = FALSE, data = annotate_label, aes(x = run_index, y = sample_plot_metabolite, label = plate_id), col = "black")
        ) %>% ##gplotly close
      #layout(
       # boxgap=100, 
        #boxgapgroup = 100) %>%
      layout(title = list(
          text =paste0("Control Chart; ", FUNC_OPTION_title, "; ", idx_metabolite),
          y = 1.1, 
          x=0.05),
        margin = list(
          l = 10, r = 10, b=65, t=85),
        legend = list(
          orientation = "v",   # show entries horizontally
          xanchor = "center",  # use center of legend as anchor
          x = 1.1,
          y=0.5)           # put legend in center of x-axis
      )
    
  }
  
  control_chart_out
  
}
    