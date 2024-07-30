#ANPC PCA quality control visualisation

#test
#pca scores plot function
#master_list$environment$user_functions$pca <- source(paste0(master_list$project_details$github_master_dir,"/functions/FUNC_lipidExploreR_PCA_ggplot.R"))

lgw_pca_run_order <- function(FUNC_data_concentration,
                              FUNC_data_concentration_corrected,
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
                              FUNC_OPTION_legend_title
){
  
  #produce plot_ly PCA scores plot
  PCA_run_order <- list()
  
  
  
  # make long data table consisting of area, 
  plot_Val <- bind_rows(
    FUNC_data_concentration %>%
      rename(
        PC1_concentration = PC1,
        PC2_concentration = PC2,
        PC3_concentration = PC3
        ) %>%
      pivot_longer(
        cols = starts_with("PC"),
        names_to = "PC",
        values_to = "score"
      ),
    FUNC_data_concentration_corrected %>%
      rename(
        PC1_concentration_corrected = PC1,
        PC2_concentration_corrected = PC2,
        PC3_concentration_corrected = PC3
      ) %>%
      pivot_longer(
        cols = starts_with("PC"),
        names_to = "PC",
        values_to = "score"
      )
  )
    
    plot_Val$sample_data_type <- factor(plot_Val$sample_data_type,
                                        levels = c("concentration", "concentration_corrected"), 
                                        ordered = TRUE)
     
    
    
    #find plate boundaries for vline
    plate_boundary = 0.5
    annotate_coordinate = NULL
    boundary_finder <- plot_Val %>%
      select(run_index, plate_id) %>%
      distinct()
    
    for(idx_plate in unique(boundary_finder$plate_id)){
      plate_boundary <- c(
        plate_boundary,
        c(
          min(
            (boundary_finder %>% 
               filter(plate_id == idx_plate))[["run_index"]]
          ) - 0.5,
          max(
            (boundary_finder %>% 
               filter(plate_id == idx_plate))[["run_index"]]
          ) + 0.5
        )
      )
      
      annotate_coordinate <- c( annotate_coordinate,
        median(
          (boundary_finder %>% 
             filter(plate_id == idx_plate))[["run_index"]]
        )
      )
    }
    
    #remove duplcates
    plate_boundary <- unique(plate_boundary)
    
    #finalise anotation co-ordinates
    annotate_coordinate <- setNames(annotate_coordinate, unique(plot_Val$plate_id))
    
    annotate_label <- plot_Val %>%
      select(PC, plate_id) %>%
      distinct() %>%
      add_column(run_index = NA,
                 sample_name = NA,
                 qc = "plate_id",
                 score = NA)
    
    for (idx_plate in unique(annotate_label$plate_id)){
      annotate_label$run_index[which(annotate_label$plate_id ==idx_plate)] <- annotate_coordinate[[idx_plate]]
    }
    
    for(idx_pc in unique(annotate_label$PC)){
      y_coordinate <- (plot_Val %>%
        filter(PC == idx_pc))[["score"]] %>%
        min()
      y_coordinate_range = (plot_Val %>%
                              filter(PC == idx_pc))[["score"]] %>%
        max() - y_coordinate
      annotate_label$score[which(annotate_label$PC ==idx_pc)] <- (y_coordinate)-abs((y_coordinate_range * 0.1))
    }
    
    
    
    #make control chart plot
    
    PCA_run_order <- ggplotly(
      ggplot(
        data = plot_Val,
        aes(x = run_index, y = score,
            group  = sample_name,
            fill = qc,
            color = qc,
            shape = qc,
            size = qc,
        )) +
        geom_vline(xintercept = plate_boundary, linetype = "dashed") +
        geom_point() +
        theme_bw() +
        theme(axis.line = element_line(color='black'))+
        scale_shape_manual(values = FUNC_OPTION_shape) +
        scale_fill_manual(values = FUNC_OPTION_fill) +
        scale_color_manual(values = FUNC_OPTION_colours) +
        scale_size_manual(values = FUNC_OPTION_size) +
        scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
        #ggtitle(paste0(FUNC_OPTION_title, "; PCA Scores vs Run Order")) +
        ylab("") +
        xlab("") +
        guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=FUNC_OPTION_legend_title)) +
        facet_wrap(facets = "PC", ncol = 2, scales = "free") +
        geom_text(data = annotate_label, aes(label = plate_id), col = "black")
       
    ) %>%
      layout(
        title =  list(
          text = paste0(FUNC_OPTION_title, "; PCA Scores vs Run Order"),
          y = 1.1, 
          x=0.05),
        margin = list(l = 10, r = 10, b=65, t=85),
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.75,
                      y=1.075)           # put legend in center of x-axis
        )
    
  
PCA_run_order
  
}
