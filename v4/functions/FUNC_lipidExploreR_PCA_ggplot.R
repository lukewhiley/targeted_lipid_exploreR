#ANPC PCA quality control visualisation

#test
#pca scores plot function
#master_list$environment$user_functions$pca <- source(paste0(master_list$project_details$github_master_dir,"/functions/FUNC_lipidExploreR_PCA_ggplot.R"))

lgw_pca <- function(FUNC_data, 
                    FUNC_metabolite_list, 
                    FUNC_HEADER_run_order,
                    FUNC_HEADER_plate_id,
                    FUNC_HEADER_colour_by, 
                    FUNC_HEADER_label_by,
                    FUNC_HEADER_highlight_by,
                    FUNC_OPTION_title,
                    FUNC_OPTION_colours,
                    FUNC_OPTION_fill,
                    FUNC_OPTION_size,
                    FUNC_OPTION_shape,
                    FUNC_OPTION_legend_title){
  
  pca_output <- list()
 
  #create data matrix for PCA
  pca_x <- FUNC_data %>%  select(all_of(FUNC_metabolite_list)) %>% as.matrix() 
  #remove na and infinate
  pca_x[is.na(pca_x)] <- 0 #remove NAs
  pca_x[is.infinite(pca_x)] <- 0 #remove all infinite values

  #log data
  pca_x <- log(pca_x+1) #log values for plotting
  
  #create PCA model
  pca_output$pca_model <- mva.plots::PCA(data = pca_x,
                                         center = TRUE,
                                         rank = 3,
                                         scale. = TRUE,
                                         plot = FALSE)

  #produce plot_ly PCA scores plot
  # create plot values
  pca_output$plot_Val <- pca_output$pca_model$data$scores %>%
    as_tibble() %>%
    add_column(sample_type = FUNC_data[[FUNC_HEADER_colour_by]],
               sample_name = FUNC_data[[FUNC_HEADER_label_by]],
               qc = FUNC_data[[FUNC_HEADER_highlight_by]],
               plate_id = FUNC_data [[FUNC_HEADER_plate_id]],
               run_index = FUNC_data[[FUNC_HEADER_run_order]]
               )
  

#make plotly scores
  pca_output$plot_scores <- ggplotly(
    ggplot(
      data = pca_output$plot_Val,
      aes(x = PC1, y = PC2,
          group  = sample_name,
          fill = sample_type,
          color = qc,
          shape = qc,
          size = qc,
          )) +
      geom_vline(xintercept = 0, colour = "darkgrey") +
      geom_hline(yintercept = 0, color = "darkgrey")+
      geom_point() +
      theme_bw() +
      scale_shape_manual(values = FUNC_OPTION_shape) +
      scale_fill_manual(values = FUNC_OPTION_fill) +
      scale_color_manual(values = FUNC_OPTION_colours) +
      scale_size_manual(values = FUNC_OPTION_size) +
      #ggtitle(paste0("PCA SCORES; ", FUNC_OPTION_title)) +
      guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=FUNC_OPTION_legend_title))
  )  %>%
    layout(
      title = list(
        text =paste0(FUNC_OPTION_title, "; PCA SCORES PLOT"),
        y = 1.05, 
        x=0.5),
      margin = list(
        l = 10, r = 10, b=65, t=85),
      legend = list(
        orientation = "h",   # show entries horizontally
        xanchor = "center",  # use center of legend as anchor
        x = 0.5,
        y=1.075)           # put legend in center of x-axis
    )
  
  
  #plot PCA scores vs run order on x
  pca_output$plot_scores_runOrder <- ggplotly(
    ggplot(
      data = pca_output$plot_Val %>%
        pivot_longer(
          cols = starts_with("PC"),
          names_to = "PC",
          values_to = "score"
        ),
      aes(x = run_index, y = score,
          group  = sample_name,
          fill = sample_type,
          color = qc,
          shape = qc,
          size = qc,
      )) +
      geom_point() +
      theme_bw() +
      scale_shape_manual(values = FUNC_OPTION_shape) +
      scale_fill_manual(values = FUNC_OPTION_fill) +
      scale_color_manual(values = FUNC_OPTION_colours) +
      scale_size_manual(values = FUNC_OPTION_size) +
      #$ggtitle(paste0("PCA SCORES; ", FUNC_OPTION_title)) +
      guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=FUNC_OPTION_legend_title)) +
      facet_wrap(facets = "PC", ncol = 1, scales = "free")
  ) %>%
    layout(
      title = list(
        text = paste0("PCA SCORES vs RUN ORDER; ", FUNC_OPTION_title),
        y = 1.05, 
        x=0.05),
      margin = list(
        l = 10, r = 10, b=65, t=85),
      legend = list(
        orientation = "h",   # show entries horizontally
        xanchor = "center",  # use center of legend as anchor
        x = 0.75,
        y=1.075)           # put legend in center of x-axis
    )
  
  
  
  pca_output

}
