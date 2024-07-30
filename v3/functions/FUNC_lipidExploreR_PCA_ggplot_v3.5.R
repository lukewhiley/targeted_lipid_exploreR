#ANPC PCA quality control visualisation

#test
#pca scores plot function
#master_list$environment$user_functions$pca <- source(paste0(master_list$project_details$github_master_dir,"/functions/FUNC_lipidExploreR_PCA_ggplot.R"))

lgw_pca <- function(FUNC_data, 
                    FUNC_HEADER_run_order,
                    FUNC_HEADER_plate_id,
                    FUNC_HEADER_colour_by, 
                    FUNC_HEADER_label_by,
                    FUNC_HEADER_highlight_by,
                    FUNC_HEADER_facet,
                    FUNC_OPTION_title,
                    FUNC_OPTION_colours,
                    FUNC_OPTION_fill,
                    FUNC_OPTION_size,
                    FUNC_OPTION_shape,
                    FUNC_OPTION_legend_title){
  
  pca_output <- list()
 
  idx_facet = unique(FUNC_data[[FUNC_HEADER_facet]])[1]
  
  pca_output$pca_model <- list()
  for(idx_facet in unique(FUNC_data[[FUNC_HEADER_facet]])){
  #create data matrix for PCA
  pca_x <- FUNC_data %>% 
    filter(get(FUNC_HEADER_facet) == idx_facet) %>%
    select(-contains("sample")) %>%
    #a NA error occurs when bind peakArea and statTarget datasets due to a mismatch on features that pass filtering. Therefore need to remove columns that are missing data
    select(-any_of(names(which(colSums(is.na(.)) == nrow(.))))) %>%
    as.matrix()
  
  #remove na and infinate
  pca_x[is.na(pca_x)] <- 0 #remove NAs
  pca_x[is.infinite(pca_x)] <- 0 #remove all infinite values

  #log data
  pca_x <- log(pca_x+1) #log values for plotting
  
  #create PCA model
  capture.output(
  pca_output$pca_model[[idx_facet]] <- ropls::opls(
    x = pca_x, 
    y = NULL,
    crossvalI = 1,
    predI = 3,
    algoC = "nipals",
    log10L = FALSE,
    scale = "pareto",
    plotSubC = NA, 
    fig.pdfC = "none",
    subset = NULL)
  )
}
  
  #produce plot_ly PCA scores plot
  # create plot values
  pca_output$plot_Val <- NULL
  for(idx_facet in unique(FUNC_data[[FUNC_HEADER_facet]])){
    pca_output$plot_Val <- bind_rows(
      pca_output$plot_Val,
      pca_output$pca_model[[idx_facet]]@scoreMN %>%
        as_tibble() %>%
        rename(PC1 = p1,
               PC2 = p2,
               PC3 = p3) %>%
        add_column(sample_type = filter(FUNC_data, get(FUNC_HEADER_facet) == idx_facet)[[FUNC_HEADER_colour_by]],
                   sample_name = filter(FUNC_data, get(FUNC_HEADER_facet) == idx_facet)[[FUNC_HEADER_label_by]],
                   #sample_data_source = filter(FUNC_data, get(FUNC_HEADER_facet) == idx_facet)[[FUNC_HEADER_facet]],
                   qc = filter(FUNC_data, get(FUNC_HEADER_facet) == idx_facet)[[FUNC_HEADER_highlight_by]],
                   plate_id = filter(FUNC_data, get(FUNC_HEADER_facet) == idx_facet) [[FUNC_HEADER_plate_id]],
                   run_index = filter(FUNC_data, get(FUNC_HEADER_facet) == idx_facet)[[FUNC_HEADER_run_order]],
                   facet = idx_facet)
    )
  }  
  
  pca_output$plot_Val$facet <- factor(
    pca_output$plot_Val$facet,
    levels= unique(FUNC_data$sample_data_source),
    ordered = T)

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
    guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=FUNC_OPTION_legend_title)) +
    facet_wrap(facets = "facet", scales = "free", ncol = 2, nrow = 2)
  ) %>%
    layout(
      title = list(
        text =paste0("PCA scores; ", FUNC_OPTION_title),
        y = 1.1, 
        x=0.05),
      margin = list(
        l = 10, r = 10, b=65, t=85),
      legend = list(
        orientation = "v",   # show entries horizontally
        xanchor = "center",  # use center of legend as anchor
        y = 0.5,
        x= 1.075)           # put legend in center of x-axis
    )
  
  
  #plot PCA scores vs run order on x
  
  #find plate boundaries for vline
  plate_boundary = 0.5
  annotate_coordinate = NULL
  boundary_finder <- pca_output$plot_Val %>%
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
    
    annotate_coordinate <- c(
      annotate_coordinate,
      median(
        (boundary_finder %>%
           filter(plate_id == idx_plate))[["run_index"]]
      )
    )
  }
  #remove duplicates
  plate_boundary <- unique(plate_boundary)
  
  #finalise annotation co-ordinate
  annotate_coordinate <- setNames(annotate_coordinate, unique(pca_output$plot_Val$plate_id))
  
  #make data.frame
  annotate_label <- tibble(
    facet = rep(unique(pca_output$plot_Val$facet), length(names(annotate_coordinate))) %>% sort(),
    plate_id = rep(names(annotate_coordinate), length(unique(pca_output$plot_Val$facet))),
    run_index = rep(annotate_coordinate, length(unique(pca_output$plot_Val$facet))),
    PC1=NA,
    PC2 =NA,
    PC3=NA
  )
  
  for(idx_facet in unique(annotate_label$facet)){
    annotate_label[["PC1"]][which(annotate_label$facet == idx_facet)] <- pca_output$plot_Val %>% filter(facet == idx_facet) %>% .[["PC1"]] %>% min()
    annotate_label[["PC2"]][which(annotate_label$facet == idx_facet)] <- pca_output$plot_Val %>% filter(facet == idx_facet) %>% .[["PC2"]] %>% min()
    annotate_label[["PC3"]][which(annotate_label$facet == idx_facet)] <- pca_output$plot_Val %>% filter(facet == idx_facet) %>% .[["PC3"]] %>% min()
  }
  
  #make plotly
  pca_output$plot_scores_runOrder <- list()
  for(idx_pc in c("PC1", "PC2", "PC3")){
    pca_output$plot_scores_runOrder[[idx_pc]] <- ggplotly(
      ggplot(
        data = pca_output$plot_Val,
        aes(x = run_index, y = get(idx_pc),
            group  = sample_name,
            fill = sample_type,
            color = qc,
            shape = qc,
            size = qc,
        )) +
      geom_vline(xintercept = plate_boundary, linetype = "dashed") +
      geom_point() +
      theme_bw() +
      scale_shape_manual(values = FUNC_OPTION_shape) +
      scale_fill_manual(values = FUNC_OPTION_fill) +
      scale_color_manual(values = FUNC_OPTION_colours) +
      scale_size_manual(values = FUNC_OPTION_size) +
      ylab(idx_pc) +
      #$ggtitle(paste0("PCA SCORES; ", FUNC_OPTION_title)) +
      guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=FUNC_OPTION_legend_title)) +
      geom_text(inherit.aes = FALSE, data = annotate_label, aes(x = run_index,  y = get(idx_pc), label = plate_id), col = "black") +
      facet_wrap(facets = "facet", 
                 ncol = 1,
                 scales = "free_y") 
    ) %>%
      layout(
        title = list(
          text = paste0("run order (x) vs PCA scores (y) ; ", FUNC_OPTION_title),
          y = 1.1, 
          x=0.05),
        margin = list(
          l = 10, r = 10, b=65, t=85),
        legend = list(
          orientation = "v",   # show entries horizontally
          xanchor = "center",  # use center of legend as anchor
          x = 1.1,
          y= 0.5
        )           # put legend in center of x-axis
      )
  }
  
  
  
  pca_output

}
