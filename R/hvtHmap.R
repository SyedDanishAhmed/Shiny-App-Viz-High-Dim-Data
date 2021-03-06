#' hvtHmap
#' 
#' Heat Map over Hierarchical Voronoi Tessellations
#' 
#' The output of the \code{HVT} function has all the required information about
#' the HVT. Now a heat map is overlayed over this HVT. The user defines the
#' level and also those variables of the data for which the heat map is to be
#' plotted. Now for each variable a separate heat map is plotted. The plot area
#' is divided into 2 screens where the first screen is relatively large and
#' will have the heat map. The second screen is small and contains the gradient
#' scale. To plot the heat map, the data is first normalized. The gradient
#' scale is divided into 'n' regions(500 is the set default). Using the
#' normalized data, the different regions into which the data items fall are
#' found. Each data item is now having a region on the gradient scale. This
#' color is filled in the tile corresponding to the data item. This procedure
#' is done for all the tiles for that level to get the complete heat map. Once
#' the heat map is ready, the higher level tessellations are plotted to
#' represent the hierarchies. The size of the centroids, the thickness of the
#' lines and the color of the tessellation lines can be given as input by the
#' user. Appropriate values for these parameters should be given to identify
#' the hierarchies properly. In the second screen the gradient scale is
#' plotted. The heat maps and hierarchical tessellations are obtained for all
#' the desired variables.
#' 
#' @param hvt.results List. A list of hvt.results obtained from the HVT
#' function.
#' @param dataset Data frame. The input data set.
#' @param child.level Numeric. Indicating the level for which the heat map is
#' to be plotted.
#' @param hmap.cols Numeric or Character. The column number of column name from
#' the dataset indicating the variables for which the heat map is to be
#' plotted.
#' @param color.vec Vector. A color vector such that length(color.vec) =
#' (child.level - 1). (default = NULL)
#' @param line.width Vector. A line width vector such that length(line.width) =
#' (child.level - 1). (default = NULL)
#' @param centroid.size Numeric. Indicating the centroid size of the first
#' level. (default = 3)
#' @param pch Numeric. Indicating the centroid's symbol type.
#' (default = 21)
#' @param palette.color Numeric. Indicating the heat map color palette. 1 -
#' rainbow, 2 - heat.colors, 3 - terrain.colors, 4 - topo.colors, 5 -
#' cm.colors, 6 - seas color. (default = 6)
#' @param previous_level_heatmap Logical. If TRUE, the heatmap of previous level will be overlayed on the heatmap of selected level. If #' FALSE, the heatmap of only selected level will be plotted 
#' @param show.points Logical. Indicating if the centroids should
#' be plotted on the tesselations. (default = FALSE)
#' @param asp Numeric. Indicating the aspect ratio type. For flexible aspect
#' ratio set, asp = NA. (default = 1)
#' @param ask Logical. If TRUE (and the R session is interactive) the user is
#' asked for input, before a new figure is drawn. (default = TRUE)
#' @param tess.label Vector. A vector for labelling the tessellations. (default
#' = NULL)
#' @param label.size Numeric. The size by which the tessellation labels should
#' be scaled. (default = 0.5)
#' @param ... The ellipsis is passed to it as additional argument. (Used internally)
#' @author Meet K. Dave <dave.kirankumar@@mu-sigma.com>
#' @seealso \code{\link{plotHVT}}
#' @keywords hplot
#' @importFrom magrittr %>%
#' @examples
#' data(USArrests)
#' hvt.results <- list()
#' hvt.results <- HVT(USArrests, nclust = 6, depth = 1, quant.err = 0.2, 
#'                   projection.scale = 10, normalize = TRUE)
#' hvtHmap(hvt.results, USArrests, child.level = 1,hmap.cols = 'Murder', line.width = c(0.2),
#' color.vec = c('#141B41'),palette.color = 6)
#'
#' hvt.results <- list()
#' hvt.results <- HVT(USArrests, nclust = 3, depth = 3, quant.err = 0.2, 
#'                   projection.scale = 10, normalize = TRUE)
#' hvtHmap(hvt.results, train_computers, child.level = 3,hmap.cols = 'quant_error', 
#' line.width = c(1.2,0.8,0.4),color.vec = c('#141B41','#0582CA','#8BA0B4'),palette.color = 6)
#' @export hvtHmap
hvtHmap <-
function (hvt.results, dataset, child.level, hmap.cols, color.vec = NULL, line.width = NULL, centroid.size = 3, pch = 21, palette.color = 6,previous_level_heatmap = T, show.points = F, asp = 1, ask = T, tess.label = NULL, label.size = .5,...)
  {
    requireNamespace("MASS")
    requireNamespace("deldir")
    requireNamespace("dplyr")
    #require(gtools)
    #require(seas)
  #  require(futile.logger)
    options(warn = -1)

    
    
    #select child level data
    if(child.level > 1){
      parlevel = child.level - 1
    }else{
      parlevel = 1
    }
    
    tess_results <- hvt.results[[1]]
    polinfo <- hvt.results[[2]]
    hvq_k <- hvt.results[[3]]
    hvqdata <- hvq_k$summary
    
    if(length(color.vec) == length(line.width) && (length(line.width)) == child.level){
      
      # flog.info("Lengths of color vector and line width vector is equal to the number of parent levels")
      #select the data only for the user-defined level
      # hvqztab <- hvqdata[which(hvqdata[, 1] == child.level), ]
      # ncolumns <- ncol(hvqdata)
      # 
      # 
      # 
      # # select only the input columns
      # if (class(hmap.cols) == "character") {
      #   
      #   if(length(list(...))){
      #     
      #     gradient_data <- hvqztab[,hmap.cols,drop=F]
      #     get_indices_for_NA <- is.na(gradient_data)
      #     if (any(get_indices_for_NA)) {
      #       gradient_data[get_indices_for_NA, 1] <- 0
      #     }
      #     
      #   }
      #   
      #   
      #   else{
      #   
      #   if (hmap.cols == "quant_error") {
      #     gradient_data <- hvqztab[, 5, drop = F]
      #     gradient_data <- gradient_data[complete.cases(hvqztab),,drop=F]
      #     # get_indices_for_NA <- is.na(gradient_data)
      #     # if (any(get_indices_for_NA)) {
      #     #   gradient_data[get_indices_for_NA, 1] <- 0
      #     # }
      #   }
      #   
      #   else if (hmap.cols == "no_of_points") {
      #     gradient_data <- hvqztab[, 4, drop = F]
      #     gradient_data <- gradient_data[complete.cases(hvqztab),,drop=F]
      #     # get_indices_for_NA <- is.na(gradient_data)
      #     # if (any(get_indices_for_NA)) {
      #     #   gradient_data[get_indices_for_NA, 1] <- 0
      #     # }
      #   }
      #     
      #     
      #   
      #   else{
      #     hmap.cols = which(colnames(dataset) == hmap.cols)
      #     if (length(hmap.cols) == 0) {
      #       stop("Column name for plotting heatmap incorrect")
      #       }
      # 
      #   }
      #   }
      # }
      # 
      # if (is.numeric(hmap.cols)) {
      #   column_no_for_hmap = hmap.cols
      #   
      #   if (length(column_no_for_hmap) == 0) {
      #     stop("Column name for plotting heatmap incorrect")
      #   }
      #   
      #   ## Get row index for all clusters in the asked child level
      #   row_index_clusters = hvq_k$idnodes[[child.level]]
      #   # Remove NULL
      #   row_index_clusters <- Filter(Negate(is.null),row_index_clusters)
      #   
      #   depth <- 2
      #   
      #   if(child.level==1){
      #     depth <- 1
      #   }
      #   
      #   gradient_data <-
      #     data.frame(unlist(
      #       purrr::modify_depth(row_index_clusters, depth,  ~ colMeans(dataset[as.vector(.x[, 1]), column_no_for_hmap,drop=F]))
      #     ))
      #   colnames(gradient_data) <-
      #     colnames(dataset[, column_no_for_hmap, drop = F])
      # }
      # 
      # 
      # #store the column names
      # grad_scale <- gradient_data
      gtitles <- 1
      #different color palette
      pal.col <- c("rainbow(500, start = .7, end = .1)", "heat.colors(500)", 
                   "terrain.colors(500)", "topo.colors(500)", "cm.colors(500)", 
                   "colorRampPalette(c(crp1,crp2,crp3,crp4,crp5))(500)","RColorBrewer::brewer.pal(n,name)")
      
      
      # #select the five colors for two color gradient heat map
      # crp1 <- "#0000FF"
      # crp2 <- "#00FFFF"
      # crp3 <- "#00FF00"
      # crp4 <- "#FFFF00"
      # crp5 <- "#FF0000"
      # 
      
      
      #for each variable in the hvqdata
      for(i in 1: length(gtitles)){
        #graphics::close.screen(all = T)
        #tiles information of user-defined level. It is the output of tile.list.
        #pdat <- polinfo[[child.level]]
        #gradient of colors is divided into n colors
        n <- 500

        plot_gg <- ggplot2::ggplot() + ggplot2::theme_bw() +  ggplot2::theme(
                                   plot.background = ggplot2::element_blank()
                                  ,panel.grid.major = ggplot2::element_blank()
                                  ,panel.grid.minor = ggplot2::element_blank())
        #gradient_values = data.frame(grad_scale[,i,drop=FALSE])
        gradient_palette = pal.col[palette.color]
        
        #call the function which plots the heat map for the user-defined level
        if(previous_level_heatmap==T){
          start.level = 1
        }
        else{
          start.level = child.level
        }
        
        for(current.child.level in start.level:child.level){
          
          
          hvqztab <- hvqdata[which(hvqdata[, 1] == current.child.level), ]
          ncolumns <- ncol(hvqdata)
          
          
          
          # select only the input columns
          if (class(hmap.cols) == "character") {
            
            if(hmap.cols %in% colnames(hvqztab)){
              
              gradient_data <- hvqztab[,hmap.cols,drop=F]
              get_indices_for_NA <- is.na(gradient_data)
              if (any(get_indices_for_NA)) {
                gradient_data[get_indices_for_NA, 1] <- 0
              }
              
            }
            
            
            else{
              
              if (hmap.cols == "quant_error") {
                # gradient_data <- hvqztab[, 5, drop = F]
                # gradient_data <- gradient_data[complete.cases(gradient_data),,drop=F]
                gradient_data <- hvqztab[,5,drop=F]
                get_indices_for_NA <- is.na(gradient_data)
                if (any(get_indices_for_NA)) {
                  gradient_data[get_indices_for_NA, 1] <- 0
                }
                # get_indices_for_NA <- is.na(gradient_data)
                # if (any(get_indices_for_NA)) {
                #   gradient_data[get_indices_for_NA, 1] <- 0
                # }
              }
              
              else if (hmap.cols == "no_of_points") {
                # gradient_data <- hvqztab[, 4, drop = F]
                # gradient_data <- gradient_data[complete.cases(gradient_data),,drop=F]
                gradient_data <- hvqztab[,4,drop=F]
                get_indices_for_NA <- is.na(gradient_data)
                if (any(get_indices_for_NA)) {
                  gradient_data[get_indices_for_NA, 1] <- 0
                }
                # get_indices_for_NA <- is.na(gradient_data)
                # if (any(get_indices_for_NA)) {
                #   gradient_data[get_indices_for_NA, 1] <- 0
                # }
              }
              
              
              
              else{
                hmap.cols = which(colnames(dataset) == hmap.cols)
                if (length(hmap.cols) == 0) {
                  stop("Column name for plotting heatmap incorrect")
                }
                
              }
            }
          }
          
          if (is.numeric(hmap.cols)) {
            column_no_for_hmap = hmap.cols
            
            if (length(column_no_for_hmap) == 0) {
              stop("Column name for plotting heatmap incorrect")
            }
            
            ## Get row index for all clusters in the asked child level
            row_index_clusters = hvq_k$idnodes[[current.child.level]]
            # Remove NULL
            row_index_clusters <- Filter(Negate(is.null),row_index_clusters)
            
            depth <- 2
            
            if(current.child.level==1){
              depth <- 1
            }
            
            gradient_data <-
              data.frame(unlist(
                purrr::modify_depth(row_index_clusters, depth,  ~ colMeans(dataset[as.vector(.x[, 1]), column_no_for_hmap,drop=F]))
              ))
            colnames(gradient_data) <-
              colnames(dataset[, column_no_for_hmap, drop = F])
          }
          
          
          #store the column names
          grad_scale <- gradient_data
        
        pdat <- polinfo[[current.child.level]]  
          
        plot_gg <- ggplotTileHmap(plot_gg,pdat,grad_scale,ptext = tess.label, polycol = gradient_palette, 
                      close = T, showpoints = show.points,  
                      lnwid = (line.width[1] / child.level),
                      frame.plot = F, xlab = "", ylab = "", 
                      asp = asp, label.size = label.size, 
                      pointmag = (centroid.size / child.level),pch=pch)
        }
        
        #plot the centroids for parent levels
        plot_gg <- ggplotTessHmap(plot_gg,hvt.results, line.width = line.width, color.vec = color.vec,pch=pch,child.level=child.level,show.points = show.points,centroid.size = centroid.size)
        
        #plot the polygons of the parent levels
        # for(lev in parlevel: 1){   
        #   len <- length(polinfo[[lev]])
        #   for(ind1 in 1: len){
        #     for(ind2 in 1: length(polinfo[[lev]][[ind1]])){
        #      # graphics::polygon(polinfo[[lev]][[ind1]][[ind2]]$x, polinfo[[lev]][[ind1]][[ind2]]$y, 
        #               #lwd = line.width[lev], border = color.vec[lev])
        #       df_pol <- data.frame(x=polinfo[[lev]][[ind1]][[ind2]]$x,y=polinfo[[lev]][[ind1]][[ind2]]$y)
        #       plot_gg <- plot_gg + ggplot2::geom_polygon(data = df_pol,mapping = ggplot2::aes_string(x="x",y="y"),size=line.width[lev],colour = color.vec[lev],fill=NA)
        #     }
        #   }
        #   # flog.debug("Polygons for Level %s are drawn", lev)
        # }
  
        return(suppressMessages(plot_gg))
      }
    }else{
      return("Length of color vector and line width vector should be equal to child level")
    }
  }
