getOptimalCentroids <-
  function (x, iter.max,algorithm, nclust,distance_metric = c("L1_Norm","L2_Norm"),error_metric = c("mean","max"),quant.err){
    requireNamespace("dplyr")
    
    # Start with splitting data into two clusters
    nclust_iter <- 3
    outkinit <- list(centers = numeric(), values = logical() , nsize = numeric())
    quantok <- rep(T, nclust)
    
    while (nclust_iter <= nclust & nrow(x) > nclust_iter & (sum(quantok,na.rm = T) > 0)) {
      # print(nclust_iter)
      resplt <- list()
      # if(nrow(x)<3){
      #   nclust_iter = 0
      #   break
      # }
       
      #outkinit will have centroids and datapoints and size of the cluster
      set.seed(100)
      outkinit <- getCentroids(x, kout = stats::kmeans(x, nclust_iter, iter.max=100, algorithm=algorithm), nclust_iter,distance_metric=distance_metric,error_metric=error_metric)
      
      #flag to check for quantization error
      resplt <- unlist(outkinit$cent) > quant.err
      quantok <- unlist(resplt)
      # print(max( unlist(outkinit$cent) ))
      # print(resplt)
      # if(nclust<=nclust_iter | NROW(x) - 1 <= nclust_iter | sum(quantok) == 0 ){
      #   break
      # }
      nclust_iter <- nclust_iter + 1      
    }
    # print(paste0("Optimal cluster: ",nclust_iter - 1))
    if(nrow(x) <= 3) {
      dummy_iter = nclust
    } else {
      dummy_iter = nclust - nclust_iter + 1  
    }
    outkinit[["centers"]] <- c(outkinit[["centers"]],as.list(rep(NA,dummy_iter)))
    outkinit[["values"]] <- c(outkinit[["values"]],as.list(rep(NA,dummy_iter)))
    outkinit[["nsize"]] <- c(outkinit[["nsize"]],as.list(rep(0,dummy_iter)))
    
    #return centroids, datapoints and size of each cluster
    return(outkinit)
  }