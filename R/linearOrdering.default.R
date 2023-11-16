linearOrdering.default <-
function(names, stimulants, ..., method = "standsum") {
  #Function performing linear ordering using one of three methods: Standardized Sums Method, Hellwig's Method, Topsis Method 
  #Args: 
    #names: list (column of data with names of objects to be ordered), mandatory
    #stimulants: list (column/s of data with values classified as stimulants), mandatory
    #destimulants: list (column/s of data with values classified as destimulants), optional
    #nominants: list (column/s of data with values classified as nominants), optional
    #optimals: numeric vector (vector with optimal values for data classified as nominants), optional, mandatory if any nominant is provided, there should be as many values provided as nominants
    #method: character (method used for linear ordering), three possible values: "standsum" (Standardized Sums Method), "hellwig" (Hellwig's Method), "topsis" (Topsis Method), default value for argument is "standsum"
  #Returns: rank_object: list (custom list containing call method to function, data frame with ordered items (Name, Result), method used for linear ordering) 
  
  #Checking if data has specific structure
  stopifnot(is.list(names))
  stopifnot(is.list(stimulants))
  stopifnot(is_numeric_values(stimulants))
  
  #Checking if optional parameters were provided
  if (!missing(...)) {
    # If additional parameters are provided, extract their value
    additional_parameters <- list(...)
    if(length(additional_parameters) == 1)
    {
      #Converting Destimulants to Stimulants
      destimulants <- data.frame(additional_parameters[1])
      stopifnot(is_numeric_values(destimulants))
      for(i in 1:length(destimulants)) {
        destimulants[i] = -destimulants[i]
      }
      stimulants <- cbind(stimulants,destimulants)
    }
    else if(length(additional_parameters) == 2)
    {
      stop("Nominants cannot be provided without providing optimal value")
    }
    else if(length(additional_parameters) == 3){
      
      #Converting Destimulants and Nominats to Stimulants
      destimulants <- data.frame(additional_parameters[1])
      stopifnot(is_numeric_values(destimulants))
      nominants <- data.frame(additional_parameters[2])
      stopifnot(is_numeric_values(nominants))
      optimals <- data.frame(additional_parameters[3])
      stopifnot(is_numeric_values(optimals))
      
      for(i in 1:length(destimulants)) {
        destimulants[i] = -destimulants[i]
      }
      stimulants <- cbind(stimulants,destimulants)
      
      for(j in 1:length(nominants)) {
        opt <- optimals[j,1]
        for(k in 1:length(nominants[,j]))
        {
          if(nominants[k,j] == opt) {
            nominants[k,j] <- 1
          }
          else if(nominants[k,j]  < opt) {
            nominants[k,j] <- -1/(nominants[k,j] - opt - 1)
          }
          else {
            nominants[k,j] <- 1/(nominants[k,j] - opt + 1)
          }
        }
      }
      stimulants <- cbind(stimulants,nominants)
      
    }
    else {
      stop("Too many parameters given")
    }
  } else {
    print("Data set does not contain destimulants and nominants")
  }
  
  
  #Standarization
  data_stand <- standarize_data_frame(stimulants)
  data_stand <- cbind(names,data_stand)
  
  #Linear ordeing - method
  if(method == "standsum")
  {
    sum <- c(1:length(data_stand[,1]))
    pointer <- c(1:length(data_stand[,1]))
    data_stand_sum <- cbind(data_stand,sum)
    data_stand_sum <- cbind(data_stand_sum,pointer)
    
    data_stand_sum["sum"] <- 0
    data_stand_sum["pointer"] <- 0
    
    data_stand_sum["sum"] <- apply(data_stand_sum[ ,-1], 1, sum)
    min_si <- min(data_stand_sum["sum"])
    max_si <- max(data_stand_sum["sum"]-min_si)
    
    #Standarization
    for(i in 1:length(data_stand_sum[,1])) {
      data_stand_sum$pointer[i] <- (data_stand_sum$sum[i]-min_si)/max_si
    }
    
    #Sorting
    data_stand_sum <- data_stand_sum[order(-data_stand_sum$pointer),]
    rank <- cbind(data_stand_sum[,1], data_stand_sum$pointer)
    rank <- as.data.frame(rank)
    colnames(rank) <- c("Name", "Result")

  }
  else if(method == "hellwig")
  {
    max_values <- sapply(data_stand[-1], max)
    distances_hellwig <- data_stand
    for(i in 1:length(max_values)) {
      for(j in 1:length(distances_hellwig[,i+1])) {
        distances_hellwig[j,i+1] <- (data_stand[j,i+1]-max_values[i])^2
      }
    }
    dist <- c(1:length(data_stand[,1]))
    distances_hellwig <- cbind(distances_hellwig,dist)
    distances_hellwig["distance"] <- 0
    for(i in 1:length(distances_hellwig$distance)) {
      for(j in 1:length(max_values)) {
        distances_hellwig$distance[i] <- distances_hellwig$distance[i]+distances_hellwig[i,j+1]
      }
      distances_hellwig$distance[i] <- sqrt(distances_hellwig$distance[i])
    }

    d0 <- mean(distances_hellwig$distance) + 2*sd(distances_hellwig$distance)
    
    d_helw <- data.frame(Name=distances_hellwig$name, meassure=0)
    
    for(i in 1:length(d_helw$meassure)) {
      d_helw$meassure[i] <- 1-(distances_hellwig$distance[i]/d0)
    }
    
    #Sorting
    rank <- d_helw[order(-d_helw$meassure),]
    colnames(rank) <- c("Name", "Result")
    
  }
  else if(method == "topsis")
  {
    min_values <- sapply(data_stand[-1], min)
    distances_topsis <- data_stand
    for(i in 1:length(min_values)) {
      for(j in 1:length(distances_topsis[,i+1])) {
        distances_topsis[j,i+1] <- (data_stand[j,i+1]-min_values[i])^2
      }
    }
    dist <- c(1:length(data_stand[,1]))
    distances_topsis <- cbind(distances_topsis,dist)
    distances_topsis["distance"] <- 0
    for(i in 1:length(distances_topsis$distance)) {
      for(j in 1:length(min_values)) {
        distances_topsis$distance[i] <- distances_topsis$distance[i]+distances_topsis[i,j+1]
      }
      distances_topsis$distance[i] <- sqrt(distances_topsis$distance[i])
    }
    
    #Additional part taken from hellwig method
    max_values <- sapply(data_stand[-1], max)
    distances_hellwig <- data_stand
    for(i in 1:length(max_values)) {
      for(j in 1:length(distances_hellwig[,i+1])) {
        distances_hellwig[j,i+1] <- (data_stand[j,i+1]-max_values[i])^2
      }
    }
    dist <- c(1:length(data_stand[,1]))
    distances_hellwig <- cbind(distances_hellwig,dist)
    distances_hellwig["distance"] <- 0
    for(i in 1:length(distances_hellwig$distance)) {
      for(j in 1:length(max_values)) {
        distances_hellwig$distance[i] <- distances_hellwig$distance[i]+distances_hellwig[i,j+1]
      }
      distances_hellwig$distance[i] <- sqrt(distances_hellwig$distance[i])
    }
    d_topsis <- data.frame(Name=distances_topsis$name, meassure=0)
    
    for(i in 1:length(d_topsis$meassure)) {
      d_topsis$meassure[i] <- distances_topsis$distance[i]/(distances_topsis$distance[i]+(distances_hellwig$distance[i]))
    }
    
    #Sorting
    rank <- d_topsis[order(-d_topsis$meassure),]
    colnames(rank) <- c("Name", "Result")
  
  }
  else {
    stop("Given method do not exist for this function")
  }
  
  row.names(rank) <- NULL #index reset
  
  rank_object <-list(call=match.call(),rank=rank, method=method)
  class(rank_object)<-"linearOrdering.default"
  return(rank_object)
  
}
