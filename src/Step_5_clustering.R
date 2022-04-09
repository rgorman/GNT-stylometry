# a script to cluster Files

require(cluster)
require(tidyverse)
require(stylo)


# cluster whole files
files.v <- dir(pattern = ".RDS") # change to directory containing binary valued files

keeper.l <- vector(mode = "list", length(files.v))

for (i in seq_along(files.v)) {
  
  working.df <- readRDS(file = files.v[i])
  
  # select cols with chosen variables
  
  x <- str_detect(colnames(working.df), "_Is_")
  y <- str_detect(colnames(working.df), "_Are_")
  x <- which(x == TRUE)
  y <- which(y == TRUE)
  z <- c(x, y)
  
  working.df <-   working.df[, z]
  
  a <- working.df %>%
    colMeans() # get average for each variable col
  
  a <- a*100 # multiply average for readability (very small numbers are hard to read quickly)
  
  keeper.l[[i]] <- a
 
  
}

names(keeper.l) <- files.v %>%
  gsub("_.*", "", .)


result.df <- do.call(bind_rows, keeper.l) # combine list object into data frame

rownames(result.df) <- names(keeper.l)

result.cluster <- agnes(result.df, diss = FALSE, method = "ward") # perform cluster aggregative analysis; see package documentation for other clustering methods

as.dendrogram(result.cluster) %>% # plot for visual examination
  plot()

gnt.dist <- dist(result.df) # a distance object

gnt.dist <- as.matrix(gnt.dist) # for easy visual examination and saving


b <- gnt.dist[, 16] # vector of distances of Hebrews to all other files
sort(b)

gnt.dist <- dist.cosine(as.matrix(result.df)) # cosine distance is popular in NLP
gnt.dist <- as.matrix(gnt.dist)
b <- gnt.dist[, 16]
sort(b)
