# the second set of steps to pre-process files for classification

library(tidyverse)




files.v <- dir(pattern = ".RDS") # load filenames into vector

##
working.df <- readRDS(files.v[1])

### create vectors of col names for morphology of target and parent tokens

z <- working.df$feats %>%
  str_split(., "\\|", simplify = TRUE) # make a matrix of each name-value pair in the "feats" col of the input (e.g., "Case=Nom")

m <- ncol(z) # for loop: number of cols in z

morphs.v <- NULL # vector to store results of loop


for (j in seq_len(m)) { # loop through cols of z
  
  a <- z[, j] %>% 
    str_split(., "=") %>% # split name from value for each pair
    sapply(., magrittr::extract, 1) # extract and keep only names, not values
  
  morphs.v <- c(morphs.v, a) # put results in vector
 
  b <- z[, j] %>% 
    str_split(., "=") %>% # split name from value for each pair
    sapply(., magrittr::extract, 2) # extract and keep only vales, not names
  
 
  
}

morphs.v <-  unique(morphs.v) # remove duplicates

morphs.v <- morphs.v[- which(morphs.v == "")] # remove empty categories
morphs.v[which(is.na(morphs.v))] <- "Not_App" # rename NA: this is not an acceptable name for a col

self_names.v <- paste0("self_", morphs.v) # vector of col names for target word
parent_names_v <- paste0("parent_", morphs.v) # vector of col names for parent of target






###


###
#### loop to populate morpho-syntactic cols


for (i in seq_along(files.v)) { # loop to make separate col for each category of morpho-syntactic data
  
  working.df <- readRDS(files.v[i]) # load file from dir; file should be data frame
  
  # create col with term_id of the parent of each target token
  # ditto for dependency distance of each target token
  
  x <- working.df$sentence_id %>%
    unique() # vector with id number of each sentence
  
  parent_holder.v <- NULL # vector to store result of loop
  dd_holder.v <- NULL # vector to store result of loop
  
  for (n in seq_along(x)) { # loop to create vector of parent term_ids
    
    a <- working.df %>%
      filter(sentence_id == x[n]) # df with rows sentence by sentence
    
    b <- as.numeric(a$head_token_id) # vector with head_token_id for each row in sentence 
    
    b[which(b == 0)] <- NA # eliminate any head_token_id with value 0
    
    dd_holder.v <- c(dd_holder.v, (a$token_id %>%
      as.numeric() ) - b )
    
    parent_holder.v <- c(parent_holder.v,  a$term_id[b] %>%
      as.numeric() ) # add parent term_token_id values for current sentence to vector
    
  }
  
  working.df[, "global_parent_id"] <- parent_holder.v # make and populate parent term_id_col in working.df
  
  
  working.df <- working.df %>%
    mutate(self_POS = upos) # create new col for part of speech of target word (marked with prefix "self")
  
  working.df <- working.df %>%
    mutate(self_rel = dep_rel) # ditto for the dependency relation 
  
  working.df <- working.df %>%
    mutate(self_dd = dd_holder.v) # ditto for dependency distance
  
  working.df[which(working.df$self_dd > 0), "self_arc_dir"] <- "parent_precedes" # ditto for arc direction
  working.df[which(working.df$self_dd < 0), "self_arc_dir"] <- "parent_follows"
  
  
  
 
  
  
  working.df[, self_names.v] <- NA # add "self-" cols for morphology
  
  z <- working.df$feats %>%
    str_split(., "\\|", simplify = TRUE) # make a matrix of each name-value pair in the "feats" col of the input (e.g., "Case=Nom")
  
  z[which(z == "")] <- "Not_App"
  z[is.na(z)] <- "Not_App"
  
  
  
  for (j in seq_len(ncol(z))) {
    
    a <- apply(z[, j, drop=F], 1,   function(x) sub(".*=", "", x)    )
    b <- apply(z[, j, drop=F], 1,   function(x) sub("=.*", "", x)    )
    
    b <- paste0("self_", b)
    
    for (n in seq_along(a)) {
      working.df[n, b[n]] <- a[n]
      
    }
    
    
  }
  
  
  ####### add values of parents to target tokens
  
  x <- working.df$global_parent_id
  
  working.df[, "parent_POS"] <- working.df$self_POS[x]
  working.df[, "parent_rel"] <- working.df$self_rel[x]
  working.df[, "parent_dd"] <- working.df$self_dd[x]
  working.df[, "parent_arc_dir"] <- working.df$self_arc_dir[x]
  
  working.df[, parent_names_v] <-   working.df[x, self_names.v]
  
  fp <- file.path("parsed_expanded", files.v[i]) # create file path for saving
  
  saveRDS(working.df, file = fp)
  print(paste0("completed file ", files.v[i]))
 
  
}
  
 
########



