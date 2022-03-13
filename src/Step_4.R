# code to populate matrix with selected var value pairs
rm(list = ls())
library(tidyverse)
library(magrittr)

# before running the loop, the selected variable combinations must be retrieved

# these data are stored at C:\Users\rgorm\Documents\non-AGLDT_trees\selected_var_val_pairs


simplex_vars.tib <- readRDS(file = "simplex_5pc.RDS")
two_tuple_vars.tib <- readRDS(file = "two_plex_5pc.RDS")
three_tuple_vars.tib <- readRDS(file = "three_plex_5pc.RDS")



##############

simplex_comb.l <- simplex_vars.tib$combined %>% #split variable and value and store in list object
  str_split("_Is_")


# split variable and value and store in list object
# there is one vector per list element
# the first element of each vector contains var name, the second contains var value

two_comb.l <- two_tuple_vars.tib$combined %>%
  str_split("_Are_")

# split the var name of each list element into component parts and store in new list
# each list element contains a vector with 2 elements
two_var.l <- sapply(two_comb.l, extract, 1) %>% # extract() needs package magrittr
  str_split("_And_")

# split the value name of each list element into component parts and store in  new list
# each list element contains a vector with 2 elements
two_val.l <- sapply(two_comb.l, extract, 2) %>%
  str_split("_And_")

# split variable and value and store in list object
# there is one vector per list element
# the first element of each vector contains var name, the second contains var value
three_comb.l <- three_tuple_vars.tib$combined %>%
  str_split("_Are_")

# split the var name of each list element into component parts and store in new list
# each list element contains a vector with 3 elements
three_var.l <- sapply(three_comb.l, extract, 1) %>%
  str_split("_And_")

# split the value name of each list element into component parts and store in  new list
# each list element contains a vector with 3 elements
three_val.l <- sapply(three_comb.l, extract, 2) %>%
  str_split("_And_")




#######################

# loop through each element in files.v and create one-hot columns for the selected combined var-value pairs
# input files should have self_ and parent_ variables for each token.

## change to working dir
files.v <- files.v <- dir( pattern=".*RDS")



for (i in seq_along(files.v)) {
  
  working.df <- readRDS(file = files.v[i])
  
  file_name <- gsub(".RDS", "", files.v[i]) # add file name to each row of df
  
  working.df  <-  cbind(file_name, working.df)
  # new.v <- gsub("-", "_", colnames(working.df)) # change hyphen to underscore in col names
  # colnames(working.df) <- new.v
  
  target.df <- working.df # create new df
  
  # add columns named according to selected var-val pairs; select name of input tibble
  # target.df[, all_vars.tib$combined] <- NA
  
  target.df[, simplex_vars.tib$combined ] <- NA
  target.df[, two_tuple_vars.tib$combined ] <- NA
  target.df[, three_tuple_vars.tib$combined ] <- NA
  
  # a loop for simplex var-val pairs
 
  
  for (j in seq_along(simplex_comb.l)) {
    
    
    target.df[which(working.df[, simplex_comb.l[[j]][1]  ]  == simplex_comb.l[[j]][2]   ), 
              simplex_vars.tib$combined[j]   ] <- 1
    # which() identifies rows in target in which var column of working.df (working.df[, simplex_comb.l[[j]][1]  ])
    # has value required (simplex_comb.l[[j]][2] ); such cells are given value 1
    
    target.df[which(is.na(target.df[, simplex_vars.tib$combined[j] ])), simplex_vars.tib$combined[j]  ] <- 0
    # all other cells are given value 0
  }
  
  
  
  
  for (j in seq_along(two_var.l)) {
    
   
    target.df[which(working.df[, two_var.l[[j]][1] ] == two_val.l[[j]][1] &  working.df[, two_var.l[[j]][2] ] 
                    == two_val.l[[j]][2]),           two_tuple_vars.tib$combined[j]] <- 1
    # the same process as in j loop above except that it identifies where 2 columns in working.df
    # have the two values inticated in two_val.l.
    
    target.df[which(is.na(target.df[, two_tuple_vars.tib$combined[j] ])), two_tuple_vars.tib$combined[j]  ] <- 0
    
    
  }
  
  
  
  for (j in seq_along(three_var.l)) {
    
   
    
    target.df[which(working.df[, three_var.l[[j]][1] ]  == three_val.l[[j]][1]  
                    & working.df[, three_var.l[[j]][2] ]  == three_val.l[[j]][2] 
                    & working.df[, three_var.l[[j]][3] ]  == three_val.l[[j]][3] 
    )      , three_tuple_vars.tib$combined[j]    ]  <- 1
    
    # the same process as in j loop above except that it identifies where 3 columns in working.df
    # have the 3 values inticated in three_val.l.
    
    target.df[which(is.na(target.df[, three_tuple_vars.tib$combined[j] ])), three_tuple_vars.tib$combined[j]  ] <- 0
    
  }
  
  file_name <- paste0(file_name,   "_binary_val",  ".RDS") # add file type to file name for saving
  
  # ! make sub directory in current working directory called "binary_valued." Otherwise the following lines will not work!
  fp <- file.path("binary_valued", file_name) # file path for save
  
  saveRDS(target.df, file = fp) # save one-hot file to disk
  
  print(paste("finished with file ", i))
  
}





