## script to populate feature columns as chosen in previous script

require(tidyverse)
require(magrittr)



simplex_vars.tib <- readRDS(file = "simplex_2_5pc.RDS")
two_tuple_vars.tib <- readRDS(file = "two_plex_2_5pc.RDS")
three_tuple_vars.tib <- readRDS(file = "three_plex_2_5pc.RDS")

simplex_comb.l <- simplex_vars.tib$combined %>% #split variable and value and store in list object
  str_split("_ISA_")


# split variable and value and store in list object
# there is one vector per list element
# the first element of each vector contains var name, the second contains var value

two_comb.l <- two_tuple_vars.tib$combined %>%
  str_split("_ISA_")

# split the var name of each list element into component parts and store in new list
# each list element contains a vector with 2 elements
two_var.l <- sapply(two_comb.l, extract, 1) %>% # extract() needs package magrittr
  str_split("_AND_")

# split the value name of each list element into component parts and store in  new list
# each list element contains a vector with 2 elements
two_val.l <- sapply(two_comb.l, extract, 2) %>%
  str_split("_AND_")

# split variable and value and store in list object
# there is one vector per list element
# the first element of each vector contains var name, the second contains var value
three_comb.l <- three_tuple_vars.tib$combined %>%
  str_split("_ISA_")

# split the var name of each list element into component parts and store in new list
# each list element contains a vector with 3 elements
three_var.l <- sapply(three_comb.l, extract, 1) %>%
  str_split("_AND_")

# split the value name of each list element into component parts and store in  new list
# each list element contains a vector with 3 elements
three_val.l <- sapply(three_comb.l, extract, 2) %>%
  str_split("_AND_")


#access GNT_DD (or whatever you have called )

# add columns for data
GNT_DD[, simplex_vars.tib$combined ] <- NA
GNT_DD[, two_tuple_vars.tib$combined ] <- NA
GNT_DD[, three_tuple_vars.tib$combined ] <- NA

## loops to populate columns with values

for (j in seq_along(simplex_comb.l)) {
  
  
  GNT_DD[which(GNT_DD[, simplex_comb.l[[j]][1]  ]  == simplex_comb.l[[j]][2]   ), 
            simplex_vars.tib$combined[j]   ] <- 1
  # which() identifies rows in target in which var column of working.df (working.df[, simplex_comb.l[[j]][1]  ])
  # has value required (simplex_comb.l[[j]][2] ); such cells are given value 1
  
  GNT_DD[which(is.na(GNT_DD[, simplex_vars.tib$combined[j] ])), simplex_vars.tib$combined[j]  ] <- 0
  # all other cells are given value 0
}



for (j in seq_along(two_var.l)) {
  
  
  GNT_DD[which(GNT_DD[, two_var.l[[j]][1] ] == two_val.l[[j]][1] &  GNT_DD[, two_var.l[[j]][2] ] 
                  == two_val.l[[j]][2]),           two_tuple_vars.tib$combined[j]] <- 1
  # the same process as in j loop above except that it identifies where 2 columns in working.df
  # have the two values inticated in two_val.l.
  
  GNT_DD[which(is.na(GNT_DD[, two_tuple_vars.tib$combined[j] ])), two_tuple_vars.tib$combined[j]  ] <- 0
  
  
}



for (j in seq_along(three_var.l)) {
  
  
  
  GNT_DD[which(GNT_DD[, three_var.l[[j]][1] ]  == three_val.l[[j]][1]  
                  & GNT_DD[, three_var.l[[j]][2] ]  == three_val.l[[j]][2] 
                  & GNT_DD[, three_var.l[[j]][3] ]  == three_val.l[[j]][3] 
  )      , three_tuple_vars.tib$combined[j]    ]  <- 1
  
  # the same process as in j loop above except that it identifies where 3 columns in working.df
  # have the 3 values indicated in three_val.l.
  
  GNT_DD[which(is.na(GNT_DD[, three_tuple_vars.tib$combined[j] ])), three_tuple_vars.tib$combined[j]  ] <- 0
  
}


## save results 
saveRDS(GNT_DD, file = "GNT_binary-valued.RDS")
