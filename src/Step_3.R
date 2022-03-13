library(tidyverse)

# script for selecting most frequent combinations



## select most common vars

files.v <- dir(pattern = ".RDS") # make vector of file names
working.df <- readRDS(files.v[1]) # load any file from directory

# drop column names for non pertinent categories

x <- working.df %>% # make vector of col names
  colnames()
# drop selected cols

x <- x[str_detect(x, "self") | str_detect(x, "parent")] # select all cols with "self" or "parent" in name

x <- x[-which(x =="global_parent_id")]
x <- x[-which(x =="self_dd")]
x <- x[-which(x =="parent_dd")]
x <- x[-which(x =="self_Not_App")]
x <- x[-which(x =="parent_Not_App")]



selected.cols.v <- x  # these are the cols for variables

# a loop to make all possible combinations of names in vector of selected cols
selected_vars.list <- vector(mode = "list", 3) # make list to store results
nomina.v <- NULL # vector to store names of list elements

for (k in 1:3) { 
  selected_vars.list[[k]] <- combn(selected.cols.v, k, simplify = FALSE) # make all possible combinations of variables
  nomina.v <- paste(length(selected.cols.v), "Choose",  k, collapse = " ") %>% # create names for elements in list
    append(nomina.v, .)
}


# a loop to make sample selection
holder.l <- vector(mode = "list", length(files.v)) # list to hold results

for (i in seq_along(files.v)) {
  
  working.df <- readRDS(file = files.v[i]) # read in files in directory
  
  working.df <-   working.df[, selected.cols.v] # drop metadata and keep only relevant columns as selected above
  
  
  working.df <- working.df %>%
    filter(!self_POS == "PUNCT") # drop punctuation from consideration
  
  if (nrow(working.df) >= 500 ) { # make random selection of n rows
    holder.l[[i]] <-working.df[sample(nrow(working.df), 500), ]
  } else {
    
    holder.l[[i]] <- working.df
  }
  
}

sample.df <- do.call(rbind, holder.l) # combine results into single data frame

########### create complex variables and find the most frequent var-val pairs

# make 2 col data frame: each row is a unique simplex name-value pair, e.g. name = "parent_voice," value = "ACT", followed by the count of that pair

freq.tib_simplex <- gather(sample.df[,   ], variable_name, variable_value,  na.rm = TRUE) %>%
  mutate(combined = paste(paste(variable_name, variable_value, sep = "_Is_"))) %>%
  select( combined) %>%
  group_by( combined) %>%
  summarize(freq = n())  %>%
  arrange(desc(freq))




# extract into separate vectors the variable names for combinations of two morphosyntactic categories
var_1 <- sapply(selected_vars.list[[2]], `[`, 1)
var_2 <- sapply(selected_vars.list[[2]], `[`, 2)


nomen.v <- paste(var_1, var_2, sep = "_And_") # combine simplex variable names into a combined name: e.g, "self_POS_And_self_rel"
# make matrix to store duplex name-value pairs
r <- nrow(sample.df)
c <- length(nomen.v)
duplex_sample.df <- matrix(ncol = c, nrow = r)
colnames(duplex_sample.df) <- nomen.v


# a loop to populate cols with values; values taken from original sample.df
for (i in seq_along(var_1)) {
  
   duplex_sample.df[, nomen.v[i]] <- paste(unlist(sample.df[, var_1[i]]), unlist(sample.df[, var_2[i] ]), sep = "_And_")
  
}

duplex_sample.df <- as.data.frame(duplex_sample.df) # convert matrix to data frame (required for next step)

# make 2 col data frame: each row is a unique duplex name-value pair

freq.tib_two_plex <- gather(duplex_sample.df[,  ], variable_name, variable_value,  na.rm = TRUE) %>%
  mutate(combined = paste(paste(variable_name, variable_value, sep = "_Are_"))) %>%
  select( combined) %>%
  group_by( combined) %>%
  summarize(freq = n())  %>%
  arrange(desc(freq))





# extract into separate vectors the variable names for combinations of three morpho-syntactic categories
var_1 <- sapply(selected_vars.list[[3]], `[`, 1)
var_2 <- sapply(selected_vars.list[[3]], `[`, 2)
var_3 <- sapply(selected_vars.list[[3]], `[`, 3)

nomen.v <- paste(var_1, var_2, var_3, sep = "_And_")

r <- nrow(sample.df)
c <- length(nomen.v)
triplex_sample.df <- matrix(ncol = c, nrow = r)
colnames(triplex_sample.df) <- nomen.v



for (i in seq_along(var_1)) {
  
  triplex_sample.df[, nomen.v[i]] <- paste( unlist(sample.df[, var_1[i]]), unlist(sample.df[, var_2[i] ]), 
                                unlist(sample.df[, var_3[i] ]),         sep = "_And_")
  
}

triplex_sample.df <- as.data.frame(triplex_sample.df)

freq.tib_three_plex <- gather(triplex_sample.df[,  ], variable_name, variable_value,  na.rm = TRUE) %>%
  mutate(combined = paste(paste(variable_name, variable_value, sep = "_Are_"))) %>%
  select( combined) %>%
  group_by( combined) %>%
  summarize(freq = n())  %>%
  arrange(desc(freq))

# remove any row containing a value of "NA"

freq.tib_simplex <- freq.tib_simplex %>%
  filter(!str_detect(combined, "NA"))


freq.tib_two_plex <- freq.tib_two_plex %>%
  filter(!str_detect(combined, "NA"))

freq.tib_three_plex <- freq.tib_three_plex %>%
  filter(!str_detect(combined, "NA"))

# drop all rows with frequency less than 5% of words
y <- nrow(sample.df)*.05

simplex_vars.tib <- freq.tib_simplex %>%
  filter(freq >= y)

two_plex.vars.tib <- freq.tib_two_plex %>%
  filter(freq >= y)

three_plex_vars.tib <-  freq.tib_three_plex %>%
  filter(freq >= y)



# create directory for variables and then change working directory to that dir

saveRDS(simplex_vars.tib, "simplex_5pc.RDS")
saveRDS(two_plex.vars.tib, "two_plex_5pc.RDS")
saveRDS(three_plex_vars.tib, "three_plex_5pc.RDS")


