require(tidyverse)


## make column of work identifiers
x <- GNT_expanded$citation_part


x <- strsplit(x," ")

x <- sapply(x, '[[', 1)

unique(x) # check results

GNT_expanded <- add_column(GNT_expanded, book_id = x, .before = TRUE)

## certain cells are NA in book_id. These rows should be dropped before processing

## create column with binned dependency distance

GNT_expanded$self_dep_dist %>% # look at quartiles of absolute values
  abs() %>%
  summary()

# reasonable bins will be 1, 2, equal to or greater than 3

GNT_expanded <- add_column(GNT_expanded, self_binned_dep_dist = abs(GNT_expanded$self_dep_dist) )
GNT_expanded <- add_column(GNT_expanded, parent_binned_dep_dist = abs(GNT_expanded$parent_dep_dist) )

a <- which(GNT_expanded$self_binned_dep_dist > 2)
GNT_expanded$self_binned_dep_dist[a] <- "gt2"

a <- which(GNT_expanded$parent_binned_dep_dist > 2)
GNT_expanded$parent_binned_dep_dist[a] <- "gt2"

# save file with its new columns in appropriate directory
saveRDS(GNT_expanded, file = "GNT_DD.RDS")

###############
## sample type-value pairs to choose input features

## identify relevant columns
a <- which(str_detect(colnames(GNT_expanded), "self_"))
b <- which(str_detect(colnames(GNT_expanded), "parent_"))

selected.cols.v <- colnames(GNT_expanded)[c(a, b)]

# delete form columns
x <- which(str_detect(selected.cols.v, "form"))
selected.cols.v <- selected.cols.v[-x]

#delete lemma columns
x <- which(str_detect(selected.cols.v, "lemma"))
selected.cols.v <- selected.cols.v[-x]

# delete unbinned dep dist
x <- which(selected.cols.v == "self_dep_dist")
selected.cols.v <- selected.cols.v[-x]

x <- which(selected.cols.v == "parent_dep_dist")
selected.cols.v <- selected.cols.v[-x]


## make all combination-types of from 1 to three elements
selected_vars.list <- vector(mode = "list", 3)
nomina.v <- NULL

for (k in 1:3) { 
  selected_vars.list[[k]] <- combn(selected.cols.v, k, simplify = FALSE) # make all possible combinations of variables
  nomina.v <- paste(length(selected.cols.v), "Choose",  k, collapse = " ") %>% # create names for elements in list
    append(nomina.v, .)
}

# populating combinations with values is computationally expensive. Thus we use a subsample.

a <- sample(1:nrow(GNT_expanded), 20000)

working.df <- GNT_expanded[a, selected.cols.v]

freq.tib_simplex <- gather(working.df, variable_name, variable_value,  na.rm = TRUE) %>%
  mutate(combined = paste(paste(variable_name, variable_value, sep = "_ISA_"))) %>%
  select( combined) %>%
  group_by( combined) %>%
  summarize(freq = n())  %>%
  arrange(desc(freq))



## combinations of two vars

var_1 <- sapply(selected_vars.list[[2]], `[`, 1)
var_2 <- sapply(selected_vars.list[[2]], `[`, 2)

nomen.v <- paste(var_1, var_2, sep = "_AND_")

working.df[, nomen.v] <- NA # add columns for combined variables

for (i in seq_along(var_1)) {
  
  working.df[, nomen.v[i]] <- paste(unlist(working.df[, var_1[i]]), unlist(working.df[, var_2[i] ]), sep = "_AND_")
  
}

freq.tib_two_plex <- gather(working.df[, 29:406  ], variable_name, variable_value,  na.rm = TRUE) %>%
  mutate(combined = paste(paste(variable_name, variable_value, sep = "_ISA_"))) %>%
  select( combined) %>%
  group_by( combined) %>%
  summarize(freq = n())  %>%
  arrange(desc(freq))


### combinations of 3 variables


var_1 <- sapply(selected_vars.list[[3]], `[`, 1)
var_2 <- sapply(selected_vars.list[[3]], `[`, 2)
var_3 <- sapply(selected_vars.list[[3]], `[`, 3)

nomen.v <- paste(var_1, var_2, var_3, sep = "_AND_")




working.df[, nomen.v] <- NA # add columns for combined variables

for (i in seq_along(nomen.v)) {
  
  working.df[, nomen.v[i]] <- paste( unlist(working.df[, var_1[i]]), unlist(working.df[, var_2[i] ]), 
                                    unlist(working.df[, var_3[i] ]),         sep = "_AND_")
  
}


freq.tib_three_plex <- gather(working.df[, 407:3682 ], variable_name, variable_value,  na.rm = TRUE) %>%
  mutate(combined = paste(paste(variable_name, variable_value, sep = "_ISA_"))) %>%
  select( combined) %>%
  group_by( combined) %>%
  summarize(freq = n())  %>%
  arrange(desc(freq))



### drop combinations containing na value

freq.tib_simplex <- freq.tib_simplex %>%
  filter(!str_detect(combined, "NA"))


freq.tib_two_plex <- freq.tib_two_plex %>%
  filter(!str_detect(combined, "NA"))

freq.tib_three_plex <- freq.tib_three_plex %>%
  filter(!str_detect(combined, "NA"))

## drop less frequent type-value features

simplex_vars.tib <- freq.tib_simplex %>%
  filter(freq >= 100)

two_plex.vars.tib <- freq.tib_two_plex %>%
  filter(freq >= 1162)

three_plex_vars.tib <-  freq.tib_three_plex %>%
  filter(freq >= 1794)

### save variables in appropriate directory
saveRDS(simplex_vars.tib, "simplex_2_5pc.RDS")
saveRDS(two_plex.vars.tib, "two_plex_2_5pc.RDS")
saveRDS(three_plex_vars.tib, "three_plex_2_5pc.RDS")
