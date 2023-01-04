# a script for classification
require(tidyverse)
require(LiblineaR)

## read in binary-valued file

working.df <- readRDS("GNT_binary-valued.RDS")
works.v <- working.df$book_id %>%
  unique()

works.v <- works.v[-which(is.na(works.v))]
features.v <- which(str_detect(colnames(working.df), "_ISA_"))
features.v <- c(1, features.v)

works.v <- works.v[- which(tokencount.v < 1000)] # remove very short works


# split files to make training and testing sets
training.l <- vector(mode = "list", length(works.v))
testing.l <- vector(mode = "list", length(works.v))

for (i in seq_along(works.v)) {
  x <- working.df %>%
    filter(book_id == works.v[i])
  a <- floor(nrow(x) / 2)
  b <- ceiling(nrow(x) / 2)
  
  training.l[[i]] <- x[1:a, features.v]
  
  y <- x[b:nrow(x), features.v[-1]] %>%
    colMeans()
  
  testing.l[[i]] <- c(x[1, 1], y)
  
  
}

training.df <- do.call(bind_rows, training.l)
testing.df <-  do.call(bind_rows, testing.l)


training.collect.l <- vector(mode = "list", length(works.v))

for (i in seq_along(works.v)) {
  
  x <- training.df %>%
    filter(book_id == works.v[i])
  
  boot.l <- vector(mode = "list", 1000)
  
  for (j in seq_along(boot.l)) {
    
    s <- sample(1:nrow(x), nrow(x), replace = TRUE)
    boot.l[[j]] <- x[s, -1] %>%
      colMeans()
    
  }
  
  boot.df <- do.call(bind_rows, boot.l)
  boot.df <-  add_column(boot.df, book_id = x$book_id[1], .before = TRUE)
  training.collect.l[[i]] <- boot.df
  
}

final.training.df <- do.call(bind_rows, training.collect.l)


## add vector of class names (formatted as factors) as column to training set
authors.v <- final.training.df$book_id %>%
  as.factor()

final.training.df <- add_column(final.training.df, class = authors.v, .before = TRUE)


# find optimal cost parameter for logistic classifer 
cost.v <- LiblineaR(final.training.df[, 3:ncol(final.training.df)], final.training.df$class, type =0, findC = TRUE )

## create model
model_liblin <- LiblineaR(final.training.df[, 3:ncol(final.training.df)], final.training.df$class, type =0, cost = cost.v)


p <- predict(model_liblin, testing.df[, 2:ncol(testing.df)], proba = TRUE)

p$predictions

rr
###

testing_2.l <- vector(mode = "list", length(works.v))

for (i in seq_along(works.v)) {
  x <- working.df %>%
    filter(book_id == works.v[i])
  a <- floor(nrow(x) / 2)
  b <- ceiling(nrow(x) / 2)
  
  training.l[[i]] <- x[1:a, features.v]
  
  
  
  testing_2.l[[i]] <- x[b:nrow(x), features.v]
  
  
}


testing_2.df <-  do.call(bind_rows, testing_2.l)

testing_2.collect.l <- vector(mode = "list", length(works.v))

for (i in seq_along(works.v)) {
  
  x <- testing_2.df %>%
    filter(book_id == works.v[i])
  
  boot.l <- vector(mode = "list", 100)
  
  for (j in seq_along(boot.l)) {
    
    s <- sample(1:nrow(x), nrow(x), replace = TRUE)
    boot.l[[j]] <- x[s, -1] %>%
      colMeans()
    
  }
  
  boot.df <- do.call(bind_rows, boot.l)
  boot.df <-  add_column(boot.df, book_id = x$book_id[1], .before = TRUE)
  testing_2.collect.l[[i]] <- boot.df
  
  print(paste("finished with file", i, works.v[i]))
  
}

final.testing_2.df <-  do.call(bind_rows, testing_2.collect.l)

p2 <-  predict(model_liblin, final.testing_2.df[, 2:ncol(final.testing_2.df)], proba = TRUE)

require(caret)


t <- confusionMatrix(p2$predictions, as.factor(final.testing_2.df$book_id))

p$probabilities[1, ]

