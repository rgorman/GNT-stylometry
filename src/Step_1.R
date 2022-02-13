
# a script to parse html files of the Greek New Testament
# Load the following packages

require(udpipe)
require(xml2)
require(rvest)
require(tidyverse)

# from the cloned repository (https://github.com/biblicalhumanities/Nestle1904) copy the files in directory \Nestle1904\xhtml to a working directory; 
# it is good practice to call the directory "data."

# within the "data" directory, create a new sub-directory, called something like "parsed_files."



# in the parent dir of "data" create a dir called "src" to store code
# copy the file "ancient_greek-perseus-ud-2.5-191206.udpipe" into the src dir.
# this file can be obtained by cloning this repo from github: https://github.com/jwijffels/udpipe.models.ud.2.5

############
############

# set working dir to "src"


udmodel <- udpipe_load_model(file = "ancient_greek-perseus-ud-2.5-191206.udpipe" ) # load the languge model for ancient Greek

##

# set the "data" dir as the working dir for R

files.v <- dir(pattern = "html") # make vector of file names

for (i in seq_along(files.v)) { #loop through dir
  
  x <-  read_html(files.v[i]) # read file into R object
  y <- html_elements(x, "p") # extract all <p> html elements from file
  z <- html_text(y) # extract text from object y
  z <- z %>%  
    gsub('[0-9]+', "", .) # remove reference numbers from object y
  
  z <- z %>%
    paste0(collapse = " ") # transform vector so that it contains a single element; this will help avoid sentence numbering errors in parser
  
  
  parsed <- udpipe_annotate(udmodel, z) # process Greek text through parser
  parsed.df <- as.data.frame(parsed, detailed = TRUE) # convert output to data frame format
  
  nomen.v <- files.v[i] %>% # change suffix on file for saving
    gsub('-ap.*',  "", .) %>%
    paste0(., ".RDS")
  
  
  fp <- file.path("parsed_data", nomen.v) # create file path for saving
  
  saveRDS(parsed.df, file = fp)
  
  print(paste(nomen.v, " is completed"))
  
}


### Note: set i to 1 and walk through each step within the loop. When you have created parsed.df, click to the right of its name in the environment window.
# That way you can see what the results of the parser look like.
