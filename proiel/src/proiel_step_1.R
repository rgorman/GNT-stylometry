

require(tidyverse)
require(xml2)


files.v <- dir(pattern = ".xml")


x <- read_xml(files.v[1]) # read in xml file listed in files.v

y <- x %>%
  xml_find_all("//token") # extract all token elements from xml file


a <- xml_attrs(y) # convert xml nodeset to list object (for ease of manipulation)



working.df <- do.call(bind_rows, a) # convert list to data frame object

a <- colnames(working.df) %>% # replace hyphen with underscore in column names
  gsub("-", "_", .)

colnames(working.df) <- a

working.df <- add_column(working.df, new_id = 1:nrow(working.df), .before = TRUE)

# new head_id numbers are needed

parent.v <- working.df$head_id

# a (slow) loop to find new_token_id index corresponding to old head_id

holder.l <- vector(mode = "list", nrow(working.df))
for (i in seq_along(holder.l)) {
  holder.l[[i]] <- which(working.df$id == parent.v[i])
}

is.na(holder.l) <- lengths(holder.l) == 0 # replace empty elements with NA



b <- unlist(holder.l) # list to vector

working.df <- add_column(working.df, new_head_id = b, .before = TRUE) # add new column of parent ids

## expand morphology etc

working.df <- add_column(working.df, self_form = working.df$form)
working.df <- add_column(working.df, self_lemma = working.df$lemma)

working.df <- add_column(working.df, self_relation = working.df$relation)



### add dependency distance etc.

working.df <- add_column(working.df, self_dep_dist = as.integer(working.df$new_id) - as.integer(working.df$new_head_id) )
ad.v <- working.df$self_dep_dist
follow.v <- which(ad.v < 0)
precede.v <- which(ad.v > 0)
ad.v[follow.v]<-"head_follows"
ad.v[precede.v]<-"head_precedes"


working.df <- add_column(working.df, self_arc_dir = ad.v)


#### expand morph code

# part of speech
holder.v <- substr(working.df$part_of_speech, 1, 1)


holder.v[which(holder.v == "N")] <- "noun"
holder.v[which(holder.v == "A")] <- "adjective"
holder.v[which(holder.v == "M")] <- "adjective"
holder.v[which(holder.v == "R")] <- "preposition"
holder.v[which(holder.v == "V")] <- "verb"
holder.v[which(holder.v == "D")] <- "adverb"
holder.v[which(holder.v == "u")] <- "punctuation"
holder.v[which(holder.v == "-")] <- "no_POS"
holder.v[which(holder.v == "C")] <- "conjunction"
holder.v[which(holder.v == "G")] <- "subjunction"
holder.v[which(holder.v == "P")] <- "pronoun"
holder.v[which(holder.v == "S")] <- "article"
holder.v[which(holder.v == "I")] <- "interjection"
holder.v[which(holder.v == "F")] <- "foreign"
holder.v[which(holder.v == "X")] <- "unassigned"


holder.v %>%
  unique()

working.df <- add_column(working.df, self_simple_pos = holder.v)

# additional parts of speech from proiel 

holder.v <- working.df$part_of_speech

holder.v[which(holder.v == "Nb")] <- "common_noun"
holder.v[which(holder.v == "Ne")] <- "proper_noun"
holder.v[which(holder.v == "Ma")] <- "cardinal_number"
holder.v[which(holder.v == "Mo")] <- "ordinal_number"
holder.v[which(holder.v == "V-")] <- NA
holder.v[which(holder.v == "A-")] <- NA
holder.v[which(holder.v == "R-")] <- NA
holder.v[which(holder.v == "Df")] <- NA
holder.v[which(holder.v == "Dq")] <- "relative_adverb"
holder.v[which(holder.v == "Du")] <- "interrogative_adverb"
holder.v[which(holder.v == "u")] <- "punctuation"
holder.v[which(holder.v == "-")] <- "no_POS"
holder.v[which(holder.v == "C-")] <- NA
holder.v[which(holder.v == "G-")] <- NA
holder.v[which(holder.v == "Pd")] <- "demonstrative_pronoun"
holder.v[which(holder.v == "Pr")] <- "relative_pronoun"
holder.v[which(holder.v == "Px")] <- "indefinite_pronoun"
holder.v[which(holder.v == "Pp")] <- "personal_pronoun"
holder.v[which(holder.v == "Pk")] <- "personal_reflexive_pronoun"
holder.v[which(holder.v == "Pt")] <- "possessive_reflexive_pronoun"
holder.v[which(holder.v == "Pi")] <- "interrogative_pronoun"
holder.v[which(holder.v == "Ps")] <- "possessive_pronoun"
holder.v[which(holder.v == "Pc")] <- "reciprical_pronoun"
holder.v[which(holder.v == "S-")] <- NA
holder.v[which(holder.v == "I-")] <- NA
holder.v[which(holder.v == "F-")] <- NA



working.df <- add_column(working.df, self_proiel_pos = holder.v)

# person
holder.v <- substr(working.df$morphology, 1, 1)

holder.v[which(holder.v == "1")] <- "first"
holder.v[which(holder.v == "2")] <- "second"
holder.v[which(holder.v == "3")] <- "third"
holder.v[which(holder.v == "x")] <- "uncertain"

holder.v[which(holder.v == "-")] <- NA
holder.v[which(holder.v == "_")] <- NA


working.df <- add_column(working.df, self_person = holder.v)

# number
holder.v <- substr(working.df$morphology, 2, 2)

holder.v[which(holder.v == "s")] <- "singular"
holder.v[which(holder.v == "p")] <- "plural"
holder.v[which(holder.v == "d")] <- "dual"
holder.v[which(holder.v == "x")] <- "uncertain"

holder.v[which(holder.v == "-")] <- NA
holder.v[which(holder.v == "_")] <- NA

working.df <- add_column(working.df, self_number = holder.v)


## tense

holder.v <- substr(working.df$morphology, 3, 3)

holder.v[which(holder.v == "a")] <- "aorist"
holder.v[which(holder.v == "p")] <- "present"
holder.v[which(holder.v == "r")] <- "perfect"
holder.v[which(holder.v == "f")] <- "future"
holder.v[which(holder.v == "i")] <- "imperfect"
holder.v[which(holder.v == "l")] <- "pluperfect"

holder.v[which(holder.v == "-")] <- NA
holder.v[which(holder.v == "_")] <- NA

working.df <- add_column(working.df, self_tense = holder.v)

## mood

holder.v <- substr(working.df$morphology, 4, 4)

holder.v[which(holder.v == "i")] <- "indicative"
holder.v[which(holder.v == "p")] <- "participle"
holder.v[which(holder.v == "n")] <- "infinitive"
holder.v[which(holder.v == "m")] <- "imperative"
holder.v[which(holder.v == "s")] <- "subjunctive"
holder.v[which(holder.v == "o")] <- "optative"
holder.v[which(holder.v == "g")] <- "gerundive"

holder.v[which(holder.v == "-")] <- NA
holder.v[which(holder.v == "_")] <- NA

working.df <- add_column(working.df, self_mood = holder.v)

## voice

holder.v <- substr(working.df$morphology, 5, 5)

holder.v[which(holder.v == "a")] <- "active"
holder.v[which(holder.v == "m")] <- "middle"
holder.v[which(holder.v == "p")] <- "passive"
holder.v[which(holder.v == "e")] <- "medio_passive"


holder.v[which(holder.v == "-")] <- NA
holder.v[which(holder.v == "_")] <- NA

working.df <- add_column(working.df, self_voice = holder.v)


## gender

holder.v <- substr(working.df$morphology, 6, 6)

holder.v[which(holder.v == "f")] <- "feminine"
holder.v[which(holder.v == "m")] <- "masculine"
holder.v[which(holder.v == "n")] <- "neuter"
holder.v[which(holder.v == "p")] <- "masculine_or_feminine"
holder.v[which(holder.v == "o")] <- "masculine_or_neuter"
holder.v[which(holder.v == "r")] <- "feminine_or_neuter"
holder.v[which(holder.v == "q")] <- "masculine_feminine_or_neuter"



holder.v[which(holder.v == "-")] <- NA
holder.v[which(holder.v == "_")] <- NA



working.df <- add_column(working.df, self_gender = holder.v)


## case

holder.v <- substr(working.df$morphology, 7, 7)

holder.v[which(holder.v == "a")] <- "accusative"
holder.v[which(holder.v == "d")] <- "dative"
holder.v[which(holder.v == "g")] <- "genitive"
holder.v[which(holder.v == "n")] <- "nominative"
holder.v[which(holder.v == "v")] <- "vocative"



holder.v[which(holder.v == "-")] <- NA
holder.v[which(holder.v == "_")] <- NA

working.df <- add_column(working.df, self_case = holder.v)

## degree

holder.v <- substr(working.df$morphology, 8, 8)

holder.v[which(holder.v == "c")] <- "comparative"
holder.v[which(holder.v == "p")] <- "positive"
holder.v[which(holder.v == "s")] <- "superlative"


holder.v[which(holder.v == "-")] <- NA
holder.v[which(holder.v == "_")] <- NA

working.df <- add_column(working.df, self_degree = holder.v)


## inflection 

holder.v <- substr(working.df$morphology, 10, 10)

holder.v[which(holder.v == "i")] <- "inflecting"
holder.v[which(holder.v == "n")] <- "non_inflecting"



holder.v[which(holder.v == "-")] <- NA
holder.v[which(holder.v == "_")] <- NA

working.df <- add_column(working.df, self_inflection = holder.v)


### add columns--based on self- columns--for parent variables

parent.v <- b # load parent indices (from above) to vector



working.df <- add_column(working.df, parent_form = working.df$self_form[parent.v])
working.df <- add_column(working.df, parent_lemma = working.df$self_lemma[parent.v])

working.df <- add_column(working.df, parent_relation = working.df$self_relation[parent.v])
working.df <- add_column(working.df, parent_dep_dist = working.df$self_dep_dist[parent.v])
working.df <- add_column(working.df, parent_arc_dir = working.df$self_arc_dir[parent.v])

working.df <- add_column(working.df, parent_simple_pos = working.df$self_simple_pos[parent.v])
working.df <- add_column(working.df, parent_proiel_pos = working.df$self_proiel_pos[parent.v])
working.df <- add_column(working.df, parent_person = working.df$self_person[parent.v])
working.df <- add_column(working.df, parent_number = working.df$self_number[parent.v])
working.df <- add_column(working.df, parent_tense = working.df$self_tense[parent.v])
working.df <- add_column(working.df, parent_mood = working.df$self_mood[parent.v])
working.df <- add_column(working.df, parent_voice = working.df$self_voice[parent.v])
working.df <- add_column(working.df, parent_gender = working.df$self_gender[parent.v])
working.df <- add_column(working.df, parent_case = working.df$self_case[parent.v])
working.df <- add_column(working.df, parent_degree = working.df$self_degree[parent.v])
working.df <- add_column(working.df, parent_inflection = working.df$self_inflection[parent.v])

### save resulting file

saveRDS(working.df, file = "GNT_expanded.RDS")

