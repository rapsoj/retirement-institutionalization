#### PREPARE WORKSPACE ####

# Set working directory
setwd("~/Desktop/Retirement and Institutionalization/Analysis")

# Load in libraries
library(haven)
library(sjlabelled)
library(stringr)
library(labelled)
library(plyr)
library(dplyr)
library(tidyr)
library(pglm)

# Read in individual wave data
df_1 <- as.data.frame(read_dta("wave_1_core_data_v3.dta"))
df_2 <- as.data.frame(read_dta("wave_2_core_data_v4.dta"))
df_3 <- as.data.frame(read_dta("wave_3_elsa_data_v4.dta"))
df_4 <- as.data.frame(read_dta("wave_4_elsa_data_v3.dta"))
df_5 <- as.data.frame(read_dta("wave_5_elsa_data_v4.dta"))
df_6 <- as.data.frame(read_dta("wave_6_elsa_data_v2.dta"))
df_7 <- as.data.frame(read_dta("wave_7_elsa_data.dta"))

# Read in financially derived variables
df_f1 <- as.data.frame(read_dta("wave_1_financial_derived_variables.dta"))
df_f2 <- as.data.frame(read_dta("wave_2_financial_derived_variables.dta"))
df_f3 <- as.data.frame(read_dta("wave_3_financial_derived_variables.dta"))
df_f4 <- as.data.frame(read_dta("wave_4_financial_derived_variables.dta"))
df_f5 <- as.data.frame(read_dta("wave_5_financial_derived_variables.dta"))
df_f6 <- as.data.frame(read_dta("wave_6_financial_derived_variables.dta"))
df_f7 <- as.data.frame(read_dta("wave_7_financial_derived_variables.dta"))


#### SELECT COLUMNS OF INTEREST ####

# Select columns to keep for wave 1 data
keep1 <- c("idauniq", "couple1", "chinhh1", "gcinhh1", "hhtot", "indager", "disex", 
           "fqethnr", "dianm", "dimar", "dimad", "dicdnm", "dianf", "difad",
           "dicdnf", "hegenh", "heill", "helim", "hefunc", "heeye", "hehear",
           "heaga", "heagb", "heagc", "heagd", "heage", "heagf", "heagg", "heagh",
           "heagi", "heagj", "heprk", "wpact1", "wpact2", "wpact3", "wpact4",
           "wpact5", "wpact6", "wpaway", "wptaw", "wpdes", "wpvw", "wpever",
           "wpsal", "wpest", "wpjact", "wpsjoby", "wphjob", "wpwly", "wpwlym",
           "wpwlyy", "wplljy", "wplljm", "wplnj", "wprage", "wperet", "wperag",
           "wperp", "wprea01", "wprea02", "wprea03", "wprea04", "wprea05",
           "wprea06", "wprea07", "wpmrea", "wprre01", "wprre02", "wprre03",
           "wprre04", "wprre05", "wpmrre", "wpwkr", "wpphi", "wphlwz", "iawork",
           "iasinc", "iafcon", "expw", "exhlim", "exrslf", "fqcbthr", "fqend")

# Select columns to keep for wave 2 data
colnames(df_2) <- tolower(colnames(df_2))
keep2 <- c("idauniq", "couple", "chinhh1", "gcinhh1", "hhtot", "indager", "disex", 
           "fqethnr", "dianm", "dimar", "dimad", "dicdnm", "dianf", "difad",
           "dicdnf", "hehelf", "heill", "helim", "helwk", "hetemp", "hefunc",
           "heeye", "hehear", "heaga", "heagb", "heagc", "heagd", "heage", "heagf",
           "heagg", "heagh", "heagi", "heagj", "heprk", "wpact1", "wpact2",
           "wpact3", "wpact4", "wpact5", "wpact6", "wpaway", "wptaw", "wpdes",
           "wpvw", "wpever", "wpljobm", "wpsal", "wpest", "wpjact", "wpsjoby",
           "wphjob", "wpwly", "wpwlym", "wpwlyy", "wplljy", "wplljm", "wplnj",
           "wpcret", "wpcrage", "wpcretl", "wprage", "wperet", "wperag",
           "wperp", "wprea01", "wprea02", "wprea03", "wprea04", "wprea05",
           "wprea06", "wprea07", "wpmrea", "wprre01", "wprre02", "wprre03",
           "wprre04", "wprre05", "wpmrre", "wpwkr", "wpphi", "wphlwz", "iawork",
           "iasinc", "iafcon", "expw", "exhlim", "exrslf", "fqcbthr", "fqend")

# Select columns to keep for wave 3 data
keep3 <- c("idauniq", "couple", "chinhh", "gcinhh", "hhtot", "indager", "disex", 
           "fqethnr", "dianm", "dimar", "dimad", "dicdnm", "dianf", "difad",
           "dicdnf", "hegenh", "heill", "helim", "helwk", "hetemp", "hefunc",
           "heeye", "hehear", "heaga", "heagb", "heagc", "heagd", "heage", "heagf",
           "heagg", "heagh", "heagi", "heagj", "heagk", "heprk", "wpactpw",
           "wpactse", "wpactvw", "wpactca", "wpactlo", "wpacted", "wpaway", "wptaw",
           "wpdes", "wpvw", "wpever", "wpljobm", "wpsal", "wpest", "wpjact", 
           "wpsjoby", "wpwywmo", "wpwywoh", "wpwywmch", "wphjob", "wpwly", "wpwlym",
           "wpwlyy", "wplljy", "wplljm", "wpystoh", "wpystpm", "wplnj", "wpcret",
           "wpcrage", "wpcretl", "wprage", "wpearly", "wperag", "wperet", "wperp",
           "wpreaoh", "wpmrea", "wprrag", "wpmrre", "wpwkr", "wpphi", "wphlwz",
           "iawork", "iasinc", "iafcon", "expw", "exhlim", "exrslf", "fqcbthr",
           "fqend", "w3edqual", "w3indout")

# Select columns to keep for wave 4 data
keep4 <- c("idauniq", "couple", "chinhh", "gcinhh", "hhtot", "indager", "disex", 
           "fqethnr", "dianm", "dimar", "dimad", "dicdnm", "dianf", "difad",
           "dicdnf", "hehelf", "heill", "helim", "helwk", "hetemp", "hefunc",
           "heeye", "hehear", "heaga", "heagb", "heagc", "heagd", "heage", "heagf",
           "heagg", "heagh", "heagi", "heagj", "heagk", "heprk", "wpactpw",
           "wpactse", "wpactvw", "wpactca", "wpactlo", "wpacted", "wpaway", "wptaw",
           "wpdes", "wpvw", "wpever", "wpljobm", "wpsal", "wpest", "wpjact", 
           "wpsjoby", "wpwywmo", "wpwywoh", "wpwywmch", "wphjob", "wpwly", "wpwlym",
           "wpwlyy", "wplljy", "wplljm", "wpystoh", "wpystpm", "wplnj", "wpcret",
           "wpcrage", "wpcretl", "wprage", "wpearly", "wperag", "wperet", "wperp",
           "wpreaoh", "wpmrea", "wprrag", "wpmrre", "wpwkr", "wpphi", "wphlwz",
           "iawork", "iasinc", "iafcon", "expw", "exhlim", "exrslf", "fqcbthr",
           "fqend", "w4edqual", "outindw4")

# Select columns to keep for wave 5 data
colnames(df_5) <- tolower(colnames(df_5))
keep5 <- c("idauniq", "couple", "chinhh", "gcinhh", "hhtot", "indager", "disex", 
           "fqethnr", "dianm", "dimar", "dimad", "dicdnm", "dianf", "difad",
           "dicdnf", "hehelf", "heill", "helim", "helwk", "hetemp", "hefunc",
           "heeye", "hehear", "heaga", "heagb", "heagc", "heagd", "heage", "heagf",
           "heagg", "heagh", "heagi", "heagj", "heagk", "heprk", "wpactpw",
           "wpactse", "wpactvw", "wpactca", "wpactlo", "wpacted", "wpaway", "wptaw",
           "wpdes", "wpvw", "wpever", "wpljobm", "wpsal", "wpest", "wpjact", 
           "wpsjoby", "wpwywmo", "wpwywoh", "wpwywmch", "wphjob", "wpwly", "wpwlym",
           "wpwlyy", "wplljy", "wplljm", "wpystoh", "wpystpm", "wplnj", "wpcret",
           "wpcrage", "wpcretl", "wprage", "wpearly", "wperag", "wperet", "wperp",
           "wpreaoh", "wpmrea", "wprrag", "wpmrre", "wpwkr", "wpphi", "wphlwz",
           "iawork", "iasinc", "iafcon", "expw", "exhlim", "exrslf", "fqcbthr",
           "fqend", "w5edqual", "w5indout")

# Select columns to keep for wave 6 data
colnames(df_6) <- tolower(colnames(df_6))
keep6 <- c("idauniq", "couple", "chinhh", "gcinhh", "hhtot", "indager", "disex", 
           "fqethnr", "dianm", "dimar", "dimad", "dicdnm", "dianf", "difad",
           "dicdnf", "hehelf", "heill", "helim", "helwk", "hetemp", "hefunc",
           "heeye", "hehear", "heaga", "heagb", "heagc", "heagd", "heage", "heagf",
           "heagg", "heagh", "heagi", "heagj", "heagk", "heprk", "wpactpw",
           "wpactse", "wpactvw", "wpactca", "wpactlo", "wpacted", "wpaway", "wptaw",
           "wpdes", "wpvw", "wpever", "wpljobm", "wpsal", "wpest", "wpjact", 
           "wpsjoby", "wpwywmo", "wpwywoh", "wpwywmch", "wphjob", "wpwly", "wpwlym",
           "wpwlyy", "wplljy", "wplljm", "wpystoh", "wpystpm", "wplnj", "wpcret",
           "wpcrage", "wpcretl", "wprage", "wpearly", "wperag", "wperet", "wperp",
           "wpreaoh", "wpmrea", "wprrag", "wpmrre", "wpwkr", "wpphi", "wphlwz",
           "iawork", "iasinc", "iafcon", "expw", "exhlim", "exrslf", "fqcbthr",
           "fqend", "w6indout")

# Select columns to keep for wave 7 data
colnames(df_7) <- tolower(colnames(df_7))
keep7 <- c("idauniq", "couple", "chinhh", "gcinhh", "hhtot", "indager", "disex", 
           "fqethnr", "dianm", "dimar", "dimad", "dicdnm", "dianf", "difad",
           "dicdnf", "hehelf", "heill", "helim", "helwk", "hetemp", "hefunc",
           "heeye", "hehear", "heaga", "heagb", "heagc", "heagd", "heage", "heagf",
           "heagg", "heagh", "heagi", "heagj", "heagk", "heprk", "wpactpw",
           "wpactse", "wpactvw", "wpactca", "wpactlo", "wpacted", "wpaway", "wptaw",
           "wpdes", "wpvw", "wpever", "wpljobm", "wpsal", "wpest", "wpjact", 
           "wpsjoby", "wpwywmo", "wpwywoh", "wpwywmch", "wphjob", "wpwly", "wpwlym",
           "wpwlyy", "wplljy", "wplljm", "wpystoh", "wpystpm", "wplnj", "wpcret",
           "wpcrage", "wpcretl", "wprage", "wpearly", "wperag", "wperet", "wperp",
           "wpreaoh", "wpmrea", "wprrag", "wpmrre", "wpwkr", "wpphi", "iawork",
           "iasinc", "iafcon", "expw", "exhlim", "exrslf", "fqcbthr", "fqend",
           "w7indout")

# Subset individual wave data
w_1 <- df_1[keep1]
w_2 <- df_2[keep2]
w_3 <- df_3[keep3]
w_4 <- df_4[keep4]
w_5 <- df_5[keep5]
w_6 <- df_6[keep6]
w_7 <- df_7[keep7]

# Select columns to keep for financial data
keep_f <- c("idauniq", "eqtotinc_bu_s")

# Subset financial data
f_1 <- df_f1[keep_f]
f_2 <- df_f2[keep_f]
f_3 <- df_f3[keep_f]
f_4 <- df_f4[keep_f]
f_5 <- df_f5[keep_f]
f_6 <- df_f6[keep_f]
f_7 <- df_f7[keep_f]


#### COMBINE WAVES ####

# Fix mismatched column names
colnames(w_1)[colnames(w_1) == "couple1"] <- "couple"
colnames(w_1)[colnames(w_1) == "chinhh1"] <- "chinhh"
colnames(w_1)[colnames(w_1) == "gcinhh1"] <- "gcinhh"
colnames(w_2)[colnames(w_2) == "chinhh1"] <- "chinhh"
colnames(w_2)[colnames(w_2) == "gcinhh1"] <- "gcinhh"
colnames(w_1)[colnames(w_1) == "hegenh"] <- "hehelf"
colnames(w_3)[colnames(w_3) == "hegenh"] <- "hehelf"

# Fix mismatched education column names
colnames(w_3)[colnames(w_3) == "w3edqual"] <- "edqual"
colnames(w_4)[colnames(w_4) == "w4edqual"] <- "edqual"
colnames(w_5)[colnames(w_5) == "w5edqual"] <- "edqual"

# Fix mismatched individual outcome names
colnames(w_3)[colnames(w_3) == "w3indout"] <- "indout"
colnames(w_4)[colnames(w_4) == "outindw4"] <- "indout"
colnames(w_5)[colnames(w_5) == "w5indout"] <- "indout"
colnames(w_6)[colnames(w_6) == "w6indout"] <- "indout"
colnames(w_7)[colnames(w_7) == "w7indout"] <- "indout"

# Create columns for wave 1 past month activities
act <- c("wpact1", "wpact2", "wpact3", "wpact4", "wpact5", "wpact6")
w_1$wpactpw <- NA
w_1$wpactse <- NA
w_1$wpactvw <- NA
w_1$wpactca <- NA
w_1$wpactlo <- NA
w_1$wpacted <- NA

# Translate wave 1 numerated past month activities
w_1$wpactpw <- ifelse(apply(w_1[, act] == 1, 1, any), 1, w_1$wpactpw)
w_1$wpactse <- ifelse(apply(w_1[, act] == 2, 1, any), 1, w_1$wpactse)
w_1$wpactvw <- ifelse(apply(w_1[, act] == 3, 1, any), 1, w_1$wpactvw)
w_1$wpactca <- ifelse(apply(w_1[, act] == 4, 1, any), 1, w_1$wpactca)
w_1$wpactlo <- ifelse(apply(w_1[, act] == 5, 1, any), 1, w_1$wpactlo)
w_1$wpacted <- ifelse(apply(w_1[, act] == 6, 1, any), 1, w_1$wpacted)

# Format no past month activities for wave 1
w_1$wpactpw <- ifelse(w_1$wpact1 > 0 & w_1$wpact1 != 1, 0, w_1$wpactpw)
w_1$wpactse <- ifelse(w_1$wpact1 > 0 & w_1$wpact1 != 2, 0, w_1$wpactse)
w_1$wpactvw <- ifelse(w_1$wpact1 > 0 & w_1$wpact1 != 3, 0, w_1$wpactvw)
w_1$wpactca <- ifelse(w_1$wpact1 > 0 & w_1$wpact1 != 4, 0, w_1$wpactca)
w_1$wpactlo <- ifelse(w_1$wpact1 > 0 & w_1$wpact1 != 5, 0, w_1$wpactlo)
w_1$wpacted <- ifelse(w_1$wpact1 > 0 & w_1$wpact1 != 6, 0, w_1$wpacted)

# Create columns for wave 2 past month activities
act <- c("wpact1", "wpact2", "wpact3", "wpact4", "wpact5", "wpact6")
w_2$wpactpw <- NA
w_2$wpactse <- NA
w_2$wpactvw <- NA
w_2$wpactca <- NA
w_2$wpactlo <- NA
w_2$wpacted <- NA

# Translate wave 2 numerated past month activities
w_2$wpactpw <- ifelse(apply(w_2[, act] == 1, 1, any), 1, w_2$wpactpw)
w_2$wpactse <- ifelse(apply(w_2[, act] == 2, 1, any), 1, w_2$wpactse)
w_2$wpactvw <- ifelse(apply(w_2[, act] == 3, 1, any), 1, w_2$wpactvw)
w_2$wpactca <- ifelse(apply(w_2[, act] == 4, 1, any), 1, w_2$wpactca)
w_2$wpactlo <- ifelse(apply(w_2[, act] == 5, 1, any), 1, w_2$wpactlo)
w_2$wpacted <- ifelse(apply(w_2[, act] == 6, 1, any), 1, w_2$wpacted)

# Format no past month activities for wave 2
w_2$wpactpw <- ifelse(w_2$wpact1 > 0 & w_2$wpact1 != 1, 0, w_2$wpactpw)
w_2$wpactse <- ifelse(w_2$wpact1 > 0 & w_2$wpact1 != 2, 0, w_2$wpactse)
w_2$wpactvw <- ifelse(w_2$wpact1 > 0 & w_2$wpact1 != 3, 0, w_2$wpactvw)
w_2$wpactca <- ifelse(w_2$wpact1 > 0 & w_2$wpact1 != 4, 0, w_2$wpactca)
w_2$wpactlo <- ifelse(w_2$wpact1 > 0 & w_2$wpact1 != 5, 0, w_2$wpactlo)
w_2$wpacted <- ifelse(w_2$wpact1 > 0 & w_2$wpact1 != 6, 0, w_2$wpacted)

# Drop numerated past month activities
w_1 <- w_1[, !(names(w_1) %in% act)]
w_2 <- w_2[, !(names(w_2) %in% act)]

# Fix wave 1 early retirement reasons
rea <- c("wprea01", "wprea02", "wprea03", "wprea04", "wprea05", "wprea06", "wprea07")
w_1$wpreaoh <- NA
w_1$wpreaoh <- ifelse(apply(w_1[, rea] == 1, 1, any), 1, w_1$wpreaoh)
w_1$wpreaoh <- ifelse(w_1$wprea01 > 0 & w_1$wprea01 != 1, 0, w_1$wpreaoh)
w_1 <- w_1[, !(names(w_1) %in% rea)]

# Fix wave 2 early retirement reasons
w_2$wpreaoh <- NA
w_2$wpreaoh <- ifelse(apply(w_2[, rea] == 1, 1, any), 1, w_2$wpreaoh)
w_2$wpreaoh <- ifelse(w_2$wprea01 > 0 & w_2$wprea01 != 1, 0, w_2$wpreaoh)
w_2 <- w_2[, !(names(w_2) %in% rea)]

# Fix wave 1 retirement reasons
rre <- c("wprre01", "wprre02", "wprre03", "wprre04", "wprre05")
w_1$wprrag <- NA
w_1$wprrag <- ifelse(apply(w_1[, rre] == 2, 1, any), 1, w_1$wprrag)
w_1$wprrag <- ifelse(w_1$wprre01 > 0 & w_1$wprre01 != 2, 0, w_1$wprrag)
w_1 <- w_1[, !(names(w_1) %in% rre)]

# Fix wave 2 retirement reasons
w_2$wprrag <- NA
w_2$wprrag <- ifelse(apply(w_2[, rre] == 2, 1, any), 1, w_2$wprrag)
w_2$wprrag <- ifelse(w_2$wprre01 > 0 & w_2$wprre01 != 2, 0, w_2$wprrag)
w_2 <- w_2[, !(names(w_2) %in% rre)]

# Add wave 1 missing columns
w_1$helwk <- NA
w_1$hetemp <- NA
w_1$heagk <- NA
w_1$wpljobm <- NA
w_1$wpwywmo <- NA
w_1$wpwywoh <- NA
w_1$wpwywmch <- NA
w_1$wpystoh <- NA
w_1$wpystpm <- NA
w_1$wpcret <- NA
w_1$wpcrage <- NA
w_1$wpcretl <- NA
w_1$wpearly <- NA
w_1$edqual <- NA
w_1$indout <- NA

# Add wave 2 missing columns
w_2$heagk <- NA
w_2$wpwywmo <- NA
w_2$wpwywoh <- NA
w_2$wpwywmch <- NA
w_2$wpystoh <- NA
w_2$wpystpm <- NA
w_2$wperet <- NA
w_2$edqual <- NA
w_2$indout <- NA
w_2$wpearly <- NA

# Add wave 6 and 7 missing columns
w_6$edqual <- NA
w_7$wphlwz <- NA
w_7$edqual <- NA

# Create column for wave number
w_1$wave <- 1
w_2$wave <- 2
w_3$wave <- 3
w_4$wave <- 4
w_5$wave <- 5
w_6$wave <- 6
w_7$wave <- 7

# Merge financial variables
w_1 <- merge(w_1, f_1)
w_2 <- merge(w_2, f_2)
w_3 <- merge(w_3, f_3)
w_4 <- merge(w_4, f_4)
w_5 <- merge(w_5, f_5)
w_6 <- merge(w_6, f_6)
w_7 <- merge(w_7, f_7)

# Re-order columns and remove labels
w_1 <- remove_labels(w_1[ , colnames(w_3)])
w_2 <- remove_labels(w_2[ , colnames(w_3)])
w_4 <- remove_labels(w_4[ , colnames(w_3)])
w_5 <- remove_labels(w_5[ , colnames(w_3)])
w_6 <- remove_labels(w_6[ , colnames(w_3)])
w_7 <- remove_labels(w_7[ , colnames(w_3)])

# Add waves vertically
df <- rbind(w_1, w_2, w_3, w_4, w_5, w_6, w_7)


#### CLEAN DATA ####

# Drop unusable variables
drop <- c("dicdnm", "dicdnf", "hehelf", "hetemp", "hefunc", "heeye", "hehear", "wpest",
          "wpjact", "wpsjoby", "wpwywmo", "wpwywoh", "wpwywmch", "wphjob", "wplljy",
          "wplljm", "wpystpm", "wpphi", "wphlwz", "wpmrea", "wpmrre", "fqend")
df <- df[, !(names(df) %in% drop)]

# Clean "couple" variable
df$couple <- ifelse(df$couple == 3, 0, df$couple)
df$couple <- as.factor(df$couple)

# Clean "chinhh" variable
df$chinhh <- ifelse(df$chinhh == 2, 0, df$chinhh)
df$chinhh <- ifelse(df$chinhh < 0, NA, df$chinhh)

# Clean "gcinhh" variable
df$gcinhh <- ifelse(df$gcinhh == 2, 0, df$gcinhh)
df$gcinhh <- ifelse(df$gcinhh < 0, NA, df$gcinhh)

# Clean "hhtot" variable
df$hhtot <- ifelse(df$hhtot == 0, NA, df$hhtot)

# Clean "indager" variable
df$indager <- ifelse(df$indager < 0, NA, df$indager)

# Clean "disex" variable
df$disex <- ifelse(df$disex == 2, 0, df$disex)
df$disex <- ifelse(df$disex < 0, NA, df$disex)

# Clean "fqethnr" variable
df$fqethnr <- ifelse(df$fqethnr == 2, 0, df$fqethnr)
df$fqethnr <- ifelse(df$fqethnr < 0, NA, df$fqethnr)

# Clean "dianm" variable
df$dianm <- ifelse(df$dianm < 0, NA, df$dianm)

# Clean "dimar" variable
df$dimar <- ifelse(df$dimar < 0, NA, df$dimar)
df$dimar <- ifelse(df$dimar == 1, 0, df$dimar)
df$dimar <- ifelse(df$wave %in% c(1,2) & df$dimar %in% c(2,3), 1, df$dimar)
df$dimar <- ifelse(df$wave %in% c(1,2) & df$dimar %in% c(4,5), 2, df$dimar)
df$dimar <- ifelse(df$wave %in% c(1,2) & df$dimar == 6, 3, df$dimar)
df$dimar <- ifelse(df$wave %in% c(3,4,5,6,7) & df$dimar %in% c(2,3,4,11), 1, df$dimar)
df$dimar <- ifelse(df$wave %in% c(3,4,5,6,7) & df$dimar %in% c(5,6,8,9), 2, df$dimar)
df$dimar <- ifelse(df$wave %in% c(3,4,5,6,7) & df$dimar %in% c(7,10), 3, df$dimar)
df$dimar <- as.factor(df$dimar)

# Clean "dimad" variable
df$dimad <- ifelse(df$dimad <= 0, NA, df$dimad)

# Clean "dianf" variable
df$dianf <- ifelse(df$dianf < 0, NA, df$dianf)

# Clean "difad" variable
df$difad <- ifelse(df$difad <= 0, NA, df$difad)

# Clean "heill" variable
df$heill <- ifelse(df$heill == 2, 0, df$heill)
df$heill <- ifelse(df$heill < 0, NA, df$heill)

# Clean "helim" variable
df$helim <- ifelse(df$helim == 2, 0, df$helim)
df$helim <- ifelse(df$helim < 0, NA, df$helim)

# Clean "helwk" variable
df$helwk <- ifelse(df$helwk == 2, 0, df$helwk)
df$helwk <- ifelse(df$helwk < 0, NA, df$helwk)

# Clean "heag" variables
df$heaga <- ifelse(df$heaga < 0, NA, df$heaga)
df$heagb <- ifelse(df$heagb < 0, NA, df$heagb)
df$heagc <- ifelse(df$heagc < 0, NA, df$heagc)
df$heagd <- ifelse(df$heagd < 0, NA, df$heagd)
df$heage <- ifelse(df$heage < 0, NA, df$heage)
df$heagf <- ifelse(df$heagf < 0, NA, df$heagf)
df$heagg <- ifelse(df$heagg < 0, NA, df$heagg)
df$heagh <- ifelse(df$heagh < 0, NA, df$heagh)
df$heagi <- ifelse(df$heagi < 0, NA, df$heagi)
df$heagj <- ifelse(df$heagj < 0, NA, df$heagj)
df$heagk <- ifelse(df$heagk < 0, NA, df$heagk)
df$heprk <- ifelse(df$heprk < 0, NA, df$heprk)

# Clean "wpact" variables
df$wpactpw <- ifelse(df$wpactpw < 0, NA, df$wpactpw)
df$wpactse <- ifelse(df$wpactse < 0, NA, df$wpactse)
df$wpactvw <- ifelse(df$wpactvw < 0, NA, df$wpactvw)
df$wpactca <- ifelse(df$wpactca < 0, NA, df$wpactca)
df$wpactlo <- ifelse(df$wpactlo < 0, NA, df$wpactlo)
df$wpacted <- ifelse(df$wpacted < 0, NA, df$wpacted)

# Clean "wpaway" variable
df$wpaway <- ifelse(df$wpaway < 0, NA, df$wpaway)
df$wpaway <- ifelse(df$wpaway %in% c(2,3), 1, df$wpaway)
df$wpaway <- ifelse(df$wpaway == 96, 0, df$wpaway)

# Clean "wptaw" variable
df$wptaw <- ifelse(df$wptaw < 0, NA, df$wptaw)
df$wptaw <- ifelse(df$wptaw > 0 & df$wptaw != 2, 0, df$wptaw)
df$wptaw <- ifelse(df$wptaw == 2, 1, df$wptaw)

# Clean "wpdes" variable
df$wpdes <- ifelse(df$wpdes < 0 | df$wpdes %in% c(85, 86, 95), NA, df$wpdes)
df$wpdes <- ifelse(df$wpdes == 1, 0, df$wpdes)
df$wpdes <- ifelse(df$wpdes == 96, 1, df$wpdes)
df$wpdes <- as.factor(df$wpdes)

# Clean "wpvw" variable
df$wpvw <- ifelse(df$wpvw < 0, NA, df$wpvw)
df$wpvw <- mapvalues(df$wpvw, from=c(6,5,4,2,1), to=c(0,1,2,4,5))

# Clean "wpever" variable
df$wpever <- ifelse(df$wpever == 2, 0, df$wpever)
df$wpever <- ifelse(df$wpever < 0, NA, df$wpever)

# Clean "wpljobm" variable
df$wpljobm <- ifelse(df$wpljobm < 0, NA, df$wpljobm)
df$wpljobm <- ifelse(df$wpljobm > 1, 0, df$wpljobm)

# Clean "wpsal" variable
df$wpsal <- ifelse(df$wpsal == 2, 0, df$wpsal)
df$wpsal <- ifelse(df$wpsal < 0, NA, df$wpsal)

# Clean "wpwly" variable
df$wpwly <- ifelse(df$wpwly == 2, 0, df$wpwly)
df$wpwly <- ifelse(df$wpwly < 0, NA, df$wpwly)

# Clean "wpwlym" variable
df$wpwlym <- ifelse(df$wpwlym < 0, NA, df$wpwlym)

# Clean "wpwlyy" variable
df$wpwlyy <- ifelse(df$wpwlyy < 0, NA, df$wpwlyy)

# Clean "wpystoh" variable
df$wpystoh <- ifelse(df$wpystoh < 0, NA, df$wpystoh)

# Clean "wplnj" variable
df$wplnj <- ifelse(df$wplnj == 2, 0, df$wplnj)
df$wplnj <- ifelse(df$wplnj < 0, NA, df$wplnj)

# Clean "wpcret" variable
df$wpcret <- ifelse(df$wpcret == 2, 0, df$wpcret)
df$wpcret <- ifelse(df$wpcret < 0, NA, df$wpcret)

# Clean "wpcrage" variable
df$wpcrage <- ifelse(df$wpcrage < 0, NA, df$wpcrage)

# Clean "wpcretl" variable
df$wpcretl <- ifelse(df$wpcretl == 2, 0, df$wpcretl)
df$wpcretl <- ifelse(df$wpcretl < 0, NA, df$wpcretl)

# Clean "wprage" variable
df$wprage <- ifelse(df$wprage < 0, NA, df$wprage)

# Clean "wpearly" variable
df$wpearly <- ifelse(df$wpearly == 2, 0, df$wpearly)
df$wpearly <- ifelse(df$wpearly < 0, NA, df$wpearly)

# Clean "wperag" variable
df$wperag <- ifelse(df$wperag < 0, NA, df$wperag)

# Clean "wperet" variable
df$wperet <- ifelse(df$wperet == 2, 0, df$wperet)
df$wperet <- ifelse(df$wperet < 0, NA, df$wperet)

# Clean "wperp" variable
df$wperp <- ifelse(df$wperp < 0, NA, df$wperp)
df$wperp <- mapvalues(df$wperp, from=c(1,2,3), to=c(2,1,0))
df$wperp <- as.factor(df$wperp)

# Clean "wpreaoh" variable
df$wpreaoh <- ifelse(df$wpreaoh < 0, NA, df$wpreaoh)

# Clean "wprrag" variable
df$wprrag <- ifelse(df$wprrag < 0, NA, df$wprrag)

# Clean "wpwkr" variable
df$wpwkr <- ifelse(df$wpwkr == 2, 0, df$wpwkr)
df$wpwkr <- ifelse(df$wpwkr < 0, NA, df$wpwkr)

# Clean "iawork" variable
df$iawork <- ifelse(df$iawork == 2, 0, df$iawork)
df$iawork <- ifelse(df$iawork < 0, NA, df$iawork)

# Clean "iasinc" variable
df$iasinc <- ifelse(df$iasinc < 0, NA, df$iasinc)

# Clean "iafcon" variable
df$iafcon <- ifelse(df$iafcon < 0, NA, df$iafcon)
df$iafcon <- mapvalues(df$iafcon, from=c(1,2,3,4,5,6), to=c(0,1,2,3,4,5))

# Clean "ex" variables
df$expw <- ifelse(df$expw < 0, NA, df$expw)
df$exhlim <- ifelse(df$exhlim < 0, NA, df$exhlim)
df$exrslf <- ifelse(df$exrslf < 0, NA, df$exrslf)

# Clean "fqcbthr" variable
df$fqcbthr <- ifelse(df$fqcbthr == 2, 0, df$fqcbthr)
df$fqcbthr <- ifelse(df$fqcbthr < 0, NA, df$fqcbthr)

# Clean "edqual" variable
df$edqual <- ifelse(df$edqual < 0, NA, df$edqual)
df$edqual <- mapvalues(df$edqual, from=c(1,2,3,4,5,6,7), to=c(6,5,4,3,2,1,0))
df$edqual <- as.factor(df$edqual)

# Clean "indout" variable
df$indout <- ifelse(df$indout < 0, NA, df$indout)
df$indout <- ifelse(df$wave %in% c(1,2), 0, df$indout)
df$indout <- ifelse(df$indout > 0 & df$indout < 24, 0, df$indout)
df$indout <- ifelse(df$indout %in% c(24,25), 1, df$indout)

# Clean "eqtotinc_bu_s" variable
df$eqtotinc_bu_s <- ifelse(df$eqtotinc_bu_s %in% c(-999,-998,-995), NA, df$eqtotinc_bu_s)



#### IMPUTE MISSING VALUES ####

# Keep usable variables
keep <- c("idauniq", "wave", "indout", "couple", "chinhh", "gcinhh", "hhtot",
          "indager", "disex", "dimar",	"heill", "heaga", "heagb", "heagc",
          "heagd", "heage", "heagf", "heagg", "heagh", "heagi", "heagj", "heagk",
          "heprk",	"wpactpw",	"wpactse",	"wpactvw",	"wpactca", "wpactlo",
          "wpacted",	"wpdes",	"wprage",	"iawork",	"edqual",	"eqtotinc_bu_s")
df <- df[, (names(df) %in% keep)]

# Replace missing ages
df$indager <- ifelse(df$indager == 99, NA, df$indager)
ids <- df["idauniq"]
ids$w1 <- left_join(ids, df[df$wave == 1, c("idauniq", "indager")])$indager
ids$w2 <- left_join(ids, df[df$wave == 2, c("idauniq", "indager")])$indager
ids$w3 <- left_join(ids, df[df$wave == 3, c("idauniq", "indager")])$indager
ids$w4 <- left_join(ids, df[df$wave == 4, c("idauniq", "indager")])$indager
ids$w5 <- left_join(ids, df[df$wave == 5, c("idauniq", "indager")])$indager
ids$w6 <- left_join(ids, df[df$wave == 6, c("idauniq", "indager")])$indager
ids$w7 <- left_join(ids, df[df$wave == 7, c("idauniq", "indager")])$indager

ids$w6 <- ifelse(is.na(ids$w6) & !is.na(ids$w7), ids$w7 - 2, ids$w6)
ids$w5 <- ifelse(is.na(ids$w5) & !is.na(ids$w6), ids$w6 - 2, ids$w5)
ids$w4 <- ifelse(is.na(ids$w4) & !is.na(ids$w5), ids$w5 - 2, ids$w4)
ids$w3 <- ifelse(is.na(ids$w3) & !is.na(ids$w4), ids$w4 - 2, ids$w3)
ids$w2 <- ifelse(is.na(ids$w2) & !is.na(ids$w3), ids$w3 - 2, ids$w2)
ids$w1 <- ifelse(is.na(ids$w1) & !is.na(ids$w2), ids$w2 - 2, ids$w1)

ids$w2 <- ifelse(is.na(ids$w2) & !is.na(ids$w1), ids$w1 + 2, ids$w2)
ids$w3 <- ifelse(is.na(ids$w3) & !is.na(ids$w2), ids$w2 + 2, ids$w3)
ids$w4 <- ifelse(is.na(ids$w4) & !is.na(ids$w3), ids$w3 + 2, ids$w4)
ids$w5 <- ifelse(is.na(ids$w5) & !is.na(ids$w4), ids$w4 + 2, ids$w5)
ids$w6 <- ifelse(is.na(ids$w6) & !is.na(ids$w5), ids$w5 + 2, ids$w6)
ids$w7 <- ifelse(is.na(ids$w7) & !is.na(ids$w6), ids$w6 + 2, ids$w7)

df$indager <- ifelse(is.na(df$indager) & df$wave == 1, ids$w1, df$indager)
df$indager <- ifelse(is.na(df$indager) & df$wave == 2, ids$w2, df$indager)
df$indager <- ifelse(is.na(df$indager) & df$wave == 3, ids$w3, df$indager)
df$indager <- ifelse(is.na(df$indager) & df$wave == 4, ids$w4, df$indager)
df$indager <- ifelse(is.na(df$indager) & df$wave == 5, ids$w5, df$indager)
df$indager <- ifelse(is.na(df$indager) & df$wave == 6, ids$w6, df$indager)
df$indager <- ifelse(is.na(df$indager) & df$wave == 7, ids$w7, df$indager)

# Impute missing fixed values by matching most original response
fixed <- c("wprage", "edqual", "heaga", "heagb", "heagc", "heagd", "heage", "heagf",
           "heagg", "heagh", "heagi", "heagk", "heagk", "heprk", "disex")
for (i in fixed){
  ids <- df["idauniq"]
  ids$w1 <- left_join(ids, df[df$wave == 1, c("idauniq", i)])[[i]]
  ids$w2 <- left_join(ids, df[df$wave == 2, c("idauniq", i)])[[i]]
  ids$w3 <- left_join(ids, df[df$wave == 3, c("idauniq", i)])[[i]]
  ids$w4 <- left_join(ids, df[df$wave == 4, c("idauniq", i)])[[i]]
  ids$w5 <- left_join(ids, df[df$wave == 5, c("idauniq", i)])[[i]]
  ids$w6 <- left_join(ids, df[df$wave == 6, c("idauniq", i)])[[i]]
  ids$w7 <- left_join(ids, df[df$wave == 7, c("idauniq", i)])[[i]]
  
  ids$value <- NA
  ids$value <- ifelse(is.na(ids$value) & !is.na(ids$w1), as.character(ids$w1), ids$value)
  ids$value <- ifelse(is.na(ids$value) & !is.na(ids$w2), as.character(ids$w2), ids$value)
  ids$value <- ifelse(is.na(ids$value) & !is.na(ids$w3), as.character(ids$w3), ids$value)
  ids$value <- ifelse(is.na(ids$value) & !is.na(ids$w4), as.character(ids$w4), ids$value)
  ids$value <- ifelse(is.na(ids$value) & !is.na(ids$w5), as.character(ids$w5), ids$value)
  ids$value <- ifelse(is.na(ids$value) & !is.na(ids$w6), as.character(ids$w6), ids$value)
  ids$value <- ifelse(is.na(ids$value) & !is.na(ids$w7), as.character(ids$w7), ids$value)
  
  df[i] <- ids$value
}

# Convert education level to factor
df$edqual <- as.factor(df$edqual)


#### MATCH TREATMENT SUBJECTS TO CONTROLS ####

# Remove rows with no retirement age
df <- df[-which(is.na(df$wprage)),]

# Note which illnesses occured before retirement
hea <- c("heaga", "heagb", "heagc", "heagd", "heage", "heagf",
         "heagg", "heagh", "heagi", "heagk", "heagk", "heprk")
for (i in hea){
  df[i] <- as.numeric(df[[i]])
  df[i] <- ifelse(df[[i]] <= df$wprage, 1, 0)
}

# Replace missing values with 0
firstcol = which(colnames(df) == "heaga")
lastcol = which(colnames(df) == "heprk")
df[c(firstcol:lastcol)][is.na(df[c(firstcol:lastcol)])] <- 0

# Create variable for pre-retirement illnesses
df$heatot <- paste(df$heaga, df$heagb, df$heagc, df$heagd, df$heage, df$heagf,
                   df$heagg, df$heagh, df$heagi, df$heagk, df$heagk, df$heprk)

# Create variable for first wave and number of waves
ids <- df["idauniq"]
ids$w1 <- left_join(ids, df[df$wave == 1, c("idauniq", "wave")])[["wave"]]
ids$w2 <- left_join(ids, df[df$wave == 2, c("idauniq", "wave")])[["wave"]]
ids$w3 <- left_join(ids, df[df$wave == 3, c("idauniq", "wave")])[["wave"]]
ids$w4 <- left_join(ids, df[df$wave == 4, c("idauniq", "wave")])[["wave"]]
ids$w5 <- left_join(ids, df[df$wave == 5, c("idauniq", "wave")])[["wave"]]
ids$w6 <- left_join(ids, df[df$wave == 6, c("idauniq", "wave")])[["wave"]]
ids$w7 <- left_join(ids, df[df$wave == 7, c("idauniq", "wave")])[["wave"]]

ids$wavef <- NA
ids$waven <- 7 - rowSums(is.na(ids))
ids$wavef <- ifelse(is.na(ids$wavef) & !is.na(ids$w1), ids$w1, ids$wavef)
ids$wavef <- ifelse(is.na(ids$wavef) & !is.na(ids$w2), ids$w2, ids$wavef)
ids$wavef <- ifelse(is.na(ids$wavef) & !is.na(ids$w3), ids$w3, ids$wavef)
ids$wavef <- ifelse(is.na(ids$wavef) & !is.na(ids$w4), ids$w4, ids$wavef)
ids$wavef <- ifelse(is.na(ids$wavef) & !is.na(ids$w5), ids$w5, ids$wavef)
ids$wavef <- ifelse(is.na(ids$wavef) & !is.na(ids$w6), ids$w6, ids$wavef)
ids$wavef <- ifelse(is.na(ids$wavef) & !is.na(ids$w7), ids$w7, ids$wavef)

df$wavef <- ids$wavef
df$waven <- ids$waven

# Create variable for age at recruitment
ids <- df["idauniq"]
ids$w1 <- left_join(ids, df[df$wave == 1, c("idauniq", "indager")])[["indager"]]
ids$w2 <- left_join(ids, df[df$wave == 2, c("idauniq", "indager")])[["indager"]]
ids$w3 <- left_join(ids, df[df$wave == 3, c("idauniq", "indager")])[["indager"]]
ids$w4 <- left_join(ids, df[df$wave == 4, c("idauniq", "indager")])[["indager"]]
ids$w5 <- left_join(ids, df[df$wave == 5, c("idauniq", "indager")])[["indager"]]
ids$w6 <- left_join(ids, df[df$wave == 6, c("idauniq", "indager")])[["indager"]]
ids$w7 <- left_join(ids, df[df$wave == 7, c("idauniq", "indager")])[["indager"]]

ids$agef <- NA
ids$agef <- ifelse(is.na(ids$agef) & !is.na(ids$w1), ids$w1, ids$agef)
ids$agef <- ifelse(is.na(ids$agef) & !is.na(ids$w2), ids$w2, ids$agef)
ids$agef <- ifelse(is.na(ids$agef) & !is.na(ids$w3), ids$w3, ids$agef)
ids$agef <- ifelse(is.na(ids$agef) & !is.na(ids$w4), ids$w4, ids$agef)
ids$agef <- ifelse(is.na(ids$agef) & !is.na(ids$w5), ids$w5, ids$agef)
ids$agef <- ifelse(is.na(ids$agef) & !is.na(ids$w6), ids$w6, ids$agef)
ids$agef <- ifelse(is.na(ids$agef) & !is.na(ids$w7), ids$w7, ids$agef)

df$agef <- ids$agef

# Create dataframe for institutionalized individuals
inst_ids <- unique(df[df$indout == 1,]$idauniq)
df_inst <- df[df$idauniq %in% inst_ids,]

# Create dataframe for non-institutionalized individuals
out_ids <- unique(df[!(df$idauniq %in% inst_ids) & df$indager >= 49 & df$waven > 1,]$idauniq)
df_out <- df[df$idauniq %in% out_ids,]

# Create list of IDs for sex, age, and health paired samples
main_ids <- c()
match_ids <- c()
for (i in inst_ids){
  disex <- df_inst[df_inst$idauniq == i, "disex"][1]
  agef <- round_any(df_inst[df_inst$idauniq == i, "agef"][1], 5)
  heatot <- df_inst[df_inst$idauniq == i, "heatot"][1]
  values <- unique(df_out[df_out$disex == disex
                          & round_any(df_out$agef, 5) == agef
                          & df_out$heatot == heatot, ]$idauniq)
  x <- append(x, length(values))
  len <- length(values)
  if (len >= 5){
    set.seed(12345)
    matches <- sample(values, 5)
    match_ids <- append(match_ids, matches)
    main_ids <- append(main_ids, i)
    df_out = df_out[!(df_out$idauniq %in% matches), ]
  }
}

# Create new dataframe with full sample set
df_sample <- df[df$idauniq %in% append(match_ids, main_ids),]


#### FIT MODELS ####

# Convert retirement age to numeric
df_sample$wprage <- as.numeric(df_sample$wprage)

# Create "is.married" variable from "dimar"
df_sample$is.married <- as.numeric(ifelse(df_sample$dimar == 1, 1, 0))

# Create "is.widowed" variable from "dimar"
df_sample$is.widowed <- as.numeric(ifelse(df_sample$dimar == 3, 1, 0))

# Create "no.qual" variable from "edqual"
df_sample$no.qual <- as.numeric(ifelse(df_sample$edqual == 0, 1, 0))

# Create "income.first" variable from "eqtotinc_bu_s"
dict <- df_sample[df_sample$wave == df_sample$wavef, c("idauniq", "eqtotinc_bu_s")]
colnames(dict) <- c("idauniq", "income.first")
df_sample <- left_join(df_sample, dict)

# Create "work.first" variable from "wpactpw"
dict <- df_sample[df_sample$wave == df_sample$wavef, c("idauniq", "wpactpw")]
colnames(dict) <- c("idauniq", "work.first")
df_sample <- left_join(df_sample, dict)

# Create "volunteer.first" variable from "wpactvw"
dict <- df_sample[df_sample$wave == df_sample$wavef, c("idauniq", "wpactvw")]
colnames(dict) <- c("idauniq", "volunteer.first")
df_sample <- left_join(df_sample, dict)

# Build model
fit <- pglm(indout ~ wprage, data = df_sample, family = binomial,
           index = c("idauniq", "wave"), model = "random")

fit2 <- pglm(indout ~ wprage + couple, data = df_sample, family = binomial,
             index = c("idauniq", "wave"), model = "random")

fit3 <- pglm(indout ~ wprage + couple, data = df_sample, family = binomial(link="logit"),
             index = c("idauniq", "wave"), model = "random")

fit4 <- pglm(indout ~ wprage + couple, data = df_sample, family = binomial(link="logit"),
             index = c("idauniq", "wave"), model = "random", effect = "individual")

fit5 <- pglm(indout ~ wprage + couple + wave, data = df_sample,
             family = binomial(link="logit"),
             index = c("idauniq", "wave"), model = "random", effect = "individual")

fit6 <- pglm(indout ~ wprage + couple + wave + chinhh, data = df_sample,
             family = binomial(link="logit"),
             index = c("idauniq", "wave"), model = "random", effect = "individual")

fit7 <- pglm(indout ~ wprage + couple + chinhh, data = df_sample,
             family = binomial(link="logit"),
             index = c("idauniq", "wave"), model = "random", effect = "individual")

fit8 <- pglm(indout ~ wprage + couple + chinhh + disex, data = df_sample,
             family = binomial(link="logit"),
             index = c("idauniq", "wave"), model = "random", effect = "individual")

fit9 <- pglm(indout ~ wprage + couple + chinhh + disex + dimar, data = df_sample,
             family = binomial(link="logit"),
             index = c("idauniq", "wave"), model = "random", effect = "individual")

fit10 <- pglm(indout ~ wprage + couple + chinhh + disex + edqual, data = df_sample,
              family = binomial(link="logit"),
              index = c("idauniq", "wave"), model = "random", effect = "individual")

fit11 <- pglm(indout ~ wprage + couple + chinhh + disex + iawork, data = df_sample,
              family = binomial(link="logit"),
              index = c("idauniq", "wave"), model = "random", effect = "individual")

fit12 <- pglm(indout ~ wprage + couple + chinhh + disex + iawork + indager,
              data = df_sample, family = binomial(link="logit"),
              index = c("idauniq", "wave"), model = "random", effect = "individual")

fit13 <- pglm(indout ~ wprage + couple + chinhh + iawork + indager,
              data = df_sample, family = binomial(link="logit"),
              index = c("idauniq", "wave"), model = "random", effect = "individual")

fit14 <- pglm(indout ~ wprage + is.married + chinhh + iawork + indager,
              data = df_sample, family = binomial(link="logit"),
              index = c("idauniq", "wave"), model = "random", effect = "individual")

fit15 <- pglm(indout ~ wprage + is.married + is.widowed + chinhh + iawork + indager,
              data = df_sample, family = binomial(link="logit"),
              index = c("idauniq", "wave"), model = "random", effect = "individual")

fit16 <- pglm(indout ~ wprage + is.married + chinhh + iawork + indager + no.qual,
              data = df_sample, family = binomial(link="logit"),
              index = c("idauniq", "wave"), model = "random", effect = "individual")

fit17 <- pglm(indout ~ wprage + is.married + is.widowed + chinhh + iawork + indager
              + no.qual + work.first + volunteer.first, data = df_sample,
              family = binomial(link="logit"), index = c("idauniq", "wave"),
              model = "random", effect = "individual")

fit18 <- pglm(indout ~ wprage + is.married + is.widowed + iawork + indager + no.qual
              + work.first, data = df_sample, family = binomial(link="logit"),
              index = c("idauniq", "wave"), model = "random", effect = "individual")

best_fit <- pglm(indout ~ wprage + is.married + is.widowed + chinhh + iawork + indager
                 + no.qual + work.first, data = df_sample, family = binomial(link="logit"),
                 index = c("idauniq", "wave"), model = "random", effect = "individual")


# Summarize model
summary(fit17)
exp(coef(fit17))