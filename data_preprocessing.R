library(glottospace)
library(mice)
library(ca)
library(fastDummies)
library(TDAstats)
library(TDA)
library(rgl)
library(ggplot2)
library(ggpubr)
library(ggforce)
library(ggrepel)
library(readxl)

source("data_imputation.R")
# source("data_points.R")

set.seed(42)

data_gb <- glottoget(glottodata = "grambank", download = F)
data_sa <- grambank[data_gb$macroarea == "South America", c(1, 2, 8:11, 13:207)]
# Remove two languages in level of dialects
data_sa <- subset(data_sa, !(id %in% c("katu1276", "tere1281")))
glottodata <- glottocreate(glottocodes = data_sa$id, variables = colnames(data_sa)[7:ncol(data_sa)])
glottodata$glottodata[, colnames(glottodata$glottodata)[-1]] <- data_sa[, c(7:ncol(data_sa))]
glottodata$structure$type <- "factor"
glottoclean_gb <- glottoclean(glottodata, tona = "?")

# The proportion of missing values for each feature
missing_ratio_col <- colSums(is.na(glottoclean_gb$glottodata[, colnames(glottoclean_gb$glottodata)[-1]])) / nrow(glottoclean_gb$glottodata)
which_col <- names(which(missing_ratio_col < 0.2))

data_col_filtered <- glottoclean_gb$glottodata[, c("glottocode", which_col)]

missing_ratio_row <- rowSums(is.na(data_col_filtered)) / length(which_col)
which_row <- which(missing_ratio_row < 0.2)
data_1 <- data_col_filtered[which_row, ]
data_1[, c(2:ncol(data_1))] <- lapply(data_1[, c(2:ncol(data_1))], factor)
glottodata_1 <- glottocreate(glottocodes = data_1$glottocode, variables = colnames(data_1)[2:ncol(data_1)])
glottodata_1$glottodata[, 2:ncol(data_1)] <- data_1[, c(2:ncol(data_1))]
glottodata_1$structure$type <- "factor"

glottoclean_1 <- glottoclean(glottodata_1)

md.pattern(glottoclean_1$glottodata[, 2:ncol(glottoclean_1$glottodata)])

ini <- mice(glottoclean_1$glottodata[, 2:ncol(glottoclean_1$glottodata)],
            maxit = 0)
imp <- mice(glottoclean_1$glottodata[, 2:ncol(glottoclean_1$glottodata)], method = "rf", maxit = 10, print=F)

md.pattern(complete(imp))

glottoclean_1$glottodata[, colnames(glottoclean_1$glottodata)[-1]] <- complete(imp)

data_2 <- glotto_dummy_col(glottoclean_1$glottodata)

glottodata_3 <- glottocreate(glottocodes = data_2$glottocode, variables = colnames(data_2)[-1])
glottodata_3$glottodata[, colnames(data_2)[-1]] <- data_2[, colnames(data_2)[-1]]
glottodata_3$structure$type <- "factor"

gb_data <- data_2
gb_data["family_name"] <- data_sa[data_sa$id %in% data_2$glottocode, "family_name"]
gb_data <- gb_data[, c(1, ncol(gb_data), 2:(ncol(gb_data) - 1))]

gb_dummy <- glottoconvert_dummy(gb_data, features = colnames(gb_data)[-c(1:2)])

save(gb_data, file = "gb_data.Rdata")
save(gb_dummy, file = "gb_dummy.Rdata")

















































