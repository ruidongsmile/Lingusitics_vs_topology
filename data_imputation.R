sep_features <- function(data, feature) {
  # data[, paste(feature, "_XY", sep = "")] <- NA
  # data[, paste(feature, "_YX", sep = "")] <- NA

  which_0 <- which(data[, feature] == 0)
  which_1 <- which(data[, feature] == 1)
  which_2 <- which(data[, feature] == 2)
  which_3 <- which(data[, feature] == 3)


  data[which_1, paste(feature, "_XY", sep = "")] <- 1
  data[which_1, paste(feature, "_YX", sep = "")] <- 0

  data[which_2, paste(feature, "_XY", sep = "")] <- 0
  data[which_2, paste(feature, "_YX", sep = "")] <- 1

  data[which_3, paste(feature, "_XY", sep = "")] <- 1
  data[which_3, paste(feature, "_YX", sep = "")] <- 1

  data[which_0, paste(feature, "_XY", sep = "")] <- 0
  data[which_0, paste(feature, "_YX", sep = "")] <- 0

  # data[, paste(feature, "_XY", sep = "")] <- factor(data[, paste(feature, "_XY", sep = "")])
  # data[, paste(feature, "_YX", sep = "")] <- factor(data[, paste(feature, "_YX", sep = "")])

  new_features <- c(paste(feature, "_XY", sep = ""), paste(feature, "_YX", sep = ""))
  data[, new_features] <- lapply(data[, new_features], factor)

  data <- subset(data, select = -c(which(colnames(data)==feature)))
  return(data)
}

glotto_dummy_col <- function(dataframe){
  features <- intersect(c("GB024", "GB025", "GB065", "GB130", "GB193", "GB203"), colnames(dataframe))
  for (x in features){
    dataframe <- sep_features(data = dataframe, feature = x)
  }
  return(dataframe)
}

glottoconvert_dummy <- function(glottodata_df, features){
  glottocode <- glottodata_df[, "glottocode"]
  glottodata_dummy <- data.frame(glottocode)

  for (var in features){
    glottodata_dummy[, paste(var, "0", sep=":")] <- 0
    glottodata_dummy[, paste(var, "1", sep=":")] <- 0

    glottodata_dummy[which(gb_data[, var] == 1), paste(var, "1", sep=":")] <- 1
    glottodata_dummy[, paste(var, "1", sep=":")] <- as.factor(glottodata_dummy[, paste(var, "1", sep=":")])

    glottodata_dummy[which(gb_data[, var] == 0), paste(var, "0", sep=":")] <- 1
    glottodata_dummy[, paste(var, "0", sep=":")] <- as.factor(glottodata_dummy[, paste(var, "0", sep=":")])
  }
  return(glottodata_dummy)
}
