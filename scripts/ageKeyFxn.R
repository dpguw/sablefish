ageKey<-function (key, formula, data, type = c("SR", "CR"), len.breaks = NULL) 
{
  #from library FSA
  #source("http://www.rforge.net/FSA/InstallFSA.R")
  ageKey.sr <- function(key, age.cats, data, data.len.cats, 
                        ca) {
    for (i in data.len.cats) {
      len.n <- dim(data[data$LCat == i, ])[1]
      age.prob <- key[which(as.numeric(rownames(key)) == 
                              i), ]
      age.freq <- floor(len.n * age.prob)
      ages <- rep(age.cats, age.freq)
      if (length(ages) < len.n) {
        num.add <- len.n - length(ages)
        ages.add <- sample(age.cats, num.add, replace = TRUE, 
                           prob = age.prob)
        ages <- c(ages, ages.add)
      }
      if (length(ages) > 1) {
        ages <- sample(ages, length(ages), replace = FALSE)
      }
      data[data$LCat == i, ca] <- ages
    }
    data
  }
  ageKey.cr <- function(key, age.cats, data, ca) {
    for (i in 1:dim(data)[1]) {
      age.prob <- key[which(as.numeric(rownames(key)) == 
                              data$LCat[i]), ]
      data[i, ca] <- sample(age.cats, 1, prob = age.prob)
    }
    data
  }
  type <- match.arg(type)
  cl <- getVarFromFormula(formula, data)
  if (length(cl) == 1) 
    ca <- "age"
  else {
    ca <- cl[1]
    cl <- cl[2]
  }
  if (any(key > 1, na.rm = TRUE)) 
    key <- prop.table(key, 1)
  key.row.sum <- apply(key, 1, sum)
  key.row.sum <- key.row.sum[!is.na(key.row.sum) & key.row.sum != 
                               0]
  if (any(key.row.sum != 1)) 
    warning("Key contains row that does not sum to 1.", call. = FALSE)
  da.len.cats <- as.numeric(names(key.row.sum))
  if (min(data[, cl]) < min(da.len.cats)) {
    stop(paste("The minimum observed length in the length sample (", 
               min(data[, cl]), ") is less than the\n                   smallest length category in the age-length key (", 
               min(da.len.cats), ").  You should include fish\n                   of these lengths in your age sample or exclude fish of this length from your length sample.\n", 
               sep = ""), call. = FALSE)
  }
  if (max(data[, cl]) > max(da.len.cats)) {
    warning(paste("The maximum observed length in the length sample (", 
                  max(data[, cl]), ") is greater than the\n                   largest length category in the age-length key (", 
                  max(da.len.cats), "). Thus,\n                   the last length category will be treated as all-inclusive.\n", 
                  sep = ""), call. = FALSE)
  }
  if (is.null(len.breaks)) 
    len.breaks <- da.len.cats
  options(warn = -1)
  data <- lencat(as.formula(paste("~", cl)), data = data, breaks = len.breaks, 
                 as.fact = FALSE)
  options(warn = 1)
  data.len.cats <- as.numeric(names(table(data$LCat)))
  if (!any(names(data) == ca)) {
    data <- data.frame(data, rep(NA, length(data[, cl])))
    names(data)[ncol(data)] <- ca
  }
  age.cats <- as.numeric(colnames(key))
  switch(type, SR = , Sr = , sr = , S = , s = {
    data <- ageKey.sr(key, age.cats, data, data.len.cats, 
                      ca)
  }, CR = , Cr = , cr = , C = , c = {
    data <- ageKey.cr(key, age.cats, data, ca)
  })
  data <- data[, -which(names(data) == "LCat")]
  data
}