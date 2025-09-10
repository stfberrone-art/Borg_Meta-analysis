
## install/load packages
install.packages(c("readxl", "dplyr", "metafor"), dependencies = TRUE)

library(readxl)
library(dplyr)
library(metafor)


## Import data from Excel

borg <- read_excel("Data_Borg.xlsx")
View(borg)


## Cleaning (ensure numeric r and n)
## Handles decimal commas like "0,83"

to_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub(",", ".", as.character(x))
  suppressWarnings(as.numeric(x))
}

borg <- borg %>%
  mutate(
    r            = to_num(r),
    n            = to_num(n),
    validity_type = as.character(validity_type),
    comparator    = as.character(comparator),
    prom          = as.character(prom),
    author_year   = as.character(author_year)
  )

##check the variables type

glimpse(borg)


## Analysis 1. Filter to: criterion validity + Borg 6–20 only
## Exclude CR10/CR100 using 'prom'
## Keep typical 6–20 naming variants

criterion <- borg %>%
  filter(validity_type == "criterion") %>%
  filter(prom != "BorgCR10")

## Compute Fisher's Z effect sizes for r

criterion_es <- escalc(
  measure = "ZCOR",
  ri = r,
  ni = n,
  data = criterion
)

## Fit random-effects meta-analysis (REML)

res <- rma(yi, vi, data = criterion_es, method = "REML")


## Forest plot 

slabs <- criterion$author_year
forest(
  res,
  slab   = slabs,
  xlab   = "Correlation (r)",
  transf = transf.ztor
)








  

