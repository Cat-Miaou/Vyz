library(readr)


dta_dnb <- read_delim("Données brutes/dnb.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
dta_eff <- read_delim("Données brutes/effectifs.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
dta_ips <- read_delim("Données brutes/ips.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

dta_eff$Session <- dta_eff$`Rentrée scolaire` + 1
dta_eff <- dta_eff[-1]

dta_ips$Session <- 2021
dta_ips <- dta_ips[-1]

test <- merge(dta_dnb, dta_eff, by = c("UAI", "Session"))

merge(table1, table2, by = c("col1", "col2"), all = FALSE) # Inner join
print(result)