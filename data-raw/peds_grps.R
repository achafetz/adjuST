#Set peds age groups for grouping purposes

peds <- c("<01", "01-04", "05-09", "10-14", "<15")
peds_o01 <- c("01-04", "05-09", "10-14", "<15")

use_data(peds, overwrite = TRUE)
use_data(peds_o01, overwrite = TRUE)

rm(peds, peds_o01)
