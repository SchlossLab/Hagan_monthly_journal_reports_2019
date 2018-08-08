library(tidyverse)
library(lubridate)

test_data <- read_csv("processed_data/test_report_data.csv")

clean_test_data <- test_data %>% 
  mutate(`Date of Publication` = mdy(`Date of Publication`)) %>% 
  mutate(Journal = paste(str_extract(manuscript.number, "^[:alpha:]*(?=[:digit:]+)"))) %>% 
  mutate(Journal = fct_collapse(Journal, "mSystems" = c("MSYSTEMS", "mSystems"))) %>% 
  filter(Journal != "NA")

get_percent <- function(x, y){
  z <- (x/y)*100
  percent <- round(z, digits = 2)
  return(percent)
}
