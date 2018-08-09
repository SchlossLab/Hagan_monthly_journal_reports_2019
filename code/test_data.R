library(tidyverse)
library(lubridate)
library(xml2)

test_data <- read_csv("processed_data/test_report_data.csv")

editor_fix <- test_data %>% pull(Editor) %>% replace_special() %>% 
  str_replace_all("(?<=;\\s[:upper:]).*", "\\.") %>% as.data.frame() %>% na_if("NA") 
names(editor_fix) <- "editor_fix"

clean_test_data <- cbind(test_data, editor_fix) %>% 
  mutate(`Date of Publication` = mdy(`Date of Publication`)) %>% 
  mutate(Journal = paste(str_extract(manuscript.number, "^[:alpha:]*(?=[:digit:]+)"))) %>% 
  mutate(Journal = fct_collapse(Journal, "mSystems" = c("MSYSTEMS", "mSystems"))) %>% 
  filter(Journal != "NA")

get_percent <- function(x, y){
  z <- (x/y)*100
  percent <- round(z, digits = 2)
  return(percent)
}

replace_special <- function(x){  #function to replace special characters with standard alphabet letters
  case_when(#these regex expressions won't work when running R on windows
    str_detect(x, fixed("\xf6")) ~ str_replace(x, fixed("\xf6"), "o"), #replace with "o"
    str_detect(x, fixed("\xfc")) ~ str_replace(x, fixed("\xfc"), "u"), #replace with "u"
    TRUE ~ paste(x)) #keep original value otherwise
}
