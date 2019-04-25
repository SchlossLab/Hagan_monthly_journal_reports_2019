# References for automation 
# http://www.r-bloggers.com/how-to-source-an-r-script-automatically-on-a-mac-using-automator-and-ical/
# http://www.engadget.com/2013/03/18/triggering-applescripts-from-calendar-alerts-in-mountain-lion/
# https://www.reed.edu/data-at-reed/software/R/markdown_multiple_reports.html

# File 1: Should be an R-Script 
# contains a loop that iteratively calls an Rmarkdown file (i.e. File 2)

# load packages
library(knitr)
library(kableExtra)
library(markdown)
library(rmarkdown)
library(tidyverse)
library(lubridate)

this_ym <- today() %>% str_extract(., "\\d{4}-\\d{2}") #the month & year of report generated

#parse data from xml
source("code/report_parse.R")

#load, join, & clean data
source("code/merge_clean_report_data.R")

# get dataset for report
source("code/load_report_data_functions.R")

#get preferred plot settings for ggplot
source("code/plot_options.R")


# create ASM report (this is outside the loop so it is only called once)
rmarkdown::render('code/monthly_report.Rmd',  # file 2
                  output_file =  paste0("ASM_journals_report_", this_ym, ".html"), 
                  output_dir = 'reports')

# for each journal in the data create a report
# these reports are saved in output_dir with the name specified by output_file
journals_list <- data %>% filter(journal != "genomeA") %>% pull(journal) %>% unique()

journals_list <- c("MCB")

for (each_journal in journals_list){
  rmarkdown::render('code/each_journal_report.Rmd',  # file 2
                    output_file =  paste0(each_journal, "_report_", this_ym, ".html"), 
                    output_dir = 'reports')
}
