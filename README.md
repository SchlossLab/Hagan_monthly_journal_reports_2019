# ASM_monthly_reports
automate journal monthly reports

#Data sourcing
1. Cites -- 
    + Hot Article Tracker
    + Click and hover over "Cites" column header
    + Click option to download data (top right of popup)
    + Click "Full data" tab
    + Download all rows as text file
1. Usage -- 
    + UV:Article Usage
    + Hot Article Tracker
    + Click and hover over "Usage" column header
    + Click option to download data (top right of popup)
    + Download all rows as text file
1. Manuscript metadata -- 
    + Need to automate process of sftp & unzipping of files from eJP
    + Source `run_monthly_reports.R` to automatically extract data from xml files, compile with usage/cites data, and generate the monthly reports.

#R Package Dependencies
1. knitr
2. kableExtra
3. markdown
4. rmarkdown
5. tidyverse
6. lubridate
7. XML
8. scales
9. RColorBrewer
    
#Workflow
1. Download Cites data. Save as "cites.csv" and place in `processed_data`
2. Download Usage data. Save as "usage.csv" and place in `processed_data`
3. Run `report_data.pbs` to:
  a. Import and unzip ejp data files
  b. Generate reports (via `run_monthly_reports.R`)
  c. Encrypt raw ejp data and delete unencrypted raw data files.