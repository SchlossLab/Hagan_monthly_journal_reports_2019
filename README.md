# ASM_monthly_reports
automate journal monthly reports

#Data sourcing
1. Cites -- 
    + Hot Article Tracker
    + Hot Article Tracker
    + Set "Year of Publication" to "Last 4 years"
    + Click and hover over "Cites" column header
    + Click option to view data (top right of popup)
    + Click "Full data" tab
    + Download all rows as text file
1. Usage -- 
    + UV:Article Usage
    + Hot Article Tracker
    + Move "Article Date of Publication" slider to reflect the last 3 years.
    + Click and hover over "Usage" column header
    + Click option to download data (top right of popup)
    + Download all rows as text file
1. Manuscript metadata -- 
    + Run the following command to retrieve updated files from ejpress:
    `sftp -b ../sftp_batch ejpress`

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
  a. Unzip ejp data files
  b. Generate reports (via `run_monthly_reports.R`)
  c. Encrypt raw ejp data and delete unencrypted raw data files