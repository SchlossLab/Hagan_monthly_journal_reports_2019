# ASM_monthly_reports
automate journal monthly reports

#Data sourcing
1. Cites -- 
    + Hot Article Tracker
    + Click and hover over "Cites" column header
    + Click option to download data (top right of popup)
    + Click "Full dataset" tab
    + Download all rows as text file
1. Usage -- 
    + UV:Article Usage
    + Hot Article Tracker
    + Click and hover over "Usage" column header
    + Click option to download data (top right of popup)
    + Click "Full dataset" tab
    + Download all rows as text file
1. Manuscript metadata -- 
    + Need to automate process of sftp & unzipping of files from eJP
    + Source `run_monthly_reports.R` to automatically extract data from xml files, compile with usage/cites data, and generate the monthly reports.