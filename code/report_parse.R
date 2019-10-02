source("code/report_xml_parse_functions.R") #source functions & libraries

#generate list of xml files in the directory containing most recent versions of all manuscripts
all_xml_list <- list.files(path = "data/ejp_transfer_comp_decry", 
  pattern = "\\.xml$", full.names = TRUE, recursive = TRUE) 

#parse & get top root of all xml files, req'd for parsing
xml_top_list <- lapply(all_xml_list, get_top) 

print("Starting report parse")

#loop through each xml for manuscript data
report_parse <- map_df(xml_top_list, parse_xml) 

print("Completed report parse")

#save CSV file of all review related data
write_csv(report_parse, paste0("processed_data/report_parse_", this_ym, ".csv"), col_names = TRUE) 

print(paste0("Saved processed_data/report_parse_", this_ym, ".csv"))

#Next steps: run report(s)