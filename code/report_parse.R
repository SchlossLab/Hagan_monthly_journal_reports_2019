source("code/report_xml_parse_functions.R") #source functions & libraries

all_xml_list <- list.files(path = " ", pattern = "\\.xml$", full.names = TRUE, recursive = TRUE) #generate list of xml files from all journals files

xml_top_list <- lapply(all_xml_list, get_top) #parse & get top root of all xml files

print("Starting report parse")

report_parse <- map_df(xml_top_list, parse_xml) #parse for manuscript info

print("Completed report parse")

date <- today() %>% str_replace_all(., "-", "")

write_csv(all_reviews, paste0("processed_data/report_parse_", date, ".csv"), col_names = TRUE) #save CSV file of all review related data

print(paste0("Saved processed_data/report_parse_", date, ".csv"))

#Next steps: run report(s)