library(tidyverse)
library(lubridate) #math w. dates

#read in data & restrict to manuscripts submitted in the last 4 years
data <- read_csv(paste0("processed_data/report_data", this_ym,".csv")) %>% 
  filter(floor_date(ymd_hms(approved.date), "month") != floor_date(today(), "month")) %>% 
  filter(floor_date(ymd_hms(approved.date), "month") >= floor_date(today(), "month") - ((years(4)+months(1)))) 

this_year <- today() %>% year()

this_month <- today() %>% month()

#function to calculate percent and round to 2 decimal places
get_percent <- function(x, y){
  z <- (as.numeric(x)/as.numeric(y))*100
  percent <- round(z, digits = 2)
  return(percent)
}

#function to generate a dataframe that includes all variables, regardless of value (or lack there of)
test_list <- function(all, decision, group){
  
  missing_list <- anti_join(all, decision, by = paste(group)) %>% pull(paste(group)) %>% #identify which items aren't listed
    .[. != "All"|. != "Total"]
  
  missing_df <- if(length(missing_list) == 0) paste("skip") else(
    cbind(missing_list, as.tibble("0")) %>% #generate df of missing items w/ na value
      setNames(., c(paste(group), colnames(decision)[2])) #rename columns (setNames allows conditional naming)
  )
  
  total <- decision %>% filter(.[1] == "All"|.[1] == "Total") #get the total row by itself
  
  corrected_decision <- if(str_detect(missing_df, "skip")) decision else(
    rbind(decision, missing_df) %>% #bind missing journals to decision table
      arrange_(group) %>% #put first column in alphabetical order
      filter(.[1] != "All") %>% filter( .[1] != "Total") %>% rbind(total)) #move the total/all row to the bottom
  
  return(corrected_decision)
}

#function to restrict selection to manuscripts with date X that occurs in the last 12 months
filter_12_mo <- function(x, date){
  filter(x, date %within% interval(start = (today() - months(12)), end = today()))
}

#function to restrict selection to manuscripts with date X that occurs in the previous 12 to 36 months -- relevant cites timeframe
filter_12_to_36_mo <- function(x, date){
  filter(x, date %within% interval(start = (today() - months(36)), end = today() - months(12)))
}

#function to group categories
collapse_cats <- function(x){
  fct_collapse(x,
                                 "Antimicrobials" = c("Antibacterial and antifungal agents",
                                                      "Antiviral agents", "Antiviral Agents",
                                                      "Pharmacology", "Experimental Therapeutics", 
                                                      "Biologic Response Modifiers"),
                                 "Clinical" = c("Clinical Immunology and Vaccines",
                                                "Clinical Microbiology Best Practices",
                                                "Clinical Therapeutics",
                                                "Clinical Veterinary Microbiology", 
                                                "Fungal and Parasitic Infections",
                                                "Bacterial Infections",
                                                "Analytical Procedures",
                                                "Practical Guidance for Clinical Microbiology (formerly &lt;i&gt;Cumitechs&lt;/i&gt;)"),
                                 "Virology" = c("Virology", "Virus-Cell Interactions", "Viruses",
                                                "(-) strand RNA viruses, double-strand RNA viruses",
                                                "(+) strand RNA viruses, including retroviruses/HIV",
                                                "Structure and Assembly", "DNA viruses",
                                                "Transformation and Oncogenesis"),
                                 "Mycology" = c("Fungi and yeast", "Mycology",
                                                "Mycobacteriology and Aerobic Actinomycetes"),
                                 "Parasitology" = c("Parasites", "Parasitology"),
                                 "Pathogenesis & Immunity" = c("Pathogenesis", 
                                                               "Pathogenesis and Immunity", 
                                                               "Molecular Pathogenesis", 
                                                               "Cellular Microbiology: Pathogen-Host Cell Molecular Interactions", 
                                                               "Host Response and Inflammation",
                                                               "Cellular Response to Infection",
                                                               "Susceptibility"),
                                 "Vaccine Immunology" = c("Microbial Immunity and Vaccines", 
                                                          "Vaccines and Antiviral Agents"),
                                 "Public Health" = c("Epidemiology", "Epidemiology and Surveillance",
                                                     "Public and Environmental Health Microbiology",
                                                     "Food Microbiology"),
                                 "Omics" = c("Metagenomics and bioinformatics", "Molecular Genomics",
                                             "Microbial Genomics"),
                                 "Host Microbiomes" = c("Host-Associated Microbial Communities", 
                                                        "Host-Microbiome Relationships", 
                                                        "Invertebrate Microbiology", "Microbiome"),
                                 "Physiology" = c("Microbial Metabolism", 
                                                  "Mechanisms of Action: Physiological Effects"),
                                 "Ecology" = c("Microbial Ecology", "Geomicrobiology", 
                                               "Environmental Microbiology", 
                                               "Marine Environmental Microbiology", 
                                               "Plant Microbiology", "Symbiosis", 
                                               "Terrestrial Environmental Microbiology"),
                                 "Protein Mechanism" = c("Mechanisms of Resistance", 
                                                         "Enzymology and Protein Engineering"),
                                 "Evolution & Genetics" = c("Gene Delivery", 
                                                            "Genetic Diversity and Evolution",
                                                            "Genetics and Molecular Biology", 
                                                            "Genome Replication and Regulation of Viral Gene Expression", 
                                                            "Evolutionary and Genomic Microbiology"),
                                 "Biochemistry" = c("Cellular Biochemistry", "Chemistry; Biosynthesis"),
                                 "Modeling" = c("Community Modeling", "Microbial Network Modeling"),
                                 "Misc" = c("Brief Case", "Challenging Clinical Case", 
                                            "Biographical Feature", "Commentary", "Editorial", "Gem",
                                            "Letter to the Editor", "Meeting Presentations", "Meeting Review", 
                                            "Photo Quiz", "Point-Counterpoint",
                                            "Retraction", "Spotlight", "Author Correction", "Author&apos;s Correction",
                                            "Erratum"),
                                 "Reviews" = c("Reviews", "Minireview"),
                                 "Methods" = c("Methods", "Immunoassays")
  )
}