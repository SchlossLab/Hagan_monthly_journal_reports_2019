#Box plot of citations/month by article category type (all, previous 12 mo?)
#Data needed: doi, WoS citation stats, manuscript type

cites_data <- data %>% 
  filter_12_to_36_mo(., .$publication.date) %>% 
  filter(measure.names == "Total Article Cites")

journal_cites_plot <- cites_data %>% 
  ggplot()+
  geom_boxplot(aes(x = journal, y = measure.values))+
  labs(x = "Journal", y = "Article Cites", title = " Article Cites Based on Journal",
       subtitle = "Articles published in the previous 12 to 36 months")+
  my_theme

#categories <- category_cites_data %>% group_by(category) %>% summarise(n = n())

category_cites_data <- cites_data %>% 
  mutate(category = fct_collapse(category,
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
  ))

cat_cites_plot <- category_cites_data %>% ggplot()+
  geom_boxplot(aes(x=category, y=measure.values))+
  labs(x = "Category", y = "Article Cites",
       title = " Article Cites Based on Category",
       subtitle = "Articles published in the previous 12 to 36 months",
       caption = "NA category = All mBio & MCB publications, plus assorted items from AAC, AEM, JB, JCM, JVI, mSphere, & mSystems")+
  coord_flip()+
  my_theme
