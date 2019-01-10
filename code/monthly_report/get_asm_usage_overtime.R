#stats over time (smooth dot plot, one line for each stat)

drop.measure.names <- c("Measure By", "Article Cites / Month", "Published Months", "Total Article Cites")

stats_data <- data %>% 
  filter_12_mo(., .$publication.date) %>% #published in last 12 months
  filter(!(measure.names %in% drop.measure.names)) %>% 
  mutate(measure.values.per.month = measure.values/months.published) %>% 
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

#all_stats_data %>% group_by(journal, measure.names) %>% #  summarise(total = sum(measure.values))

#abstract_time <- stats_data %>% 
#  filter(measure.names == "Abstract") %>% 
#  ggplot()+
#  geom_line(aes(x=publication.date, y=measure.values, color = journal))+
#  facet_wrap(~journal, scales = "free", ncol = 2)+
#  labs(x = "Publication Date",
#       y = "Abstract Views",
#       title = "Abstract Views",
#       subtitle = "Articles published in last 12 months")+
#  my_theme_horiz
#
#html_time <- stats_data %>% 
#  filter(measure.names == "HTML") %>% 
#  ggplot()+
#  geom_line(aes(x=publication.date, y=measure.values, color = journal))+
#  facet_wrap(~journal, scales = "free", ncol = 2)+
#  labs(x = "Publication Date",
#       y = "HTML Views",
#       title = "HTML Views",
#       subtitle = "Articles published in last 12 months")+
#  my_theme_horiz

asm_time <- stats_data %>% 
  group_by(journal, floor_date(ymd(publication.date), unit = "month"), measure.names) %>% 
  summarise(total.views = sum(measure.values)) %>% 
  #filter(measure.names == "PDF") %>% 
  ggplot()+
  geom_line(aes(x=`floor_date(ymd(publication.date), unit = "month")`, y=total.views, color = measure.names, group = measure.names))+
  facet_wrap( ~ journal, scales = "free_y", shrink = TRUE,
              strip.position = "right", ncol = 1)+
  labs(x = "Month of Publication",
       y = "Views",
       title = "Total Article Views by Journal",
       subtitle = "For articles published in the last 12 months",
       color = "Measures")+
  my_theme_leg

pdf_cat <- stats_data %>% 
  filter(measure.names == "PDF") %>% 
  group_by(category, floor_date(ymd(publication.date), unit = "month"), measure.names) %>% 
  summarise(total.views = sum(measure.values)) %>% 
  ggplot()+
  geom_line(aes(x=`floor_date(ymd(publication.date), unit = "month")`, y=total.views, group = category))+
  #coord_cartesian(ylim = c(0,5000))+
  facet_wrap( ~ category,  
              strip.position = "right", ncol = 4, labeller = label_wrap_gen(width = 13))+
  labs(x = "Month of Publication",
       y = "PDF Views",
       title = "PDF Views by Category",
       subtitle = "For articles published in the last 12 months",
       caption = "NA category = All mBio & MCB publications, plus assorted items from AAC, AEM, JB, JCM, JVI, mSphere, & mSystems")+
  my_theme

html_cat <- stats_data %>% 
  filter(measure.names == "HTML") %>% 
  group_by(category, floor_date(ymd(publication.date), unit = "month"), measure.names) %>% 
  summarise(total.views = sum(measure.values)) %>% 
  ggplot()+
  geom_line(aes(x=`floor_date(ymd(publication.date), unit = "month")`, y=total.views, group = category))+
  #coord_cartesian(ylim = c(0,5000))+
  facet_wrap( ~ category, ncol = 4,  
              strip.position = "right", labeller = label_wrap_gen(width = 13))+
  labs(x = "Month of Publication",
       y = "HTML Views",
       title = "HTML Views by Category",
       subtitle = "For articles published in the last 12 months",
       caption = "NA category = All mBio & MCB publications, plus assorted items from AAC, AEM, JB, JCM, JVI, mSphere, & mSystems")+
  my_theme

abstract_cat <- stats_data %>% 
  filter(measure.names == "Abstract") %>% 
  group_by(category, floor_date(ymd(publication.date), unit = "month"), measure.names) %>% 
  summarise(total.views = sum(measure.values)) %>% 
  ggplot()+
  geom_line(aes(x=`floor_date(ymd(publication.date), unit = "month")`, y=total.views, group = category))+
  #coord_cartesian(ylim = c(0,10000))+
  facet_wrap( ~ category, ncol = 4, 
              strip.position = "right", labeller = label_wrap_gen(width = 13))+
  labs(x = "Month of Publication",
       y = "Abstract Views",
       title = "Abstract Views by Category",
       subtitle = "For articles published in the last 12 months",
       caption = "NA category = All mBio & MCB publications, plus assorted items from AAC, AEM, JB, JCM, JVI, mSphere, & mSystems")+
  my_theme
