#**Acceptance & rejection rates (YTD?)**
#  
#Columns: journal, percent accept, reject, and editorial reject
#Row: total
#Data needed: journal, ejp decisions

acc_rej_data <- data %>% 
  filter_12_mo(., ymd_hms(.$decision.date)) %>% #limit to decisions made this year
  select(journal, manuscript.number, ejp.decision, version.reviewed) %>% #pull needed data
  filter(ejp.decision == "Reject"|ejp.decision == "Accept, no revision") %>% #limit to reject/accept decisions
  unique() #this year only

all_desc <- acc_rej_data %>% group_by(journal) %>% 
  summarise(total = n()) %>% as.data.frame() #count rej/acc decisions by journal

total_row <- tibble(journal = "All", total = sum(all_desc[1:dim(all_desc)[1],2])) #generate total row

all_desc <- all_desc %>% rbind(total_row) # bind total decisions to journal

#calculate editorial rejection rates----
ed_reject <- acc_rej_data %>% filter(ejp.decision == "Reject" & is.na(version.reviewed)) %>% #editorial rejections
  group_by(journal) %>% summarise(editorial_rejections = n()) %>% #count by journal
  as.data.frame() #convert to dataframe

ed_rej_row <- tibble(journal = "All", editorial_rejections = sum(ed_reject[1:nrow(ed_reject),2])) #generate total row for ed rej

ed_reject <- ed_reject %>% rbind(ed_rej_row) %>% test_list(all_desc, ., "journal")  %>% #bind total ed rej row
  mutate(percent_ed_rej = get_percent(editorial_rejections, all_desc$total)) #add column counting percents

#calculate rejection rates----
reject <- acc_rej_data %>% filter(ejp.decision == "Reject" & !is.na(version.reviewed)) %>% #rejections following review
  group_by(journal) %>% summarise(rejected = n()) %>% as.data.frame() #count by journal & convert to df

rej_row <- tibble(journal = "All", rejected = sum(reject[1:nrow(reject),2])) #generate total row for rejections

reject <- reject %>% rbind(rej_row) %>% test_list(all_desc, ., "journal") %>%  #add total row
  mutate(percent_rej = get_percent(rejected, all_desc$total)) #add column counting percents

#calculate acceptance rates----
accept <- acc_rej_data %>% filter(ejp.decision == "Accept, no revision") %>% #accepts
  group_by(journal) %>% summarise(accepted = n()) %>% as.data.frame() #count by journals & convert to df

accept_row <- tibble(journal = "All", accepted = sum(accept[1:dim(accept)[1],2])) #generate row for total accepts

accept <- accept %>% rbind(accept_row) %>% test_list(all_desc, ., "journal")  %>% #add total accepts row
  mutate(percent_accepted = get_percent(accepted, all_desc$total)) #add column counting percents

#summary df-----
acc_rej_summary <- bind_cols(ed_reject, reject[,2:3], accept[,2:3]) #bind into single df
