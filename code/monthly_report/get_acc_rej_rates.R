#**Acceptance & rejection rates (YTD?)**
#  
#Columns: journal, percent accept, reject, and editorial reject
#Row: total
#Data needed: journal, ejp decisions

acc_rej_data <- data %>% 
  filter_12_mo(., ymd_hms(.$decision.date)) %>% #limit to decisions made this year
  select(journal, manuscript.number, ejp.decision, reviewed) %>% #pull needed data
  filter(ejp.decision == "Reject"|ejp.decision == "Accept, no revision") %>% #limit to reject/accept decisions
  unique() #this year only

#count rej/acc decisions ----
all_desc <- acc_rej_data %>% group_by(journal) %>% #by journal
  summarise(total = n()) %>% as.data.frame() 

total_row <- tibble(journal = "All", total = sum(all_desc[1:dim(all_desc)[1],2])) #all journals combined

all_desc <- all_desc %>% rbind(total_row) #bind combined journal decisions to each journal

#calculate editorial rejection rates----
ed_reject <- acc_rej_data %>% 
  filter(ejp.decision == "Reject" & reviewed == "no") %>% #select editorial rejections
  group_by(journal) %>% summarise(editorial_rejections = n()) %>% #count by journal
  as.data.frame() #convert to dataframe

ed_rej_row <- tibble(journal = "All", editorial_rejections = sum(ed_reject[1:nrow(ed_reject),2])) #editorial rejections for all journals combined

ed_reject <- ed_reject %>% rbind(ed_rej_row) %>% #bind total ed rej row
  test_list(all_desc, ., "journal") %>% #generate table including all journals
  mutate(percent_ed_rej = get_percent(editorial_rejections, all_desc$total)) #add column calculating percents

#calculate rejection rates----
reject <- acc_rej_data %>% 
  filter(ejp.decision == "Reject" & reviewed == "yes") %>% #rejections following review
  group_by(journal) %>% summarise(rejected = n()) %>% as.data.frame() #count by journal 

rej_row <- tibble(journal = "All", rejected = sum(reject[1:nrow(reject),2])) #rejections from all journals combined

reject <- reject %>% rbind(rej_row) %>% #bind total rejection row to each journal data
  test_list(all_desc, ., "journal") %>% #generate table including all journals
  mutate(percent_rej = get_percent(rejected, all_desc$total)) #add column calculating percents

#calculate acceptance rates----
accept <- acc_rej_data %>% 
  filter(ejp.decision == "Accept, no revision") %>% #accepts
  group_by(journal) %>% summarise(accepted = n()) %>% as.data.frame() #count by journals & convert to df

accept_row <- tibble(journal = "All", accepted = sum(accept[1:dim(accept)[1],2])) #generate row for total accepts

accept <- accept %>% rbind(accept_row) %>% #add total accepts row to each journal data
  test_list(all_desc, ., "journal") %>% #generate table including all journals
  mutate(percent_accepted = get_percent(accepted, all_desc$total)) #add column calculating percents

#summary df-----
acc_rej_summary <- bind_cols(ed_reject, reject[,2:3], accept[,2:3]) #bind into single df
