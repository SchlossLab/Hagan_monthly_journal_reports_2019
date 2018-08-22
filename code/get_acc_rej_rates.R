#**Acceptance & rejection rates (YTD?)**
#  
#Columns: journal, percent accept, reject, and editorial reject
#Row: total
#Data needed: journal, ejp decisions

acc_rej_data <- clean_test_data %>% 
  select(Journal, manuscript.number, EJP.decision, version.reviewed) %>% #pull needed data
  filter(EJP.decision == "Reject"|EJP.decision == "Accept, no revision") %>% unique() #limit to reject/accept decisions

all_desc <- acc_rej_data %>% group_by(Journal) %>% 
  summarise(Total = n()) %>% as.data.frame() #count rej/acc decisions by journal

total_row <- tibble(Journal = "All", Total = sum(all_desc[1:15,2])) #generate total row

all_desc <- all_desc %>% rbind(total_row) # bind total decisions to journal

ed_reject <- acc_rej_data %>% filter(EJP.decision == "Reject" & is.na(version.reviewed)) %>% #editorial rejections
  group_by(Journal) %>% summarise(Editorial_rejections = n()) %>% #count by journal
  as.data.frame() #convert to dataframe

ed_rej_row <- tibble(Journal = "All", Editorial_rejections = sum(ed_reject[1:15,2])) #generate total row for ed rej

ed_reject <- ed_reject %>% rbind(ed_rej_row) %>% #bind total ed rej row
  mutate(percent_ed_rej = get_percent(Editorial_rejections, all_desc$Total)) #add column counting percents

reject <- acc_rej_data %>% filter(EJP.decision == "Reject" & !is.na(version.reviewed)) %>% #rejections following review
  group_by(Journal) %>% summarise(Rejected = n()) %>% as.data.frame() #count by journal & convert to df

rej_row <- tibble(Journal = "All", Rejected = sum(reject[1:15,2])) #generate total row for rejections

reject <- reject %>% rbind(rej_row) %>% #add total row
  mutate(percent_rej = get_percent(Rejected, all_desc$Total)) #add column counting percents

accept <- acc_rej_data %>% filter(EJP.decision == "Accept, no revision") %>% #accepts
  group_by(Journal) %>% summarise(Accepted = n()) %>% as.data.frame() #count by journals & convert to df

accept_row <- tibble(Journal = "All", Accepted = sum(accept[1:15,2])) #generate row for total accepts

accept <- accept %>% rbind(accept_row) %>% #add total accepts row
  mutate(percent_accepted = get_percent(Accepted, all_desc$Total)) #add column counting percents

acc_rej_summary <- bind_cols(ed_reject, reject[,2:3], accept[,2:3]) #bind into single df
