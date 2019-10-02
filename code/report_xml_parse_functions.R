library(XML) #for extracting data from XML files
library(tidyverse)
library(lubridate) #for dealing with dates

#function to parse XML doc & find the root node before running further parsing functions
get_top <- function(input_XML){
  xmldoc1 <- xmlParse(input_XML) #parse
  xmltop <- xmlRoot(xmldoc1) #find rootnode - enables pulling node text
  return(xmltop)
}  

#function to generate a named column from the xml data of a single type, use tryCatch to avoid errors due to missing data
get_column <- function(input_xml, node, newname){ #input of source file, node relative path, and name for column
  column <- tryCatch(
    setNames(xmlToDataFrame( #name column generated from xmlToDataFrame
      nodes = getNodeSet(input_xml, node)), #retrieve text from all nodes with the node path & place in df
      newname), #name for column
    error = function(e) {setNames(as.data.frame("NA"), newname)} #return an empty column if data are absent
  )
}

#function to fill empty nodes with "NA" to maintain dataframes when data are missing
fill_if_null <- function(input){
  when(
    is.null(input) ~ paste("NA"),
    !is.null(input) ~ paste(input))
}

#extract data required for the reports from each manuscript
#due to overlap between variable names and column names, variable names have _ & column names . as spacers
parse_xml <- function(input_xmltop){
  
  #Unique manuscript identifier
  manu_number <- get_column(input_xmltop, "//manuscript-number", "manuscript.number") %>% head(n=1)
  
  #person ids for editor data
  editor_id <- get_column(input_xmltop, "//editor-person-id", "person.id") #all editors assigned
  sen_editor_id <- get_column(input_xmltop, "//senior-editor-person-id", "person.id") #senior editor assigned

  #person identifiers and demographic data - links names to person ids
  persons <- get_column(input_xmltop, "//person", "person")#all info for all persons
  names(persons) <- c("address", "area.of.interest", "first.name", "gender", "gender.count", "gender.probability", "institution", "last.name", "middle.name", "person.id", "title") #assign functional names
  
  person_data <- cbind(data.frame(manu_number), persons) %>% #add manuscript identifier to person data
    select(manuscript.number, first.name, last.name, person.id) #only keep name & person id info
  
  #pool person data by first assigning individuals their "role"
  role <- c("editor", "senior.editor")

  editor <- cbind(data.frame(role[1]), #first column contains individuals' role
                  person.id = editor_id) %>% #add editor person ids
    rename(role = role.1.)
  
  senior_editor <- cbind(data.frame(role[2]), #first column contains individuals' role
                         person.id = sen_editor_id) %>% #add senior editor person ids
    rename(role = role.2.) 

  #final dataframe of people, their roles, and demographics
  people <- list(editor, senior_editor) %>% #compile variables into list
    reduce(full_join, by = c("person.id", "role")) %>% #merge editor & senior editor ids & roles
    left_join(person_data, by = "person.id") %>% #merge ids and roles with the identifying data using person.id
    dplyr::distinct() %>% #filter duplicate entries (an issue if multiple versions)
    filter(role != "NA", person.id != "NA") #filter out non-editor persons (e.g., authors, reviewers)
  
  #parse publication-related data
  doi <- get_column(input_xmltop, "//production-data-doi", "doi") #associated doi number
  ready_for_production_date <- get_column(input_xmltop, "//production-data-ready-for-production-date", "ready.for.production.date") #when it was "completed"
  published_online_date <- get_column(input_xmltop, "//production-data-online-publication-date", "published.online.date")
  journal_title <- get_column(input_xmltop, "//journal-abbrev", "journal")

  #version specific information - specified the desired information that is possible in all versions
  version <- get_column(input_xmltop, "//version-number", "version") #version
  approved_date <- get_column(input_xmltop, "//version/author-approval-date", "approved.date") #version approved by author date
  submitted_date <- get_column(input_xmltop, "//version/submission-date", "submitted.date") #version submission date
  decision_date <- get_column(input_xmltop, "//version/decision-date", "decision.date") #version decision date
  decision <- get_column(input_xmltop, "//version/ejp-decision", "EJP.decision") #final decision
  
  #Manuscript metadata - included in every version, but probably doesn't change, use head to get first entry
  related_manu <- get_column(input_xmltop, "//related-manuscript-number-from", "related.manu") %>% head(n =1) #previous manu
  category <- get_column(input_xmltop, "//category", "category") %>% head(n =1) #research category
  title <- get_column(input_xmltop, "//title", "title") %>% head(n =1)
  manuscript_type <- get_column(input_xmltop, "//manuscript-type", "manuscript.type") %>% head(n =1) #type (review vs research, etc)
  is_resubmission <- get_column(input_xmltop, "//is-resubmission", "is.resubmission") %>% head(n =1) #resubmitted? almost always no?
  commissioned <- get_column(input_xmltop, "//commissioned", "commissioned") %>% head(n =1) 
  open_access <- get_column(input_xmltop, "//open-access", "open.access") %>% head(n =1)
  number_authors <- xmlToDataFrame(nodes = getNodeSet(input_xmltop, "//author-person-id")) %>% 
    n_distinct()#remove duplicated authors (b/c listed more than once if>2 versions)
  status <- get_column(input_xmltop, "//current-stage", "status")
  review_date <- get_column(input_xmltop, "//referee-started-date", "review.date") %>% head(n = 1) #date sent to first reviewer
  
  #join version data
  version_meta <- cbind(data.frame(manu_number, stringsAsFactors = FALSE), #add unqiue identifier
                        version, approved_date, submitted_date, decision_date, review_date,
                        decision, stringsAsFactors = FALSE) %>% 
    mutate(days.to.decision = as.duration(ymd_hms(submitted.date) %--% 
                                            ymd_hms(decision.date))/ddays(1)) #calculate days to make decision
           

  #join transfer data
  transfers <- get_column(input_xmltop, "//transfers/transfer", "transfer.journal")#scrapes all nodes in person and returns as df
  if(dim(transfers)[[2]] == 4){#check for number of columns: 4 = transfer data is present
    names(transfers) <- c('transfer.journal', 'transfer.date', 'transfer.msno', 'transfer.type')}else{
      names(transfers) <- "transfer.journal"} 
  
  transfer_data <- cbind(data.frame(manu_number), transfers) #add unique identifier to transfer data

  #dataframe of manuscript meta data
  manu_meta <- cbind(manu_number, related_manu, is_resubmission, category, title, commissioned,
                     manuscript_type, number_authors, doi, ready_for_production_date, 
                     published_online_date, journal_title, open_access, status, review_date)
  
  #full join of manuscript, version, transfer and people data
  manu_data <- list(version_meta, manu_meta, transfer_data, people) %>% #list all dfs
    reduce(full_join, by = "manuscript.number") %>% #join by manuscript identifier
    rename("editor.id" = "person.id", "number.authors" = "number_authors") %>% 
    mutate(reviewed = if_else(is.na(review_date), "no", "yes")) #determine if manuscript was reviewed or not
  
  #print(paste("completed", manu_number[[1]])) #status indicator for troubleshooting help
  
  return(manu_data)
}
