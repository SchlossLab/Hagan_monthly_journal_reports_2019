library(XML) #for extracting data from XML files
library(tidyverse)
library(lubridate) #for dealing with dates

#parse XML doc & find the root node before running further parsing functions
get_top <- function(input_XML){
  xmldoc1 <- xmlParse(input_XML) #parse
  xmltop <- xmlRoot(xmldoc1) #find rootnode - enables pulling node text
  return(xmltop)
}  

#generate a named column from scraped xml data of a single type, use tryCatch to avoid errors for blank XML nodes & return NA values instead
get_column <- function(input_xml, node, newname){ #input of source file, node relative path, and name for column
  column <- tryCatch(
    setNames(xmlToDataFrame(nodes = getNodeSet(input_xml, node)), newname), #uses input to retrieve text from all nodes with the relative path & generates a df
    error = function(e) {setNames(as.data.frame("NA"), newname)} #if nothing present, return NA value in a dataframe
  )
}

#function to fill empty nodes with "NA" to maintain dataframes
fill_if_null <- function(input){
  when(
    is.null(input) ~ paste("NA"),
    !is.null(input) ~ paste(input))
}

#need to assign a version to referee data/outcomes so that I can track them by revision - use dates where if review return falls between submitted & decision date then its assigned that version
assign_version <- function(x, version_meta) {
  
  f_to_date <- function(x){
    ymd_hms(as.character(x)) #convert to characters and read as dates
  }
  
  x <- f_to_date(x) #ensure that all inputs are converted to characters and read as dates
  
  #each line compares date x to the manuscripts associated submitted & decision dates, whichever row it fits in determines the assigned version
  case_when( 
    x >= f_to_date(version_meta[1,"submitted.date"]) & x <= f_to_date(version_meta[1,"decision.date"]) ~ as.character(0), #Intital version
    x >= f_to_date(version_meta[2,"submitted.date"]) & x <= f_to_date(version_meta[2,"decision.date"]) ~ as.character(1),
    x >= f_to_date(version_meta[3,"submitted.date"]) & x <= f_to_date(version_meta[3,"decision.date"]) ~ as.character(2),
    x >= f_to_date(version_meta[4,"submitted.date"]) & x <= f_to_date(version_meta[4,"decision.date"]) ~ as.character(3),  
    x >= f_to_date(version_meta[5,"submitted.date"]) & x <= f_to_date(version_meta[5,"decision.date"]) ~ as.character(4), #Highest version level is 4
    x <= f_to_date(version_meta[5,"submitted.date"]) ~ as.character(NA) #>5 versions is assigned a value of NA.
  )
}

#get the people involved in the manuscript, assign their roles, identify by manuscript number
parse_xml <- function(input_xmltop){
  
  #Manuscript identifiers and person data, variable names have _ & column names .
  manu_number <- get_column(input_xmltop, "//manuscript-number", "manuscript.number") %>% head(n=1)#use manuscript number as unique identifier
  
  editor_id <- get_column(input_xmltop, "//editor-person-id", "person.id") #all editors assigned
  sen_editor_id <- get_column(input_xmltop, "//senior-editor-person-id", "person.id") #senior editor assigned

  #person identifiers and demographic data
  persons <- get_column(input_xmltop, "//person", "person")#scrapes all nodes in person and returns as df
  names(persons) <- c("address", "area.of.interest", "first.name", "gender", "gender.count", "gender.probability", "institution", "last.name", "middle.name", "person.id", "title") 
  
  person_data <- cbind(data.frame(manu_number), persons) %>% select(manuscript.number, first.name, last.name, person.id) #_id, first_name, middle_name) #convert into useable dataframe & add manuscript number identifier
  
  #pool person data & demographics by first assigning each individual a "role"
  role <- c("editor", "senior.editor")

  editor <- cbind(data.frame(role[1]), person.id = editor_id) %>% rename(role = role.1.) #combine editor data
  senior_editor <- cbind(data.frame(role[2]), person.id = sen_editor_id) %>% rename(role = role.2.) #combine senior editor data

  #final dataframe of people, their roles, and demographics
  people <- list(editor, senior_editor) %>% #compile variables into list
    reduce(full_join, by = c("person.id", "role")) %>% #merge author, editor, senior editor & reviewer ids & roles
    left_join(person_data, by = "person.id") %>% #merge ids and roles with the identifying data using person.id
    dplyr::distinct() %>% #filter duplicate entries (an issue if multiple versions)
    filter(role != "NA", person.id != "NA") #filter any persons who don't have an assigned role in the manuscript (unused reviewers) or any NA person ids (e.g., if no reviewers used)
  
  #parse doi and date ready for production
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
  
  #join version data
  version_meta <- cbind(data.frame(manu_number, stringsAsFactors = FALSE), #use manuscript number as unqiue identifier
                        version, approved_date, submitted_date, decision_date, decision, stringsAsFactors = FALSE) %>% 
    mutate(days.to.decision = as.duration(ymd_hms(submitted.date) %--% ymd_hms(decision.date))/ddays(1))#calculate days to make decision for each version
  
  #reviwer specific data
  reviewer_id <- get_column(input_xmltop, "//referee-person-id", "person.id") #all listed reviewers
  reviewer_start <- get_column(input_xmltop, "//referee-started-date", "review.start") #review start date
  reviewer_return <- get_column(input_xmltop, "//referee-received-date", "review.return") #review completed date

    #join referee data
  review_outcome <- cbind(data.frame(manu_number, stringsAsFactors = FALSE), #manu number as common identifier
                          reviewer_id, reviewer_start, reviewer_return, stringsAsFactors = FALSE) %>% 
    mutate(days.to.review = as.duration(ymd_hms(review.start) %--% ymd_hms(review.return))/ddays(1), #calc how long review took
           version.reviewed = assign_version(review.return, version_meta)) #associate review decision with correct version
  
  #dataframe of manuscript meta data
  manu_meta <- cbind(manu_number, related_manu, is_resubmission, category, title, commissioned,
                     manuscript_type, number_authors, doi, ready_for_production_date, published_online_date,
                     journal_title, open_access)
  
  #full join of manuscript meta data & decisions
  manu_data <- list(version_meta, manu_meta, review_outcome, people) %>% #list all dfs
    reduce(full_join, by = "manuscript.number") %>% #join by manuscript identifier
    rename("reviewer.id" = "person.id.x", "editor.id" = "person.id.y")
  
  print(paste("completed", manu_number[[1]])) #status indicator for troubleshooting help
  
  return(manu_data)
}
