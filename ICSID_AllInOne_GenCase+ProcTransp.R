###########################################################################################################
#

# file: "ICSID_AllInOne_GenCase+ProcTransp.R"

# Author: J.M.Reis

# Purpose: Pull general case data as well as procedural transparency variables from the ICSID dispute repository

# date: 
(dateOfAccess <- Sys.Date())
#13/10/2017

#
##########################################################################################################


#########################################################################################

                        ## Pulling case data by published doc. ##

#########################################################################################


#### Load the relevant packages----------------------------------------------------------------------------

require(rvest)
require(RSelenium)
require(RCurl)
require(tidyverse)
require(stringr)
require(lubridate)


#### Extraction strategy--------------------------------------------------------------------------------

#### The target page will be the ICSID repo @https://icsid.worldbank.org/en/Pages/cases/AdvancedSearch.aspx. Because I am only interested in (i) concluded cases, which (ii) reached the arbitration stage (no pre-trial) and which were decided there, the following parameters will be fixed in this way in all extractions: (1) Case Status: Concluded, (2) Outcome: (2.1) Award, (2.2) Decision on Correction of the Award (AF), (2.3) Decision on Interpretation of the Award, (2.4) Decision on Rectification of the Award,(2.5) Decision on Request for Supplementary Decision, (2.6) Decision on Revision of the Award, (2.7) Decision on annulment of the Award; and (3) with views per page set at: All.


#### Setting the remote driver----------------------------------------------------------------------------------

### Pulling an image, and getting a port and container at the docker ps

## Open the docker and type the following commands

# "     $docker pull selenium/standalone-firefox:2.53.0     "
# "  $ docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0   "


### Setting up the remote driver
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",
                      port = 4445)

### start it
remDr$open()

###Navigate to the basePage
basePage <- "https://icsid.worldbank.org/en/Pages/cases/AdvancedSearch.aspx"
remDr$navigate(basePage)
remDr$getCurrentUrl() # just to check
remDr$screenshot(file= "screenS_base_noPar.JPEG")# idem


#### extract all concluded cases (baseline dataset)---------------------------------------------------------------------------------

##Navigate to the basePage
### navigate to the ICSID case repository
basePage <- "https://icsid.worldbank.org/en/Pages/cases/AdvancedSearch.aspx"
remDr$navigate(basePage)
print(remDr$getCurrentUrl()) # just to check

###Set the baseline query parameters for the table
## case status: "Concluded"
#Case_status button
button_CaseStatus <- remDr$findElement(using = "css selector",
                                       value = "#section2 .btnChkClass")
# open drop down menu
button_CaseStatus$clickElement()
# select the button for option concluded
option_concluded <- remDr$findElement(using = "css selector",
                                      value ="#section2 :nth-child(6) .ng-binding")
# click it
option_concluded$clickElement()
# confirm 
remDr$screenshot(file= "screenS_base_par1.JPEG", useViewer = TRUE)# it worked
# close ddm
button_CaseStatus$clickElement()
## pagesize: "All"
#page size button
button_pageSize <- remDr$findElement(using="css selector",
                                     value=".CVpagecount")
# send the relevant key
button_pageSize$sendKeysToElement(list("All"))
# click on it
button_pageSize$clickElement()
# confirm 
remDr$screenshot(file= "screenS_base_par2.JPEG", useViewer = TRUE)

### Getting the outcome parameters
# (2.1) Award, (2.2) Decision on Correction of the Award (AF), (2.3) Decision on Interpretation of the Award, (2.4) Decision on Rectification of the Award,(2.5) Decision on Request for Supplementary Decision, (2.6) Decision on Revision of the Award, (2.7) Decision on annulment of the Award

### Button
button_outcome <- remDr$findElement(using = "css selector",
                                    value = "#Div14 .btnChkClass")
##open drop down menu
button_outcome$clickElement()

### Award
option_award <- remDr$findElement(using="css selector",
                                  value = "#Div14 :nth-child(6) .ng-binding")
option_award$clickElement()
## confirm 
remDr$screenshot(file= "screenS_base_par3.JPEG")# it worked
# close it
button_outcome$clickElement()

###Decision on Correction of the Award
##open drop down menu
button_outcome$clickElement()

option_dec1 <- remDr$findElement(using="css selector",
                                 value = "#Div14 :nth-child(7) .ng-binding")
option_dec1$clickElement()
## confirm 
remDr$screenshot(file= "screenS_base_par4.JPEG")# it worked
# close it
button_outcome$clickElement()

### Decision on Interpretation of the Award

#open drop down menu
button_outcome$clickElement()
option_dec2 <- remDr$findElement(using="css selector",
                                 value = "#Div14 :nth-child(8) .ng-binding")
option_dec2$clickElement()
## confirm 
remDr$screenshot(file= "screenS_base_par5.JPEG")# it worked
#close it
button_outcome$clickElement()

### Decision on Rectification of the Award
#open drop down menu
button_outcome$clickElement()
# click on the option
option_dec3 <- remDr$findElement(using="css selector",
                                 value = "#Div14 :nth-child(9) .ng-binding")
option_dec3$clickElement()

## confirm 
remDr$screenshot(file= "screenS_base_par6.JPEG")# it worked
#close it
button_outcome$clickElement()

### Decision on Rectification of the Award
#open it
button_outcome$clickElement()
# click option
option_dec4 <- remDr$findElement(using="css selector",
                                 value = "#Div14 :nth-child(10) .ng-binding")
option_dec4$clickElement()
## confirm 
remDr$screenshot(file= "screenS_base_par7.JPEG")# it worked
#close it
button_outcome$clickElement()

###  Decision on Revision of the Award
#open it
button_outcome$clickElement()
#click option
option_dec5 <- remDr$findElement(using="css selector",
                                 value = "#Div14 :nth-child(11) .ng-binding")
option_dec5$clickElement()
## confirm 
remDr$screenshot(file= "screenS_base_par8.JPEG")# it worked
#close it
button_outcome$clickElement()
###Decision on annulment of the Award  
#open it
button_outcome$clickElement()
#click options
option_dec6 <- remDr$findElement(using="css selector",
                                 value = "#Div14 :nth-child(12) .ng-binding")
option_dec6$clickElement()
## confirm 
remDr$screenshot(file= "screenS_base_par9.JPEG")# it worked
#close it
button_outcome$clickElement()

## extract the page source
page_html <- remDr$getPageSource()[[1]]

base_cases<- page_html%>% 
  read_html()%>%
  html_nodes("table.casedetbl")%>%
  html_table()%>%
  as.data.frame()

## Close the server
remDr$close()

names(base_cases) <- c("id", "Complain", "Resp", "status")

### Export it
save(base_cases,
     file="data/base_cases.RData")


#### Extraction of case tables conditional on the published materials--------------------------

### Now we will create a function that goes to ICSID's dispute databset, sets the baseline parameters for loading (i) all (ii) concluded cases, and then, based on the input of published documents, sets the publication document parameter through a css selector and extracts the case table of cases where said documents where publish as a data frame

Extract.table_byDoc <- function(css_selector, screenS_name) {
  ### check if a remDr exists, if not start it. !!! Do not forget to pull a image container at the docker ps!!!
  if(exists("remDr")==FALSE) {
    # Setting up the remote driver
    remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",
                          port = 4445)
    # start it
    remDr$open()
    
  }else {
    # just start it
    remDr$open()
  }
  ### navigate to the ICSID case repository
  basePage <- "https://icsid.worldbank.org/en/Pages/cases/AdvancedSearch.aspx"
  remDr$navigate(basePage)
  print(remDr$getCurrentUrl()) # just to check
  
  ###Set the baseline query parameters for the table
  ## case status: "Concluded"
  #Case_status button
  button_CaseStatus <- remDr$findElement(using = "css selector",
                                         value = "#section2 .btnChkClass")
  # open drop down menu
  button_CaseStatus$clickElement()
  # select the button for option concluded
  option_concluded <- remDr$findElement(using = "css selector",
                                        value ="#section2 :nth-child(6) .ng-binding")
  # click it
  option_concluded$clickElement()
  # confirm 
  remDr$screenshot(file= "screenS_base_par1.JPEG", useViewer = TRUE)# it worked
  # close ddm
  button_CaseStatus$clickElement()
  ## pagesize: "All"
  #page size button
  button_pageSize <- remDr$findElement(using="css selector",
                                       value=".CVpagecount")
  # send the relevant key
  button_pageSize$sendKeysToElement(list("All"))
  # click on it
  button_pageSize$clickElement()
  # confirm 
  remDr$screenshot(file= "screenS_base_par2.JPEG", useViewer = TRUE)
  
  ## Getting the outcome parameters: (2.1) Award, (2.2) Decision on Correction of the Award (AF), (2.3) Decision on Interpretation of the Award, (2.4) Decision on Rectification of the Award,(2.5) Decision on Request for Supplementary Decision, (2.6) Decision on Revision of the Award, (2.7) Decision on annulment of the Award
  # Button outcome
  button_outcome <- remDr$findElement(using = "css selector",
                                      value = "#Div14 .btnChkClass")
  #open drop down menu
  button_outcome$clickElement()
  # Award
  option_award <- remDr$findElement(using="css selector",
                                    value = "#Div14 :nth-child(6) .ng-binding")
  option_award$clickElement()
  ##confirm 
  remDr$screenshot(file= "screenS_base_par3.JPEG")# it worked
  # close it
  button_outcome$clickElement()
  ##Decision on Correction of the Award
  #open drop down menu
  button_outcome$clickElement()
  option_dec1 <- remDr$findElement(using="css selector",
                                   value = "#Div14 :nth-child(7) .ng-binding")
  option_dec1$clickElement()
  # confirm 
  remDr$screenshot(file= "screenS_base_par4.JPEG")# it worked
  # close it
  button_outcome$clickElement()
  ##Decision on Interpretation of the Award
  #open drop down menu
  button_outcome$clickElement()
  option_dec2 <- remDr$findElement(using="css selector",
                                   value = "#Div14 :nth-child(8) .ng-binding")
  option_dec2$clickElement()
  # confirm 
  remDr$screenshot(file= "screenS_base_par5.JPEG")# it worked
  #close it
  button_outcome$clickElement()
  ## Decision on Rectification of the Award
  #open drop down menu
  button_outcome$clickElement()
  # click on the option
  option_dec3 <- remDr$findElement(using="css selector",
                                   value = "#Div14 :nth-child(9) .ng-binding")
  option_dec3$clickElement()
  # confirm 
  remDr$screenshot(file= "screenS_base_par6.JPEG")# it worked
  #close it
  button_outcome$clickElement()
  ## Decision on Rectification of the Award
  #open it
  button_outcome$clickElement()
  # click option
  option_dec4 <- remDr$findElement(using="css selector",
                                   value = "#Div14 :nth-child(10) .ng-binding")
  option_dec4$clickElement()
  # confirm 
  remDr$screenshot(file= "screenS_base_par7.JPEG")# it worked
  #close it
  button_outcome$clickElement()
  ##  Decision on Revision of the Award
  #open it
  button_outcome$clickElement()
  #click option
  option_dec5 <- remDr$findElement(using="css selector",
                                   value = "#Div14 :nth-child(11) .ng-binding")
  option_dec5$clickElement()
  ## confirm 
  remDr$screenshot(file= "screenS_base_par8.JPEG")# it worked
  #close it
  button_outcome$clickElement()
  ###Decision on annulment of the Award  
  #open it
  button_outcome$clickElement()
  #click options
  option_dec6 <- remDr$findElement(using="css selector",
                                   value = "#Div14 :nth-child(12) .ng-binding")
  option_dec6$clickElement()
  #confirm 
  remDr$screenshot(file= "screenS_base_par9.JPEG")# it worked
  #close it
  button_outcome$clickElement()
  
  ### Set the published document parameter
  #set the button for pub doc ddm
  button_pubDoc <- remDr$findElement(using="css selector",
                                     value="#Div15 .btnChkClass")
  #open ddm
  button_pubDoc$clickElement()
  # set the relevant doc
  option_doc <- remDr$findElement(using = "css selector",
                                  value=css_selector)
  # click it
  option_doc$clickElement()
  #confirm
  remDr$screenshot(file=screenS_name, useViewer = T)
  #close ddm
  button_pubDoc$clickElement()
  
  ### Extract the table
  page_html <- remDr$getPageSource()[[1]]# extract the html source code
  
  result <- page_html%>% 
    read_html()%>%
    html_nodes("table.casedetbl")%>%
    html_table()%>%
    as.data.frame()
  
  ## Close the server
  remDr$close()
  #change collumn names to the used standard
  names(result) <- c("id", "Complain", "Resp", "status")
  
  return(result)
}

#### Extracting case tables based on published documents-------------------------------------------

### cases where the award was published
table_byPubAward <- Extract.table_byDoc(css_selector = "#Div15 :nth-child(6) .ng-binding",
                                        screenS_name = "par.Award_check.JPEG")

caseTable_byAward <- table_byPubAward
## Export it
save(caseTable_byAward,
     file="data/caseTable_byAward.RData")

### cases where the "decision" was published
table_byPubDec <- Extract.table_byDoc(css_selector = "#Div15 :nth-child(7) .ng-binding",
                                      screenS_name = "par.Dec_check.JPEG")

caseTable_byDecision <- table_byPubDec
## export it
save(caseTable_byDecision,
     file="data/caseTable_byDecision.RData")

### cases where the "Procedural order" was published
table_byProcOrder <- Extract.table_byDoc(css_selector = "#Div15 :nth-child(8) .ng-binding",
                                         screenS_name = "par.ProcOrder_check.JPEG")

caseTable_byProcOrder <- table_byProcOrder
## export it
save(caseTable_byProcOrder,
     file="data/caseTable_byProcOrder.RData")

### cases with published "introductory notes"
table_byIntro <- Extract.table_byDoc(css_selector = "#Div15 :nth-child(9) .ng-binding",
                                     screenS_name = "par.into_check.JPEG")

caseTable_byIntro <- table_byIntro
## export it
save(caseTable_byIntro,
     file="data/caseTable_byIntro.RData")

### Cases where "others" were published
table_byOthers <- Extract.table_byDoc(css_selector = "#Div15 :nth-child(10) .ng-binding",
                                      screenS_name = "par.others_check.JPEG")
caseTable_byOthers <- table_byOthers
## export it
save(caseTable_byOthers,
     file="data/caseTable_byOthers.RData")

### cases where "Declaration" were published

table_byDecl <- Extract.table_byDoc(css_selector = "#Div15 :nth-child(11) .ng-binding",
                                    screenS_name = "par.Decl_check.JPEG")
caseTable_byDeclaration <- table_byDecl
##export it
save(caseTable_byDeclaration,
     file="data/caseTable_byDeclaration.RData")

### Cases where "Minutes" were published
table_byMin <- Extract.table_byDoc(css_selector = "#Div15 :nth-child(12) .ng-binding",
                                   screenS_name = "par.Min_check.JPEG")

caseTable_byMinute <- table_byMin
## export it
save(caseTable_byMinute,
     file="data/caseTable_byMinute.RData")

#### cases where "pleadings or submissions" where published
table_bySubm <- Extract.table_byDoc(css_selector = "#Div15 :nth-child(13) .ng-binding",
                                    screenS_name = "par.Subm_check.JPEG")
caseTable_bySummission <- table_bySubm
##export it
save(caseTable_bySummission,
     file="data/caseTable_bySummission.RData")

#### cases where "Transcripts" were published
table_byTransc <- Extract.table_byDoc(css_selector = "#Div15 :nth-child(14) .ng-binding",
                                      screenS_name = "par.Tansc_check.JPEG")

caseTable_byTranscript <- table_byTransc
## export it
save(caseTable_byTranscript,
     file="data/caseTable_byTranscript.RData")


#########################################################################################

                            ## Pulling general case data ##

#########################################################################################

#### Load the latest dataset---------------------------------------------------------------
load("data/base_cases.RData")

#### Brief description of the variables-----------------------------------------------------

## Disp_subj: subject of the dispute as defined by the ICSID

## Econ_sector: economic sector as defined by the ICSID

## InstrumentOfDisp: instruments invoked for the dispute

## Applicable rules: applicable arbitration rules

## dateOfComplain: date of registration of complaint (date registered)

## dateOfConclusion: date of Award

### Extraction---------------------------------------------------------------------------

## step 1: set the static URL
staticURL <- "https://icsid.worldbank.org/en/Pages/cases/casedetail.aspx?CaseNo="
## step2: set the css selectors for each variable
#disp_subj
css_dispSubj <- ".proceedingcaselist1 :nth-child(1) .ng-binding"
css_EconSector <- ".proceedingcaselist1 :nth-child(2) .rightcol"
css_Instrument <- ".proceedingcaselist1 :nth-child(3) .rightcol"
css_AdoptRule <- ".proceedingcaselist1 :nth-child(4) .rightcol"
css_ComplainDate <- "#sectionA :nth-child(3) .rightcol.ng-binding"
css_outcomeDate <- "#sectionA .ng-scope li :nth-child(5) .ng-binding"


## step 3: set some things for the loop

# set and start the remote driver
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",
                      port = 4445)

remDr$open()

### Add the variables to the dataset
base_cases$complainDate <- rep(NA,nrow(base_cases))
base_cases$conclusionDate<- rep(NA,nrow(base_cases))
base_cases$subjOfDisp <- rep(NA,nrow(base_cases))
base_cases$adoptedProcRules <- rep(NA,nrow(base_cases))
base_cases$invInstrument <- rep(NA,nrow(base_cases))
base_cases$econSector<- rep(NA,nrow(base_cases))

# var. names
names(base_cases)

### start the loop

for (i in seq_len(nrow(base_cases))) {
  
  page <- paste0(staticURL, base_cases[i,1]) #goes to case-specific details page (using the case id)
  
  remDr$navigate(page)# navigate to the case specific page
  Sys.sleep(2)# let the JS load...min 2 secs!
  page_html <- remDr$getPageSource()[[1]]# extract the html code
  
  ## get complainDate
  
  base_cases[i,5] <- page_html%>%
    read_html()%>%
    html_node(css_ComplainDate)%>% 
    html_text()%>%
    str_trim()
  
  print(paste0("Case id: ", base_cases[i,1], " gets: ", base_cases[i,5]))
  
  ## get outcomeDate
  base_cases[i,6] <- page_html%>%
    read_html()%>%
    html_node(css_outcomeDate)%>% 
    html_text()%>%
    str_trim()
  
  print(paste0("Case id: ", base_cases[i,1], " gets: ", base_cases[i,6]))
  
  ## subjOfDispute
  base_cases[i,7] <- page_html%>%
    read_html()%>%
    html_node(css_dispSubj)%>% 
    html_text()%>%
    str_trim()
  
  print(paste0("Case id: ", base_cases[i,1], " gets: ", base_cases[i,7]))
  
  ## Adopted proc rules
  base_cases[i,8] <- page_html%>%
    read_html()%>%
    html_node(css_AdoptRule)%>% 
    html_text()%>%
    str_trim()
  
  print(paste0("Case id: ", base_cases[i,1], " gets: ", base_cases[i,8]))
  
  ## invoked instrument
  base_cases[i,9] <- page_html%>%
    read_html()%>%
    html_node(css_Instrument)%>% 
    html_text()%>%
    str_trim()
  
  print(paste0("Case id: ", base_cases[i,1], " gets: ", base_cases[i,9]))
  
  ## econ sector
  base_cases[i,10] <- page_html%>%
    read_html()%>%
    html_node(css_EconSector)%>% 
    html_text()%>%
    str_trim()
  
  print(paste0("Case id: ", base_cases[i,1], " gets: ", base_cases[i,10]))
  
  Sys.sleep(runif(3,2,4))# for the server..
  
}

save(base_cases,
     file="base_cases.RData")

#### Some cleaning-----------------------------------------------------------------------

### Turning the date variables into class date(YYYY-MM-DD)
## complain date
# reverse the order to dmy
base_cases$complainDate <- sapply(base_cases$complainDate, function(x) {
  
  # split the substrings
  month <- str_extract(x, pattern="\\D{3,9}")
  day <- str_extract(x, pattern="\\d{1,2}")
  year <- str_extract(x, pattern = "\\d{4}")
  
  # put them together, and turn to date
  y <- paste(day, month, year, collapse = " ")
  
})
# apply dmy
base_cases$complainDate <- dmy(base_cases$complainDate)

## conclusionDate
# extract the date from the rest of the paragraph and turn it into dmy format

patDate <- "\\D{3,9}\\s\\d{1,2}\\,\\s\\d{4}"# reads as: "get me a non-digit with 3 to 9 char., followed by a space, then 1 to 2 digits, a comma, a space and finally 4 digits.

base_cases$conclusionDate <- sapply(base_cases$conclusionDate, function(x) {
  
  y <- str_extract(x,patDate)# extracts the date from the text
  # split the substrings
  month <- str_extract(y, pattern="\\D{3,9}")
  day <- str_extract(y, pattern="\\d{1,2}")
  year <- str_extract(y, pattern = "\\d{4}")
  
  # put them together, and turn to date
  y <- paste(day, month, year, collapse = " ")
  
})
# turn it to date
base_cases$conclusionDate <- dmy(base_cases$conclusionDate)

### remove ugly encoding from instrument invoked
base_cases$invInstrument <- sapply(base_cases$invInstrument, function(x) {
  uglyPat <- paste(c("\\\n", "\\-\ "), collapse = "|")
  
  y <- gsub(x, pattern=uglyPat, replacement ="")
})

### Add date of Access
base_cases$dateOfAccess <- rep(dateOfAccess, nrow(base_cases))

#### Export the data-----------------------------------------------------------------------

save(base_cases,
     file="data/base_cases.RData")

write.csv(base_cases,
          file="data/base_cases.csv")

#########################################################################################

              ## Generating the procedural transparency dummy variables ##

#########################################################################################

#### Load the datasets---------------------------------------------------------------------------

relevant.data <- c("data/base_cases.RData",
                   "data/caseTable_byAward.RData",
                   "data/caseTable_byDecision.RData",
                   "data/caseTable_byDeclaration.RData",
                   "data/caseTable_byIntro.RData",
                   "data/caseTable_byMinute.RData",
                   "data/caseTable_byOthers.RData",
                   "data/caseTable_byProcOrder.RData",
                   "data/caseTable_bySummission.RData",
                   "data/caseTable_byTranscript.RData")

lapply(relevant.data,
       load,
       .GlobalEnv)

#### ICSID_procTranspv1-----------------------------------------------------------------------

### We first start by coding the existent data based on the by_doc case tables. award = byaward; Decision will be coded as a ICSID specific variable for now; proceduralOrder = by_procOrder; writtenSubmissions = by_subm; transcript = by_transc...


### create a data frame with the base data + the above mentioned empty variables
ICSID_procTranspv1 <- as.tibble(base_cases)%>%
  mutate(award = rep(NA, n=nrow(base_cases)),
         decision = rep(NA, n=nrow(base_cases)),
         proceduralOrder = rep(NA, n=nrow(base_cases)),
         writtenSubmissions=rep(NA, n=nrow(base_cases)),
         transcript=rep(NA, n=nrow(base_cases)))

### code the Variables. In the main data frame under the relevant variable, a observation  will get a score of 1 conditional on the case id being found in the variable relevant by_doc caseTable (e.g. in 1 for award if case is in the caseTable_byAward...), and 0 otherwise. 

ICSID_procTranspv1 <- ICSID_procTranspv1%>%
  mutate(award = ifelse(id %in% caseTable_byAward$id, 1,0),
         decision = ifelse(id %in% caseTable_byDecision$id,1,0),
         proceduralOrder = ifelse(id %in% caseTable_byProcOrder$id,1,0),
         writtenSubmissions = ifelse(id %in% caseTable_bySummission$id,1,0),
         transcript = ifelse(id %in% caseTable_byTranscript$id,1,0))


### Add dateOfAccess
ICSID_procTranspv1 <- ICSID_procTranspv1%>%
  mutate(dateOfAccess2 = rep(Sys.Date(), nrow(ICSID_procTranspv1)))

## export it
save(ICSID_procTranspv1,
     file="data/ICSID_procTranspv1.RData")

write.csv(ICSID_procTranspv1,
          file="data/ICSID_procTranspv1.csv")