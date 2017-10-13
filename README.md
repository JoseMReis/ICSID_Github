# ICSID Disputes: Pulling General Case and Procedural Transparency Data
José M. Reis
(13/10/2017)
```

## Pulling the data

In this session we will generate a dataset composed by all concluded disputes (which have reached the arbitration stage) as well as their "procedural transparency variables". The extraction strategy goes as follows: we start by extracting all (again, non-settled nor discontinued) concluded cases as our baseline dataset;next, we exploit the "database" "published materials" parameter for getting case tables by published doc (by_doc tables); then we will use the case id to loop around the case specific pages and extract case specific data; finally, we wrap things up by generating procedural transparency variables and coding them based on matches between by_doc and baseline case ids.

First, we start by loading the relevant packages. Because most of ICSID's dispute database is dynamic and rendered in Java Script we will need to create a remote driver, and so we load the "RSelenium" package. "Rvest" will be used to parse and extract information from the page's html code. "RCurl" will be used to manage the HTTP requests. We will mostly resort to ` RCurl::getURL()` for that. Tidyverse packages, namely "dplyr", "stringr"", and "lubridate" will mostly be used for shapping up the extracted content.

```{r}

require(rvest)
require(RSelenium)
require(RCurl)
require(tidyverse)
require(stringr)
require(lubridate)


```


## Pulling the baseline and by_doc case tables

### start

```{r}
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

```

We start by setting up our selenium remote drive. We will run an image in a docker container using "Docker toolbox". Open "Docker"'s quick-start terminal, pull and run an image by running the following code```$docker pull selenium/standalone-firefox:2.53.0``` and ```$docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0 ```. Now we can set and start our remote driver.

```{r}
### Setting up the remote driver
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",
                      port = 4445)

### start it
remDr$open()

```

### Pulling the baseline case table 

First, we navigate to ICSID's dispute database page.

```{r}
### navigate to the ICSID case repository
basePage <- "https://icsid.worldbank.org/en/Pages/cases/AdvancedSearch.aspx"
remDr$navigate(basePage)
print(remDr$getCurrentUrl()) # just to check
```

To extract the baseline table we will have to tweak several case detail parameters.

1. We are only interested in obtaining data on concluded cases; so we navigate to the drop-down-menu of that parameter and click on it.

```{r}
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
```

2. Next, we turn to the page size and select all by sending the keys "All" to the relevant webelement.

```{r}
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
```

3. Finally, to avoid settled or discontinued cases we select several outcome parameters.

```{r}
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
```

Finally, we extract the case table, close the server and export it as .RData

```{r}
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

```


### Pulling the by_doc case tables

Now we will create a function that goes to ICSID's dispute database, sets the baseline parameters for loading (i) all (ii) concluded cases, and then, based on the input of published documents, sets the publication document parameter through a css selector and extracts the a case table, composed by cases where those documents were published, and turns it into a data frame.

```{r}
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
```

Now, we just have to call the function using as main input the procedural documents we are interested in.

```{r}
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
```

## Pulling general case data

We start by loading the base_cases.RData data frame

```{r}
load("data/base_cases.RData")
```

### Brief description of the variables

* Disp_subj: subject of the dispute as defined by the ICSID

* Econ_sector: economic sector as defined by the ICSID

* InstrumentOfDisp: instruments invoked for the dispute

* Applicable rules: applicable arbitration rules

* dateOfComplain: date of registration of complaint (date registered)

* dateOfConclusion: date of Award

### Extraction strategy

Using the case id's from base_cases, we can add them to the static url to arrive at the case specific case details page. There, and for all cases, the above mentioend variables will be under the similar nodes. We can then use css selectors to extract the information.

We start by setting the static url.

```{r}
staticURL <- "https://icsid.worldbank.org/en/Pages/cases/casedetail.aspx?CaseNo="
```

Next, we define our relevant css selectors.

```{r}
css_dispSubj <- ".proceedingcaselist1 :nth-child(1) .ng-binding"
css_EconSector <- ".proceedingcaselist1 :nth-child(2) .rightcol"
css_Instrument <- ".proceedingcaselist1 :nth-child(3) .rightcol"
css_AdoptRule <- ".proceedingcaselist1 :nth-child(4) .rightcol"
css_ComplainDate <- "#sectionA :nth-child(3) .rightcol.ng-binding"
css_outcomeDate <- "#sectionA .ng-scope li :nth-child(5) .ng-binding"
```

Finaly, we start our remote driver and test the css selectors, by extracting some data from randomly assign cases.

```{r}
# dispute subject
(sampled <- sample(nrow(base_cases), 1))
(page <- paste0(staticURL, base_cases[sampled,1]))
remDr$navigate(page)
Sys.sleep(2)
page_html <- remDr$getPageSource()[[1]]
page_html%>%
  read_html()%>%
  html_node(css_dispSubj)%>% 
  html_text()%>%
  str_trim()

# Econ sector
(sampled <- sample(nrow(base_cases), 1))
(page <- paste0(staticURL, base_cases[sampled,1]))
remDr$navigate(page)
Sys.sleep(2)
page_html <- remDr$getPageSource()[[1]]
page_html%>%
  read_html()%>%
  html_node(css_EconSector)%>% 
  html_text()%>%
  str_trim()

# instrument invoked 
(sampled <- sample(nrow(base_cases), 1))
(page <- paste0(staticURL, base_cases[sampled,1]))
remDr$navigate(page)
Sys.sleep(2)
page_html <- remDr$getPageSource()[[1]]
page_html%>%
  read_html()%>%
  html_node(css_Instrument)%>% 
  html_text()%>%
  str_trim()

# adopted rules
(sampled <- sample(nrow(base_cases), 1))
(page <- paste0(staticURL, base_cases[sampled,1]))
remDr$navigate(page)
Sys.sleep(2)
page_html <- remDr$getPageSource()[[1]]
page_html%>%
  read_html()%>%
  html_node(css_AdoptRule)%>% 
  html_text()%>%
  str_trim()

# complain date
(sampled <- sample(nrow(base_cases), 1))
(page <- paste0(staticURL, base_cases[sampled,1]))
remDr$navigate(page)
Sys.sleep(2)
page_html <- remDr$getPageSource()[[1]]
page_html%>%
  read_html()%>%
  html_node(css_ComplainDate)%>% 
  html_text()%>%
  str_trim()

# outcome date
(sampled <- sample(nrow(base_cases), 1))
(page <- paste0(staticURL, base_cases[sampled,1]))
remDr$navigate(page)
Sys.sleep(2)
page_html <- remDr$getPageSource()[[1]]
page_html%>%
  read_html()%>%
  html_node(css_outcomeDate)%>% 
  html_text()%>%
  str_trim()
```

### The loop

```{r}
### Add the variables to the dataset
base_cases$complainDate <- rep(NA,nrow(base_cases))
base_cases$conclusionDate<- rep(NA,nrow(base_cases))
base_cases$subjOfDisp <- rep(NA,nrow(base_cases))
base_cases$adoptedProcRules <- rep(NA,nrow(base_cases))
base_cases$invInstrument <- rep(NA,nrow(base_cases))
base_cases$econSector<- rep(NA,nrow(base_cases))

#names
names(base_cases)
# [1] "id"               "Complain"         "Resp"            
# [4] "status"           "complainDate"     "conclusionDate"  
# [7] "subjOfDisp"       "adoptedProcRules" "invInstrument"   
# [10] "econSector"  

### start the loop

for (i in seq_len(nrow(base_cases))) {
  
  page <- paste0(staticURL, base_cases[i,1])
  
  remDr$navigate(page)# navigate to the case specific page
  Sys.sleep(2)# let the java scripts load
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
  
  Sys.sleep(runif(3,2,4))
  
}
```

### Some cleaning

Mostly turning written dates into YYYY-MM-DD and date classs, getting rid of some ugly encoding from the invoked instruments variable, adding a "dateOfAccess" variable to record the extraction date.

```{r}

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
base_cases$dateOfAccess <- rep(Sys.Date(), nrow(base_cases))


```

Export the data as .RData and .csv.

## Generating the "procedural transparency variables"

### Load the base_cases and by_doc datasets

```{r}
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
```

We start by coding the existent data based on the by_doc case tables. award = byaward; Decision = by_decisions; proceduralOrder = by_procOrder...

Create empty variables for each relevant variable.

```{r}
ICSID_procTranspv1 <- as.tibble(base_cases)%>%
  mutate(award = rep(NA, n=nrow(base_cases)),
         decision = rep(NA, n=nrow(base_cases)),
         proceduralOrder = rep(NA, n=nrow(base_cases)),
         writtenSubmissions=rep(NA, n=nrow(base_cases)),
         transcript=rep(NA, n=nrow(base_cases)))
```

Now we just have to code the Variables. In the main data frame under the relevant variable, a observation  will get a score of 1 conditional on the case id being found in the variable relevant by_doc caseTable (e.g. in 1 for award if base_cases id can be found in the caseTable_byAward...), and 0 otherwise. We can use ``` ifelse() ``` for the conditional assignment.

```{r}
ICSID_procTranspv1 <- ICSID_procTranspv1%>%
  mutate(award = ifelse(id %in% caseTable_byAward$id, 1,0),
         decision = ifelse(id %in% caseTable_byDecision$id,1,0),
         proceduralOrder = ifelse(id %in% caseTable_byProcOrder$id,1,0),
         writtenSubmissions = ifelse(id %in% caseTable_bySummission$id,1,0),
         transcript = ifelse(id %in% caseTable_byTranscript$id,1,0))

## Add dateOfAccess
ICSID_procTranspv1 <- ICSID_procTranspv1%>%
  mutate(dateOfAccess2 = rep(Sys.Date(), nrow(ICSID_procTranspv1)))

## export it
save(ICSID_procTranspv1,
     file="data/ICSID_procTranspv1.RData")

write.csv(ICSID_procTranspv1,
          file="data/ICSID_procTranspv1.csv")

################################## end #########################################################################
```
