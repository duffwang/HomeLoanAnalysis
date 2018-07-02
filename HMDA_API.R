library(data.table) #For large datasets, data.table is significantly faster than plyr
library(Hmisc) #Used for cut2
library(reshape2) #Used for melt and dcast functions 
library(ggplot2)
library(shiny)
library(jsonlite)
library(stringdist)
library(knitr)

source('HMDA_Util.R')

#This file contains two API functions, hmda_init and hdma_to_json

#' #This function reads the data files and returns an expanded HDMA data table
#'
#' @param chr.file.loans loan filename, assumes csv file extension
#' @param chr.file.institutions institutions filename, assumes csv file extension 
#' 
#' @return data.table with the loan data, merged with the institutions information
#'
#' @author Duff Wang
#'
#This function reads the data files and returns an expanded HDMA data table
#It uses a caching system to save the data tables to compressed RDS format, so it can be loaded more quickly next time.
hmda_init <- function(chr.file.loans = "2012_to_2014_loans_data", chr.file.institutions = "2012_to_2014_institutions_data") {
  
  #Since these are large csvs, we'd like to cache the results so we don't need to parse them in every time
  #This function reads a file (chr.filename).csv and returns a data table, caching results for speed
  read.csv.memoized <- function(chr.filename) {
    chr.filename.cached <- paste0(chr.filename, "_cached.rds") 
    if (file.exists( chr.filename.cached )) {
      return(readRDS(chr.filename.cached))
    } else {
      if (file.exists(paste0(chr.filename, ".csv"))) {
        dt.data <- data.table( read.csv( paste0(chr.filename, ".csv"), stringsAsFactors = FALSE ) )
        #@todo: we could optimize memory usage and speed up our code by using factors instead of strings
        saveRDS(dt.data, chr.filename.cached)
        return(dt.data)
      } else {
        stop("Couldnt find the csv data files. Please add them to the directory")
      }
    }
  }
  
  #Read input data
  dt.loans <- read.csv.memoized(chr.file.loans)
  dt.institutions <- read.csv.memoized(chr.file.institutions)
  
  #Next, we clean up the data.
  
  #Standardize the column names, and ensure numeric data types are correct
  #For speed reasons, we won't convert to mixed case on the character vectors until needed, since that is an expensive operation
  dt.loans <- dt.loans[,list(year = as.numeric.na(As_of_Year, bool.integer = TRUE),
                             code.agency = as.numeric.na(Agency_Code, bool.integer = TRUE),
                             code.agency.desc = Agency_Code_Description,
                             id.respondent = Respondent_ID, #this column is truly alphanumeric, leave as character
                             sequence.number = as.numeric.na(Sequence_Number, bool.integer = TRUE),
                             loan.usd = as.numeric.na(Loan_Amount_000, bool.integer = TRUE) * 1000,
                             applicant.income.usd = as.numeric.na(Applicant_Income_000, bool.integer = TRUE) * 1000,
                             loan.purpose.desc = Loan_Purpose_Description,
                             loan.type.desc = Loan_Type_Description,
                             lien.status.desc = Lien_Status_Description,
                             code.status = as.numeric.na(State_Code, bool.integer = TRUE),
                             state.short = State,
                             code.county = as.numeric.na(County_Code, bool.integer = TRUE),
                             MSA.MD = as.numeric.na(MSA_MD, bool.integer = TRUE),
                             MSA.MD.desc = MSA_MD_Description,
                             tract.number = as.numeric.na(Census_Tract_Number),
                             MSA.MD.median.income = as.numeric.na(FFIEC_Median_Family_Income, bool.integer = TRUE),
                             tract.to.MSA.MD.median.income.pct = as.numeric.na(Tract_to_MSA_MD_Income_Pct), #Treat blanks as NA
                             tract.units = as.numeric.na(Number_of_Owner_Occupied_Units, bool.integer = TRUE),
                             county.name = County_Name,
                             conforming.limit = as.numeric.na(Conforming_Limit_000, bool.integer = TRUE) * 1000,
                             status.conventional = Conventional_Status,
                             status.conforming = Conforming_Status,
                             status.conventional.conforming = Conventional_Conforming_Flag
  )]
  
  
  dt.institutions <- dt.institutions[,list(year = as.numeric.na(As_of_Year, bool.integer = TRUE),
                                           code.agency = as.numeric.na(Agency_Code, bool.integer = TRUE),
                                           id.respondent = Respondent_ID,
                                           respondent.name = HmdaMixedCase(Respondent_Name_TS),
                                           respondent.city = HmdaMixedCase(Respondent_City_TS),
                                           respondent.state = Respondent_State_TS,
                                           respondent.zip = Respondent_ZIP_Code,
                                           parent.name = HmdaMixedCase(Parent_Name_TS),
                                           parent.city = HmdaMixedCase(Parent_City_TS),
                                           parent.state = Parent_State_TS,
                                           parent.zip = Parent_ZIP_Code,
                                           assets.panel = as.numeric.na(Assets_000_Panel) * 1000 #using numeric because values exceeds int max value (2e9)
  )]
  
  setkey(dt.institutions, year, id.respondent, code.agency)
  setkey(dt.loans, year, id.respondent, code.agency, sequence.number)
  
  #Check data for primary key issues
  if (nrow(dt.loans) != 
      nrow(unique(dt.loans[,list(year, id.respondent, code.agency, sequence.number)]))) {
    warning("Duplicate year, respondent id, agency code, sequence number  entries in institutions data file.")
  }
  if (nrow(dt.institutions) != 
      nrow(unique(dt.institutions[,list(year, id.respondent, code.agency)]))) {
    warning("Duplicate year, respondent id, agency code entries in institutions data file.")
  }
  
  #We'll merge the two data tables
  #I will keep only respondent name from the institutions table as that is my interpretation of the instructions, and otherwise
  #the dataset will be too wide for easy use for the user, containing mostly useless info about the respondents.
  #We'll also create a new attribute that buckets loan amount. I'll bucket it into 10 groups, each with roughly equal number.
  dt.loans <- dt.institutions[,list(year, code.agency, id.respondent, respondent.name)][dt.loans]
  dt.loans <- dt.loans[, loan.usd.bucket := cut2(loan.usd, g = 10)]
  
  setkey(dt.loans, year, id.respondent, code.agency, sequence.number)

  return(dt.loans)
}

#' This function takes the data table returned by hdma_init and exports it as a JSON file
#'
#' @param data data.table containing loan and institution information
#' @param states (optional) character vector of 2 letter state abbreviations to filter by
#' @param conventional_conforming (optional) logical TRUE will filter loans to conventional, conforming loans. FALSE returns all other loans. Leave as NA to include all loans.
#' @param chr.output.file Filename of the exported JSON file, written with ".json" file extension. Default is "hdma"
#' 
#' @return \code{TRUE}
#'
#' @author Duff Wang
#'
hmda_to_json <- function(data, states = NA, conventional_conforming = NA, chr.output.file = "hdma") {
  #Input checks
  if (!any(grepl("data.table", class(data)))) { stop("Expecting a data.table. Check your input data") }
  dt.input <- data 
  
  #Ensures states is in right format if provided
  if (length(states) == 1 && !is.na(states)) {
    if (class(states) != "character") { stop("states must be a character vector") }
    if (any(nchar(states) != 2)) { stop("States must contain two character abbreviations") }
  }
  
  #Ensures conventional_conforming is in right format if provided
  if (length(conventional_conforming) > 1) { stop("conventional_conforming must be a single value") }
  if (!is.na(conventional_conforming) && class(conventional_conforming) != "logical") {
     stop("conventional_conforming must be a logical vector") 
  }

  #Subset the data accordingly
  if (length(states) != 1 || !is.na(states)) {
    dt.input <- dt.input[state.short %chin% states]  #%chin% is much faster than %in%
  }
  if (!is.na(conventional_conforming)) {
    dt.input <- dt.input[(status.conventional.conforming == 'Y') == conventional_conforming] 
  }
  
  #Export JSON object
  json.output <- toJSON(dt.input)
  write(json.output, paste0(chr.output.file, ".json"))
  
  return(TRUE)
}