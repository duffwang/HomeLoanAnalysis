#This shiny app will take a demographic profile, and show what loans such an applicant
#historically received. It will also describe how commonly loans were given to this kind of applicant, and 
#describe how this applicant's expected loan profile changed from 2012 to 2014.

source('HMDA_API.R')

LoanSimulationShiny <- function(dt.loans) {
  
  #Load option information from the large dataset
  chr.loan.type <- unique(dt.loans$loan.purpose.desc)
  int.year = sort(unique(dt.loans$year))
  dt.metro.map <- data.table(value = unique(dt.loans$MSA.MD.desc))
  dt.metro.map <- dt.metro.map[value != ""]
  dt.metro.map <- dt.metro.map[,metro.clean := HmdaMixedCaseAddress(value)]
  setkey(dt.metro.map, metro.clean)
  
  #Sets it the the columns we will filter by, for fast subsetting
  setkey(dt.loans, MSA.MD.desc, loan.purpose.desc, applicant.income.usd, year, code.agency, id.respondent, sequence.number)
  global.run.counter <- 0L
 
  shinyApp(
    #ui section lays out the shiny tool format using the fluid page format
    ui = fluidPage(
      titlePanel("Home Loan Customer Profile Simulation"),
      hr(),
      fluidRow(
        column(4,
               h4("Run Settings"),
               actionButton("run.simulation", "Run Simulation"),
               checkboxInput('autorun', 'Auto-run when inputs are changed', 1),
               hr(),
               h4("My Applicant Profile"),
               uiOutput("year.select"),
               uiOutput("metro.select"),
               uiOutput("income.select"),
               uiOutput("loan.type.select"),
               hr(),
               h4("Output Settings"),
               uiOutput("bucket.select")
        ),
        column(8, 
               uiOutput("runSimulationInfo"),
               br(),
               tabsetPanel(
                 tabPanel('Summary'
                          ,uiOutput("msg.intro")
                          ,plotOutput("plotSummary"))
                 ,tabPanel('By Year'
                           ,plotOutput("plotYearly"))
               )
               ,uiOutput("msg.1"),
               br(),
               uiOutput("msg.2"),
               br(),
               uiOutput("msg.3")
      ))
    ),
    
    server = function(input, output, session) {

      #These are the reactive inputs
      output$metro.select <- renderUI({
        return( selectInput('metro', 'My Metropolitan Area', c("Any", dt.metro.map$metro.clean)))  
      })
      output$income.select <- renderUI({
        return( sliderInput("income", label = "My Annual Income:",
                            min = 0, max = 250000, value = 60000, step = 10000) )  
      })
      output$loan.type.select <- renderUI({
        return( radioButtons('loan.type', 'My Desired Loan Type', c("Either", chr.loan.type)))  
      })
      output$bucket.select <- renderUI({
        return(  sliderInput("bucket", label = "Number of bins:",
                             min = 2, max = 14, value = 8, step = 1) )
      })
      
      #This renders the actual simulation output
      output$runSimulationInfo <- renderText({
        
        output$msg.intro <- renderText({})
        #We want the graph to be generated if the run button has been clicked, or if the run
        #button has previously been clicked and autorun is on
        if (input$run.simulation > 0 & (input$autorun | input$run.simulation > global.run.counter)) {
          
          #Provides a progress bar so user knows the loan data is still being processed
          shiny:::withProgress({
              
              isolate(global.run.counter <<- input$run.simulation)
              shiny:::setProgress(value = .2)
              #Subset our data according to input parameters
              dt.loans.temp <- dt.loans[!is.na(applicant.income.usd)]
              if (input$metro != "Any")
                dt.loans.temp <- dt.loans.temp[MSA.MD.desc == dt.metro.map[input$metro]$value[[1]]]
              if (input$loan.type != "Either")
                dt.loans.temp <- dt.loans.temp[loan.purpose.desc == input$loan.type]
              
              shiny:::setProgress(value = .4)
              dt.loans.cut <- dt.loans.temp[applicant.income.usd > input$income - 5000 & applicant.income.usd < input$income + 5000]
              int.loans.cut.count <- nrow(dt.loans.cut)
              #Checks to make sure there is data to show
              if (int.loans.cut.count == 0) {
                output$plotSummary <- renderPlot({ })
                output$plotYearly <- renderPlot({ })
                output$msg.1 <- renderText({ return("") })
                output$msg.2 <- renderText({ return("") })
                output$msg.3 <- renderText({ return("") })
                return("Simulation could not complete because there were no historical loans with your demographic profile.")
              }
              shiny:::setProgress(value = .6)
              #To reduce the effects of outliers, make cut points up to 95th percentile. cut2's minmax will take care of outliers.
              int.cut.quantile <- quantile(dt.loans.cut$loan.usd, 0.95)
    
              int.cuts = seq(from = 0, to = int.cut.quantile, length.out = as.integer(input$bucket))
             
              dt.loans.cut <- dt.loans.cut[,loan.bucket := cut2(loan.usd/1000, cuts = round(int.cuts/1000))]     
              dt.loans.cut.hist <- dt.loans.cut[,list(count = .N), by = list(year, loan.bucket)]
              
              shiny:::setProgress(value = .8)
              #These functions control the plots that are displayed.
              output$plotSummary <- renderPlot({
                p <- ggplot(data=dt.loans.cut.hist[,list(count = sum(count)), by = loan.bucket], aes(x = loan.bucket, y = count)) +
                  geom_bar(stat='identity', position = 'dodge') + 
                  labs(x = "Loan Amount (thousands)",
                       y = "Count",
                       title = "Loan Profile")+ theme(text = element_text(size=14), axis.text.x = element_text(angle=45, hjust = 1, vjust = 1, size = 14))
                print(p)
              })
              
              output$plotYearly <- renderPlot({
                p <- ggplot(data=dt.loans.cut.hist, aes(x = loan.bucket, y = count, fill = as.character(year))) +
                  geom_bar(stat='identity', position = 'dodge') + 
                  labs(x = "Loan Amount (thousands)",
                       y = "Count",
                       title = "Loan Profile")+ theme(text = element_text(size=14), axis.text.x = element_text(angle=45,hjust = 1, vjust = 1, size = 14))+ 
                  scale_fill_discrete(name="Year")
                print(p)
              })
              
              #Now we generate some information to tell the user
              
              #Loan distribution summary numbers
              int.loan.quantile <- round(quantile(dt.loans.cut$loan.usd, c(0.25, 0.5, 0.75))/1000)
              chr.msg.1 <- paste0("Someone like you typically gets a average home loan around <b>$", int.loan.quantile[2], 
                                  "k</b>. Half of all applicants similar to you will borrow between <b>$", 
                                  int.loan.quantile[1], "k</b> and <b>$", int.loan.quantile[3], "k</b>.", collapse = '')
              output$msg.1 <- renderText({ chr.msg.1 })
              
              #Compare proportion of number of loans to other income brackets
              int.loans.count <- nrow(dt.loans.temp)
              num.loan.proportion <- int.loans.cut.count / int.loans.count
              int.median.income <- quantile(dt.loans.temp$applicant.income.usd, 0.5)
              if (input$income > int.median.income) {
                chr.msg.2 <- "Your median income is <b>higher than average</b> for this type of loan in this area."
              } else if (num.loan.proportion > 1/25) { 
                chr.msg.2 <- "Although your median income is <b>lower than average</b>, people in your income bracket have <b>good success</b> getting home loans in your metropolitan area."
              } else {
                chr.msg.2 <- "Compared to other income brackets, <b>not many home loans</b> are issued to people in your income bracket."
              }
              output$msg.2 <- renderText({ chr.msg.2 })
              
              #Describe how loans changed year to year
              dt.count.year <- data.table(year = int.year, count = lapply(int.year, function(x) { 
                sum(dt.loans.cut.hist[year == x]$count) }))
              
              int.count.2012 <- dt.count.year[year == 2012, count][[1]]
              int.count.2013 <- dt.count.year[year == 2013, count][[1]]
              int.count.2014 <- dt.count.year[year == 2014, count][[1]]
              
              #Can't compare the years if there were no loans in the previous year (division by zero)
              if (int.count.2012 == 0 | int.count.2013 == 0 | int.count.2014 == 0) {
                chr.msg.3 <- paste0("Cannot compare yearly numbers due to lack of data points.")
              } else {
                #Look at percent change
                num.change.2012.2013 <- (dt.count.year[year == 2013, count][[1]] - dt.count.year[year == 2012,count][[1]]) / 
                  dt.count.year[year == 2012,count][[1]]
                num.change.2013.2014 <- (dt.count.year[year == 2014, count][[1]] - dt.count.year[year == 2013,count][[1]]) / 
                  dt.count.year[year == 2013,count][[1]]
          
                if (num.change.2012.2013 < 0) {
                  chr.msg.3 <- paste0("The home loan market is shrinking for you. There will be a <b>", abs(round(num.change.2012.2013 * 100)), 
                                      "%</b> decrease in the number of these loans issued in <b>2013</b> compared to the previous year, ", collapse = "")
                  if (num.change.2013.2014 < 0) {
                    chr.msg.3 <- paste0(chr.msg.3, "with a further <b>", abs(round(num.change.2013.2014 * 100)), 
                                        "%</b> decrease in <b>2014</b>!", collapse = "")
                  } else {
                    chr.msg.3 <- paste0(chr.msg.3, "though that number will then recover by <b>", abs(round(num.change.2013.2014 * 100)), 
                                        "%</b> in <b>2014</b>.", collapse = "")
                  }
                } else {
                  chr.msg.3 <- paste0("The home loan market will improve for you. There will be a  <b>", abs(round(num.change.2012.2013 * 100)), 
                                      "%</b> increase in the number of these loans issued in <b>2013</b> compared to the previous year, ", collapse = "")
                  if (num.change.2013.2014 < 0) {
                    chr.msg.3 <- paste0(chr.msg.3, "though there is then a drop of <b>", abs(round(num.change.2013.2014 * 100)), 
                                        "%</b> in <b>2014</b>.", collapse = "")
                  } else {
                    chr.msg.3 <- paste0(chr.msg.3, "and a further increase by <b>", abs(round(num.change.2013.2014 * 100)), 
                                        "%</b> in <b>2014</b>!", collapse = "")
                  }
                }
              }
              output$msg.3 <- renderText({ chr.msg.3 })
              shiny:::setProgress(value = 1)
          }, message = "Simulating home loan")
          
          return("")
        }
        
        #Introductory sentence
        if (input$run.simulation == 0) { 
          output$msg.intro <- renderText({ paste0("<b>Welcome to the year 2012. Press the run simulation button to begin.</b>") })
        }
        
        return("")
        
      })
      
     
    },
    options = list(height = 800)
  )
  
}



