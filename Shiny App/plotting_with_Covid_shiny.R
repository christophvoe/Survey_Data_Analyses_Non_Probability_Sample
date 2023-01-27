library(shiny)
library(ggplot2)
library(readr)

source("plotting_with_Covid_shiny.R")

cases_weeks <- read_csv("data/cases.weeks.csv")
data_age_weeks <- read_csv("data/data.age.weeks.csv")
data_gender_weeks <- read_csv("data/data.gender.weeks.csv")
data_hhtype_weeks <- read_csv("data/data.hhtype.weeks.csv")
data_seniors_hh <- read_csv("data/data.seniors.hh.csv")
data_seniors_weeks <- read_csv("data/data.seniors.weeks.csv")
data_weeks <- read_csv("data/data.weeks.csv")

ui <- fluidPage(
  
  titlePanel(h1("Inference on the non-probability CoMix data", align = "center")),
  
  #Line break
  br(), 
  
  # Introduction text 
  p("This is a non-probability data set from the CoMix project. 
    The data contains information on the number of contacts people in the UK had with other
    people during the COVID-19 pandemic, the studied period spans from March 2020 
    to August 2020. The different plots show the average number of contacts per day 
    (per stratum/age group/gender) as well as the number of registered new COVID cases/stringency of government
    measures at that time, over the different weeks. 
    The data were collected using quota sampling, based on age, gender and region.
    This informed our decision for post-stratification on similar strata. 
    The data is available on the Zenodo 
    website", a("https://zenodo.org/record/4905746#.YY4oLlPvKji"), "and is spread over different files. 
    
    Since the data was sampled using quota sampling, I use post-stratification. 
    To perform our weighting correctly we need to compare our data to the population statistics.
    The relevant UK population data was retrieved from the UN site: ", 
    a("https://population.un.org/wpp/Download/Standard/Interpolated/"),), 
  
  #Line break
  br(), 
  
  #Introduction statistical analysis
  p("To deal with thr challenge of doing inference from non-probability 
      data I have applied post-stratification and created some initial plots 
      to check some of the trends regarding contacts, COVID-19 cases, and government
      policy. Check out the Shiny app below to see these plots!"), 
  
  
  #Line break 
  br(), 
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("plot",
                   h3("Plot input:"), 
                   choices = list("Contacts over Time" = list(data.weeks,data.weeks$wgt,"Contacts over Time"), 
                                  "Contacts by Age" = list(data.age.weeks,data.age.weeks$part_age_group,"Contacts by Age"), 
                                  "Contacts by Gender" = list(data.gender.weeks,data.gender.weeks$part_gender,"Contacts by Gender"),
                                  "Contacts by Household" = list(data.hhtype.weeks,data.hhtype.weeks$hh_type,"Contacts by Household"), 
                                  "Contacts by Age and Gender" = list(data.seniors.weeks,interaction(factor(data.seniors.weeks$senior), data.seniors.weeks$part_gender),"Contacts by Age and Gender"),
                                  "Contacts by Age and Household" = list(data.seniors.hh,interaction(factor(data.seniors.hh$senior), data.seniors.hh$hh_type),"Contacts by Age and Household")),
                   selected = c("Contacts over Time")),
      
      
      radioButtons("plot_type",
                   h3("Plot Type:"),
                   choices = list("Contact" = "Contact", "Cases" = "Cases"), 
                   selected = "Contacts"),
      
      
    ),
    mainPanel(
      
      
      h4("The contatcs plot:"), # header directly above the plot
      plotOutput("contact_plot", width = "70%"), # the output for the plot
      
    )
  )
)
server <- function(input, output) {
  
  output$contact_plot <- renderPlot({
    plotting_covid(as.data.frame(input$plot[1]),input$plot[2]),input$plot[3],content = as.character(input$plot_type))})

}
# Run the application 
shinyApp(ui = ui, server = server)