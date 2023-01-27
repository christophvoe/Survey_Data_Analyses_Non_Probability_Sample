require(shiny)
require(tidyverse)
require(ggplot2)

source("Code_only_shiny.R")

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Inference on the non-probability CoMix data", align = "center")),
  
  #Line break
  br(), 
  
  # Introduction text 
  p("We are working on a non-probability data set from the CoMix project. 
    The data contain information on the number of contacts people had with other
    people during the COVID-19 pandemic, the studied period spans from March 2020 
    to August 2020. The different plots show the average number of contacts per day 
    (per stratum/age group/gender) as well as the number of registered new COVID cases/stringency of government
    measures at that time, over the different weeks. 
    The data were collected using quota sampling, based on age, gender and region.
    This informed our decision for post-stratification on similar strata. 
    The data is available on the Zenodo 
    website", a("https://zenodo.org/record/4905746#.YY4oLlPvKji"), "and is spread over different files. 
    The different files contain information 
    on the participants filling out the survey, the people they were in contact with,
    and some auxiliary information (such as date the survey was filled in). 
    So our first task was to merge the data sets and put all relevant information 
    together. During this data preparation we also calculate the average number 
    of contacts per participants and per survey day. Plus, we calculate the mean
    number of contacts for our age x gender categories/strata. These are needed 
    for our further analysis. 
    
    Since the data was sampled using quota sampling, we use post-stratification. 
    To perform our weighting correctly we need to compare our data to the population statistics.
    The relevant UK population data was retrieved from the UN site: ", 
    a("https://population.un.org/wpp/Download/Standard/Interpolated/"),), 
  
  #Line break
  br(), 
  
  #Introduction statistical analysis
  p("To deal with our challenge of doing inference from non-probability 
      data we have applied post-stratification and created some initial plots 
      to check some of the trends regarding contacts, COVID-19 cases, and government
      policy. Check out the Shiny app below to see these plots!"), 
  
  
  #Line break 
  br(), 
  
  # Sidebar with interactive elements
  sidebarLayout(
    sidebarPanel(
      
      helpText("Choose the statistic of interest"),
      
      selectizeInput(
        inputId = "selectedstat",  # id for selected [...]
        label = "Select statistic:",
        choices = c("Number of cases", "Government stringency"), # choices are all different periods
        selected = "Number of cases", # selected first
        multiple = FALSE, # allows no multiple items to be selected 
        options = list(maxItems = 1) # limits the selection to 
      ),
      
      
      helpText("Choose the statistic of interest"),
      
      
      selectizeInput(
        inputId = "selectstrat",  # id for selected [...]
        label = "Select stratum:",
        choices = c("Stratum", "Gender", "Age group"), # choices are all different periods
        selected = "Stratum", # selected first
        multiple = FALSE, # allows no multiple items to be selected 
        options = list(maxItems = 1) # limits the selection to 
      ),
      
      
    ),
    
    
    mainPanel(
      
      h4("Legend (line interpretation):", align = "top"),
      verbatimTextOutput("txt1"),
      
      h4("The plot:"), # header directly above the plot
      plotOutput("plot1", width = "70%"), # the output for the plot
      
      
      h4("Interpretation: "), # header directly above interpretation of statistical analysis
      textOutput("txt2") # statistical analysis interpretation
      
    )
    
  )))



if (xy[1] == "Contacts" & xy[2] == "Contacts over Time") {
  plotting_post(data.weeks,data.weeks$wgt,"Figure 1: Contacts over Time")}
if (x == "Contacts/Cases" & y == "Contacts over Time") {
  plotting_covid(data.weeks,data.weeks$wgt,"Figure 2: Contacts over Time")}
if (x == "Contacts" & y == "Contacts by Age") {
  plotting_post(data.age.weeks,data.age.weeks$part_age_group,"Figure 3: Contacts by Age Group")}
if (x == "Contacts/Cases" & y == "Contacts by Age") {
  plotting_covid(data.age.weeks,data.age.weeks$part_age_group,"Figure 4: Contacts by Age Group")}
if (x == "Contacts" & y == "Contacts by Gender") {
  plotting_post(data.gender.weeks,data.gender.weeks$part_gender,"Figure 5: Contacts by Gender")}
if (x == "Contacts/Cases" & y == "Contacts by Gender") {
  plotting_covid(data.gender.weeks,data.gender.weeks$part_gender,"Figure 6: Contacts by Gender")}
if (x == "Contacts" & y == "Contacts by Household") {
  plotting_post(data.hhtype.weeks,data.hhtype.weeks$hh_type,"Figure 7: Contacts by Household Typ")}
if (x == "Contacts/Cases" & y == "Contacts by Household") {
  plotting_covid(data.hhtype.weeks,data.hhtype.weeks$hh_type,"Figure 8: Contacts by Household Typ")}
if (x == "Contacts" & y == "Contacts by Age and Gender") {
  plotting_post(data.seniors.weeks,interaction(factor(data.seniors.weeks$senior), data.seniors.weeks$part_gender),"Figure 9: Contacts by Age Group and Gender")}
if (x == "Contacts/Cases" & y == "Contacts by Age and Gender") {
  plotting_covid(data.seniors.weeks,interaction(factor(data.seniors.weeks$senior), data.seniors.weeks$part_gender),"Figure 10: Contacts by Age Group and Gender")}
if (x == "Contacts" & y == "Contacts by Age and Household") {
  plotting_post(data.seniors.hh,interaction(factor(data.seniors.hh$senior), data.seniors.hh$hh_type),"Figure 11: Contacts by Age Group and Household Type")}
if (x == "Contacts/Cases" & y == "Contacts by Age and Household") {
  plotting_covid(data.seniors.hh,interaction(factor(data.seniors.hh$senior), data.seniors.hh$hh_type),"Figure 12: Contacts by Age Group and Household Type")}

server <- function(input, output) {
  
  observe({
    
    x <- input$selectedstat 
    
    y <- input$selectstrat
    
    # creates plot 1
    output$plot1 <- renderPlot({
      
      if (x == "Number of cases" & y == "Stratum") {
        
        # this plot plots the number of covid cases per week against the average number of 
        # weekly contacts per stratum
        
        g1 <- ggplot(data.weeks) +
          theme_pubclean() +
          #geom_col(aes(x=weeks, y=stringency_index*50), 
          #fill="white", colour="grey")+
          geom_col(aes(x=weeks, y=cases_new), 
                   fill="white", colour="black")+
          geom_smooth(mapping = aes(x=weeks, y=F_18_29_wgt*500), se=FALSE,
                      col = "red", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_18_29_wgt*500), se=FALSE,
                      col = "blue", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=F_30_39_wgt*500), se=FALSE,
                      col = "orange", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_30_39_wgt*500), se=FALSE,
                      col = "yellow", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=F_40_49_wgt*500), se=FALSE,
                      col = "purple", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_40_49_wgt*500), se=FALSE,
                      col = "green", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=F_50_59_wgt*500), se=FALSE,
                      col = "pink", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_50_59_wgt*500), se=FALSE,
                      col = "brown", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=F_60_69_wgt*500), se=FALSE,
                      col = "turquoise2", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_60_69_wgt*500), se=FALSE,
                      col = "aquamarine1", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=F_70_120_wgt*500), se=FALSE,
                      col = "#006633", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_70_120_wgt*500), se=FALSE,
                      col = "grey27", size=0.3) +
          scale_y_continuous("COVID-19 cases",
                             sec.axis = sec_axis(~ . / 500, name = "Mean Contacts by Strata and Stringency (1-10)")) +
          labs(title="COVID Cases and Contact Frequency by Stratum", x="Calendar Weeks")
        
        print(g1)
        
      }
      
      if (x == "Number of cases" & y == "Gender") {
        
        # this plot plots the number of covid cases per week against the average number of 
        # weekly contacts per gender
        g2 <- ggplot(data.weeks) +
          theme_pubclean() +
          geom_col(aes(x=weeks, y=cases_new), 
                   fill="white", colour="black")+
          geom_smooth(mapping = aes(x=weeks, y=(F_18_29_wgt+F_30_39_wgt+F_40_49_wgt+
                                                  F_50_59_wgt+F_60_69_wgt+F_70_120_wgt)*100),
                      se=FALSE, col = "red", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(M_18_29_wgt+M_30_39_wgt+M_40_49_wgt+
                                                  M_50_59_wgt+M_60_69_wgt+M_70_120_wgt)*100),
                      se=FALSE, col = "blue", size=0.3) +
          scale_y_continuous("COVID-19 cases",
                             sec.axis = sec_axis(~ . / 100, name = "Number of Average Contacts by Gender")) +
          labs(title="COVID Cases and Contact Frequency by Gender", x="Calendar Weeks")
        
        print(g2)
        
      }
      
      if (x == "Number of cases" & y == "Age group") {
        
        # this plot plots the number of covid cases per week against the average number of 
        # weekly contacts per age group.
        g3 <- ggplot(data.weeks) +
          theme_pubclean() +
          geom_col(aes(x=weeks, y=cases_new), 
                   fill="white", colour="black")+
          geom_smooth(mapping = aes(x=weeks, y=(F_18_29_wgt+M_18_29_wgt)*250), se=FALSE,
                      col = "red", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(F_30_39_wgt+M_30_39_wgt)*250), se=FALSE,
                      col = "blue", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(F_40_49_wgt+M_40_49_wgt)*250), se=FALSE,
                      col = "green", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(F_50_59_wgt+M_50_59_wgt)*250), se=FALSE,
                      col = "yellow", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(F_60_69_wgt+M_60_69_wgt)*250), se=FALSE,
                      col = "orange", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(F_70_120_wgt+M_70_120_wgt)*250), se=FALSE,
                      col = "purple", size=0.3) +
          scale_y_continuous("COVID-19 cases",
                             sec.axis = sec_axis(~ . / 250, name="Number of Average Contacts by Age Group")) +
          labs(title="COVID Cases and Contact Frequency by Age Group", x="Calendar Weeks")
        
        print(g3)
        
      }
      
      
      if (x == "Government stringency" & y == "Stratum") {
        
        # this plot plots the stringency of the covid measures against the average number of 
        # weekly contacts per stratum.
        
        g4 <- ggplot(data.weeks) +
          theme_pubclean() +
          geom_col(aes(x=weeks, y=stringency_index), 
                   fill="white", colour="black")+
          geom_smooth(mapping = aes(x=weeks, y=F_18_29_wgt*10), se=FALSE,
                      col = "red", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_18_29_wgt*10), se=FALSE,
                      col = "blue", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=F_30_39_wgt*10), se=FALSE,
                      col = "orange", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_30_39_wgt*10), se=FALSE,
                      col = "yellow", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=F_40_49_wgt*10), se=FALSE,
                      col = "purple", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_40_49_wgt*10), se=FALSE,
                      col = "green", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=F_50_59_wgt*10), se=FALSE,
                      col = "pink", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_50_59_wgt*10), se=FALSE,
                      col = "brown", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=F_60_69_wgt*10), se=FALSE,
                      col = "turquoise2", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_60_69_wgt*10), se=FALSE,
                      col = "aquamarine1", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=F_70_120_wgt*10), se=FALSE,
                      col = "#006633", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=M_70_120_wgt*10), se=FALSE,
                      col = "grey27", size=0.3) +
          scale_y_continuous("Stringency of Government Measures",
                             sec.axis = sec_axis(~ . / 10, name = "Number of Average Contacts by Strata")) +
          labs(title="Government Stringency and Contact Frequency by Stratum", 
               x="Calendar Weeks")
        
        print(g4)
        
      }
      
      else if (x == "Government stringency" & y == "Gender") {
        
        # this plot plots the number of covid cases per week against the average number of 
        # weekly contacts by gender
        
        g5 <- ggplot(data.weeks) +
          theme_pubclean() +
          geom_col(aes(x=weeks, y=stringency_index), 
                   fill="white", colour="black")+
          geom_smooth(mapping = aes(x=weeks, y=(F_18_29_wgt+F_30_39_wgt+F_40_49_wgt+
                                                  F_50_59_wgt+F_60_69_wgt+F_70_120_wgt)*3),
                      se=FALSE, col = "red", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(M_18_29_wgt+M_30_39_wgt+M_40_49_wgt+
                                                  M_50_59_wgt+M_60_69_wgt+M_70_120_wgt)*3),
                      se=FALSE, col = "blue", size=0.3) +
          scale_y_continuous("Stringency of Government Measures",
                             sec.axis = sec_axis(~ . / 3, name = "Number of Average Contacts by Gender")) +
          labs(title="Government Stringency and Contact Frequency by Gender", 
               x="Calendar Weeks")
        
        print(g5)
        
      }
      
      
      
      else if (x == "Government stringency" & y == "Age group") {
        
        g6 <- ggplot(data.weeks) +
          theme_pubclean() +
          geom_col(aes(x=weeks, y=stringency_index), 
                   fill="white", colour="black")+
          geom_smooth(mapping = aes(x=weeks, y=(F_18_29_wgt+M_18_29_wgt)*3), se=FALSE,
                      col = "red", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(F_30_39_wgt+M_30_39_wgt)*3), se=FALSE,
                      col = "blue", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(F_40_49_wgt+M_40_49_wgt)*3), se=FALSE,
                      col = "green", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(F_50_59_wgt+M_50_59_wgt)*3), se=FALSE,
                      col = "yellow", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(F_60_69_wgt+M_60_69_wgt)*3), se=FALSE,
                      col = "orange", size=0.3) +
          geom_smooth(mapping = aes(x=weeks, y=(F_70_120_wgt+M_70_120_wgt)*3), se=FALSE,
                      col = "purple", size=0.3) +
          scale_y_continuous("Stringency of Government Measures",
                             sec.axis = sec_axis(~ . / 3, name = "Number of Average Contacts by Age Group")) +
          labs(title="Government Stringency and Contact Frequency by Age Group", 
               x="Calendar Weeks")
        
        print(g6)
        
      }
      
      
    })
    
  })
  
  
  # Interpretation of the plot
  #
  
  
  output$txt1 <- renderText({ 
    
    
    
    
    paste("For 'Stratum': red = F 18-29 y/o, bue = M 18-29 y/o, orange = F 30-39 y/o,
             yellow = M 30-39 y/o, purple = F 40-49 y/o, green = M 40-49 y/o, 
              pink = F 50-59 y/o, brown = M 50-59 y/o, turquoise = F 60-69 y/o, 
              aqua = M 60-69 y/o, dark green = F ≥ 70 y/o, grey = M ≥ 70 y/o",
          
          
          "For 'Gender': blue = male, red = female", 
          
          "For 'Age group': red = 18-29 y/o, blue = 30-39 y/o, green = 40-49,
            yellow = 50-59, orange = 60-69 y/o, purple ≥ 70 y/o", sep="\n")
    
    
  })
  
  
  
  
  output$txt2 <- renderText({ 
    
    
    
    if (input$selectedstat == "Number of cases" & input$selectstrat == "Stratum") {
      
      paste("This plot shows the number of registered COVID-19 cases as well 
                  as the contact frequency per age x gender stratum over time. 
                  We see some clear differences between the different strata. 
                  For example, women aged 40-49 stand out with a clear increase
                  in contacts around the 25 week mark. Further analysis will 
                  have to be performed to examine all these differences. In terms
                  of general trends across time we see that the number of contacts
                  seems to increase when COVID cases are low. This is most likely
                  due COVID regulations being less stringent in the periods with
                  low numbers of COVID cases. Previous analysis (Gimma et al., 2021)
                  has already shown that these trends coincide with changes in
                  COVID-19 measures/policy implemented by the government. This 
                  is something we will analyze further as well (take a look at the
                  Government Stringency plots to check if you can already see this trend) .")
      
    }
    
    else if (input$selectedstat == "Number of cases" & input$selectstrat == "Gender") {
      
      paste("This plot shows the number of registered COVID-19 cases as 
            well as the contact frequency per gender group over time. We only 
            see a difference between gender in the later weeks. We still need to do further 
            analysis to investigate this difference. Our initial thoughts are 
            hat this may be due to attrition (loss of participants over time), 
            so in the different waves we may find a difference in for example 
            average age in men versus women which causes this trend pictured 
            above. In terms of trends across time we see that the number of 
            contacts seems to increase when COVID cases are low. This is most 
            likely due COVID regulations being less stringent in the periods 
            with low numbers of COVID cases. Previous analysis (Gimma et al., 2021) 
            has already shown that these trends coincide with changes in COVID-19
            measures/policy implemented by the government. This is something we 
            will analyze further as well (take a look at the
            Government Stringency plots to check if you can already see this trend).")
      
    }
    
    else if (input$selectedstat == "Number of cases" & input$selectstrat == "Age group") {
      
      paste("This plot shows the number of registered COVID-19 cases as 
                  well as the contact frequency per age group over time. We see 
                  clear differences between groups, and between trends. The oldest
                  age group always has a lower frequency of contact and for this
                  group we also see a relatively flat trend. For the other age 
                  groups we see relatively similar trends, where the number of 
                  contacts seems to increase when COVID cases are low. This is
                  most likely due COVID regulations being less stringent in the
                  periods with low numbers of COVID cases. Previous analysis 
                  (Gimma et al., 2021) has already shown that these trends coincide 
                  with changes in COVID-19 measures/policy implemented by the government.
                  This is something we will analyze further as well (take a look at the
                  Government Stringency plots to check if you can already see this trend).")
      
    }
    
    else if (input$selectedstat == "Government stringency" & input$selectstrat == "Stratum") {
      
      paste("This plot shows the level of stringency of the government measures 
                      as well as the contact frequency per age x gender stratum over time. 
                      We see some clear differences between the different strata. For example, 
                      women aged 40-49 stand out with a clear increase in contacts around 
                      the 25 week mark. Further analysis will have to be performed to 
                      examine all these differences. In terms of government stringency
                      we see a slight difference in contact frequency, with the frequency
                      being lower when government stringency was higher. But we also 
                      see an interesting peak around week 25 where government frequency
                      was high but contact frequency was also high. Further research
                      will be done to investigate this peak.")
      
    }
    
    else if (input$selectedstat == "Government stringency" & input$selectstrat == "Gender") {
      
      paste("This plot shows the level of stringency of the government 
                      measures as well as the contact frequency contact frequency per 
                      gender over time. We only see a difference between genders 
                      in the later weeks. We still need to do further analysis to 
                      investigate this difference. Our initial thoughts are that
                      this may be due to attrition (loss of participants over time), 
                      so in the different waves we may find a difference in for
                      example average age in men versus women which causes this 
                      trend pictured above. In terms of government stringency we
                      see a slight difference in contact frequency, with the
                      frequency being lowest when government stringency was highest.")
      
    }
    
    else if (input$selectedstat == "Government stringency" & input$selectstrat == "Age group") {
      
      paste("This plot shows the level of stringency of the government
                      measures as well as the contact frequency per age group over
                      time. We see clear differences between groups, and between
                      trends. The oldest age group always has a lower frequency 
                      of contact and for this group we also see a relatively flat 
                      trend. For the other age groups we see relatively similar trends,
                      where we see a slight difference in contact frequency, with 
                      the frequency being lowest when government stringency was highest. 
                      But we also see an interesting peak around week 25 where 
                      government frequency was high but contact frequency was also high.
                      Further research will be done to investigate this peak.")
      
    }
    
    
  })
  
  
  
}

