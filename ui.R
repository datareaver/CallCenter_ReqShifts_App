library(shinythemes)
shinyUI(fluidPage(them = shinytheme("flatly"),

    titlePanel("Service Center Staffing"),
    h6('Created by Thomas Roh'),
    br(),
    tabsetPanel(


      tabPanel('Main',

               sidebarPanel(
                          fileInput('file', h4('Choose CSV File containing Hourly Arrivals:'),
                                    accept=c('text/csv',
                                             'text/comma-separated-values,text/plain',
                                             '.csv')),

                          checkboxGroupInput("shift.length", label = h4("Shift Lengths:"),
                                             choices = c(1,2,3,4,5,6,7,8,9,10,11,12), selected = 8),
                          numericInput('waiting_time','Desired Maximum Average Waiting Time in Queue',value=30,min=0,step=.5),
                          numericInput('service_rate','Average Service Rate (per time unit)',value=1,min=0,step=.5),
                          br(),
                          h3('Results:'),
                          textOutput('mean_Wq'),
                          textOutput('excess_capacity')
               ),
               mainPanel(
                 fixedRow(
                   column(1, offset = 0,

                          plotOutput('loadplot',width = 900, height = 900),
                          br(),
                          p(uiOutput('table'))

                   )
                 )

               )#wellpanel
      ),#tabpanel


      tabPanel('Schedules',

               wellPanel(
                 h3('Weekly Schedules'),
                 p(uiOutput('schedtext'))
                 #textOutput('excess_capacity'),

               )

      )
    )


))