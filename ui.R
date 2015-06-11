library(shinythemes)
shinyUI(fluidPage(them = shinytheme("flatly"),

    titlePanel("Service Center Staffing (M/M/C)"),
    h6('Created by Thomas Roh'),
    br(),
    tabsetPanel(


      tabPanel('Main',

               sidebarPanel(

                          h3('Description:'),

                          p("This application takes hourly arrival rates and converts those numbers to the required number of
                          servers needed for each hour based on the desired average waiting time in queue (MMC
                          Queueing Theory). A mixed integer program is then used to find the optimal
                          schedules for the service center. The optimization is constrained by the allowable shift length and
                          shift start times."),

                          h3('Instructions:'),

                          p("Upload a csv file with a single column vector of 168 hourly arrivals. The starting point of the
                          vector is Sunday 12:00 AM. Place '0's in intervals that have no arrivals or the center is closed."),

                          p("Choose the possible length of shifts that you want to include."),

                          p("Enter the desired maximum hourly average of waiting time in queue."),

                          p("Enter the current rate of service."),

                          p("Click on the 'Select Shifts' tab to control the possible shift start times."),

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


#       tabPanel('Select Shifts',
#
#                fluidRow(
#
#                  column(1,
#
#                         checkboxGroupInput('start1', label = h4('Sunday'),
#                                            choices = c('0:00' = 1, '1:00' = 2, '2:00' = 3, '3:00' = 4,
#                                                        '4:00' = 5, '5:00' = 6, '6:00' = 7, '7:00' = 8,
#                                                        '8:00' = 9, '9:00' = 10, '10:00' = 11, '11:00' = 12,
#                                                        '12:00' = 13, '13:00' = 14, '14:00' = 15, '15:00' = 16,
#                                                        '16:00' = 17, '17:00' = 18, '18:00' = 19, '19:00' = 20,
#                                                        '20:00' = 21, '21:00' = 22, '22:00' = 23, '23:00' = 24),
#                                            selected = 1:24,inline = F)
#                  ),
#
#
#                  column(1,offset = .9,
#
#                         checkboxGroupInput('start2', label = h4('Monday'),
#                                            choices = c('0:00' = 25,'1:00' = 26,'2:00' = 27,'3:00' = 28,
#                                                        '4:00' = 29,'5:00' = 30,'6:00' = 31,'7:00' = 32,
#                                                        '8:00' = 33,'9:00' = 34,'10:00' = 35,'11:00' = 36,
#                                                        '12:00' = 37,'13:00' = 38,'14:00' = 39,'15:00' = 40,
#                                                        '16:00' = 41,'17:00' = 42,'18:00' = 43,'19:00' = 44,
#                                                        '20:00' = 45,'21:00' = 46,'22:00' = 47,'23:00' = 48),
#                                            selected = 25:48,inline = F)
#                  ),
#
#
#                  column(1, offset = .9,
#
#                         checkboxGroupInput('start3', label = h4('Tuesday'),
#                                            choices = c('0:00' = 49,'1:00' = 50,'2:00' = 51,'3:00' = 52,
#                                                        '4:00' = 53,'5:00' = 54,'6:00' = 55,'7:00' = 56,
#                                                        '8:00' = 57,'9:00' = 58,'10:00' = 59,'11:00' = 60,
#                                                        '12:00' = 61,'13:00' = 62,'14:00' = 63,'15:00' = 64,
#                                                        '16:00' = 65,'17:00' = 66,'18:00' = 67,'19:00' = 68,
#                                                        '20:00' = 69,'21:00' = 70,'22:00' = 71,'23:00' = 72),
#                                            selected = 49:72,inline = F)
#                  ),
#
#
#                  column(1, offset = .9,
#
#                         checkboxGroupInput('start4', label = h4('Wednesday'),
#                                            choices = c('0:00' = 73,'1:00' = 74,'2:00' = 75,'3:00' = 76,
#                                                        '4:00' = 77,'5:00' = 78,'6:00' = 79,'7:00' = 80,
#                                                        '8:00' = 81,'9:00' = 82,'10:00' = 83,'11:00' = 84,
#                                                        '12:00' = 85,'13:00' = 86,'14:00' = 87,'15:00' = 88,
#                                                        '16:00' = 89,'17:00' = 90,'18:00' = 91,'19:00' = 92,
#                                                        '20:00' = 93,'21:00' = 94,'22:00' = 95,'23:00' = 96),
#                                            selected = 73:96,inline = F)
#                  ),
#
#
#                  column(1, offset = .9,
#
#                         checkboxGroupInput('start5', label = h4('Thursday'),
#                                            choices = c('0:00' = 97,'1:00' = 98,'2:00' = 99,
#                                                        '3:00' = 100,'4:00' = 101,'5:00' = 102,
#                                                        '6:00' = 103,'7:00' = 104,'8:00' = 105,
#                                                        '9:00' = 106,'10:00' = 107,'11:00' = 108,
#                                                        '12:00' = 109,'13:00' = 110,'14:00' = 111,
#                                                        '15:00' = 112,'16:00' = 113,'17:00' = 114,
#                                                        '18:00' = 115,'19:00' = 116,'20:00' = 117,
#                                                        '21:00' = 118,'22:00' = 119,'23:00' = 120),
#                                            selected = 97:120,inline = F)
#                  ),
#
#
#                  column(1, offset = .9,
#
#                         checkboxGroupInput('start6', label = h4('Friday'),
#                                            choices = c('0:00' = 121,'1:00' = 122,'2:00' = 123,'3:00' = 124,
#                                                        '4:00' = 125,'5:00' = 126,'6:00' = 127,'7:00' = 128,
#                                                        '8:00' = 129,'9:00' = 130,'10:00' = 131,'11:00' = 132,
#                                                        '12:00' = 133,'13:00' = 134,'14:00' = 135,'15:00' = 136,
#                                                        '16:00' = 137,'17:00' = 138,'18:00' = 139,'19:00' = 140,
#                                                        '20:00' = 141,'21:00' = 142,'22:00' = 143,'23:00' = 144),
#                                            selected = 121:144,inline = F)
#                  ),
#
#
#                  column(1, offset = .9,
#
#                         checkboxGroupInput('start7', label = h4('Saturday'),
#                                            choices = c('0:00' = 145,'1:00' = 146,'2:00' = 147,'3:00' = 148,
#                                                        '4:00' = 149,'5:00' = 150,'6:00' = 151,'7:00' = 152,
#                                                        '8:00' = 153,'9:00' = 154,'10:00' = 155,'11:00' = 156,
#                                                        '12:00' = 157,'13:00' = 158,'14:00' = 159,'15:00' = 160,
#                                                        '16:00' = 161,'17:00' = 162,'18:00' = 163,'19:00' = 164,
#                                                        '20:00' = 165,'21:00' = 166,'22:00' = 167,'23:00' = 168),
#                                            selected = 145:168,inline = F)
#                  )
#                )
#       ),


      tabPanel('Schedules',

               wellPanel(
                 h3('Weekly Schedules'),
                 p(uiOutput('schedtext'))
                 #textOutput('excess_capacity'),

               )

      )
    )


))




# checkboxGroupInput('start1', label = h4('Sunday'),
#                    choices = c('0:00' = 1, '1:00' = 2, '2:00' = 3, '3:00' = 4,
#                                '4:00' = 5, '5:00' = 6, '6:00' = 7, '7:00' = 8,
#                                '8:00' = 9, '9:00' = 10, '10:00' = 11, '11:00' = 12,
#                                '12:00' = 13, '13:00' = 14, '14:00' = 15, '15:00' = 16,
#                                '16:00' = 17, '17:00' = 18, '18:00' = 19, '19:00' = 20,
#                                '20:00' = 21, '21:00' = 22, '22:00' = 23, '23:00' = 24),
#                    selected = 1:24,inline=TRUE),
#
# checkboxGroupInput('start2', label = h4('Monday'),
#                    choices = c('0:00' = 25,'1:00' = 26,'2:00' = 27,'3:00' = 28,
#                                '4:00' = 29,'5:00' = 30,'6:00' = 31,'7:00' = 32,
#                                '8:00' = 33,'9:00' = 34,'10:00' = 35,'11:00' = 36,
#                                '12:00' = 37,'13:00' = 38,'14:00' = 39,'15:00' = 40,
#                                '16:00' = 41,'17:00' = 42,'18:00' = 43,'19:00' = 44,
#                                '20:00' = 45,'21:00' = 46,'22:00' = 47,'23:00' = 48),
#                    selected = 25:48,inline=TRUE),
#
# checkboxGroupInput('start3', label = h5('Tuesday'),
#                    choices = c('0:00' = 49,'1:00' = 50,'2:00' = 51,'3:00' = 52,
#                                '4:00' = 53,'5:00' = 54,'6:00' = 55,'7:00' = 56,
#                                '8:00' = 57,'9:00' = 58,'10:00' = 59,'11:00' = 60,
#                                '12:00' = 61,'13:00' = 62,'14:00' = 63,'15:00' = 64,
#                                '16:00' = 65,'17:00' = 66,'18:00' = 67,'19:00' = 68,
#                                '20:00' = 69,'21:00' = 70,'22:00' = 71,'23:00' = 72),
#                    selected = 49:72,inline=TRUE),
#
# checkboxGroupInput('start4', label = h5('Wednesday'),
#                    choices = c('0:00' = 73,'1:00' = 74,'2:00' = 75,'3:00' = 76,
#                                '4:00' = 77,'5:00' = 78,'6:00' = 79,'7:00' = 80,
#                                '8:00' = 81,'9:00' = 82,'10:00' = 83,'11:00' = 84,
#                                '12:00' = 85,'13:00' = 86,'14:00' = 87,'15:00' = 88,
#                                '16:00' = 89,'17:00' = 90,'18:00' = 91,'19:00' = 92,
#                                '20:00' = 93,'21:00' = 94,'22:00' = 95,'23:00' = 96),
#                    selected = 73:96,inline=TRUE),
#
# checkboxGroupInput('start5', label = h5('Thursday'),
#                    choices = c('0:00' = 97,'1:00' = 98,'2:00' = 99,
#                                '3:00' = 100,'4:00' = 101,'5:00' = 102,
#                                '6:00' = 103,'7:00' = 104,'8:00' = 105,
#                                '9:00' = 106,'10:00' = 107,'11:00' = 108,
#                                '12:00' = 109,'13:00' = 110,'14:00' = 111,
#                                '15:00' = 112,'16:00' = 113,'17:00' = 114,
#                                '18:00' = 115,'19:00' = 116,'20:00' = 117,
#                                '21:00' = 118,'22:00' = 119,'23:00' = 120),
#                    selected = 97:120,inline=TRUE),
#
# checkboxGroupInput('start6', label = h5('Friday'),
#                    choices = c('0:00' = 121,'1:00' = 122,'2:00' = 123,'3:00' = 124,
#                                '4:00' = 125,'5:00' = 126,'6:00' = 127,'7:00' = 128,
#                                '8:00' = 129,'9:00' = 130,'10:00' = 131,'11:00' = 132,
#                                '12:00' = 133,'13:00' = 134,'14:00' = 135,'15:00' = 136,
#                                '16:00' = 137,'17:00' = 138,'18:00' = 139,'19:00' = 140,
#                                '20:00' = 141,'21:00' = 142,'22:00' = 143,'23:00' = 144),
#                    selected = 121:144,inline=TRUE),
#
# checkboxGroupInput('start7', label = h5('Saturday'),
#                    choices = c('0:00' = 145,'1:00' = 146,'2:00' = 147,'3:00' = 148,
#                                '4:00' = 149,'5:00' = 150,'6:00' = 151,'7:00' = 152,
#                                '8:00' = 153,'9:00' = 154,'10:00' = 155,'11:00' = 156,
#                                '12:00' = 157,'13:00' = 158,'14:00' = 159,'15:00' = 160,
#                                '16:00' = 161,'17:00' = 162,'18:00' = 163,'19:00' = 164,
#                                '20:00' = 165,'21:00' = 166,'22:00' = 167,'23:00' = 168),
#                    selected = 145:168,inline=TRUE)
# )
#
# ),