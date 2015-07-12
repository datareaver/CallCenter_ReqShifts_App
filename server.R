library(dplyr)
library(reshape2)
library(lpSolve)
library(RColorBrewer)
library(ggplot2)
library(shiny)
library(queueing)
library(xtable)

handle_zero_queues <- function(x,y) {
  if (x != 0) {
    NewInput.MMC(x,service_rate,y) %>%
      QueueingModel.i_MMC() %>%
      Wq()
  } else 0
}

span <- 168 #based on week sub units for arrivals

set.seed(82) #makes download example file consistent

Weekday <- ordered(c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'),
                   levels = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))
Hour <- ordered(c('0:00','0:30','1:00','1:30','2:00','2:30','3:00','3:30','4:00','4:30',
                '5:00','5:30','6:00','6:30','7:00','7:30','8:00','8:30','9:00','9:30',
                '10:00','10:30','11:00','11:30','12:00','12:30','13:00','13:30','14:00',
                '14:30','15:00','15:30','16:00','16:30','17:00','17:30','18:00','18:30',
                '19:00','19:30','20:00','20:30','21:00','21:30','22:00','22:30','23:00',
                '23:30'),levels = c('0:00','0:30','1:00','1:30','2:00','2:30','3:00','3:30','4:00','4:30',
                                    '5:00','5:30','6:00','6:30','7:00','7:30','8:00','8:30','9:00','9:30',
                                    '10:00','10:30','11:00','11:30','12:00','12:30','13:00','13:30','14:00',
                                    '14:30','15:00','15:30','16:00','16:30','17:00','17:30','18:00','18:30',
                                    '19:00','19:30','20:00','20:30','21:00','21:30','22:00','22:30','23:00',
                                    '23:30'))


# Reactive Data
# results.df
# shifts.df
# solutions.df
#
# Output:
# loadplot
# mean_Wq
# excess_capacity
# sched_text

shinyServer(function(input, output) {


# MMC Queue Server Calculations -------------------------------------------

    output$example_load <- downloadHandler(


      filename = function() {
        paste("example","csv", sep = ".")
      },

      content = function(file) {
        # Write to a file specified by the 'file' argument
        example_load <- unlist(lapply(rep(c(2,5,9,15,11,9,8,5),7),rpois,n=3))
        example_load[example_load == 0] <- 1
        write.table(example_load, file,sep=",",row.names = FALSE,col.names = FALSE)
      }


    )


    #Construct the shifts based on inputs
    results.df <- reactive({


      waiting_time <- input$waiting_time
      service_rate <- input$service_rate
      #if(is.null(input$file)) { arrivals <- unlist(lapply(rep(c(2,5,9,15,11,9,8,5),8),rpois,n=6))
      #  } else arrivals <- unlist(read.csv(input$file$datapath,header=F))
      arrivals <- unlist(read.csv(input$file$datapath,header=F)[1:span,])

      opt_servers <- vector(mode="integer",length(arrivals))
      Wq <- vector(mode="numeric",length(arrivals))
      i <- 1
      #calculate optimal servers
      for (arrival in arrivals) {

        min_servers <- (arrival %/% service_rate) + (arrival > 0)
        opt_server <- min_servers
        mmc_Wq <- waiting_time

        repeat {

          if (opt_server != 0) {
            mmc_input <- NewInput.MMC(arrival,service_rate,c=opt_server)
            mmc <- QueueingModel.i_MMC(mmc_input)
            mmc_Wq <- Wq(mmc)
          } else {
            mmc_Wq <- 0
            break
          }

          if (mmc_Wq < waiting_time) break
          opt_server <- opt_server + 1
        }

        opt_servers[i] <- opt_server
        Wq[i] <- mmc_Wq
        i <- i + 1
      }

      data.frame(Weekday=rep(Weekday,each=24),Hour=rep(0:23,7),Arrival_Rate = arrivals,Servers = opt_servers,Wq)


    })


# Shift matrix -------------------------------------------------



    shifts.df <- reactive({


        if(is.null(input$file))     return(NULL)
        #if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)

        shift.length <- as.numeric(input$shift.length)

        #construct shift matrices
        shift.build <- c()
        shifts <- c()
        for (i in shift.length) {

            shift1 <- c(rep(1,i),rep(0,span-i))
            shift.build <- shift1
            for (j in 1:(span-1)) {
                shift1 <- c(shift1[length(shift1)],shift1[1:(length(shift1)-1)])
                shift.build <- cbind(shift.build,shift1)
            }
            shifts <- cbind(shifts,shift.build)
        }
        rm(i)
        dimnames(shifts) <- NULL

        shifts


    })


# Optimization MIP ---------------------------------------------------------



    solutions.df <- reactive({


        if(is.null(input$file))     return(NULL)
        #if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)

        shift.length <- as.numeric(input$shift.length)
        load <- results.df()$Servers
        starts <- as.numeric(c(input$start1,input$start2,input$start3,input$start4,
                               input$start5,input$start6,input$start7))

        #Construct LP matrices
        #
        #Objective:     min shifts
        #Constraints:   No. of Servers Scheduled >= Required Servers
        #               Shift lengths constrained by user choice
        #               Shift start times constrained by user choice
        #
        #

        shifts <- shifts.df()

        lhs <- shifts

        #find columns that will not be turned off
        select <- rep(0:(length(shift.length)-1)*span,each=length(starts)) + starts

        lhs[,-select] <- 0

        oper <- rep('>=',nrow(shifts))

        rhs <- as.numeric(load)

        obj <- rep(1,ncol(shifts))

        int <- 1:ncol(shifts)

        #Solve LP
        lp.model <- lp(direction = 'min',obj,lhs,oper,rhs,transpose.constraints = T,all.int=TRUE)
        #lp.model <- Rglpk_solve_LP(obj,lhs,oper,rhs,types=rep("I",ncol(shifts)))
        #lp.model <- Rsymphony_solve_LP(obj,lhs,oper,rhs,types=rep("I",ncol(shifts)),first_feasible = FALSE,write_lp=TRUE)
        lp.model$solution


    })


# Server Optimization Plot -------------------------------------------------



    output$loadplot <- renderPlot({


        if(is.null(input$file))     return(NULL)
        #if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)

        #pass in derived variables and matrices
        results <- results.df()
        solutions <- solutions.df()
        shifts <- shifts.df()


        #Develop Load vs. Scheduled Capacity Plot
        Excess_Servers <- rowSums(t(solutions * t(shifts)))-results$Servers
        plot.input <- data.frame(select(results,Weekday,Hour,Required_Servers=Servers),Excess_Servers) %>%
          melt(id.vars = c('Weekday','Hour'),variable.name = 'Type',value.name = 'Value')
        colors <- brewer.pal(12,'Set3')[c(5,12)]
        #generate plot for output
        ggplot(plot.input,aes(Hour,Value,fill=Type,color=Type)) + geom_bar(stat="identity",alpha=.3) +
          theme(text=element_text(size = 20)) + facet_grid(Weekday ~ .,scales="free",margins=TRUE) +
          scale_color_manual(values=colors,name="") + scale_fill_manual(values=colors,name="") +
          theme_bw() + labs(title="Daily Comparison of Optimal Solution vs. Minimum Requirements") +
          ylab(NULL) + scale_x_continuous(breaks=0:23)


    })


# Mean Waiting Time in Queue -----------------------------------------


    output$mean_Wq <- renderText({


      if(is.null(input$file))     return(NULL)
      #if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)


      arrivals <- results.df()$Arrival_Rate
      solutions <- solutions.df()
      shifts <- shifts.df()
      optimal_servers <- rowSums(t(solutions * t(shifts)))
      service_rate <- input$service_rate

      Wq <- mapply(handle_zero_queues,x=arrivals,y=optimal_servers)

      results <- data.frame(Arrival_Rate = arrivals,optimal_servers,Wq)

      paste('The average waiting time in queue is ',
           sprintf("%.2f",as.numeric(summarize(results,sum(Wq*Arrival_Rate)/sum(Arrival_Rate)))))


    })


# Excess Capacity ---------------------------------------------------------

    output$excess_capacity <- renderText({


      if(is.null(input$file))     return(NULL)
      #if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)

      results <- results.df()
      solutions <- solutions.df()
      shifts <- shifts.df()
      optimal_servers <- rowSums(t(solutions * t(shifts)))


      paste('The excess capacity is ',sum(optimal_servers-results[['Servers']]))


  })

# Table Schedules ---------------------------------------------------------

    output$schedtext <- renderTable({



        if(is.null(input$file))     return(NULL)

        solutions <- solutions.df()
        shifts <- shifts.df()
        shift.length <- as.numeric(input$shift.length)
        wd.list <- factor(c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'),
                          levels = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

        shift.times <- data.frame(Weekday = unlist(lapply(wd.list,function(x) rep(x,24))),
                                  Shift.Start = paste0(0:23,":00"),
                                  Shift.Length = sort(rep(shift.length,span)))

        schedule <- cbind(shift.times[solutions > 0,],N = solutions[solutions > 0])
        schedule <- schedule[order(schedule$Weekday),]
        row.names(schedule) <- NULL
        xtable(schedule,type = 'html')


    })

    output$table <- renderTable({


      if(is.null(input$file))     return(NULL)

      results <- results.df()
      arrivals <- results.df()$Arrival_Rate
      solutions <- solutions.df()
      shifts <- shifts.df()
      service_rate <- input$service_rate

      optimal_servers <- rowSums(t(solutions * t(shifts)))

      optimal_Wq <- mapply(handle_zero_queues,x=arrivals,y=optimal_servers)

      output_table <- data.frame(results,Optimal_Servers=optimal_servers,Optimal_Wq=optimal_Wq,row.names = NULL) %>%
        rename(Required_Servers = Servers,Required_Wq = Wq)

      xtable(output_table,type='html')


    })

})



