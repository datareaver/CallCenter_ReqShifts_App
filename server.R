library(dplyr)
library(ggplot2)
library(reshape2)
library(lpSolve)
library(RColorBrewer)
library(ggplot2)
library(shiny)
library(queueing)

# Reactive Data
# load.df
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


    #Construct the shifts based on inputs
    load.df <- reactive({
      waiting_time <- input$waiting_time
      service_rate <- input$service_rate

      opt_servers <- vector(mode="integer",length(arrivals))
      Wq <- vector(mode="numeric",length(arrivals))
      i <- 1
      #calculate optimal servers
      for (arrival in arrivals) {

        min_servers <- ceiling(arrival/service_rate)
        opt_server <- min_servers
        mmc_Wq <- waiting_time

        repeat {
          mmc_input <- NewInput.MMC(arrival,service_rate,c=opt_server)
          mmc <- QueueingModel.i_MMC(mmc_input)
          mmc_Wq <- Wq(mmc)
          if (mmc_Wq < waiting_time) break
          opt_server <- opt_server + 1
        }

        opt_servers[i] <- opt_server
        Wq[i] <- mmc_Wq
        i <- i + 1
      }

      data.frame(Arrival_Rate = arrivals,Servers = opt_servers,Wq)
    })


# Shift matrix -------------------------------------------------



    shifts.df <- reactive({
        span <- 168
        start <- as.numeric(input$start)


        if(is.null(input$file))     return(NULL)
        if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)

        shift.length <- as.numeric(input$shift.length)
        load <- output$load.df()$Servers


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

        #reduce to selected start times
        select <- as.numeric(c(input$start1,input$start2,input$start3,input$start4,
                               input$start5,input$start6,input$start7))
        shifts <- shifts[,select]
        shifts

    })


# Optimization MIP ---------------------------------------------------------



    solutions.df <- reactive({


        span <- 168
        if(is.null(input$file))     return(NULL)
        if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)


        shift.length <- as.numeric(input$shift.length)
        load <- unlist(read.csv(input$file$datapath,header = F))

        #Construct LP matrices
        #
        #Objective:     min shifts
        #Constraints:   No. of Servers Scheduled >= Required Servers
        #               Shift lengths constrained
        #
        #

        shifts <- shifts.df()

        #diag is slack variable
        lhs <- cbind(shifts,diag(1,nrow(shifts)))

        oper <- rep('>=',nrow(shifts))

        rhs <- as.numeric(load)

        obj <- rep(0:1,each = ncol(shifts))

        int <- 1:ncol(shifts)

        #Solve LP
        lp.model <- lp(direction = 'min',obj,lhs,oper,rhs,transpose.constraints = T,int)
        solutions <- lp.model$solution[1:ncol(shifts)]
        solutions
    })


# Server Optimization Plot -------------------------------------------------



    output$loadplot <- renderPlot({


        span <- 168
        if(is.null(input$file))     return(NULL)
        if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)

        #pass in derived variables and matrices
        shift.length <- as.numeric(input$shift.length)
        load <- unlist(read.csv(input$file$datapath,header = F))
        solutions <- solutions.df()
        shifts <- shifts.df()


        #Develop Load vs. Scheduled Capacity Plot
        sched.cap <- rowSums(t(solutions * t(shifts)))
        plot.input <- data.frame(Time = 1:span,
                                 Excess_Servers = sched.cap,Required_Servers = load) %>%
          melt(id.vars = 'Time',variable.name = 'Type',value.name = 'Value')

        #Create Labels with Hours and Weekdays and assign custom colors
        wd.list <- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')
        labels <- c('')
        for (i in wd.list) {
            labels <- c(labels,i,'')
        }
        labels <- paste(c(rep(seq(0,23,12),7),0),labels,sep = '\n')
        colors <- brewer.pal(12,'Set3')[c(5,12)]


        #generate plot for output
        ggplot(plot.input,aes(Time,Value,fill=Type,color=Type)) + geom_density(stat="identity",alpha=.3) +
          theme(text=element_text(size = 20)) + scale_fill_manual(values=colors,name="") +
          scale_color_manual(values=colors,name="") +
          scale_x_discrete(breaks = c(seq(1,168,12),168),labels = labels) + xlab(NULL) + ylab(NULL) +theme_bw()

    })


# Mean Waiting Time in Queue -----------------------------------------


    output$mean_Wq <- renderText({
      arrivals <- output$load.df()[['Arrival_Rate']]
      solutions <- solutions.df()
      sched.cap <- sched.cap <- rowSums(t(solutions * t(shifts)))
      service_rate <- input$service_rate

      Wq <- mapply(function(x,y) NewInput.MMC(x,service_rate,y) %>%
                     QueueingModel.i_MMC() %>% Wq(),arrivals,sched.cap)
      results <- data.frame(Arrival_Rate = arrivals,sched.cap,Wq)
      paste('The average waiting time in queue is ',
            as.numeric(summarize(results,sum(Wq*Arrival_Rate)/sum(Arrival_Rate))))
    })


# Excess Capacity ---------------------------------------------------------

    output$excess_capacity <- renderText({
    results <- output$load.df()
    solutions <- solutions.df()
    paste('The excess capacity is ',sum(results[['sched.cap']]-solutions))
  })

# Table Schedules ---------------------------------------------------------

    output$schedtext <- renderTable({
        library(xtable)
        if(is.null(input$file))     return(NULL)

        span <- 168
        solutions <- solutions.df()
        shifts <- shifts.df()
        start <- as.numeric(input$start)
        shift.length <- as.numeric(input$shift.length)
        wd.list <- factor(c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'),
                          levels = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

        select <- as.numeric(c(input$start1,input$start2,input$start3,input$start4,
                               input$start5,input$start6,input$start7))

        shift.times <- data.frame(Weekday = unlist(lapply(wd.list,function(x) rep(x,24))),
                                  Shift.Start = paste(0:23,":00",sep = ''),
                                  Shift.Length = sort(rep(shift.length,span)))[select,]

        schedule <- cbind(shift.times[solutions > 0,],N = solutions[solutions > 0])
        schedule <- schedule[order(schedule$Weekday),]
        row.names(schedule) <- NULL
        xtable(schedule,type = 'html')



    })


})



# testing ---------------------------------------------------------------------
results <- data.frame(Arrival_Rate = arrivals,Servers = opt_servers,Wq)
mean_Wq <- as.numeric(summarize(results,sum(Wq*Arrival_Rate)/sum(Arrival_Rate)))

