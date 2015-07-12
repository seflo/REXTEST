######################
# R.Ex v. 0.1

######################
## Admin Functionality


  #Error Address
  observe({

    if(my$id!="error")
      return()

    output$errorlabel <- renderUI({HTML("</br></br></br></br></br></br>YOU HAVE ENTERED AN IMPROPER ADDRESS" )})
    
  })


  #Admin labels
  observe({

    if(my$id!="admin")
      return()

    output$adminlabel1 <- renderUI({HTML("<h4>CONTROL PAGE </h4> <br/>" )})
    output$adminlabel2 <- renderUI({HTML("<h5>CONFIG FILE </h5> " )})
    output$adminrule <- renderUI({HTML("<hr>" )})

  })

  #Show status table
  observe({
  
    if(my$id!="admin")
      return()

    output$statustab <- renderDataTable(admin_status(), options = list(searching=FALSE,paging = FALSE))

  })

admin_status<-reactive({
  data.frame(
        Subject=state$connected,
        Period=rep(state$period,length(state$connected)),
        Round=rep(state$round,length(state$connected)),
        Stage=rep(state$stage,length(state$connected)),
        Phase=rep(state$phase,length(state$connected)),
        Paused=rep(state$paused,length(state$connected)),
        Earnings=floor(state$totalearnings[state$connected]),
        Ready=state$ready[state$connected]
      )
})


  #Show pause button
  output$pausebutton <- renderUI({
 
    if(my$id!="admin")
      return()

      if(state$phase=="start"){

        actionButton("start", "Start Experiment",icon("play"))

      }else{

        if(state$phase=="instructions"){

          actionButton("end_instructions", "End Instructions",icon("play"))

        }else{

          if(state$phase=="quiz"){

            actionButton("end_quiz", "End Quiz",icon("play"))
          
          }else{

            if(state$phase=="survey"){
              
              actionButton("end_survey", "End Survey",icon("play"))

            }else{

              if(state$paused==TRUE){
                actionButton("pause", "Resume Game",icon("play"))
              }else{
                actionButton("pause", "Pause Game",icon("pause"))
              }

            }


          }

        }

      }

  })

  #Show download button
  output$downloadbutton<-renderUI({

    if(my$id!="admin")
      return()

    downloadButton('downloadData', 'Download Data')
  
  })

  #Show force period button
  output$periodbutton <- renderUI({
  
    if(my$id!="admin")
      return()

      actionButton("forceperiod", "Advance Period",icon("forward"))
      
  })

  #Show end experiment button
  output$experimentbutton <- renderUI({
  
    if(my$id!="admin")
      return()

      actionButton("forceend", "End Experiment",icon("stop"))
      
  })

  #Show debugging button
  output$debugbutton<-renderUI({
    
    if(my$id!="admin")
      return()

    actionButton("console","Debug",icon("search"))
  
  })

  #Show end experiment button
  output$restartbutton <- renderUI({
  
    if(my$id!="admin")
      return()

      actionButton("restart", "Restart Experiment",icon("refresh"))
      
  })
 
  # Start Experiment
  observeEvent(input$start,{

       if(training()){
        iterate("period")
       }else{
        state$phase<-"waiting"
      }

  })

  # Force Period
  observeEvent(input$forceperiod,{

    #iterate("period")

    if(survey()!="" & state$period!=0){
      state$survey<-rep(TRUE,n()); state$phase<-"survey"
    }else{
      state$ready<<-rep(TRUE,n()); iterate("period")
    }

  })

  #Enter Debugging Browser
  observeEvent(input$console,{

    if(input$console != 0) {
      options(browserNLdisabled=TRUE)
      saved_console<-".RDuetConsole"
      if (file.exists(saved_console)) load(saved_console)
      isolate(browser())
      save(file=saved_console,list=ls(environment()))
      #https://groups.google.com/forum/#!topic/shiny-discuss/YIusppqZ8cg
    }

  })


  # End Experiment
  observeEvent(input$forceend,{

    if(length(state$pay[,1])<=0)
      return()

    event("experiment_end")
    state$phase<<-"finished"

  })

  # Restart Experiment
  observeEvent(input$restart,{

    rex_build()
    refresh$me<<-rep(TRUE,n()) 
    output$reset <- renderText({ "<script>window.location.reload();</script>" })

  })

  # Pause Experiment
  observeEvent(input$pause,{

    if(input$pause<1)
      return()

      if(state$paused==TRUE){
        state$paused<-FALSE
      }else{
        state$paused<-TRUE
      }
      
  })

  observeEvent(input$end_instructions,{

    if(input$end_instructions<1)
      return()

    state$instructions<-rep(FALSE,n())

    if(quiz()!=""){
      state$phase<-"quiz"
      state$quiz<-rep(TRUE,n())
    }else{

      state$phase<-ifelse(prechoice(),"prechoice",ifelse(duration()>0,"timed","untimed"))
      state$ready<-rep(FALSE,n())
      event("period_start")        
      
    }

  })

  observeEvent(input$end_quiz,{

    if(input$end_quiz<1)
      return()

    state$quiz<-rep(FALSE,n())
    
    state$phase<-ifelse(prechoice(),"prechoice",ifelse(duration()>0,"timed","untimed"))
    state$ready<-rep(FALSE,n())
    event("period_start")    
    
  })


  observeEvent(input$end_survey,{

    if(input$end_survey<1)
      return()

    state$survey<-rep(FALSE,n())
    state$ready<<-rep(TRUE,n()); iterate("period")


  })


  #Show config upload button
  output$uploadconfig <- renderUI({

      if(my$id!="admin")
        return()

      fileInput('file1', 'Upload Config',
                accept=c('text/csv', 
                  'text/comma-separated-values,text/plain', 
                  '.csv')
      )
  })


  #Show config table
  observe({
    
    if (is.null(config))
      return(NULL)


    if(my$id!="admin")
      return()

    output$configtab <- renderDataTable(config, options = list(searching=FALSE,paging = FALSE))

  })

  #Upload config file
  observe({

    if(my$id!="admin")
      return()

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)
      
    config<<-read.table(inFile$datapath,fill=TRUE,header=TRUE,sep=",")
    #isolate({state$n<-config$n[1]})
    rex_build()
    refresh$me<<-rep(TRUE,n())
    output$reset <- renderText({ "<script>window.location.reload();</script>" }) 

    input$idinput
    input$file1

    if (is.null(config))
      return(NULL)

    output$configtab <- renderDataTable(config, options = list(searching=FALSE,paging = FALSE,binfo=FALSE))

  })

  #Download Data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      #data<-state$data
      #names(data)<-c("Period","Subject","Tick","Round","Group","Action")
      write.csv(state$data, file,row.names=FALSE)
    }
  )


  ######################
  ## Payment Page Functionality

  #Admin labels
  observe({

    if(my$id!="payment")
      return()

    output$paymentlabel <- renderUI({HTML("<h4>PAYMENTS</h4> <br/>" )})

  })

  #Show payoff table
  output$allpaytab <- renderDataTable(if(my$id=="payment"){state$pay}, options = list(searching=FALSE,paging = FALSE,binfo=FALSE))




  # output$adminPage<- renderUI({

  #   if(my$id!="admin")
  #     return()

  #   #fluidPage(
  #     fluidRow(
  #       fluidRow(
  #         column(4,offset=4,htmlOutput("adminlabel1",align="center"))
  #       ),
  #       fluidRow(
  #         column(6,offset=3,div(dataTableOutput("statustab"),style="font-size:90%"))
  #       ),
  #       br(), #br(),
  #       #Admin's Config Upload
  #       fluidRow(
  #         column(2,offset=1,htmlOutput("pausebutton",align="center")),
  #         column(2,offset=0,htmlOutput("downloadbutton",align="center")),
  #         column(2,offset=0,htmlOutput("periodbutton",align="center")),
  #         column(2,offset=0,htmlOutput("restartbutton",align="center")),
  #         column(2,offset=0,htmlOutput("experimentbutton",align="center"))
  #         #column(2,offset=0,tags$button( "End Experiment", id="experimentbutton", type="button", class="btn action-button", onclick="return confirm('Are you sure?');" )),
  #       ),
  #       fluidRow(column(10,offset=1,htmlOutput("adminrule"))),
  #       fluidRow(
  #         column(3,offset=1,htmlOutput("uploadconfig",align="center")),
  #         column(2,offset=1,htmlOutput("adminlabel2",align="center"))
  #       ),
  #       #Admin's Config Table
  #       fluidRow(
  #         column(9,offset=1,div(dataTableOutput("configtab"),style="font-size:80%"))
  #       )
  #     )
  # })

  # output$paymentPage<- renderUI({

  #   if(my$id!="payment")
  #     return()

  #   #fluidPage(
  #     fluidRow(
  #       fluidRow(
  #         column(2,offset=5,htmlOutput("paymentlabel",align="center"))
  #       ),
  #       fluidRow(
  #         column(6,offset=3,dataTableOutput("allpaytab"))
  #       )
  #     )
  #   #) 

  # })




