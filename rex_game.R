

#  GGVIS Clicking https://groups.google.com/forum/#!topic/ggvis/f1itIL5v3PA



#######################
## Functions to handle 
## 1. Configurable Variables with Defaults
## 2. Connection
## 3. Timing (Periods, Rounds, Stages)
## 4. Groupings
## 5. Payments
## 6. Training Tools -- Instructions, Quizzes

#######################
## Configurable Variables with Defaults

periods<-reactive({
    max(config$period)  
})

rounds<-reactive({
    if("rounds"%in%names(config)){parsebrackets(config[config$period==min(max(1,state$period),periods()),]$rounds)}else{1}  
})

stages<-reactive({
    if("stages"%in%names(config)){parsebrackets(config[config$period==min(max(1,state$period),periods()),]$stages)}else{1}  
})

duration<-reactive({
    if("duration"%in%names(config)){parsebrackets(config[config$period==min(max(1,state$period),periods()),]$duration)}else{0}  
})

turn<-reactive({
    if("turn"%in%names(config)){parsebrackets(config[config$period==min(max(1,state$period),periods()),]$turn)}else{rep(state$stage,n())}  
})

stages<-reactive({
    if("stages"%in%names(config)){config[config$period==min(max(1,state$period),periods()),]$stages}else{1}  
})

prechoice<-reactive({
    if("prechoice"%in%names(config)){config[config$period==min(max(1,state$period),periods()),]$prechoice}else{FALSE}  
})

autostart<-reactive({
    if("autostart"%in%names(config)){config[config$period==min(max(1,state$period),periods()),]$autostart}else{FALSE}  
})

autoadvance<-reactive({
    if("autoadvance"%in%names(config)){config[config$period==min(max(1,state$period),periods()),]$autoadvance}else{FALSE}  
})

rate<-reactive({
    if("rate"%in%names(config)){config[config$period==min(max(1,state$period),periods()),]$rate}else{5}  
})

pause<-reactive({
    if("pause"%in%names(config)){config[config$period==min(max(1,state$period),periods()),]$pause}else{FALSE}  
})

practice<-reactive({
    if("practice"%in%names(config)){config[config$period==min(max(1,state$period),periods()),]$practice}else{FALSE}  
})



############################
### Timing (ticks, stages, rounds, periods)

#Rate at which timer updates
increment<-reactive(1/rate())
#Only one client can iterate time
timekeeper<-reactive({state$connected[1]})
# Assembles conditions under which game should be frozen
frozen<-reactive({if(length(unique(state$connected))<n() || state$paused==TRUE || state$phase=="waiting" || state$phase=="finished"){TRUE}else{FALSE} })


iterate<-function(key){

      #Iterate Tick
      if(key=="tick"){

          event("tick_end")
          state$tick<-state$tick+increment()

          if(state$tick>=duration()){
            iterate("stage")
          }

      }

      #Iterate Stage
      if(key=="stage"){

          event("stage_end")
          state$stage<-state$stage+1

          #Prepare next stage according timing structure
          if(duration()==0){
            state$ready[turn()==state$stage]<-FALSE
          }else{
            state$tick<-0  
          }

          #If last stage is finished, move to next round
          if(state$stage>stages()){   
            state$stage<-1
            state$ready<-rep(FALSE,n())
            iterate("round")
          }

      }

      #Iterate Round
      if(key=="round"){

          event("round_end")
          state$round<-state$round+1

          #If last round is finished, end period 
          if(state$round>rounds()){

            #Sstate$round<-1
            event("period_end")

            if(autoadvance()){ 
              
              #state$ready<<-rep(TRUE,n()); iterate("period")

              if(survey()!=""){
                state$survey<-rep(TRUE,n()); state$phase<-"survey"
              }else{
                state$ready<<-rep(TRUE,n()); iterate("period")
              }

            }else{
              state$ready<<-rep(FALSE,n()); state$phase<-"waiting"
            }

          }

      }

      # #Advance to Training Window
      if(key=="training"){
        
        if(deliver()){
          state$instructions<-rep(TRUE,n()); state$phase<-"instructions"
        }else{
          state$quiz<-rep(TRUE,n()); state$phase<-"quiz"
        }

      }

      #Iterate Period
      if(key=="period"){

          #ex_advance_period(); event("period")
          state$period<-state$period+1
          #state$round<-1

          ### If last period is over, finish
          if(state$period>periods()){
            
            event("experiment_end")
            state$phase<-"finished"

          }else{
            state$groups<-assign_groups(); state$paused<-pause()
            state$round<-1; state$stage<-1


            if(training()){

              iterate("training")

            }else{

              state$phase<-ifelse(prechoice(),"prechoice",ifelse(duration()>0,"timed","untimed"))
              state$ready<-rep(FALSE,n())
              event("period_start")

            }

          }

      }

}

# Handling Autostart
observe({

  if(state$phase=="start" && autostart()){

     if(training()){
       #iterate("training")
      iterate("period")
     }else{
      state$phase<-"waiting"
    }
    
  }

})




#######################
## User Connections

is.subject<-reactive({
  my$id!="admin" & my$id!="payment" & my$id!="error"
})

legal.client<-reactive({

      if("alias"%in%names(config)){
        c(parsebrackets(config[config$period==1,]$alias),"admin","payment")
      }else{
        c(1:n(),"admin","payment")
      }  

})

# Local, reactive variables for this specific subject
my <- reactiveValues(id = "", init=FALSE, group=NULL)

#Connect subject
observe({

  #Pull in query from url line
  id<-substring(session$clientData$url_search,2)
  
  isolate({

    if(sum(state$connected==id)>0 || sum(legal.client()==id)==0){
      my$id<-"error"
      return()
    }

    #Now update with the new one
    my$id <- id

    if(is.subject()){

      #Need to assign 1:N ID's based on position in id vector
      #my$id<-as.numeric(my$id)
      my$id<-which(id==legal.client())
      refresh$me[my$id]<-FALSE
      my$init <<- TRUE
      isolate(state$connected <- unique(c(state$connected, my$id)))

    }

  })

})

# Disconnect subject 
session$onSessionEnded(function() {

  isolate({
    state$connected <- state$connected[state$connected != my$id]
  })

})

# Subject refreshes
observe({

  refresh$me

  if(!is.subject())
    return()

  if(refresh$me[my$id]==TRUE){
    output$reset <- renderText({ "<script>window.location.reload();</script>" })
  }else{
    output$reset <- renderText({ "" })
  }

})

# TO DO:  Add Random String IDs as option

n<-reactive({config[config$period==min(max(1,state$period),periods()),]$n}) # Number of subjects for session


############################
### Events 

# Clicking the next period button
observeEvent(input$nextperiod,{ 

 if(input$nextperiod != 1)
   return()

   state$ready[my$id]<-TRUE 

   # If everyone has agreed to advance
   if(sum(state$ready)>=n()){

       #state$ready<<-rep(TRUE,n()); iterate("period")

      if(survey()!=""){
        state$survey<-rep(TRUE,n()); state$phase<-"survey"
      }else{
        state$ready<<-rep(TRUE,n()); iterate("period")
      }
     
   }

})

observeEvent(input$submit,{

  state$ready[my$id]<-TRUE

  #See if everyone whose turn it is has decided
  if(sum(state$ready[turn()==state$stage])>=sum(turn()==state$stage)){

    if(duration()>0){

      event("prechoice")
      state$phase<-"timed"
    
    }else{

      state$phase<-"untimed"
      iterate("stage")
      
    }

  }

})

#Timer
observe({

  state$period; state$ready; timekeeper()

  #Only one user iterates time 

  isolate({

    if(state$phase!="timed" || state$tick> duration()*stages()*rounds() || my$id!=timekeeper())
      return() 

  })

  invalidateLater(1000/rate(),NULL) 

  isolate({
    
    if(frozen()==TRUE)
      return()

    iterate("tick")
  
  })

})


############################
### Buttons For Advancing Game

# Generate Next Period Button
output$nextperiodbutton <- renderUI({

    #Useful for testing but not sure it belongs in final version...
    #if(state$period==0  & state$phase=="waiting" & autoadvance()){
    if(state$period==0  & state$phase=="waiting"){
        state$ready<<-rep(TRUE,n())
        iterate("period")
        return()
    }

    #if (my$init==TRUE & state$ready[my$id]==FALSE & state$phase=="waiting"  &  state$finished==FALSE) {
    if (my$init==TRUE & state$ready[my$id]==FALSE & state$phase=="waiting"  &  state$phase!="finished") {        
        #actionButton("nextperiod", if(state$period>0){"Ready for Next Period"}else{"Ready to Start Experiment"},icon("arrow-circle-right"))
        actionButton("nextperiod", "Ready for Next Period",icon("arrow-circle-right"))
    }
    
})

# Generate Submit Button
output$submitbutton <- renderUI({

    if (chose()==TRUE & turn()[my$id]==state$stage & (state$phase=="untimed"||state$phase=="prechoice") & state$ready[my$id]==FALSE  & my$init==TRUE &  state$phase!="finished" & frozen()==FALSE) {
        
        actionButton("submit", "Submit",icon("thumbs-up"))
    }
    
})

############################
### Grouping 

assign_groups<-function(){
  
    if("groups"%in%names(config)){
      x<-parsebrackets(config[config$period==max(1,state$period),]$groups)
    }else{
      x<-1
    }
    
    #If assigned in config
    if(length(x)>1){
      return(x)
    #If not, then x is a group size and assignment is random
    }else{
      return(sample(rep(seq(1,n()/x,1),each=x)))
    }  

}

group<-reactive({state$groups[my$id]})
counterparts<-reactive({which(state$groups==state$groups[my$id])[which(state$groups==state$groups[my$id])!=my$id]}) # A list of subject numbers of members of your group
ingroup<-reactive({which(state$groups==state$groups[my$id])}) # A list of subject numbers of members of your group

# Print data to csv
save<-function(){

  write.table(state$data,file=paste('data-', Sys.Date(), '.csv', sep=''),sep=",",row.names=FALSE)

}

############################
### Payment

exchange<-reactive({
    temp<-if("exchange"%in%names(config)){parsebrackets(config[config$period==max(1,state$period),]$exchange)}else{1} 
    if(length(temp)>1){
      return(temp)
    }else{
      return(rep(temp,n()))
    }
})

# showup<-reactive({
#     if(state$period!=1)
#       return()

#     if("showup"%in%names(config)){parsebrackets(config[config$period==max(1,state$period),]$showup)}else{""} 
# })


update_pay<-function(key="period"){

  if(practice()==TRUE)
    return()

  if(key=="period"){
    state$pay<<-rbind(state$pay,
      cbind( Period=rep(state$period,n()),Subject=seq(1,n()),Earnings=state$earnings,Exchange=exchange())
      )
  }

  if(key=="round"){
    state$pay<<-rbind(state$pay,
      cbind(Period=rep(state$period,n()),Round=rep(state$round,n()),Subject=seq(1,n(),1),Earnings=state$earnings,Exchange=exchange())
    )   
  }

  if(key=="stage"){
    state$pay<<-rbind(state$pay,
      cbind(Period=rep(state$period,n()),Round=rep(state$round,n()),Stage=rep(state$stage,n()),Subject=seq(1,n(),1),Earnings=state$earnings,Exchange=exchange())
      )   
  }

}

#Probably can make this a bit better -- hard to use this before the end of the full game.  Maybe this gets called when "End Experiment" is pressed?

finalize_pay<-function(method="all",unit="cash",param=100){

  if(unit=="lottery"){
    #cat(state$pay$Earnings)
    state$pay$Payment<-rbinom(length(state$pay$Earnings),1,state$pay$Earnings/param)*state$pay$Exchange
  }else{
    state$pay$Payment<-state$pay$Earnings*state$pay$Exchange
  }

  

  if(method=="all"){
    state$pay$Cumulative<-0
    for(i in 1:n()){
      state$pay[state$pay$Subject==i,]$Cumulative<-paste("$",round(cumsum(state$pay[state$pay$Subject==i,]$Payment),2))
    }
    state$pay$Payment<-paste("$",round(state$pay$Payment,2))
  }
  
  if(method=="random"){
    state$pay$Paid<-""
    for(i in 1:n()){
      selected<-sample(1:length(state$pay[state$pay$Subject==i,]$Paid),1)
      state$pay[state$pay$Subject==i,]$Paid[selected]<-"X"
    }
    state$pay$Payment<-paste("$",round(state$pay$Payment,2))

  }

  if(method=="none"){
    state$pay$Payment<-paste("$",round(state$pay$Payment,2))
  }


}

#Show payoff table at end of experiment
observe({

   if(state$phase=="finished" & is.subject()){
     output$mypaytab <- renderDataTable(state$pay[state$pay$Period!=0 & state$pay$Subject==my$id,], options = list(searching=FALSE,paging = FALSE))
   }

})




#######################
### TRAINING TOOLS (Instructions, Quizzes)

instructions<-reactive({

    if("instructions"%in%names(config)){config[config$period==min(max(1,state$period),periods()),]$instructions}else{""} 
})

quiz<-reactive({

#Q# 
    #if("quiz"%in%names(config)){config[config$period==min(max(1,state$period+1),periods()),]$quiz}else{""}  
  if("quiz"%in%names(config)){config[config$period==min(max(1,state$period),periods()),]$quiz}else{""}  
})

deliver<-reactive({

#Q#    
    #if("deliver"%in%names(config)){config[config$period==min(max(1,state$period+1),periods()),]$deliver}else{FALSE}  
    if("deliver"%in%names(config)){config[config$period==min(max(1,state$period),periods()),]$deliver}else{FALSE}
})

training<-reactive({deliver()||quiz()!=""})


#Create Link to Toggle Instructions
output$instructionslink <- renderUI({

  if(instructions()==""  || !is.subject())
    return()
     
  actionLink("showinstructions", "|||")
    
})

#Click Link to Toggle Instructions
observeEvent(input$showinstructions,{

  if(instructions()=="")
    return()

  state$instructions[my$id]<-if(state$instructions[my$id]){FALSE}else{TRUE}

})

#Show Instructions
output$instructionsviewer <- renderText({
    
    if(instructions()=="" || !is.subject() || state$instructions[my$id]==FALSE)
      return()
    
    return(paste('<iframe style="height:700px; width:100%" src="', instructions(), '"></iframe>', sep = ""))   

})


#Generate the Quiz
output$quiz<-renderUI({

  #if(my$id=="admin" || my$id=="payment" || state$phase!="quiz" || state$quiz[my$id]==FALSE || quiz()=="")
  if(!is.subject() || state$phase!="quiz" || quiz()=="")
    return()

  output$quiz_correct<-renderText({""});  output$quiz_incorrect<-renderText({""})

  #quiz<-read.table("quiz.csv",fill=TRUE,header=TRUE,sep=",")
  quiz<-read.table(as.character(quiz()),fill=TRUE,header=TRUE,sep=",")

  question_list<-lapply(1:max(quiz$number),function(i){
    
    q_name<-paste("q",i,sep="")
    answers<-parsebrackets(quiz[quiz$number==i,]$answers,type="character")

    selectInput(q_name,paste(i,quiz[quiz$number==i,]$question,sep=". "),choices=c("",answers),selected = NULL,width="800px")

  })

  #do.call(tagList,question_list)
  do.call(tagList,
    list(
      uiOutput("quiz_label",style="color:gray"),br(),br(),
      question_list,
      actionButton("quiz_answer", "Submit Answers",icon("arrow-circle-down")),
      br(),br(), uiOutput("quiz_correct",style="color:green;"),uiOutput("quiz_incorrect",style="color:red;")
    )
  )
  
})

output$quiz_label<-renderText({"PLEASE ANSWER EACH OF THE FOLLOWING QUESTIONS:"})

# Submitting Quiz Answers, Checking Answers
observeEvent(input$quiz_answer,{ 

  #quiz<-read.table("quiz.csv",fill=TRUE,header=TRUE,sep=",")
  quiz<-read.table(as.character(quiz()),fill=TRUE,header=TRUE,sep=",")
  
  wrong<-vector(); hint<-vector()
  #"Grade" the Quiz
  for(i in 1: max(quiz$number)){

    q_name<-paste("q",i,sep="")
    answers<-parsebrackets(quiz[quiz$number==i,]$answers,type="character")

    #if(input[[q_name]]!=answers[quiz[quiz$number==i,]$correct]){wrong<-c(wrong,i)}
    if(input[[q_name]]!=answers[quiz[quiz$number==i,]$correct]){
      
      wrong<-c(wrong,i)

      if("hint"%in%names(quiz)){ 
        hint<-c(hint,paste("Hint For Q",i,": ",as.character(quiz[quiz$number==i,]$hint),sep=""))
      }

    }

  }
  
  event("quiz_submit")

  if(length(wrong)==0){
    output$quiz_correct<-renderText({"You Passed!"});  output$quiz_incorrect<-renderText({""})
    state$quiz[my$id]<-FALSE
  }else{
    #output$quiz_correct<-renderText({""}); output$quiz_incorrect<-renderText({paste0("Please correct your answers to questions: ",paste(wrong, collapse=", "))})
    output$quiz_correct<-renderText({""}); 
    output$quiz_incorrect<-renderUI({
      HTML(
        paste0(
          "Please correct your answers to questions: ", 
          paste(wrong, collapse=", "),
          #paste("<br/><br/>","Hints:"),
          paste("<br/><br/>",hint, collapse="")
        )
      )
    })

  }

  # When everyone is done, move forward
  if(sum(state$quiz)==0){

    event("quiz_end")

    output$quiz_correct<-renderText({""});  output$quiz_incorrect<-renderText({""})

    #Back to beginning of period stuff    
    state$phase<-ifelse(prechoice(),"prechoice",ifelse(duration()>0,"timed","untimed"))
    state$ready<-rep(FALSE,n())
    event("period_start")


  }

})




#####################
###  Survey Tool


survey<-reactive({
  if("survey"%in%names(config)){config[config$period==min(max(1,state$period),periods()),]$survey}else{""}  
})

#Generate the Quiz
output$survey<-renderUI({

  #if(my$id=="admin" || my$id=="payment" || state$phase!="quiz" || state$quiz[my$id]==FALSE || quiz()=="")
  if(!is.subject() || state$phase!="survey" || survey()=="")
    return()

  output$survey_complete<-renderText({""});  output$survey_incomplete<-renderText({""})
  survey<-read.table(as.character(survey()),fill=TRUE,header=TRUE,sep=",")

  question_list<-lapply(1:max(survey$number),function(i){

    if(survey[survey$number==i,]$type=="selectInput"){

      q_name<-paste("q",i,sep="")
      answers<-parsebrackets(survey[survey$number==i,]$answers,type="character")
      selectInput(q_name,paste(i,survey[survey$number==i,]$question,sep=". "),choices=c("",answers),selected = NULL,width="800px")
      
    }else{

      if(survey[survey$number==i,]$type=="textInput"){
        q_name<-paste("q",i,sep="")   

          list(
            tags$label(paste(i,survey[survey$number==i,]$question,sep=". ")),
            tags$textarea(id = q_name,name="whatever", placeholder = 'Type here', rows = 6, "")
          )
        
      }      
    
    }

  })

  do.call(tagList,
    list(
      uiOutput("survey_label",style="color:gray"),br(),br(),
      tags$style(type="text/css", "textarea {width:100%}"),
      question_list,
      actionButton("survey_answer", "Submit Answers",icon("arrow-circle-down")),
      br(),br(), uiOutput("survey_complete",style="color:green;"),uiOutput("survey_incomplete",style="color:red;")
    )
  )
  
})




output$survey_label<-renderText({"PLEASE ANSWER EACH OF THE FOLLOWING QUESTIONS:"})

# # Submitting Quiz Answers, Checking Answers
observeEvent(input$survey_answer,{ 

  #quiz<-read.table("quiz.csv",fill=TRUE,header=TRUE,sep=",")
  survey<-read.table(as.character(survey()),fill=TRUE,header=TRUE,sep=",")


  
  unanswered<-vector(); 
  #"Grade" the Quiz
  for(i in 1: max(survey$number)){

    q_name<-paste("q",i,sep="")

    state$survey.answer[my$id]<-paste(state$survey.answer[my$id],input[[q_name]],sep="||")

    if(input[[q_name]]==""){      
      unanswered<-c(unanswered,i)
    }


  }

  
  event("survey_submit")

  #cat(state$survey.answer[my$id])

  if(length(unanswered)==0){
    output$survey_complete<-renderText({"Thank you!  Please wait for others to finish"});  output$survey_incomplete<-renderText({""})
    state$survey[my$id]<-FALSE
  }else{

    #output$quiz_correct<-renderText({""}); output$quiz_incorrect<-renderText({paste0("Please correct your answers to questions: ",paste(wrong, collapse=", "))})
    output$survey_complete<-renderText({""}); 
    output$survey_incomplete<-renderUI({
      HTML(
        paste0(
          "Please answer the following questions: ", 
          paste(unanswered, collapse=", ")
        )
      )
    })

  }


  # When everyone is done, move forward
  if(sum(state$survey)==0){

    event("survey_end")
    output$survey_complete<-renderText({""});  output$survey_incomplete<-renderText({""})
    state$ready<<-rep(TRUE,n()); iterate("period")


  }

})
