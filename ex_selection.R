  

##############################################
##############################################
#### GAME LOGIC
##############################################
##############################################


  chose<-reactive({

    if(task()=="confidence"){

      #if(state$period==0)return(FALSE)
      return(state$guess[my$id]!=0)

    }else{

      return(TRUE)

    }
    
  })

  gameoff<-reactive({
    state$phase=="start" || state$phase=="finished" || !is.subject() || state$paused || state$phase=="instructions" || state$phase=="quiz" || state$phase=="survey"
  })

  winner<-reactive({

    if(task()=="confidence"){
      #rep(1-(state$guess[my$id]==1)*1,rounds())
      rep(1-(state$guess[my$id]==1)*1,length(parsebrackets(config[config$period==max(state$period,1),]$states)))
    }else{
      parsebrackets(config[config$period==max(state$period,1),]$states)
    }
    
  })

  r<-reactive({
    parsebrackets(config[config$period==max(state$period,1),]$r)
  })

  w<-reactive({
    parsebrackets(config[config$period==max(state$period,1),]$w)
  })

  delta<-reactive({
    parsebrackets(config[config$period==max(state$period,1),]$delta)
  })

  maxt<-reactive({
    parsebrackets(config[config$period==max(state$period,1),]$maxt)
  })

  L<-reactive({
    parsebrackets(config[config$period==max(state$period,1),]$L)
  })

#Event Handler
event<-function(key){

    if(key=="prechoice"){

        state$w<-rep(w(),n())
        for(p in 1:length(winner())){

          ifelse(task()=="confidence",
            winner<-1-(state$guess==1)*1,
            winner<-rep(winner()[p],n())
          )
          
          state$data<-rbind(state$data,cbind.data.frame(
              Period=rep(state$period,n()),
              Subject=1:n(),
              Round=rep(p,n()),
              Group=state$groups,
              w=state$w,
              c=state$c,
              a1=state$a1,
              a2=state$a2,
              utility=sapply(state$w*state$c,FUN=function(x)utility(x)),
              winner=winner,
              quiz=state$quiz_tries,
              profit=rep(0,n()),
              task=rep(as.character(task()),n())
              #task=rep(0,n())
            )
          )

          state$w<-((state$a1*(winner==0)+state$a2*(winner==1))*(1-state$c)*state$w)*(r())

        }

        #Calculate subject's earnings ahead of time
        for(i in 1:n()){

          ifelse(task()=="confidence",
            winner<-rep(1-(state$guess[i]==1)*1,length(winner())),
            winner<-winner()
          )

          #temp<-state$data[state$data$Period==state$period & state$data$Subject==my$id & state$data$Round==1,]
          temp<-state$data[state$data$Period==state$period & state$data$Subject==i & state$data$Round==1,]
          #correct<-(winner()==0)*temp$a1 + (winner()==1)*temp$a2
          correct<-(winner==0)*temp$a1 + (winner==1)*temp$a2

          z<-(delta()/(1-delta()))*sum( 
                #sapply(correct,FUN=function(x)log(x))* sapply(1:length(winner()),FUN=function(x)delta()^x)
                sapply(correct,FUN=function(x)log(x))* sapply(1:length(winner),FUN=function(x)delta()^(x-1))
              )  
            

          state$earnings[i]<-max(
            0,
            #(L()/maxt())*(z+(log(temp$c*w())/(1-delta()))+(delta()/((1-delta())^2))*log((1-temp$c)*r()))
            (z+(log(temp$c*w())/(1-delta()))+(delta()/((1-delta())^2))*log((1-temp$c)*r()))
          )
          
          state$data[state$data$Period==state$period & state$data$Subject==i,]$profit<-state$earnings[i]
          #L/maxt*[ln(c*w1)/(1-delta)+delta/(1-delta)^2*ln((1-c)*r)+Z]
          
        }

        state$data<-state$data[!(state$data$Period==state$period &  state$data$Round>rounds()+1),]
        state$w<-rep(w(),n())
        state$quiz_tries<-rep(0,n())
      #record()
      #state$earnings<-state$w*state$c
      update_pay()
      

    }###

    if(key=="tick_end"){

    }###

    if(key=="stage_end"){
      
    }###

    if(key=="round_end"){

      if(state$round<rounds()){

        if(task()=="monty"){
          state$rand<-c(sample(c("gray","gray","green","green","blue"),replace=FALSE),"white")##
          state$ui_win<-ifelse(winner()[state$round+1]==0,sample(which(state$rand=="green"),1),which(state$rand=="blue"))
        }

        if(task()=="flip"){
          state$rand<-c(rep("green",task_detail()),rep("blue",4-task_detail()),"white","white")
          #state$ui_win<-ifelse(winner()[state$round+1]==0,sample(1:task_detail(),1),sample((task_detail()+1):4,1))
          state$ui_win<-ifelse(winner()[state$round+1]==0,sample(rep(1:task_detail(),2),1),sample(rep((task_detail()+1):4,2),1))
          #cat(sample((task_detail()+1):4,1))
        }        

      }
      
    }###

    if(key=="period_end"){
      
      state$totalearnings<<-state$totalearnings+state$earnings;
      
    }###

    if(key=="period_start"){

      #cat("here")

      state$earnings<-state$earnings+rep(0,n()); 
      state$c<-rep(0,n()); state$a1<-rep(0.5,n()); state$a2<-rep(0.5,n())  
      state$w<-rep(w(),n())

      if(task()=="monty"){
        state$rand<-c(sample(c("gray","gray","green","green","blue"),replace=FALSE),"white")##
        state$ui_win<-ifelse(winner()[1]==0,sample(which(state$rand=="green"),1),which(state$rand=="blue"))
      }

      if(task()=="flip"){

        state$rand<-c(rep("green",task_detail()),rep("blue",4-task_detail()),"white","white")
        #state$ui_win<-ifelse(winner()[state$round]==0,sample(1:task_detail(),1),sample((task_detail()+1):4,1))
        state$ui_win<-ifelse(winner()[state$round]==0,sample(rep(1:task_detail(),2),1),sample(rep((task_detail()+1):4,2),1))

      }

      if(task()=="confidence"){
        state$guess<-rep(0,n())
      }

      if(task()=="momentum"){
        temp<-readLines("randomwalk.csv")
        state$randomwalk<-as.numeric(as.character(temp))
      }
  


    }

    if(key=="experiment_end"){

      finalize_pay(method="all",unit="lottery",param=72)
      
    }

    if(key=="quiz_submit"){
      state$quiz_tries[my$id]<-state$quiz_tries[my$id]+1
    }

    if(key=="quiz_end"){
      
    }

    if(key=="survey_end"){


      state$data<-rbind(state$data,cbind.data.frame(
          Period=rep(state$period,n()),
          Subject=1:n(),
          Round=rep(-1,n()),
          Group=state$groups,
          w=rep(-1,n()),
          c=rep(-1,n()),
          a1=rep(-1,n()),
          a2=rep(-1,n()),
          utility=rep(-1,n()),
          winner=rep(-1,n()),
          quiz=rep(-1,n()),
          profit=rep(-1,n()),
          task=state$survey.answer
        )
      )


    }

}


  utility<-function(x){

    if(log(x)==-Inf){
      -100
    }else{
      log(x)
    }
      
  }


  # Save Data
  record<-function(key="choice"){

    inturn<-which(turn()==state$stage)
    state$data<<-rbind(state$data,cbind(
      Period=rep(state$period,length(inturn)),
      Subject=inturn,
      Round=rep(state$round,length(inturn)),
      Group=state$groups[inturn],
      Stage=rep(state$stage,length(inturn)),
      w=state$w[inturn],
      c=state$c[inturn],
      a1=state$a1[inturn],
      a2=state$a2[inturn]
      )
    )

  }


##############################################
##############################################
#### SUBJECT DECISION MAKING
##############################################
##############################################

# observe({

#   if(state$phase!="prechoice")
#     return()

#   updateTextInput(session,"input1",value=round(state$c[my$id]*100,digits=1))
# })

output$consumption<-renderUI({

  #if(gameoff()|| (state$ready[my$id]==TRUE && duration()==0) || frozen()==TRUE || turn()[my$id]!=state$stage || (state$phase!="timed" && state$phase!="untimed" && state$phase!="prechoice"))
  if(gameoff()|| (state$ready[my$id]==TRUE) || frozen()==TRUE || turn()[my$id]!=state$stage || (state$phase!="prechoice"))
    return()

  #textInput("input1",value = round(state$c[my$id]*100,digits=1),label="% Withdrawn")
  #textInput("input1",value = 0,digits=1),label="% Withdrawn")
  numericInput("input1",value=0,label="% Withdrawn",min=0,max=100,step=1)

})

observeEvent(input$input1,{

  if(state$phase!="prechoice" ||  is.na(as.numeric(input$input1)))
    return()


    state$c[my$id]<-max(0,min(as.numeric(input$input1),100))/100
    my$change<-runif(1)
})

#Update when action changes
# observe({

#   if(state$phase!="prechoice")
#     return()

#   updateTextInput(session,"input2",value=round(state$a1[my$id]*100,digits=1))
# })

#Show Slider
output$allocation<-renderUI({

  #if(gameoff()|| (state$ready[my$id]==TRUE && duration()==0) || frozen()==TRUE || turn()[my$id]!=state$stage || (state$phase!="timed" && state$phase!="untimed" && state$phase!="prechoice"))
  if(gameoff()|| (state$ready[my$id]==TRUE) || frozen()==TRUE || turn()[my$id]!=state$stage || (state$phase!="prechoice"))
    return()

  #textInput("input2", value = round(state$a1[my$id]*100,digits=1),label="% In Green")
  #textInput("input2", value = 50,digits=1),label="% In Green")
  numericInput("input2",value=50,label="% In Green",min=0,max=100,step=1)
})

observeEvent(input$input2,{

    if(state$phase!="prechoice" || is.na(as.numeric(input$input2)))
      return()

    state$a1[my$id]<-max(0,min(as.numeric(input$input2),100))/100
    state$a2[my$id]<-1-state$a1[my$id]
    my$change<-runif(1)
})


#Register your action with plot click
# observeEvent(input$plotclick, {

#   if(frozen()==TRUE|| is.null(input$plotclick)  || (state$ready[my$id]==TRUE && duration()==0) ||  !is.subject() ||  turn()[my$id]!=state$stage || state$phase!="prechoice")
#     return()

#   y<-max(min(input$plotclick$y,1),0)

#   if(input$plotclick$x<0.5){
    
#     state$c[my$id]<-1-y
    
#   }else{

#     if(y<(1-state$c[my$id])){
#       state$a1[my$id]<-y/(1-state$c[my$id])
#       state$a2[my$id]<-1-state$a1[my$id]
#     }
#   }

#   my$change<-runif(1)

# }) 


##############################################
##############################################
#### GENERATE THE UI
##############################################
##############################################


## Create UI
# output$subjectPage<- renderUI({

#   fluidRow(

#     fluidRow(
#        column(12,offset=0,textOutput("information"))
#     ),
#     fluidRow(
#       column(12,offset=0,htmlOutput("Announce"))
#     ),
#     fluidRow(
#       column(3,offset=5,uiOutput("Earn",style="color:red;"))
#     ),
#     br(),
#     #Payfeedback
#     fluidRow(
#       column(6,offset=3,dataTableOutput("mypaytab"))
#     ),

#     fluidRow(
#      column(3,offset=1,
#        plotOutput('tickets',height="375px")
#      ),
#       column(2,offset=0,
#         #plotOutput('plot',clickId="plotclick",hoverId="plothover",hoverDelay=0)
#         #fluidRow(plotOutput('plot',clickId="plotclick",hoverId="plothover",hoverDelay=0,height="350px")),
#         fluidRow(plotOutput('plot',click="plotclick",height="350px")),
#         fluidRow(
#          column(5,offset=2,uiOutput("consumption",style="color:red;font-size:9px")),
#          column(5,offset=0,uiOutput("allocation",style="color:green;font-size:9px"))
#          )
#       ),
#       column(6,offset=0,
#        fluidRow(plotOutput('wealth',height="425px"))
        
#       )
#     ),
#     fluidRow(
#       column(1,offset=4, 
#         htmlOutput("nextperiodbutton",style="margin-top:100px;"),
#         htmlOutput("submitbutton",style="margin-top:100px;")
#       ),
#       column(6,offset=1,
#         plotOutput('task',click="taskclick",height="275px")
#         )

#     ),
#      fluidRow(
#       htmlOutput("refreshbutton")
#        #column(6,offset=3,dataTableOutput("thedata"))
#      )

#   )

# })







  # output$refreshbutton <- renderUI({

  #         actionButton("refresh", "Refresh",icon("thumbs-up"))
      
  # })

  # observeEvent(input$refresh,{ 

  #   state$refresh
  #   output$reset <- renderText({ "<script>window.location.reload();</script>" })

  # })


########################
## Create Decision Plot


# reactive({

#   data.frame(
#     vals=c(state$a1[my$id]*(1-state$c[my$id]),state$a2[my$id]*(1-state$c[my$id]),state$c[my$id]),
#     group=c(1,2,3),
#     labels=c(paste("Bet on Green: ",state$a1[my$id]*100),paste("Bet on Blue: ",state$a2[my$id]*100),paste("Withdraw: ",state$c[my$id]*100)),
#     g=c(1,1,1),
#     col=c("green","blue","red"),
#     t.heights=c(0.5*(1-state$c[my$id])*state$a1[my$id],
#            (1-state$c[my$id])*state$a1[my$id]+0.5*(1-state$c[my$id])*state$a2[my$id],
#            (1-state$c[my$id])*state$a1[my$id]+(1-state$c[my$id])*state$a2[my$id]+0.5*state$c[my$id]),
#     t.widths=c(1.6,1.6,-0.5)     
#   )%>%
#     group_by(group)%>%
#     ggvis(x=~g,y=~vals,fill=~group)%>%
#     add_axis("x", ticks=0,title = "",grid=FALSE)%>%
#     add_axis("y", ticks=0,title = "",grid=FALSE)%>%
#     layer_bars(fill:=~col)%>%
#     set_options(width=200,height=300,resizable=FALSE)%>%
#     hide_legend("fill")%>%
#     scale_numeric("x", domain = c(-0.5, 2.5), nice = FALSE)%>%
#     layer_text(x=~t.widths,y=~t.heights,text:=~labels,fill:=~col,fontSize := 10)

# })%>%
#      bind_shiny("ggvis", "ggvis_ui")


  decision<-reactive({

    my$change;state$period

    isolate({

      data.frame(
        vals=c(state$a1[my$id]*(1-state$c[my$id]),state$a2[my$id]*(1-state$c[my$id]),state$c[my$id]),
        group=c(1,2,3),
        labels=c(paste("Invest in Green: ",state$a1[my$id]*100,"%"),paste("Invest in Blue: ",state$a2[my$id]*100,"%"),paste("Withdraw: ",state$c[my$id]*100,"%")),
        g=c(1,1,1),
        col=c("green","blue","red"),
        t.heights=c(0.5*(1-state$c[my$id])*state$a1[my$id],
               (1-state$c[my$id])*state$a1[my$id]+0.5*(1-state$c[my$id])*state$a2[my$id],
               (1-state$c[my$id])*state$a1[my$id]+(1-state$c[my$id])*state$a2[my$id]+0.5*state$c[my$id]),
        t.widths=c(1.6,1.6,-1.1)
      )

    })
  })



observe({

  my$change

  if(gameoff())
    return()

  decision%>%
    group_by(group)%>%
    ggvis(x=~g,y=~vals,fill=~group)%>%
    hide_axis("x")%>%hide_axis("y")%>%
    layer_bars(fill:=~col)%>%
    set_options(width=220,height=300,resizable=FALSE)%>%
    hide_legend("fill")%>%
    scale_numeric("x", domain = c(-1, 4), nice = FALSE)%>%
    layer_text(x=~t.widths,y=~t.heights,text:=~labels,fill:=~col,fontSize := 8)%>%
     bind_shiny("decision_plot", "ggvis_ui")


})




# output$plot <- renderPlot({

#   #state$stage; state$round #state$c[my$id]; #state$a1[my$id]; 
#   my$change

#   if(gameoff())
#     return()

#   isolate({

#     if(state$w[my$id]>0){

#       # Make blank plot
#       par(mar=c(0,0,4,0))
#       plot(0,0,type="n",xlim=c(-1.2,2.2),ylim=c(0,1),xlab="",ylab="Francs this Round",frame=FALSE,xaxt="n",yaxt="n",main="Decision Bar")

#       rect(0.2,(1-state$c[my$id]),0.9,1,col="red")
#       segments(0,0,0,1,col="gray",lwd=1.5)
#       segments(0,(1-state$c[my$id]),0,1,col="red",lwd=2)
#       points(0,(1-state$c[my$id]),pch="-",cex=2,col="red")

#       rect(0.2,0,0.9,(1-state$c[my$id]),col="blue")
#       rect(0.2,0,0.9,state$a1[my$id]*(1-state$c[my$id]),col="green")

#       segments(1,0,1,(1-state$c[my$id]),col="blue",lwd=2)
#       segments(1,0,1,state$a1[my$id]*(1-state$c[my$id]),col="green",lwd=2)
#       points(1,state$a1[my$id]*(1-state$c[my$id]),pch="-",cex=2,col="black")
#       text(1.05,0.5*state$a1[my$id]*(1-state$c[my$id]),labels=paste(round(state$a1[my$id]*100,2),"% in Green"),cex=0.75,col="darkgreen",pos=4)
#       text(1.05,state$a1[my$id]*(1-state$c[my$id])+0.5*(1-state$a1[my$id]),labels=paste(round(state$a2[my$id]*100,2),"% in Blue"),cex=0.75,col="blue",pos=4)
#       text(0,(1-state$c[my$id])+0.5*state$c[my$id],labels=paste("Withdraw ",round(state$c[my$id]*100,2),"%"),cex=0.75,col="red",pos=2)


#     }else{

#       # Make blank plot
#       par(mar=c(3,4,4,2))
#       plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",frame=FALSE,xaxt="n",yaxt="n")
#       text(0.5,0.5,labels="You Are BANKRUPT",col="red",cex=0.75)        

#     }

#   })

# })



mydata<-reactive({


  state$period; state$phase

  isolate({

    temp<-state$data[state$data$Subject==my$id & state$data$Period==max(state$period,1),]

    temp$bottom<-(temp$winner==0)*temp$w*(1-temp$c)*temp$a1 + (temp$winner==1)*temp$w*(1-temp$c)*temp$a2
    temp$middle<-temp$bottom+(temp$winner==1)*temp$w*(1-temp$c)*temp$a1 + (temp$winner==0)*temp$w*(1-temp$c)*temp$a2
    temp$top<-temp$w
    
    temp$color<-ifelse(temp$winner==0,"green","blue"); temp$utility<-round(temp$utility,2)
    temp$x1<-temp$Round-0.1; temp$x2<-temp$Round+0.1
    
    #temp$cumutility<-cumsum(temp$utility)

    return(temp)

  })
  
})



history<-reactive({mydata()[1:state$round-1,]})
current<-reactive({mydata()[1:state$round,]})


observe({

  state$period

  isolate({
    my$ylimit<-2*w()
  })

})


ylimit<-reactive({

  if(state$round==1){
    w()*2
  }else{
    
    #min(c(20,seq(0,10000,w()*2))[c(20,seq(0,10000,w()*2))>current()[state$round,]$w])
     max(
       min(c(20,seq(0,100000,w()*2))[c(20,seq(0,100000,w()*2))>current()[state$round,]$w]),
       current()[max(1,state$round-10):state$round,]$w
     )


  }

})


xlimit<-reactive({

  c(max(0,state$round-10),max(10,state$round+1))

})


output$wealth<-renderPlot({

  state$round; #my$change
  #state$c[my$id]; state$a1[my$id]; state$round

  if(gameoff())
    return()

  isolate({


      plot(current()$w~current()$Round,xlim=c(xlimit()[1],xlimit()[2]),pch="-",type="b",ylim=c(0,ylimit()),cex=4,xlab="Round",ylab="Francs",main="")


      
      if(state$round>1 || (state$phase=="waiting" & state$period>0)){
        
        rect(history()$x1,0,history()$x2,history()$bottom,col=history()$color,border=NA)
        rect(history()$x1,history()$middle,history()$x2,history()$top,col="red",border=NA)

         text(history()$w~history()$Round,pos=3,label=history()$utility,cex=0.9,col="red")
         
         if(state$round>1){
           arrows((state$round-1),current()[state$round-1,]$bottom,(state$round),current()[state$round,]$w,col=current()[state$round-1,]$color,angle=25,length=0.1,lwd=0.65)
           text(state$round-1,history()[state$round-1,]$bottom,label=paste(r(),"x"),col=current()[state$round-1,]$color,pos=4,cex=0.75)

         }
        
      }
      

  })

})








  ########################
  ## Create Tickets Plot


  now<-reactive({

    if(state$round>1){
      current()[state$round-1,]
    }else{
      data.frame(w=state$w[my$id],c=1,a1=0.5,a2=0.5)
    }

  })

  output$tickets<-renderPlot({

    my$change; state$round

    #if(gameoff() || state$round>1)

    if(gameoff())
      return()

    isolate({

      # if(gameoff())
      #   return()

      plot(log,0.01,now()$w,col="red",main="",ylab="Tickets",xlab="Withdraw")

      abline("h"=0)
      points(now()$w*state$c[my$id],utility(now()$w*state$c[my$id]),pch=20,col="red",cex=1.5)
      text(now()$w*state$c[my$id],utility(now()$w*state$c[my$id]),labels=round(utility(now()$w*state$c[my$id]),2),pos=4,col="red",cex=0.75)

    })

  })





  output$Earn<-renderUI({

    state$phase; 
    if(my$init!=TRUE || gameoff() || state$phase!="waiting"||state$period==0)return()

      HTML(paste("<br/>","You Earned ",round(state$earnings[my$id],2)," Tickets This Period."),"<br/>")

  })


  ### Print information
  output$information<-renderText({

    state$updater; state$round; 
    if(my$init!=TRUE || gameoff())return()

    paste(
      ifelse(practice(),paste("Practice Period:",state$period),paste("Period:",state$period)),
      paste("Round:",state$round),
      #paste("Tickets:",if(state$round>1){round(mydata()$cumutility[state$round-1],2)}else{0}),
      #paste("Tickets:",if(state$phase=="waiting"){round(state$earnings[my$id],1)}else{"-"}),
      paste("ID:",my$id),
      #paste("Phase:",state$phase),
      #paste("Total:",round(state$totalearnings[my$id]+state$earnings[my$id],1)),
      #if(duration()>0){paste("Tick:",round(state$tick,1))},
      sep="             ")


  })

  #Announcements
  observe({

    if(!is.subject())
      return()

    str<-""

    if(state$phase=="start"){str<-"PLEASE WAIT FOR THE EXPERIMENT TO BEGIN"}
    if(state$phase=="finished"){str<-"THE EXPERIMENT IS FINISHED"}
    if(state$paused==TRUE){str<-"THE EXPERIMENT IS PAUSED"}
    if(state$phase=="untimed" & turn()[my$id]==state$stage & state$ready[my$id]==FALSE){str<-"PLEASE MAKE YOUR CHOICE"}
    if(state$phase=="untimed" & (turn()[my$id]!=state$stage | state$ready[my$id]==TRUE)){str<-"PLEASE WAIT FOR OTHERS TO CHOOSE"}
    if(state$phase=="waiting" & state$ready[my$id]==FALSE){str<-"PLEASE CLICK WHEN READY TO START NEXT PERIOD"}
    if(state$phase=="waiting" & state$ready[my$id]==TRUE){str<-"PLEASE WAIT UNTIL OTHERS ARE READY"}
    if(state$phase=="timed" & turn()[my$id]==state$stage){str<-""}
    if(state$phase=="timed" & turn()[my$id]!=state$stage){str<-"PLEASE WAIT FOR OTHERS TO CHOOSE"}
    if(state$phase=="prechoice" & turn()[my$id]==state$stage){str<-"PLEASE MAKE YOUR CHOICES"}

    output$Announce <- renderUI({
        HTML(paste("<br/>",  str, "<br/>"))
    })

  })



   observe({

  #   z <- data.frame(x=c(1,2),y=c(3,4))

        output$thedata <- renderDataTable(state$pay, options = list(searching=FALSE,paging = FALSE))

   })















##############################################
##############################################
#### TASKS AND ANIMATIONS
##############################################
##############################################


task<-reactive({
  #parsebrackets(config[config$period==max(state$period,1),]$task,"character")
  parsebrackets(config[config$period==min(periods(),max(state$period,1)),]$task,"character")

})

task_detail<-reactive({
    parsebrackets(config[config$period==max(state$period,1),]$task_detail)
  })

  task_iterator<-reactive({
  
    if(state$tick< 0.25*duration()){
      return(1)
    }

    if(state$tick>=0.25*duration() & state$tick< 0.5*duration()){
      return(2)
    }

    if(state$tick>=0.5*duration() & state$tick< 0.75*duration()){
      return(3)
    }

    if(state$tick>=0.75*duration()){
      return(4)
    }

  })


  output$task<-renderPlot({


    #state$c[my$id]; state$round; task_iterator(); state$guess[my$id]

    if(gameoff())
      return()

    task_iterator(); state$guess[my$id]; 
    #state$round; 

    isolate({


        #########################
        # Training Task
        if(task()=="flip" ){

          if(task_iterator()<3){
            cups(list(0,
                state$rand,
                c(0,0,0,0,0,0),
                ifelse(state$phase=="prechoice","Is the coin under a GREEN or BLUE cup?",""),
                "black"
              ))
          }

          if(task_iterator()>=3){
            cups(list(state$ui_win,
                state$rand,
                c(0.5,0.5,0.5,0.5,0.5,0.5),
                ifelse(winner()[state$round]==0,"Coin is under GREEN:  GREEN wins this round!","Coin is under BLUE:  BLUE wins this round!"),
                ifelse(winner()[state$round]==0,"darkgreen","blue")
              ))
          }


        }


        ##########################
        # Monty Hall Task
        if(task()=="monty"){

          #win<-ifelse(winner()[state$round]==0,sample(which(state$rand=="green"),1),which(state$rand=="blue"))
          #orders(sample("gray","gray"))

          if(task_iterator()==1){

            cups(list(0,
                sapply(state$rand,FUN=function(x)ifelse(x=="green"||x=="white",x,"gray")),
                c(0,0,0,0,0,0),
                "Two cups randomly colored GREEN.",
                "black"
              ))
          }

          if(task_iterator()==2){
            cups(list(0,
                sapply(state$rand,FUN=function(x)ifelse(x=="green"||x=="white",x,"gray")),
                sapply(state$rand,FUN=function(x)ifelse(x=="gray",0.5,0)),
                "Show two of remaining cups WITHOUT coin.",
                "black"
              ))
          }

          if(task_iterator()==3||state$phase=="prechoice"){
            cups(list(0,
                state$rand,
                sapply(state$rand,FUN=function(x)ifelse(x=="gray",0.5,0)),
                #c(0,0.5,0,0.5,0),
                ifelse(state$phase=="prechoice","Is the coin under a GREEN or BLUE cup?","Last remaining cup is colored BLUE."),
                "black"
              ))
          }

          if(task_iterator()==4){
            cups(list(state$ui_win,
                state$rand,
                c(0.5,0.5,0.5,0.5,0.5,0),
                ifelse(winner()[state$round]==0,"Coin is under GREEN:  GREEN wins this round!","Coin is under BLUE:  BLUE wins this round!"),
                ifelse(winner()[state$round]==0,"darkgreen","blue")
              ))
          }

        }

        ##########################
        # Rational Expectations Task
        if(task()=="expectations"){

          #title<-ifelse()
          
          plot(0,0,type="n",xlim=c(-1,1.5),ylim=c(0,1.3),xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",
            col.main=ifelse(state$phase=="prechoice","black",ifelse(winner()[state$round]==0,"darkgreen","blue")),
            main=
            ifelse(state$phase=="prechoice",
              "GREEN wins if B chose Right, BLUE if B chose Left",
              ifelse(task_iterator()>3,
              ifelse(
                winner()[state$round]==0,
                "Player B Chose Right: GREEN Wins",
                "Player B Chose Left:  BLUE Wins"
              ),
              ""
              )
            )
          )

          rect(0,0,0.5,0.5,col=ifelse(winner()[state$round]==1 & task_iterator()>3,"lightgray","white"))#DownLeft
          rect(0.5,0,1,0.5,col=ifelse(winner()[state$round]==0 & task_iterator()>3,"lightgray","white"))#DownRight
          rect(0.5,0.5,1,1,col=ifelse(winner()[state$round]==0 & task_iterator()>3,"lightgray","white"))#UpRight
          rect(0,0.5,0.5,1,col=ifelse(winner()[state$round]==1 & task_iterator()>3,"lightgray","white"))#UpLeft
          text(-0.2,0.75,labels="Blue",col="blue",font=2)
          text(-0.2,0.25,labels="Green",col="darkgreen",font=2)
          text(0.25,1.1,labels="Left")
          text(0.75,1.1,labels="Right")

          text(-0.4,0.5, "Player A", font=2, srt = 90)
          text(0.5,1.3, "Player B", font=2)

          #DownLeft
          text(0.25,0.25,labels=40,col="darkgreen",pos=2,font=2)
          text(0.25,0.25,labels=0,col="black",pos=4)

          text(0.75,0.25,labels=20,col="darkgreen",pos=2,font=2)
          text(0.75,0.25,labels=20,col="black",pos=4)

          text(0.25,0.75,labels=45,col="blue",pos=2,font=2)
          text(0.25,0.75,labels=45,col="black",pos=4)

          text(0.75,0.75,labels=0,col="blue",pos=2,font=2)
          text(0.75,0.75,labels=40,col="black",pos=4)

        }


        ##########################
        # Confidence Task
        if(task()=="confidence"){

          plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",
            col.main=ifelse(state$phase=="prechoice","black",ifelse(winner()[state$round]==0,"darkgreen","blue")),
            main=
            ifelse(state$phase=="prechoice",
              "GREEN wins if you answer correctly, BLUE if you answer incorrectly",
              ifelse(task_iterator()>3||state$round>1,
                ifelse(
                  winner()[state$round]==0,
                  "Your Choice was CORRECT: GREEN Wins",
                  "Your Choice was INCORRECT:  BLUE Wins"
                ),
                ""
              )
            )
            )

            text(0.1,0.9,labels="You invested $1000 in a stock two years ago.  The stock's trading price \n declined 40% the first year and rose 40% the next.  As a result, you've",pos=4)

            points(0.35,0.5,cex=2)
            text(0.35,0.5,labels="lost money",adj=-0.15)
            points(0.35,0.3,cex=2)
            text(0.35,0.3,labels="broke even",adj=-0.15)
            # points(0.35,0.1,cex=2)
            # text(0.35,0.1,labels="broken even",adj=-0.15)

              if(state$guess[my$id]==1){
                points(0.35,0.5,cex=2,pch=20)
              }

              if(state$guess[my$id]==2){
                points(0.35,0.3,cex=2,pch=20)
              }
            
              # if(state$guess[my$id]==3){
              #   points(0.35,0.1,cex=2,pch=20)
              # }


        }


        ##########################
        # Cursedness Task
        if(task()=="curse"){

          # 1  B selected, 2 chose left
          # 2  A chosen but 2 chose left
          # 3  B chosen and 2 chose right
          # 4  A chosen and 2 chose right *
          
          plot(0,0,type="n",xlim=c(-5,4.25),ylim=c(0,5),xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",
            col.main=ifelse(state$phase=="prechoice","black",ifelse(winner()[state$round]==0,"darkgreen","blue")),
            main=
            ifelse(state$phase=="prechoice",
              " BLUE wins if BETTING earns Player 1 a higher Payoff; GREEN wins otherwise,",
            ifelse(task_iterator()>3,
              ifelse(
                winner()[state$round]==0,
                "BETTING Would NOT Earn Player 1 a Higher Payoff: GREEN Wins",
                "BETTING Would Earn Player 1 a Higher Payoff:  BLUE Wins"
              ),
              ""
              )
              )
            )
          
          #axis(side=1,at=-1,labels="(Player 1 Knows Only that the State is Either A or B)")

          rect(0,2,2,3,col="lightgray") #Your Info Set
          rect(2,2,4,3) 
          rect(0,1,1,2) 
          rect(1,1,3,2) #Other Info Set
          rect(3,1,4,2)

          # if(state$phase!="prechoice" & task_iterator()>3){

          #   if(task_detail()[state$round]==1){
          #     text(-4,5.5,bquote(symbol("\326")),cex=1.5)
          #   }
             
          #   if(task_detail()[state$round]!=1){
          #     text(2,5.5,bquote(symbol("\326")),cex=1.5)
          #   }             

          # }


          if(state$phase!="prechoice" & task_iterator()>3){

            if(task_detail()[state$round]==2 || task_detail()[state$round]==3){
              text(-4,0,"2 Didn't BET",cex=1)
            }else{
              text(2,0,"2 Did BET",cex=1)
            }           

          }


          if(state$phase!="prechoice" & task_iterator()>3){

            if(task_detail()[state$round]==3 || task_detail()[state$round]==4){
              points(0.5,3.5,cex=3)
            }else{
              points(1.5,3.5,cex=3)
            }
                         

          }



          text(-5.25,4.25,labels="Either DOESN'T BET: \n Both Earn 36",col="darkgreen",pos=4)

          text(2,4.5,labels="Both BET:\n Payoff Depends on A, B, C, D:",col="blue")
          text(c(0.5,1.5,2.5,3.5),c(3.5,3.5,3.5,3.5),labels=c("A","B","C","D"),font=2)
          text(c(0.5,1.5,2.5,3.5),c(2.5,2.5,2.5,2.5),labels=c(67,7,55,19),col=c("blue","blue","black","black"))
          text(c(0.5,1.5,2.5,3.5),c(1.5,1.5,1.5,1.5),labels=c(3,63,15,51))

          text(c(0,0),c(2.5,1.5),labels=c("Player 1 \nPayoff Is","Player 2\n Payoff Is"),pos=2,col=c("blue","black"))


        }


        ##########################
        # Iterated Dominance Task
        if(task()=="dominance"){


          plot(0,0,type="n",xlim=c(0,4),ylim=c(0,4),yaxt="n",xaxt="n",frame=FALSE,xlab="",ylab="",
            col.main=ifelse(state$phase=="prechoice","black",ifelse(winner()[state$round]==0,"darkgreen","blue")),
            main=
            ifelse(state$phase=="prechoice",
              "GREEN wins if 300 is Closer to 0.7 TIMES B's Guess, BLUE if 350 is Closer",
            ifelse(task_iterator()>2,
              ifelse(
                winner()[state$round]==0,
                paste("300 is Closer to 0.7 TIMES B's Guess Of ", task_detail()[state$round], ": GREEN Wins"),
                  paste("350 is Closer to 0.7 TIMES B's Guess Of ", task_detail()[state$round], ": BLUE Wins")
              ),
              ""
              )
              )

            )
          
          text(0,4,labels="Person A:  Guess Between 300-500:  Higher payoff the closer guess is to 0.7 times B's Guess",pos=4)
          text(0,3.5,labels="Person B:  Guess Between 100-900:  Higher payoff the closer guess is to 0.5 times A's Guess",pos=4)
          text(0,2.5,labels="Would Person A Get a Higher Payoff By:",pos=4)
          text(0.5,2,labels="- Guessing 300",pos=4,col="darkgreen")
          text(0.5,1.5,labels="- Guessing 350",pos=4,col="blue")

        }


        ##########################
        # Momentum Task
        if(task()=="momentum"){

          plot(state$randomwalk,type="l",col="red",xaxt="n",xlab="",ylab="Price",
            col.main=ifelse(state$phase=="prechoice","black",ifelse(winner()[state$round]==0,"darkgreen","blue")),
            main=
            ifelse(state$phase=="prechoice",
              "GREEN wins if price goes DOWN, BLUE if price goes UP",
            ifelse(task_iterator()>3,
              ifelse(
                winner()[state$round]==0,
                "Price Went DOWN: GREEN Wins",
                "Price Went UP:  BLUE Wins"
              ),
              ""
              )
              )

            )

          if(task_iterator()>=3){
            
            if(winner()[state$round]==1){
              lines(c(length(state$randomwalk),length(state$randomwalk)+1),
                c(state$randomwalk[length(state$randomwalk)],state$randomwalk[length(state$randomwalk)]+1),
                col="blue",lwd=3)
            }else{
              lines(c(length(state$randomwalk),length(state$randomwalk)+1),
                c(state$randomwalk[length(state$randomwalk)],state$randomwalk[length(state$randomwalk)]-1),
                col="green",lwd=3)
            }

          }

        }


    })

  })



  #Task interaction
  observeEvent(input$taskclick, {

    if(frozen()==TRUE|| is.null(input$taskclick)  || (state$ready[my$id]==TRUE && duration()==0) ||  !is.subject() ||  turn()[my$id]!=state$stage || state$phase!="prechoice")
      return()

    if(task()=="confidence"){

      if(input$taskclick$x<0.3||input$taskclick$x>0.4||input$taskclick$y>0.6||input$taskclick$y<0)
        return()
      

      if(input$taskclick$y>0.4){
          state$guess[my$id]<-1
          
        }else{

          if(input$taskclick$y>0.2){
            state$guess[my$id]<-2
          }
          
        }

        #state$data[state$data$Subject==my$id & state$data$Period==max(state$period,1),]$winner<-rep(1-(state$guess[my$id]==1)*1,length(parsebrackets(config[config$period==max(state$period,1),]$states)))

    }


  }) 


  cups<-function(x){
      win=x[[1]]
      cols=x[[2]]
      shift=x[[3]]

    plot(0,0,type="n",xlim=c(0,6),ylim=c(0,1.25),xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",col.main=x[[5]],main=x[[4]])
    for(i in 1:6){
      o<-i-1+0.1
      
      if(i==win){
        points(i-0.4,0.15,cex=8,col="gold",pch=20)
        text(i-0.4,0.15,cex=2,labels="$")   
      }

      polygon(
        #bottomleft,bottomright,topright,topleft
        x=c(o+0.05,o+1-0.05,o+1-0.25,o+0.25),
        y=c(shift[i]+0,shift[i]+0,shift[i]+1,shift[i]+1),
        col=cols[i],
        border=NA
      )
      abline("h"=0)
    }
  }



