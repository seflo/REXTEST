


library(shiny)
library(stringr)


#Function for parsing bracket notation in config file 
parsebrackets<-function(x,type="numeric"){
  
  x<-gsub("\\]","",gsub("\\[","",x))

  if(type=="character"){
      return(as.character(unlist(strsplit(x, split=","))))
  }else{
      return(as.numeric(as.character(unlist(strsplit(x, split=",")))))
  }

  #return(as.numeric(as.character(unlist(strsplit(x, split=",")))))

}


###################
### Selection Code

config<-read.table("config selection test.csv",fill=TRUE,header=TRUE,sep=",")


rex_build<<-function(){

# Set default reactive values
  state <<- reactiveValues(
    connected=NULL, 
    tick=0, 
    updater=0,
    round=1,
    period=0,
    stage=1,
    paused=FALSE, 
    groups=1:config$n[1],
    earnings=rep(0,config$n[1]), 
    totalearnings=rep(0,config$n[1]), 
    profit=rep(0,config$n[1]),
    ready=rep(FALSE,config$n[1]),
    choosing=rep(FALSE,config$n[1]), 
    chose=rep(FALSE,config$n[1]),
    action=rep(NA,config$n[1]),
    phase="start",
    instructions=rep(FALSE,config$n[1])
  )

  state$pay <<- data.frame(
    Period=numeric(), 
    Subject=numeric(),
    Earnings=character()
  )

  #Selection
  state$data<<-data.frame(
    Period=numeric(),
    Subject=numeric(),
    Round=numeric(),
    Group=numeric(),
    w=numeric(),
    c=numeric(),
    a1=numeric(),
    a2=numeric(),
    utility=numeric(),
    winner=numeric(),
    quiz=numeric(),
    profit=numeric(),
    task=numeric()
  )

  state$w<<-rep(12.5,config$n[1])
  state$c<<-rep(0,config$n[1])
  state$a1<<-rep(0.5,config$n[1])
  state$a2<<-rep(0.5,config$n[1])
  state$guess<<-rep(0,config$n[1])
  state$quiz_tries<<-rep(0,config$n[1])
  state$quiz.answer<-rep("",config$n[1])
  state$survey.answer<-rep("",config$n[1])


  #state$survey<<-rep(TRUE,config$n[1])

}





# Import a default config file from the local folder
#config<-read.table("config selection alt.csv",fill=TRUE,header=TRUE,sep=",")
#config<-read.table("config selection alt 2.csv",fill=TRUE,header=TRUE,sep=",")
#config<-read.table("config pricelist.csv",fill=TRUE,header=TRUE,sep=",")
#config<-read.table("config survival.csv",fill=TRUE,header=TRUE,sep=",")
#config<-read.table("config matrix.csv",fill=TRUE,header=TRUE,sep=",")
#config<-read.table("config matrix multi.csv",fill=TRUE,header=TRUE,sep=",")
#config<-read.table("config bubbles.csv",fill=TRUE,header=TRUE,sep=",")
#config<-read.table("config ringgame.csv",fill=TRUE,header=TRUE,sep=",")
#config<-read.table("config ringgame history.csv",fill=TRUE,header=TRUE,sep=",")
#config<-read.table("config basic.csv",fill=TRUE,header=TRUE,sep=",")


# Function that builds the experiment




#initiate<-function(){
#Change variable names (ready-->advance, chose-->ready)?
# state <- reactiveValues(
#   connected=NULL, tick=0, updater=0,round=1,period=0,stage=1,paused=FALSE, groups=1:config$n[1],
#   earnings=rep(0,config$n[1]), totalearnings=rep(0,config$n[1]), profit=rep(0,config$n[1]),
#   ready=rep(FALSE,config$n[1]),choosing=rep(FALSE,config$n[1]), chose=rep(FALSE,config$n[1]),
#   action=rep(NA,config$n[1]),phase="start",instructions=rep(FALSE,config$n[1])
# )

# state$pay <- data.frame(Period=numeric(), Subject=numeric(),Earnings=character())



# #Selection
#   state$data<-data.frame(
#     Period=numeric(),Subject=numeric(),Round=numeric(),Group=numeric(),w=numeric(),c=numeric(),a1=numeric(),a2=numeric(),utility=numeric(),winner=numeric()
#   )

#   state$w<-rep(12.5,config$n[1])
#   state$c<-rep(0,config$n[1])
#   state$a1<-rep(0.5,config$n[1])
#   state$a2<-rep(0.5,config$n[1])




# # A dataframe for earnings
# if("pay_protocol"%in%names(config)==FALSE||config$pay_protocol=="all"){ 
#   #Pay all
#   state$pay <- data.frame(Period=numeric(), Subject=numeric(),Earnings=character(),Cumulative=character())

# }else{
  
#   if("pay_unit"%in%names(config)==FALSE||config$pay_unit=="period"){ 
#     #Pay one period
#     state$pay <- data.frame(Period=numeric(), Subject=numeric(),Earnings=character())
#   }else{
#     #Pay one round
#     state$pay <- data.frame(Period=numeric(), Round=numeric(), Subject=numeric(),Earnings=character())
#   }

# }


#Continuous Games
# state$data<-data.frame(
#   Period=numeric(),Subject=numeric(),Tick=numeric(),Round=numeric(),Group=numeric(),Action=numeric()
# )

#Basic
 # state$data<-data.frame(
 #   Period=numeric(),Subject=numeric(),Round=numeric(),Group=numeric(),Turn=numeric(),Action=numeric()
 # )






# #Survival
# state$data <- data.frame(
#   Period=numeric(),Subject=numeric(),Tick=numeric(),Round=numeric(),Cash=numeric(),Action=numeric(),Profit=numeric(),Bankrupt=numeric()
# )
# state$cash<-rep(40,config$n[1])
# state$bankrupt<-rep(FALSE,config$n[1])

# Ring Game
# state$data <- data.frame(
#   Period=numeric(),Subject=numeric(),Tick=numeric(),Round=numeric(),Group=numeric(),Action=numeric(),Role=numeric(),Earnings=numeric()
# )

# choice<-reactiveValues(s1=c(numeric(),numeric(),numeric(),numeric()))
# #choice<-reactiveValues(s1=c(1,2,3,4))
# for(i in 2:config$n[1]){
#   choice[[paste("s",i,sep="")]]<-c(numeric(),numeric(),numeric(),numeric())
# }

# state$diagnostic <- data.frame(
#   Period=numeric(),Subject=numeric(),Tick=numeric(),Round=numeric(),Group=numeric(),Action=numeric(),Role=numeric()
# )

# Pricelist
# state$data <- data.frame(
#   Period=numeric(),Subject=numeric(),Tick=numeric(),Round=numeric(),Action=numeric()
# )

#}


