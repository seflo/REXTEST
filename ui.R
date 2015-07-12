
library(shiny)
library(stringr)
library(ggvis)

#shinyUI(
  fluidPage(

###############################
### Admin Page
###############################

     # uiOutput("adminPage"),
     # uiOutput("paymentPage"),
  fluidRow(
    fluidRow(
      column(4,offset=4,htmlOutput("errorlabel",align="center")),
      column(4,offset=4,htmlOutput("adminlabel1",align="center"))
    ),
    fluidRow(
      column(6,offset=3,div(dataTableOutput("statustab"),style="font-size:90%"))
    ),
    br(), #br(),
    #Admin's Config Upload
    fluidRow(
      column(2,offset=1,htmlOutput("pausebutton",align="center")),
      column(2,offset=0,htmlOutput("downloadbutton",align="center")),
      column(2,offset=0,htmlOutput("periodbutton",align="center")),
      column(2,offset=0,htmlOutput("restartbutton",align="center")),
      column(2,offset=0,htmlOutput("experimentbutton",align="center"))
      #column(2,offset=0,tags$button( "End Experiment", id="experimentbutton", type="button", class="btn action-button", onclick="return confirm('Are you sure?');" )),
    ),
    fluidRow(column(10,offset=1,htmlOutput("adminrule"))),
    fluidRow(
      column(3,offset=1,htmlOutput("uploadconfig",align="center")),
      column(2,offset=1,htmlOutput("adminlabel2",align="center")),
      column(2,offset=2,htmlOutput("debugbutton",align="center"))
    ),
    #Admin's Config Table
    fluidRow(
      column(9,offset=1,div(dataTableOutput("configtab"),style="font-size:80%"))
    )
  ),

  fluidRow(
    fluidRow(
      column(2,offset=5,htmlOutput("paymentlabel",align="center"))
    ),
    fluidRow(
      column(6,offset=3,dataTableOutput("allpaytab"))
    )
  ),

###############################
### Training Functions
###############################

    fluidRow(
      column(1,offset=11,uiOutput("instructionslink"))
    ),

    htmlOutput('instructionsviewer'), htmlOutput("reset"),

    fluidRow(
     column(6,offset=3,br(),br(),uiOutput("quiz"))
    ),
    
    fluidRow(
     column(6,offset=3,br(),br(),uiOutput("survey"))
    ),    

###############################
### Subject Page
###############################
    #uiOutput("subjectPage"),

    fluidRow(

      fluidRow(
         column(12,offset=0,textOutput("information"))
      ),
      fluidRow(
        column(12,offset=0,htmlOutput("Announce"))
      ),
      fluidRow(
        column(3,offset=5,uiOutput("Earn",style="color:red;"))
      ),
      br(),
      #Payfeedback
      fluidRow(
        column(6,offset=3,dataTableOutput("mypaytab"))
      ),

      fluidRow(
       column(3,offset=1,
         plotOutput('tickets',height="375px")
       ),
        column(2,offset=0,
          #plotOutput('plot',clickId="plotclick",hoverId="plothover",hoverDelay=0)
          #fluidRow(plotOutput('plot',clickId="plotclick",hoverId="plothover",hoverDelay=0,height="350px")),
          #fluidRow(plotOutput('plot',click="plotclick",height="350px")),
          fluidRow(
           uiOutput("ggvis_ui"),
           ggvisOutput("decision_plot")
           ),
          fluidRow(
           column(5,offset=2,uiOutput("consumption",style="color:red;font-size:9px")),
           column(5,offset=0,uiOutput("allocation",style="color:green;font-size:9px"))
           )
        ),
        column(6,offset=0,
         fluidRow(plotOutput('wealth',height="425px"))
          
        )
      ),
      fluidRow(
        column(1,offset=4, 
          htmlOutput("nextperiodbutton",style="margin-top:100px;"),
          htmlOutput("submitbutton",style="margin-top:100px;")
        ),
        column(6,offset=1,
          plotOutput('task',click="taskclick",height="275px")
          )

      ),
       fluidRow(
        #htmlOutput("refreshbutton")
         #column(6,offset=3,dataTableOutput("thedata"))
       )

    ),
tags$style(type="text/css", ".recalculating { opacity: 1.0; }"),



###########################################
###########################################


    # tags$script('
    #   $(document).keypress(function(e) {

    #     if (e.which == 13){
    #       jQuery("#nextperiod").click();
    #       jQuery("#submit").click();
    #     };

    #     if (e.which == 32){
    #       var val = Math.random()
    #       Shiny.onInputChange("spacepress", val);
    #     };
         
    #   });
 
      
    # '),

    #Some CSS Styling for Label Colors
    tags$head(
      tags$style("#information{white-space: pre-wrap; color:gray; text-align:center;} #Announce{color:gray; text-align:center;} #adminlabel1{color:gray} #adminlabel2{color:gray} #paymentlabel{color:gray}" )
      #tags$style(type="text/css", ".form-control{ font-size:12px; }")
      )
  )
  

  
# tags$script('
#   Shiny.addCustomMessageHandler("browserrefresh",
#       function(x){alert("here")});
# '),


  #)




#   fluidPage(

#   #Import the admin/payment pages
#   #source("rex_admin_ui.R",local=TRUE)

#   #br(),
#   ##########################
#   ## INSTRUCTIONS

  # fluidRow(
  #   column(1,offset=11,uiOutput("instructionslink"))
  # ),

  # htmlOutput('instructionsviewer'),

#   ##########################
#   ## PAYMENT PAGE


#   ##########################
#   ## ADMIN PAGE



  

  ##########################
  ## SUBJECT PAGE



  # fluidRow(
  #    column(12,offset=0,textOutput("information"))
  # ),
  # fluidRow(
  #   column(12,offset=0,htmlOutput("Announce"))
  # ),
  # #Payfeedback
  # fluidRow(
  #   column(6,offset=3,dataTableOutput("mypaytab"))
  # ),


  #Subject's Next Period Button
  # fluidRow(
  #   column(1,offset=5,
  #     fluidRow(
  #       column(3,offset=0,htmlOutput("nextperiodbutton"))
  #     )
  #   )
  # ),

  # #Subject's Submit Button
  # fluidRow(
  #   column(1,offset=5,
  #     fluidRow(
  #       column(3,offset=6,htmlOutput("submitbutton"))

  #     )
  #   )
  # ),









   #Basic Subjects' Plot
  #  fluidRow(
  #    column(8,offset=2,
  #      plotOutput('plot',clickId="plotclick",hoverId="plothover",hoverDelay=0)
  #    )
  #  ),
  #  fluidRow(
  #   column(6,offset=4, 
  #     uiOutput("slider_action")
  #   )
  # ),



######


 # fluidRow(
 #  column(3,offset=1,
 #    plotOutput('tickets',height="375px")
 #  ),
 #   column(2,offset=0,
 #     #plotOutput('plot',clickId="plotclick",hoverId="plothover",hoverDelay=0)
 #     fluidRow(plotOutput('plot',clickId="plotclick",hoverId="plothover",hoverDelay=0,height="375px")),
 #     fluidRow(
 #      column(5,offset=2,uiOutput("consumption",style="color:red;font-size:9px")),
 #      column(5,offset=0,uiOutput("allocation",style="color:green;font-size:9px"))
 #      )
 #   ),
 #   column(6,offset=0,
 #    fluidRow(plotOutput('wealth',height="375px"))
 #    ,fluidRow(plotOutput('task',height="275px"))
     
 #   )
 # ),






 # fluidRow(
 #  column(12,plotOutput("task",height="200px"))
 # ),
 # fluidRow(
 #    uiOutput("consumption")
 #  ),


  #Bubbles Subjects' Plot
   # fluidRow(
   #   column(8,offset=2,
   #     plotOutput('plot',clickId="plotclick",hoverId="plothover",hoverDelay=0)
   #   )
   # ),


  #Matrix Subjects' Plot
  # fluidRow(
  #  column(4,offset=1,
  #    plotOutput('matrix',clickId="plotclick",hoverId="plothover",hoverDelay=0)
  #  ),
  #  column(6,offset=0,
  #    plotOutput('timeseries')
  #  )
  #  ,fluidRow(
  #    column(8,offset=2,dataTableOutput("temptab"))
  #  )
  # ),


  #Ring Game
#   fluidRow(
#   #br(),
#    column(2,offset=2,
#      plotOutput('matrix4',clickId="plotclick",hoverId="plothover",hoverDelay=0,height="150px",width="150px")
#    ),
#    column(2,offset=0,
#      plotOutput('matrix1',clickId="plotclick1",hoverId="plothover1",hoverDelay=0,height="150px",width="150px")
#      #textOutput("label1")
#    ),
#    column(2,offset=0,
#      plotOutput('matrix2',clickId="plotclick2",hoverId="plothover2",hoverDelay=0,height="150px",width="150px")
#    ),
#    column(2,offset=0,
#      plotOutput('matrix3',clickId="plotclick3",hoverId="plothover3",hoverDelay=0,height="150px",width="150px")
#    )
#   ),
#   br(),br(),

#   fluidRow(
#     column(2,offset=2,
#       #htmlOutput("black")
#       htmlOutput("title4")
#     ),
#     column(2,offset=0,
#       htmlOutput("title1")
#       #div(style = "color: red;", "RED PLAYER")
#     ),
#     column(2,offset=0,
#       htmlOutput("title2")
#     ),
#     column(2,offset=0,
#       htmlOutput("title3")
#     ) 
#   ),    

# br(),

#   fluidRow(
#     column(2,offset=2,
#       #htmlOutput("black")
#       htmlOutput("label4")
#     ),
#     column(2,offset=0,
#       htmlOutput("label1")
#       #div(style = "color: red;", "RED PLAYER")
#     ),
#     column(2,offset=0,
#       htmlOutput("label2")
#     ),
#     column(2,offset=0,
#       htmlOutput("label3")
#     ) 
#   ),
#   br(),br(),      
  # br(),


##########################
## GENERIC BUTTONS

  # fluidRow(
  #   column(1,offset=5,
  #     fluidRow(
  #       column(3,offset=0,htmlOutput("mainbutton"))
  #     )
  #   )
  # ),

######
#  QUIZ - EXPERIMENTAL






  #uiOutput("instructions"),
  #tags$iframe(src="Instructions.pdf", width="900", height="600"),

  # fluidRow(
  #   column(6,offset=3,dataTableOutput("thedata"))
  # ),


  #Selection Plot


#Keypress events

# fluidRow(
#   column(6,offset=3,dataTableOutput("thedata"))
# ),

# tags$script('
#   $(document).keypress(function(e) {

#     if (e.which == 13){
#       jQuery("#nextperiod").click();
#       jQuery("#submit").click();
#     };

#     if (e.which == 32){
#       var val = Math.random()
#       Shiny.onInputChange("spacepress", val);
#     };
     
#   });
# '),

#Some javascript for key events
# tags$script('
#   $(document).on("keypress", function (e) {
#      Shiny.onInputChange("keyevent", e.which);
#   });
# '),

# Arrow keys...
# tags$script('
#     $(document).keydown(function(e) {
#         switch(e.which) {
#             case 37: // left
#             Shiny.onInputChange("spacepress", Math.random());
#             break;

#             case 39: // right
#             Shiny.onInputChange("spacepress", Math.random());
#             break;

#             default: return; // exit this handler for other keys
#         }
#         e.preventDefault(); // prevent the default action (scroll / move caret)
#     });
# '),





#input, select, textarea {font-size: 80%;}

 #)




