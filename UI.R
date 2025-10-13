
library(shiny)
library(r2d3)
library(shinyjs)
library(speechcollectr)

ui <- tagList(
  useShinyjs(),
  useRecorder(),
  
  
  tags$head(tags$style(HTML('
  /* hide these tabs before any JS runs  */
  #topnav li a[data-value="Talk"],
  #topnav li a[data-value="Highlight"],
  #topnav li a[data-value="Summary"] {
    display: none;   
  }
  '))),
  
  navbarPage(
    title = "Perception of Statistical Graphics",
    id = "topnav",
    selected = "Consent",  
  
    #Consent tab
    tabPanel("Informed Consent", value = "Consent",
             fluidPage(
               h3("Consent"),
               p("By continuing, you confirm you are 18+ and agree to take part in this study."),
               checkboxInput("consent_ok", "I agree", FALSE),
               actionButton("consent_continue", "Continue")
             )
    ),
    
    # Demographics tab
    tabPanel("Demographics", value = "Demographics",
             fluidPage(
               h3("Demographics"),
               selectInput("demo_exp", "Age Range:",
                           c("Under 19","19-25","26-30","31-35","Prefer not to say")),
               selectInput("education", "Highest Educational Level:",
                           c("Highschool or Less","Undergraduate Degree","Graduate Degree","Prefer not to say")),
               actionButton("demo_continue", "Continue")
             )
    ),
    
    #Lineup options tab
    tabPanel("Lineup Study", value = "Lineup",
             fluidPage(
               h3("Which plot is the most different?"),
               br(), br(), br(),
               p("Instructions........",  style = "font-size:18px; font-weight:bold;"),
               br(), br(), br(),
               fluidRow(
                 column(4, actionButton("choose_talk", "Talk aloud", class = "btn btn-primary", width = "100%")),
                 column(4, actionButton("choose_highlight", "Highlight", class = "btn btn-primary", width = "100%")),
                 column(4, actionButton("choose_summary", "Text summary", class = "btn btn-primary", width = "100%"))
               )
             )
    ),
    
    #Talk aloud tab
    tabPanel("Talk Aloud", value = "Talk",
             fluidPage(
               h3("Talk aloud"),
               fluidRow(
                 column(3,
                        p("Click Start recording. After recording starts, the lineup will appear.",  style = "font-size:18px;"),
                        actionButton("rec_start", "Start recording", class = "btn btn-primary",width = "100%"),
                        br(), br(),
                        div(style = "font-size:17px;",textOutput("rec_status")),
                        br(), br(),
                        shinyjs::hidden(actionButton("rec_stop", "Stop & save", class = "btn btn-primary", width = "100%")),
                      
          
                        br(),br(),
                        shinyjs::hidden(actionButton("goto_highlight", "Go to Highlight", class = "btn btn-primary", width = "100%")),
                        br(), br() #actionButton("talk_back", "Back to Lineup options", width = "100%")
                 ),
                 column(9,
                        div(id = "talk_lineup_wrap",
                            d3Output("lineup_talk", height = "700px")
                        )
                 )
               )
             )
    ),
    
    #Highlight tab
    tabPanel("Highlight", value = "Highlight",
             fluidPage(
               h3("Highlight"),
               fluidRow(
                 column(3,
                        tags$ol(
                          style = "font-size:16px;",
                          tags$li("Select plot(s) that look different (you can select upto 2 plots)"),
                          br(),
                          tags$li(HTML('Click <span style="color:gold; font-weight:bold;">Highlight</span> button above the plot and mark the key features')),
                          br(),
                          tags$li("Save & Next.")
                          ),
                        br(),
              
                        actionButton("save_highlight", "Save & Next", class = "btn btn-primary", width = "100%"),
                        br(), br(),
                    
                 ),
                 column(9, d3Output("lineup_highlight", height = "700px"))
               )
             )
    ),
    

    #Summary tab
    tabPanel(
      "Summary", value = "Summary",
      fluidPage(
        fluidRow(
          # left
          column(
            width = 3,
            h3("Text summary"),
            p("Which plot(s) look different from the others? If you have more than one selection, select 2 plots and briefly explain why.", style = "font-size:16px;"),
            div(
              style = "border:1px solid #e5e5e5; border-radius:8px; padding:12px; margin-bottom:14px;",
              tags$label("Selection 1"),
              selectInput("sum_sel1", NULL, choices = c("-", as.character(1:20)), selected = "-", width = "100%"),
              textAreaInput(
                "sum_why1", "Why?",
                placeholder = "Briefly explain what made this plot different…",
                rows = 5, width = "100%"
              )
            ),br(), br(),
            div(
              style = "border:1px solid #e5e5e5; border-radius:8px; padding:12px; margin-bottom:14px;",
              tags$label("Selection 2"),
              selectInput("sum_sel2", NULL, choices = c("-", as.character(1:20)), selected = "-", width = "100%"),
              textAreaInput(
                "sum_why2", "Why?",
                placeholder = "Briefly explain what made this plot different…",
                rows = 5, width = "100%"
              )
            ),
            actionButton("submit_summary", "Submit Responses", class = "btn btn-primary", width = "100%"),
            br(), br()
            #actionButton("summary_back", "Back to Lineup options", width = "100%")
          ),
          
          # right: lineup preview
          column(
            width = 9,
            d3Output("lineup_summary", height = "700px")
          )
        )
      )
    ),
    
    # Done
    tabPanel("Done", value = "Done", fluidPage(h3("Thank you!")))
  )
)
