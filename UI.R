
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
  
   
    # Informed Consent tab (2025 General Consent Text)
    tabPanel("Informed Consent", value = "Consent",
             fluidPage(
               h3("Informed Consent Form"),
               tags$div(
                 style = "font-size:16px; line-height:1.6; max-height:500px; overflow-y:auto; padding:10px; border:1px solid #ccc; border-radius:8px;",
                 HTML("
      <p><strong>Study Title:</strong> Perception and Decision Making Using Statistical Graphs</p>

      <h4>Invitation:</h4>
      <p>This study is intended to assess how people perceive statistical graphs and charts, and how charts are used to make decisions. You may participate in this study if:</p>
      <ul>
        <li>You are above the age of consent in your jurisdiction (18+ in most states, 19+ in Nebraska and Alabama, 21+ in Mississippi)</li>
        <li>You have normal or corrected-to-normal vision</li>
        <li>(if relevant) Your computer has a working microphone and you are willing to have your voice recorded.</li>
      </ul>

      <h4>Purpose of the study</h4>
      <p>Statistical charts and graphs are everywhere – in news articles, advertisements, and on TV. We are interested in whether people read information from charts accurately, and whether certain types of charts are more useful when making data-informed decisions. Unfortunately, we know relatively little about how people read and perceive charts. This study is designed to address this gap in research by systematically investigating the use of charts in different tasks and contexts. </p>

      <h4>Procedure of the study </h4>
      <p>Participation in this study should require less than ___ minutes of your time. You will be asked to look at statistical charts and then answer questions or complete tasks based on the visualization and contextual information.<p>
      <p>You may be asked to estimate, predict, or make decisions based on one or more graphs. You will be able to provide an explanation of your response, if you choose to do so. </p>
      <p>(If relevant) ) We may ask you to talk out loud as you think through the task, and record this information using your computer’s microphone. These recordings will be saved to the server and transcribed, but will be anonymous and should not contain any personally identifying information. <p> 
      <p>We expect that each of the questions we ask will take less than 3 minutes to complete. We will start out with practice questions so that you can become accustomed to the interface. After the practice task(s), there will be a series of questions. At the end of the study, you will be asked for some demographic information, such as your age, education level, and occupation.<p>
      <p>Participation will take place in a location of your choosing, on your computer.</p>

      <h4>Possible risks of being in this research study</h4>
      <p>There are no known risks to you from participating in this study.</p>

      <h4>Possible benefits</h4>
      <p>You are not expected to receive any direct benefit from this study.</p>

      <h4>Compensation</h4>
      <p><strong>Prolific participants:</strong> We will pay you $XX for participating in this study through Prolific. At the conclusion of this study, you will be provided a URL that will direct you back to Prolific, signaling study completion. While you are free to quit at any time or to decline to answer any question, you will only be compensated if you complete the study and click on the redirect URL provided at the end of the study.</p>
      <p><strong>Reddit participants:</strong> You will not be paid for participation.</p
      <p>Reddit statement (study run through Prolific as well as Reddit): You will not be paid to take part in this study if you participate through Reddit. In recognition of the fact that not all individuals who want to contribute to science are comfortable providing identifying information to an outside service, we have designed this study with two different participation options. If you wish to be compensated for your participation, you may register with Prolific and locate the study on that platform. <p>


      <h4>Confidentiality</h4>
      <p>Reasonable steps will be taken to protect the privacy and the confidentiality of your study data; however, in some circumstances we cannot guarantee absolute privacy and/or confidentiality. This study will never link personally identifying information with your responses.</p>
      <p>If you are participating through an outside service, such as Prolific, your participation will be recorded and linked to your Prolific account so that you can receive payment. At no point will any identifiable information be linked with your data: any responses you provide are completely anonymous. </p>
      <p>Research records will be securely stored electronically through University approved methods and will only be seen by the research team and/or those authorized to view, access, or use the records during the study.<p>
      <p>Any audio recordings we collect will be stored securely. Recordings will be transcribed using a computer and checked for accuracy by one of the researchers conducting the study. We do not expect any personally identifying information will be present on these recordings. Anonymization techniques will be applied to disguise your voice before recordings are shared. Any personally identifying information will be manually redacted from the transcripts and from audio recordings.<p>
      <p>The researchers conducting this study attempt to make all collected data available to the public to ensure scientific reproducibility; however, in the case of recorded audio, only anonymized audio files and transcripts will be shared with the public.<p>
      <p>Those who will have access to your research records are the study personnel, the Institutional Review Board (IRB), and any other person, agency, or sponsor as required by law or contract or institutional responsibility. The information from this study may be published in scientific journals or presented at scientific meetings. The individual data records, plus summaries and group-level analyses, will be published, but your identity will be kept strictly confidential.<p>

      <h4>Your rights as a participant</h4>
      <p>You may ask any questions about this research before or during your participation.</p>
      <ul>
        <li>For study-related questions, contact: <strong>Susan Vanderplas (susan.vanderplas@unl.edu)</strong></li>
        <li>For questions about your rights or complaints about the research, contact the Institutional Review Board (IRB):<br>
            Phone: (402) 472-6965   Email: irb@unl.edu
        </li>
      </ul>

      <h4>What will happen if you decide not to be in this research study or decide to stop participating once you start? </h4>
      <p>You can decide not to be in this research study, or you can stop being in this research study (“withdraw’) at any time before, during, or after the research begins for any reason. Deciding not to be in this research study or deciding to withdraw will not affect your relationship with the investigator or with the University of Nebraska-Lincoln.</p>
      <p>You will not lose any benefits to which you are entitled. However, if you withdraw before you receive a redirect link, we cannot compensate you for participation in the study.</p>
      <p>If you wish to withdraw from the study after completion, we will attempt to remove your data. To facilitate this process, you will need to provide the time you started and ended the study as well as any details you can remember about your responses. If we cannot uniquely identify a set of responses from the information you provide,  we may not be able to delete your responses from the study because we have designed the study to ensure that we are not collecting identifiable information. <p>

      <h4>Documentation of Informed Consent</h4>
      <p>You are voluntarily making a decision whether or not to participate in this research study. By clicking on the I Agree button below, your consent to participate is implied. You should print a copy of this page for your records. </p>
      ")
               ),
               br(),
               checkboxInput("consent_ok", "I Agree", FALSE),
               actionButton("consent_continue", "Continue", class = "btn btn-primary")
             )
    ),
    
    
  
    
    #Lineup options tab
    tabPanel("Lineup Study", value = "Lineup",
             fluidPage(
               h3("Which plot is the most different?"),
               br(), br(), br(),
               p("Select an option below and follow the instructions.",  style = "font-size:18px;"),
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
    
    tabPanel("Demographics", value = "Demographics",
             fluidPage(
               h3("Demographics"),
               selectInput("demo_exp", "Age Range:",
                           c("Under 19","19-25","26-30","31-35","Prefer not to say")),
               selectInput("education", "Highest Educational Level:",
                           c("Highschool or Less","Undergraduate Degree",
                             "Graduate Degree","Prefer not to say")),
               actionButton("demo_continue", "Continue")
             )
    ),
   
    
    # Done
    tabPanel("Done", value = "Done", fluidPage(h3("Thank you!")))
  )
)
