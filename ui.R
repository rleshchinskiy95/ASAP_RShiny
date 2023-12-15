
library(shiny)
library(shinythemes)
plotly::plotlyOutput
DT::DTOutput
library(fontawesome)
library(summaryBox)

fluidPage(
  theme = shinytheme("cosmo"),
  
  navbarPage("A S A P",
             tabPanel("Home",
                      navlistPanel(
                        id = "home_tabset",
                        "Introduction",
                        tabPanel(
                          "About ASAP",
                          fluidRow(
                            column(width = 4,
                                   HTML("<img src='Jason.jpg' width='200' style='display: block; margin: 0 auto;'>")
                                   ),
                            column(width = 8,
                                   HTML("<img src='ASAP-Logo.png' width='250'>"),
                                   HTML("<h1 style='color: blue;'>Adaptive Safety Analytics Program</h1>
                                   <p>In the ultimate goal to build a safer future for construction 
                                   workers and pedestrians, ASAP is your all-in-one tool to check the 
                                   Safety Status of your Business, Projects, and Contractors. Through 
                                   visualizations and analyses, the data 'talks' to you to help bring
                                   awareness and enhance your future safety-conscious decisions."),
                                   HTML("<h3 style='color: red;'>Disclaimer</h3>
                                   <p>The demonstrated data in the app does not reflect real-world data. 
                                   The Client and Contractor Names used are only for demonstrational
                                   purposes and their respective data is fictional."),
                                   HTML("<br><br><br><br><br><br><br><br><br>
                                        <i><h4>By Roman Leshchinskiy</h4></i>")
                                   )
                            )
                          ),
                        "Safety Explained",
                        tabPanel(
                          "Recordable",
                            fluidRow(
                              column(width = 4,
                                     HTML("<img src='osha-recordkeeping-requirements.jpg' width='350'>"),
                                     HTML("<p>Ref: https://www.grainger.com/know-how/safety-health/quick-tips/kh-osha-recordkeeping-requirements-183-qt</p>")
                                     ),
                              column(width = 8,
                                     HTML("<h1 style='color: black;'>What is a Recordable?</h2>
                                     <ul>
                                      <li>Any work-related fatality.<br>(Fatality)</li>
                                      <li>Any work-related injury or illness that results in loss of 
                                      consciousness, days away from work, restricted work, or transfer 
                                      to another job.<br>(Restricted Duty / Lost Time Injury)</li>
                                      <li>Any work-related injury or illness requiring medical treatment 
                                      beyond first aid.<br>(Medical Only)</li>
                                      <li>Any work-related diagnosed case of cancer, chronic irreversible 
                                      diseases, fractured or cracked bones or teeth, and punctured eardrums.
                                      <br>(Medical Only)</li>")
                                     )
                              )
                            ),
                        tabPanel(
                          "TRIR",
                          HTML("<h2 style='color: blue;'>TRIR</h2>
                               <p>The Total Recordable Incident Rate formula considers the total 
                               number of recordable incidents and the total hours worked by all 
                               employees. <br><u>The lower the rate, the better!</u></p>
                               <ul>
                                <li>Tracking this data helps improve safety initiatives.</li>
                                <li>Potential problems can be discovered before OSHA must 
                                intervene.</li>
                                <li>This information lends credibility to the users in the event 
                                of an inspection.</li>
                               </ul>
                               
                               <img src='TRIR-formula.jpg' width='550'>
                               <p>Ref: https://www.creativesafetysupply.com/articles/osha-incident-rates-calculators-formulas/</p>")
                          ),
                        tabPanel(
                          "DART",
                          HTML("<h2 style='color: blue;'>DART</h2>
                               <p>A DART incident is any injury or illness by an employee that results 
                               the employee taking days off/ lost-time or doctor-requested restrictions 
                               on work duty. The DART rate is a safety metric that is commonly used by OSHA 
                               to audit high-risk industries.The Days Away, Restricted, Time-Lost Rate formula 
                               considers the total number of DART incidents and the total hours 
                               worked by all employees.<br><u>The lower the rate, the better!</u></p>
                               
                               <img src='DART-rate-formula.jpg' width='550'>
                               <p>Ref: https://www.creativesafetysupply.com/articles/osha-incident-rates-calculators-formulas/</p>")
                          ),
                        tabPanel(
                          "LTIR",
                          HTML("<h2 style='color: blue;'>LTIR</h2>
                               <p>A Lost Time Injury is any injury or illness sustained by an employee that 
                               results in a loss of productive worktime. Lost Time Incidents provide an overview 
                               of how workforce injuries end up impacting a business’s productivity. The Lost Time 
                               Incident Rate formula considers the total number of Lost Time incidents and the 
                               total hours worked by all employees.<br><u>The lower the rate, the better!</u></p>
                               
                               <img src='LTIIR-formula.jpg' width='550'>
                               <p>Ref: https://www.creativesafetysupply.com/articles/osha-incident-rates-calculators-formulas/</p>")
                          ),
                        tabPanel(
                          "KPIs",
                          HTML("<h3 style='color: blue;'>Business-To-Date</h3>
                               <p>Data will be aggregated from the start date of the Business to latest date 
                               avaliable in data provided.</p>"),
                          HTML("<h3 style='color: blue;'>Project-To-Date</h3>
                               <p>Data will be aggregated from the start date of the Project to end date of project 
                               or latest date avaliable in data provided.</p>"),
                          HTML("<h3 style='color: blue;'>Contractor-To-Date</h3>
                               <p>Data will be aggregated from the earliest employment date of the Contractor to 
                               the latest employment date of Contractor.</p>"),
                          HTML("<h3 style='color: blue;'>Year-To-Date</h3>
                               <p>Data will be aggregated from the beginning of user-inputed year to the end of the
                               user-inputed year.</p>"),
                          HTML("<h3 style='color: blue;'>Month-To-Date</h3>
                               <p>Data will be aggregated from the beginning of user-inputed month of the user-inputed
                               year to the end of the user-inputted month of the user-inputed year.</p>"),
                          HTML("<h3 style='color: blue;'>Contractors</h3>
                               <p>Data will be display the number of Contractors working based on user-inputed 
                               context.</p>"),
                          HTML("<h3 style='color: blue;'>Projects</h3>
                               <p>Data will be display the number of Projects sites based on user-inputed context.</p>"),
                          HTML("<h3 style='color: blue;'>Workers</h3>
                               <p>Data will be display the number of Workers working based on user-inputed context.</p>"),
                          HTML("<h3 style='color: blue;'>Work-Hours</h3>
                               <p>Data will be display the number of Work-Hours based on user-inputed context.</p>")
                          )
                        )
                      ),
             navbarMenu("Analysis",
                        tabPanel("Overview",
                                   fluidRow(
                                     tabsetPanel(
                                       tabPanel("Business-To-Date",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    width = 4
                                                  ),
                                                  mainPanel(
                                                    h3("General Contractor LLC"),
                                                    fluidRow(
                                                      uiOutput("ov_btd_trir"),
                                                      uiOutput("ov_btd_dart"),
                                                      uiOutput("ov_btd_ltir")
                                                      ),
                                                    fluidRow(
                                                      column(3,
                                                             style = "margin-top: 90px;",
                                                             align = "center",
                                                             tableOutput("btd_ov_recTable")),
                                                      column(9, plotly::plotlyOutput("btd_ov_RecKPIplot"))
                                                      ),
                                                    fluidRow(
                                                      uiOutput("ov_btd_numCont"),
                                                      uiOutput("ov_btd_numProj"),
                                                      uiOutput("ov_btd_numWrk"),
                                                      uiOutput("ov_btd_numhour")
                                                      ) %>% tags$div(style = "margin-top: 10px;")
                                                    )
                                                  )
                                                ),
                                       tabPanel("Year-To-Date",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("year_input_ytd_ov",
                                                                label = "Year",
                                                                choices = NULL)
                                                    ),
                                                  mainPanel(
                                                    h3("General Contractor LLC"),
                                                    fluidRow(
                                                      uiOutput("ov_ytd_trir"),
                                                      uiOutput("ov_ytd_dart"),
                                                      uiOutput("ov_ytd_ltir")
                                                    ),
                                                    fluidRow(
                                                      column(3,
                                                             style = "margin-top: 90px;",
                                                             align = "center",
                                                             tableOutput("ytd_ov_recTable")),
                                                      column(8, plotly::plotlyOutput("ytd_ov_RecKPIplot"))
                                                    ),
                                                    fluidRow(
                                                      uiOutput("ov_ytd_numCont"),
                                                      uiOutput("ov_ytd_numProj"),
                                                      uiOutput("ov_ytd_numWrk"),
                                                      uiOutput("ov_ytd_numhour")
                                                      ) %>% tags$div(style = "margin-top: 10px;")
                                                    )
                                                  )
                                                ),
                                       tabPanel("Month-To-Date",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("year_input_mtd_ov",
                                                                label = "Year",
                                                                choices = NULL),
                                                    selectInput("month_input_mtd_ov",
                                                                label = "Month",
                                                                choices = NULL)
                                                  ),
                                                  mainPanel(
                                                    h3("General Contractor LLC"),
                                                    fluidRow(
                                                      uiOutput("ov_mtd_trir"),
                                                      uiOutput("ov_mtd_dart"),
                                                      uiOutput("ov_mtd_ltir")
                                                    ),
                                                    fluidRow(
                                                      column(3,
                                                             style = "margin-top: 90px;",
                                                             align = "center",
                                                             tableOutput("mtd_ov_recTable")),
                                                      column(8, plotly::plotlyOutput("mtd_ov_RecKPIplot"))
                                                    ),
                                                    fluidRow(
                                                      uiOutput("ov_mtd_numCont"),
                                                      uiOutput("ov_mtd_numProj"),
                                                      uiOutput("ov_mtd_numWrk"),
                                                      uiOutput("ov_mtd_numhour")
                                                      ) %>% tags$div(style = "margin-top: 10px;")
                                                    )
                                                  )
                                                )
                                       )
                                     )
                                 ),
                          tabPanel("Projects",
                                       fluidRow(
                                       tabsetPanel(
                                         tabPanel("Project-To-Date",
                                                  sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("projnum_input_ptd",
                                                                label = "Project #",
                                                                choices = sort(unique(active_proj$`Project #`)),
                                                                multiple = FALSE)
                                                  ),
                                                  mainPanel(
                                                    h3(textOutput("projTitle_ptd")),
                                                    fluidRow(
                                                      uiOutput("ptd_trir"),
                                                      uiOutput("ptd_dart"),
                                                      uiOutput("ptd_ltir")
                                                    ),
                                                    fluidRow(
                                                      column(6, plotly::plotlyOutput("ptdRecKPIplot")),
                                                      column(6, plotly::plotlyOutput("ptdrecplot"))
                                                    ),
                                                    fluidRow(
                                                      uiOutput("ptd_numCon"),
                                                      uiOutput("ptd_numWrk"),
                                                      uiOutput("ptd_numhour")
                                                    ) %>% tags$div(style = "margin-top: 10px;")
                                                  )
                                                  )
                                                  ),
                                         tabPanel("Year-To-Date",
                                                  sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("projnum_input_ytd",
                                                                label = "Project #",
                                                                choices = sort(unique(active_proj$`Project #`)),
                                                                multiple = FALSE),
                                                    selectInput("year_input_ytd",
                                                                label = "Year",
                                                                choices = NULL)
                                                  ),
                                                  mainPanel(
                                                    h3(textOutput("projTitle_ytd")),
                                                    fluidRow(
                                                      uiOutput("ytd_trir"),
                                                      uiOutput("ytd_dart"),
                                                      uiOutput("ytd_ltir")
                                                    ),
                                                    fluidRow(
                                                      column(6, plotly::plotlyOutput("ytdRecKPIplot")),
                                                      column(6, plotly::plotlyOutput("ytdrecplot"))
                                                      ),
                                                    fluidRow(
                                                      uiOutput("ytd_numCon"),
                                                      uiOutput("ytd_numWrk"),
                                                      uiOutput("ytd_numhour")
                                                    ) %>% tags$div(style = "margin-top: 10px;")
                                                  )
                                         )),
                                         tabPanel("Month-To-Date",
                                                  sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("projnum_input_mtd",
                                                                label = "Project #",
                                                                choices = sort(unique(active_proj$`Project #`)),
                                                                multiple = FALSE),
                                                    selectInput("year_input_mtd",
                                                                label = "Year",
                                                                choices = NULL),
                                                    selectInput("month_input_mtd",
                                                                label = "Month",
                                                                choices = NULL)
                                                  ),
                                                  mainPanel(
                                                    h3(textOutput("projTitle_mtd")),
                                                    fluidRow(
                                                      uiOutput("mtd_trir"),
                                                      uiOutput("mtd_dart"),
                                                      uiOutput("mtd_ltir")
                                                    ),
                                                    fluidRow(
                                                      column(6, plotly::plotlyOutput("mtdRecKPIplot")),
                                                      column(6, plotly::plotlyOutput("mtdrecplot"))
                                                    ),
                                                    fluidRow(
                                                      uiOutput("mtd_numCon"),
                                                      uiOutput("mtd_numWrk"),
                                                      uiOutput("mtd_numhour")
                                                    ) %>% tags$div(style = "margin-top: 10px;")
                                                  )
                                                  ))
                                         )
                                       )
                                   ),
                          tabPanel("Contractor",
                                   fluidRow(
                                     tabsetPanel(
                                       tabPanel("Contractor-To-Date",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("cont_input_ctd",
                                                                label = "Contractor Name",
                                                                choices = sort(unique(active_cont$`Contractor Name`)),
                                                                multiple = FALSE
                                                                )
                                                  ),
                                                  mainPanel(
                                                    h3(textOutput("contTitle_ctd")),
                                                    fluidRow(
                                                      uiOutput("cont_ctd_trir"),
                                                      uiOutput("cont_ctd_dart"),
                                                      uiOutput("cont_ctd_ltir")
                                                    ),
                                                    fluidRow(
                                                      column(6, plotly::plotlyOutput("ctdRecKPIplot")),
                                                      column(6, plotly::plotlyOutput("ctdrecplot"))
                                                    ),
                                                    fluidRow(
                                                      uiOutput("cont_ctd_numProj"),
                                                      uiOutput("cont_ctd_numWrk"),
                                                      uiOutput("cont_ctd_numhour")
                                                    ) %>% tags$div(style = "margin-top: 10px;")
                                                  )
                                                )
                                       ),
                                        tabPanel("Year-To-Date",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     selectInput("cont_input_ytd",
                                                                 label = "Contractor Name",
                                                                 choices = sort(unique(active_cont$`Contractor Name`)),
                                                                 multiple = FALSE),
                                                     selectInput("year_input_ytd_cont",
                                                                 label = "Year",
                                                                 choices = NULL)
                                                   ),
                                                   mainPanel(
                                                     h3(textOutput("contTitle_ytd")),
                                                     fluidRow(
                                                       uiOutput("cont_ytd_trir"),
                                                       uiOutput("cont_ytd_dart"),
                                                       uiOutput("cont_ytd_ltir")
                                                     ),
                                                     fluidRow(
                                                       column(6, plotly::plotlyOutput("ytd_cont_RecKPIplot")),
                                                       column(6, plotly::plotlyOutput("ytd_cont_recplot"))
                                                     ),
                                                     fluidRow(
                                                       uiOutput("cont_ytd_numProj"),
                                                       uiOutput("cont_ytd_numWrk"),
                                                       uiOutput("cont_ytd_numhour")
                                                       ) %>% tags$div(style = "margin-top: 10px;")
                                                     )
                                                   )
                                                 ),
                                        tabPanel("Month-To-Date",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     selectInput("cont_input_mtd",
                                                                 label = "Contractor Name",
                                                                 choices = sort(unique(active_cont$`Contractor Name`)),
                                                                 multiple = FALSE),
                                                     selectInput("year_input_mtd_cont",
                                                                 label = "Year",
                                                                 choices = NULL),
                                                     selectInput("month_input_mtd_cont",
                                                                 label = "Month",
                                                                 choices = NULL)
                                                   ),
                                                   mainPanel(
                                                     h3(textOutput("contTitle_mtd")),
                                                     fluidRow(
                                                       uiOutput("cont_mtd_trir"),
                                                       uiOutput("cont_mtd_dart"),
                                                       uiOutput("cont_mtd_ltir")
                                                     ),
                                                     fluidRow(
                                                       column(6, plotly::plotlyOutput("mtd_cont_RecKPIplot")),
                                                       column(6, plotly::plotlyOutput("mtd_cont_recplot"))
                                                     ),
                                                     fluidRow(
                                                       uiOutput("cont_mtd_numProj"),
                                                       uiOutput("cont_mtd_numWrk"),
                                                       uiOutput("cont_mtd_numhour")
                                                       ) %>% tags$div(style = "margin-top: 10px;")
                                                     )
                                                   )
                                                 )
                                       )
                                     )
                                   )
                        )
             )
  )
