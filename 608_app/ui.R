shinyUI(pageWithSidebar(headerPanel("CMS 2012 Provider Data"),

  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1", helpText(""),
                                   radioButtons("radio", label = h3("Select Graphs to display"),
                                  choices = list("Medicare Provider Payment Per State" = 1,
                                                "Medicare Provider Payment Per Enrolled Per State" = 2,
                                                "Average Medicare Payment by Provider Type Per Beneficiary" = 3,
                                                "Medicare Payment by Provider" = 4,
                                                "Average Medicare Payment to Provider per Beneficiary" = 5,
                                                "Average Medicare Payment per Beneficiary per Zipcode" = 6,
                                                "Total Medicare Payment per Beneficiary per State" = 7),
                                 selected = 1),
                     uiOutput(outputId = "varslider"),
                     checkboxInput("chkvalue", label = strong("Show Table"),value=FALSE)
    ),
    conditionalPanel(condition="input.conditionedPanels==2", helpText(h3("Mapping CMS Data using Google Maps")),

                     radioButtons("panel_2_radio", label = h4("Select Maps to display"),
                                  choices = list("Medicare Enrollees by State" = 1,
                                                 "Medicare Payments per Enrollee by State" = 2,
                                                 "Total Medicare Payments per Enrollee by State" = 3),
                                                 #"Medicare 4" = 4,
                                                 #"Medicare Payments per Enrollee by Zip Code" = 5),
                                  selected = 1),
                    # sliderInput("grid", "Select Number of returns:",min=3,max=61,value=3,step=1,animate=TRUE),
                    # sliderInput("i", "Range of desired grid:",min=1,max=14,value=3),
                     checkboxInput("panel_2_chkvalue", label = strong("Show Table"),value=FALSE)
    ),

    conditionalPanel(condition="input.conditionedPanels==3", helpText(h3("Statistical Analysis using CMS Data")),

                     radioButtons("panel_3_radio", label = h3("Select Analysis to display"),
                                  choices = list("Linear Regression Model" = 1,
                                                 "95% Confidence Intervals" = 2,
                                                 "Plot of Fitted Relationship Model vs Data" = 3,
                                                 "Plot of Residuals" = 4),
                                                 #"Medicare 4" = 5,
                                                 #"Medicare Payments per Enrollee by Zip Code" = 6),
                                  selected = 1)#,
                     # sliderInput("grid", "Select Number of returns:",min=3,max=61,value=3,step=1,animate=TRUE),
                     # sliderInput("i", "Range of desired grid:",min=1,max=14,value=3),
                     #checkboxInput("panel_2_chkvalue", label = strong("Show Table"),value=FALSE)
    ),



 conditionalPanel(condition="input.conditionedPanels==4", helpText(h3("Documentation")),
                     radioButtons("panel_4_radio", label = h3("Select Documents to display"),
                                  choices = list("Title and Introduction" = 1,
                                                 "Backgound and Datasets" = 2,
                                                 "Program Functionality, Design and Implementation" = 3,
                                                 "Constraints and Limitations" = 4,
                                                 "References" = 5),
                                  selected = 1))





  ),
  mainPanel(
    tabsetPanel(
      tabPanel("CMS Data 2012", value=1,
               #textOutput("panel_1_text"),
               #verbatimTextOutput("value"),
               plotOutput("main_plot",height=720,width=1200),
               #downloadButton(outputId = "downloadPlot", label = "Download Plot"),
               dataTableOutput('panel_1_table')),


     tabPanel("CMS Data Maps", value=2,
               h3(textOutput("panel_2_text")),
               #verbatimTextOutput("value1"),
               #plotOutput("panel_2_plot",height=720,width=1200),
               htmlOutput("panel_2_view", height=720,width=1200),
               #downloadButton(outputId = "downloadPlot1", label = "Download Plot"),
               dataTableOutput('panel_2_table')),

     tabPanel("Statistical Analysis", value=3,
              #h3(textOutput("panel_3_text")),
              verbatimTextOutput("panel_3_verbatim"),
              plotOutput("panel_3_plot",height=720,width=1200),
              #htmlOutput("panel_3_view", height=720,width=1200),
              #downloadButton(outputId = "downloadPlot1", label = "Download Plot"),
              dataTableOutput('panel_3_table')),

     tabPanel("Documentation", value=4,
               h3(textOutput("panel_4_text")),
               #verbatimTextOutput("value1"),
               #plotOutput("panel_2_plot",height=720,width=1200),
               htmlOutput("panel_4_view", height=600,width=800),
               #downloadButton(outputId = "downloadPlot1", label = "Download Plot"),
               dataTableOutput('panel_4_table')






               ), id = "conditionedPanels" ))
))
