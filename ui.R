library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyEffects)
library(shinyWidgets)


## Constants (also see script.js)

popup_offset_left = 440

## Choices for drop-downs #####################

# color_vars <- c(
#   "Presidential Vote '20" = "per_dem_2020",
#   "Presidential Vote '16" = "per_dem_2016",
#   "Median Household Income" = "hh_income",
#   "Percent With College Degree" = "per_degree",
#   # "Population Density" = "pop_dens",
#   # "Percent Black" = "pct_black",
#   "Urban Classification" = "code"
# )

noramlize_vars <- c(
  "Number of migrants" = "No",
  "Number of migrants per capita" = "Yes"
)

flow_vars_agg_old <- c(
  "Incoming flow to selected counties" = "incoming", 
  "Outgoing flow from selected counties" = "outgoing",
  "Net incoming flow to selected counties" = "net_incoming",
  "Net outgoing flow from selected counties" = "net_outgoing"
)

flow_vars_agg <- c(
  "Migrants into counties w/ attribute" = "incoming", 
  "Migrants from counties w/ attribute" = "outgoing",
  "Net migration into counties w/ attribute" = "net_incoming",
  "Net migration from counties w/ attribute" = "net_outgoing"
)

flow_vars_new <- c(
  "migrants into" = "incoming", 
  "migrants from" = "outgoing",
  "net migration into" = "net_incoming",
  "net migration from" = "net_outgoing",
  "efficient migration into" = "eff_incoming",
  "efficient migration from" = "eff_outgoing"
)

view_vars <- c(
  "County shapes" = "counties",
  "Flow circles" = "circles"
)

select_by_vars_old <- c("by % Republican in 2016" = "gop_pct_16",
                        "by Median Household Income" = "hh_income",
                        "by Urban Classification" = "code",
                        "individually on the map" = "individual")

select_by_vars <- c("% Republican in 2020" = "gop_pct_20",
                    "% Republican in 2016" = "gop_pct_16",
                    "Median Household Income" = "hh_income",
                    "% With College Degree" = "per_degree",
                    "Urban Classification" = "code")

choose_by_vars <- c("individual county" = "individual",
                    "attribute" = "attribute")

## Page ###############################################

useShinyjs()

navbarPage("Robin: Migration in the U.S. 2010-2019", id="nav", windowTitle = "Robin: Migration in the U.S.",
# fluidPage(

  # tabPanel("Map",
  mainPanel(
           useShinyjs(),
  shiny::includeScript("script.js"),
    div(class="outer",

      tags$head(
        # Include custom CSS
        includeCSS("styles.css")),
      
      actionButton("about", "About", class = "menu_button float_right"),
      downloadButton("download_data", "Get Data",
                   class = "menu_button float_right", icon = icon("download"), style="display:none;"),
      actionButton("demo", "Interactive Quiz", class = "menu_button float_right"),
      # actionButton("help", "Get Started", class = "menu_button float_right"),
      # actionButton("example_4", "4", class = "menu_button float_left"),
      # actionButton("example_2", "2", class = "menu_button float_left"),
      # actionButton("example_3", "3", class = "menu_button float_left"),
      actionButton("example_1", "Example", class = "menu_button float_left"),
      # actionButton("toggle_menus", "Hide Menus", class = "menu_button float_right"),
      actionButton("scatterplot_show", "Show Scatterplot", class = "menu_button float_right", style="display:none;"),
      # actionButton('feedback', "Give Feedback", class = "menu_button float_right",
      #              onclick ="window.open('https://gatech.co1.qualtrics.com/jfe/form/SV_dh5XLhG8xzLGRwO', '_blank')"),
      actionButton("snapshots", "Public Snapshots", class = "menu_button float_left"),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      conditionalPanel('input.hide_menus != true',
                       
      ## SIDEBAR PANEL ##########################################
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    style="overflow-y:auto; max-height: calc(70vh - 25px);",
                    top = 225, left = 20, right = "auto", bottom = "auto",
                    width = 350, height = "auto", draggable = FALSE,
        
        ## NO CLICKED COUNTY: HEADER & INDIVIDUAL/ATTR. BUTTONS
        conditionalPanel("input.myEvent != 'open'",
          h3("Options"),
          conditionalPanel("false",
          radioButtons("choose_by0", "Choose mode:",
                       choose_by_vars, selected = "individual",
                       inline=TRUE)
          )
        ),
        conditionalPanel("input.no_selection != true",
        
        ## ATTRIBUTE MODE (NO CLICK): SLIDER AND SIDEBAR MAP
        conditionalPanel("input.myEvent != 'open' && input.select_by != 'individual' && input.choose_by != 'individual' && input.details_mode != true", #input.myEvent == 'open'
           selectInput("select_by0", "Attribute of interest", select_by_vars,
                       selected = "gop_pct_20", selectize=FALSE),
           # 
           # conditionalPanel("input.select_by == 'gop_pct_16'",
           #                  sliderInput("gop_pct_16", "% Republican in 2016", min=0, max=100, value=c(50, 65))
           # ),
           # conditionalPanel("input.select_by == 'gop_pct_20'",
           #                  sliderInput("gop_pct_20", "% Republican in 2020", min=0, max=100, value=c(50, 65))
           # ),
           # conditionalPanel("input.select_by == 'per_degree'",
           #                  sliderInput("per_degree", "% With College Degree", min=0, max=80, value=c(25, 35))
           # ),
           # conditionalPanel("input.select_by == 'hh_income'",
           #                  sliderInput("hh_income", "Median Household Income", min=18000, max=125000, value=c(50000, 65000), step=1000)
           # ),
           # 
           # conditionalPanel("input.select_by == 'code'",
           #                  tags$div(id = "slider1", class="slider1",
           #                           # sliderInput("code_raw", "Urbanicity (1 = most urban, 6 = most rural)",
           #                           #             min=1, max=6, value=2, ticks=TRUE),
           #                           sliderTextInput("code", "Urban Classification", c("Urban", "Suburban", "Exurban", "Rural"), 
           #                                           selected="Suburban", grid=TRUE)
           # 
           #                  ),
           # ),
           h4(textOutput("small_map_title0")),
           # h4("Counties with Attribute"),
           plotOutput("small_map0", height = 200),
           # h4("Large Map Options"),
           # selectInput("in_out", "Show counties with the most...", flow_vars2, selected = "incoming"),
        ),

        ## COUNTY NAME HEADER (CLICK)
        conditionalPanel("input.myEvent == 'open'", #input.myEvent == 'open'
           # h3(textOutput("county_name"))
           h3("Options")
           # selectInput("in_out", "Show counties with the most...", flow_vars3, selected = "incoming"),
        ),
        
        # ## DETAILS MODE TEXT AND MAP
        # conditionalPanel("input.details_mode == true",
        #                  # h3("Blah"),
        #                  textOutput("details_mode_sidebar_text"),
        #                  plotOutput("small_map_2", height = 200)
        # ),
        
        # ## INCOMING/OUTGOING SELECTOR
        # conditionalPanel("input.details_mode != true",
        #                  selectInput("old_in_out", "On large map, show counties with the most...", flow_vars_agg, selected = "incoming")
        #                  ),
        
        # ## NUMBER OF COUNTIES SLIDER
        # sliderInput("num", "Show how many top counties?", min=1, max=1500, value=50),
        
        ## NORMALIZE OPTION
        conditionalPanel("false", #input.myEvent == 'open'
                         selectInput("norm", "Show top counties based on...",
                                     noramlize_vars, selected = "No")#,
        ),
        
      ),
      
      # ## PROMPT WHEN IN INDIVIDUAL MODE WITH NO CLICK
      # conditionalPanel("input.no_selection == true",
      #                  h4("Click a County"),
      #                  p("Click a county on the map to view its migration flows.")
      # ),
      
      ## SELECT COLOR
      selectInput("color_old", "Color", color_vars, selected = "per_dem_2020", selectize=FALSE),
      
      ## VIEW (COUNTIES OR CIRCLES)
      conditionalPanel("input.no_selection != true",
                       # selectInput("view", "View", view_vars, selected = "counties"),
                       radioButtons("view_old", "View", view_vars, selected = "counties",
                                    inline=TRUE)
      ),
      
      ## SHOW ARROWS FOR ALL COUNTIES?
      conditionalPanel("input.myEvent == 'open'",
                       # radioButtons("show_arrows", "Show arrows for each county?",
                       #              c("Yes", "No"), selected = "Yes",
                       #              inline=TRUE),
                       checkboxInput("show_arrows_alt_old", "Show arrows for each county",
                                     value = TRUE)
      )
    ),
    
    ## HELP ###########################################
    conditionalPanel("input.help_skip != 'yes'", 
                     absolutePanel(id = "help_panel", class = "panel panel-default", fixed = TRUE,
                                   draggable = TRUE, top = 60, left = popup_offset_left, right = "auto", bottom = "auto", # left = "auto", right = 70, 
                                   width = 500, height = "auto",  # Width = 330   # left = 60, right = "auto"
                                   
                                   h3("Get Started", style="display: inline-block;"),
                                   
                                   actionButton("help_close", "",
                                                icon = icon("remove", lib="glyphicon"),
                                                class = "close_button"),
                                   
                                   # textOutput("demo_text"),
                                   htmlOutput("help_text"),
                                   p(),
                                   
                                   actionButton("help_back", "Back", style="display:inline-block", disabled=TRUE),
                                   conditionalPanel("input.help_done != 'yes'", style="display:inline-block", 
                                                    # actionButton("help_skip", "Close", style="display:inline-block"),
                                                    actionButton("help_next", "Next",
                                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4; display:inline-block")
                                   ),
                                   conditionalPanel("input.help_done == 'yes'", style="display:inline-block",
                                                    actionButton("help_end", "Done",
                                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4; display:inline-block")
                                   ),
                                   div(
                                     class = "footer",
                                     htmlOutput("help_pagenum"),
                                     style="float:right"
                                   )
                     )),
    
    
      ## DEMO ###########################################
      conditionalPanel("input.demo_show == 'yes'", 
       absolutePanel(id = "demo_panel", class = "panel panel-default", fixed = TRUE,
                     draggable = TRUE, top = 60, left = popup_offset_left, right = "auto", bottom = "auto", # left = "auto", right = 70, 
                     width = 500, height = "auto",  # Width = 330   # left = 60, right = "auto"
                     
                     h3("Interactive Quiz", style="display: inline-block;"),
                     
                     actionButton("demo_close", "",
                                  icon = icon("remove", lib="glyphicon"),
                                  class = "close_button"),
                     
                     # textOutput("demo_text"),
                     
                     htmlOutput("demo_main_text"),
                     p(),
                     # p("Are most migrants moving out from denver county moving locally?"),
                     radioButtons("question_radio",
                                  "placeholderText",
                                  choices=c("placeholderA","placeholderB"),
                                  selected = character(0),
                                  width = "100%"),
                     
                     htmlOutput("demo_feedback"),
                     
                     p(),
                     
                     actionButton("demo_back", "Back", style="display:inline-block", disabled=TRUE),
                     conditionalPanel("input.demo_done != 'yes'", style="display: inline-block;",
                                      # actionButton("demo_skip", "Close", style="display:inline-block"),
                                      actionButton("demo_next", "Next",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4; display:inline-block")
                     ),
                     conditionalPanel("input.demo_done == 'yes'", style="display: inline-block;",
                                      actionButton("demo_end", "Done",
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4; display:inline-block")
                     ),
                     div(
                       class = "footer",
                       htmlOutput("demo_pagenum")
                     ),
       )),
    
    
      ## ABOUT PANEL ###########################################
      absolutePanel(id = "about_panel", class = "panel panel-default", fixed = TRUE, style="display:none",
                    draggable = FALSE, top = 60, left = popup_offset_left, right = "auto", bottom = "auto", # left = "auto", right = 70, 
                    width = 500, height = "auto",  # Width = 330   # left = 60, right = "auto"
                   
                    h3("About", style="display: inline-block;"),
                    
                    actionButton("about_close_alt", "",
                                 icon = icon("remove", lib="glyphicon"),
                                 class = "close_button"),
                   
                    # p("Migration data from ", a("the IRS", href="https://www.irs.gov/statistics/soi-tax-stats-migration-data", target="_blank"),
                    p("For documentation, please refer to ", a("this document",
                                                                               href="https://drive.google.com/file/d/1DGk05Rp58fToMztrrTd_VEtR_ZBacPeA/view?usp=sharing",
                                                                               target="_blank"),
                      br(), br(),
                      "For questions or comments, contact", strong("Alexander Bendeck"), "(abendeck3@gatech.edu)"),
                    
                    actionButton("about_close", "Close"),
      ),
    
    ## SNAPSHOT PANEL ###########################################
    absolutePanel(id = "snapshot_view_panel", class = "panel panel-default", fixed = TRUE, style="display:none",
                  draggable = FALSE, top = 60, left = "auto", right = 30, bottom = "auto",
                  width = 500, height = "auto",  # Width = 330   # left = 60, right = "auto"
                  
                  h3("Public Snapshots", style="display: inline-block;"),
                  actionButton("add_snapshot", "Add",
                               class = "menu_button", icon = icon("pencil"),
                               style="float: right; margin-top: 15px;"),
                  h5(HTML(paste0("Loading...<br><br>", 
                            "Try reloading the site if this takes more than 20 seconds.")), 
                     style="display:none", id="table_loading_message"),
                  h5(HTML(paste0("ERROR!<br><br>Try closing this panel and loading it again, ", 
                            "or reload the site if the problem persists.")), 
                     style="display:none", id="table_error_message"),
                  DT::dataTableOutput("table")
    ),
    
    
    
      ## SHAPSHOT SUBMISSION ###########################################
      absolutePanel(id = "snapshot_submission_panel", class = "panel panel-default", fixed = TRUE, style="display:none",
                    draggable = FALSE, top = 60, left = popup_offset_left, right = "auto", bottom = "auto", # left = "auto", right = 70, 
                    width = 500, height = "auto",  # Width = 330   # left = 60, right = "auto"
                    
                    h3("Submit Snapshot", style="display: inline-block;"),
                    
                    actionButton("snapshot_submission_close_alt", "",
                                 icon = icon("remove", lib="glyphicon"),
                                 class = "close_button"),
                    
                    textInput("snapshot_submission_title", "Title", value = "",
                              width = 450, placeholder = "Snapshot Title (optional)"), # width = NULL
                    
                    textAreaInput("snapshot_submission_comments", "Comments", value = "",
                              width = 450, placeholder = "Comments (optional)"), # width = NULL
                    
                    actionButton("snapshot_submission_close", "Cancel", style="display:inline-block"),
                    actionButton("snapshot_submission_submit", "Submit",
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4; display:inline-block")
      ),
    
    ## EXAMPLES #######################################
    conditionalPanel("input.example_panel == 'yes'", 
                     absolutePanel(id = "example_panel", class = "panel panel-default", fixed = TRUE,
                                   draggable = TRUE, top = 60, left = popup_offset_left, right = "auto", bottom = "auto", # left = "auto", right = 70, 
                                   width = 500, height = "auto",  # Width = 330   # left = 60, right = "auto"
                                   
                                   h3(textOutput("example_name"),
                                      style="display: inline-block;"),
                                   
                                   actionButton("example_close", "",
                                                icon = icon("remove", lib="glyphicon"),
                                                class = "close_button"),
                                   
                                   # textOutput("demo_text"),
                                   htmlOutput("example_text"),
                                   # p(),
                                   # img(src='myImage.png', align = "right", height=34),
                                   # HTML(example_text[1]),
                                   p(),
                                   
                                   actionButton("example_end", "Done",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                   
                                   htmlOutput("example_img"),
                                   
                                   
                     )),
                     
    
    ## TEXT DESCRIPTION (INSIGHT) ########################
    conditionalPanel("true && input.no_selection != true",
                       # absolutePanel(id = "text_desc", class = "panel panel-default", fixed = TRUE,
                       #               draggable = FALSE, bottom = 10, left = popup_offset_left, right = "auto", top = "auto",
                       #               width = 500, height = "auto",  # Width = 330   # left = 60, right = "auto"
                       # 
                       #               htmlOutput("desc_text")),


      ## SCATTERPLOT #######################################
      conditionalPanel("input.no_selection != true && input.scatterplot_hidden != true",  #true  #input.myEvent == 'open'
                       absolutePanel(id = "plot2", class = "panel panel-default", fixed = TRUE,
                                     draggable = FALSE, top = "auto", left = "auto", right = 20, bottom = 10,
                                     width = 400, height = 250,  # right = 20, left="auto
                                     actionButton("scatterplot_close", "",
                                                  icon = icon("remove", lib="glyphicon"),
                                                  class = "close_button"),
                                     plotlyOutput("scatter", height=240),
                                     )
      )  ## Scatterplot conditional panel
    ), ## input.no_selection != true (Text description (insight), scatterplot)
    
    ## NARRATIVE CONTROLS ###########################################
    # conditionalPanel("true",
           absolutePanel(id = "new_controls", class = "panel panel-default", fixed = TRUE,
                         style="max-width: 425px; overflow-y:auto; max-height: calc(90vh - 25px);",
                         # style="max-width: 665px;", # max-width: 665px
                         draggable = FALSE, top = 60, left = 20, right = "auto", bottom = "auto",
                         width = "auto", height = "auto",  # Width = 330   # left = 60, right = "auto"
                         
                         conditionalPanel("input.myEvent != 'open'",
                         h3("Choose Mode", class="top-header"),
                         radioButtons("choose_by", NULL,# "Choose mode:",
                                      choose_by_vars, selected = "individual",
                                      inline=TRUE),
                         conditionalPanel("input.myEvent != 'open' && input.select_by != 'individual' && input.choose_by != 'individual' && input.details_mode != true",
                                          selectInput("select_by", "Attribute:", select_by_vars,
                                                      selected = "gop_pct_20", selectize=FALSE),
                                          style="display:block;"
                                          ),
                         style="display:block;"
                         ),
                         
                         conditionalPanel('input.no_selection == true',
                                          h3("Click a County"),
                                          p("Click a county on the map to view its migration flows.")
                         ),
                         
                         conditionalPanel('input.no_selection != true',
                         
                         conditionalPanel("input.myEvent != 'open'",
                                          h3("Show the..."), # "Current View"
                                          ),
                         
                         conditionalPanel("input.myEvent == 'open'",
                                          h3("Show the...", class="top-header"), # "Current View"
                                          ),
                         br(),
                         
                         div(
                         p("Top",
                           class="inline-block-text"),
                         
                         div(
                         numericInput("new_num_counties", label = NULL,
                                      value = 50, min = 1, max = 9999,
                                      width = "fit-content"),
                         class="inline-input-container buffer-right"),
                         # style="display:block;"),
                         
                         p(textOutput("max_num_counties", inline=TRUE), class="inline-block-text"),
                         
                         conditionalPanel("input.details_mode == true",
                                          htmlOutput("details_mode_text", inline=TRUE),
                                          br(),
                                          style="display: inline;"),  #  line-height: 2;
                         
                         conditionalPanel("input.details_mode != true",
                         p("counties with the most ",
                           class="inline-block-text"),
                         br(),
                         # style="display:block;"),
                         
                         div(
                         selectInput("in_out", label = NULL,
                                     flow_vars_new, selectize=FALSE,
                                     width = "fit-content", selected = "incoming"),
                         class="inline-input-container buffer-right"),
                         
                         # htmlOutput("statusText", style="display: inline;"),
                         
                         conditionalPanel("input.select_by == 'gop_pct_20' && input.choose_by == 'attribute'",
                                          p(HTML("counties that voted<br>"),
                                            class="inline-text right-of-dropdown"),
                                          numericInput("gop_pct_20_A", label = NULL,
                                                       value = 50, min = 0, max = 100,
                                                       width = "fit-content"),
                                          p("% to",
                                            class="inline-block-text"),
                                          numericInput("gop_pct_20_B", label = NULL,
                                                       value = 65, min = 0, max = 100,
                                                       width = "fit-content"),
                                          p("% Republican in 2020",
                                            class="inline-block-text"),
                                          style="display: inline;"
                         ),
                         
                         conditionalPanel("input.select_by == 'gop_pct_16' && input.choose_by == 'attribute'",
                                          p(HTML("counties that voted<br>"),
                                            class="inline-text right-of-dropdown"),
                                          numericInput("gop_pct_16_A", label = NULL,
                                                       value = 50, min = 0, max = 100,
                                                       width = "fit-content"),
                                          p("% to",
                                            class="inline-block-text"),
                                          numericInput("gop_pct_16_B", label = NULL,
                                                       value = 65, min = 0, max = 100,
                                                       width = "fit-content"),
                                          p("% Republican in 2016",
                                            class="inline-block-text"),
                                          style="display: inline;"
                         ),
                         
                         conditionalPanel("input.select_by == 'per_degree' && input.choose_by == 'attribute'",
                                          p(HTML("counties where<br>"),
                                            class="inline-text right-of-dropdown"),
                                          numericInput("per_degree_A", label = NULL,
                                                       value = 25, min = 0, max = 100,
                                                       width = "fit-content"),
                                          p("% to",
                                            class="inline-block-text"),
                                          numericInput("per_degree_B", label = NULL,
                                                       value = 35, min = 0, max = 100,
                                                       width = "fit-content"),
                                          p("% of residents have a degree",
                                            class="inline-block-text"),
                                          style="display: inline;"
                         ),
                         
                         conditionalPanel("input.select_by == 'hh_income' && input.choose_by == 'attribute'",
                                          p(HTML("counties with a<br>$"),
                                            class="inline-text right-of-dropdown"),
                                          numericInput("hh_income_A", label = NULL,
                                                       value = 50000, min = 18000, max = 125000,
                                                       step = 1000, width = "fit-content"),
                                          p("to $",
                                            class="inline-block-text"),
                                          div(
                                          numericInput("hh_income_B", label = NULL,
                                                       value = 65000, min = 18000, max = 125000,
                                                       step = 1000, width = "fit-content"),
                                          class="inline-input-container buffer-right"),
                                          p(HTML(" median income"),
                                            class="inline-text right-of-dropdown"),
                                          
                                          #p(".",
                                          #  class="inline-block-text"),
                                          style="display: inline;"
                         ),
                         
                         conditionalPanel("input.select_by == 'code' && input.choose_by == 'attribute'",
                                          
                                          p(HTML(""),
                                            class="inline-text right-of-dropdown"),
                                          
                                          div(
                                          selectInput("code_select", label = NULL,
                                                      c("urban" = "Urban",
                                                        "suburban" = "Suburban",
                                                        "small metro" = "Small metro",
                                                        "rural" = "Rural"), 
                                                      selected="Suburban",
                                                      selectize=FALSE, width = "fit-content"),
                                          class="inline-input-container buffer-right"),
                                          
                                          
                                          p(HTML("counties"),
                                            class="inline-text right-of-dropdown"),
                                          style="display: inline;"
                         ),
                         
                         # div(
                         conditionalPanel("input.choose_by == 'attribute'",
                                          h5(textOutput("small_map_title"), style="margin-bottom: 0px;"),
                                          # h4("Counties with Attribute", style="margin-bottom: 0px;"),
                                          style="display: block;"),# style="display: block;"),
                         conditionalPanel("input.choose_by == 'attribute'",
                           # h4(textOutput("small_map_title")),
                           plotOutput("small_map", height = 200, width=300),
                         style="display: block; text-align: center;"),# style="display: block;"),
                         
                         conditionalPanel("input.choose_by == 'individual'",
                           strong(textOutput("new_county_name"),
                              style="display: inline-block;")# margin-left: 10px;")#,
                           # style="display: inline;"
                         ),
                         style="display:inline"), #input.details_mode != true 
                         style="display:block;", class="current-view-controls"), # div
                         ), #input.no_selection != true
                         # conditionalPanel("input.myEvent == 'open'",
                         #                  p("Click anywhere on the map to deselect."),
                         #                  style="display:block; margin-top:10px; margin-bottom:15px;"
                         # ),
                         
                         conditionalPanel("input.choose_by != 'attribute'",
                                          h3("Options"), style="display: block;"),
                         conditionalPanel("input.choose_by == 'attribute'",
                                          h3("Options", style="margin-top: 0px;"), style="display: block;"),
                         div(
                          selectInput("color", "Color:", color_vars, selected = "per_dem_2020", selectize=FALSE),
                         style="display:block;"),
                         
                         conditionalPanel('input.no_selection != true',
                             div(
                               radioButtons("view", "View:", view_vars, selected = "counties",
                                            inline=TRUE),
                             style="display:block;"),
                             conditionalPanel("input.no_selection != true && !(input.choose_by == 'attribute' && input.details_mode != true)",  # input.choose_by !== 'attribute'
                                              
                             div(
                               # radioButtons("show_arrows2", "Show arrows for each county?",
                               #              c("Yes", "No"), selected = "Yes", inline=TRUE),
                               checkboxInput("show_arrows_alt", "Show arrows for each county",
                                             value = TRUE),
                             style="display:block;")
                             ),
                             
                             # 
                             # conditionalPanel("input.choose_by == 'attribute'",
                             #   htmlOutput("blahText", style="display: inline-block;"),
                             #   style="display:inline;"
                             # ),

                         ) #input.no_selection != true
           ), # new_controls panel
     # ), # conditional pannel wrapping new_controls
    
    # conditionalPanel("false",
    #                  absolutePanel(id = "new_controls_alt", class = "panel panel-default", fixed = TRUE,
    #                                draggable = TRUE, top = 60, left = popup_offset_left, right = "auto", bottom = "auto",
    #                                width = 650, height = "auto",  # Width = 330   # left = 60, right = "auto"
    #                                
    #                                h3("Click a County"),
    #                                p("Click a county on the map to view its migration flows.")
    #                                
    #                                )
    #                 ),
    
    ) ## input.hide_menus != true
    ) ## div(class="outer", ...
  ),  ## mainPanel
# tabPanel(title = "Public Snapshots", value = "snapshots",
#          h4("This table shows snapshots submitted by others."),
#          h5(paste0("ERROR! Try exiting this panel and loading it again, ", 
#                    "or reload the site if the problem persists."), 
#             style="display:none", id="table_error_message_old"),
#          br(),
#          DT::dataTableOutput("table_old")
# ),

## CUSTOMIZE HTML USING JAVASCRIPT ##########################
tags$script(HTML("var header = $('.navbar> .container-fluid');
                 header.append($('#about')); 
                 header.append($('#demo')); 
                 header.append($('#download_data'));
                 header.append($('#scatterplot_show')); 
                 // header.append($('#toggle_menus')); 
                 // header.append($('#feedback')); 
                 header.append($('<p id=\"example_label\"><b>Snapshots:</b></p>'));
                 header.append($('#example_1')); 
                 // header.append($('#example_2')); 
                 // header.append($('#example_3')); 
                 // header.append($('#example_4')); 

                 header.append($('#snapshots')); 
                 // header.append($('#help')); 

                 // console.log(header)"))

)

