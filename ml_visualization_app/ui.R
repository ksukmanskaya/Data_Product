#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
shinyUI(navbarPage(
    'COVID-19 viz',
    # shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
    theme = shinytheme("flatly"),
    
    # Documentation
    tabPanel(
        'Documentation',
        h1('Intro'),
        p("This is the RShiny app for vizualizing recent COVID-19 data in Unite States collected by John Hopkins University.
        and fitting a simple linear model for the pandemic development for specified geo region."),
        tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data", "For further information proceed here"),
        p("Please read the inscturction before going to the next pages, as the whole data preparation steps are being made right after
          launching the app, so it could take some time to produce output plots and data tables."),
        br(),
        p("The app consists of several tabs:"),
        tags$ul(
            tags$li("Documentation"),
            tags$li("Main"),
            tags$li("Citation")
        ),
        h2('Main'),
        p('The Main tab consists of several internal tabs described below.'),
        
        h3('Preview'),
        p('Provides a data preview for specified dataset and date.'),
        
        h3('Geo'),
        p('Geo projection of COVID-19 data in the USA for the particular date, which can be selected on the side panel.'),
        p('One can also see the dynamic of the pandemic over time by clicking to the small "play" button on the date slider.'),
        
        h3('Metrics'),
        p('Metrics tab consists of several reports:'),
        tags$ul(
          tags$li("Annotation Chart: dives the analyst an overview of pandemic metrics and compare the situation in different states."),
          tags$li("Motion Chart: gives the analyst an oportunity to observe the dynamic of pandemic over time for specified states and metrics."),
          tags$li("Geo Chart: quick overview of geo distribution with an ability to observe mertics in dynamic.")
        ),
        
        h3('Prediction'),
        p('This section gets the data for a specified geo region (particular state OR the whole United States) and ties to fit a simple linear model for the incidence:'),   
        code('log(y) = r*t + b'),
        p("Depending on the area selected the framework tries to find the epidemic peak date and fit two separate models for the data before the peak and the data after the peak."),
        p("If there is no peak found, then only one model is fitted."),
        p("As a result we'll have growth rate with confidence bounds and if we're lucky - negative growth rate after the peak with corresponding confidence bounds."),
        p('The analyst is asked to select the area with on the first plot to launch fitting process on selected data.'),
        tags$br(),
        em('All the modeling is being made using "incidence" package. Find more on Citation tab.')
    ),
    
    # main panel
    tabPanel(
      'Main',
      navbarPage(
          # theme=shinytheme('cerulean'),
          title='',
          # icon=icon('virus'),
          id='main_nav',
          position='static-top',
          inverse=T,
        
          # ---------------
          # 1. Data preview
          tabPanel(
              'Preview',
              icon=icon("table"),
              sidebarLayout(
                  sidebarPanel(
                      radioButtons(
                          'data_preview',
                          'Choose dataset for preview',
                          choices=c('Confirmed cases', 'Deaths', 'Recovered')
                      ),
                      dateInput(
                          'data_preview_date',
                          'Choose date',
                          min=min_date, max=max_date,
                          value=Sys.Date()-3
                      )
                  ),
                  mainPanel(
                      DT::dataTableOutput('output_data_preview')
                  )
              )
            ),
            
        
          # --------------- 
          # 2. Geo map
          tabPanel(
            'Map',
            icon=icon('map-marked-alt'),
            tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
            leafletOutput("mymap", width="100%", height="600"),
            absolutePanel(
                id='map_controls', 
                class = "panel panel-default",
                left = 50, bottom = 5, width = 320, fixed=TRUE,
                draggable = TRUE, height = 300,
                h4('US data as for chosen date'),
                h6(textOutput("for_date")),
                h5(textOutput("confirmed_cnt")),
                h5(textOutput("deaths_cnt")),
                h5(textOutput('deaths_rate')),
                h5(textOutput("recovered_cnt")),
                sliderInput(
                  'map_plot_show_date',
                  'Select date',
                  min=min_date, max=max_date,
                  value=Sys.Date()-50,
                  timeFormat = "%d %b",
                  animate=animationOptions(interval = 1000, loop = FALSE)
                )
            )
          ),
            
            
          # ---------------
          # 3. Metrics
          tabPanel(
            'Metrics',
            icon=icon('chart-bar'),
            sidebarLayout(
              sidebarPanel(
              # width=2,
                selectInput('gvis_type', 'Select plot type',
                          choices=c('Geo Map', 'Motion Chart', 'Annotation Chart'),
                          selected='Annotation Chart'),
                conditionalPanel(
                  # h3('Geo Map options:'),
                  condition = "input.gvis_type == 'Geo Map'",
                  sliderInput(
                    'gvis_geo_date',
                    'Select date for geo vis',
                    min=min_date, max=max_date,
                    value=Sys.Date()-50,
                    timeFormat = "%d %b",
                    animate=animationOptions(interval = 1000, loop = FALSE)
                  ),
                  selectInput(
                    'gvis_geo_var',
                    'Select variable for geo vis',
                    choices=c('confirmed', 'new_cases', 'deaths', 'new_deaths', 'population'),                    selected='confirmed'
                  )
                ),
                
                conditionalPanel(
                  # h3('Annotation Map options:'),
                  condition = "input.gvis_type == 'Annotation Chart'",
                  selectInput(
                    'gvis_ann_var',
                    'Select variable for annotation plot',
                    choices=c('confirmed',  'new_cases', 'new_cases_%','deaths', 'death_rate_%')
                  )
                ),
                    
                conditionalPanel(
                  condition="input.gvis_type=='Annotation Chart' || input.gvis_type=='Motion Chart'",
                  pickerInput(
                  inputId = "gvis_states_picker", 
                  label = "Select states", 
                  choices = unique(df_gvis$Province_State), 
                  selected = c('New York', 'California'),
                  options = list(
                    `actions-box` = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 3",
                    maxItems = 4
                  ), 
                  multiple = TRUE
                  )
                )
              ),
              mainPanel(
                htmlOutput('test_gvis')
              )
            )
          ),
          
          
          # ---------------
          # 4. Prediction
          tabPanel(
            'Prediction',
            icon=icon('chart-line'),
            sidebarLayout(
              sidebarPanel(width=4,
                h4('Incidence model fit'),
                selectInput('forecast_geo', 'Select geo region for model fit',
                            choices=unique(confirmed_by_geo$Province_State),
                            selected='All States'
                ),
                p('This framework tries to fit a simple linear model to the selected sets of points:'),
                code('log(y)=r*t+b'),
                p("Select the points on the first plot with your cursor to launch model fitting process."),
                h4('Model results:'),
                # textOutput('ann_text'),
                htmlOutput('ann_text_html')
                # p("Depending on the area selected the framework tries to find the epidemic peak date and fit two separate models for the data before the peak and the data after the peak."),
                # p("If there is no leak found, then only one model is fitted."),
                # p("As a result we'll have growth rate with confidence bounds and if we're lucky - negative growth rate after the peak with corresponding confidence bounds.")
                
              ),
              mainPanel(
  
                h4('New cases curve for specified geo region:'),
                plotOutput(
                  outputId='plot_incidence_df',
                  brush=brushOpts(id="plot_brush"),
                  width='90%', height='250px',
                ),
                h4('Model fit on selected data points:'),
                plotlyOutput('forecast_plot', width='90%', height='250px')
              )
            )
          )
        )
    ),
    
    tabPanel(
      'Citation',
      icon=icon('copyright'),
      h4('Epidemic model fit was built using "incidence" R package'),
      tags$a(href="https://www.repidemicsconsortium.org/incidence/authors.html", "Learn more")
    )
))
