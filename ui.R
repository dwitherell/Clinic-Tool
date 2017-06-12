#### header ----
header <- dashboardHeader(
    
    ### header styling
    title = "Clinic Mapping Tool",
    titleWidth = bar_width,
    
    ### logo/button
    tags$li(a(href = 'http://diamond/home/',
              img(src = 'morpace.png',
                  title = "Intranet", 
                  height = "30px"),
              id = "morpace_logo"),
            class = "dropdown")
)

#### sidebar ----
sidebar <- dashboardSidebar(
    
    ### sidebar styling
    width = bar_width,

    ### sidebar menu
    sidebarMenu(
        
                ## instructions tab
                menuItem('Instructions', tabName = 'INSTRUCTIONS_TAB', icon = icon('home')),

                ## load data tab
                menuItem('Load/Preview Data', tabName = 'LOAD_DATA_TAB', icon = icon('upload')),
                
                ## lever map tab
                menuItem('Lever Map', tabName = "LEVER_TAB", icon = icon('map-o')),
                
                ## perceptual map tab
                menuItem('Perceptual Map', tabName = 'PERCEPTUAL_TAB', icon = icon('map')),
                
                ## correlation tab
                menuItem('Correlations', tabName = 'CORRELATIONS_TAB', icon = icon('random')),
                
                ## about analyses tab
                menuItem("About", tabName = "ABOUT_TAB", icon = icon('question-circle'))
                
    ) # end sidebar menu
    
) # end sidebar

#### body ----
body <- dashboardBody(
    
    ### styling ----
    tags$head(
        tags$link(
            rel = "stylesheet", 
            type = "text/css", 
            href = "theme.css"
            )
        ),
    
    tags$head(
        tags$style(HTML("
                        @import url('https://fonts.googleapis.com/css?family=Open+Sans');
                        "))
        ),
    
    ### begin tab content ----
    tabItems(
        
        ## TAB 1: INSTRUCTIONS ----
        tabItem(
            tabName = 'INSTRUCTIONS_TAB',
            h3("Instructions go here.")
        ), # end instructions tab
        
        ## TAB 2: LOAD AND PREVIEW DATA ----
        tabItem(tabName = 'LOAD_DATA_TAB',
                
                # row 1: data load
                fluidRow(
                    box(
                        title = 'Select your data',
                        width = 4,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        fileInput('lever_percep_corr_file',
                                  'Choose file to upload',
                                  accept = '.rds',
                                  multiple = FALSE))
                ), # end row
                
                # row 2: data preview
                fluidRow(
                    box(
                        title = 'Preview raw stacked data',
                        width = 6,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        dataTableOutput('preview_raw')),
                    
                    box(title = 'Preview means',
                        width = 6,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        dataTableOutput('preview_means'))
                ) #end row
        ), # end load data tab
        
        ## TAB 3: LEVER MAP ----
        tabItem(tabName = 'LEVER_TAB',
                
                # row 1: lever map parameters
                fluidRow(
                    box(title = "Inputs to lever map calculation",
                        width = 12,
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        column(width = 2,
                               uiOutput("lvr_select_banner_UI"),
                               uiOutput("lvr_filter1_UI"),
                               uiOutput("lvr_filter2_UI"),
                               uiOutput("lvr_filter3_UI"),
                               uiOutput("lvr_corr_UI"),
                               uiOutput("lvr_uncheck_UI"),
                               br(),
                               textOutput("lvr_atts_selected"),
                               uiOutput("lvr_sample_size_UI")),
                        column(tweaks, width = 10,
                               uiOutput("lvr_attributes_UI")))
                ), # end row 1
                
                # action button
                actionButton(inputId = 'lvr_submit',
                             label = "Submit changes",
                             icon = icon('refresh'), width = '100%'),
                
                # row 2: lever map output and chart parameters
                fluidRow(
                    
                    # lever map output
                    box(title = "Lever map output",
                        width = 9,
                        height = '700px',
                        collapsible = FALSE,
                        solidHeader = TRUE,
                        plotOutput("lever_map")),
                    
                    # lever map parameters
                    tabBox(width = 3,
                           height = '700px',
                           
                           # SUBTAB: Build
                           tabPanel("Build",
                                    column(width = 12, textInput("lvr_plot_title", "Plot title")),
                                    column(width = 4, checkboxInput("lvr_axis_Text", "Axis text", value = FALSE)),
                                    column(width = 4, checkboxInput("lvr_axis_Ticks", "Axis ticks", value = FALSE)),
                                    column(width = 4, checkboxInput("lvr_axis_Labels", "Axis labels", value = TRUE)),
                                    column(width = 12, selectInput("lvr_label_position", "Label position",
                                                                   multiple = FALSE,
                                                                   choices = c("Best fit", 
                                                                               "Corners",
                                                                               "Top",
                                                                               "Bottom",
                                                                               "Left",
                                                                               "Right"), 
                                                                   selected = "Best fit")),
                                    column(width = 6, 
                                          numericInput("lvr_xmin", "X Min", 
                                                        value = NA,
                                                        min = -1.2, 
                                                        max = 1.2, 
                                                        step = 0.05),
                                           numericInput("lvr_ymin", "Y Min", 
                                                        value = NA,
                                                        min = -10, 
                                                        max = 10, 
                                                        step = 0.05),
                                          numericInput("lvr_point_padding", "Point padding",
                                                       value = 0.25, min = 0, max = 2, step = 0.05)),
                                    column(width = 6, 
                                           numericInput("lvr_xmax", "X Max", 
                                                        value = NA,
                                                        min = -1.2, 
                                                        max = 1.2, 
                                                        step = 0.05),
                                           numericInput("lvr_ymax", "Y Max", 
                                                        value = NA,
                                                        min = -10, 
                                                        max = 10, 
                                                        step = 0.05),
                                           numericInput("lvr_box_padding", "Box padding",
                                                        value = 0.25, min = 0, max = 2, step = 0.05))

                           ), # end tab panel

                           # SUBTAB: Style
                           tabPanel("Style",
                                    column(width = 6, numericInput("lvr_label_size", "Font size",
                                                        value = 4.5, min = 1, max = 10, step = 0.1)),
                                    column(width = 6, selectInput("lvr_font_face", "Font style",
                                                                  selected = "bold", 
                                                                  choices = fontList)),
                                    column(width = 6, numericInput("lvr_point_size", "Point size",
                                                        value = 3, min = 1, max = 10, step = 0.1)),
                                    column(width = 6, selectInput("lvr_point_select", "Point style",
                                                                  selected = "circle", 
                                                                  choices = as.character(pointList$shape))),
                                    column(width = 6, numericInput("lvr_line_size", "Intcpt. line width",
                                                                   value = 0.5, min = 0, max = 5, step = 0.1)),
                                    column(width = 6, selectInput("lvr_line_type", "Intercept line style",
                                                       choices = lineList,
                                                       selected = "longdash")),
                           
                                    column(width = 12, sliderInput("lvr_line_alpha", "Intercept line transparency", 
                                                                   value = 0.30, min = 0, max = 1, step = 0.05))
                                    # column(width = 12, sliderInput("lvr_nonsig_alpha", "Non-sig area transparency",
                                    #             value = 0.30, min = 0, max = 1, step = 0.05))
                           ), # end style tab
                                    
                           # SUBTAB: Colors
                           tabPanel("Colors",
                                    colourInput("lvr_sig_higher_color", "Sig higher color",
                                                value = "black", allowTransparent = TRUE),
                                    colourInput("lvr_sig_lower_color", "Sig lower color",
                                                value = "#D10000", allowTransparent = TRUE),
                                    colourInput("lvr_not_sig_color", "Not sig color",
                                                value = "#1875C2", allowTransparent = TRUE),
                                    
                                    colourInput("lvr_line_color", "Intercept line color", 
                                                value = "black", allowTransparent = TRUE),
                                    # colourInput("lvr_nonsig_color", "Non-sig area color:",
                                    #             value = "#F0F0F0", allowTransparent = TRUE),
                                    colourInput("lvr_segment_color", "Leader line color",
                                                value = "#B5B5B5", allowTransparent = TRUE), 
                                    colourInput("lvr_background_color", "Background color", 
                                                value = "transparent", allowTransparent = TRUE)
                                    
                           ), # end colors tab
                           
                           # SUBTAB: Download
                           tabPanel("Download",
                                    column(width = 12, textInput("lvr_map_title", "Lever map file name")),
                                    column(width = 12, textInput("lvr_table_title", "Lever data file name")),
                                    column(width = 6, downloadButton("lvr_download_map", "Download plot")),
                                    column(width = 6, downloadButton("lvr_download_data", "Download data"))

                           ) # end download tab
                    ) # end tab box
                ), # end row
                
                # table of lever map data
                fluidRow(
                    box(title = "Lever map data",
                        width = 12,
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        dataTableOutput("lvr_data_table"))
                ) # end row
        ), # end lever map tab
        
        ## TAB 4: PERCEPTUAL MAP ----
        tabItem(tabName = 'PERCEPTUAL_TAB',

                # row 1: perceptual map parameters
                fluidRow(
                    box(title = "Inputs to perceptual map calculation",
                        width = 12,
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        column(width = 2,
                               uiOutput("pm_filter1_UI"),
                               uiOutput("pm_filter2_UI"),
                               uiOutput("pm_filter3_UI"),
                               uiOutput("pm_uncheck_UI"),
                               br(),
                               textOutput("pm_atts_selected"),
                               uiOutput("pm_sample_size_UI")),
                        column(tweaks, width = 10,
                               uiOutput("pm_attributes_UI")))
                ), # end row 1
                
                # action button
                actionButton(inputId = 'pm_submit',
                             label = "Submit changes",
                             icon = icon('refresh'), width = '100%'),

                # row 2: perceptual map output and chart parameters
                fluidRow(
                    
                    # perceptual map output
                    box(title = "Perceptual map output",
                        width = 9,
                        height = '700px',
                        collapsible = FALSE,
                        solidHeader = TRUE,
                        plotOutput("perceptual_map")),
                    
                    # lever map parameters
                    tabBox(width = 3,
                           height = '700px',

                           # SUBTAB: Build
                           tabPanel("Build",
                                    column(width = 12, textInput("pm_plot_title", "Plot title")),
                                    column(width = 12, selectInput("pm_label_position", "Label position",
                                                                   multiple = FALSE,
                                                                   choices = c("Best fit", 
                                                                               "Corners",
                                                                               "Top",
                                                                               "Bottom",
                                                                               "Left",
                                                                               "Right"), 
                                                                   selected = "Best fit")),
                                    column(width = 6, 
                                           numericInput("pm_point_padding", "Point padding",
                                                        value = 0.25, min = 0, max = 2, step = 0.05)),
                                    column(width = 6, 
                                           numericInput("pm_box_padding", "Box padding",
                                                        value = 0.25, min = 0, max = 2, step = 0.05))
                                    
                           ), # end build tab
                           
                           # SUBTAB: Style
                           tabPanel("Style",
                                    column(width = 6, numericInput("pm_label_size", "Font size",
                                                                   value = 4.5, min = 1, max = 10, step = 0.1)),
                                    column(width = 6, selectInput("pm_font_face", "Font style",
                                                                  selected = "bold", 
                                                                  choices = fontList)),
                                    column(width = 6, numericInput("pm_point_size", "Point size",
                                                                   value = 3, min = 1, max = 10, step = 0.1)),
                                    column(width = 6, selectInput("pm_point_select", "Point style",
                                                                  selected = "circle", 
                                                                  choices = as.character(pointList$shape))),
                                    column(width = 6, numericInput("pm_line_size", "Int. line width",
                                                                   value = 0.5, min = 0, max = 5, step = 0.1)),
                                    column(width = 6, selectInput("pm_line_type", "Intercept line style",
                                                                  choices = lineList,
                                                                  selected = "longdash")),
                                    column(width = 12, sliderInput("pm_line_alpha", "Intercept line transparency", 
                                                                   value = 0.30, min = 0, max = 1, step = 0.05))   
                           ), # end style tab
                           
                           # SUBTAB: Colors
                           tabPanel("Colors",
                                    colourInput("pm_attribute_color", "Attribute color",
                                                value = "black", allowTransparent = TRUE),
                                    colourInput("pm_banner_color", "Banner color",
                                                value = "#D10000", allowTransparent = TRUE),
                                    colourInput("pm_line_color", "Intercept line color", 
                                                value = "black", allowTransparent = TRUE),
                                    colourInput("pm_segment_color", "Leader line color",
                                                value = "#B5B5B5", allowTransparent = TRUE), 
                                    colourInput("pm_background_color", "Background color", 
                                                value = "transparent", allowTransparent = TRUE)
                                    
                           ), # end colors tab
                           
                           # SUBTAB: Download
                           tabPanel("Download",
                                    textInput("pm_map_title", "Perceptual map file name"),
                                    textInput("pm_table_title", "Perceptual data file name"),
                                    column(width = 6, downloadButton("pm_download_map", "Download plot")),
                                    column(width = 6, downloadButton("pm_download_data", "Download data")),
                                    tags$hr()
                           ) # end download tab
                    ) # end tab box
                ), # end row
                
                
                fluidRow(
                    # table of perceptual map data
                    box(title = "Perceptual map data",
                        width = 9,
                        collapsible = TRUE,
                        solidHeader = TRUE,
                        dataTableOutput("pm_data_table"))#,
                    
                    # box(title = "Variance explained",
                    #     width = 3,
                    #     collapsible = TRUE,
                    #     collapsed = FALSE,
                    #     solidHeader = TRUE,
                    #     status = 'primary',
                    #     dataTableOutput("variance_explained"))
                ) # end row
                

                ) # end perceptual map tab
    ) # end all tabs
) # end body

#### combine ----
dashboardPage(
    
    title = "Clinic Mapping Tool",
    header = header,
    sidebar = sidebar,
    body = body
)