###
##### LikertMakeR_online 
###
##### Hume Winzar
##### February 2026
###

#  https://winzarh-likertmaker-online.share.connect.posit.cloud/
#  https://winzar.shinyapps.io/likertMakeR_online/


# "cerulean"  "cosmo"
# "journal"   "litera"    "materia"
# "pulse"     "quartz"    "sandstone"
# "simplex"   "sketchy"   "slate"     "solar"
# "vapor"     "yeti"      "zephyr"


# ---- UI ----
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  bslib::page_navbar(
    id = "main_nav",
    theme = bs_theme(version = 5, preset = "cosmo"),
    
    navbar_options = navbar_options(class = "bg-light", theme = "light"),
    title = "LikertMakeR online",
    sidebar = bslib::sidebar(
      width = 420,
      open = list(desktop = "open", mobile = "open"),
      
      # --- Instructions sidebar ---
      conditionalPanel(
        condition = "input.main_nav == 'instructions'",
        h5("How to use LikertMakeR online"),
        div(img(src = "LikertMakeR_4.png", width = "45%"))
      ),
      
      # --- Matrix sidebar ---
      conditionalPanel(
        condition = "input.main_nav == 'matrix'",
        h5("Matrix parameters"),
        radioButtons(
          "matrixSource", "Correlation Matrix Source",
          choices = c(
            "Generate from Cronbach's alpha" = "alpha",
            "Upload CSV matrix" = "upload"
          ),
          selected = "alpha",
          inline = FALSE
        ),
        uiOutput("matrixAlphaControls"),
        uiOutput("uploadMatrixStatus"),
        conditionalPanel(
          condition = "input.matrixSource == 'upload'",
          downloadButton("downloadMatrixTemplate", "Download CSV template", class = "lm-btn-auto")
        ),
        uiOutput("downloadCurrentMatrixBtn")
      ),
      
      # --- Data generation sidebar (your current uiOutput) ---
      conditionalPanel(
        condition = "input.main_nav == 'data'",
        uiOutput("dataGenSidebar")
      ),
      
      # --- Validation sidebar (your current plot controls) ---
      conditionalPanel(
        condition = "input.main_nav == 'validate'",
        h5("Visualisation Controls"),
        downloadButton("downloadPlot", "Download Plot")
      ),
      
      # --- About sidebar ---
      conditionalPanel(
        condition = "input.main_nav == 'playtime'",
        h5("Things to try"),
        img(src = "LikertMakeR_4.png", width = "45%", align = "center")
      )
    ),
    
    ####
    ### Instructions
    ####
    nav_panel(
      title = "Instructions", value = "instructions",
      accordion(
        multiple = FALSE,
        accordion_panel(
          "Purpose",
          includeMarkdown("www/purpose.md")
        ),
        accordion_panel(
          "How to use LikertMakeR online",
          includeMarkdown("www/how_to.md")
        ),
        accordion_panel(
          "I have Cronbach's alpha and I want to generate items that
          create a multi-item scale",
          includeMarkdown("www/likert_from_alpha.md")
        ),
        accordion_panel(
          "I want to generate a dataframe of correlated scales,
          each representing separate constructs",
          includeMarkdown("www/correlated_scales.md")
        )
      )
    ),
    
    
    ####
    ### Correlation matrix
    ####
    nav_panel(
      title = "Correlation Matrix", value = "matrix",
      bslib::accordion(
        id = "matrixAccordion",
        multiple = TRUE,
        bslib::accordion_panel(
          "Current correlation matrix",
          value = "matrix",
          DT::DTOutput("corrMatrixDT")
        ),
        bslib::accordion_panel(
          "Matrix diagnostics",
          value = "diag",
          uiOutput("matrixDiagBoxes"),
          tags$hr(class = "my-2"),
          verbatimTextOutput("matrixDiagDetail")
        )
      )
    ),
    
    
    ####
    ### Data generation
    ####
    nav_panel(
      title = "Data Generation", value = "data",
      bslib::card(
        title = "generated data",
        DT::dataTableOutput("syntheticData") |>
          shinycssloaders::withSpinner(type = 5)
      )
    ), # END nav_panel "Generate Synthetic Data"
    
    
    ####
    ### Data validation
    ####
    nav_panel(
      title = "Data validation", value = "validate",
      div(
        class = "lm-main-stack",
        
        # Main: plot
        div(
          class = "lm-main-top",
          card(
            full_screen = TRUE,
            class = "border-0",
            card_header(
              "Visual Summary",
              tooltip(bs_icon("info-circle"), 
                      "How to read: 
                      \nUpper panels - correlations; 
                      \ndiagonal panels - distributions; 
                      \nlower panels - scatterplots & fitted lines"
              )
            ),
            card_body(
              style = "display:flex; flex-direction:column; height:100%;",
              div(
                plotOutput("dataVis", height = "calc(100vh - 22rem)") |>
                  withSpinner(type = 5)
              ) 
            )
          )
        ),
        
        # Bottom dock: accordion
        div(
          class = "lm-bottom-dock",
          bslib::accordion(
            id = "validateDock",
            multiple = FALSE,
            bslib::accordion_panel(
              "Summary moments",
              div(class = "stats-scroll", tableOutput("dataSummary"))
            ),
            bslib::accordion_panel(
              "Reliability (Cronbach’s alpha)",
              tags$div(
                class = "text-muted small",
                "Note: This treats all columns as items of one scale.
                       Ignore if columns represent different constructs."
              ),
              textOutput("cronbachAlphaOutput")
            ),
            bslib::accordion_panel(
              "Correlation-matrix eigenvalues",
              tags$div(
                class = "text-muted small",
                "Eigenvalues of the data *correlation matrix*.
                \nUseful for checking near-singularity / factor structure hints."
              ),
              verbatimTextOutput("eigenSummary")
            )
          )
        )
      )
    ), # end nav_panel "Data validation"
    
    
    ####
    ### About
    ####
    nav_panel(
      title = "Things to Try", value = "playtime",
      accordion(
        multiple = FALSE,
        accordion_panel(
          "Things to try",
          includeMarkdown("www/playtime.md")
        ),
        accordion_panel(
          "About LikertMakeR_online",
          includeMarkdown("www/about.md")
        ),
        accordion_panel(
          "How to cite LikertMakeR",
          includeMarkdown("www/citation.md")
        ),
        accordion_panel(
          "GitHub README",
          includeMarkdown("README.md")
        )
      )
    ), # end nav_panel "About LikertMakeR"
    
    
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(link_github),
      nav_item(link_cran)
    ) # end links nav menu
  ), # close page_navbar
) # close taglist/ ui
###
##### END User Interface 
###
