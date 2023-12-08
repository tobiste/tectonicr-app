# Define UI for app that draws a histogram ----
fluidPage(

  # App title ----
  titlePanel("Stress analysis using tectonicr (Stephan et al., 2023)"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      checkboxGroupInput(
        inputId = "regime_filt",
        label = "Stress regime",
        choices = list(
          "Thrust" = "T",
          "Oblique reverse" = "TS",
          "Strike-slip" = "S",
          "Oblique normal" = "NS",
          "Normal fault" = "N",
          "Unknown" = "U"
        ),
        selected = c("T", "TS", "S", "NS", "N", "U")
      ),
      sliderInput(
        inputId = "quality_filt",
        label = "Quality (\u00B0)",
        min = 0,
        max = 40,
        value = c(0, 40)
      ),

      # Input: Slider for the maximum depth ----
      sliderInput(
        inputId = "depth_filt",
        label = "Depth (km)",
        min = 0,
        max = 40,
        value = c(0, 40)
      ),
      sliderInput(
        inputId = "lon_filt",
        label = "Longitude (\u00B0)",
        min = -180,
        max = 180,
        value = c(-180, 180)
      ),
      sliderInput(
        inputId = "lat_filt",
        label = "Latitude (\u00B0)",
        min = -90,
        max = 90,
        value = c(-90, 90)
      ),
      selectInput("plate_boundary_choice", ("Plate boundary model"),
        choices = list(
          "NUVEL1" = "nuvel",
          "MORVEL56" = "morvel",
          "PB2002" = "pb2002"
        ),
        selected = "morvel"
      ),
      selectInput("motion_choice", ("Plate motion model"),
        choices = list(
          "NNR-NUVEL1A" = "NNR-NUVEL1A",
          "HS3-NUVEL1A" = "HS3-NUVEL1A",
          "NNR-MORVEL56" = "NNR-MORVEL56",
          "PB2002" = "PB2002",
          "REVEL" = "REVEL",
          "GSRM2.1" = "GSRM2.1"
        ),
        selected = "NNR-MORVEL56"
      ),
      fluidRow(
        column(
          5,
          textInput("plate_rot", ("Rotating plate"),
            value = "Enter plate..."
          )
        ),
        column(
          5,
          textInput("plate_fix", ("Fixed plate"),
            value = "Enter plate..."
          )
        )
      ),
      checkboxGroupInput(
        inputId = "traj_filt",
        label = "Modelled stress trajectories",
        choices = list(
          "Inward displacement (small circles)" = "sc",
          "Outward (displacement (great circles)" = "gc",
          "Left-lateral tangential displacement (clockwise loxodromes)" = "lc",
          "Right-lateral tangential displacement (counterclockwise loxodromes)" = "lcc"
        ),
        selected = "sc"
      ),
      radioButtons(
        inputId = "prd_type",
        label = "Displacement type",
        choices = list(
          "Unknown" = "none",
          "Outward (0\u00B0)" = "out",
          "Left-Lateral tangential (45\u00B0)" = "left",
          "Inward (90\u00B0)" = "in",
          "Right-lateral trangential (135\u00B0)" = "right"
        ),
        selected = "none"
      ),

      # sliderInput(
      #   inputId = "pb_dist_filt",
      #   label = "Distance to plate boundary",
      #   min = 0,
      #   max = 40075/2,
      #   value = c(0, 40075/2)
      # ),


      wellPanel(
      checkboxInput("por_crs", "Transform map into PoR coordinates", value = FALSE)
    )
    ),




    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Interactive map ----
      #plotOutput(outputId = "interact_map", height = "600px", click = "plot1_click", brush = brushOpts(id = "plot1_brush")),
      girafeOutput(outputId = "interact_map", height = "500px"),
      column(8,
             fluidRow(h3("Rose diagram")),
             plotOutput(outputId = "rose")
      ),
      column(4,
      # plotOutput(outputId = "distance_plot"),
      fluidRow(h3("Statistics")),
      verbatimTextOutput("stats")
      )
    )
  )
)
