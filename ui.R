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
        label = "Quality (degree)",
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
        label = "Longitude (degree)",
        min = -180,
        max = 180,
        value = c(-180, 180)
      ),
      sliderInput(
        inputId = "lat_filt",
        label = "Latitude (degree)",
        min = -90,
        max = 90,
        value = c(-90, 90)
      ),
      selectInput("plate_boundary_choice", ("Plate boundary model"),
        choices = list(
          "PB2002" = "pb2002", "MORVEL56" = "morvel",
          "NUVEL1" = "nuvel"
        ),
        selected = "pb2002"
      ),
      selectInput("motion_choice", ("Plate motion model"),
        choices = list("NNR-NUVEL1A" = "NNR-NUVEL1A", "NNR-MORVEL56" = "NNR-MORVEL56", "GSRM2.1" = "GSRM2.1", "HS3-NUVEL1A" = "HS3-NUVEL1A", "REVEL" = "REVEL", "PB2002" = "PB2002"),
        selected = "PB2002"
      ),
      textInput("plate_fix", ("Fixed plate"),
        value = "Enter plate..."
      ),
      textInput("plate_rot", ("Rotating plate"),
        value = "Enter plate..."
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
          "Outward (0)" = "out",
          "Left-Lateral tangential (45)" = "left",
          "Inward (90)" = "in",
          "Right-lateral trangential (135)" = "right"
        ),
        selected = "none"
      ),


      checkboxInput("por_crs", "Transform map into PoR coordinates", value = FALSE)
    ),




    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Interactive map ----
      plotOutput(outputId = "interact_map"),
      plotOutput(outputId = "rose"),
      verbatimTextOutput(outputId = "stats")
    )
  )
)
