fluidPage(

  # App title ----
  headerPanel(
    HTML("<p>Stress analysis using <b>tectonicr</b><br></p>")
  ),

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
            value = ""
          )
        ),
        column(
          5,
          textInput("plate_fix", ("Fixed plate"),
            value = ""
          )
        )
      ),
      # p("Abbreviations: https://www.unavco.org/software/geodetic-utilities/plate-motion-calculator/plate-motion-calculator.html"),
      p(tags$a(href = "https://www.unavco.org/software/geodetic-utilities/plate-motion-calculator/plate-motion-calculator.html", "Plate abbreviations")),
      h5("Manual Euler pole coordinates"),
      fluidRow(
        column(
          5,
          numericInput("my_elat", "Lat.:", value = NA),
        ),
        column(
          5,
          numericInput("my_elon", "Lon.:", value = NA),
        )
      ),
      h6("Enter coordinates in decimal degreees"),
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
        sliderInput(
          inputId = "rose_bw",
          label = "Rose diagram's bin width",
          min = 0,
          max = 45,
          value = 8
        ),
        checkboxInput("rose_bw_opt", "Optimal bin width", value = TRUE)
      ),
      wellPanel(
        checkboxInput("por_crs", "Transform map into PoR coordinates", value = FALSE)
      ),
      hr(),
      wellPanel(
        # html("Referece:<br>Stephan, T., Enkelmann, E., & Kroner, U. (2023). Analyzing the horizontal orientation of the crustal stress adjacent to plate boundaries. Scientific Reports, 13(1), 15590. https://doi.org/10.1038/s41598-023-42433-2")
        HTML("<p>Reference:<br>
           Stephan, T., Enkelmann, E., & Kroner, U. (2023).
           Analyzing the horizontal orientation of the crustal stress adjacent to plate boundaries. <i>Scientific Reports</i>,
           13(1), 15590. <a href='https://doi.org/10.1038/s41598-023-42433-2'>https://doi.org/10.1038/s41598-023-42433-2</a></p>")
      ),
    ),




    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Interactive map ----
      # plotOutput(outputId = "interact_map", height = "600px", click = "plot1_click", brush = brushOpts(id = "plot1_brush")),
      girafeOutput(outputId = "interact_map", height = "500px"),
      column(
        width = 8,
        fluidRow(h3("Rose diagram")),
        plotOutput(outputId = "rose")
      ),
      column(
        width = 3,
        # plotOutput(outputId = "distance_plot"),
        fluidRow(h3("Statistics")),
        verbatimTextOutput("stats")
      )
    )
  ),
  h4("Tobias Stephan (2024)",
     style = "position: absolute; bottom: 0;right:0;")
)
