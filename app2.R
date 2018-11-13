#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
# load the required libraries
library(dplyr)
# devtools::install_github("r-spatial/sf")
library(sf)
# devtools::install_github("pokyah/agrometVars")
library(agrometeorVars)
# installing new version of agrometAPI
#devtools::install_github("pokyah/agrometAPI")
library(agrometAPI)
# installing custo mversion of mlr
# devtools::install_github("pokyah/mlr", ref = "gstat")
library(mlr)
library(leaflet)
# library(leaflet.extras)


source("functions.R")

# loading the datasets from agrometeorVars package
data("grid.sf")
data("grid.static")
data("grid.dyn")

data("stations.sf")
data("stations.static")
data("stations.dyn")

data("intersections")

data("wallonia")


# newdata on which to predict
newdata = grid.static
newdata = sf::st_transform(newdata, 3812)
coords = data.frame(sf::st_coordinates(newdata))
st_geometry(newdata) = NULL
newdata = newdata %>%
  dplyr::bind_cols(coords) %>%
  dplyr::rename(x = X) %>%
  dplyr::rename(y = Y)

# polygrid = sf::st_buffer(x = st_transform(grid.sf, 3812), 500, nQuadSegs = 1)

# learners
lrns = list(
  # defining the simple learners
  lrn.lm.alt_x_y = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.lm",
      id = "multiReg.alt_x_y",
      predict.type = 'se'),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("elevation", "y", "x"),
    fw.abs = 3),

  # this learner does not support missing values. So we input these
  # lrn.lm.alt_x_y = makeImputeWrapper(
  #   lrn.lm.alt_x_y,
  #   cols = list(tsa_hp1 = imputeMedian()))

  lrn.gstat.idw = makeLearner(
    cl = "regr.gstat",
    id = "idw",
    predict.type = "se"),

  lrn.gstat.ts1 = makeLearner(
    cl = "regr.gstat",
    id = "ts1",
    par.vals = list(degree = 1),
    predict.type = "se"),

  lrn.gstat.ts2 = makeLearner(
    cl = "regr.gstat",
    id = "ts2",
    par.vals = list(degree = 2),
    predict.type = "se"),

  lrn.gstat.ok = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "ok",
      par.vals = list(
        range = 800,
        psill = 200000,
        model.manual = "Sph",
        nugget = 0),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x"),
    fw.abs = 2),

  lrn.gstat.ked = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "ked",
      par.vals = list(
        range = 800,
        psill = 200000,
        model.manual = "Sph",
        nugget = 0),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x", "elevation"),
    fw.abs = 3),

  lrn.gstat.1nn = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "nn1",
      par.vals = list(
        nmax = 1),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x"),
    fw.abs = 2)

)

# Choices for drop-downs
learners <- names(lrns)


parameters <- c(
  "TSA" = "TSA",
  "HRA" = "HRA",
  "HCT" = "HCT"
)

mtime = sample_n(data.frame(stations.dyn$mtime), 50)


# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  headerPanel('Spatialisation'),
  sidebarPanel(
    selectInput("learner", "learner", learners),
    selectInput("mtime", "mtime", mtime)
    #selectInput("parameter", "parameter", parameters, selected = "TSA")
  ),
  mainPanel(
    #leafletOutput("map", width="100%", height="100%")
    plotOutput('plot1'),
    tableOutput('rmse')
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  data <- reactive({
    set.seed(100)
    bmr = make.1h.bmr(
      task = mlr::makeRegrTask(
        data = make.1h.data(dateTime = input$mtime),
        target = "tsa"
      ),
      dateTime = input$mtime,
      learners = lrns)

    lrn =  lrns[[input$learner]]

    model = mlr::train(
      learner = lrn,
      task = mlr::makeRegrTask(
        data = make.1h.data(dateTime =  input$mtime),
        target = "tsa"))

    p = predict(model, newdata = newdata)
    s = grid.sf %>%
      dplyr::bind_cols(p$data)

    list(bmr = bmr, s = s)

    # ourPredictedGrid = sf::st_as_sf(data()[["s"]], coords = c("X", "Y"))
    # ourPredictedGrid = sf::st_set_crs(ourPredictedGrid, 4326)
    # sfgrid = sf::st_sf(sf::st_make_grid(x = sf::st_transform(wallonia, 3812),  cellsize = 1000, what = "polygons"))
    # ourPredictedGrid = sf::st_transform(ourPredictedGrid, crs = 3812)
    # ourPredictedGrid = sf::st_join(sfgrid, ourPredictedGrid)
    # # limit it to Wallonia
    # ourPredictedGrid = sf::st_intersection(ourPredictedGrid, sf::st_transform(wallonia, crs = 3812))
    # # stations
    # records.df = stations.dyn %>%
    #   dplyr::filter(mtime == input$mtime)
    # records.sf = stations.sf %>%
    #   dplyr::left_join(stations.sf, by = "sid")
    # records.sf = sf::st_set_crs(records.sf, 4326)
    #
    # list(bmr = bmr, ourPredictedGrid = ourPredictedGrid, records.sf = records.sf)
  })


  # quickplot
  output$plot1 <- renderPlot({
    plot = plot(data()[["s"]]["response"], pch = 15, cex = 1)
  })

  output$rmse <- renderTable({
    table = getBMRAggrPerformances(data()["bmr"]$bmr$res, as.df = TRUE)
    table = table %>%
      dplyr::arrange(rmse.test.rmse)
  })


  # Create the map
  # output$map <- renderLeaflet({
  #   leafletize(
  #     data.sf = data()[["ourPredictedGrid"]],
  #     borders = wallonia,
  #     stations = data()[["records.sf"]])
  # })
}

# Run the application
shinyApp(ui = ui, server = server)

