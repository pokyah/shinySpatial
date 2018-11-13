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
library(leaflet.extras)


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

responsiveness.chr = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"

# newdata on which to predict
newdata = grid.static
newdata = sf::st_transform(newdata, 3812)
coords = data.frame(sf::st_coordinates(newdata))
st_geometry(newdata) = NULL
newdata = newdata %>%
  dplyr::bind_cols(coords) %>%
  dplyr::rename(x = X) %>%
  dplyr::rename(y = Y)

sfgrid = sf::st_sf(sf::st_make_grid(x = sf::st_transform(grid.static, 3812),  cellsize = 1000, what = "polygons"))


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

mtime = head(stations.dyn$mtime, 24)


# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  headerPanel('Spatialisation'),
  sidebarPanel(
    selectInput("learner", "learner", learners),
    selectInput("mtime", "mtime", mtime)#,
    #selectInput("parameter", "parameter", parameters, selected = "TSA")
  ),
  mainPanel(
    leafletOutput("map")
    #plotOutput('plot1')
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ourbmr <- reactive({
    bmr = make.1h.bmr(
      task = mlr::makeRegrTask(
        data = make.1h.data(dateTime = input$mtime),
        target = "tsa"
      ),
      dateTime = input$mtime,
      learners = lrns[[input$learner]])
  })

  ourPred <- reactive({
    lrn = lrns[[input$learner]]
    model = mlr::train(
      learner = lrn,
      task = mlr::makeRegrTask(
        data = make.1h.data(dateTime = mtime[[1]]),#input$mtime),
        target = "tsa"))
    p = predict(model, newdata = newdata)
    s = grid.sf %>%
      dplyr::bind_cols(p$data)
    # making points look polygons !
    ourPredictedGrid = sf::st_as_sf(s, coords = c("X", "Y"))
    ourPredictedGrid = sf::st_set_crs(ourPredictedGrid, 4326)
    ourPredictedGrid = sf::st_transform(ourPredictedGrid, crs = 3812)
    ourPredictedGrid = sf::st_join(sfgrid, ourPredictedGrid)
    # limit it to Wallonia
    ourPredictedGrid = sf::st_intersection(ourPredictedGrid, sf::st_transform(wallonia, crs = 3812))
    ourPredictedGrid = sf::st_transform(ourPredictedGrid, 4326)
  })

  ourObs <- reactive({
    # stations
    records.df = stations.dyn %>%
      dplyr::filter(mtime == input$mtime)
    records.sf = stations.sf %>%
      dplyr::left_join(records.df, by = "sid")
    records.sf = sf::st_set_crs(records.sf, 4326)
    head(records.sf)
    records.sf
  })


  # Create the map
  output$map <- renderLeaflet({
    # to make the map responsive
    responsiveness.chr = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"

    # Sometimes the interpolation and the stations don't have values in the same domain.
    # this lead to mapping inconsistency (transparent color for stations)
    # Thus we create a fullDomain which is a rowbinding of interpolated and original data
    fullDomain = c(ourPred()$response, ourObs()$tsa)

    # defining the color palette for the response
    varPal <- leaflet::colorNumeric(
      palette = "RdYlBu", #"RdBl",
      reverse = TRUE,
      domain = fullDomain, #data.sf$response,
      na.color = "transparent"
    )

    # Definition of the function to create whitening
    alphaPal <- function(color) {
      alpha <- seq(0,1,0.1)
      r <- col2rgb(color, alpha = T)
      r <- t(apply(r, 1, rep, length(alpha)))
      # Apply alpha
      r[4,] <- alpha*255
      r <- r/255.0
      codes <- (rgb(r[1,], r[2,], r[3,], r[4,]))
      return(codes)
    }

    # actually building the map
    prediction.map = leaflet::leaflet(ourPred()) %>%
      # basemaps
      addProviderTiles(group = "Stamen",
        providers$Stamen.Toner,
        options = providerTileOptions(opacity = 0.25)
      ) %>%
      addProviderTiles(group = "Satellite",
        providers$Esri.WorldImagery,
        options = providerTileOptions(opacity = 1)
      ) %>%
      # centering the map
      fitBounds(sf::st_bbox(ourPred())[[1]],
        sf::st_bbox(ourPred())[[2]],
        sf::st_bbox(ourPred())[[3]],
        sf::st_bbox(ourPred())[[4]]
      ) %>%
      # adding layer control button
      addLayersControl(baseGroups = c("Stamen", "Satellite"),
        overlayGroups = c("prediction", "se", "Stations", "Admin"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      # fullscreen button
      addFullscreenControl() %>%
      # location button
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Locate Me",
        onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      htmlwidgets::onRender(paste0("
        function(el, x) {
        $('head').append(",responsiveness.chr,");
        }")
    ) %>%
      # predictions
      addPolygons(
        group = "prediction",
        color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.8,
        opacity = 1.0, fillOpacity = 0.9,
        fillColor = ~varPal(response),
        highlightOptions = highlightOptions(color = "white", weight = 2,
          bringToFront = TRUE),
        label = ~htmltools::htmlEscape(as.character(response))
      ) %>%
      addLegend(
        position = "bottomright", pal = varPal, values = ~response,
        title = "prediction",
        group = "prediction",
        opacity = 1
      )

    # if se.bool = TRUE
    if (!is.null(ourPred()$se)) {
      uncPal <- leaflet::colorNumeric(
        palette = alphaPal("#5af602"),
        domain = ourPred()$se,
        alpha = TRUE
      )

      prediction.map = prediction.map %>%
        addPolygons(
          group = "se",
          color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1,
          fillColor = ~uncPal(se),
          highlightOptions = highlightOptions(color = "white", weight = 2,
            bringToFront = TRUE),
          label = ~ paste("prediction:", signif(ourPred()$response, 2), "\n","se: ", signif(ourPred()$se, 2))
        ) %>%
        addLegend(
          group = "se",
          position = "bottomleft", pal = uncPal, values = ~se,
          title = "se",
          opacity = 1
        )
    }

    prediction.map = prediction.map %>%
      # admin boundaries
      addPolygons(
        data = wallonia,
        group = "Admin",
        color = "#444444", weight = 1, smoothFactor = 0.5,
        opacity = 1, fillOpacity = 0, fillColor = FALSE) %>%
      # stations location
      addCircleMarkers(
        data = ourObs(),
        group = "Stations",
        color = "black",
        weight = 2,
        fillColor = ~varPal(tsa),
        stroke = TRUE,


        fillOpacity = 1,
        label = ~htmltools::htmlEscape(as.character(tsa)))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

