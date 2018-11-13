# defining the function that create a dataset for a specific hour (mtime)
make.1h.data = function(dateTime){
  # extracting the tsa_hp1 var at stations points for current mtime
  stations.mtime = stations.dyn %>%
    # filtering for current time
    dplyr::filter(mtime == dateTime) %>%
    # adding corresponding grid px
    dplyr::left_join(
      data.frame(stations.sf)[c("sid", "px")],
      by = "sid"
    ) %>%
    # adding tsa_hp1
    dplyr::left_join(
      (grid.dyn %>%
          dplyr::filter(mtime == dateTime) %>%
          dplyr::select(c("px", "tsa_hp1"))
      ),
      by = "px"
    ) %>%
    # adding static vars
    dplyr::left_join(
      stations.static,
      by = "sid"
    ) %>%
    # removing useless px
    dplyr::select(-px) %>%
    # adding the lat and lon as projected Lambert 2008 (EPSG = 3812)
    dplyr::left_join(
      (data.frame(st_coordinates(st_transform(stations.sf, 3812))) %>%
          dplyr::bind_cols(stations.sf["sid"]) %>%
          dplyr::select(-geometry)
      ),
      by = "sid"
    ) %>%
    # removing mtime
    dplyr::select(-mtime) %>%
    # removing tsa_hp1 because missing values
    dplyr::select(-tsa_hp1) %>%
    # renaming X and Y to x and y
    dplyr::rename(x = X) %>%
    dplyr::rename(y = Y)
}
# defining the function that performs the benchmark
make.1h.bmr = function(task, dateTime, learners){
  # make bmr reproducible
  set.seed(2585)

  # ::FIXME:: coordinates ?

  # removing ens for now
  # task = dropFeatures(task, "ens")
  # removing tsa_hp1 for now
  # task = dropFeatures(task, "tsa_hp1")

  # input missing values

  # grid search for param tuning
  ctrl = makeTuneControlGrid()

  # absolute number feature selection paramset for fusing learner with the filter method
  ps = makeParamSet(makeDiscreteParam("fw.abs", values = seq_len(getTaskNFeats(task))))

  # inner resampling loop
  inner = makeResampleDesc("LOO")

  # outer resampling loop
  outer = makeResampleDesc("LOO")

  # benchmarking
  res = benchmark(
    measures = list(mae, mse, rmse, timetrain),
    tasks = task,
    learners = learners,
    resamplings = outer,
    show.info = FALSE,
    models = FALSE
  )

  bmr = list(
    res = res,
    task = task
  )

}

leafletize <- function(data.sf, borders, stations){


  # to make the map responsive
  responsiveness.chr = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"

  # Sometimes the interpolation and the stations don't have values in the same domain.
  # this lead to mapping inconsistency (transparent color for stations)
  # Thus we create a fullDomain which is a rowbinding of interpolated and original data
  fullDomain = c(data.sf$response, stations$tsa)

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
  prediction.map = leaflet::leaflet(data.sf) %>%
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
    fitBounds(sf::st_bbox(data.sf)[[1]],
      sf::st_bbox(data.sf)[[2]],
      sf::st_bbox(data.sf)[[3]],
      sf::st_bbox(data.sf)[[4]]
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
  if (!is.null(data.sf$se)) {
    uncPal <- leaflet::colorNumeric(
      palette = alphaPal("#5af602"),
      domain = data.sf$se,
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
        label = ~ paste("prediction:", signif(data.sf$response, 2), "\n","se: ", signif(data.sf$se, 2))
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
      data = borders,
      group = "Admin",
      color = "#444444", weight = 1, smoothFactor = 0.5,
      opacity = 1, fillOpacity = 0, fillColor = FALSE) %>%
    # stations location
    addCircleMarkers(
      data = stations,
      group = "Stations",
      color = "black",
      weight = 2,
      fillColor = ~varPal(tsa),
      stroke = TRUE,


      fillOpacity = 1,
      label = ~htmltools::htmlEscape(as.character(tsa)))

}
