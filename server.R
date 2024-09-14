library(shiny)
library(plotly)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(viridis)
library(shinyjs)
library(htmlwidgets)
library(ggthemes)
library(geosphere)
library(igraph)


## GLOBAL VARIABLES #############################

# Options
add_legend = TRUE
reverse_legend = TRUE
add_small_map_arrows = TRUE
edge_bundling = TRUE
num_possible_counties <- 50
old_scatterplot = FALSE

# Global vars
snapshot_window_up <- reactiveVal(FALSE)
countydata_for_download <- reactiveVal(data.frame())

## UTILITY FUNCTIONS ############################

getCountyNameFromFips <- function(fips) {
  if (is.null(fips) || fips == "NULL" || fips == "") {
    return("")
  }
  
  selected_county <- countydata0 %>%
    filter(GEOID == fips)
  
  s <- paste0(selected_county$NAMELSAD, ", ", selected_county$state)
  return(s)
}

getCountyNameFromFips <- Vectorize(getCountyNameFromFips)

getArrowCoordinates <- function(county_lat, county_lon, incoming=TRUE) {
  ret <- list()
  
  offset <- 0.5
  county_lat_north <- county_lat + offset
  county_lat_south <- county_lat - offset
  county_lon_east <- county_lon + offset
  county_lon_west <- county_lon - offset
  
  ret$lng_north = c(county_lon - 0.2, county_lon, county_lon, county_lon, county_lon + 0.2)
  ret$lat_north = c(county_lat_north + 0.3, county_lat_north + 0.5, county_lat_north, county_lat_north + 0.5, county_lat_north + 0.3)
  
  ret$lng_south = c(county_lon - 0.2, county_lon, county_lon, county_lon, county_lon + 0.2)
  ret$lat_south = c(county_lat_south - 0.3, county_lat_south - 0.5, county_lat_south, county_lat_south - 0.5, county_lat_south - 0.3)
  
  ret$lng_east = c(county_lon_east + 0.3, county_lon_east + 0.5, county_lon_east, county_lon_east + 0.5, county_lon_east + 0.3)
  ret$lat_east = c(county_lat - 0.2, county_lat, county_lat, county_lat, county_lat + 0.2)
  
  ret$lng_west = c(county_lon_west - 0.3, county_lon_west - 0.5, county_lon_west, county_lon_west - 0.5, county_lon_west - 0.3)
  ret$lat_west = c(county_lat - 0.2, county_lat, county_lat, county_lat, county_lat + 0.2)
  
  if (incoming) {
    dummy <- ret$lat_north
    ret$lat_north = ret$lat_north - (0.5 + 2*offset)
    ret$lat_south = ret$lat_south + (0.5 + 2*offset)
    ret$lng_east = ret$lng_east - (0.5 + 2*offset)
    ret$lng_west = ret$lng_west + (0.5 + 2*offset)
  }
  
  ret
}

getCountyArrowCoordinates <- function(lat_A, lon_A, lat_B, lon_B,
                                      incoming=TRUE, arrow_mid=TRUE,
                                      adjacent=FALSE) {
  if (incoming) {
    lat_start <- lat_A
    lon_start <- lon_A
    lat_end <- lat_B
    lon_end <- lon_B
  } else {
    lat_start <- lat_B
    lon_start <- lon_B
    lat_end <- lat_A
    lon_end <- lon_A
  }
  
  lat_diff <- lat_end - lat_start
  lon_diff <- lon_end - lon_start
  
  if (arrow_mid) {
    lat_mid <- (lat_start + lat_end) / 2
    lon_mid <- (lon_start + lon_end) / 2
  }
  
  ret <- list()
  ret$lng_east = c(0 + 0.3, 0 + 0.5, 0 + 0.3) - 0.5
  ret$lat_east = c(0 - 0.2, 0, 0 + 0.2)
  
  if (adjacent) {
    arrow_size_constant <- 2
  } else {
    arrow_size_constant <- 1
  }
  
  ret$lng_east = ret$lng_east / arrow_size_constant
  ret$lat_east = ret$lat_east / arrow_size_constant
  
  angle <- atan2(lat_diff, lon_diff)
  
  ret2 <- list()
  ret2$lng_east <- c()
  ret2$lat_east <- c()
  
  for (i in seq_along(ret$lng_east)) {
    x <- ret$lng_east[i]
    y <- ret$lat_east[i]
    
    ret2$lng_east <- c(ret2$lng_east, -(x*cos(angle) - y*sin(angle)))
    ret2$lat_east <- c(ret2$lat_east, -(x*sin(angle) + y*cos(angle)))
  }
  if (arrow_mid) {
    ret2$lng_east <- ret2$lng_east + lon_mid
    ret2$lat_east <- ret2$lat_east + lat_mid
  } else {
    ret2$lng_east <- ret2$lng_east + lon_start	
    ret2$lat_east <- ret2$lat_east + lat_start
  }
  # print(ret2)
  final <- list()
  final$lng_east <- c()
  final$lat_east <- c()
  
  for (i in seq(5)) {
    if (i == 1 || i == 2) {
      final$lng_east[i] <- ret2$lng_east[i]
      final$lat_east[i] <- ret2$lat_east[i]
    } else if (i == 3) {
      final$lng_east[i] <- lon_end
      final$lat_east[i] <- lat_end
    } else if (i == 4) {
      final$lng_east[i] <- final$lng_east[2]
      final$lat_east[i] <- final$lat_east[2]
    } else if (i == 5) {
      final$lng_east[i] <- ret2$lng_east[3]
      final$lat_east[i] <- ret2$lat_east[3]
    }
  }
  if (arrow_mid) {
    final$lng_east <- c(final$lng_east, final$lng_east[2], lon_start)
    final$lat_east <- c(final$lat_east, final$lat_east[2], lat_start)
  }
  # print(ret2)
  # print(final)
  final
}

getArrowCoordinates3 <- function(lat_start, lon_start, lat_end, lon_end) {
  lat_diff <- lat_end - lat_start
  lon_diff <- lon_end - lon_start
  
  ret <- list()
  ret$lng_east = c(0 + 0.3, 0 + 0.5, 0 + 0.3) - 0.5
  ret$lat_east = c(0 - 0.2, 0, 0 + 0.2)
  
  angle <- atan2(lat_diff, lon_diff)
  
  ret2 <- list()
  ret2$lng_east <- c()
  ret2$lat_east <- c()
  
  for (i in seq_along(ret$lng_east)) {
    x <- ret$lng_east[i]
    y <- ret$lat_east[i]
    
    ret2$lng_east <- c(ret2$lng_east, -(x*cos(angle) - y*sin(angle)))
    ret2$lat_east <- c(ret2$lat_east, -(x*sin(angle) + y*cos(angle)))
  }
  ret2$lng_east <- ret2$lng_east + lon_start
  ret2$lat_east <- ret2$lat_east + lat_start
  # print(ret2)
  final <- list()
  final$lng_east <- c()
  final$lat_east <- c()
  
  suppressWarnings({
    thing <- gcIntermediate(c(lon_start,lat_start),
                            c(lon_end,lat_end),
                            n=100, 
                            addStartEnd=TRUE,
                            sp=TRUE)
    
  })
  lng = thing@lines[[1]]@Lines[[1]]@coords[,1]
  lat = thing@lines[[1]]@Lines[[1]]@coords[,2]
  
  final$lng_east <- c(ret2$lng_east[1],
                      ret2$lng_east[2],
                      lng,
                      rev(lng),
                      ret2$lng_east[2],
                      ret2$lng_east[3])
  final$lat_east <- c(ret2$lat_east[1],
                      ret2$lat_east[2],
                      lat,
                      rev(lat),
                      ret2$lat_east[2],
                      ret2$lat_east[3])
  
  # for (i in seq(5)) {
  #   if (i == 1 || i == 2) {
  #     final$lng_east[i] <- ret2$lng_east[i]
  #     final$lat_east[i] <- ret2$lat_east[i]
  #   } else if (i == 3) {
  #     final$lng_east[i] <- lon_end
  #     final$lat_east[i] <- lat_end
  #   } else if (i == 4) {
  #     final$lng_east[i] <- final$lng_east[2]
  #     final$lat_east[i] <- final$lat_east[2]
  #   } else if (i == 5) {
  #     final$lng_east[i] <- ret2$lng_east[3]
  #     final$lat_east[i] <- ret2$lat_east[3]
  #   }
  # }
  # print(ret2)
  # print(final)
  final
}

getCountyGroupsForArrows <- function(codes, selectedCounty,
                               bundle=TRUE,
                               bundleAdjacentToSelected=FALSE) {
  # codes <- readRDS("test.rds")
  
  if (!bundleAdjacentToSelected) {
    adjacency <- adjacency  %>%
      filter(fipscounty != fipsneighbor,
             fipscounty %in% codes & fipsneighbor %in% codes,
             substr(fipscounty, 0, 2) == substr(fipsneighbor, 0, 2))
  } else {
    adjacency <- adjacency  %>%
      filter(fipscounty != fipsneighbor,
             fipscounty %in% codes & fipsneighbor %in% codes,
             substr(fipscounty, 0, 2) == substr(fipsneighbor, 0, 2),
             fipscounty != selectedCounty,
             fipsneighbor != selectedCounty)
  }
  
  if (nrow(adjacency) == 0) {
    x <- list()
    for (i in seq_along(codes)) {
      code <- codes[i]
      if (code != selectedCounty) {
        x[i] <- code
      }
    }
    return(x)
  }
  
  g <- make_empty_graph(directed = FALSE) %>%
    add_vertices(length(codes))
  
  V(g)$name <- c(codes)
  
  if (bundle) {
    g <- g %>% add_edges(as.vector(t(adjacency)))
  }
  #g
  
  x <- data.frame(clusters(g)$membership)
  
  y <- clusters(g)
  
  z <- split(names(V(g)), components(g)$membership)
  
  if (!bundleAdjacentToSelected) {
    name_neighbors <- NULL
    for (name in names(z)) {
      # print(z[[name]])
      if (selectedCounty %in% z[[name]]) {
        name_neighbors <- name
        break
      }
    }
    
    neighbors <- z[[name_neighbors]]
    
    neighbors <- neighbors[! neighbors %in% selectedCounty]
    
    # z$`1` <- NULL
    z[[name_neighbors]] <- NULL
    
    z <- append(z, neighbors)
  }
  
  z
  
}

getKmeansLabels <- function(vec, n_clusters, rounded=FALSE) {
  set.seed(0)
  # n_clusters <- 4
  # 
  # vec <- sample(c(14, 13, 6, 5, 4, 4, 3, 3, 2, 2, 1, 1, 1, 1))
  # print(vec)
  
  model <- kmeans(vec, n_clusters, nstart = 10)
  
  # print(model[['cluster']])
  # print(model[['tot.withinss']])
  
  centers_ordered = unname(rank(model[["centers"]]))
  clusters_clean <- c(model[['cluster']])
  
  for (cluster_rank in seq(n_clusters)) {
    i <- match(cluster_rank, centers_ordered)
    cluster_i_vals <- vec[model[["cluster"]] == i]
    if (rounded) {
      cluster_i_vals = round(cluster_i_vals, digits = 1)
    }
    
    if (cluster_rank == 1) {
      cluster_i_label <- paste(min(cluster_i_vals),
                               max(cluster_i_vals),
                               sep = " – ")
    } else {
      i_1 <- match(cluster_rank - 1, centers_ordered)
      cluster_i_1_vals <- vec[model[["cluster"]] == i_1]
      if (rounded) {
        cluster_i_1_vals = round(cluster_i_1_vals, digits = 1)
      }
      
      cluster_i_label <- paste(max(cluster_i_1_vals)+1,
                               max(cluster_i_vals),
                               sep = " – ")
    }
    clusters_clean[clusters_clean == i] <- cluster_i_label
  }

  return(clusters_clean)
}

## SERVER SESSION #################################

function(input, output, session) {
  
  # Either "Individual Mode", or attribute of interest
  select_by <- reactive({
    if (input$choose_by == "individual") {
      input$choose_by
    } else {
      input$select_by
    }
  })
  
  observe({
    choose_by <- input$choose_by
    restoreScatterplot()
  })
  
  # Blah
  reset_color_selection <- reactiveVal()
  reset_in_out_selection <- reactiveVal()
  
  # Indicates if we are getting details on one county in attribute mode
  details_mode_bool <- reactiveVal(FALSE)
  
  # Current clicked county, if any
  clicked_county <- reactiveVal()
  
  # Small map title
  small_map_title <- reactiveVal("")
  
  # On map shape click, freeze map click
  observeEvent(input$map_shape_click, {
    freezeReactiveValue(input, 'map_click')
  })
  
  # Set details_mode_bool and clicked_county
  observe({
    event <- input$map_shape_click
    # print(event)
    
    # By default, try to show top 50 for newly clicked county
    # Logic later in the code will deal with edge cases
    if (!is.null(event) && is.null(event$example)) {
      delay(250, {
        updateNumericInput(session, "new_num_counties",
                           value=50)
      })
    }
    
    sel_by <- select_by()#input$select_by
    # ## NO DETAILS MODE FOR NOVICES
    # if (sel_by != "individual" && is.null(event$example)) {
    #   runjs("Shiny.setInputValue('map_shape_click', null);")
    #   return()
    # }
    # 
    # print(isolate(details_mode_bool()))
    ## CLICKED COUNTY
    if (is.null(event) || !isolate(details_mode_bool()) || !is.null(event$example)) {
      clicked_county(event)
      hideSnapshotsPanel()
      restoreScatterplot()
    }
    # print(clicked_county)
    
    ## DETAILS MODE
    if (sel_by != "individual" && !is.null(event)) {
      details_mode_bool(TRUE)
      runjs("Shiny.setInputValue('details_mode', true);")
    } else {
      details_mode_bool(FALSE)
      runjs("Shiny.setInputValue('details_mode', null);")
    }
    
    ## CLEAR EXAMPLE PANEL
    if (!is.null(event) && is.null(event$example)) {
      runjs("Shiny.onInputChange('example_panel', null);")
    }
    
  })
  
  # Indicates if we are looking at incoming or outgoing flow
  incoming_bool <- reactive({
    incoming <- grepl("incoming", input$in_out) # == "incoming"
    if (details_mode_bool()) {
      incoming <- !incoming
    }
    incoming
  })
  
  # NULL when nothing hovered, otherwise same as input$map_shape_mouseover
  map_shape_hover <- reactiveVal()
  
  # On mouseover, update map_shape_hover to what was hovered
  observe({
    mouseover <- input$map_shape_mouseover
    curr_hover <- isolate(map_shape_hover())
    if (is.null(curr_hover)) {
      map_shape_hover(mouseover)
    }
  })
  
  # On mouseout, update map_shape_hover to NULL
  observe({
    input$map_shape_mouseout
    map_shape_hover(NULL)
  })
  
  ## OBSERVE CLICK EVENTS ##########################
  
  event_select_helper <- reactive({
    event <- clicked_county()#input$map_shape_click
    selectBy <- select_by()#input$select_by
    
    list(event = event, selectBy = selectBy)
  })
  
  # Set 'no_selection' event if we are in individual mode with no click
  observe({
    event_select_helper_d <- event_select_helper %>% debounce(200)
    event_select_list <- event_select_helper_d()
    event <- event_select_list$event#clicked_county()#input$map_shape_click
    selectBy <- event_select_list$selectBy#select_by()#input$select_by
    # print(event)
    # print(selectBy)
    sel <- isolate(input$color)
    if (!is.null(isolate(reset_color_selection()))) {
      sel <- isolate(reset_color_selection())
      reset_color_selection(NULL)
    }

    if (is.null(event) && selectBy == "individual") {
      runjs("Shiny.setInputValue('no_selection', true);")
      runjs("Shiny.setInputValue('example_panel', null);")
      # print("HERE")
      if (sel == "migrants_bin") {
        sel <- "per_dem_2020"
      }
      ## freezeReactiveValue(input, "color")
      updateSelectInput(session = session, "color",
                        choices = color_vars, selected = sel)
      delay(250, {
        runjs("var x = $('#color').siblings('.selectize-control').children('.selectize-input')[0]; if (x !== undefined) { console.log(x); x.addEventListener('click', function() { var y = $('#color').siblings('.selectize-control').children('.selectize-dropdown')[0]; y.scrollIntoView({ behavior: 'smooth' }); })};")
      })
    } else {
      runjs("Shiny.setInputValue('no_selection', null);")
      # sel <- isolate(input$color)
      # if (isolate(reset_color_selection())) {
      #   sel <- "per_dem_2020"
      #   print("**here**")
      # }
      if (!(sel %in% color_vars_selection)) {
        sel <- "per_dem_2020"
      }
      updateSelectInput(session = session, "color",
                        choices = color_vars_selection, selected = sel)
      delay(250, {
        runjs("var x = $('#color').siblings('.selectize-control').children('.selectize-input')[0]; if (x !== undefined) { console.log(x); x.addEventListener('click', function() { var y = $('#color').siblings('.selectize-control').children('.selectize-dropdown')[0]; y.scrollIntoView({ behavior: 'smooth' }); })};")
      })
    }
  })
  
  deselectAll <- function(hide_example_panel=TRUE) {
    # print("HERE")
    # print(hide_example_panel)
    runjs("Shiny.onInputChange('myEvent', 'close');")
    runjs("Shiny.setInputValue('map_shape_click', null);")
    if (hide_example_panel) {
      runjs("Shiny.onInputChange('example_panel', null);")
    }

    map_shape_hover(NULL)
    
    sel = isolate(input$in_out)
    if (!is.null(isolate(reset_in_out_selection()))) {
      sel <- isolate(reset_in_out_selection())
      reset_in_out_selection(NULL)
    }
    updateSelectInput(session = session, "in_out",
                      #choices = flow_vars_agg,
                      selected = sel)
  }
  
  # Observe map click (not shape click) and respond accordingly
  observeEvent(input$map_click, {
    deselectAll()
    return()
  })
  
  # Observe county shape click
  observeEvent(clicked_county(), {
    event <- clicked_county()#input$map_shape_click
    if (is.null(event$id)) {
      return()
    }
    # print(event)
    runjs("Shiny.onInputChange('myEvent', 'open');")
    if (is.null(event$example)) {
      sel <- isolate(input$in_out)
    } else {
      sel <- "net_outgoing"
    }
    if (!is.null(isolate(reset_in_out_selection()))) {
      sel <- isolate(reset_in_out_selection())
      reset_in_out_selection(NULL)
    }
    
    # sel = isolate(input$in_out)
    updateSelectInput(session = session, "in_out",
                      #choices = flow_vars_one,
                      selected = sel)
    isolate({
      updateText(event$id)
    })
  })
  
  # Observe demo button clicks
  initializeDemo <- function() {
    reset_color_selection("per_dem_2020")
    reset_in_out_selection("incoming")
    runjs("Shiny.onInputChange('help_skip', 'yes');")
    runjs("Shiny.onInputChange('demo_done', null);")
    runjs("Shiny.onInputChange('example_panel', null);")
    runjs("Shiny.onInputChange('myEvent', 'close');")
    runjs("Shiny.setInputValue('map_shape_click', null);")
    shinyjs::hide(id="snapshot_submission_panel")
    hideSnapshotsPanel()
    clicked_county(NULL)
    if (isolate(demo_idx() != 1)) {
      demo_idx(1)
    }
    shinyjs::disable(id="demo_back")
    
    ## Reset sliders to defaults
    updateSelectInput(session = session,
                      "view", selected = "counties")
    updateRadioButtons(session = session,
                       "choose_by", selected = "individual")
    updateSelectInput(session = session,
                      "select_by", selected = "gop_pct_20")
    # updateSliderInput(session = session,
    #                   "gop_pct_20", value = c(50, 65))
    updateNumericInput(session = session,
                       "gop_pct_20_A", value = 50)
    updateNumericInput(session = session,
                       "gop_pct_20_B", value = 65)
    updateSelectInput(session = session,
                      "in_out", selected = "incoming")
    # delay() stops slider value oscillation
    delay(25, {
      # updateSliderInput(session = session,
      #                   "num", value = 50)
      updateNumericInput(session = session,
                         "new_num_counties", value = 50)
    })
    delay(50, {
      updateSelectInput(session = session,
                        "color", selected = "per_dem_2020")
    })
  }
  
  observeEvent(input$demo, {
    runjs("Shiny.onInputChange('demo_show', 'yes');")
    runjs("resetDemoLocation()")
    initializeDemo()
  })
  
  observeEvent(input$demo_close,{
    runjs("Shiny.onInputChange('demo_show', null);")
  })
  
  # observeEvent(input$demo_skip,{
  #   runjs("Shiny.onInputChange('demo_show', null);")
  # })
  
  observeEvent(input$demo_end,{
    runjs("Shiny.onInputChange('demo_show', null);")
  })
  
  # Observe About button clicks
  observeEvent(input$about, {
    shinyjs::hide(id="snapshot_submission_panel")
    runjs("Shiny.onInputChange('demo_show', null);")
    runjs("Shiny.onInputChange('help_skip', 'yes');")
    runjs("Shiny.onInputChange('help_done', null);")
    runjs("Shiny.onInputChange('example_panel', null);")

    shinyjs::show(id="about_panel")
  })
  
  observeEvent(input$about_close, {
    shinyjs::hide(id="about_panel")
  })
  
  observeEvent(input$about_close_alt, {
    shinyjs::click(id="about_close")
  })
  
  # Observe Help button clicks
  # observeEvent(input$help_skip,{
  #   runjs("Shiny.onInputChange('help_skip', 'yes');")
  # })
  
  observeEvent(input$help_end,{
    runjs("Shiny.onInputChange('help_skip', 'yes');")
  })
  
  observeEvent(input$help_close, {
    runjs("Shiny.onInputChange('help_skip', 'yes');")
  })
  
  initializeHelp <- function() {
    shinyjs::hide(id="snapshot_submission_panel")
    hideSnapshotsPanel()
    if (isolate(input$color) != "per_dem_2020") {
      reset_color_selection("per_dem_2020")
    }
    runjs("Shiny.onInputChange('demo_show', null);")
    runjs("Shiny.onInputChange('help_skip', null);")
    runjs("Shiny.onInputChange('help_done', null);")
    runjs("Shiny.onInputChange('example_panel', null);")
    shinyjs::disable(id="help_back")
    if (isolate(help_idx()) != 1) {
      help_idx(1)
    }
    
    ## Reset sliders to defaults
    updateSelectInput(session = session,
                      "color", selected = "per_dem_2020")
    
    # delay() stops slider value oscillation
    delay(50, {
      updateRadioButtons(session = session,
                         "choose_by", selected = "individual")
      clicked_county(NULL)
    })
    
    runjs("Shiny.onInputChange('myEvent', 'close');")
    runjs("Shiny.setInputValue('map_shape_click', null);")
  }
  
  observeEvent(input$help, {
    runjs("resetHelpLocation();")
    initializeHelp()
  })
  
  # Observe Example 1 button click
  observeEvent(input$example_1, {
    output$example_name <- renderText({
      "Migration to Maricopa"
    })
    
    output$example_text <- renderText({
      example_text[1]
    })
    
    output$example_img <- renderText({
      '<img src="example1.png" style="height:34px;">'
    })
    
    runjs("Shiny.onInputChange('demo_show', null);")
    runjs("Shiny.onInputChange('help_skip', 'yes');")
    runjs("Shiny.onInputChange('example_panel', 'yes');")
    runjs("resetExampleLocation();")
    shinyjs::hide(id="snapshot_submission_panel")
    hideSnapshotsPanel()
    
    reset_color_selection("per_dem_2020")
    updateSelectInput(session = session,
                      "color", selected = "per_dem_2020")
    
    updateRadioButtons(session = session,
                       "choose_by", selected = "individual")
    
    runjs("Shiny.setInputValue('details_mode', null);")
    runjs("Shiny.onInputChange('myEvent', 'open');")
    
    event <- paste("'id': '04013'",
                   "'.nonce': '0.7399132'",
                   "'group': 'counties'",
                   "'lat': '33.49179'",
                   "'lng': '-112.7711'",
                   "'example': true",
                   sep = ", ")
    runjs(paste0("Shiny.setInputValue('map_shape_click', {", event, "});"))
    
    reset_in_out_selection("net_incoming")
    updateSelectInput(session = session,
                      "in_out", selected = "net_incoming")
    updateRadioButtons(session = session,
                       "view", selected = "counties")
    # updateRadioButtons(session = session,
    #                    "show_arrows", selected = "Yes")
    updateCheckboxInput(session = session,
                        "show_arrows_alt", value = TRUE)
    # delay() stops slider value oscillation
    # updateSliderInput(session = session,
    #                   "num", value = 50)
    updateNumericInput(session = session,
                       "new_num_counties", value = 50)
    
    # delay(800, {
    # })
    
    
  })
  
  # Observe Example 2 button click
  observeEvent(input$example_2, {
    output$example_name <- renderText({
      "Snapshot 2"
    })
    
    output$example_text <- renderText({
      example_text[2]
    })
    
    output$example_img <- renderText({
      '<img src="example2.png" style="height:34px;">'
    })
    
    runjs("Shiny.onInputChange('demo_show', null);")
    runjs("Shiny.onInputChange('help_skip', 'yes');")
    runjs("Shiny.onInputChange('example_panel', 'yes');")
    runjs("resetExampleLocation();")
    shinyjs::hide(id="snapshot_submission_panel")
    deselectAll(hide_example_panel = FALSE)
    hideSnapshotsPanel()
    clicked_county(NULL)
    
    reset_color_selection("code")
    
    updateRadioButtons(session = session,
                       "choose_by", selected = "attribute")
    updateSelectInput(session = session,
                      "select_by", selected = "code")
    # updateSliderInput(session = session,
    #                   "code_raw", value = 2)
    # updateSliderTextInput(session = session,
    #                   "code", selected = "Suburban")
    updateSelectInput(session = session,
                      "code_select", selected = "Suburban")
    reset_in_out_selection("net_incoming")
    updateSelectInput(session = session,
                      "in_out", selected = "net_incoming")
    # delay() stops slider value oscillation
    delay(80, {
      # updateSliderInput(session = session,
      #                   "num", value = 50)
      updateNumericInput(session = session,
                         "new_num_counties", value = 50)
    })
    delay(80, {
      updateSelectInput(session = session,
                        "color", selected = "code")
    })
    
  })
  
  # Observe Example 3 button click
  observeEvent(input$example_3, {
    output$example_name <- renderText({
      "Snapshot 3"
    })
    
    output$example_text <- renderText({
      example_text[3]
    })
    
    output$example_img <- renderText({
      '<img src="example3.png" style="height:34px;">'
    })
    
    reset_color_selection("per_dem_2020")
    
    runjs("Shiny.onInputChange('demo_show', null);")
    runjs("Shiny.onInputChange('help_skip', 'yes');")
    runjs("Shiny.onInputChange('example_panel', 'yes');")
    runjs("resetExampleLocation();")
    shinyjs::hide(id="snapshot_submission_panel")
    hideSnapshotsPanel()
    deselectAll(hide_example_panel = FALSE)
    clicked_county(NULL)
    
    updateRadioButtons(session = session,
                       "choose_by", selected = "attribute")
    updateSelectInput(session = session,
                      "select_by", selected = "gop_pct_20")
    # updateSliderInput(session = session,
    #                   "gop_pct_20", value = c(40, 60))
    updateNumericInput(session = session,
                       "gop_pct_20_A", value = 40)
    updateNumericInput(session = session,
                       "gop_pct_20_B", value = 60)
    reset_in_out_selection("net_outgoing")
    updateSelectInput(session = session,
                      "in_out", selected = "net_outgoing")
    # delay() stops slider value oscillation
    delay(80, {
      # updateSliderInput(session = session,
      #                   "num", value = 50)
      updateNumericInput(session = session,
                         "new_num_counties", value = 50)
    })
    delay(80, {
      updateSelectInput(session = session,
                        "color", selected = "per_dem_2020")
    })
    
  })
  
  # Observe Example 4 button click
  observeEvent(input$example_4, {
    output$example_name <- renderText({
      "Snapshot 4"
    })
    
    output$example_text <- renderText({
      example_text[4]
    })
    
    output$example_img <- renderText({
      '<img src="example4.png" style="height:34px;">'
    })
    
    reset_color_selection("hh_income")
    
    runjs("Shiny.onInputChange('demo_show', null);")
    runjs("Shiny.onInputChange('help_skip', 'yes');")
    runjs("Shiny.onInputChange('example_panel', 'yes');")
    runjs("resetExampleLocation();")
    shinyjs::hide(id="snapshot_submission_panel")
    hideSnapshotsPanel()
    deselectAll(hide_example_panel = FALSE)
    clicked_county(NULL)
    
    updateRadioButtons(session = session,
                       "choose_by", selected = "attribute")
    updateSelectInput(session = session,
                      "select_by", selected = "hh_income")
    # updateSliderInput(session = session,
    #                   "hh_income", value = c(75000, 100000))
    updateNumericInput(session = session,
                       "hh_income_A", value = 75000)
    updateNumericInput(session = session,
                       "hh_income_B", value = 100000)
    reset_in_out_selection("net_outgoing")
    updateSelectInput(session = session,
                      "in_out", selected = "net_outgoing")
    # delay() stops slider value oscillation
    delay(80, {
      # updateSliderInput(session = session,
      #                   "num", value = 50)
      updateNumericInput(session = session,
                         "new_num_counties", value = 50)
    })
    delay(80, {
      updateSelectInput(session = session,
                        "color", selected = "hh_income")
    })
    
  })
  
  # Observe Toggle Menus button click
  observeEvent(input$toggle_menus, {
    hideSnapshotsPanel()
    runjs("var x = $('#toggle_menus')[0]; var text = x.textContent; var y = $('.legend')[0]; if (text == 'Hide Menus') {Shiny.setInputValue('hide_menus', true); x.textContent = 'Show Menus'; y.classList.add('legend_noMenus');} else {Shiny.setInputValue('hide_menus', null); x.textContent = 'Hide Menus'; y.classList.remove('legend_noMenus');};")
    # runjs("Shiny.setInputValue('hide_menus', null);")
  })
  
  observeEvent(input$scatterplot_close, {
    runjs("Shiny.setInputValue('scatterplot_hidden', true);")
    # shinyjs::hide("plot2")
    shinyjs::show("scatterplot_show")
    shinyjs::addClass(selector=".legend", class="legend_noScatterplot")
    # runjs("var x = $('#toggle_scatterplot')[0]; var text = x.textContent; var y = $('.legend')[0]; if (text == 'Hide Scatterplot') {x.textContent = 'Show Scatterplot'; y.classList.add('legend_noMenus');} else {x.textContent = 'Hide Scatterplot'; y.classList.remove('legend_noMenus');};")
  })
  
  restoreScatterplot <- function() {
    runjs("Shiny.setInputValue('scatterplot_hidden', false);")
    # shinyjs::show("plot2")
    shinyjs::hide("scatterplot_show")
    shinyjs::removeClass(selector=".legend", class="legend_noScatterplot")
  }
  
  observeEvent(input$scatterplot_show, {
    restoreScatterplot()
  })
  
  observeEvent(input$example_end, {
    runjs("Shiny.onInputChange('example_panel', 'no');")
  })
  
  observeEvent(input$example_close, {
    shinyjs::click(id="example_end")
  })
  
  
  # For Demo
  demo_idx <- reactiveVal(1)
  
  observeEvent(input$demo_next,{
    demo_idx(demo_idx()+1) # increment by 1
  })
  
  observeEvent(input$demo_back,{
    demo_idx(demo_idx()-1) # increment by 1
    
    if (demo_idx() == 1) {
      initializeDemo()
    }
  })
  
  observe({
    if (demo_idx() != 1) {
      shinyjs::enable(id="demo_back")
    }
    
    if (demo_idx() == nrow(demo_data)) {
      runjs("Shiny.onInputChange('demo_done', 'yes');")
    } else {
      runjs("Shiny.onInputChange('demo_done', null);")
    }
  })
  
  observe({
    idx <- demo_idx()
    # print(idx)
    
    currData <- demo_data %>% slice(idx)
    # print(currData)
    
    if (is.null(currData)) {
      return()
    }
    
    output$demo_main_text <- renderText({
      currData$main_text
    })
    
    if (currData$question_bool) {
      shinyjs::show(id="question_radio")
    } else {
      shinyjs::hide(id="question_radio")
      output$demo_feedback <- renderText({
        ""
      })
    }
    
    set.seed(10)
    responses <- select(currData, starts_with("response_")) %>%
      unlist(use.names=FALSE) %>%
      stringi::stri_remove_empty_na() %>%
      sample()
    
    updateRadioButtons(session = session,
                       "question_radio",
                       label = currData$question_text,
                       choices = responses,
                       selected = character(0))
    
  })
  
  observe({
    idx <- demo_idx()
    # print(idx)
    
    currData <- demo_data %>% slice(idx)
    # print(currData)
    
    if (is.null(currData) ||
        is.null(input$question_radio) ||
        !currData$question_bool) {
      return()
    }
    
    test <- input$question_radio
    
    if (is.null(test)) {
      text <- ""
    } else if (test == currData$response_correct) {
      text <- currData$feedback_correct  # <br><br>
      # shinyjs::hide(id="test")
    } else {
      text <- currData$feedback_incorrect
    }
    
    output$demo_feedback <- renderText({
      text
    })
  })
  
  output$demo_pagenum <- renderText({
    paste0("(", demo_idx(), "/", nrow(demo_data), ")")
  })
  
  
  # For Help
  help_idx <- reactiveVal(1)
  
  observeEvent(input$help_next,{
    help_idx(help_idx()+1) # increment by 1
  })
  
  observeEvent(input$help_back,{
    help_idx(help_idx()-1) # increment by 1
  })
  
  observe({
    if (help_idx() == 1) {
      initializeHelp()
      # shinyjs::click(id="help")
    } else if (help_idx() == 2) { # CHANGE TO Inf if necessary
      ## Deselect anything that was...
      runjs("Shiny.onInputChange('myEvent', 'close');")
      runjs("Shiny.setInputValue('map_shape_click', null);")
      runjs("Shiny.onInputChange('help_done', null);")
      shinyjs::enable(id="help_back")
      deselectAll(hide_example_panel = FALSE)
      clicked_county(NULL)
      
      reset_color_selection("per_dem_2020")
      
      ## Reset sliders to defaults for attribute mode
      updateSelectInput(session = session,
                        "view", selected = "counties")
      updateRadioButtons(session = session,
                         "choose_by", selected = "attribute")
      updateSelectInput(session = session,
                        "select_by", selected = "gop_pct_20")
      # updateSliderInput(session = session,
      #                   "gop_pct_20", value = c(50, 65))
      updateNumericInput(session = session,
                         "gop_pct_20_A", value = 50)
      updateNumericInput(session = session,
                         "gop_pct_20_B", value = 65)
      reset_in_out_selection("incoming")
      updateSelectInput(session = session,
                        "in_out", selected = "incoming")
      # delay() stops slider value oscillation
      delay(80, {
        # updateSliderInput(session = session,
        #                   "num", value = 50)
        updateNumericInput(session = session,
                           "new_num_counties", value = 50)
      })
      delay(80, {
        updateSelectInput(session = session,
                          "color", selected = "per_dem_2020")
      })
    } else if (help_idx() == length(help_text)) {
      shinyjs::hide(id="snapshot_submission_panel")
      reset_color_selection("per_dem_2020")
      shinyjs::enable(id="help_back")
      runjs("Shiny.onInputChange('demo_show', null);")
      runjs("Shiny.onInputChange('help_skip', null);")
      runjs("Shiny.onInputChange('help_done', null);")
      runjs("Shiny.onInputChange('example_panel', null);")
      # help_idx(1)
      
      ## Reset sliders to defaults
      updateSelectInput(session = session,
                        "color", selected = "per_dem_2020")
      
      # delay() stops slider value oscillation
      delay(50, {
        updateRadioButtons(session = session,
                           "choose_by", selected = "individual")
        clicked_county(NULL)
      })
      
      runjs("Shiny.onInputChange('myEvent', 'close');")
      runjs("Shiny.setInputValue('map_shape_click', null);")
      
      runjs("Shiny.onInputChange('help_done', 'yes');")
    } 
  })
  
  output$help_text <- renderText({
    help_text[help_idx()]
  })
  
  output$help_pagenum <- renderText({
    paste0("(", help_idx(), "/", length(help_text), ")")
  })
  
  
  ## REACTIVE VARIABLES ###############################
  
  countydata_fips <- reactive({
    sel_by <- select_by()#input$select_by
    if (sel_by == "individual") {
      countydata_fips <- tibble(countydata0)
    } else if (is.null(sel_by) || grepl("gop_pct", sel_by)) {
      
      if (is.null(sel_by)) {
        gop_pct_A <- input$gop_pct_20_A
        gop_pct_B <- input$gop_pct_20_B
        year <- "20"
      } else {
        gop_pct_A <- input[[paste0(sel_by, "_A")]]
        gop_pct_B <- input[[paste0(sel_by, "_B")]]
        year <- substr(sel_by, 9, 10)
      }
      if (year == "") {
        year <- "16"
      }
      column <- paste0("per_gop_20", year)
      
      gop_pct_low <- min(gop_pct_A, gop_pct_B) / 100
      gop_pct_high <- max(gop_pct_A, gop_pct_B) / 100
      
      countydata <- countydata0#countydata()
      countydata_fips <- countydata %>%
        filter(.[[column]] >= gop_pct_low,
               .[[column]] <= gop_pct_high) #%>%
      #select(GEOID)
    } else if (sel_by == "hh_income") {
      hh_income_A <- input$hh_income_A
      hh_income_B <- input$hh_income_B
      
      hh_income_low <- min(hh_income_A, hh_income_B)
      hh_income_high <- max(hh_income_A, hh_income_B)
      
      countydata <- countydata0#countydata()
      countydata_fips <- countydata %>%
        filter(hh_income >= hh_income_low,
               hh_income <= hh_income_high) #%>%
      #select(GEOID)
    } else if (sel_by == "code") {
      # code_sel <- as.numeric(input$code_raw)
      # 
      # countydata <- countydata0#countydata()
      # countydata_fips <- countydata %>%
      #   filter(code_raw == code_sel) #%>%
      code_sel <- input$code_select

      countydata <- countydata0#countydata()
      countydata_fips <- countydata %>%
        filter(code == code_sel) #%>%
      
    } else if (sel_by == "per_degree") {
      per_degree_A <- input$per_degree_A
      per_degree_B <- input$per_degree_B
      
      per_degree_low <- min(per_degree_A, per_degree_B)
      per_degree_high <- max(per_degree_A, per_degree_B)
      
      countydata <- countydata0
      countydata_fips <- countydata %>%
        filter(per_degree >= per_degree_low,
               per_degree <= per_degree_high)
    } else {
      print("*****BUG*****")
    }
    
    countydata_fips$GEOID 
    
  })
  
  countydata <- reactive({
    # dataHandlerFinal <- dataHandler()# %>% debounce(800)
    dataHandlerList <- dataHandler()#dataHandlerFinal#()
    
    countydata = countydata0
    event <- dataHandlerList$event#clicked_county()#input$map_shape_click

    ## DETAILS MODE
    details_mode <- dataHandlerList$details_mode#details_mode_bool()
    # print(details_mode)
    
    ## RETRIEVE RELEVANT INPUT
    incoming <- dataHandlerList$incoming#incoming_bool() #grepl("incoming", input$in_out) # == "incoming"
    # if (details_mode) {
    #   incoming <- !incoming
    # }
    net <- grepl("net", dataHandlerList$in_out)
    eff <- grepl("eff", dataHandlerList$in_out)
    normalize <- dataHandlerList$norm == "Yes"
    num_top_counties <- dataHandlerList$num
    if (is.na(num_top_counties) || num_top_counties < 1) {
      num_top_counties <- 1
    }
    
    ## SET LOCAL VARIABLES TO MAKE SUBSET OF MIGRATION DATA
    if (incoming) {
      selected_county_type <- "des_county"
      other_county_type <- "ori_county"
      hist_title_word <- "origin"
    } else {
      selected_county_type <- "ori_county"
      other_county_type <- "des_county"
      hist_title_word <- "destination"
    }
    pop_type <- paste0(other_county_type, "_pop")
    
    
    ## MAKE SUBSET OF MIGRATION DATA
    if (net) {
      migration_data$sort_variable <- migration_data$exemptions_net
    } else if (eff) {
      migration_data$sort_variable <- migration_data$efficiency
    } else {
      migration_data$sort_variable <- migration_data$exemptions
    }
    # if (normalize) {
    #   migration_data$sort_variable = migration_data$sort_variable / migration_data[[pop_type]] * 1000
    # }
    ## Attribute mode, not details mode
    if (is.null(event) || is.null(event$id)) {
      countydata_fips = dataHandlerList$countydata_fips
      
      migration_data <- migration_data %>%
        mutate(indicator = .[[selected_county_type]] %in% countydata_fips)
      
      migration_data_agg <- migration_data %>%
        group_by(indicator, .data[[other_county_type]]) %>%
        summarize(exemptions = sum(exemptions),
                  exemptions_net = sum(exemptions_net),
                  total = sum(total),
                  .groups="keep") %>%
        mutate(efficiency = exemptions_net / total * 100) %>%
        filter(indicator == TRUE) %>%
        ungroup()
      
      if (eff) {
        migration_data_agg <- migration_data_agg %>%
          mutate(sort_variable = efficiency) %>%
          filter(total >= 100 * sqrt(length(countydata_fips)))
      } else if (net) {
        migration_data_agg <- migration_data_agg %>%
          mutate(sort_variable = exemptions_net)
      } else {
        migration_data_agg <- migration_data_agg %>%
          mutate(sort_variable = exemptions)
      }

      migration_data_intermediate <- migration_data_agg %>%
        filter(sort_variable > 0) %>%
        arrange(desc(sort_variable)) 
    } else if (details_mode) {
      fips_of_interest <- event$id
      
      migration_data_intermediate <- migration_data %>%
        filter(.[[selected_county_type]] == fips_of_interest,
               .[[other_county_type]] %in% dataHandlerList$countydata_fips,
               sort_variable > 0) %>%
        arrange(desc(sort_variable))
    } else {
      fips_of_interest <- event$id
      
      migration_data_intermediate <- migration_data %>%
        filter(.[[selected_county_type]] == fips_of_interest,
               sort_variable > 0) %>%
        arrange(desc(sort_variable))
    }
    
    if (eff) {
      migration_data_intermediate <- migration_data_intermediate %>%
        filter(total >= 100)
    }
    
    countydata_full <- inner_join(countydata0, migration_data_intermediate,
                                  by = c("GEOID"=other_county_type)) %>%
      arrange(desc(sort_variable))
    countydata_for_download(countydata_full)
      
    num_possible_counties <- nrow(countydata_full)
    ## UPDATE SLIDER
    if (dataHandlerList$num > num_possible_counties) {
      val <- num_possible_counties
      # updateSliderInput(session, "num",
      #                   min=1, max=num_possible_counties,
      #                   value=val)
      updateNumericInput(session, "new_num_counties",
                         min=1, max=num_possible_counties,
                         value=val)
      
    } else {
      val <- dataHandlerList$num
      # updateSliderInput(session, "num",
      #                   min=1, max=num_possible_counties)
      updateNumericInput(session, "new_num_counties",
                         min=1, max=num_possible_counties)
      
    }
    output$max_num_counties <- renderText({
      paste0("(of ", num_possible_counties, ") ")
    })

    ## Continue with getting data to display    
    countydata <- countydata_full %>%
      slice_head(n=num_top_counties)
    
    if (nrow(countydata) > 0) {
      countydata <- countydata %>%
        mutate(idx = 1:nrow(.))
    }
    
    if (eff) {
      num_clusters <- min(5, length(unique(countydata$sort_variable))-1)
    } else {
      num_clusters <- min(5, nrow(countydata)-1)
    }
    if (num_clusters < 1) {
      countydata$migrants_bin <- factor(countydata$sort_variable,
                                        ordered = TRUE)
    } else {
      labels <- getKmeansLabels(countydata$sort_variable, num_clusters, rounded=eff)
      countydata$migrants_bin <- factor(labels,
                                        ordered = TRUE,
                                        levels = rev(gtools::mixedsort(unique(labels))))
    }
    
    countydata
  })
  
  # Update small map title
  observe({
    event <- clicked_county()
    
    sel_by <- select_by()
    if (sel_by == "individual" & !(is.null(event))) {
      selected_county <- countydata0 %>%
        filter(GEOID == event$id)
      text <- paste0("<b>",
                     selected_county$NAMELSAD, 
                     ", ", selected_county$state,
                     "</b>")
      
    } else if (grepl("gop_pct", sel_by)) {
      # gop_pct <- input[[sel_by]]
      gop_pct_A <- input[[paste0(sel_by, "_A")]]
      gop_pct_B <- input[[paste0(sel_by, "_B")]]
      
      year <- substr(sel_by, 9, 10)
      if (year == "") {
        year = "16"
      }
      
      gop_pct_low <- min(gop_pct_A, gop_pct_B)
      gop_pct_high <- max(gop_pct_A, gop_pct_B)
      
      # text <- paste0("counties that voted between ", gop_pct_low, "%",
      #               " and ", gop_pct_high, "% Republican in 20", year)
      text <- paste0(gop_pct_low, "% to ",
                     gop_pct_high, "% Republican counties:")
      
    } else if (sel_by == "hh_income") {
      hh_income_A <- input$hh_income_A
      hh_income_B <- input$hh_income_B
      
      hh_income_low <- format(min(hh_income_A, hh_income_B),
                              big.mark=",", trim=TRUE)
      hh_income_high <- format(max(hh_income_A, hh_income_B),
                               big.mark=",", trim=TRUE)
      
      # text <- paste0("counties with a median household income between ",
      #               "$", hh_income_low, " and $", hh_income_high)
      text <- paste0("$", hh_income_low, " to $", hh_income_high, " median income counties:")
      
    } else if (sel_by == "code") {
      code_sel <- input$code_select
      
      countydata <- countydata0#countydata()
      countydata_fips <- countydata %>%
        filter(code == code_sel) #%>%
      
      text <- paste0(code_sel, " counties:")
      
    } else if (sel_by == "per_degree") {
      per_degree_A <- input$per_degree_A
      per_degree_B <- input$per_degree_B
      
      per_degree_low <- min(per_degree_A, per_degree_B)
      per_degree_high <- max(per_degree_A, per_degree_B)
      
      # text <- paste0("counties where between ",
      #               per_degree_low, "% and ", per_degree_high,
      #               "% of residents have a college degree")
      text <- paste0(per_degree_low, "% to ", per_degree_high,
                     "% college-educated counties:")
      
    } else {
      # print("*****BUG*****")
      text <- ""
    }
    # print("***TEXT:***")
    # print(text)
    output$small_map_title <- renderText({
      text
    })
    small_map_title(stringr::str_sub(text, end=-2))
  })
  
  # output$small_map_title <- renderText({
  #   getSmallMapTitleHelper()
  # })
  
  getDescriptiveText <- function() {
    event <- isolate(clicked_county())
    sizeBy <- isolate(input$size)
    selectBy <- isolate(select_by())
    
    # num_top_counties <- isolate(input$num)
    num_top_counties <- isolate(input$new_num_counties)
    if (is.na(num_top_counties) || num_top_counties < 1) {
      num_top_counties <- 1
    }
    
    countydata <- isolate(countydata())
    
    #### DESCRIPTIVE TEXT
    incoming <- incoming_bool()#grepl("incoming", isolate(input$in_out))
    if (incoming) {
      direction <- "into"
      direction_sidebar <- "from"
    } else {
      direction <- "from"
      direction_sidebar <- "into"
    }
    net <- grepl("net", isolate(input$in_out))
    if (net) {
      net_infix <- "net migration "
      prefix_sidebar <- "Net migration"
    } else {
      net_infix <- " migrants "
      prefix_sidebar <- "Migration"
    }
    details_mode <- isolate(details_mode_bool())
    
    if (selectBy == "individual" & is.null(event)) {
      content <- "Select a county."
    } else if (!is.null(event) && !(details_mode)) {
      selected_county <- countydata0 %>%
        filter(GEOID == event$id)
      # View(selected_county)
      content <- paste0("<b>Current view:</b> ",
                        "Top ", num_top_counties, " counties",
                        " with the most ", net_infix,# "migrants ",
                        direction, " ", selected_county$NAMELSAD, 
                        ", ", selected_county$state)
    } else if (!is.null(event) && details_mode) {
      selected_county <- countydata0 %>%
        filter(GEOID == event$id)
      # View(selected_county)
      content <- paste0("<b>Current view:</b> ",
                        "Top ", num_top_counties, " ",
                        getDescriptiveTextHelper(),
                        " with the most ", net_infix,# "migrants ",
                        direction,  " ",
                        selected_county$NAMELSAD, 
                        ", ", selected_county$state)
    } else {
      content <- paste0("<b>Current view:</b> ",
                        "Top ", num_top_counties, " counties",
                        " with the most ", net_infix,# "migrants ",
                        direction, " ", getDescriptiveTextHelper())
    }
    ## Only in details mode
    # output$details_mode_sidebar_text <- renderText({
    #   paste0(prefix_sidebar, " ", direction_sidebar,
    #          " ", getDescriptiveTextHelper())
    output$small_map_title0 <- renderText({
      paste0(getMapTitleHelper())
    })
    ##
    if (!content == "Select a county." &&
        !details_mode &&
        !isolate(input$color) %in% c("pct_black")) {
      # content <- paste0(content, "<br>",
      #                   "<b>Insight:</b> ", getInsightHelper())
      content <- paste0("<b>Insight:</b> ", getInsightHelper())
      
    }
    
    output$statusText <- renderText({getDescriptiveTextHelper()})
    
    content
  }
  
  getDescriptiveTextHelper <- function() {
    event <- isolate(clicked_county())
    
    sel_by <- isolate(select_by())
    if (sel_by == "individual" & !(is.null(event))) {
      selected_county <- countydata0 %>%
        filter(GEOID == event$id)
      text <- paste0("<b>",
                     selected_county$NAMELSAD, 
                     ", ", selected_county$state,
                     "</b>")
      
      return(text)
    } else if (grepl("gop_pct", sel_by)) {
      # gop_pct <- input[[sel_by]]
      gop_pct_A <- input[[paste0(sel_by, "_A")]]
      gop_pct_B <- input[[paste0(sel_by, "_B")]]
      
      year <- substr(sel_by, 9, 10)
      if (year == "") {
        year = "16"
      }
      
      gop_pct_low <- min(gop_pct_A, gop_pct_B)
      gop_pct_high <- max(gop_pct_A, gop_pct_B)
      
      return(paste0("counties that voted between ", gop_pct_low, "%",
             " and ", gop_pct_high, "% Republican in 20", year))
      
    } else if (sel_by == "hh_income") {
      hh_income_A <- input$hh_income_A
      hh_income_B <- input$hh_income_B
      
      hh_income_low <- format(min(hh_income_A, hh_income_B),
                              big.mark=",", trim=TRUE)
      hh_income_high <- format(max(hh_income_A, hh_income_B),
                               big.mark=",", trim=TRUE)
      
      return(paste0("counties with a median household income between ",
             "$", hh_income_low, " and $", hh_income_high))
      
    } else if (sel_by == "code") {
      code_sel <- input$code_select
      
      countydata <- countydata0#countydata()
      countydata_fips <- countydata %>%
        filter(code == code_sel) #%>%
      
      return(paste0(tolower(code_sel), " counties"))
      
    } else if (sel_by == "per_degree") {
      per_degree_A <- input$per_degree_A
      per_degree_B <- input$per_degree_B
      
      per_degree_low <- min(per_degree_A, per_degree_B)
      per_degree_high <- max(per_degree_A, per_degree_B)
      
      return(paste0("counties where between ",
                    per_degree_low, "% and ", per_degree_high,
                    "% of residents have a college degree"))
    } else {
      # print("*****BUG*****")
      return("")
    }
    
}
  
  getMapTitleHelper <- function() {
    event <- isolate(clicked_county())
    
    sel_by <- isolate(select_by())
    if (sel_by == "individual" & !(is.null(event))) {
      return("Counties with Attribute")
    } else if (grepl("gop_pct", sel_by)) {
      # gop_pct <- input[[sel_by]]
      gop_pct_A <- input[[paste0(sel_by, "_A")]]
      gop_pct_B <- input[[paste0(sel_by, "_B")]]
      
      year <- substr(sel_by, 9, 10)
      if (year == "") {
        year = "16"
      }
      
      gop_pct_low <- min(gop_pct_A, gop_pct_B)
      gop_pct_high <- max(gop_pct_A, gop_pct_B)
      
      return(paste0("Counties that voted ", gop_pct_low, "%",
                    "-", gop_pct_high, "% Republican in 20", year, ":"))
      
    } else if (sel_by == "hh_income") {
      hh_income_A <- input$hh_income_A
      hh_income_B <- input$hh_income_B
      
      hh_income_low <- format(min(hh_income_A, hh_income_B),
                              big.mark=",", trim=TRUE)
      hh_income_high <- format(max(hh_income_A, hh_income_B),
                               big.mark=",", trim=TRUE)
      
      return(paste0("Counties with median incomes between ",
                    "$", hh_income_low, " and $", hh_income_high, ":"))
      
    } else if (sel_by == "code") {
      code_sel <- input$code_select
      
      countydata <- countydata0#countydata()
      countydata_fips <- countydata %>%
        filter(code == code_sel) #%>%
      
      return(paste0(code_sel, " counties", ":"))
      
    } else if (sel_by == "per_degree") {
      per_degree_A <- input$per_degree_A
      per_degree_B <- input$per_degree_B
      
      per_degree_low <- min(per_degree_A, per_degree_B)
      per_degree_high <- max(per_degree_A, per_degree_B)
      
      return(paste0("Counties where ",
                    per_degree_low, "%-", per_degree_high,
                    "% of residents have a college degree", ":"))
    } else {
      # print("*****BUG*****")
      return("")
    }
    
  }
  
  getInsightHelper <- function() {
    dataHandler_d <- dataHandler %>% debounce(500)
    dataHandlerList <- dataHandler_d()

    countydata_d <- countydata %>% debounce(500)
    countydata <- countydata_d()#dataHandlerList$countydata
    
    colorBy <- dataHandlerList$color #"per_dem_2016"
    incoming <- dataHandlerList$incoming
    num <- dataHandlerList$num_top_counties
    if (incoming) {
      direction <- "incoming"
      to_from <- "from"
      cont_rec <- " accounted for "
    } else {
      direction <- "outgoing"
      to_from <- "to"
      cont_rec <- " received "
    }
    # net <- grepl("net", dataHandlerList$in_out)
    # if (net) {
    #   net_infix <- "net "
    # } else {
    #   net_infix <- ""
    # }
    
    
    if (colorBy %in% c("per_dem_2016", "per_dem_2020")) {
      year <- substr(colorBy, 9, 12)
      # View(countydata)
      countydata_agg <- countydata %>%
        sf::st_drop_geometry() %>%
        mutate(voted_dem = .[[colorBy]] > .[[stringr::str_replace(colorBy, 
                                                                  "dem", 
                                                                  "gop")]]) %>%
        group_by(voted_dem) %>%
        summarize(tot = sum(sort_variable))
      # View(countydata_agg)
      pct_flow_dem <- countydata_agg %>%
        filter(voted_dem == TRUE) %>%
        select(tot) %>%
        unlist(use.names = FALSE) / sum(countydata$sort_variable)
      # print(pct_flow_dem)
      if (length(pct_flow_dem) == 0) {
        pct_flow_dem = 0
      }
      if (pct_flow_dem > 0.5) {
        pct_flow <- round(pct_flow_dem * 100, digits = 1)
        party <- "Democratic"
      } else {
        pct_flow <- round((1-pct_flow_dem) * 100, digits = 1)
        party <- "Republican"
      }
      
      return(paste0("Counties that voted ",
                    party, " in ", year, cont_rec, pct_flow,
                    "% of this ", direction, " flow."))
      
    } else if (colorBy == "code") {
      countydata_agg <- countydata %>%
        sf::st_drop_geometry() %>%
        group_by(code) %>%
        summarize(tot = sum(sort_variable)) %>%
        arrange(desc(tot))
      # View(countydata_agg)
      top_class <- countydata_agg %>% slice(1)
      # print(top_class)
      pct_flow_raw <- top_class$tot / sum(countydata$sort_variable)
      pct_flow <- round(pct_flow_raw * 100, digits = 1)
      top_code <- top_class$code
      
      return(paste0(top_code,
                    " counties", cont_rec, pct_flow,
                    "% of this ", direction, " flow."))
      
    } else if (colorBy == "hh_income") {
      countydata_agg <- countydata %>%
        sf::st_drop_geometry() %>%
        mutate(high_income = hh_income > 50000) %>%
        group_by(high_income) %>%
        summarize(tot = sum(sort_variable))
      # View(countydata_agg)
      pct_flow_high_income <- countydata_agg %>%
        filter(high_income == TRUE) %>%
        select(tot) %>%
        unlist(use.names = FALSE) / sum(countydata$sort_variable)
      # print(pct_flow_dem)
      if (length(pct_flow_high_income) == 0) {
        pct_flow_high_income = 0
      }
      if (pct_flow_high_income > 0.5) {
        pct_flow <- round(pct_flow_high_income * 100, digits = 1)
        type <- "higher"
      } else {
        pct_flow <- round((1-pct_flow_high_income) * 100, digits = 1)
        type <- "lower"
      }
      
      return(paste0("Counties with median incomes ", type,
                    " than $50,000", cont_rec, pct_flow,
                    "% of this ", direction, " flow."))
      
    } else if (colorBy == "per_degree") {
      countydata_agg <- countydata %>%
        sf::st_drop_geometry() %>%
        mutate(high_ed = per_degree > 25) %>%
        group_by(high_ed) %>%
        summarize(tot = sum(sort_variable))
      # View(countydata_agg)
      pct_flow_high_ed <- countydata_agg %>%
        filter(high_ed == TRUE) %>%
        select(tot) %>%
        unlist(use.names = FALSE) / sum(countydata$sort_variable)
      # print(pct_flow_dem)
      if (length(pct_flow_high_ed) == 0) {
        pct_flow_high_ed = 0
      }
      if (pct_flow_high_ed > 0.5) {
        pct_flow <- round(pct_flow_high_ed * 100, digits = 1)
        more_less <- "more"
      } else {
        pct_flow <- round((1-pct_flow_high_ed) * 100, digits = 1)
        more_less <- "less"
      }
      
      return(paste0("Counties where ", more_less,
                    " than a quarter of adults have a college degree",
                    cont_rec, pct_flow,
                    "% of this ", direction, " flow."))
      
    } else {
      top_county <- countydata %>% slice(1)

      pct_flow_top_county = top_county$sort_variable / sum(countydata$sort_variable)
      pct_flow <- round(pct_flow_top_county * 100, digits = 1)
      
      return(paste0(top_county$NAMELSAD, ", ", top_county$state, cont_rec,
                    " ", pct_flow, "% of this ", direction, " flow."))
    }
    
    # sel_by <- isolate(select_by())
    # if (sel_by == "individual" || is.null(sel_by)) {
    #   return("...")
    # } else if (grepl("gop_pct", sel_by)) {
    #   gop_pct <- input[[sel_by]]
    #   
    #   year <- substr(sel_by, 9, 10)
    #   if (year == "") {
    #     year = "16"
    #   }
    #   
    #   gop_pct_low <- gop_pct[1]
    #   gop_pct_high <- gop_pct[2]
    #   
    #   return(paste0("counties that voted between ", gop_pct_low, "%",
    #                 " and ", gop_pct_high, "% Republican in 20", year))
    #   
    # } else if (sel_by == "hh_income") {
    #   hh_income <- input$hh_income
    #   
    #   hh_income_low <- hh_income[1]
    #   hh_income_high <- hh_income[2]
    #   
    #   return(paste0("counties with a median household income between ",
    #                 "$", hh_income_low, " and $", hh_income_high))
    #   
    # } else if (sel_by == "code") {
    #   code_sel <- input$code
    #   
    #   countydata <- countydata0#countydata()
    #   countydata_fips <- countydata %>%
    #     filter(code == code_sel) #%>%
    #   
    #   return(paste0(tolower(code_sel), " counties"))
    #   
    # } else if (sel_by == "per_degree") {
    #   per_degree <- input$per_degree
    #   
    #   per_degree_low <- per_degree[1]
    #   per_degree_high <- per_degree[2]
    #   
    #   return(paste0("counties where between ",
    #                 per_degree_low, "% and ", per_degree_high,
    #                 "% of residents have a college degree"))
    # } else {
    #   print("*****BUG*****")
    # }
    
  }
  
  getDetailsModeText <- function() {
    event <- isolate(clicked_county())
    sizeBy <- isolate(input$size)
    selectBy <- isolate(select_by())
    
    # num_top_counties <- isolate(input$num)
    num_top_counties <- isolate(input$new_num_counties)
    if (is.na(num_top_counties) || num_top_counties < 1) {
      num_top_counties <- 1
    }
    
    countydata <- isolate(countydata())
    
    #### DESCRIPTIVE TEXT
    incoming <- incoming_bool()#grepl("incoming", isolate(input$in_out))
    if (incoming) {
      direction <- "into"
    } else {
      direction <- "from"
    }
    net <- grepl("net", isolate(input$in_out))
    eff <- grepl("eff", isolate(input$in_out))
    if (net) {
      net_infix <- "net migration "
    } else if (eff) {
      net_infix <- "efficient migration "
    } else {
      net_infix <- " migrants "
    }
    details_mode <- isolate(details_mode_bool())
    
    if (!is.null(event) && details_mode) {
      selected_county <- countydata0 %>%
        filter(GEOID == event$id)
      # View(selected_county)
      content <- paste0(getDescriptiveTextHelper(),
                        " with the most ", net_infix,# "migrants ",
                        direction,  ' <div class="details-mode-county-text">',
                        selected_county$NAMELSAD, 
                        ", ", selected_county$state, "</div><br>")
    } else {
      content <- ""
    }
    ##
    content
  }
  
  ## CREATE INTERACTIVE MAP ###########################################
  
  user_map <- reactiveVal(NULL)
  
  map_reactive <- reactiveVal(
    leaflet(options = leafletOptions(zoomSnap = 0, zoomDelta=0.5,
                                     zoomControl = FALSE)) %>%
      # addProviderTiles(providers$Jawg.Light) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # addProviderTiles(providers$CartoDB.Voyager) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
      setView(lng = -98.85, lat = 37.45, zoom = 4) %>% 
      # addPolygons(data = regions, stroke = TRUE,
      #             weight = 0.5, color="gray", opacity=0.8, #weight = 1.0, color = "black", opacity = 1.0,
      #             smoothFactor = 0.2, fill = FALSE,
      #             options = pathOptions(interactive = FALSE)) %>%
      onRender("
                     function(el, x) {
                         this.on('shape_click', function(e) {
                             Shiny.onInputChange('myEvent', 'open');
                         });
                     }")
  )
  
  
  output$map <- renderLeaflet({
    map_reactive()
  })

  
  dataHandler <- reactive({
    colorBy <- input$color #"per_dem_2016"
    sizeBy <- input$size
    view <- input$view
    selectBy <- select_by()#input$select_by
    # show_county_arrows <- input$show_arrows == "Yes"
    show_county_arrows <- input$show_arrows_alt
    
    # countydata <- countydata()
    event <- clicked_county()#input$map_shape_click
    incoming <- incoming_bool()
    in_out <- input$in_out
    norm <- input$norm
    # num_top_counties <- input$num
    num_top_counties <- input$new_num_counties
    if (is.na(num_top_counties) || num_top_counties < 1) {
      num_top_counties <- 1
    }
    details_mode <- details_mode_bool()
    countydata_fips = countydata_fips()
    
    
    list(colorBy = colorBy,
         sizeBy = sizeBy,
         selectBy = selectBy,
         show_county_arrows = show_county_arrows,
         # countydata = countydata,
         event = event,
         view = view,
         incoming = incoming,
         details_mode = details_mode,
         in_out = in_out,
         norm = norm,
         num_top_counties = num_top_counties,
         countydata_fips = countydata_fips
         )
  })
  
  
  ## UPDATE MAP WITH INPUT CHANGES ######################
  
  observe({
    dataHandler_d <- dataHandler %>% debounce(500)
    dataHandlerList <- dataHandler_d()
    # print(event2)
    colorBy <- dataHandlerList$color #"per_dem_2016"
    sizeBy <- dataHandlerList$size
    selectBy <- dataHandlerList$selectBy
    show_county_arrows <- dataHandlerList$show_county_arrows

    countydata_d <- countydata %>% debounce(500)
    countydata <- countydata_d()#dataHandlerList$countydata
    event <- dataHandlerList$event#input$map_shape_click
    
    view <- dataHandlerList$view
    incoming <- dataHandlerList$incoming
    
    # print(dataHandlerList)

    output$desc_text <- renderText({
      getDescriptiveText()
    })
    
    ####
    output$details_mode_text <- renderText({
      getDetailsModeText()
    })
    
    ####
    
    # Which dataset to use for map? Subset or full?
    if (selectBy == "individual" && is.null(event)) {
      countydata_formap <- countydata0
      no_selection <- TRUE
      shinyjs::hide("download_data")
      # if (colorBy == "migrants_bin") {
      #   colorBy <- "per_dem_2020"
      # }
    } else {
      countydata_formap <- countydata
      no_selection <- FALSE
      shinyjs::show("download_data")
    }
    
    # Silently fail if we have incongruous variables (which are still updating)
    req(!(no_selection && colorBy == "migrants_bin"))
    
    # Deal with map colors
    colorData <- countydata_formap[[colorBy]]
    colorDataForPal <- countydata0[[colorBy]]
    
    if (grepl("per_dem", colorBy)) { #(colorBy == "per_dem_2016")
      hexes_6_darker <- c('#b2182b','#d6604d','#f4a582','#92c5de','#4393c3','#2166ac')
      
      year <- substr(colorBy, 11, 12)
      label <- paste0("% Dem '", year)
      if (reverse_legend) {
        hexes_6_darker <- rev(hexes_6_darker)
        label <- paste0("% GOP '", year)
      }
      
      bin_vector_6 = c(0.0, 0.3, 0.4, 0.5, 0.6, 0.7, 1.0)
      pal <- colorBin(hexes_6_darker, colorData, bins=bin_vector_6, reverse=reverse_legend)
      
    } else if ((colorBy == "pop_dens") || (colorBy == "pct_black")) {
      pal <- colorFactor("viridis", colorDataForPal, ordered=TRUE)
      pal2 <- colorFactor("viridis", colorDataForPal, ordered=TRUE, reverse=reverse_legend)
      
      qpal_colors <- unique(pal2(sort(colorDataForPal))) # hex codes
      qpal_labs <- unique(sort(colorDataForPal)) # depends on n from pal
      
      qpal_labs <- gsub("\\[", "", qpal_labs)
      qpal_labs <- gsub("\\]", "", qpal_labs)
      qpal_labs <- gsub("\\(", "", qpal_labs)
      qpal_labs <- gsub("\\)", "", qpal_labs)
      qpal_labs <- gsub("\\s", "", qpal_labs)
      qpal_labs <- gsub(",", " – ", qpal_labs)
      # print(length(qpal_colors))
      if (reverse_legend) {
        qpal_labs <- rev(qpal_labs)
      }
      label <- as.character(color_label_map[colorBy])
      
    } else if (colorBy == "code") {
      pal <- colorFactor("viridis", colorDataForPal, ordered=TRUE, reverse=TRUE) 
      
      qpal_colors <- unique(pal(sort(colorDataForPal)))
      qpal_labs <- unique(sort(colorDataForPal))
      
      label <- "Urban Classification"
      
    } else if (colorBy == "hh_income") {
      # probs <- c(0, 0.3, 0.6, 0.75, 0.95, 1)
      probs <- c(0, 0.3, 0.6, 0.718, 0.95, 1)
      pal <- colorQuantile("viridis", colorDataForPal, probs=probs, reverse=FALSE)
      pal2 <- colorQuantile("viridis", colorDataForPal, probs=probs, reverse=reverse_legend)
      
      qpal_colors <- unique(pal2(sort(colorDataForPal))) # hex codes
      qpal_labs <- quantile(colorDataForPal, probs=probs)
      qpal_labs <- signif(qpal_labs, 3)
      qpal_labs <- paste(paste0("$", lag(qpal_labs)),
                         paste0("$", qpal_labs),
                         sep = " – ")[-1] # first lag is NA
      
      if (reverse_legend) {
        qpal_labs <- rev(qpal_labs)
      }
      
      label <- as.character(color_label_map[colorBy])
      
    } else if (colorBy == "per_degree") {
      # quantile with n=7 --> probs=seq(0, 1, length.out=8)
      probs <- seq(0, 1, length.out=8)
      probs[6] <- probs[6] + 0.012
      pal <- colorQuantile("viridis", colorDataForPal, probs=probs, reverse=FALSE)
      pal2 <- colorQuantile("viridis", colorDataForPal, probs=probs, reverse=reverse_legend)

      qpal_colors <- unique(pal2(sort(colorDataForPal))) # hex codes
      
      qpal_labs <- quantile(colorDataForPal, probs=probs)
      qpal_labs <- round(qpal_labs, 2)
      qpal_labs <- paste(paste0(lag(qpal_labs), "%"),
                         paste0(qpal_labs, "%"),
                         sep = " – ")[-1] # first lag is NA
      
      if (reverse_legend) {
        qpal_labs <- rev(qpal_labs)
      }
      
      label <- as.character(color_label_map[colorBy])
      
    } else if (colorBy == "migrants_bin") {
      # migrants_bin_colors <- rev(c('#c7e9c0','#a1d99b','#74c476',
      #                              '#41ab5d','#238b45','#006d2c'))
      migrants_bin_colors <- rev(c('#c7e9c0','#a1d99b','#74c476',
                                   '#238b45','#00441b'))
      # migrants_bin_colors <- rev(c('#c7e9c0','#74c476','#41ab5d',
      #                              '#238b45','#00441b'))
      
      pal <- colorFactor(migrants_bin_colors, colorData, ordered=TRUE) 
      
      labels <- gtools::mixedsort(unique(colorData))
      qpal_colors <- unique(pal(sort(labels)))
      qpal_labs <- unique(sort(labels))
      
      if (grepl("eff", isolate(input$in_out))) {
        label <- "Migration Efficiency (%)"
      } else {
        label <- as.character(color_label_map[colorBy])
      }
      
    } else if (grepl("total_in", colorBy) ||
               grepl("total_out", colorBy)) {
      # View(countydata)
      
      colors_in <- c('#c7e9c0','#a1d99b','#74c476',
                     '#238b45','#00441b')
      
      colors_out <- c('#fee5d9','#fcae91','#fb6a4a',
                      '#de2d26','#a50f15')
      
      if (grepl("total_in", colorBy)) {
        migrants_bin_colors <- colors_in
      } else if (grepl("total_out", colorBy)) {
        migrants_bin_colors <- colors_out
      }
      
      if (grepl("_norm", colorBy)) {
        bins <- c(0, 80, 185, 300, 460, ceiling(max(colorDataForPal)))
      } else {
        bins <- c(0, 260, 2325, 20800, 185000, max(colorDataForPal))
      }# else if (colorBy == "total_in_norm")
      
      # num_bins <- 5
      
      # bins <- rgeoda::natural_breaks(k = 5, data_frame(colorDataForPal))
      # bins <- c(0, bins, max(colorDataForPal))
      # print(bins)
      
      pal <- colorBin(migrants_bin_colors, colorData, bins=bins, reverse=FALSE)
      pal2 <- colorBin(migrants_bin_colors, colorData, bins=bins, reverse=reverse_legend)
      
      # pal <- colorQuantile(migrants_bin_colors, colorDataForPal, n=num_bins, reverse=FALSE)
      # pal2 <- colorQuantile(migrants_bin_colors, colorDataForPal, n=num_bins, reverse=reverse_legend)
      
      label <- as.character(color_label_map[colorBy])
      
      qpal_colors <- unique(pal2(sort(colorDataForPal))) # hex codes
      
      qpal_labs <- paste(lag(bins), bins,
                         sep = " – ")[-1] # first lag is NA
      
      # qpal_labs <- quantile(colorDataForPal, probs=seq(0, 1, length.out=num_bins+1))
      # # depends on n from pal
      # qpal_labs <- round(qpal_labs, 2)
      # qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA
      
      if (reverse_legend) {
        qpal_labs <- rev(qpal_labs)
      }
      
    } else if (grepl("total_net", colorBy)) {
      
      # red-green?
      # colors_net <- c('#d7191c','#fdae61','#ffffbf',
      #                 '#a6d96a','#1a9641')
      # colorblind-safe
      colors_net <- c('#a6611a','#dfc27d','#f5f5f5',
                      '#80cdc1','#018571')
      
      if (colorBy == "total_net") {
        bins <- c(min(colorDataForPal), -500, -100,
                  100, 500, max(colorDataForPal))
      } else {
        bins <- c(floor(min(colorDataForPal)), -40, -10,
                  10, 40, ceiling(max(colorDataForPal)))
      }
      
      pal <- colorBin(colors_net, colorDataForPal,
                      bins=bins, reverse=FALSE)
      pal2 <- colorBin(colors_net, colorDataForPal,
                       bins=bins, reverse=reverse_legend)
      
      label <- as.character(color_label_map[colorBy])
      
      qpal_colors <- unique(pal2(sort(colorDataForPal))) # hex codes
      
      qpal_labs <- paste(lag(bins), bins,
                         sep = " – ")[-1] # first lag is NA
      
      
      # qpal_colors <- unique(pal2(sort(colorDataForPal))) # hex codes
      # 
      # qpal_labs <- quantile(colorDataForPal, probs=probs)
      # # depends on n from pal
      # qpal_labs <- round(qpal_labs, 2)
      # qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA
      
      if (reverse_legend) {
        qpal_labs <- rev(qpal_labs)
      }
      
    } else { ## No pre-set color scheme, use quantiles
      num_bins <- 4
      
      pal <- colorQuantile("viridis", colorDataForPal, n=num_bins, reverse=FALSE)
      pal2 <- colorQuantile("viridis", colorDataForPal, n=num_bins, reverse=reverse_legend)
      
      label <- colorBy
      
      qpal_colors <- unique(pal2(sort(colorDataForPal))) # hex codes

      qpal_labs <- quantile(colorDataForPal, probs=seq(0, 1, length.out=num_bins+1))
      # depends on n from pal
      qpal_labs <- round(qpal_labs, 2)
      qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA

      if (reverse_legend) {
        qpal_labs <- rev(qpal_labs)
      }
      
      # print(label)
    }
    
    # Render Scatterplot
    if (!no_selection) {
      if (old_scatterplot || 
          # colorBy == "code" ||
          colorBy == "migrants_bin") {
        ax <- list(
          title = "",
          visible = FALSE,
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        )
        bottom_margin <- 30
      } else if (colorBy == "code") {
        ax <- list(
          title = FALSE,
          visible = TRUE,
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = TRUE,
          range = c(0.5, 4.5),
          tickvals = c(1, 2, 3, 4),
          ticktext = c("Rural", "Small\nmetro", "Suburban", "Urban"),
          tickmode= "array",
          showgrid = FALSE
        )
        bottom_margin <- 2
      } else if (grepl("per_dem", colorBy)) {
        ax <- list(
          title = color_label_map_scatter[colorBy],
          visible = TRUE,
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = FALSE,
          range = c(0, 1),
          tickvals = c(0.2, 0.4, 0.6, 0.8, 1),
          tickformat = ".0%"
        )
        bottom_margin <- 10
      # } if (colorBy == "code" ||
      #            colorBy == "migrants_bin") {
      #   ax <- list(
      #     title = "",
      #     visible = FALSE,
      #     zeroline = FALSE,
      #     showline = FALSE,
      #     showticklabels = FALSE,
      #     showgrid = FALSE
      #   )
      } else if (colorBy == "per_degree") {
        ax <- list(
          title = color_label_map_scatter[colorBy],
          visible = TRUE,
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = FALSE,
          # dtick = 0.15,
          ticksuffix = "%"
        )
        bottom_margin <- 10
      } else if (colorBy == "hh_income") {
        ax <- list(
          title = color_label_map_scatter[colorBy],
          visible = TRUE,
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = FALSE,
          # dtick = 0.15,
          tickprefix = "$"
        )
        bottom_margin <- 10
      } else {
        ax <- list(
          title = color_label_map_scatter[colorBy],
          visible = TRUE,
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = FALSE
        )
        bottom_margin <- 30
      }
      
      if (grepl("eff", isolate(input$in_out))) {
        scatter_title <- "Efficiency (%)"
      } else {
        scatter_title <- "# Migrants"
      }
      
      ay <- list(
        title = list(text = scatter_title, standoff = 1L),
        # title = scatter_title,
        rangemode="tozero",
        zerolinecolor = "rgb(229,229,229)"
      )
      
      if (grepl("out", input$in_out)) {
        t <- list(
          text = "<b> Top Destination Counties </b>",
          y=0.95
        )     
      } else {
        t <- list(
          text = "<b> Top Origin Counties </b>",
          y=0.95
        )     
      }
      
      # t <- list(
      #   text = "<b> Top Migrant Counties </b>",
      #   y=0.95
      # )
      
      # colorData <- countydata[["per_dem_2016"]]
      # 
      # hexes_6_darker = c('#b2182b','#d6604d','#f4a582','#92c5de','#4393c3','#2166ac')
      # bin_vector_6 = c(0.0, 0.3, 0.4, 0.5, 0.6, 0.7, 1.0)
      # pal <- colorBin(hexes_6_darker, colorData, bins=bin_vector_6)
      
      no_subtitle <- TRUE
      ## SCATTERPLOT
      if (old_scatterplot || colorBy == "migrants_bin"){#} || colorBy == "code") {
        scatter_x <- "idx"
        no_subtitle <- TRUE
      } else if (colorBy == "code") {
        scatter_x <- "code_num"
        no_subtitle <- TRUE
      } else if (grepl("per_dem", colorBy)) {
        scatter_x <- stringr::str_replace(colorBy, 
                                          "dem", 
                                          "gop")
      } else {
        scatter_x <- colorBy
      }
      # print(selectBy)
      shape <- NULL
      subtitle_text <- ""
      # if (!is.null(event)) {
      #   county_clicked <- countydata0 %>% filter(GEOID == event$id)
      #   selected_val <- county_clicked[[scatter_x]]
      #   shape <- list(type = "line", y0 = 0, y1 = 1, yref = "paper",
      #                 x0 = selected_val, x1 = selected_val,
      #                 line = list(color = "black", dash="dot")
      #                 # layer="below"
      #   )
      #   if (no_subtitle) {
      #     subtitle_text <- ""
      #   } else {
      #     subtitle_text <- paste("Dotted line shows the clicked",
      #                            "county's x-axis value")
      #   }
      # 
      # } else {
      #   county_clicked <- NULL
      #   selected_val <- 0
      #   if (no_subtitle) {
      #     subtitle_text <- ""
      #   } else {
      #     subtitle_text <- paste("Shaded area covers the attribute",
      #                            "range of interest")
      #   }
      #   
      #   
      #   #}
      #   
      #   
      #   
      #   # selectBy <- isolate(input$selectBy)
      #   # print(selectBy)
      #   if (selectBy == "code") {
      #     attribute_range_low <- 0
      #     attribute_range_high <- 0
      #     shape <- NULL
      #   } else {
      #     
      #     if (grepl("gop_pct", selectBy)) {
      #       # print("here")
      #       attribute_range_low <- input[[selectBy]][1] / 100
      #       attribute_range_high <- input[[selectBy]][2] / 100
      #     } else {
      #       attribute_range_low <- input[[selectBy]][1]
      #       attribute_range_high <- input[[selectBy]][2]
      #       # print(selectBy)
      #       # print(input[[selectBy]])
      #     }
      #     shape <- list(type = "rect",
      #                   fillcolor = "grey", line = list(color = "grey"),
      #                   opacity = 0.2, yref="paper", layer="below",
      #                   y0 = 0, y1 = 1,
      #                   x0 = attribute_range_low, x1 = attribute_range_high)
      #     
      #   }
      #   
      #   # print(attribute_range_low)
      #   # print(attribute_range_high)
      # }
      
      
      output$scatter <- renderPlotly({
        if (nrow(countydata) == 0) {
          plotly_empty(type = "scatter", mode = "markers") %>%
            config(
              displayModeBar = FALSE
            ) %>%
            layout(
              title = list(
              text = "No migrant counties",
              yref = "paper",
              y = 0.5
              )
            )
        } else {
          plot_ly(
            countydata, source="B", type="scatter", mode="markers") %>%
            add_trace(
              x = as.formula(paste0("~`", scatter_x, "`")), y = ~sort_variable,
              opacity=0.5, marker=list(color=pal(colorData), line=list(color = "000000", 
                                                                       width = 1)),  # color=I("black") 
              # Hover text:
              hovertemplate = ~paste0(NAMELSAD, ', ', state, '<extra></extra>', sep="")#,
            ) %>%
            add_trace(x = NULL,
                      y = NULL,
                      color = I("#fe019a"),
                      alpha = 0.9) %>%
            layout(showlegend = FALSE,
                   xaxis = ax, 
                   yaxis = ay,
                   title=t,
                   dragmode = FALSE,
                   # shapes = shape,
                   # annotations = list(x = 0.46, y = 1.175, text = subtitle_text,
                   #                    showarrow = F, xref='paper', yref='paper',
                   #                    font = list(size=10.5)),
                   margin=list(l = 68, r = 40,
                               b = bottom_margin, t = 35,
                               pad = 5)
            ) %>%
            config(displayModeBar = F)
        }
      })
    }

    # Render the shapes on the map    
    if (view == "circles" && !(no_selection)) {  #input$view == "circles"
      
      diffs = c()
      top5 <- countydata_formap %>% slice_head(n = 5)
      for (i in seq(1, 4)) {
        for (j in seq(i+1, 5)) {
          r1 <- top5 %>% slice(i)
          r2 <- top5 %>% slice(j)
          # print(r1$INTPTLAT)
          lat_diff <- abs(as.numeric(r1$INTPTLAT) - as.numeric(r2$INTPTLAT))
          lon_diff <- abs(as.numeric(r1$INTPTLON) - as.numeric(r2$INTPTLON))
          diffs = c(diffs, lat_diff + lon_diff)
        }
      }
      # print(diffs)
      if (min(diffs) < 1) {
        scale <- 0.7
      } else {
        scale <- 1
      }
      
      
      # View(countydata_formap)
      m <- sort(countydata_formap$sort_variable)[nrow(countydata) - 1]
      if (identical(m, integer(0))) {
        m <- 1
      }
      countydata_formap <- countydata_formap %>%
        mutate(INTPTLON = as.numeric(INTPTLON),
               INTPTLAT = as.numeric(INTPTLAT),
               radii = sort_variable / m * 40000 * scale, 
               radii = pmin(radii, 50000 * scale),
               radii = pmax(radii, 7500 * scale))
      
      leafletProxy("map", data = countydata_formap) %>%
        clearGroup("counties") %>%
        addCircles(~INTPTLON, ~INTPTLAT, radius=~radii, layerId=~GEOID,
                   fillOpacity=0.3, fillColor=pal(colorData),
                   stroke=TRUE, weight=1, color='black', group = "counties")
    } else {
      leafletProxy("map") %>%
        clearGroup("counties") %>%
        addPolygons(data = countydata_formap, stroke = TRUE, weight = 0.5, color="gray", opacity=0.8, smoothFactor = 0.2, fillOpacity = 0.3,
                    fillColor = pal(colorData), layerId=~GEOID, group="counties")
    }
    
    if (add_legend) {
      scatterplot_hidden <- isolate(input$scatterplot_hidden)
      if (no_selection) {
        class_name <- "legend_noSelection"
      } else {
        class_name <- "legend_selection"
      }
      if (!is.null(scatterplot_hidden) && scatterplot_hidden == TRUE) {
        class_name <- paste(class_name, "legend_noScatterplot")
      }
      if (isolate(snapshot_window_up())) {
        class_name <- paste(class_name, "legend_noMenus")
      }
      
      if (grepl("per_dem", colorBy)) { #(colorBy == "per_dem_2016")
        leafletProxy("map")  %>%
          addLegend("bottomright", pal=pal, values=colorData,
                    title=label, layerId="colorLegend", className = paste("info", "legend", class_name),
                    labFormat = labelFormat(transform = function(x) sort(x * 100, decreasing = reverse_legend)))
      } else {
        # print(qpal_colors)
        # print(qpal_labs)
        leafletProxy("map")  %>%
          addLegend("bottomright", colors = qpal_colors, labels = qpal_labs,
                    title=label, layerId="colorLegend", className = paste("info", "legend", class_name))
      }
    }
    
    if (!is.null(event)) {
      fips_of_interest <- event$id
      county = countydata0 %>% filter(GEOID == fips_of_interest)

      county_lon <- as.numeric(county$INTPTLON)
      county_lat <- as.numeric(county$INTPTLAT)
      
      arrows <- getArrowCoordinates(county_lat, county_lon,
                                    incoming=incoming)#grepl("incoming", input$in_out))
      # print(arrows)
      if (view == "circles") {  #input$view == "circles"
        leafletProxy("map", data = countydata_formap) %>%
          clearGroup("counties") %>%
          addCircles(~INTPTLON, ~INTPTLAT, radius=~radii, layerId=~GEOID,
                     fillOpacity=0.3, fillColor=pal(colorData),
                     stroke=TRUE, weight=1, color='black', group = "counties") %>%
          addPolygons(data = county, stroke = TRUE, weight = 1.0, color="black", opacity=0.9, smoothFactor = 0.2, fillOpacity = 0.5,
                      fillColor = "#fe019a", layerId=~GEOID, group = "counties")
        # print(map_reactive())
        if (!is.null(map_reactive())) {
        m <- map_reactive()  %>%
          addCircles(countydata_formap$INTPTLON, countydata_formap$INTPTLAT, 
                     radius=countydata_formap$radii, layerId=countydata_formap$GEOID,
                     fillOpacity=0.3, fillColor=pal(colorData),
                     stroke=TRUE, weight=1, color='black', group = "counties") %>%
          addPolygons(data = county, stroke = TRUE, weight = 1.0, color="black", opacity=0.9, smoothFactor = 0.2, fillOpacity = 0.5,
                      fillColor = "#fe019a", layerId=~GEOID, group = "counties")
        
        user_map(m)
        }
        
        # map_reactive(        leafletProxy("map", data = countydata_formap) %>%
        #                        clearGroup("counties") %>%
        #                        addCircles(~INTPTLON, ~INTPTLAT, radius=~radii, layerId=~GEOID,
        #                                   fillOpacity=0.3, fillColor=pal(colorData),
        #                                   stroke=TRUE, weight=1, color='black', group = "counties") %>%
        #                        addPolygons(data = county, stroke = TRUE, weight = 1.0, color="black", opacity=0.9, smoothFactor = 0.2, fillOpacity = 0.5,
        #                                    fillColor = "#fe019a", layerId=~GEOID, group = "counties")
        # )
        
        
      } else {
        leafletProxy("map") %>%
          addPolygons(data = county, stroke = TRUE, weight = 1.0, color="black", opacity=0.9, smoothFactor = 0.2, fillOpacity = 0.5,
                      fillColor = "#fe019a", layerId=~GEOID, group = "counties") 
        
        # map_reactive(        leafletProxy("map") %>%
        #                        addPolygons(data = county, stroke = TRUE, weight = 1.0, color="black", opacity=0.9, smoothFactor = 0.2, fillOpacity = 0.5,
        #                                    fillColor = "#fe019a", layerId=~GEOID, group = "counties") 
        # )
        
        
        
        
      }
      if (!show_county_arrows) {
        arrowOpacity <- 0.5
        
        leafletProxy("map") %>%
          addPolylines(arrows$lng_north, arrows$lat_north,
                       weight = 2, color = "black",
                       fillOpacity = arrowOpacity, smoothFactor = 0.2,
                       options = pathOptions(interactive = FALSE),
                       group = "counties") %>%
          addPolylines(arrows$lng_south, arrows$lat_south,
                       weight = 2, color = "black",
                       fillOpacity = arrowOpacity, smoothFactor = 0.2,
                       options = pathOptions(interactive = FALSE),
                       group = "counties") %>%
          addPolylines(arrows$lng_east, arrows$lat_east,
                       weight = 2, color = "black",
                       fillOpacity = arrowOpacity, smoothFactor = 0.2,
                       options = pathOptions(interactive = FALSE),
                       group = "counties") %>%
          addPolylines(arrows$lng_west, arrows$lat_west,
                       weight = 2, color = "black",
                       fillOpacity = arrowOpacity, smoothFactor = 0.2,
                       options = pathOptions(interactive = FALSE),
                       group = "counties")
      } else { ## show_county_arrows == TRUE

        county_fips_formap <- c(countydata_formap$GEOID, county$GEOID)
        
        bundled_counties <- getCountyGroupsForArrows(county_fips_formap,
                                               county$GEOID,
                                               bundle=edge_bundling)
        
        county_neighbors <- adjacency %>%
          filter(fipscounty == county$GEOID) %>%
          select(fipsneighbor) %>%
          unlist() %>%
          unname()

        for (county_group in bundled_counties) {
          if (length(county_group) == 1 &&
              county_group[[1]] %in% county_neighbors &&
              length(county_neighbors)/nrow(countydata_formap) < 0.5) {
            next
          } else if (length(county_group) == 1 &&
                     county_group[[1]] %in% county_neighbors) {
            adjacent = TRUE
          } else {
            adjacent = FALSE
          }
          
          if (length(county_group) > 1) {
            top_counties <- countydata_formap %>% filter(GEOID %in% county_group)
            top_counties_bbox <- sf::st_bbox(top_counties$geometry)
            
            x_mid <- (as.numeric(top_counties_bbox['xmax']) + as.numeric(top_counties_bbox['xmin'])) / 2
            y_mid <- (as.numeric(top_counties_bbox['ymax']) + as.numeric(top_counties_bbox['ymin'])) / 2
            
            county_lon2 <- as.numeric(x_mid)
            county_lat2 <- as.numeric(y_mid)
          } else {
            top_county <- countydata_formap %>% filter(GEOID == county_group[[1]])

            county_lon2 <- as.numeric(top_county$INTPTLON)
            county_lat2 <- as.numeric(top_county$INTPTLAT)
          }


          incoming <- incoming#grepl("incoming", isolate(input$in_out))
          arrows <- getCountyArrowCoordinates(county_lat, county_lon,
                                              county_lat2, county_lon2,
                                              incoming=incoming,
                                              adjacent=adjacent)
          
          leafletProxy("map") %>%
            addPolylines(arrows$lng_east, arrows$lat_east,
                         weight = 1, color = "black",
                         opacity = 0.3, smoothFactor = 0.2,
                         options = pathOptions(interactive = FALSE),
                         group = "counties")
        } # for loop

      } # show_county_arrows == TRUE
      
    }
  })
  
  # Update scatterplot on map shape hover
  observeEvent(map_shape_hover(), {
    event2 <- map_shape_hover()
    if (!is.null(event2)) {
      dh <- isolate(dataHandler())
      colorBy <- dh$color
      selected_county <- isolate(countydata()) %>%
        filter(GEOID == event2$id)
      # xcoor <- selected_county$idx[1]
      if (old_scatterplot || colorBy == "migrants_bin") {
        xcoor <- selected_county[['idx']][1]
      } else if (colorBy == "code") {
        xcoor <- selected_county[['code_num']][1]
      } else if (grepl("per_dem", colorBy)) {
        colorBy <- stringr::str_replace(colorBy, 
                                        "dem", 
                                        "gop")
        xcoor <- selected_county[[colorBy]][1]
      } else {
        xcoor <- selected_county[[colorBy]][1]
      }
      ycoor <- selected_county$sort_variable[1]
      
      plotlyProxy("scatter", session) %>%
      plotlyProxyInvoke("addTraces", list(list(x=c(xcoor, xcoor),
                                          y=c(ycoor, ycoor),
                                          type = 'scatter',
                                          mode = 'markers',
                                          marker=list(
                                            color = "#fe019a",
                                            alpha = 0.9
                                          ))))
    }
    
  })
  
  # Update scatterplot on map shape mouseout
  observeEvent(input$map_shape_mouseout, {
    # print("HERE")
    plotlyProxy("scatter", session) %>%
      plotlyProxyInvoke("deleteTraces", -c(as.integer(1)))
  })
  
  
  ## MANAGE COUNTY POPUPS AND SIDEBAR TEXT ########################
  
  showCountyPopup <- function(county_fips, lat, lng, pan_bool=TRUE) {
    leafletProxy("map") %>% clearPopups()
    selectedCounty <- countydata0[countydata0$GEOID == county_fips,]

    countydata <- isolate(countydata())
    selectedCountyMig <- countydata %>%
      filter(GEOID == selectedCounty$GEOID)
    
    event <- isolate(clicked_county())
    
    if (!is.null(event) && event$id == selectedCounty$GEOID) {
      content <- as.character(tagList(
        tags$h5(paste0(selectedCounty$NAMELSAD, ", ", selectedCounty$state)))
      )
      
    } else if (isolate(select_by()) == "individual" && is.null(event)) {
      content <- as.character(tagList(
        tags$h5(paste0(selectedCounty$NAMELSAD, ", ", selectedCounty$state)))
      )
      
    } else {
      incoming <- incoming_bool()#grepl("incoming", isolate(input$in_out))
      if (incoming) {
        direction <- "into"
      } else {
        direction <- "from"
      }
      net <- grepl("net", input$in_out)
      eff <- grepl("eff", input$in_out)
      if (net) {
        net_infix <- " net migration "
      } else if (eff) {
        net_infix <- " migration efficiency "
      } else {
        net_infix <- " migrants "
      }
      selectBy <- isolate(select_by())
      if (!is.null(event)) {
        clicked_county <- countydata0 %>%
          filter(GEOID == event$id)
        
        suffix <- paste0(" ", clicked_county$NAMELSAD)
      } else {
        # suffix <- " counties with attribute"
        suffix <- paste0(" ", small_map_title())
      }
      
      if ( (! ("state" %in% colnames(selectedCounty))) || (is.na(selectedCounty$state))) {
        content <- as.character(tagList(
          tags$h5(paste0(selectedCounty$NAMELSAD))
        ))
      } else {
        if (grepl("eff", isolate(input$in_out))) {
          num_formatted <- paste0(round(selectedCountyMig$sort_variable, 1),
                                  "%")
        } else {
          num_formatted <- format(selectedCountyMig$sort_variable,
                                  big.mark=",", trim=TRUE)
        }
        
        content <- as.character(tagList(
          tags$h5(paste0(selectedCounty$NAMELSAD, ", ", selectedCounty$state)),
          tags$span(paste0(num_formatted, net_infix,# " migrants ",
                         direction, suffix)))
        )
      }
      
    }
    bounds <- input$map_bounds
    height <- bounds$north - bounds$south
    correction <- height / 75
    leafletProxy("map") %>% addPopups(lng, lat + correction, content, layerId = county_fips, options=popupOptions(closeButton=FALSE,
                                                                                                                  maxWidth=200,
                                                                                                                  autoPan=pan_bool))#, options=popupOptions(autoPan=FALSE))
  }
  
  # Update sidebar text
  updateText <- function(county_fips) {
    selectedCounty <- countydata0[countydata0$GEOID == county_fips,]
    if ( (! ("state" %in% colnames(selectedCounty))) || (is.na(selectedCounty$state))) {
      content <- selectedCounty$NAMELSAD
    } else {
      content <- paste0(selectedCounty$NAMELSAD, ", ", selectedCounty$state)
    }
    output$county_name <- renderText({ content })
    output$new_county_name <- renderText({ content })
  }
  
  # When map shape is hovered, show a popup with info
  observe({
    #input$state
    # leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_mouseover
    # print(event)
    if (is.null(event) || is.null(event$id))
      return()
    
    # countydata <- isolate(countydata())
    selectedData <- countydata0 %>% filter(GEOID == event$id)
    # View(selectedData)
    lat_top <- as.numeric(sf::st_bbox(selectedData$geometry)['ymax'])
    
    bounds <- isolate(input$map_bounds)
    # print(bounds)
    vscale <- bounds$north - bounds$south
    
    # print(event)
    # print(event$id)
    isolate({
      showCountyPopup(event$id, 
                      max(event$lat, lat_top - vscale * 0.005),
                      as.numeric(selectedData$INTPTLON))
    })
    
  })
  
  # Clear popup on mouseout
  observe({
    event <- input$map_shape_mouseout
    leafletProxy("map") %>% clearPopups()
    })
  
  # Popups when hover on scatterplot
  observe({
    event3 <- event_data("plotly_hover", source="B")
    
    if (is.null(event3)) {
      leafletProxy("map") %>% clearPopups()
    } else {
      pointNum <- event3$pointNumber + 1
      countydata <- isolate(countydata())
      selectedData <- countydata[pointNum,]
      
      curr_zoom = isolate(input$map_zoom)
      curr_center = isolate(input$map_center)
      lat_diff = curr_center$lat - as.numeric(selectedData$INTPTLAT)
      lon_diff = curr_center$lng - as.numeric(selectedData$INTPTLON)
      dist <- sqrt(lat_diff^2 + lon_diff^2)
      dur <- dist / 60
      # print(dur)
      
      # leafletProxy("map") %>%
      #   setView(lat = selectedData$INTPTLAT,
      #           lng = selectedData$INTPTLON,
      #           zoom = curr_zoom,
      #           options = list(duration=min(max(dur, 0.3),
      #                                       0.75),  # 0.4
      #                          animate=TRUE))
      
      lat_top <- as.numeric(sf::st_bbox(selectedData$geometry)['ymax'])
      
      # This delay() call gets rid of weird "blinking popup" behavior
      delay(0, {
        bounds <- isolate(input$map_bounds)
        # print(bounds)
        vscale <- bounds$north - bounds$south
        showCountyPopup(selectedData$GEOID,
                        lat_top - vscale * 0.015,
                        as.numeric(selectedData$INTPTLON),
                        pan_bool=FALSE)
        # print(selectedData)
        
      })

    }
  })
  
  # Pan when click on scatterplot
  observe({
    event3 <- event_data("plotly_click", source="B")
    
    if (is.null(event3)) {
      leafletProxy("map") %>% clearPopups()
    } else {
      pointNum <- event3$pointNumber + 1
      countydata <- isolate(countydata())
      selectedData <- countydata[pointNum,]
      
      curr_zoom = isolate(input$map_zoom)
      curr_center = isolate(input$map_center)
      lat_diff = curr_center$lat - as.numeric(selectedData$INTPTLAT)
      lon_diff = curr_center$lng - as.numeric(selectedData$INTPTLON)
      dist <- sqrt(lat_diff^2 + lon_diff^2)
      dur <- dist / 60
      # print(dur)
      
      leafletProxy("map") %>%
        setView(lat = selectedData$INTPTLAT,
                lng = selectedData$INTPTLON,
                zoom = curr_zoom,
                options = list(duration=min(max(dur, 0.3),
                                            0.75),  # 0.4
                               animate=TRUE))
      
      lat_top <- as.numeric(sf::st_bbox(selectedData$geometry)['ymax'])
      
      # # This delay() call gets rid of weird "blinking popup" behavior
      # delay(0, {
      #   bounds <- isolate(input$map_bounds)
      #   # print(bounds)
      #   vscale <- bounds$north - bounds$south
      #   showCountyPopup(selectedData$GEOID,
      #                   lat_top - vscale * 0.015,
      #                   as.numeric(selectedData$INTPTLON))
      #   # print(selectedData)
      #   
      # })
      
    }
  })
  
  
  ## UPDATE SMALL SIDEBAR MAP(S) ###########################
  observe({

    countydata_fips <- countydata_fips()
    incoming <- incoming_bool()#grepl("incoming", input$in_out)
    
    county_map <- socviz::county_map
    county_map$color <- case_when(
      county_map$id %in% countydata_fips ~ "selected",  # "red"
      TRUE ~ "not_selected"
    )

    p <- ggplot(data = county_map,
                mapping = aes(x = long, y = lat,
                              fill = color, 
                              group = group))
    
    p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()
    
    # Alt. colors: "#99d8c9" (green), "#fe019a" (big map pink), #fbb4ae" (dull pink)
    p2 <- p1 + scale_fill_manual(values=c("gray90", "#ff49b7"),
                                 na.value="gray90")
    
    if (incoming) {
      output$small_map <- renderPlot({
        p3 <- p2 + theme_map() + theme(legend.position = "none")
        
        if (add_small_map_arrows) {
          p3 + annotate("segment",
                        x = 3600000, y = 100,
                        xend = 2600000, yend = 100,  ## "curve" 
                        size = 5, color = "gray50",
                        arrow = arrow(type="closed",
                                      length = unit(0.1, "npc"),
                                      angle = 40)) +
            # annotate("text",
            #            x = 3070000, y = 20000,  # y = 20000, -500000
            #            label = "Migration", color = "white",
            #            fontface = "bold")
            annotate("text",
                     x = 3070000, y = -450000,  # y = 20000, -500000
                     label = "Migration", color = "gray50",
                     fontface = "bold")
          
        } else {
          p3
        }
        
      })
    } else {
      output$small_map <- renderPlot({
        p3 <- p2 + theme_map() + theme(legend.position = "none")
        
        if (add_small_map_arrows) {
          p3 + annotate("segment",
                        x = 2500000, y = 100,
                        xend = 3500000, yend = 100,  ## "curve" 
                        size = 5, color = "gray50",
                        arrow = arrow(type="closed",
                                      length = unit(0.1, "npc"),
                                      angle = 40)) +
            # annotate("text",
            #          x = 3050000, y = 20000,  # y = 20000, -500000
            #          label = "Migration", color = "white",
            #          fontface = "bold")
            annotate("text",
                     x = 3050000, y = -450000,  # y = 20000, -500000
                     label = "Migration", color = "gray50",
                     fontface = "bold")
        } else {
          p3
        }
      })
    }
    
    if (incoming) {
      output$small_map_2 <- renderPlot({
        p3 <- p2 + theme_map() + theme(legend.position = "none")
        p3
      })
    } else {
      output$small_map_2 <- renderPlot({
        p3 <- p2 + theme_map() + theme(legend.position = "none")
        p3
      })
    }
    
  })
  
  
  ## TABLE STUFF ################################
  
  table_data <- reactiveVal(data_frame())
  # updateTableData()
  
  updateTableData <- function() {
    # print("here")
    x <- tryCatch(
      {
        ss <- gs4_get(sheet_url)
        
        read_sheet(ss, col_names=TRUE)
      },
      error = function(e) {
        print("Error connecting to spreadsheet")
        shinyjs::show(id="table_error_message")
        shinyjs::hide(id="table_loading_message")
        return()
        
      } # end error function
    ) # end tryCatch
    
    # If failed tryCatch, stop here
    if (is.null(x)) {
      return()
    }
    
    # print(typeof(x))
    rownames(x) <- NULL
    # View(x)
    x <- x %>%
      mutate(" " = paste(' <button class="btn btn-default action-button btn-info action_button" id="', id, '"',
                         ' style="color: #fff; background-color: #337ab7; border-color: #2e6da4; float:right;"',
                         ' type="button" onclick=get_id(this.id)>View</button>',
                         sep=""))
    table_data(x)
    
    shinyjs::hide(id="table_error_message")
    shinyjs::hide(id="table_loading_message")
    return()
  }
  
  # updateTableData()
  
  output$table_old <- DT::renderDataTable({
    
    data <- table_data() 
    
    if (nrow(data) == 0) {
      return(data)
    }

    df <- data %>%
      mutate("mode" = case_when(
        selectBy == "individual" ~ "Individual",
        TRUE ~ "Attribute"
      )) %>%
      # mutate("details" = case_when(
      #   selectBy != "individual" ~ paste0(select_by_vars_labels[selectBy], ": ", 
      #                                     attribute_range_low, "-",
      #                                     attribute_range_high),
      #   TRUE ~ details
      # )) %>%
      mutate("time" = format(time, "%m/%d/%Y %H:%M")) %>%
      select("title", "time", "mode", "details", "text", " ") %>%
      rename("Timestamp" = "time",
             "Title" = "title",
             "Mode" = "mode",
             "Details" = "details",
             "Comments" = "text")

    # DT::datatable(df, rownames=FALSE)
    action <- DT::dataTableAjax(session, df, outputId = "table")
    # print(df$Timestamp)
    # print(typeof(df$Timestamp))
    DT::datatable(df, escape = FALSE, rownames = FALSE, selection = 'none') %>%
      DT::formatStyle(columns = c("Title"), fontWeight = 'bold', width='150px') %>%
      DT::formatStyle(columns = c("Details"), width='125px')
    
  }
  # rownames= FALSE
  )
  
  output$table <- DT::renderDataTable({
    data <- table_data() 
    
    if (nrow(data) == 0) {
      return(data)
    }
    
    df <- data %>%
      mutate("mode" = case_when(
        selectBy == "individual" ~ "Individual",
        TRUE ~ "Attribute"
      )) %>%
      # mutate("details" = case_when(
      #   selectBy != "individual" ~ paste0(select_by_vars_labels[selectBy], ": ", 
      #                                     attribute_range_low, "-",
      #                                     attribute_range_high),
      #   TRUE ~ details
      # )) %>%
      # filter(details_mode == "FALSE") %>%
      mutate("time" = format(time, "%m/%d/%Y %H:%M")) %>%
      select("title", "time", "mode", "details", "text", " ") %>%
      rename("Timestamp" = "time",
             "Title" = "title",
             "Mode" = "mode",
             "Details" = "details",
             "Comments" = "text") %>%
      select("Title", "Comments", " ")
    
    # DT::datatable(df, rownames=FALSE)
    action <- DT::dataTableAjax(session, df, outputId = "table")
    # print(df$Timestamp)
    # print(typeof(df$Timestamp))
    DT::datatable(df, escape = FALSE, rownames = FALSE, selection = 'none',
                  options=list(  pageLength = 5,
                                 lengthChange = FALSE,
                                 ordering = FALSE,
                                 pagingType = "simple",
                                 columnDefs = list(list(
                                   targets = 1,
                                   render = JS(
                                     "function(data, type, row, meta) {",
                                     "return type === 'display' && data != null && data.length > 70 ?",
                                     "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                                     "}")
                                 ))
                  )) %>%
      DT::formatStyle(columns = c("Title"), fontWeight = 'bold', width='100px') #%>%
      # DT::formatStyle(columns = c("Details"), width='125px')
    
  }
  # rownames= FALSE
  )
  
  
# observeEvent(input$nav, {
#     # print(input$nav)
#     
#     if (input$nav == "snapshots") {
#       
#       x <- tryCatch(
#         {
#           ss <- gs4_get(sheet_url)
#           
#           read_sheet(ss, col_names=TRUE)
#         },
#         error = function(e) {
#           print("Error connecting to spreadsheet")
#           shinyjs::show(id="table_error_message")
#           return()
#           
#         } # end error function
#       ) # end tryCatch
#       
#       # If failed tryCatch, stop here
#       if (is.null(x)) {
#         return()
#       } else {
#         shinyjs::hide(id="table_error_message")
#       }
#       
#       # print(typeof(x))
#       rownames(x) <- NULL
#       # View(x)
#       x <- x %>%
#         mutate(" " = paste(' <button class="btn btn-default action-button btn-info action_button" id="', id, '"',
#                            ' style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"',
#                            ' type="button" onclick=get_id(this.id)>View</button>',
#                            sep=""))
#       table_data(x)
#     }
#   })
  
hideSnapshotsPanel <- function() {
  shinyjs::hide(id="snapshot_view_panel")
  updateActionButton(session, "snapshots", "Public Snapshots")
  shinyjs::show(id="plot2")
  shinyjs::removeClass(selector=".legend", class="legend_noMenus")
  snapshot_window_up(FALSE)
}

showSnapshotsPanel <- function() {
  shinyjs::show(id="snapshot_view_panel")
  updateActionButton(session, "snapshots", "Hide Public Snapshots")
  shinyjs::hide(id="plot2")
  shinyjs::addClass(selector=".legend", class="legend_noMenus")
  snapshot_window_up(TRUE)
  
  # shinyjs::click(id="demo_close")
  # shinyjs::click(id="example_close")
  # shinyjs::click(id="help_close")
  # shinyjs::click(id="about_close_alt")
}
  
observeEvent(input$snapshots, {
  if (isolate(snapshot_window_up())) {
    # print("HIDE")
    hideSnapshotsPanel()
  } else {
    showSnapshotsPanel()
    # print("SHOW")
  }

  # shinyjs::toggle(id="snapshot_view_panel")
  # snapshot_window_up <- !snapshot_window_up
  # 
  # shinyjs::toggle(id="plot2", condition = !snapshot_window_up)
  # shinyjs::toggleClass(selector=".legend", class="legend_noMenus",
  #                      condition = !snapshot_window_up)
  

  table_data_test <- isolate(table_data())
  if (nrow(table_data_test) == 0) {
    shinyjs::show(id="table_loading_message")
    # print("***")
    updateTableData()
  }    
})
  
  observeEvent(input$add_snapshot, {
    runjs("Shiny.onInputChange('demo_show', null);")
    runjs("Shiny.onInputChange('help_skip', 'yes');")
    runjs("Shiny.onInputChange('example_panel', null);")
    
    shinyjs::show(id="snapshot_submission_panel")
  })
  
  
  observeEvent(input$snapshot_submission_close, {
    shinyjs::hide(id="snapshot_submission_panel")
    updateTextInput(session, "snapshot_submission_title", value = "")
    updateTextAreaInput(session, "snapshot_submission_comments", value = "")
  })
  
  
  observeEvent(input$snapshot_submission_close_alt, {
    shinyjs::click(id="snapshot_submission_close")
  })
  
  
  observeEvent(input$snapshot_submission_submit, {
    
    x <- tryCatch(
      {
        ss <- gs4_get(sheet_url)
    
        read_sheet(ss, col_names=TRUE)
      },
      error = function(e) {
        print("Error connecting to spreadsheet")
        
        # Text popup
        runjs("Shiny.onInputChange('example_panel', 'yes');")
        runjs("resetExampleLocation();")
        output$example_name <- renderText({
          "ERROR"
        })
        output$example_text <- renderText({
          "Failed to save snapshot. Try again, or reload the site if the problem persists."
        })
        output$example_img <- renderText({
          ''
        })
        return()
      
      } # end error function
    ) # end tryCatch

    # If failed tryCatch, stop here
    if (is.null(x)) {
      return()
    }
    
    if (nrow(x) == 0) {
      entry_id = 0
    } else {
      entry_id = max(x$id) + 1
    }
    
    data <- isolate(dataHandler())
    
    if (data$selectBy != "individual") {
      if (data$selectBy == "code") {
        attribute_range_low <- input$code_select
        attribute_range_high <- "NULL"
        details <- paste0(select_by_vars_labels[data$selectBy], ": ", 
                          attribute_range_low)
      } else {
        attribute_range_low <- input[[paste0(data$selectBy, "_A")]]
        attribute_range_high <- input[[paste0(data$selectBy, "_B")]]
        details <- paste0(select_by_vars_labels[data$selectBy], ": ", 
                          attribute_range_low, "-",
                          attribute_range_high)
      }
    } else {
      attribute_range_low <- "NULL"
      attribute_range_high <- "NULL"
      details <- ""
    }
    
    data_event <- data$event
    # print(data_event)
    if (is.null(data_event)) {
      event <- "NULL"
    } else {
      event <- paste("'id': '", data_event$id, "'", ", ",
                     "'.nonce': '", data_event$.nonce, "'", ", ",
                     "'group': '", data_event$group, "'", ", ",
                     "'lat': '", data_event$lat, "'", ", ",
                     "'lng': '", data_event$lng, "'", ", ",
                     "'example': true",
                     sep = "")
      if (details == "") {
        details <- getCountyNameFromFips(data_event$id)
      } else {
        details <- paste(details, getCountyNameFromFips(data_event$id),
                         sep = "<br/>")
      }
    }
    
    
    entry <- data.frame(
      id = entry_id,
      time = Sys.time(),
      colorBy = data$colorBy,
      selectBy = data$selectBy,
      attribute_range_low = attribute_range_low,
      attribute_range_high = attribute_range_high,
      show_county_arrows = data$show_county_arrows,
      event = event,
      view = data$view,
      in_out = data$in_out,
      details_mode = data$details_mode,
      # norm = data$norm,
      num_top_counties = data$num_top_counties,
      title = input$snapshot_submission_title,
      text = input$snapshot_submission_comments,
      details = details
    )
    
    tryCatch(
      {
        ss %>% sheet_append(entry)
        
        # print(data$colorBy)
        
        # if (is.null(user_map())) {
        #   mapview::mapshot(map_reactive(), file = paste0(getwd(), "/map.png"))
        # } else {
        #   mapview::mapshot(user_map(), file = paste0(getwd(), "/map.png"))
        # }
        
        shinyjs::hide(id="snapshot_submission_panel")
        updateTextInput(session, "snapshot_submission_title", value = "")
        updateTextAreaInput(session, "snapshot_submission_comments", value = "")
        
        # Text popup
        runjs("Shiny.onInputChange('example_panel', 'yes');")
        runjs("resetExampleLocation();")
        output$example_name <- renderText({
          "Success"
        })
        output$example_text <- renderText({
          "Snapshot saved."
        })
        output$example_img <- renderText({
          ''
        })
        
        updateTableData()
    },
      error = function(e) {
        print("Error adding to spreadsheet")
        
        # Text popup
        runjs("Shiny.onInputChange('example_panel', 'yes');")
        runjs("resetExampleLocation();")
        output$example_name <- renderText({
          "ERROR"
        })
        output$example_text <- renderText({
          "Failed to save snapshot. Try again, or reload the site if the problem persists."
        })
        output$example_img <- renderText({
          ''
        })

      } # end error function
    ) # end tryCatch
    
  })  
  
  observeEvent(input$current_id, {
    hideSnapshotsPanel()
    
    # print(input$current_id)
    # updateNavbarPage(session = session, "nav",
    #                  selected = "snapshots")
    
    
    
    data <- isolate(table_data())
    data_clicked <- data %>% filter(id == isolate(input$current_id))
    # print()
    
    runjs("Shiny.onInputChange('demo_show', null);")
    runjs("Shiny.onInputChange('help_skip', 'yes');")
    # runjs("Shiny.onInputChange('example_panel', null);")
    
    # Text popup
    if (is.na(data_clicked$text) || data_clicked$text != "") {
      runjs("Shiny.onInputChange('example_panel', 'yes');")
      runjs("resetExampleLocation();")
      if (is.na(data_clicked$title) || data_clicked$title == "") {
        popupText <- "Snapshot"
      } else {
        popupText <- paste("Snapshot:", data_clicked$title)
      }
      output$example_name <- renderText({
        popupText
      })
      
      output$example_text <- renderText({
        data_clicked$text
      })
      
      output$example_img <- renderText({
        ''
      })
    } else {
      runjs("Shiny.onInputChange('example_panel', null);")
    }

    # Color
    reset_color_selection(unlist(data_clicked$colorBy))
    updateSelectInput(session = session,
                      "color", selected = unlist(data_clicked$colorBy))
    
    # Mode
    if (data_clicked$selectBy == "individual") {
      updateRadioButtons(session = session,
                         "choose_by", selected = "individual")
      delay(1000, {})
    } else {
      updateRadioButtons(session = session,
                         "choose_by", selected = "attribute")
      updateSelectInput(session = session,
                        "select_by", selected = data_clicked$selectBy)
      if (data_clicked$selectBy == "code") {
        # print(unlist(data_clicked$attribute_range_low))
        # updateSliderTextInput(session = session,
        #                       "code", selected = unlist(data_clicked$attribute_range_low))
        updateSelectInput(session = session,
                          "code_select", selected = unlist(data_clicked$attribute_range_low))
      } else {
        # updateSliderInput(session = session,
        #                   data_clicked$selectBy, value = c(data_clicked$attribute_range_low,
        #                                                    data_clicked$attribute_range_high))
        updateNumericInput(session = session,
                          paste0(data_clicked$selectBy, "_A"), value = data_clicked$attribute_range_low)
        updateNumericInput(session = session,
                           paste0(data_clicked$selectBy, "_B"), value = data_clicked$attribute_range_high)
      }
    }
    
    # View
    updateRadioButtons(session = session,
                       "view", selected = data_clicked$view)
    
    # Arrows?
    if (data_clicked$show_county_arrows) {
      # arrows <- "Yes"
      arrows <- TRUE
    } else {
      # arrows <- "No"
      arrows <- FALSE
    }
    # updateRadioButtons(session = session,
    #                    "show_arrows", selected = arrows)
    updateCheckboxInput(session = session,
                        "show_arrows_alt", value = arrows)
    
    # Number of counties
    delay(50, {
      # updateSliderInput(session = session,
      #                   "num", value = data_clicked$num_top_counties)
      updateNumericInput(session = session,
                        "new_num_counties", value = data_clicked$num_top_counties)
    })
    
    # Click event and direction
    if (data_clicked$event == "NULL") {
      # Click event
      deselectAll(hide_example_panel = FALSE)
      clicked_county(NULL)
      # Direction
      updateSelectInput(session = session,
                        "in_out",# choices = flow_vars_agg,
                        selected = data_clicked$in_out)
    } else {
      # Click event
      # runjs("Shiny.setInputValue('details_mode', null);")
      runjs("Shiny.onInputChange('myEvent', 'open');")
      runjs(paste0("Shiny.setInputValue('map_shape_click', {", data_clicked$event, "});"))
      # Direction
      reset_in_out_selection(data_clicked$in_out)
      updateSelectInput(session = session,
                        "in_out", 
                        selected = data_clicked$in_out)
    }
    
    
    # # Change tab
    # delay(500, {
    #   runjs('$($("#nav a")[0]).tab("show");')
    # })
    
  })
  
  
  ## DATA DOWNLOAD ##############################
  
  dataCleaner <- function() {
    d <- countydata_for_download()
    d <- d %>%
      select(GEOID, NAMELSAD, state, exemptions, exemptions_net, efficiency) %>%
      rename(fips = GEOID, name = NAMELSAD) %>%
      sf::st_drop_geometry()
    return(d)
  }
  
  output$download_data <- downloadHandler(
    filename = function(){"migration_data.csv"}, 
    content = function(fname){
      write.csv(dataCleaner(), fname)
    }
  )
  
  
  ## EXTRA JAVASCRIPT ###########################
  # runjs("function get_id(clicked_id) {Shiny.setInputValue('current_id', clicked_id, {priority: 'event'});}")
  # runjs("var x = $('.selectize-input')[3]; console.log(x); x.addEventListener('click', function() { var y = $('.selectize-dropdown')[3]; y.scrollIntoView({ behavior: 'smooth' }); });")
  # runjs("var x = $('#color').siblings('.selectize-control').children('.selectize-input')[0]; console.log(x); x.addEventListener('click', function() { var y = $('#color').siblings('.selectize-control').children('.selectize-dropdown')[0]; y.scrollIntoView({ behavior: 'smooth' }); });")
  
  # runjs("var x = $('#color').parent().children('.selectize_control'); console.log(x); x.addEventListener('click', function() { var y = $('#color')[0].parentNode.childNodes[2].childNodes[0]; y.scrollIntoView({ behavior: 'smooth' }); });")
  # runjs("var x = $('.selectize-input')[1]; console.log(x); x.addEventListener('click', function() { var y = $('.selectize-dropdown')[1]; y.scrollIntoViewIfNeeded({ behavior: 'smooth' }); });")

}
