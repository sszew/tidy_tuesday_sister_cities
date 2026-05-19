# Author: Scottie Szewczyk
# Tidy Tuesday: Sister Cities (May 12, 2026)
# Source: https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-05-12
# Created with the assistance of Claude Sonnet 4.6 through Posit Assistant in RStudio.

library(shiny)
library(bslib)
library(leaflet)
library(tidyverse)
library(geosphere)
library(jsonlite)
library(shinyjs)
library(htmltools)

# --- Data loading ---
# Data is loaded once when the app starts
cities   <- read_csv("sistercities.csv")
links    <- read_csv("sistercities_links.csv")

# --- Constants ---

COLOR_SELECTED <- "#F1C40F"  # yellow
COLOR_SISTER   <- "#E74C3C"  # red

# --- Helpers ---

# Generate great-circle arc points between two locations.
# Returns a data frame with columns lon and lat.
# Returns a list of data frames (one per segment). Arcs that cross the
# antimeridian are split into two segments to avoid a horizontal connecting line.
make_arc <- function(lng1, lat1, lng2, lat2, n = 80) {
  tryCatch({
    pts <- gcIntermediate(
      p1 = c(lng1, lat1),
      p2 = c(lng2, lat2),
      n  = n,
      addStartEnd     = TRUE,
      breakAtDateLine = TRUE
    )
    if (is.list(pts)) lapply(pts, as.data.frame) else list(as.data.frame(pts))
  }, error = function(e) {
    list(data.frame(lon = c(lng1, lng2), lat = c(lat1, lat2)))
  })
}

# Leaflet label HTML for a city row
city_label <- function(name, country, continent, is_selected = FALSE) {
  header <- if (is_selected) {
    paste0("<b>", name, "</b> <span style='color:#FFD700;'>(selected)</span>")
  } else {
    paste0("<b>", name, "</b>")
  }
  HTML(paste0(header, "<br/>", country, "<br/><i>", continent, "</i>"))
}

# --- UI ---

ui <- page_sidebar(
  title = "Sister Cities Explorer",
  theme = bs_theme(bootswatch = "darkly"),
  useShinyjs(),
  
  sidebar = sidebar(
    width = 270,
    selectInput(
      "continent", "1. Select a Continent",
      choices = c("— Choose —" = "", sort(unique(cities$continent)))
    ),
    selectInput(
      "country", "2. Select a Country",
      choices = c("— Choose —" = "")
    ),
    selectInput(
      "city", "3. Select a City",
      choices = c("— Choose —" = "")
    ),
    actionButton(
      "find", "Find Sister Cities",
      class = "btn-primary mt-3 w-100",
      icon  = icon("globe")
    )
  ),
  
  layout_columns(
    col_widths = 12,
    card(
      fill = FALSE,
      card_header("World Map"),
      uiOutput("map_title_ui"),
      p(
        id    = "instructions",
        style = "color: #aaa; font-style: italic; margin-bottom: 6px;",
        "Use the sidebar to select a continent, country, and city, then click",
        strong("Find Sister Cities"), "to visualize connections on the map."
      ),
      leafletOutput("map", height = "520px")
    ),
    card(
      id    = "result_card",
      style = "display: none;",
      card_header("Sister Cities"),
      tableOutput("city_table")
    )
  )
)

# --- Server ---

server <- function(input, output, session) {
  
  # Cascade: continent -> country
  observeEvent(input$continent, ignoreInit = TRUE, {
    req(input$continent)
    countries <- cities |>
      filter(continent == input$continent) |>
      pull(country) |>
      unique() |>
      sort()
    updateSelectInput(session, "country",
                      choices = c("— Choose —" = "", countries)
    )
    updateSelectInput(session, "city",
                      choices = c("— Choose —" = "")
    )
  })
  
  # Cascade: country -> city
  observeEvent(input$country, ignoreInit = TRUE, {
    req(input$country)
    city_df <- cities |>
      filter(country == input$country) |>
      arrange(name)
    updateSelectInput(session, "city",
                      choices = c("— Choose —" = "", setNames(city_df$id, city_df$name))
    )
  })
  
  # --- Initial map: all cities greyed out ---
  output$map <- renderLeaflet({
    leaflet(cities) |>
      addProviderTiles(providers$CartoDB.DarkMatter) |>
      addCircleMarkers(
        lng         = ~lng,
        lat         = ~lat,
        radius      = 3.5,
        fillColor   = "#777777",
        fillOpacity = 0.45,
        stroke      = FALSE
      ) |>
      setView(lng = 10, lat = 20, zoom = 2)
  })
  
  # --- Sister city lookup (fires on button click) ---
  sister_result <- eventReactive(input$find, {
    req(input$city, nchar(input$city) > 0)
    
    sel_id <- input$city
    
    sister_ids <- links |>
      filter(source == sel_id | target == sel_id) |>
      mutate(sister_id = if_else(source == sel_id, target, source)) |>
      pull(sister_id) |>
      unique()
    
    list(
      sel_id     = sel_id,
      sister_ids = sister_ids,
      sel_city   = cities |> filter(id == sel_id),
      sis_cities = cities |> filter(id %in% sister_ids),
      other      = cities |> filter(!id %in% c(sel_id, sister_ids))
    )
  })
  
  # --- Update map after button click ---
  observeEvent(input$find, {
    req(sister_result())
    sr <- sister_result()
    
    sel_city  <- sr$sel_city
    sis       <- sr$sis_cities
    other     <- sr$other
    
    proxy <- leafletProxy("map")
    proxy |> clearMarkers() |> clearShapes()
    
    # Background cities (greyed out)
    proxy |> addCircleMarkers(
      data        = other,
      lng         = ~lng,
      lat         = ~lat,
      radius      = 3,
      fillColor   = "#555555",
      fillOpacity = 0.3,
      stroke      = FALSE
    )
    
    # Build arcs
    arc_js_coords <- list()
    
    if (nrow(sis) > 0) {
      for (i in seq_len(nrow(sis))) {
        arc_segs <- make_arc(
          sel_city$lng, sel_city$lat,
          sis$lng[i],   sis$lat[i],
          n = 80
        )
        # Draw each segment separately so date-line crossings don't produce
        # a horizontal connecting line across the map
        for (seg in arc_segs) {
          proxy |> addPolylines(
            lng     = seg$lon,
            lat     = seg$lat,
            color   = "#CCCCCC",
            weight  = 1.2,
            opacity = 0.65,
            options = pathOptions(interactive = FALSE)
          )
        }
        # Collect [lon, lat] pairs for JS animation (concatenate all segments)
        arc_all_pts <- do.call(rbind, arc_segs)
        arc_js_coords[[i]] <- lapply(seq_len(nrow(arc_all_pts)), function(j) {
          c(arc_all_pts$lon[j], arc_all_pts$lat[j])
        })
      }
      
      # Sister city markers with hover labels
      sis_labels <- lapply(seq_len(nrow(sis)), function(i) {
        city_label(sis$name[i], sis$country[i], sis$continent[i])
      })
      
      proxy |> addCircleMarkers(
        data         = sis,
        lng          = ~lng,
        lat          = ~lat,
        radius       = 6,
        fillColor    = COLOR_SISTER,
        fillOpacity  = 0.9,
        color        = "#FFFFFF",
        weight       = 1,
        stroke       = TRUE,
        label        = sis_labels,
        labelOptions = labelOptions(
          style     = list("font-family" = "sans-serif", "padding" = "4px 8px"),
          textsize  = "13px",
          direction = "auto"
        )
      )
    }
    
    # Selected city marker
    proxy |> addCircleMarkers(
      data         = sel_city,
      lng          = ~lng,
      lat          = ~lat,
      radius       = 9,
      fillColor    = COLOR_SELECTED,
      fillOpacity  = 1,
      color        = "#FFFFFF",
      weight       = 2.5,
      stroke       = TRUE,
      label        = city_label(sel_city$name, sel_city$country, sel_city$continent, is_selected = TRUE),
      labelOptions = labelOptions(
        style     = list("font-family" = "sans-serif", "font-weight" = "bold", "padding" = "4px 8px"),
        textsize  = "14px",
        direction = "auto"
      )
    )
    
    # Reveal result table
    shinyjs::show("result_card")
    
    # --- Arc animation (pure client-side JS) ---
    if (length(arc_js_coords) > 0) {
      arc_json <- toJSON(arc_js_coords, auto_unbox = FALSE)
      
      js <- sprintf('
        (function () {
          // Stop any prior animation loop via shared flag
          window._animRunning = false;

          // Remove prior animated dots
          if (window._animMarkers) {
            try {
              var m = HTMLWidgets.getInstance(document.getElementById("map")).getMap();
              window._animMarkers.forEach(function (mk) { m.removeLayer(mk); });
            } catch(e) {}
          }
          window._animMarkers = [];

          var allArcs = %s;
          var mapObj  = HTMLWidgets.getInstance(document.getElementById("map")).getMap();

          // Small delay so leafletProxy redraws finish first
          setTimeout(function () {
            window._animRunning = true;

            allArcs.forEach(function (arcPts, idx) {
              var n     = arcPts.length;
              var t     = idx / allArcs.length; // stagger start positions
              var dir   = 1;
              var speed = 0.0035;

              var dot = L.circleMarker(
                [arcPts[0][1], arcPts[0][0]],
                { radius: 3, fillColor: "white", fillOpacity: 0.95, color: "white", weight: 0 }
              ).addTo(mapObj);
              window._animMarkers.push(dot);

              (function step() {
                if (!window._animRunning) return;
                t += speed * dir;
                if      (t >= 1) { t = 1; dir = -1; }
                else if (t <= 0) { t = 0; dir =  1; }
                var i = Math.round(t * (n - 1));
                dot.setLatLng([arcPts[i][1], arcPts[i][0]]);
                requestAnimationFrame(step);
              })();
            });
          }, 150);
        })();
      ', arc_json)
      
      shinyjs::runjs(js)
    }
  })
  
  # --- Map title ---
  output$map_title_ui <- renderUI({
    req(sister_result())
    city_name <- sister_result()$sel_city$name
    h4(
      paste("Sister Cities for", city_name),
      style = "margin: 0 0 6px 0; font-weight: 600;"
    )
  })
  
  # --- Table ---
  output$city_table <- renderTable({
    req(sister_result())
    sr <- sister_result()
    
    if (length(sr$sister_ids) == 0) {
      return(data.frame(Note = "No sister cities found for this city in the dataset."))
    }
    
    sel  <- cities |> filter(id == sr$sel_id)
    sis  <- cities |> filter(id %in% sr$sister_ids)
    
    # Haversine distance from selected city to each sister city (meters -> miles)
    sis_dist <- distHaversine(
      p1 = c(sel$lng, sel$lat),
      p2 = cbind(sis$lng, sis$lat)
    ) / 1609.344
    
    bind_rows(
      sel |> mutate(Role = "Selected City", `Distance (miles)` = 0),
      sis |> mutate(Role = "Sister City",   `Distance (miles)` = sis_dist)
    ) |>
      arrange(`Distance (miles)`) |>
      select(Role, City = name, Country = country, Continent = continent, `Distance (miles)`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 1)
}

shinyApp(ui, server)
