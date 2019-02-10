

states = geojsonio::geojson_read("data/gz_2010_us_040_00_500k_noPR.json", what = "sp")
names(states) = tolower(names(states))
states@data <- states@data %>% rename( state_id = state, state = name )
states@data$state <- as.character(states@data$state)

shinyServer(function(input, output){
    bins = reactiveValues()
    labs = reactiveValues()
    ltitle = reactiveValues()
    
    # Reactive Data For National Trends by Spend Category Line Chart
    nat1_df_chart = reactive({
        req(input$time1)
        req(input$cat1)
        req(input$metric1)
        
        nat1_df %>% filter( year >= input$time1[1] &
                       year <= input$time1[2] & metric == input$metric1 ) %>% select(year, input$cat1) 
        
    })
    
    # National Trends by Spend Category Line Chart with googlevis
    output$nat1_gvis <- renderGvis({
        gvisLineChart(
            nat1_df_chart(),
            options = list(
                title ="US Personal Healthcare Expenditure by Category",
                width = "automatic",
                height = "500px",
                vAxis = "{title: 'metric' }",
                hAxis = "{title: 'Year'}",
                animation = "{startup: true}"
            )
        )
    })
    
    # National Trends by Spend Category Line Chart with ggplot2
    # output$nat1_gglot <- renderPlot({
    #     plotdata1 = gather(nat1_df_chart(),key = category, value = value, input$cat1)
    #     
    #     ggplot(data = plotdata1, aes(x = year,y = value, color = category)) + 
    #         geom_line( )
    # })
    
    st_df_map = reactive({
        st_df_select <- st_df %>% filter( year == input$year2 & category == input$cat2 ) %>%
            select(state, input$metric2)
    })
    
    observe({
        bins$b = quantile(st_df_map()[,input$metric2], probs = seq(0,1,0.2) )
        if (input$metric2 == "percap_dollars" ){
            labs$a = "<strong>%s</strong><br/>%g $ per capita"
            ltitle$t = "USD per capita"
        } else if (input$metric2 == "total_dollars_millions") {
            labs$a = "<strong>%s</strong><br/>%g million $" 
            ltitle$t = "Total spend (Millions $)"
             
        } else if (input$metric2 == "population"){    
            labs$a = "<strong>%s</strong><br/>%g millions people"
            ltitle$t = "Population (millions)"
            
        } else if (input$metric2 == "percap_lq"){
            labs$a = "<strong>%s</strong><br/>%g relative to national"
            ltitle$t = "Per capita spend relative to nation"
        }
        
    })
    
    output$usmap = renderLeaflet({
        states0 = states
        states0@data <- left_join(states0@data, st_df_map(), by="state")
        
        pal <- colorBin("BuPu", domain = states0@data[,input$metric2], bins = bins$b)
        
        
        labels <- sprintf(labs$a, states0$state, 
                          states0@data[,input$metric2]
        ) %>% lapply(htmltools::HTML)
        
        
        leaflet(states0) %>%
            setView(-96, 37.8, 4) %>%
            addProviderTiles(
                "MapBox", 
                options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))
                ) %>% 
            addPolygons(
                fillColor = ~pal(states0@data[,input$metric2]),
                weight = 2,
                opacity = 3,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 4,
                    color = "grey",   
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "14px",
                    direction = "auto")
            ) %>%
            addLegend(pal = pal, values = ~states0@data[,input$metric2], opacity = 0.7, title = ltitle$t,
                      position = "bottomleft")
        
    })
    
    
})