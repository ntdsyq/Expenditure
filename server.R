

states = geojsonio::geojson_read("data/gz_2010_us_040_00_500k_noPR.json", what = "sp")
names(states) = tolower(names(states))
states@data <- states@data %>% rename( state_id = state, state = name )
states@data$state <- as.character(states@data$state)

shinyServer(function(input, output){
    bins = reactiveValues()
    labs = reactiveValues()
    ltitle = reactiveValues()
    vaxistxt = reactiveValues()
    
    # Reactive Data For National Trends by Spend Category Line Chart
    nat1_df_chart = reactive({
        req(input$time1)
        req(input$cat1)
        req(input$metric1)
        
        nat1_df %>% filter( year >= input$time1[1] &
                       year <= input$time1[2] & metric == input$metric1 ) %>% select(year, input$cat1) 
        
    })
    
    nat2_df_chart = reactive({
        req(input$time2)
        req(input$sofund2)
        req(input$metric2)
        
        nat2_df %>% filter( year >= input$time2[1] &
                                year <= input$time2[2] & metric == input$metric2 ) %>% select(year, input$sofund2) 
        
    })
    
    # National Trends by Source of Funding Bar Chart with ggplot2 and plotly
    output$nat2_gglot <- renderPlotly({
        plotdata2 = gather(nat2_df_chart(),key = Fund.Type, value = metric.value, input$sofund2)
        
        g <- ggplot(data = plotdata2, aes(x = year,y = metric.value)) +
            geom_col(aes(fill = Fund.Type) ) + 
            ylab("Metric Value") + theme_bw()
            # theme(axis.title.x = element_text(face= "bold", size = 14),
            #       axis.title.y = element_text(face= "bold", size = 14),
            #       axis.text.x = element_text(size = 12),
            #       axis.text.y = element_text(size = 12),
            #       legend.text = element_text(size = 14),
            #       legend.title = element_text(size = 14)) +
            guides(fill=guide_legend(title="Fund Type"))
        #labs(fill = "Fund Type")  or scale_fill_discrete(name = "Fund Type") works too
        
        ggplotly(g, tooltip = c("year","metric.value")) 
    })
    
    observe({
        if (input$metric1 == "PctOf_GDP"){
            vaxistxt$h = "{title: 'Metric Value', titleTextStyle: {fontSize: 16} , format: 'percent' }"
        } else{
            vaxistxt$h = "{title: 'Metric Value', titleTextStyle: {fontSize: 16} }"
        }
    })
    
    # National Trends by Spend Category Line Chart with googlevis
    output$nat1_gvis <- renderGvis({
        gvisLineChart(
            nat1_df_chart(),
            options = list(
                title ="US Personal Healthcare Expenditure by Category",
                width = "automatic",
                height = "500px",
                chartArea = "{left: '100', right:'180'}",
                vAxis = vaxistxt$h,
                hAxis = "{title: 'Year', titleTextStyle: {fontSize: 16} }",
                animation = "{startup: true}",
                legend = "{textStyle: {fontSize: 12}}"
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
        st_df_select <- st_df %>% filter( year == input$year3 & category == input$cat3 ) %>%
            select(state, input$metric3)
    })
    
    observe({
        bins$b = quantile(st_df_map()[,input$metric3], probs = seq(0,1,0.2) )
        if (input$metric3 == "percap_dollars" ){
            labs$a = "<strong>%s</strong><br/>%g $ per capita"
            ltitle$t = "USD per capita"
        } else if (input$metric3 == "total_dollars_millions") {
            labs$a = "<strong>%s</strong><br/>%g million $" 
            ltitle$t = "Total spend (Millions $)"
             
        } else if (input$metric3 == "population"){    
            labs$a = "<strong>%s</strong><br/>%g millions people"
            ltitle$t = "Population (millions)"
            
        } else if (input$metric3 == "percap_lq"){
            labs$a = "<strong>%s</strong><br/>%g relative to national"
            ltitle$t = "Per capita spend relative to nation"
        }
        
    })
    
    output$usmap = renderLeaflet({
        states0 = states
        states0@data <- left_join(states0@data, st_df_map(), by="state")
        
        pal <- colorBin("BuPu", domain = states0@data[,input$metric3], bins = bins$b)
        
        
        labels <- sprintf(labs$a, states0$state, 
                          states0@data[,input$metric3]
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
                fillColor = ~pal(states0@data[,input$metric3]),
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
            addLegend(pal = pal, values = ~states0@data[,input$metric3], opacity = 0.7, title = ltitle$t,
                      position = "bottomleft")
        
    })
    
    
})