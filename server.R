function(input, output) {
  output$data <- renderDataTable({
                    datatable(airbnb_clean, 
                              options = list(scrollX = T, 
                                             pageLength = 5),
                              colnames = c(
                                'Headline' = 2,
                                'Host ID' = 3,
                                'Host Name' = 4,
                                'Neighbourhood Group' = 5,
                                'Neighbourhood' = 6,
                                'Lat' = 7,
                                'Long' = 8,
                                'Room Type' = 9,
                                'Price' =10,
                                'Min. Nights' = 11,
                                'Num. Reviews' = 12,
                                'Last Review' = 13,
                                'Reviews/Month' =14,
                                'Host Listing Count' = 15,
                                'Availability' = 16
                              ))
                    })
  
  # output$plot_corr <- renderPlotly({
  #                     pilih_var <- input$num %>% 
  #                               str_to_lower() %>% 
  #                               str_replace_all(pattern = " ", replacement = "_")
  #                     
  #                     plot_corr_gg <- airbnb_clean_1000 %>% 
  #                                         ggplot(aes_string(x= "price",
  #                                                           y= pilih_var
  #                                                           )
  #                                                ) +
  #                                         geom_jitter(aes(col = room_type,
  #                                                         text = glue("Price: {price}
  #                                                                Neighbourhood: {neighbourhood_group}
  #                                                                Room Type: {room_type}"),
  #                                                         alpha = 0.2, 
  #                                                         size =0.2
  #                                                         ),
  #                                                     show.legend = F
  #                                                     ) +
  #                                         geom_smooth() +
  #                                         labs(y = glue("{input$num}"),
  #                                              x = "Price",
  #                                             title = glue("Correlation Plot of Price and {input$num}")) +
  #                                         scale_color_brewer(palette = "Dark2") +
  #                                         theme
  #                         
  #                       ggplotly(plot_corr_gg, tooltip = "text") %>% 
  #                         layout(margin=m)
  #                     })
  
  output$plot_map <- renderLeaflet({
                        airbnb_spatial <- airbnb_clean %>% 
                                            select(longitude, 
                                                   latitude, 
                                                   neighbourhood_group, 
                                                   neighbourhood, 
                                                   price, 
                                                   room_type,
                                                   minimum_nights,
                                                   est_revenue) %>% 
                                            filter((price >= as.integer(input$pr1[1]) & price <= as.integer(input$pr1[2])),
                                                   room_type == as.character(input$cat1),
                                                   minimum_nights == as.character(input$mn1))
                        
                        getColor <- function(x) {
                          sapply(airbnb_spatial$price, function(price){
                            if (price <= 100) {
                              "green"
                            } else if (price > 100 & price <=300) {
                              "yellow"
                            } else {
                              "red"
                            }
                          })
                        }
                        
                        icons <- awesomeIcons(
                          library = "ion",
                          text = glue("${airbnb_spatial$price}"),
                          iconColor = "black",
                          markerColor = getColor(airbnb_spatial$price)
                        )
                        
                        leaflet(airbnb_spatial) %>% 
                          setView(lng = mean(airbnb_spatial$longitude), lat = mean(airbnb_spatial$latitude), zoom = 10) %>% 
                          addTiles() %>% 
                          addAwesomeMarkers(label= glue("Daily Rent Price: {airbnb_spatial$price},
                                              Neighbourhood: {airbnb_spatial$neighbourhood_group},
                                              Est. Revenue: {airbnb_spatial$est_revenue}"),
                                            labelOptions = labelOptions(textsize = "12px"),
                                            icon = icons,
                                            clusterOptions = markerClusterOptions()) 
                     })
  
  # output$boxplot_rt <- renderPlotly({
  #                               box_plot <- ggplot(airbnb_clean_1000, 
  #                                                  aes(x=room_type, 
  #                                                      y=price,
  #                                                      )
  #                                                  ) +
  #                                 geom_boxplot(aes(fill=room_type)) +
  #                                 geom_hline(yintercept = median(airbnb_clean$price), 
  #                                            col="firebrick", 
  #                                            linetype =2
  #                                            ) +
  #                                 scale_y_continuous(n.breaks = 10) +
  #                                 labs( x = NULL,
  #                                       y = "Price",
  #                                       title = "Pricing Summary by Room Type") +
  #                                 theme +
  #                                 theme(legend.position = "none")
  #                               
  #                               ggplotly(box_plot) %>% 
  #                                 layout(margin=m)
  #                         })
  
  output$boxplot_nb <- renderPlotly({
                                      select_cat <- input$cat %>% 
                                        str_to_lower() %>% 
                                        str_replace_all(pattern = " ", replacement = "_")
                                      
                                      box_plot_price <-  airbnb_clean_1000 %>% 
                                                      ggplot(aes_string(y= "price",
                                                                        x= select_cat
                                                                        )
                                                            ) +
                                                      geom_boxplot(aes(fill=select_cat)) +
                                                      geom_hline(yintercept = median(airbnb_clean$price), 
                                                                 col="firebrick", 
                                                                 linetype =2
                                                      ) +
                                                      scale_y_continuous(n.breaks = 10) +
                                                      scale_x_discrete(labels = wrap_format(10)) +
                                                      labs( x = NULL,
                                                            y = "Price",
                                                            title = glue("Pricing by {input$cat}")) +
                                                      theme +
                                                      theme(legend.position = "none")
                                      
                                      ggplotly(box_plot_price) %>% 
                                        layout(margin=m)
  })
  
  output$boxplot_rt <- renderPlotly({
                                      select_cat2 <- input$cat %>%
                                        str_to_lower() %>%
                                        str_replace_all(pattern = " ", replacement = "_")
                                      
                                      box_plot_mn <-  airbnb_mn_45%>% 
                                        ggplot(aes_string(y= "minimum_nights",
                                                          x= select_cat2
                                              )
                                        ) +
                                        geom_boxplot(aes(fill=select_cat2)) +
                                        geom_hline(yintercept = median(airbnb_mn_45$minimum_nights), 
                                                   col="firebrick", 
                                                   linetype =2
                                        ) +
                                        scale_y_continuous(n.breaks = 10) +
                                        scale_x_discrete(labels = wrap_format(10)) +
                                        labs( x = NULL,
                                              y = "Min. Nights",
                                              title = glue("Min. Nights by {input$cat}")) +
                                        theme +
                                        theme(legend.position = "none")
                                      
                                      ggplotly(box_plot_mn) %>% 
                                        layout(margin=m)
  })
  
  output$reviews_pricing_plot <- renderPlotly({
                                    corr_plot <- ggplot(airbnb_clean_1000,
                                                        aes(x= number_of_reviews, 
                                                            y=price)) +
                                                    geom_point(aes(size=reviews_per_month,
                                                                   fill=room_type), 
                                                               alpha=0.2) +
                                                    labs(x = "Number of Reviews",
                                                         y = "Price" 
                                                        )+
                                                    theme +
                                                    theme(legend.position = "none")
                                    
                                    ggplotly(corr_plot) %>% 
                                      layout(margin=m)
  })
  
  output$pr_words <- renderPlot({
                                wc_pr_words <- airbnb_clean %>% 
                                  filter(room_type == "Private room",
                                         price < 300,
                                         number_of_reviews > 5) %>% 
                                  select(name)
                                
                                wordcloud(words = wc_pr_words$name, 
                                          min.freq = 2, 
                                          max.words = 200, 
                                          color=brewer.pal(8, "Dark2"),
                                          random.order = F,
                                          rot.per = 0.35)
                                
                                # ggplotly(gg_pr_words)
  })
  
  output$apt_words <-  renderPlot({
                                    wc_apt_words <- airbnb_clean %>% 
                                      filter(room_type == "Entire home/apt",
                                             number_of_reviews > 2) %>% 
                                      select(name)
                                    
                                    wordcloud(words = wc_apt_words$name, 
                                              min.freq = 2, 
                                              max.words = 200, 
                                              color=brewer.pal(8, "Dark2"),
                                              random.order = F,
                                              rot.per = 0.35)
                                    
                                    # ggplotly(gg_apt_words)
  })
  
  output$mn_plot <- renderPlotly({
                                  mn_box_plot <- ggplot(airbnb_mn_45, aes(x=room_type, y=minimum_nights)) +
                                    geom_boxplot(aes(fill=room_type),show.legend = F) +
                                    geom_hline(yintercept = median(airbnb_mn_45$minimum_nights), col="firebrick", linetype =2) +
                                    scale_y_continuous(breaks=seq(0,45,5)) + 
                                    labs( x = NULL,
                                          y = "Minimum Nights") +
                                    theme+
                                    theme(legend.position = "none")
                                  
                                  ggplotly(mn_box_plot) %>% 
                                    layout(margin=m)
  })
  
  output$corr_col <- renderPlotly({
                          corr_plot <- ggplot(data=airbnb_corr_clean_melt, mapping = aes(x=Var1, 
                                                                                         y=Var2, 
                                                                                         fill=value,
                                                                                         text = glue("Corr value: {round(value,2)}"))) +
                            geom_tile() +
                            scale_fill_gradient(low = "#1f0101", high = "#fc0303") +
                            labs(x = NULL,
                                 y = NULL) +
                            scale_x_discrete(labels = wrap_format(10)) +
                            scale_y_discrete(labels = wrap_format(30)) +
                            theme
                          
                          ggplotly(corr_plot, tooltip = "text")  %>% 
                            layout(margin=m)
  })
  
  output$top_40_plot <- renderPlotly({
                          top_40_plot <- ggplot(top_40, aes(y=reorder(host_name, est_total_revenue), 
                                                            x=est_total_revenue,
                                                            text = glue("Est. Revenue: {est_total_revenue},
                                                     Property Count: {total_properties}"))) +
                            geom_col(aes(fill=est_total_revenue)) +
                            scale_x_continuous(labels = dollar_format()) +
                            scale_fill_gradient(low = "#1f0101", high = "#fc0303") +
                            labs(x = NULL,
                                 y = NULL) +
                            scale_y_discrete(labels = wrap_format(25)) +
                            theme +
                            theme(legend.position = "none")
                          
                          ggplotly(top_40_plot, tooltip = "text")
  })
  
  output$top_rev_map <- renderLeaflet({
    getColor2 <- function(x) {
      sapply(host_rev_10000$est_total_revenue, function(rev){
        if (rev >= 1000000) {
          "green"
        } else if (rev >= 500000 & rev <1000000) {
          "yellow"
        } else {
          "red"
        }
      })
    }
    
    icons <- awesomeIcons(
      library = "ion",
      iconColor = "black",
      markerColor = getColor2(host_rev_10000$est_total_revenue)
    )
    
    leaflet(host_rev_10000) %>% 
      setView(lng = mean(host_rev_10000$longitude), lat = mean(host_rev_10000$latitude), zoom = 10) %>% 
      addTiles() %>% 
      addAwesomeMarkers(label= glue("Daily Rent Price: {host_rev_10000$price},
                                      Neighbourhood: {host_rev_10000$neighbourhood_group},
                                      Room Type: {host_rev_10000$room_type}"),
                        labelOptions = labelOptions(textsize = "12px"),
                        icon = icons,
                        clusterOptions = markerClusterOptions())
  })
        
  output$neighbourhood_map <- renderLeaflet({
                                airbnb_neighbourhood <- airbnb_clean %>% 
                                  filter(room_type == input$rt_neigh_id,
                                         neighbourhood == input$neigh_id )%>% 
                                  select(longitude, 
                                         latitude, 
                                         neighbourhood, 
                                         price, 
                                         room_type,
                                         minimum_nights,
                                         est_revenue) 
          
                                  getColor3 <- function(x) {
                                    sapply(airbnb_neighbourhood$price, function(price){
                                      if (price <= 100) {
                                        "green"
                                      } else if (price > 100 & price <=300) {
                                        "yellow"
                                      } else {
                                        "red"
                                      }
                                    })
                                  }
                                  
                                  icons <- awesomeIcons(
                                    library = "ion",
                                    text = glue("${airbnb_neighbourhood$price}"),
                                    iconColor = "black",
                                    markerColor = getColor3(airbnb_neighbourhood$price)
                                  )
                                  
                                  leaflet(airbnb_neighbourhood) %>% 
                                    setView(lng = mean(airbnb_neighbourhood$longitude), lat = mean(airbnb_neighbourhood$latitude), zoom = 15) %>% 
                                    addTiles() %>% 
                                    addAwesomeMarkers(label= glue("Daily Rent Price: {airbnb_neighbourhood$price},
                                                                   Est. Revenue: {airbnb_neighbourhood$est_revenue}"),
                                                      labelOptions = labelOptions(textsize = "12px"),
                                                      icon = icons
                                                      
                                                      ) 
  })
  
  output$price_filter <- renderText({
                            pricing_neigh <- airbnb_clean %>% 
                              filter(room_type == input$rt_neigh_id,
                                     neighbourhood == input$neigh_id )%>% 
                              select(price) 
                            
                            median(pricing_neigh$price)
  })
  
  output$mn_filter <- renderText({
                            mn_neigh <- airbnb_clean %>% 
                              filter(room_type == input$rt_neigh_id,
                                     neighbourhood == input$neigh_id )%>% 
                              select(minimum_nights) 
                            
                            median(mn_neigh$minimum_nights)
  })
  
  output$rev_filter <- renderText({
                            rev_neigh <- airbnb_clean %>% 
                              filter(room_type == input$rt_neigh_id,
                                     neighbourhood == input$neigh_id )%>% 
                              select(est_revenue) 
                            
                            mean(rev_neigh$est_revenue)
  })

}