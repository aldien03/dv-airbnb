header <- dashboardHeader(title = "Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(menuItem(text = "Summary",
                       tabName = "db-id",
                       icon = icon("airbnb")
                       # ,
                       # badgeLabel = "HOT",
                       # badgeColor = "red"
                       ),
              # menuItem(text = "Room Type",
              #          tabName = "rt-id",
              #          icon = icon("hotel")),
              # menuItem(text = "Correlation with Price",
              #          tabName = "corr-id",
              #          icon = icon("dashboard")),
              menuItem(text = "Modelling Success",
                       tabName = "words-id",
                       icon = icon("angle-double-up"),
                       badgeLabel = "HOT",
                       badgeColor = "red"
                       ),
              menuItem(text = "Data",
                       tabName = "data-id",
                       icon = icon("database")
                       )
              )
)

body <- dashboardBody(
          tabItems(
            tabItem(
                      tabName = "db-id",
                      h2("AirBNB Data Analytics for Property Investor"),
                      h6("Data: 2019, New York (pre-pandemic)"),
                      fluidRow(valueBox(nrow(airbnb_clean), 
                                        "Number of Properties", 
                                        icon = icon("airbnb"), 
                                        color = "red"),
                               valueBox(round(median(airbnb_clean$price),2), 
                                        "Overall Median Price per Day", 
                                        icon = icon("credit-card"),
                                        color = "red"),
                               valueBox(round(median(airbnb_clean$minimum_nights), 2), 
                                        "Overall Median Min. Nights", 
                                        icon = icon("adjust"),
                                        color = "red")
                               ),
                      fluidRow(
                        box(width = 12,
                            title = "Quick Summary",
                            background = "black",
                            column(width = 5,
                                   plotlyOutput("boxplot_nb")
                                   ),
                            column(width = 5,
                                   plotlyOutput("boxplot_rt")
                                   ),
                            column(width = 2,
                                   radioButtons(inputId = "cat",
                                                label="Choose Category",
                                                choices = airbnb_clean%>%
                                                          select(room_type, neighbourhood_group) %>%
                                                          names() %>%
                                                          str_replace_all(pattern = "_", replacement = " ") %>%
                                                          str_to_title(),
                                                ),
                                   h4("Key Takeaway:"),
                                   p("AirBNB users has no problem to be pushed to rent property for more than one day."),
                                   br(),
                                   p("Manhattan is a sweet spot! Higher price and people tends to stay a bit longer. "))
                            )
                      ),
                      fluidRow(
                        box(width=12,
                            background = "black",
                            title = "Room Type Summary",
                            valueBox(round(median(airbnb_apt$price), 2), 
                                     "Median Price - Entire Apt/Home", 
                                     icon = icon("credit-card"),
                                     color = "red",
                            ),
                            valueBox(round(median(airbnb_pvt$price),2), 
                                     "Median Price - Private Room", 
                                     icon = icon("credit-card"),
                                     color = "red",
                            ),
                            valueBox(round(median(airbnb_shr$price), 2), 
                                     "Median Price - Shared Room", 
                                     icon = icon("credit-card"),
                                     color = "red",
                            ),
                            valueBox(round(median(airbnb_apt$minimum_nights),2), 
                                     "Median Min. Nights - Entire Apt/Home", 
                                     icon = icon("adjust"), 
                                     color = "red",
                            ),
                            valueBox(round(median(airbnb_pvt$minimum_nights),2), 
                                     "Median Min. Nights - Private Room", 
                                     icon = icon("adjust"), 
                                     color = "red",
                            ),
                            valueBox(round(median(airbnb_shr$minimum_nights),2), 
                                     "Median Min. Nights - Shared Room", 
                                     icon = icon("adjust"), 
                                     color = "red",
                            )
                        )
                      ),
                      fluidRow(
                        box(width = 12, 
                            title = "Spatial Analysis",
                            background = "black",
                            column(width = 10,
                                   leafletOutput("plot_map")),
                            column(width = 2,
                                   selectInput(inputId = "cat1",
                                               label="Choose Room Type",
                                               choices = airbnb_clean%>%
                                                          select(room_type) %>% 
                                                          unique(),
                                               selected = "Entire home/apt"
                                               ),
                                   numericInput(inputId = "mn1",
                                                label="Choose Min. Nights",
                                                value = 1,
                                                min = 1,
                                                max = 365,
                                                step = 1
                                                ), 
                                   sliderInput(inputId = "pr1",
                                               label="Choose Price Range",
                                               min = min(airbnb$price),
                                               max = max(airbnb$price),
                                               value = c(min(airbnb$price), 300)
                                   ),
                                   submitButton(text = "Apply Changes"),
                                   h4("Are you targetting High Profiles?"),
                                   p("Then Manhattan is the area that you are looking for."),
                                   p("Most of daily rent price between $1.000 to $10.000 are located there.")
                                   # selectInput(inputId = "nb1",
                                   #      label="Choose Neighbourhood",
                                   #      choices = airbnb_clean%>%
                                   #        select(neighbourhood_group) %>% 
                                   #        unique(),
                                   #      selected = airbnb_clean[,"neighbourhood_group"]
                                   #      )
                                  
                          ),
                        )
                      ),
                    ),
                   # tabItem(
                   #    tabName = "rt-id",
                      # fluidRow(
                      #   box(width=12,
                      #       background = "black",
                      #       title = "Room Type Summary",
                      #       valueBox(round(median(airbnb_pvt$price),2), 
                      #                "Median Price - Private Room", 
                      #                icon = icon("credit-card"),
                      #                color = "red",
                      #       ),
                      #       valueBox(round(median(airbnb_apt$price), 2), 
                      #                "Median Price - Entire Apt/Home", 
                      #                icon = icon("credit-card"),
                      #                color = "red",
                      #       ),
                      #       valueBox(round(median(airbnb_shr$price), 2), 
                      #                "Median Price - Shared Room", 
                      #                icon = icon("credit-card"),
                      #                color = "red",
                      #       ),
                      #       valueBox(round(median(airbnb_pvt$minimum_nights),2), 
                      #                "Median Min. Nights - Private Room", 
                      #                icon = icon("adjust"), 
                      #                color = "red",
                      #       ),
                      #       valueBox(round(median(airbnb_apt$minimum_nights),2), 
                      #                "Median Min. Nights - Entire Apt/Home", 
                      #                icon = icon("adjust"), 
                      #                color = "red",
                      #       ),
                      #       valueBox(round(median(airbnb_shr$minimum_nights),2), 
                      #                "Median Min. Nights - Shared Room", 
                      #                icon = icon("adjust"), 
                      #                color = "red",
                      #       )
                      #     )
                      # ),
                    #   fluidRow(
                    #     box(width = 12,
                    #         title = "More reviews doesn't translate to higher pricing",
                    #         background = "black",
                    #         plotlyOutput("reviews_pricing_plot")
                    #     )
                    #   ),
                    #   fluidRow(
                    #     box(width = 12,
                    #         title = "AirBNB user has no problem to be pushed book propery for more than one day",
                    #         background = "black",
                    #         plotlyOutput("mn_plot")
                    #     )
                    #   ),
                    # ),
                   # tabItem(
                   #   tabName = "corr-id",
                   #   selectInput(inputId = "num",
                   #               label = "Choose variable",
                   #               choices = airbnb_clean_1000 %>% 
                   #                          select(-c(latitude, longitude, host_id, id, price)) %>% 
                   #                          select_if(is.numeric) %>% 
                   #                          names() %>% 
                   #                          str_replace_all(pattern = "_", replacement = " ") %>%
                   #                          str_to_title() 
                   #                ),
                   #   plotlyOutput("plot_corr")
                   #    ),
                   tabItem(
                     tabName = "words-id",
                     h2("Modelling Success AirBNB on New York City, USA"),
                     h6("Data: 2019, New York (pre-pandemic)"),
                     fluidRow(
                       box(width=12,
                           background = "black",
                           title = "Top Hosts",
                           column(width = 2,
                                  h4("Estimating Gross Revenue"),
                                  p("The formula to estimating host revenue is Price x Num. Reviews x Median Min. Nights"),
                                  p("Number of reviews is a validated component, since AirBNB only allow real customer to leave reviews."),
                                  p("However minimum nights and price is all depending to the host and keep adjusted from time to time")
                                  ),
                           column(width = 5,
                                  box(width = 12,
                                      title = "Top AirBNB Hosts based on estimated revenue",
                                      background = "black",
                                      plotlyOutput("top_40_plot")
                                      )
                                  ),
                           column(width = 5,
                                  box(width = 12,
                                      title = "Where the est revenue > $100.000 properties are located",
                                      background = "black",
                                      leafletOutput("top_rev_map")
                                      )
                                  )
                       )
                     ),
                     fluidRow(
                           box(width = 12,
                               background = "black",
                               title = "Get Potential Renters by using proven and catchy headline",  
                               column(width = 5,
                                  box(width = 12,
                                      title = "Power Words - Private Room",
                                      background = "black",
                                      plotOutput("pr_words"))
                                  ), 
                               column(width = 5,
                                  box(width = 12,
                                      title = "Power Words - Entire Apt/Home",
                                      background = "black",
                                      plotOutput("apt_words"))
                               ),
                               column(width = 2,
                                      h4("Key Takeaway:"),
                                      p("Private room: The word of cozy, spacious, private and near seems to be power words that are proven to be good AirBNB title."),
                                      p("Entire Apt/Home: The word of cozy, spacious, beautiful and loft (if any) seems to be power words that are proven to be good AirBNB title")
                               )
                           )
                       ),
                     fluidRow(
                       box(width = 12,
                           background = "black",
                           title = "Stay Competitive on the neighbourhood",
                           fluidRow(column(width = 10,
                                           box(
                                             width = 12,
                                             background = "black",
                                             leafletOutput("neighbourhood_map")
                                              )
                                           ),
                                    column(width = 2,
                                           box(
                                             width = 12,
                                             background = "black",
                                             selectInput(inputId = "rt_neigh_id",
                                                         label="Choose Room Type",
                                                         choices = airbnb_clean%>%
                                                           select(room_type) %>% 
                                                           unique(),
                                                         selected = "Entire home/apt"
                                             ),
                                             selectInput(inputId = "neigh_id",
                                                         label="Select Neighbourhood",
                                                         choices = airbnb_clean%>%
                                                           select(neighbourhood) %>% 
                                                           unique(),
                                                         selected = "Harlem"
                                             ),
                                             submitButton(text = "Apply Changes")
                                              )
                                           )
                                    ),
                           fluidRow(
                             valueBox(textOutput("price_filter"), 
                                      "Median Pricing", 
                                      icon = icon("credit-card"),
                                      color = "red"
                             ),
                             valueBox(textOutput("mn_filter"), 
                                      "Median Minimum Nights", 
                                      icon = icon("hotel"),
                                      color = "red"
                             ),
                             valueBox(textOutput("rev_filter"), 
                                      "Average Est. Revenue", 
                                      icon = icon("money"),
                                      color = "red"
                             )
                           ),
                          )
                     )
                     ),
                   tabItem(
                     tabName = "data-id",
                     fluidRow(
                       box(width = 12, 
                           title = "Data Correlation",
                           background = "black",
                           column(width = 10,
                                  plotlyOutput("corr_col")),
                           column(width = 2,
                                  h3("Key Takeaway:"),
                                  p("There are no strong correlation between price and the rest of data"),
                                  br(),
                                  p("Surprisingly, review has negative correlation with price."))
                       )
                      ),
                      fluidRow(
                        box(width = 12,
                            title = "Datasets",
                            background = "black",
                            dataTableOutput("data")
                            )
                      )
                    )
                  )
                )
                  
dashboardPage(title = "AirBNB NYC - 2019",
              skin = "red",
              header = header,
              body = body,
              sidebar = sidebar)
