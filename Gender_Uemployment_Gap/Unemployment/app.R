# load necessary packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(viridis)
library(sf)
library(tmap)

# set the map mode to interative 
tmap_mode("view")

# Read in the dataset
vec_avg_by_gender_age <- read_csv('vec_avg_by_gender_age.csv')
unem_by_gender <- read_csv('unem_by_gender.csv')
tidy_unemp <- read_csv('tidy_unemp.csv')
counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv')
# state level shapefiles (see function documentation to customize)
st_shape <- tigris::states()

# county level shapefiles (see function documentation to customize)
county_shape <- tigris::counties()

## Clean State data

unemp_state_shape <- tidy_unemp %>%
  inner_join(counties) %>% 
  select(-c(county_name, state_abbreviation)) %>%
  # Group by gender and state
  group_by(state_name, gender) %>%
  # Calculate the mean unemployment rate of different genders for each state
  summarize(
    mean_rate = mean(rate, na.rm = TRUE),
    .groups = 'drop' 
  ) %>%
  spread(key = gender, value = mean_rate) %>%
  # Calculate the gap
  mutate(
    unem_gender_gap_state = Male - Female
  ) %>%
  inner_join(st_shape, by = c("state_name" = "NAME"))

## Clean County data
tidy_unemp_state_county <- tidy_unemp %>%
  inner_join(counties)%>%
  rename("geoid" = "county_fips_code")

## Calculate the mean unemp rate for every county
county_unemp_gap <- tidy_unemp_state_county %>%
  group_by(state_name, geoid, county_name, gender) %>%
  summarize(
    mean_rate = mean(rate, na.rm = TRUE),
    .groups = 'drop' 
  ) %>%
  spread(key = gender, value = mean_rate) %>%
  # Calculate the gap
  mutate(
    unem_gender_gap_county = Male - Female
  )

## Filter to get state names

st_tibble <- st_shape %>%
  set_names(
    names(.) %>%
      tolower()
  ) %>%
  as_tibble() %>%
  select(statefp, name) %>%
  rename('state_name' = 'name')

## Append state names to County level data
county_shape_clean <- county_shape %>%
  set_names(
    names(.) %>%
      tolower()
  ) %>%
  left_join(st_tibble,
            by = 'statefp')


## Merge the county_shape_clean to tidy_unemp_state

county_unemp_gap <- county_unemp_gap %>%
  merge(county_shape_clean, by = "geoid") %>%
  rename("State_Name" = "state_name.x")


## Create the app
ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("The Gender Unemployment Gap"),
  h2("Yetong Xu"),
  h3("Introduction"),
  p("In recent years, following the COVID-19 pandemic, many countries have experienced 
    periods of high unemployment rates. So, could there be gender differences in unemployment 
    rates due to various factors?"),
  p("UNCTAD analysis(Zarrilli, 2021) shows that the COVID-19 pandemic has had a negative impact on both women's and men's employment – but at different stages of the crisis due to the gender segregation of economic activities in many countries. Early measures to curb the spread of the virus first hit jobs held predominantly by women, such as personal services. At the outset of the pandemic, a higher prevalence of the virus correlated with a higher rate of female unemployment. But as the crisis worsened and disrupted cross-border value chains, the impact on men’s employment increased because they tend to work in sectors and jobs that are more dependent on international trade."),
  p("However, the issue of the gender gap in unemployment rates is not just a contemporary concern. Even before the pandemic, various factors contributed to this disparity. As mentioned in the study of Stefania(2013), the unemployment gender gap, defined as the difference between female and male unemployment rates, was positive until 1980. This gap virtually disappeared after 1980--except during recessions, when men’s unemployment rates always exceed women’s. At the cyclical frequency, they find that gender differences in industry composition are important in recessions."),
  h3("Objective"),
  p("In this project, my goal is to explore whether there are significant 
    differences in the unemployment gender gap in the United States from 2008 to 2018, 
    across different years, age groups or states."),
  img(src = "Unemployment_Gender_Gap.jpg", height = "350px", width = "60%", align = "right"),
  p("You can select different genders, age groups, and states to observe the corresponding average unemployment rates as well as the unemployment gender gaps."),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender_choice", "Gender", 
                  choices = c("Overall", "Male", "Female")),
      selectInput("age_choice", "Age", 
                  choices = c("16 and over", "20 to 64")),
      pickerInput("state_choice", "State", 
                  choices = c("West Virginia", "Delaware","Florida","Georgia", 
                              "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
                              "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                              "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                              "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                              "New Jersey", "New Mexico", "New York", "North Carolina",
                              "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
                              "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                              "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                               "Wisconsin", "Wyoming", "California" ))
    ),
    mainPanel(
      h3("Part 1"),
      p("Explore the trends of average unemployment rates across different genders and age groups over time (years: 2008-2018)."),
      div(class = "container-fluid",
          fluidRow(
            column(12,
                   plotOutput("gender_age_unemp_plot")
            ),
            column(6,
                   tags$ul(
                     tags$li("For both men and women, the average unemployment rates continuously rose, peaking in 2013, and then showed a downward trend."),
                     tags$li("The average unemployment rate for men was consistently higher than that for women.")
                   )
            )
          ),
          h3("Part 2"),
          p("Explore the changes of the unemployment gender gap across different age groups over time (years: 2008-2018)."),
          fluidRow(
            column(12,
                   plotOutput("unem_gap_gender_plot")
            ),
            column(6,
                   tags$ul(
                     tags$li("The age group 16 and over has a higher unemployment gender gap compared to the 20 to 64 age group."),
                     tags$li("For both two age groups, the peak of the unemployment gender gap was reached in 2013.")
                   )
            )
          ),
          h3("Part 3"),
          p("Explore the unemployment gender gap across different states (years: 2008-2018)."),
          fluidRow(
            column(12,
                   plotOutput("unem_gap_state_plot")
            ),
            column(6,
                   tags$ul(
                     tags$li("The unemployment gender gap varies significantly between different states."),
                     tags$li("Michigan and West Virginia, among others, having a higher unemployment gender gap during the period from 2008 to 2018.")
                   )
            )
          ),
          h3("Part 4"),
          p("Explore the specifics and nuances of the unemployment gender gap in particular states, with a focus on more detailed insights (e.g. Michigan, West Virginia)."),
          fluidRow(
            column(12,
                   tmapOutput("unem_gap_county_plot")
            ),
            column(6,
                   tags$ul(
                     tags$li("Higher unemployment gender gap in state such as Michigan and West Virginia can be attributed to their economic structures."),
                     tags$li("Michigan's economy, heavily reliant on the automotive industry, and West Virginia's focus on coal mining."),
                     tags$li("These two industries are predominantly male-dominated sectors, and experienced significant job losses during the 2008 financial crisis.")
                   )
            )
          ),
          tags$style(type = 'text/css', "
  .conclusion-text {
    text-align: justify;
    text-justify: inter-word;
  }
"),
          div(class = "conclusion-text",
              h3("Conclusion"),
              p("From the findings above, policy-makers should develop policies which aim to encourage gender diversity in traditionally male or female-dominated sectors, and implement educational initiatives that encourage young people, regardless of gender, to pursue a variety of career paths, including those in STEM fields. Meanwhile, policy-makers should recognize that the economic structure of each state is unique, recovery strategies during economic downturns should be state-specific.  For example, they should focus on supporting industries which are most affected in each state, giving economic incentives for companies that actively reduce the gender gap in employment, and providing better access to childcare and other support services, enabling more individuals, especially women, to participate in the workforce.")
          )
      )
    )))

server <- function(input, output) {
  ## Different genders' unemployment rate at different age, changing over time
  output$gender_age_unemp_plot <- renderPlot({
    filtered_gender_age_data <- vec_avg_by_gender_age %>% 
      dplyr::filter(gender == input$gender_choice,
             age_category == input$age_choice)
    
    # Determine the title based on the selected gender
    title <- paste("Average unemployment rate for", input$gender_choice, "at age", input$age_choice)
    
    # Draw the line chart 
    ggplot(filtered_gender_age_data, aes(x = study_year, y = gender_age_avg_unem, group = interaction(gender, age_category), color = gender)) +
      geom_line() +
      scale_x_continuous(breaks = seq(2008,2018,by = 1)) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    labs(title = title,
         subtitle = 'From 2008 to 2013, the average unemployment rate continuously rose, \npeaking in 2013, and then showed a downward trend. ',
         x = "Year", 
         y = "Unemployment rate",
         caption = "Data Source: National Database of Childcare Prices") +
      scale_color_manual(values=c('Overall' = 'gray', 'Male' = 'blue', 'Female' = 'pink')) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, family = "Times"),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_text(size = 12, family = "Times"),
        axis.text = element_text(size = 10, family = "Times"),
        plot.subtitle = element_text(hjust = 0, size = 10, family = "Times"),
        plot.caption = element_text(hjust = 1, size = 8, family = "Times")
      )
  })
  
  ## Unemployment Gap between genders
  output$unem_gap_gender_plot <- renderPlot({
    # Filter the data by input gender and age
    filtered_age_data <- unem_by_gender %>%
      dplyr::filter(age_category == input$age_choice)

    # Determine the title based on the selected gender and age
    title <- paste("Unemployment gap between male and female for", input$age_choice)

    # Draw the bar chart
    ggplot(filtered_age_data, aes(x = factor(study_year), y = unem_gap_gender, fill = unem_gap_gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = title,
           subtitle = "In each age group, no matter 16 and over, or 20 to 64, \nthe peak in the unemployment gender gap was reached in 2013.",
           x = "Year", 
           y = "Unemployment rate gap",
           caption = "Data Source: National Database of Childcare Prices") +
      scale_fill_gradient(low = "lightblue", high = "blue") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0, family = "Times"),
        legend.position = "none", # No legend needed for a single variable
        axis.title = element_text(size = 12, family = "Times"),
        axis.text = element_text(size = 10, family = "Times"),
        plot.subtitle = element_text(hjust = 0, size = 10, family = "Times"),
        plot.caption = element_text(hjust = 1, size = 8, family = "Times")
      )
  })
  
  ## Unemployment Gap between states
  output$unem_gap_state_plot <- renderPlot({

    
    unemp_state_shape %>%
      dplyr::filter(state_name != "Alaska") %>%
      st_as_sf() %>%
      st_transform(crs = 4326) %>%
      ggplot() +
      geom_sf(aes(fill = unem_gender_gap_state)) +
      scale_fill_viridis(name = "unemployment rates' gender gap",
                         option = "magma",
                         breaks = seq(-1, 4, by = 0.5), 
                         na.value = "#dcdcdc") +
      labs(
        title = "Unemployment rates' gender gap in the US(2008-2018)",
        subtitle = "From 2008 to 2018, most states in the US didn't have a large unemployment rates' gender gap \n. A few states had large unemployment rates' gender gap, such as Michigan, West Virginia, etc.",
        caption = "Data Source: National Database of Childcare Prices"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 4),
        plot.subtitle = element_text(hjust = 0.5, size = 1),
        plot.caption = element_text(hjust = -1, size = 2)
      ) +
      theme_void(base_family = "Times")
  })
  
  ## Unemployment Gap between counties
  output$unem_gap_county_plot <- renderTmap({
    county_unemp_gap <- county_unemp_gap %>%
      st_as_sf() %>%
      st_transform(crs = 4326)
    # Build the interactive map
    tm_basemap(
      c('OpenStreetMap',
        'Esri.WorldImagery')) +
      tm_shape(county_unemp_gap %>%
                 filter(State_Name == input$state_choice)) +
      tm_polygons("unem_gender_gap_county", 
                  palette = "-viridis", 
                  title = "Unemployment gender gap",
                  alpha = 0.6)  +
      tm_shape(
        county_unemp_gap %>%
          filter(State_Name == input$state_choice,
                 (unem_gender_gap_county > 0.2|unem_gender_gap_county< -0.2))
      ) +
      tm_dots(
        clustering = TRUE
      ) +
      tm_layout(
        title = "Unemployment gender gap in specific state",
        title.position = c("center", "top"),
        title.size = 1.5,
        title.fontface = "bold",
        inner.margins = c(0.1, 0.1, 0.1, 0.1)  # Adjust margins
      )
  })
}


shinyApp(ui = ui, server = server)