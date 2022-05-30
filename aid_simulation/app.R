


library(shiny)
library(tidyverse)
library(scales)
library(shinythemes)
library(ggdark)
library(snakecase)


###########################################
# Data input
###########################################


inst_aid <- read_csv("inst_aid.csv") %>%
  mutate(upper_ci = Amount + (1.96 * SE)) %>%
  mutate(lower_ci = Amount - (1.96 * SE)) %>%
  mutate(
    Income = fct_relevel(
      Income ,
      "Income:0-30k",
      "Income:30k-60k",
      "Income: 50-70k",
      "Income: 70-100k",
      "Income 100-150" ,
      "Income 150k+"
    )
  )


all_aid <- read_csv("aid_amounts.csv") %>%
  pivot_longer(cols = -Year) %>%
  mutate(value = value * 1e6) %>%
  rename(Total = value,
         `Aid Type` = name)


sim_data <- read_rds("admit_data.rds")%>%
rename_with(to_title_case)



##################################
# Summary Data Function
##################################


summary_plot <- function(myvar) {
  gg <-
    if (length(unique(sim_data[[myvar]])) > 10) {
      sim_data %>%
        mutate(plot_x_var = ntile(.data[[myvar]], 5)) %>%
        group_by(plot_x_var) %>%
        summarize("Average Yield" = mean(Yield)) %>%
        ggplot(aes(
          x = plot_x_var,
          y = `Average Yield`,
          fill = as_factor(plot_x_var)
        )) +
        geom_col() +
        xlab(paste(myvar, "Quintile")) +
        dark_theme_minimal()+
        theme(legend.position="none")
    }else {
      sim_data %>%
        group_by(.data[[myvar]]) %>%
        summarize("Average Yield" = mean(Yield)) %>%
        ggplot(aes(
          x = as_factor(.data[[myvar]]),
          y = `Average Yield`,
          fill = as_factor(.data[[myvar]])
        )) +
        geom_col() +
        dark_theme_minimal()+
        xlab(myvar)+
        theme(legend.position = "none")
    } # End if/else
  gg
}

##################################
# Aid Plot Function
##################################

total_aid_plot <- function(total_aid_type) {
  gg <- all_aid %>%
    filter(`Aid Type` %in% total_aid_type) %>%
    ggplot(aes(x = Year, y = Total, color = `Aid Type`)) +
    geom_line() +
    scale_y_continuous(labels = dollar_format(scale_cut = cut_short_scale())) +
    ggtitle("Total Amount of Student Financial Aid by Source, 1973-2019")+
    dark_theme_minimal()
  gg
}



inst_aid_plot <- function(inst_type) {
  gg <- inst_aid %>%
    filter(Type %in% inst_type) %>%
    ggplot(aes(x = Income, y = Amount, color = Type)) +
    geom_point() +
    geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci)) +
    scale_y_continuous(labels = dollar_format()) +
    ggtitle("Average Aid Awarded to Students by Income Level, 2016")+
    dark_theme_minimal()
  gg
}



######################################################
# Shiny App Elements
######################################################


# Define UI
ui <- fluidPage(
  theme = shinytheme("superhero"),

  navbarPage(
    "Privacy, Proprietary Data and Simulation \n
    William R. Doyle",

#####################################################
# Begin Total Aid Panel
#####################################################

    tabPanel("Total Aid",

             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   checkboxGroupInput(
                     label = "Choose Aid Type",
                     inputId = "aid_type",
                     choices = unique(all_aid$`Aid Type`),
                     selected = "Institutional Grants")
                 ),
                 fluidRow(img(
                   src = "vu06br.jpg",
                   align = "bottom",
                   height = 150,
                   width = 140
                 ))
               ), # End sidebar

               # Show a plot of the generated distribution
               mainPanel(plotOutput("TotalAidPlot")) ## End Panel
            )  # End sidebarlayout
            ), # End tab panel
#####################################################
# End Total Aid Panel
#####################################################


#####################################################
# Begin  Aid By Income Panel
#####################################################
    tabPanel(
      "Institutional Aid by Income",

      # Sidebar with a checkbox for type
      sidebarLayout(sidebarPanel(
        fluidRow(
          checkboxGroupInput(
            label = "Choose Institution Type",
            inputId = "inst_type",
            choices = unique(inst_aid$Type),
            selected = "All")
          ), # End first row
        fluidRow(img(
          src = "vu06br.jpg",
          align = "bottom",
          height = 150,
          width = 140
        ))
      ), # End sidebar

      # Show a plot of the generated distribution
      mainPanel(plotOutput("InstAidPlot")) ## End Panel) # End sidebarlayout
      )
      ),# End aid by income tab panel
#####################################################
# End Aid By Income Panel
#####################################################



#####################################################
# Begin  Summary Simulated Panel
#####################################################
tabPanel(
  "Simulated Yield Data: Characteristics",

  # Sidebar with a checkbox for type
  sidebarLayout(sidebarPanel(
    fluidRow(
     varSelectInput(inputId="simvar",
                    label="Select student characteristic",
                    data=select(sim_data,-Id,-Yield,-Tuition),
                    selected = "income"
                      )
    ), # End first row
    fluidRow(img(
      src = "vu06br.jpg",
      align = "bottom",
      height = 150,
      width = 140
    ))
  ), # End sidebar

  # Show a plot of the generated distribution
  mainPanel(plotOutput("SimDataPlot"))  # End sidebarlayout
  )
), # End aid by income tab panel
#####################################################
# End Summary Simulated Panel
#####################################################


#####################################################
# Begin Modeling Panel
#####################################################
tabPanel(
  "Modeling Decisions",

  ## Load up different version of model, plot
  ## the result

  # Sidebar with a checkbox for type
  sidebarLayout(sidebarPanel(
    fluidRow(img(
      src = "vu06br.jpg",
      align = "bottom",
      height = 150,
      width = 140
    ))
  ), # End sidebar

  # Show a plot of the generated distribution
  mainPanel()  # End sidebarlayout
  )
),# End modeling panel
#####################################################
# End Modeling Panel
#####################################################

#####################################################
# Begin Policy Panel
#####################################################
tabPanel(
  "Policy Decisions Based on Model",

  ## Choose: need aid more responsive
  ## merit aid more responsive
  ## charge 5k more to one or all of legacies, sent scores, registered
  ## Total Enrollment

  # Sidebar with a checkbox for type
  sidebarLayout(sidebarPanel(

    fluidRow(img(
      src = "vu06br.jpg",
      align = "bottom",
      height = 150,
      width = 140
    ))
  ), # End sidebar

  # Show a plot
  mainPanel()
  ) # End sidebar
)# End policy tab panel
#####################################################
# End Policy Panel
#####################################################





) # End all panels


    ) # End fluidpageUI

#####################################################
# Server Section
#####################################################

    # Define server
    server <- function(input, output) {


      ## Total aid by type plot
      output$TotalAidPlot <- renderPlot({
        total_aid_plot(total_aid_type = input$aid_type)
      }) ## End Aid Plot Output


      ## Institutional aid by income plot
      output$InstAidPlot <- renderPlot({
        inst_aid_plot(inst_type = input$inst_type)
      },bg="transparent") ## End Income Plot Output


      ## Institutional aid by income plot
      output$SimDataPlot <- renderPlot({
        summary_plot(input$simvar)
      },bg="transparent") ## End Income Plot Output


    }# End Server

    # Run the application
    shinyApp(ui = ui, server = server)
