library(shiny)
library(tidyverse)
library(scales)
library(shinythemes)
library(ggdark)
library(snakecase)
library(tidymodels)
library(glmnet)


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


yield_data <- read_rds("admit_data.rds")%>%
  mutate(sat=sat/100,
         income=income/1000,
         distance=distance/1000)

load("model_fit_data.rdata")

model_fit_data<-model_fit_data%>%
  mutate(lower_ci=Estimate-(1.96*SE))%>%
    mutate(upper_ci=Estimate-(1.96*SE))

load("fit_list.rdata")

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
# Total Aid Plot Function
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


##################################
# Aid by Income  Plot Function
##################################


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



##################################
# Model Fit  Plot Function
##################################


model_fit_plot<-function(my_metric){
  gg<-
    model_fit_data%>%
    filter(Metric==my_metric)%>%
    ggplot(aes(x=`Model Type`,y=Estimate,color=`Model Type`))+
    geom_point(size=10)+
    geom_pointrange(aes(ymin=lower_ci,ymax=upper_ci))+
    ylab(my_metric)+
    coord_flip()+
    dark_theme_minimal()+
    theme(legend.position = "none")
gg
}



##################################
# Summary Stats Function
##################################

summary_change<-function(
  model_type="All Variables",
                         prob_min=.45,
                         prob_max=.55,
                         price_drop=10000){

yield_fit<-fit_list[[model_type]]

np<-yield_data

## Predict for current

np<-yield_fit%>%
  predict(np,type="prob")%>%
  bind_cols(np)

pre_price<-  np%>%
    mutate(`Income Quintile`=ntile(income,5))%>%
    group_by(`Income Quintile`)%>%
    summarize(`Current Price`=mean(net_price))

pre_enroll<-np%>%
  filter(.pred_Enrolled>.5)%>%
  count()%>%
  as_vector()%>%
  number(big.mark = ",")

pre_revenues<-dollar(sum(np$net_price[np$.pred_Enrolled>.5]))
## Use inputs to set these three amounts

## Predict for targeted policy

## Set prices based on new policy
np_post<-np%>%
    mutate(net_price=ifelse(.pred_Enrolled>prob_min|.pred_Enrolled<prob_max,
           net_price-price_drop,
           net_price
           ))

## Drop old predictions
np_post<-np_post%>%select(-starts_with(".pred"))

## Add new predictions
  np_post<-yield_fit%>%
    predict(np_post,type="prob")%>%
    bind_cols(np_post)

## Calculate Net Price by Income
  post_price<-np_post%>%
    mutate(`Income Quintile`=ntile(income,5))%>%
    group_by(`Income Quintile`)%>%
    summarize(`New Price`=mean(net_price))

## Count Enrollment
  post_enroll<-np_post%>%
    filter(.pred_Enrolled>.5)%>%
    count()%>%
    as_vector()%>%
    number(big.mark = ",")

  post_revenues<-dollar(sum(np_post$net_price[np_post$.pred_Enrolled>.5]))

summary_data<-left_join(pre_price,post_price)%>%
  pivot_longer(cols=-`Income Quintile`)%>%
  rename(Policy=name,`Net Price`=value)

gg<-summary_data%>%
  ggplot(aes(x=`Income Quintile`,y=`Net Price`,fill=Policy))+
  geom_col(position="dodge")+
  labs(caption = paste("Enrollment under current policy:",pre_enroll,"\n",
                       "Enrollment under new policy:",post_enroll,"\n",
                       "Revenues under current policy:",pre_revenues,"\n",
                       "Revenues under new policy:",post_revenues))+
  ggtitle("Impact of Policy Change")+
  dark_theme_minimal()+
 theme(plot.caption=element_text(size=12))
gg
} # End summary change function

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
                 fluidRow(HTML("Source: College Board, Trends in Student Aid")),
                 fluidRow(img(
                   src = "vu06br.jpg",
                   align = "bottom",
                   height = 150,
                   width = 140
                 )),
                 fluidRow(img(
                   src = "aid_simulation.png",
                   align = "bottom",
                   height = 150,
                   width = 140
                 )),
              fluidRow(HTML("https://wdoyle42.shinyapps.io/aid_simulation/")),
              fluidRow(HTML("tw:@wdoyle42, email:w.doyle@vanderbilt.edu"))
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
           fluidRow(HTML("Source: National Postsecondary Student Aid Survey, DataLab")),
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
   fluidRow(HTML("Data generated using covariances estimated from High School Longitudinal Study" )),
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
    fluidRow(
selectInput(
  inputId = "model_metric",
  label="Choose Measure of Model Fit",
  choices=unique(model_fit_data$Metric),
  selected = "Accuracy")),
fluidRow(HTML("Measures of model fit based on cross validation from three different specifications of model. Models utilize feature selection via elastic net, with hyparameters tuned via 1000 Monte Carlo cross validations." )),
    fluidRow(img(
      src = "vu06br.jpg",
      align = "bottom",
      height = 150,
      width = 140
    )
  )), # End sidebar

  # Show a plot
  mainPanel(plotOutput("ModelFitPlot"))  # End sidebarlayout
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


  # Sidebar with a checkbox for type
  sidebarLayout(sidebarPanel(
fluidRow(
    sliderInput(inputId = "enroll_prob",
                label="Minimum Enrollment Probability",
                min=.2,
                max=.8,
                value=c(.45,.55)
                )),
fluidRow(
  sliderInput(inputId = "price_drop",
              label="Reduction in Net Price",
              min=1000,
              max=10000,
              value=5000
  )),
fluidRow(HTML("Model results using all variables. Inputs above change net price for selected students, after which new probabilities of enrollment are calculated" )),
    fluidRow(img(
      src = "vu06br.jpg",
      align = "bottom",
      height = 150,
      width = 140
    ))
  ), # End sidebar

  # Show a plot
  mainPanel(plotOutput("PolicyChangePlot"))
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


      ## Model Fit Plot
      output$ModelFitPlot <- renderPlot({
        model_fit_plot(input$model_metric)
      },bg="transparent") ## End Income Plot Output


      ## Policy Change Plot
      output$PolicyChangePlot<-renderPlot({
        summary_change(prob_min = input$enroll_prob[1],
                        prob_max = input$enroll_prob[2],
        price_drop=input$price_drop)
      })

    }# End Server

    # Run the application
    shinyApp(ui = ui, server = server)
