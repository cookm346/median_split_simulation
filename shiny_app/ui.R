library(shiny)

shinyUI(fluidPage(
    titlePanel("Correlation versus t-test simulation"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("n_sims", "Number of simulations:", min = 0, max = 100000, value = 2500, step = 500),
            sliderInput("pop_cor", "Population correlation:", min = -1, max = 1, value = 0, step = 0.05),
            sliderInput("sample_size", "Sample size:", min = 20, max = 245, value = 50, step = 5),
            sliderInput("split", "Percentile split:", min = 0.1, max = 0.5, value = 0.5, step = 0.05),
            submitButton("Run")
        ),

        mainPanel(
            plotOutput("plot", height = "525px")
        )
    )
))
