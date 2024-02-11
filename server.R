### BASIC STATISTICS VISUALIZATIONS APP (SERVER) ########################

#-- Libraries -------------------------

library(tidyverse)
library(shiny)

#-- Define server -------------------------

shinyServer(function(input, output) {

    source("functions.R")

    output$plot_clt <- renderPlot({

        print(plot_clt(input))
    },
    height = 600
    )

    output$plot_fwer <- renderPlot({

        print(plot_fwer(input))
    },
    height = 500
    )

    output$plot_cohensd <- renderPlot({

        print(plot_cohens_d_distributions(input))
    },
    height = 500
    )

    output$plot_bdpdx <- renderPlot({

        print(plot_birthday_paradox(input))
    },
    height = 500
    )

    output$plot_norm_dist <- renderPlot({

        print(plot_normal_distribution(input))
    },
    height = 500
    )
})
