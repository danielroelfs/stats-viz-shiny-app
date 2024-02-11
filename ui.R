### BASIC STATISTICS VISUALIZATIONS APP (UI) ########################

#-- Libraries -------------------------

library(shiny)
library(shinyWidgets)
library(colourpicker)

#-- Define UI -------------------------

ui <- navbarPage(
    title = "Basic statistics visualizations",
    tabPanel(
        "FWER (Type 1 Errors)",
        sidebarPanel(
            chooseSliderSkin("Flat", color = "#BD2D87"),
            sliderInput(
                "fwer_n",
                "Number of comparisons",
                min = 1,
                max = 200,
                value = 36,
                ticks = FALSE
            ),
            sliderInput(
                "fwer_alpha",
                "\u03b1-level",
                min = 0.005,
                max = 0.1,
                value = 0.05,
                step = 0.005,
                ticks = FALSE
            ),
            colourInput("fwer_accent", "Accent color", "#BD2D87"),
            materialSwitch(inputId = "fwer_dark", label = "Dark theme")
        ),
        mainPanel(
            plotOutput(outputId = "plot_fwer"),
        )
    ),
    tabPanel(
        "Central Limit Theorem",
        sidebarPanel(
            chooseSliderSkin("Flat", color = "#BD2D87"),
            radioGroupButtons(
                inputId = "clt_pop_dist_type",
                label = "Population distribution",
                choices = c("Normal", "Uniform", "Right skewed", "Left skewed"),
                selected = "Normal"
            ),
            sliderInput(
                "clt_sample_size",
                "Sample size",
                min = 10,
                max = 200,
                value = 30,
                step = 10,
                ticks = FALSE
            ),
            sliderInput(
                "clt_n_samples",
                "Number of samples",
                min = 10,
                max = 1000,
                value = 100,
                step = 10,
                ticks = FALSE
            ),
            sliderInput(
                "clt_seed",
                "Random state (seed)",
                min = 10,
                max = 100,
                value = 42,
                step = 1,
                ticks = FALSE
            ),
            colourInput("clt_accent", "Accent color", "#BD2D87"),
            materialSwitch(inputId = "clt_dark", label = "Dark theme")
        ),
        mainPanel(
            plotOutput(outputId = "plot_clt"),
        )
    ),
    tabPanel(
        "Cohen's d",
        sidebarPanel(
            chooseSliderSkin("Flat", color = "#BD2D87"),
            sliderInput(
                "cohens_d",
                "Cohen's d",
                min = 0.1,
                max = 4,
                value = 0.3,
                step = 0.1,
                ticks = FALSE
            ),
            sliderInput(
                "cohensd_sd",
                "Standard Deviation",
                min = 0.5,
                max = 3,
                value = 1,
                step = 0.1,
                ticks = FALSE
            ),
            colourInput("cohensd_accent", "Accent color", "#BD2D87"),
            materialSwitch(inputId = "cohensd_dark", label = "Dark theme")
        ),
        mainPanel(
            plotOutput(outputId = "plot_cohensd"),
        )
    ),
    tabPanel(
        "Birthday paradox",
        sidebarPanel(
            chooseSliderSkin("Flat", color = "#BD2D87"),
            sliderInput(
                "bdpdx_n",
                "Number of people",
                min = 2,
                max = 100,
                value = 20,
                step = 1,
                ticks = FALSE
            ),
            colourInput("bdpdx_accent", "Accent color", "#BD2D87"),
            materialSwitch(inputId = "bdpdx_dark", label = "Dark theme")
        ),
        mainPanel(
            plotOutput(outputId = "plot_bdpdx"),
        )
    ),
    tabPanel(
        "Normal distribution",
        sidebarPanel(
            chooseSliderSkin("Flat", color = "#BD2D87"),
            sliderInput(
                "norm_sd",
                "Standard deviation",
                min = 0.5,
                max = 3,
                value = 1,
                step = 0.1,
                ticks = FALSE
            ),
            colourInput("norm_accent", "Accent color", "#BD2D87"),
            materialSwitch(inputId = "norm_dark", label = "Dark theme")
        ),
        mainPanel(
            plotOutput(outputId = "plot_norm_dist"),
        )
    ),
)
