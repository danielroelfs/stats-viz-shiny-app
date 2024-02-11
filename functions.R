### BASIC STATISTICS VISUALIZATIONS APP (FUNCTIONS) ########################

#-- Libraries -------------------------

library(tidyverse)
library(ggtext)
library(patchwork)

#-- Functions -------------------------

fwer <- function(n = NULL, alpha = 0.05) {
    #' Calculate the family-wise error rate

    if (is.null(n)) warning("n cannot be empty, no default is available for n,
        please set the number of comparisons")
    if (alpha >= 1) warning("alpha level has to be between 0 and 1")

    fwer <- 1 - ((1 - alpha)^n)

    return(fwer)
}

plot_fwer <- function(input) {
    #' Create a plot for the FWER option

    t1e <- reactive({
        t1e <- tibble(
            n = seq(0, 220),
            fwer = fwer(n, alpha = input$fwer_alpha)
        )
    })

    fwer_arrow <- reactive({
        fwer_arrow <- tibble(
            x_start = input$fwer_n + 28,
            y_start = round(fwer(input$fwer_n, input$fwer_alpha), 2) - 0.16,
            x_end = input$fwer_n + 3,
            y_end = fwer(input$fwer_n, input$fwer_alpha) - 0.01,
            cvt = 0.2
        )
    })

    ins <- fwer(n = input$fwer_n, alpha = input$fwer_alpha)

    txt_x <- input$fwer_n + 28
    txt_y <- round(ins, 2) - 0.26

    # Curve options
    fwer_arrow <- fwer_arrow()
    if (input$fwer_n > 150) {
        fwer_arrow$x_start <- input$fwer_n - 28
        fwer_arrow$x_end <- input$fwer_n - 3
        fwer_arrow$y_end <- ins - 0.02
        fwer_arrow$cvt <- -0.2
        txt_x <- input$fwer_n - 28
    }
    if (ins < 0.3) {
        fwer_arrow$y_start <- round(ins, 2) + 0.30
        fwer_arrow$y_end <- ins + 0.01
        fwer_arrow$cvt <- -0.2
        txt_y <- round(ins, 2) + 0.40
    }
    if (input$fwer_n > 150 && ins < 0.4) {
        fwer_arrow$cvt <- 0.2
    }

    dark <- reactive({
        dark <- input$fwer_dark
    })

    if (dark()) {
        line_col <- "white"
        text_col <- "white"
        bg_col <- "#333333"
        grid_col <- "grey40"
    } else {
        line_col <- "grey30"
        text_col <- "grey30"
        bg_col <- "white"
        grid_col <- "grey90"
    }

    plot <- ggplot(t1e()) +
        geom_line(
            mapping = ,
            aes(x = n, y = fwer),
            color = line_col,
            alpha = 1,
            linewidth = 2,
            lineend = "round"
        ) +
        geom_segment(
            x = input$fwer_n,
            xend = input$fwer_n,
            y = 0,
            yend = ins,
            linewidth = 1,
            color = input$fwer_accent,
            linetype = "dashed"
        ) +
        geom_segment(
            x = 0,
            xend = input$fwer_n,
            y = ins,
            yend = ins,
            linewidth = 1,
            color = input$fwer_accent,
            linetype = "dashed"
        ) +
        geom_curve(
            data = fwer_arrow,
            mapping = aes(
                x = x_start,
                y = y_start,
                xend = x_end,
                yend = y_end
            ),
            arrow = arrow(length = unit(0.08, "inch")),
            size = 0.5,
            color = text_col,
            curvature = fwer_arrow$cvt
        ) +
        geom_label(
            data = tibble(),
            aes(
                x = txt_x,
                y = txt_y
            ),
            size = 4,
            color = text_col,
            fill = "transparent",
            label.size = 0,
            label = str_glue(
                "{input$fwer_n} comparisons at
                \u03b1-level {input$fwer_alpha} gives a
                FWER of {scales::percent(ins, accuracy = 0.01)}",
            )
        ) +
        scale_x_continuous(breaks = seq(0, 200, 40)) +
        scale_y_continuous(
            labels = scales::percent_format(accuracy = 1),
            limits = c(0, 1),
            breaks = seq(0, 1, 0.2)
        ) +
        coord_cartesian(x = c(0, 200)) +
        labs(
            x = "Number of comparisons",
            y = "Chance of getting at least one false positive"
        ) +
        theme_minimal(base_size = 14) +
        theme(
            text = element_text(color = text_col),
            axis.text = element_text(color = text_col),
            plot.background = element_rect(fill = bg_col, color = "transparent"),
            plot.margin = margin(30, 30, 30, 30, "points"),
            panel.background = element_rect(fill = bg_col, color = "transparent"),
            panel.grid = element_line(color = grid_col)
        )

    return(plot)
}

plot_normal_distribution <- function(input) {
    #' Plot a normal distribution

    if (input$norm_dark) {
        line_col <- "white"
        text_col <- "white"
        bg_col <- "#333333"
        grid_col <- "grey40"
    } else {
        line_col <- "grey30"
        text_col <- "grey30"
        bg_col <- "white"
        grid_col <- "grey90"
    }

    annotations <- tibble(
        x = seq(-1, -3) * input$norm_sd,
        xend = seq(3) * input$norm_sd,
        y = seq(0.45, 0.65, 0.1),
        percs = c(65, 95, 99),
        sds = seq(3),
        label = str_glue("{percs}% of the data lies<br>between {sds} SD")
    )

    plot <- ggplot(data = tibble(x = 3.2 * input$norm_sd * c(-1, 1)), aes(x)) +
        geom_hline(
            yintercept = 0,
            linewidth = 1,
            color = line_col
        ) +
        geom_area(
            stat = "function",
            fun = dnorm,
            args = list(mean = 0, sd = input$norm_sd),
            fill = input$norm_accent,
            alpha = 0.75
        ) +
        geom_segment(
            data = annotations,
            aes(
                x = x,
                xend = x,
                y = 0,
                yend = y
            ),
            linewidth = 1,
            color = line_col
        ) +
        geom_segment(
            data = annotations,
            aes(
                x = xend,
                xend = xend,
                y = 0,
                yend = y
            ),
            linewidth = 1,
            color = line_col
        ) +
        geom_segment(
            data = annotations,
            aes(
                x = x,
                xend = xend,
                y = y,
                yend = y
            ),
            linewidth = 1,
            color = line_col
        ) +
        geom_richtext(
            data = annotations,
            aes(
                y = y,
                label = label
            ),
            x = 0,
            color = text_col,
            fill = bg_col
        ) +
        labs(
            x = "Standard deviation",
            y = NULL
        ) +
        scale_x_continuous(
            breaks = seq(-9, 9)
        ) +
        scale_y_continuous(
            breaks = seq(0, 1, 0.1),
            expand = expansion(add = c(0, 0.025))
        ) +
        coord_cartesian(
            ylim = c(0, 0.7),
            clip = "off"

        ) +
        theme_minimal(base_size = 16) +
        theme(
            text = element_text(color = text_col),
            plot.background = element_rect(fill = bg_col, color = "transparent"),
            plot.margin = margin(30, 30, 30, 30, "points"),
            panel.background = element_rect(fill = bg_col, color = "transparent"),
            panel.grid = element_line(color = grid_col),
            axis.text.x = element_text(color = text_col, margin = margin(t = 10)),
            axis.text.y = element_blank(),
            axis.title.x = element_text(margin = margin(t = 10))
        )

    return(plot)
}

plot_cohens_d_distributions <- function(input) {
    #' Plot a difference in Cohen's d

    r <- 20
    norm <- tibble(
        x = seq(-r, r, 1e-3),
        y = dnorm(x = seq(-r, r, 1e-3), mean = 0, sd = input$cohensd_sd),
        p = length(seq(-r, r, 1e-3))
    ) |>
        filter(x > -(input$cohensd_sd * 5) & x < (input$cohensd_sd * 5))

    sd_p <- reactive({
        sd_p <- sqrt((input$cohensd_sd^2 + input$cohensd_sd^2) / 2)
    })

    mean_diff <- reactive({
        mean_diff <- abs((mean(norm$x) - input$cohens_d) * sd_p())
    })

    effect <- reactive({
        effect <- tibble(
            x = seq(-r, r, 1e-3),
            y = dnorm(x = seq(-r, r, 1e-3), mean = mean_diff(), sd = input$cohensd_sd),
            p = length(seq(-r, r, 1e-3))
        ) |>
            filter(x > -(input$cohensd_sd * 5) + mean_diff() & x < (input$cohensd_sd * 5) + mean_diff())
    })

    if (input$cohensd_dark) {
        line_col <- "white"
        text_col <- "white"
        bg_col <- "#333333"
        grid_col <- "grey40"
    } else {
        line_col <- "grey30"
        text_col <- "grey30"
        bg_col <- "white"
        grid_col <- "grey90"
    }

    arr_y <- max(effect()$y)
    mid <- mean_diff() / 2

    eff_arrow <- tibble(
        x_start = mid + 0.1 * max(effect()$x),
        y_start = arr_y + 0.05,
        x_end = mid,
        y_end = arr_y + 0.015
    )

    plot <- ggplot() +
        geom_hline(
            yintercept = 0,
            color = ifelse(input$cohensd_dark, "grey70", "grey30"),
            linewidth = 1
        ) +
        geom_ribbon(
            data = norm,
            aes(
                x = x,
                ymin = 0,
                ymax = y
            ),
            fill = "grey30",
            alpha = 0.6
        ) +
        geom_ribbon(
            data = effect(),
            aes(
                x = x,
                ymin = 0,
                ymax = y
            ),
            fill = input$cohensd_accent,
            alpha = 0.6
        ) +
        geom_segment(
            aes(
                x = 0,
                xend = mean_diff(),
                y = arr_y + 0.01,
                yend = arr_y + 0.01
            ),
            arrow = arrow(length = unit(0.04, "inch")),
            linewidth = 0.5,
            color = text_col
        ) +
        geom_segment(
            aes(
                x = mean_diff(),
                xend = 0,
                y = arr_y + 0.01,
                yend = arr_y + 0.01
            ),
            arrow = arrow(length = unit(0.04, "inch")),
            linewidth = 0.5,
            color = text_col
        ) +
        geom_curve(
            data = eff_arrow,
            aes(
                x = x_start,
                xend = x_end,
                y = y_start,
                yend = y_end
            ),
            arrow = arrow(length = unit(0.08, "inch")),
            linewidth = 0.5,
            lineend = "round",
            color = text_col, curvature = 0.3) +
        geom_richtext(
            data = NULL,
            aes(
                x = eff_arrow$x_start + 0.15 * max(effect()$x),
                y = arr_y + 0.05
            ),
            size = 5,
            color = text_col,
            label = str_glue("Cohen's *d* = {input$cohens_d}"),
            fill = NA,
            label.color = NA,
            label.padding = grid::unit(rep(0, 4), "pt")
        ) +
        labs(
            x = NULL,
            y = NULL
        ) +
        scale_x_continuous(
            breaks = seq(-20, 20),
            limits = c(min(norm$x), max(effect()$x)),
        ) +
        scale_y_continuous(
            breaks = NULL,
            expand = expansion(add = c(0, 0.1))
        ) +
        theme_minimal(base_size = 14) +
        theme(
            text = element_text(color = text_col),
            plot.background = element_rect(fill = bg_col, color = "transparent"),
            plot.margin = margin(30, 30, 30, 30, "points"),
            panel.background = element_rect(fill = bg_col, color = "transparent"),
            panel.grid = element_line(color = grid_col),
            axis.text.x = element_text(color = text_col, margin = margin(t = 10)),
        )

    return(plot)
}

plot_birthday_paradox <- function(input) {
    #' Plot birthday paradox curve

    calc_prob <- function(n) {

        p <- 1 - (364 / 365)^(n * (n - 1) / 2)

        return(p)
    }

    data <- tibble(
        n = seq(1, 100),
        p = map_dbl(n, calc_prob)
    )

    ins <- data |>
        filter(n == input$bdpdx_n) |>
        pull(p)

    if (input$bdpdx_dark) {
        line_col <- "white"
        text_col <- "white"
        bg_col <- "#333333"
        grid_col <- "grey40"
    } else {
        line_col <- "grey30"
        text_col <- "grey30"
        bg_col <- "white"
        grid_col <- "grey90"
    }

    bdpdx_arrow <- reactive({
        bdpdx_arrow <- tibble(
            x_start = input$bdpdx_n + 18,
            y_start = ins - 0.175,
            x_end = input$bdpdx_n + 2,
            y_end = data |> filter(n == input$bdpdx_n) |> pull(p),
            cvt = 0.2
        )
    })

    txt_x <- input$bdpdx_n + 26
    txt_y <- ins - 0.23

    # Curve options
    bdpdx_arrow <- bdpdx_arrow()
    if (input$bdpdx_n > 60) {
        bdpdx_arrow$x_start <- input$bdpdx_n - 12
        bdpdx_arrow$x_end <- input$bdpdx_n - 1
        bdpdx_arrow$y_start <- ins - 0.22
        bdpdx_arrow$y_end <- ins - 0.02
        bdpdx_arrow$cvt <- 0.2
        txt_x <- input$bdpdx_n - 27
    }
    if (ins < 0.5) {
        bdpdx_arrow$y_start <- ins + 0.22
        bdpdx_arrow$y_end <- ins + 0.01
        bdpdx_arrow$cvt <- -0.2
        txt_y <- ins + 0.28
    }
    if (input$bdpdx_n > 150 && ins < 0.4) {
        bdpdx_arrow$cvt <- 0.2
    }

    plot <- ggplot(data, aes(x = n, y = p)) +
        geom_line(
            color = line_col,
            linewidth = 2
        ) +
        geom_segment(
            x = input$bdpdx_n,
            xend = input$bdpdx_n,
            y = 0,
            yend = ins,
            linewidth = 1,
            color = input$bdpdx_accent,
            linetype = "dashed"
        ) +
        geom_segment(
            x = 0,
            xend = input$bdpdx_n,
            y = ins,
            yend = ins,
            linewidth = 1,
            color = input$bdpdx_accent,
            linetype = "dashed"
        ) +
        geom_curve(
            data = bdpdx_arrow,
            mapping = aes(
                x = x_start,
                y = y_start,
                xend = x_end,
                yend = y_end
            ),
            arrow = arrow(length = unit(0.08, "inch")),
            size = 0.5,
            color = text_col,
            curvature = bdpdx_arrow$cvt
        ) +
        geom_label(
            data = tibble(),
            aes(
                x = txt_x,
                y = txt_y
            ),
            size = 4,
            color = text_col,
            fill = "transparent",
            label.size = 0,
            label = str_glue(
                "{input$bdpdx_n} people gives a {scales::percent(ins, accuracy = 0.01)} chance
                of at least one shared birthday",
            )
        ) +
        labs(
            x = "Number of people in the group",
            y = "Probability of at least one shared birthday"
        ) +
        scale_y_continuous(
            labels = scales::label_percent()
        ) +
        theme_minimal(base_size = 14) +
        theme(
            text = element_text(color = text_col),
            plot.background = element_rect(fill = bg_col, color = "transparent"),
            plot.margin = margin(30, 30, 30, 30, "points"),
            panel.background = element_rect(fill = bg_col, color = "transparent"),
            panel.grid = element_line(color = grid_col),
            axis.text = element_text(color = text_col),
            axis.text.x = element_text(margin = margin(t = 10)),
        )

    return(plot)
}

plot_clt <- function(input) {
    #' Plot a simulation of the central limit theorem

    set.seed(input$clt_seed)

    if (input$clt_dark) {
        line_col <- "white"
        text_col <- "white"
        bg_col <- "#333333"
        grid_col <- "grey40"
    } else {
        line_col <- "grey30"
        text_col <- "grey30"
        bg_col <- "white"
        grid_col <- "grey90"
    }

    dist_type <- case_when(
        str_detect(str_to_lower(input$clt_pop_dist_type), "normal") ~ "norm",
        str_detect(str_to_lower(input$clt_pop_dist_type), "uniform") ~ "uniform",
        str_detect(str_to_lower(input$clt_pop_dist_type), "left") ~ "left-skew",
        str_detect(str_to_lower(input$clt_pop_dist_type), "right") ~ "right-skew"
    )

    if (dist_type == "norm") {
        pop_dist <- rnorm(1e5, mean = 0, sd = 1)
    } else if (dist_type == "uniform") {
        pop_dist <- runif(1e5, min = -10, max = 10)
    } else if (dist_type == "left-skew") {
        pop_dist <- rbeta(1e5, shape1 = 5, shape2 = 1)
    } else if (dist_type == "right-skew") {
        pop_dist <- rlnorm(1e5, meanlog = 0, sdlog = 0.5)
    }

    sample_dist <- replicate(
        input$clt_n_samples,
        sample(pop_dist, input$clt_sample_size, replace = TRUE)
    )

    data_sampled <- tibble(
        mean = colMeans(sample_dist),
    )

    pop_plot <- ggplot(data = NULL, aes(x = pop_dist, y = ..density..)) +
        geom_histogram(
            color = bg_col,
            fill = input$clt_accent,
            alpha = 0.8
        ) +
        geom_density(
            color = line_col,
            linewidth = 2
        ) +
        ggpp::geom_text_npc(
            data = tibble(),
            aes(
                npcx = ifelse(dist_type != "left-skew", 0.85, 0.6),
                npcy = "top"
            ),
            label = str_glue("\u03bc = {round(mean(pop_dist), 4)}
                \u03c3 = {round(sd(pop_dist), 4)}"),
            size = 6,
            hjust = 0,
            color = text_col
        ) +
        labs(
            x = NULL,
            y = NULL
        ) +
        scale_x_continuous() +
        scale_y_continuous(
            expand = expansion(add = c(0, 0.05))
        ) +
        theme_minimal(base_size = 14) +
        theme(
            text = element_text(color = text_col),
            plot.background = element_rect(fill = bg_col, color = "transparent"),
            plot.margin = margin(30, 30, 30, 30, "points"),
            panel.background = element_rect(fill = bg_col, color = "transparent"),
            panel.grid = element_line(color = grid_col),
            axis.text = element_blank(),
        )

    sample_plot <- ggplot(data = data_sampled, aes(x = mean, y = ..density..)) + 
        geom_histogram(
            color = bg_col,
            fill = input$clt_accent,
            alpha = 0.8
        ) +
        geom_density(
            color = line_col,
            linewidth = 2
        ) +
        ggpp::geom_text_npc(
            data = tibble(),
            aes(
                npcx = 0.85,
                npcy = "top"
            ),
            label = str_glue("\u03bc = {round(mean(data_sampled$mean), 4)}
                \u03c3 = {round(sd(data_sampled$mean), 4)}"),
            hjust = 0,
            size = 6,
            color = text_col
        ) +
        labs(
            x = NULL,
            y = NULL
        ) +
        scale_x_continuous() +
        scale_y_continuous(
            expand = expansion(add = c(0, 0.05))
        ) +
        theme_minimal(base_size = 14) +
        theme(
            text = element_text(color = text_col),
            plot.background = element_rect(fill = bg_col, color = "transparent"),
            plot.margin = margin(30, 30, 30, 30, "points"),
            panel.background = element_rect(fill = bg_col, color = "transparent"),
            panel.grid = element_line(color = grid_col),
            axis.text = element_blank(),
        )

    return(pop_plot / sample_plot)
}
