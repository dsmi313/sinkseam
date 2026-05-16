# app.R
# Shiny app for exploring CU/SL/ST movement profiles and model results.
# Run with: shiny::runApp("app/")
#
# Tabs:
#   1. Movement Explorer – interactive scatter with handedness and speed filters
#   2. PCA               – PC scores biplot, variance explained, loadings table
#   3. GMM Clusters      – G=2, G=3, and BIC-optimal solutions, ARI, contingency
#   4. Model Results     – posterior summaries for beta_sl, beta_st, beta_st_vs_sl

library(shiny)
library(dplyr)
library(ggplot2)
library(mclust)

stopifnot(
  file.exists("data/clean/cu_sl_st_clean.rds"),
  file.exists("data/clean/pca_results.rds"),
  file.exists("data/clean/gmm_results.rds"),
  file.exists("data/clean/jags_results.rds")
)

clean        <- readRDS("data/clean/cu_sl_st_clean.rds")
pca_results  <- readRDS("data/clean/pca_results.rds")
gmm_results  <- readRDS("data/clean/gmm_results.rds")
jags_results <- readRDS("data/clean/jags_results.rds")

OUTCOMES <- c(
  "Whiff rate"   = "fit_whiff",
  "Chase rate"   = "fit_chase",
  "wOBA against" = "fit_woba"
)

LABEL_COLOURS <- c(CU = "#0072B2", SL = "#D55E00", ST = "#009E73")
LABEL_NAMES   <- c(CU = "Curveball", SL = "Slider", ST = "Sweeper")

CONTRAST_COLOURS <- c("SL vs. CU" = "#D55E00",
                       "ST vs. CU" = "#009E73",
                       "ST vs. SL" = "#CC79A7")

theme_app <- theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom")

ui <- navbarPage(
  title = "Breaking Ball Taxonomy",

  tabPanel(
    "Movement Explorer",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        checkboxGroupInput(
          "pt_filter", "Pitch type",
          choices  = c("CU", "SL", "ST"),
          selected = c("CU", "SL", "ST")
        ),
        checkboxGroupInput(
          "hand_filter", "Pitcher handedness",
          choices  = c("RHP" = "R", "LHP" = "L"),
          selected = c("R", "L")
        ),
        sliderInput(
          "speed_range", "Release speed (mph)",
          min   = floor(min(clean$release_speed, na.rm = TRUE)),
          max   = ceiling(max(clean$release_speed, na.rm = TRUE)),
          value = c(floor(min(clean$release_speed, na.rm = TRUE)),
                    ceiling(max(clean$release_speed, na.rm = TRUE))),
          step  = 1
        ),
        numericInput("n_sample", "Points to plot (max)", 20000, min = 1000,
                     max = 50000, step = 1000),
        hr(),
        helpText("pfx_x flipped for LHP; positive = arm-side, negative = glove-side.")
      ),
      mainPanel(
        plotOutput("movement_scatter", height = "500px"),
        tableOutput("label_counts")
      )
    )
  ),

  tabPanel(
    "PCA",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        checkboxGroupInput(
          "pca_pt", "Pitch types to highlight",
          choices  = c("CU", "SL", "ST"),
          selected = c("CU", "SL", "ST")
        ),
        numericInput("pca_n", "Points to plot (max)", 15000, min = 1000,
                     max = 40000, step = 1000),
        hr(),
        verbatimTextOutput("var_exp_text"),
        hr(),
        h5("Feature loadings"),
        tableOutput("loadings_tab")
      ),
      mainPanel(
        plotOutput("pca_scatter", height = "480px")
      )
    )
  ),

  tabPanel(
    "GMM Clusters",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(
          "gmm_choice", "Cluster solution",
          choices  = c("Forced G=2" = "g2",
                       "Forced G=3" = "g3",
                       "BIC-optimal" = "bic"),
          selected = "g3"
        ),
        hr(),
        verbatimTextOutput("ari_text"),
        verbatimTextOutput("purity_text"),
        verbatimTextOutput("spin_axis_note")
      ),
      mainPanel(
        plotOutput("gmm_scatter", height = "460px"),
        hr(),
        h4("Contingency table"),
        tableOutput("contingency_tab")
      )
    )
  ),

  tabPanel(
    "Model Results",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(
          "outcome_sel", "Outcome",
          choices  = OUTCOMES,
          selected = "fit_whiff"
        )
      ),
      mainPanel(
        h4("Posterior summaries — label effects (CU reference)"),
        tableOutput("post_summary"),
        plotOutput("post_density", height = "380px"),
        hr(),
        h4("Full fixed-effect posterior means"),
        tableOutput("full_summary")
      )
    )
  )
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    clean |>
      filter(pitch_type %in% input$pt_filter,
             p_throws %in% input$hand_filter,
             release_speed >= input$speed_range[1],
             release_speed <= input$speed_range[2])
  })

  sampled_data <- reactive({
    df <- filtered_data()
    slice_sample(df, n = min(nrow(df), input$n_sample))
  })

  output$movement_scatter <- renderPlot({
    ggplot(sampled_data(), aes(pfx_x_adj, pfx_z, colour = pitch_type)) +
      geom_point(alpha = 0.15, size = 0.6) +
      scale_colour_manual(values = LABEL_COLOURS, labels = LABEL_NAMES) +
      labs(x = "pfx_x (adj., inches)", y = "pfx_z (inches)",
           colour = NULL, title = "CU / SL / ST movement profiles") +
      theme_app
  })

  output$label_counts <- renderTable({
    filtered_data() |>
      count(pitch_type) |>
      rename(Label = pitch_type, Count = n)
  }, striped = TRUE, hover = TRUE)

  output$pca_scatter <- renderPlot({
    sub <- pca_results$scores_df |>
      filter(pitch_type %in% input$pca_pt) |>
      slice_sample(n = min(n(), input$pca_n))

    var1 <- round(pca_results$var_exp[1] * 100, 1)
    var2 <- round(pca_results$var_exp[2] * 100, 1)

    arrow_scale <- max(abs(sub[, c("PC1", "PC2")])) * 0.4
    ldg <- as.data.frame(pca_results$pca$rotation[, 1:2]) |>
      tibble::rownames_to_column("feature") |>
      mutate(xend = PC1 * arrow_scale, yend = PC2 * arrow_scale,
             label = recode(feature, pfx_x_z = "H.break", pfx_z_z = "V.break",
                            speed_z = "Velo", spin_axis_z = "Spin axis"))

    ggplot(sub, aes(PC1, PC2, colour = pitch_type)) +
      geom_point(alpha = 0.15, size = 0.5) +
      geom_segment(data = ldg, aes(x = 0, y = 0, xend = xend, yend = yend),
                   colour = "black", linewidth = 0.7,
                   arrow = arrow(length = unit(0.18, "cm")), inherit.aes = FALSE) +
      geom_text(data = ldg, aes(x = xend * 1.2, y = yend * 1.2, label = label),
                colour = "black", size = 3, inherit.aes = FALSE) +
      scale_colour_manual(values = LABEL_COLOURS, labels = LABEL_NAMES) +
      labs(x = sprintf("PC1 (%.1f%%)", var1), y = sprintf("PC2 (%.1f%%)", var2),
           colour = NULL, title = "PCA of breaking ball movement space") +
      theme_app
  })

  output$var_exp_text <- renderText({
    sprintf(
      "PC1: %.1f%%\nPC1+PC2: %.1f%%",
      pca_results$var_exp[1] * 100,
      pca_results$cum_var[2] * 100
    )
  })

  output$loadings_tab <- renderTable({
    as.data.frame(round(pca_results$pca$rotation[, 1:2], 3)) |>
      tibble::rownames_to_column("Feature")
  }, striped = TRUE)

  cluster_col <- reactive({
    switch(input$gmm_choice,
           g2  = "cluster_g2",
           g3  = "cluster_g3",
           bic = "cluster_bic")
  })

  output$gmm_scatter <- renderPlot({
    df <- sampled_data() |>
      mutate(cluster = factor(.data[[cluster_col()]]))

    ggplot(df, aes(pfx_x_adj, pfx_z, colour = pitch_type, shape = cluster)) +
      geom_point(alpha = 0.2, size = 0.8) +
      scale_colour_manual(values = LABEL_COLOURS, labels = LABEL_NAMES) +
      labs(x = "pfx_x (adj., inches)", y = "pfx_z (inches)",
           colour = "Label", shape = "Cluster",
           title = "GMM clusters vs. Statcast label") +
      theme_app
  })

  output$ari_text <- renderText({
    ari <- switch(input$gmm_choice,
                  g2  = gmm_results$ari_g2,
                  g3  = gmm_results$ari_g3,
                  bic = gmm_results$ari_bic)
    sprintf("Adjusted Rand Index: %.3f", ari)
  })

  output$purity_text <- renderText({
    sprintf("Cluster purity (G=3): %.3f", gmm_results$purity_g3)
  })

  output$spin_axis_note <- renderText({
    if (gmm_results$include_spin_axis) "Spin axis included as 4th GMM feature."
    else "Spin axis excluded (movement + speed only)."
  })

  output$contingency_tab <- renderTable({
    tab <- if (input$gmm_choice == "g2") gmm_results$tab_g2 else gmm_results$tab_g3
    as.data.frame.matrix(tab)
  }, rownames = TRUE, striped = TRUE, hover = TRUE)

  selected_fit <- reactive({
    jags_results[[input$outcome_sel]]
  })

  output$post_summary <- renderTable({
    betas <- c("beta_sl", "beta_st", "beta_st_vs_sl")
    labels <- c("SL vs. CU", "ST vs. CU", "ST vs. SL (derived)")
    rows <- lapply(seq_along(betas), function(i) {
      b    <- betas[i]
      s    <- selected_fit()$summary[b, ]
      rhat <- selected_fit()$Rhat[[b]]
      data.frame(
        Contrast = labels[i],
        Mean     = round(s["mean"], 4),
        SD       = round(s["sd"],   4),
        `2.5%`   = round(s["2.5%"], 4),
        `97.5%`  = round(s["97.5%"], 4),
        Rhat     = round(rhat, 3),
        check.names = FALSE
      )
    })
    do.call(rbind, rows)
  }, striped = TRUE)

  output$post_density <- renderPlot({
    samps <- do.call(rbind, lapply(selected_fit()$samples, as.data.frame))

    post_df <- bind_rows(
      data.frame(value = samps[["beta_sl"]], contrast = "SL vs. CU"),
      data.frame(value = samps[["beta_st"]], contrast = "ST vs. CU"),
      data.frame(value = samps[["beta_st_vs_sl"]], contrast = "ST vs. SL")
    )

    ggplot(post_df, aes(value, fill = contrast, colour = contrast)) +
      geom_density(alpha = 0.3, linewidth = 0.8) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      facet_wrap(~ contrast, scales = "free_y") +
      scale_colour_manual(values = CONTRAST_COLOURS) +
      scale_fill_manual(values = CONTRAST_COLOURS) +
      labs(x = expression(beta ~ "(log-odds or raw)"), y = "Density",
           title = "Posterior distributions of label contrasts",
           fill = NULL, colour = NULL) +
      theme_app +
      theme(legend.position = "none")
  })

  output$full_summary <- renderTable({
    params <- c("beta_pfx_x", "beta_pfx_z", "beta_speed",
                "beta_sl", "beta_st", "beta_st_vs_sl",
                "mu_alpha", "tau_alpha")
    s <- selected_fit()$summary[params, c("mean", "sd", "2.5%", "97.5%"),
                                drop = FALSE]
    as.data.frame(round(s, 4)) |>
      tibble::rownames_to_column("Parameter")
  }, striped = TRUE, hover = TRUE)
}

shinyApp(ui = ui, server = server)
