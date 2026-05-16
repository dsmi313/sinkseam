# app.R
# Shiny app for exploring SL/ST movement profiles and model results.
# Run with: shiny::runApp("app/")
#
# Tabs:
#   1. Movement Explorer  – interactive scatter with filters
#   2. GMM Clusters       – cluster assignments, contingency table, ARI
#   3. Model Results      – posterior summaries and density plots for beta_label

library(shiny)
library(dplyr)
library(ggplot2)
library(mclust)

stopifnot(
  file.exists("data/clean/sl_st_clean.rds"),
  file.exists("data/clean/gmm_results.rds"),
  file.exists("data/clean/jags_results.rds")
)

clean        <- readRDS("data/clean/sl_st_clean.rds")
gmm_results  <- readRDS("data/clean/gmm_results.rds")
jags_results <- readRDS("data/clean/jags_results.rds")

OUTCOMES <- c(
  "Whiff rate"   = "fit_whiff",
  "Chase rate"   = "fit_chase",
  "wOBA against" = "fit_woba"
)

LABEL_COLOURS <- c(SL = "#D55E00", ST = "#009E73")

theme_app <- theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom")

ui <- navbarPage(
  title = "Sweeper vs. Slider",

  tabPanel(
    "Movement Explorer",
    sidebarLayout(
      sidebarPanel(
        width = 3,
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
        helpText("pfx_x is sign-flipped for RHP so positive = glove-side for both hands.")
      ),
      mainPanel(
        plotOutput("movement_scatter", height = "520px"),
        tableOutput("label_counts")
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
          choices  = c("Forced G=2" = "g2", "BIC-optimal" = "bic"),
          selected = "g2"
        ),
        hr(),
        verbatimTextOutput("ari_text"),
        verbatimTextOutput("purity_text"),
        verbatimTextOutput("spin_axis_note")
      ),
      mainPanel(
        plotOutput("gmm_scatter", height = "460px"),
        hr(),
        h4("Contingency table (G=2 vs. label)"),
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
        h4("Posterior summary — beta_label (ST vs. SL effect)"),
        tableOutput("post_summary"),
        plotOutput("post_density", height = "340px"),
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
      filter(p_throws %in% input$hand_filter,
             release_speed >= input$speed_range[1],
             release_speed <= input$speed_range[2])
  })

  sampled_data <- reactive({
    df <- filtered_data()
    n  <- min(nrow(df), input$n_sample)
    slice_sample(df, n = n)
  })

  output$movement_scatter <- renderPlot({
    ggplot(sampled_data(), aes(pfx_x_adj, pfx_z, colour = pitch_type)) +
      geom_point(alpha = 0.2, size = 0.7) +
      scale_colour_manual(values = LABEL_COLOURS,
                          labels = c(SL = "Slider (SL)", ST = "Sweeper (ST)")) +
      labs(x = "Glove-side break (inches)", y = "Vertical break (inches)",
           colour = NULL, title = "SL / ST movement profiles") +
      theme_app
  })

  output$label_counts <- renderTable({
    filtered_data() |>
      count(pitch_type) |>
      rename(Label = pitch_type, Count = n)
  }, striped = TRUE, hover = TRUE)

  cluster_col <- reactive({
    if (input$gmm_choice == "g2") "cluster_g2" else "cluster_bic"
  })

  output$gmm_scatter <- renderPlot({
    df <- sampled_data() |>
      mutate(cluster = factor(.data[[cluster_col()]]))

    ggplot(df, aes(pfx_x_adj, pfx_z, colour = pitch_type, shape = cluster)) +
      geom_point(alpha = 0.2, size = 0.8) +
      scale_colour_manual(values = LABEL_COLOURS) +
      labs(x = "Glove-side break (inches)", y = "Vertical break (inches)",
           colour = "Label", shape = "GMM cluster",
           title = "GMM clusters vs. Statcast label") +
      theme_app
  })

  output$ari_text <- renderText({
    ari <- if (input$gmm_choice == "g2") gmm_results$ari_g2 else gmm_results$ari_bic
    sprintf("Adjusted Rand Index: %.3f", ari)
  })

  output$purity_text <- renderText({
    sprintf("Cluster purity (G=2): %.3f", gmm_results$purity)
  })

  output$spin_axis_note <- renderText({
    if (gmm_results$include_spin_axis) {
      "Spin axis included as 4th GMM feature."
    } else {
      "Spin axis excluded from GMM (movement + speed only)."
    }
  })

  output$contingency_tab <- renderTable({
    as.data.frame.matrix(gmm_results$tab_g2)
  }, rownames = TRUE, striped = TRUE, hover = TRUE)

  selected_fit <- reactive({
    jags_results[[input$outcome_sel]]
  })

  output$post_summary <- renderTable({
    s    <- selected_fit()$summary["beta_label", , drop = FALSE]
    rhat <- selected_fit()$Rhat$beta_label
    data.frame(
      Parameter = "beta_label",
      Mean      = round(s[, "mean"], 4),
      SD        = round(s[, "sd"], 4),
      `2.5%`    = round(s[, "2.5%"], 4),
      `97.5%`   = round(s[, "97.5%"], 4),
      Rhat      = round(rhat, 3),
      check.names = FALSE
    )
  }, striped = TRUE)

  output$post_density <- renderPlot({
    samps <- do.call(rbind, lapply(selected_fit()$samples, as.data.frame))
    df    <- data.frame(value = samps[["beta_label"]])
    ci    <- quantile(df$value, c(0.025, 0.975))

    ggplot(df, aes(value)) +
      geom_density(fill = "#009E73", alpha = 0.3, colour = "#009E73") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = ci, linetype = "dotted", colour = "red") +
      labs(x = expression(beta["label"]), y = "Density",
           title = "Posterior distribution of SL/ST label effect",
           caption = "Red dotted lines: 95% credible interval") +
      theme_app
  })

  output$full_summary <- renderTable({
    params <- c("beta_pfx_x", "beta_pfx_z", "beta_speed", "beta_label",
                "mu_alpha", "tau_alpha")
    s <- selected_fit()$summary[params, c("mean", "sd", "2.5%", "97.5%"),
                                drop = FALSE]
    as.data.frame(round(s, 4)) |>
      tibble::rownames_to_column("Parameter")
  }, striped = TRUE, hover = TRUE)
}

shinyApp(ui = ui, server = server)
