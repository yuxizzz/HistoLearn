# app.R for HistoLearn Shiny interface
# Location: inst/shiny-scripts/app.R

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("HistoLearn: Histology Embedding Explorer"),

  sidebarLayout(
    sidebarPanel(
      h4("1. Upload data"),

      fileInput(
        "feature_file",
        "Upload feature embeddings (CSV)",
        accept = c(".csv", "text/csv", "text/comma-separated-values")
      ),

      fileInput(
        "label_file",
        "Upload labels (CSV, one column)",
        accept = c(".csv", "text/csv", "text/comma-separated-values")
      ),

      checkboxInput("header", "Files have header", TRUE),
      radioButtons(
        "sep", "Separator",
        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
        selected = ","
      ),

      tags$hr(),
      h4("2. Visualization options"),
      sliderInput(
        "viz_dims",
        "Number of dimensions to visualize (PCA)",
        min = 2, max = 8, value = 2, step = 1
      ),
      actionButton("run_viz", "Visualize embeddings"),

      tags$hr(),
      h4("3. Model training & evaluation"),
      sliderInput(
        "train_frac",
        "Train fraction (1 − test fraction)",
        min = 0.5, max = 0.9, value = 0.7, step = 0.05
      ),
      numericInput(
        "dr_k",
        "Reduced dimension (dr_k for PCA)",
        value = 10, min = 2, max = 256, step = 1
      ),
      selectInput(
        "model_type",
        "Classifier",
        choices = c("K-Nearest Neighbors" = "knn"),
        selected = "knn"
      ),
      actionButton("run_model", "Train model & evaluate")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data preview",
          h4("Head of feature embeddings"),
          tableOutput("feature_head"),
          tags$hr(),
          h4("Head of labels"),
          tableOutput("label_head")
        ),
        tabPanel(
          "Embedding visualization",
          h4("Dimensionality-reduced embeddings"),
          plotOutput("viz_plot")
        ),
        tabPanel(
          "Model performance",
          h4("Confusion matrix"),
          plotOutput("cm_plot"),
          tags$hr(),
          h4("Metrics"),
          verbatimTextOutput("metric_text")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # ---- Reactives: load raw feature + label data ----

  feature_df <- reactive({
    req(input$feature_file)

    df <- tryCatch(
      {
        read.csv(
          input$feature_file$datapath,
          header = input$header,
          sep = input$sep,
          check.names = FALSE
        )
      },
      error = function(e) {
        showNotification(
          paste("Error reading feature file:", e$message),
          type = "error"
        )
        return(NULL)
      }
    )

    df
  })

  label_vec <- reactive({
    req(input$label_file)

    lab_df <- tryCatch(
      {
        read.csv(
          input$label_file$datapath,
          header = input$header,
          sep = input$sep,
          check.names = FALSE
        )
      },
      error = function(e) {
        showNotification(
          paste("Error reading label file:", e$message),
          type = "error"
        )
        return(NULL)
      }
    )

    if (is.null(lab_df)) return(NULL)

    if (ncol(lab_df) != 1L) {
      showNotification(
        "Label file must have exactly one column.",
        type = "error"
      )
      return(NULL)
    }

    as.vector(lab_df[[1]])
  })

  # ---- Preview outputs ----

  output$feature_head <- renderTable({
    df <- feature_df()
    if (is.null(df)) return(NULL)
    head(df)
  })

  output$label_head <- renderTable({
    lab <- label_vec()
    if (is.null(lab)) return(NULL)
    head(data.frame(label = lab))
  })

  # ---- Construct histofeature object using load_embeddings() ----

  histofeature_obj <- reactive({
    feat <- feature_df()
    lab  <- label_vec()
    req(feat, lab)

    if (nrow(feat) != length(lab)) {
      showNotification(
        paste0(
          "Number of rows in feature embeddings (", nrow(feat),
          ") does not match length of labels (", length(lab), ")."
        ),
        type = "error"
      )
      return(NULL)
    }

    # This is YOUR function:
    # load_embeddings(feature, label)
    hf <- HistoLearn::load_embeddings(
      feature = feat,
      label   = lab
    )

    hf
  })

  # ---- Visualization using visualize_embeddings() ----

  viz_obj <- reactiveVal(NULL)

  observeEvent(input$run_viz, {
    hf <- histofeature_obj()
    req(hf)

    dims <- input$viz_dims
    if (dims < 2 || dims > 8) {
      showNotification("Dimensions must be between 2 and 8.", type = "error")
      return(NULL)
    }

    vis <- HistoLearn::visualize_embeddings(
      input_data = hf,
      dimensions = dims,
      type       = "pca"
    )

    viz_obj(vis)
  })

  output$viz_plot <- renderPlot({
    vis <- viz_obj()
    if (is.null(vis)) return(NULL)
    print(vis)
  })

  # ---- Model training & evaluation using train_model() + evaluate_model() ----

  trained_model_rv <- reactiveVal(NULL)
  eval_result_rv   <- reactiveVal(NULL)

  observeEvent(input$run_model, {
    hf <- histofeature_obj()
    req(hf)

    feature <- hf$feature
    label   <- hf$label

    if (is.null(feature) || is.null(label)) {
      showNotification(
        "Both features and labels are required to train a model.",
        type = "error"
      )
      return(NULL)
    }

    n <- nrow(feature)
    if (n < 5) {
      showNotification(
        "Need at least 5 samples to train and test.",
        type = "error"
      )
      return(NULL)
    }

    train_frac <- input$train_frac
    set.seed(123)
    train_idx <- sample(seq_len(n), size = floor(train_frac * n))

    feature_train <- feature[train_idx, , drop = FALSE]
    label_train   <- label[train_idx]

    feature_test  <- feature[-train_idx, , drop = FALSE]
    label_test    <- label[-train_idx]

    # histofeature train/test using your constructor
    hf_train <- HistoLearn::load_embeddings(
      feature = feature_train,
      label   = label_train
    )

    hf_test <- HistoLearn::load_embeddings(
      feature = feature_test,
      label   = label_test
    )

    # Train model (your function)
    tm <- HistoLearn::train_model(
      feature_embedding = hf_train,
      dr    = "pca",
      dr_k  = input$dr_k,
      model = input$model_type
    )

    trained_model_rv(tm)

    # Evaluate model (your function)
    ev <- HistoLearn::evaluate_model(
      trained_model = tm,
      test_data     = hf_test
    )

    eval_result_rv(ev)
  })

  output$cm_plot <- renderPlot({
    ev <- eval_result_rv()
    if (is.null(ev)) return(NULL)

    cm_plot <- ev$conf_matrix
    if (inherits(cm_plot, "ggplot")) {
      print(cm_plot)
    } else {
      plot(cm_plot)
    }
  })

  output$metric_text <- renderPrint({
    ev <- eval_result_rv()
    if (is.null(ev)) {
      cat("No evaluation results yet.\n")
      return(invisible(NULL))
    }

    metric <- ev$metric
    if (is.null(metric)) {
      cat("No metrics available.\n")
    } else {
      print(metric)
    }
  })
}

shinyApp(ui = ui, server = server)
