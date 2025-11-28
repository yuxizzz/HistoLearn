library(shiny)
library(ggplot2)

# UI -------------------
ui <- fluidPage(
  titlePanel("HistoLearn: Histology Embedding Explorer"),

  sidebarLayout(
    sidebarPanel(
      # App description and usage context
      tags$p("Description: This Shiny App is part of the HistoLearn R package.
      It provides an interactive interface for exploring, visualizing, and
      modeling histological feature embeddings derived from computational
      pathology foundation models. Users can upload embedding matrices and
      corresponding sample labels, generate PCA-based dimensionality-reduced
      visualizations, and train supervised learning models through the
      HistoLearn workflow. Supported classifiers include k-nearest neighbors
      (kNN) and logistic regression. The app also enables model evaluation,
      including confusion matrices and accuracy metrics for train and test sets.
      Example data files (subset_embeddings.csv and subset_labels.csv) are
      available on GitHub at: https://github.com/yuxizzz/HistoLearn/tree/main/inst/extdata."),
      br(),

      # Instructions for workflow
      tags$b("Instructions: Begin by uploading a feature embedding matrix
      (rows = samples, columns = features) and the corresponding label file.
      After loading the data, you may visualize the embeddings using PCA by
      selecting the number of dimensions and clicking 'Visualize embeddings'.
      To train a classifier, specify the training proportion, reduced
      dimensionality, and model type, then press 'Train model & evaluate'.
      Navigate through the tabs on the right to view the embedding
             visualization, confusion matrices, and evaluation metrics."),

      br(),
      h4("1. Upload data"),

      tags$p("Please upload a file (csv or tsv) containing your feature embeddings
      and another file containing your labels for the feature embeddings.
             Note: In the feature embeddings file, the row should correspond to
             a sample and each column to a feature. The label_file should only
             contain 1 column."),

      # File inputs for features and labels
      fileInput(
        "feature_file",
        "Upload feature embeddings",
        accept = c(".csv", "text/csv", "text/comma-separated-values")
      ),

      fileInput(
        "label_file",
        "Upload labels (one column)",
        accept = c(".csv", "text/csv", "text/comma-separated-values")
      ),

      # CSV parsing options
      checkboxInput("header", "Files have header", TRUE),
      radioButtons(
        "sep", "Separator",
        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
        selected = ","
      ),

      tags$hr(),
      h4("2. Visualization options"),

      # PCA visualization settings
      sliderInput(
        "viz_dims",
        "Number of dimensions to visualize (PCA)",
        min = 2, max = 10, value = 2, step = 1
      ),
      actionButton("run_viz", "Visualize embeddings"),

      tags$hr(),
      h4("3. Model training & evaluation"),
      tags$p("Note: The train fraction ranges from 0.5 to 0.9 to ensure sufficient
        data for both training and testing. Choose the PCA reduced dimension (k)
        carefully—do not set it too large. In practice, selecting around 10–20
        components is often appropriate."),
      # Training fraction slider
      sliderInput(
        "train_frac",
        "Train fraction (1 − test fraction)",
        min = 0.5, max = 0.9, value = 0.7, step = 0.05
      ),
      # Reduced dimension and classifier selection
      numericInput(
        "dr_k",
        "Reduced dimension (k for PCA)",
        value = 10, min = 2, max = 256, step = 1
      ),
      selectInput(
        "model_type",
        "Classifier",
        choices = c(
          "K-Nearest Neighbors" = "knn",
          "Logistic Regression" = "logistic"
        ), selected = "knn"),
        actionButton("run_model", "Train model & evaluate")
    ),

    mainPanel(
      tabsetPanel(
        # Tab: data preview
        tabPanel(
          "Data preview",
          h4("Head of feature embeddings"),
          tableOutput("feature_head"),
          tags$hr(),
          h4("Head of labels"),
          tableOutput("label_head")
        ),
        # Tab: embedding visualization
        tabPanel(
          "Embedding visualization",
          h4("Dimensionality-reduced embeddings"),
          plotOutput("viz_plot")
        ),
        # Tab: model performance (confusion matrices & metrics)
        tabPanel(
          "Model performance",
          h4("Train Set Confusion matrix"),
          plotOutput("cm_plot_train"),
          h4("Test Set Confusion matrix"),
          plotOutput("cm_plot"),
          tags$hr(),
          h4("Model Metrics"),
          verbatimTextOutput("metric_text")
        )
      )
    )
  )
)

# Server: data handling, modeling, and visualization -------------------
server <- function(input, output, session) {

  # Reactives: load raw feature + label data

  feature_df <- reactive({
    req(input$feature_file)

    # Attempt to read feature file; report any errors via notification
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

  # Reactives: load label data
  label_vec <- reactive({
    req(input$label_file)

    # Attempt to read label file; report any errors via notification
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
    # Ensure label file has exactly one column
    if (ncol(lab_df) != 1L) {
      showNotification(
        "Label file must have exactly one column.",
        type = "error"
      )
      return(NULL)
    }

    as.vector(lab_df[[1]])
  })

  # Preview outputs: show head of features and label
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

  # Construct histofeature object
  histofeature_obj <- reactive({
    feat <- feature_df()
    lab  <- label_vec()
    req(feat, lab)

    # Check that number of labels matches number of samples
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

    # Use HistoLearn helper to create histofeature object
    hf <- HistoLearn::load_embeddings(
      feature = feat,
      label   = lab
    )

    hf
  })

  # Visualization using visualize_embeddings()

  viz_obj <- reactiveVal(NULL)

  observeEvent(input$run_viz, {
    hf <- histofeature_obj()
    req(hf)

    dims <- input$viz_dims

    # Sanity check on number of dimensions
    if (dims < 2 || dims > 10) {
      showNotification("Dimensions must be between 2 and 10.", type = "error")
      return(NULL)
    }

    # Generate PCA-based embedding visualization via HistoLearn
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

  # Model training & evaluation
  trained_model_rv <- reactiveVal(NULL)
  eval_result_rv   <- reactiveVal(NULL)

  observeEvent(input$run_model, {
    hf <- histofeature_obj()
    req(hf)

    feature <- hf$feature
    label   <- hf$label

    # Ensure both feature matrix and labels are available
    if (is.null(feature) || is.null(label)) {
      showNotification(
        "Both features and labels are required to train a model.",
        type = "error"
      )
      return(NULL)
    }

    n <- nrow(feature)
    # Require minimum sample size for meaningful train/test split
    if (n < 5) {
      showNotification(
        "Need at least 5 samples to train and test.",
        type = "error"
      )
      return(NULL)
    }

    # Check that requested reduced dimension does not exceed feature count
    p <- ncol(feature)
    if (input$dr_k > p) {
      showNotification(
        paste0(
          "Reduced dimension (k = ", input$dr_k,
          ") cannot exceed the number of features (", p, ")."
        ),
        type = "error"
      )
      return(NULL)
    }

    # Train/test split based on user-specified fraction
    train_frac <- input$train_frac
    set.seed(123)
    train_idx <- sample(seq_len(n), size = floor(train_frac * n))

    feature_train <- feature[train_idx, , drop = FALSE]
    label_train   <- label[train_idx]

    feature_test  <- feature[-train_idx, , drop = FALSE]
    label_test    <- label[-train_idx]

    # histofeature train/test
    hf_train <- HistoLearn::load_embeddings(
      feature = feature_train,
      label   = label_train
    )

    hf_test <- HistoLearn::load_embeddings(
      feature = feature_test,
      label = label_test
    )

    # Train model
    tm <- HistoLearn::train_model(
      feature_embedding = hf_train,
      dr    = "pca",
      dr_k  = input$dr_k,
      model = input$model_type
    )

    trained_model_rv(tm)

    # Evaluate model
    ev <- HistoLearn::evaluate_model(
      trained_model = tm,
      test_data = hf_test
    )

    eval_result_rv(ev)
  })

  # Render test confusion matrix plot
  output$cm_plot <- renderPlot({
    ev <- eval_result_rv()
    if (is.null(ev)) return(NULL)

    cm_plot <- ev$test_conf_matrix
    if (inherits(cm_plot, "ggplot")) {
      print(cm_plot)
    }

  })

  # Render train confusion matrix plot
  output$cm_plot_train <- renderPlot(
    {
      ev <- eval_result_rv()
      if (is.null(ev)) return(NULL)

      cm_plot_train <- ev$train_conf_matrix
      if (inherits(cm_plot_train, "ggplot")) {
        print(cm_plot_train)
      }
    }
  )

  # Render model performance metrics (accuracy)
  output$metric_text <- renderPrint({
    ev <- eval_result_rv()
    if (is.null(ev)) {
      cat("No evaluation results yet.\n")
      return(invisible(NULL))
    }
    tm <- trained_model_rv()

    cat("Model type:", tm$method[2], "\n")
    test_acc  <- ev$test_metric
    train_acc <- ev$train_metric
    cat(sprintf("Train accuracy: %.3f\n", train_acc))
    cat(sprintf("Test  accuracy: %.3f\n", test_acc))
  })
}

# Launch the Shiny application
shinyApp(ui = ui, server = server)
# [END]
