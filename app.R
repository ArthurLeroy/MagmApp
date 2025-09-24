# app.R — local only, avec onglet Admin et ré-entrainement
library(MagmaClustR)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(shiny)
library(tibble)

# ---------------------------
#   CONFIG DOSSIERS & FICHIERS (LOCAL)
# ---------------------------
data_dir    <- "data"
inputs_dir  <- file.path(data_dir, "inputs")
model_dir   <- file.path(data_dir, "model")
dir.create(inputs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir,  recursive = TRUE, showWarnings = FALSE)

df_train_path <- file.path(model_dir, "df_train.csv")
model_se_path <- file.path(model_dir, "model_se.rds")

# ---------------------------
#   UI
# ---------------------------
ui <- navbarPage(
  "MagmApp",

  tabPanel("Saisie des données",
    textInput("nameid", "Entrez le nom et la couleur de votre vache (puis cliquez sur 'Valider')"),
    actionButton("validationid", "Valider"),
    selectInput("button_samples", "Visualisation de l'incertude",
                choices = c("Bande de crédibilité" = "FALSE",
                            "Echantillon" = "TRUE"),
                selected = "FALSE"),
    actionButton("reset1", "Effacer"),
    actionButton("sendpoints", "Sauvegarder les données"),
    fluidRow(
      plotOutput("dataplot", click = "dataplot_click", width= "90%", height = "700px")
    )
  ),

  tabPanel("Prédiction",
    uiOutput("choose_name_ui"),
    actionButton("reset2", "Effacer les points"),
    selectInput("button_samples2", "Visualisation de l'incertude",
                choices = c("Bande de crédibilité" = "FALSE",
                            "Echantillon" = "TRUE"),
                selected = "FALSE"),
    fluidRow(
      plotOutput("magmaplot", click = "magmaplot_click", width = "90%", height = "700px")
    ),
    verbatimTextOutput("info")
  ),

  tabPanel("Admin",
    h3("Gestion du modèle"),
    actionButton("train_model", "Ré-entraîner le modèle"),
    verbatimTextOutput("train_log")
  )
)

# ---------------------------
#   SERVER
# ---------------------------
server <- function(input, output, session) {

  # ---------- Etat réactif pour le modèle/df_train ----------
  model_se <- reactiveVal(if (file.exists(model_se_path)) readRDS(model_se_path) else NULL)
  df_train <- reactiveVal(
    if (file.exists(df_train_path)) read_csv(df_train_path, show_col_types = FALSE)
    else tibble(ID = character(), Input = numeric(), Output = numeric())
  )

  # Mettre à jour le sélecteur d'ID en prédiction quand df_train change
  output$choose_name_ui <- renderUI({
    ids <- unique(df_train()$ID)
    selectInput('choose_name', 'Quelle vache souhaitez vous étudier ?', choices = ids, selected = if (length(ids)) ids[1] else NULL)
  })

  # ---------- Paramètres par défaut ----------
  grid_inputs      <- seq(0, 200, 0.2)        # pour la saisie (x 0..200)
  ylim_collect     <- c(0, 500)               # pour la saisie (y 0..500)
  hp               <- tibble(se_variance = 7, se_lengthscale = 5)

  grid_inputs_pred <- reactive({
    # cohérent avec ton ancien script : données d'entraînement au format [0,2] (Input/100)
    if (nrow(df_train()) > 0) seq(0, 2, 0.01) %>% union(unique(df_train()$Input)) else seq(0, 2, 0.01)
  })

  # hyperposterior selon le modèle + grille
  hyperpost <- reactive({
    if (is.null(model_se())) return(NULL)
    tryCatch(
      hyperposterior_clust(trained_model = model_se(), grid_inputs = grid_inputs_pred()),
      error = function(e) NULL
    )
  })

  # ---------- COLLECTE ----------
  empty_db <- NULL
  name_id <- reactiveVal(NULL)
  nom_valide <- reactiveVal(FALSE)

  observeEvent(input$validationid, {
    name_id(input$nameid)
    nom_valide(TRUE)
  })

  df_click <- reactiveVal(empty_db)
  df_name <- reactiveVal(empty_db)
  df_pred_name <- reactiveVal(empty_db)

  observeEvent(input$choose_name, {
    if (nrow(df_train()) > 0 && !is.null(input$choose_name) && nzchar(input$choose_name)) {
      df_pred_name(
        df_train() %>%
          dplyr::filter(ID == input$choose_name) %>%
          dplyr::select(-any_of(c("Cluster", "Proba")))
      )
    } else {
      df_pred_name(empty_db)
    }
    df_click_magmaplot(empty_db)
  }, ignoreInit = TRUE)

  observeEvent(input$dataplot_click, {
    if (!isTRUE(nom_valide())) return(NULL)
    click_x <- input$dataplot_click$x
    click_y <- input$dataplot_click$y
    if (is.na(click_x) || is.na(click_y)) return(NULL)
    df_click_val <- df_click()
    nouvelle_ligne <- data.frame(ID = input$nameid,
                                 Input = round(click_x, 1),
                                 Output = round(click_y, 2))
    df_click(if (is.null(df_click_val)) nouvelle_ligne else rbind(df_click_val, nouvelle_ligne))
  })

  df_click_magmaplot <- reactiveVal(empty_db)
  observeEvent(input$magmaplot_click, {
    click_x <- input$magmaplot_click$x
    click_y <- input$magmaplot_click$y
    if (is.na(click_x) || is.na(click_y) || is.null(input$choose_name)) return(NULL)
    df_click_val_magmaplot <- df_click_magmaplot()
    nouvelle_ligne <- data.frame(ID = input$choose_name,
                                 Input = round(click_x, 2),
                                 Output = round(click_y, 2))
    df_click_magmaplot(if (is.null(df_click_val_magmaplot)) nouvelle_ligne else rbind(df_click_val_magmaplot, nouvelle_ligne))
  })

  df_selectionne <- reactive({ rbind(df_name(), df_click()) })
  df_pred <- reactive({ rbind(df_pred_name(), df_click_magmaplot()) })

  reset_df_selectionne <- function() { df_name(NULL); df_click(NULL); df_click_magmaplot(NULL) }
  observeEvent(input$reset1, { reset_df_selectionne() })
  observeEvent(input$reset2, { reset_df_selectionne() })

  observeEvent(input$sendpoints, {
    nom_utilisateur <- name_id()
    if (is.null(nom_utilisateur) || is.null(df_selectionne())) return(NULL)
    nom_fichier_local <- file.path(inputs_dir, paste0(nom_utilisateur, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"))
    write.csv(df_selectionne(), nom_fichier_local, row.names = FALSE)
    showNotification(paste("Points sauvegardés dans", nom_fichier_local), type = "message")
  })

  # GP (onglet saisie)
  prior <- reactive({
    pred_gp(
      data = df_selectionne(),
      kern = 'SE',
      mean = 200,
      grid_inputs = grid_inputs,
      hp = hp,
      get_full_cov = TRUE,
      plot = FALSE
    )
  })

  output$dataplot <- renderPlot({
    req(prior())
    plot_gp(
      pred_gp = prior(),
      data = df_selectionne(),
      samples = input$button_samples
    ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, 200, 25), limits = c(0, 200)) +
      scale_y_continuous(breaks = seq(0, 500, 50), limits = ylim_collect)
  })

  # ---------- PREDICTION ----------
  pred <- reactive({
    if (is.null(model_se())) return(NULL)
    # hyperpost peut être NULL : on laisse MagmaClustR gérer s'il peut
    tryCatch(
      pred_magmaclust(
        data = df_pred(),
        trained_model = model_se(),
        kern = 'SE',
        hyperpost = hyperpost(),
        grid_inputs = grid_inputs_pred(),
        get_full_cov = TRUE,
        get_hyperpost = TRUE,
        plot = FALSE
      ),
      error = function(e) NULL
    )
  })

  output$magmaplot <- renderPlot({
    pr <- pred()
    if (is.null(pr)) return(NULL)
    df_tr <- df_train()
    if (is.null(df_tr) || nrow(df_tr) == 0) return(NULL)

    pm <- tryCatch(pr$hyperpost$mean, error = function(e) NULL)
    p <- tryCatch(
      plot_magmaclust(
        pred_clust  = pr,
        data        = df_pred(),
        data_train  = df_tr,
        col_clust   = TRUE,
        prior_mean  = pm,
        samples     = input$button_samples2
      ),
      error = function(e) NULL
    )
    if (inherits(p, "ggplot")) p else NULL
  })

  output$info <- renderPrint({
    list(
      n_train = nrow(df_train()),
      ids = unique(df_train()$ID),
      has_model = !is.null(model_se()),
      grid_pred_len = length(grid_inputs_pred()),
      has_hyperpost = !is.null(hyperpost())
    )
  })

  # ---------- ADMIN : Ré-entraînement local ----------
  output$train_log <- renderPrint({ "Appuyez sur le bouton pour ré-entraîner le modèle à partir de data/inputs/*.csv" })

  observeEvent(input$train_model, {
    withProgress(message = "Entraînement du modèle en cours...", value = 0, {
      fichiers <- list.files(inputs_dir, full.names = TRUE, pattern = "\\.csv$")
      if (length(fichiers) == 0) {
        output$train_log <- renderPrint({ "Aucune donnée trouvée dans data/inputs/" })
        return(NULL)
      }

      datasets <- lapply(fichiers, function(f) tryCatch(read_csv(f, show_col_types = FALSE), error = function(e) NULL))
      datasets <- Filter(Negate(is.null), datasets)
      if (length(datasets) == 0) {
        output$train_log <- renderPrint({ "Impossible de lire les CSV de data/inputs/." })
        return(NULL)
      }

      df_train_new <- bind_rows(datasets) %>%
        distinct(ID, Input, .keep_all = TRUE) %>%
        mutate(Input = Input/100, Output = Output/100)

      incProgress(0.5, detail = "Apprentissage...")
      model_se_new <- tryCatch(
        train_magmaclust(
          data       = df_train_new,
          nb_cluster = 3,
          prior_mean = 2,
          kern_k     = "SE",
          kern_i     = "SE"
        ),
        error = function(e) { output$train_log <- renderPrint({ paste("Erreur train_magmaclust:", e$message) }); NULL }
      )
      if (is.null(model_se_new)) return(NULL)

      incProgress(0.8, detail = "Attribution des clusters...")
      df_train_new <- tryCatch(data_allocate_cluster(model_se_new), error = function(e) df_train_new)

      # Sauvegardes locales
      saveRDS(model_se_new, model_se_path)
      write_csv(df_train_new, df_train_path)

      # 🔁 Mettre à jour les réactifs -> tout se met à jour (hyperpost, choix, graphs)
      model_se(model_se_new)
      df_train(df_train_new)

      incProgress(1, detail = "Terminé.")
      output$train_log <- renderPrint({
        paste("Nouveau modèle entraîné avec", nrow(df_train_new), "lignes. Modèle et df_train sauvegardés dans data/model/.")
      })

      showNotification("Modèle ré-entrainé et mis à jour ✅", type = "message")
    })
  })
}

# ---------------------------
#   RUN APP
# ---------------------------
shinyApp(ui, server)
