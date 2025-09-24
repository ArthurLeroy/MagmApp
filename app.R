# app.R
library(shiny)
library(ggplot2)

# ---- CONFIG ----
data_dir <- "data"
inputs_dir <- file.path(data_dir, "inputs")
model_dir  <- file.path(data_dir, "model")
dir.create(inputs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

# canonical "training" model file (le modèle principal)
model_file <- file.path(model_dir, "training.rds")

# ---- TEXTS (dictionnaire) ----
texts <- list(
  fr = list(
    app_title            = "Démo GP multitâches",
    collect_tab          = "Collecte",
    prediction_tab       = "Prédiction",
    admin_tab            = "Admin",
    user_id              = "Identifiant utilisateur :",
    save_points          = "Enregistrer les points",
    click_to_add         = "Cliquez pour ajouter des points",
    points_saved         = "Données sauvegardées :",
    no_points            = "Aucun point à sauvegarder",
    reload_model         = "Recharger modèle (training.rds)",
    pred_summary         = "Résumé des prédictions",
    admin_pwd            = "Mot de passe admin",
    train_model          = "Ré-entraîner modèle",
    list_files           = "Lister fichiers inputs",
    clear_files          = "Effacer tous les fichiers",
    lang_choice          = "Langue",
    model_trained        = "Modèle ré-entraîné et sauvegardé.",
    no_files             = "Aucun fichier d'entrée trouvé.",
    all_deleted          = "Tous les fichiers supprimés.",
    model_loaded         = "Modèle chargé :",
    model_missing        = "Pas de modèle trouvé",
    no_model             = "Aucun modèle chargé",
    summary_title        = "Points collectés et moyenne (proxy de prédiction)",
    model_select_label   = "Choisir un modèle :",
    no_models            = "Aucun fichier .rds disponible.",
    delete_model         = "Supprimer le modèle sélectionné",
    delete_confirm_title = "Confirmer suppression",
    delete_confirm_text  = "Voulez-vous vraiment supprimer ce modèle ?",
    confirm_yes          = "Oui, supprimer",
    confirm_no           = "Annuler",
    model_deleted        = "Modèle supprimé :",
    model_delete_missing = "Le modèle n'existe plus."
  ),
  en = list(
    app_title            = "Multi-task GP demo",
    collect_tab          = "Data collection",
    prediction_tab       = "Prediction",
    admin_tab            = "Admin",
    user_id              = "User ID:",
    save_points          = "Save points",
    click_to_add         = "Click to add points",
    points_saved         = "Data saved:",
    no_points            = "No points to save",
    reload_model         = "Reload model (training.rds)",
    pred_summary         = "Prediction summary",
    admin_pwd            = "Admin password",
    train_model          = "Retrain model",
    list_files           = "List input files",
    clear_files          = "Delete all files",
    lang_choice          = "Language",
    model_trained        = "Model retrained and saved.",
    no_files             = "No input files found.",
    all_deleted          = "All files deleted.",
    model_loaded         = "Model loaded :",
    model_missing        = "No model found",
    no_model             = "No model loaded",
    summary_title        = "Collected points and mean (prediction proxy)",
    model_select_label   = "Choose a model:",
    no_models            = "No .rds models available.",
    delete_model         = "Delete selected model",
    delete_confirm_title = "Confirm deletion",
    delete_confirm_text  = "Do you really want to delete this model?",
    confirm_yes          = "Yes, delete",
    confirm_no           = "Cancel",
    model_deleted        = "Model deleted :",
    model_delete_missing = "Model no longer exists."
  )
)

# ---- UI ----
ui <- navbarPage(
  title = textOutput("app_title"),

  # Onglet Collecte
  tabPanel(title = textOutput("collect_tab"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("user_id_ui"),
        uiOutput("save_points_ui")
      ),
      mainPanel(
        plotOutput("plot_collect", click = "plot_click"),
        verbatimTextOutput("points_display")
      )
    )
  ),

  # Onglet Prédiction
  tabPanel(title = textOutput("prediction_tab"),
    mainPanel(
      uiOutput("reload_model_ui"),
      verbatimTextOutput("pred_summary"),
      plotOutput("pred_plot")
    )
  ),

  # Onglet Admin
  tabPanel(title = textOutput("admin_tab"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("admin_pwd_ui"),
        uiOutput("train_model_ui"),
        uiOutput("list_files_ui"),
        uiOutput("clear_files_ui"),
        uiOutput("lang_choice_ui"),
        uiOutput("model_select_ui"),  # <- menu déroulant des .rds
        uiOutput("delete_model_ui")   # <- bouton supprimer modèle
      ),
      mainPanel(
        verbatimTextOutput("admin_log")
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  # ---- LANGUE ----
  current_lang <- reactiveVal("fr")
  t <- function(key) {
    texts[[current_lang()]][[key]]
  }
  observeEvent(input$lang, {
    # si on change la sélection de langue (via le select rendu en renderUI)
    current_lang(input$lang)
  })

  # ---- NAVBAR TEXTS ----
  output$app_title      <- renderText({ t("app_title") })
  output$collect_tab    <- renderText({ t("collect_tab") })
  output$prediction_tab <- renderText({ t("prediction_tab") })
  output$admin_tab      <- renderText({ t("admin_tab") })

  # ---- UI dynamique (labels traduits) ----
  output$user_id_ui <- renderUI({
    textInput("user", t("user_id"), value = "anon")
  })
  output$save_points_ui <- renderUI({
    actionButton("save_points", t("save_points"))
  })
  output$reload_model_ui <- renderUI({
    actionButton("reload_model", t("reload_model"))
  })
  output$admin_pwd_ui <- renderUI({
    passwordInput("admin_pwd", t("admin_pwd"))
  })
  output$train_model_ui <- renderUI({
    actionButton("train_model", t("train_model"))
  })
  output$list_files_ui <- renderUI({
    actionButton("list_files", t("list_files"))
  })
  output$clear_files_ui <- renderUI({
    actionButton("clear_files", t("clear_files"))
  })
  output$lang_choice_ui <- renderUI({
    selectInput("lang", t("lang_choice"), choices = c("fr", "en"),
                selected = current_lang())
  })

  # ---- MODELS LIST (reactive, mise à jour manuelle) ----
  models_list <- reactiveVal(list.files(model_dir, pattern = "\\.rds$", full.names = FALSE))

  # UI for model selection
  output$model_select_ui <- renderUI({
    files <- models_list()
    if (length(files) == 0) {
      tags$p(t("no_models"))
    } else {
      # si training.rds est présent, on le propose en valeur par défaut
      default <- if (basename(model_file) %in% files) basename(model_file) else files[1]
      selectInput("model_choice", t("model_select_label"), choices = files, selected = default)
    }
  })

  # UI for delete model button (only shown if there are models)
  output$delete_model_ui <- renderUI({
    files <- models_list()
    if (length(files) == 0) return(NULL)
    actionButton("delete_model", t("delete_model"))
  })

  # admin log (simple reactiveVal pour garder l'historique/affichage)
  admin_log_val <- reactiveVal("")
  output$admin_log <- renderPrint({
    admin_log_val()
  })

  # ---- COLLECTE ----
  rv <- reactiveValues(points = data.frame(x = numeric(), y = numeric()))
  observeEvent(input$plot_click, {
    rv$points <- rbind(rv$points, data.frame(x = input$plot_click$x, y = input$plot_click$y))
  })
  output$plot_collect <- renderPlot({
    ggplot(rv$points, aes(x, y)) +
      geom_point(color = "blue", size = 3) +
      xlim(0, 10) + ylim(0, 10) +
      theme_minimal() +
      labs(title = t("click_to_add"))
  })
  output$points_display <- renderPrint({ rv$points })

  observeEvent(input$save_points, {
    if (nrow(rv$points) > 0) {
      fname <- file.path(inputs_dir, paste0(input$user, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"))
      write.csv(rv$points, fname, row.names = FALSE)
      showNotification(paste(t("points_saved"), basename(fname)), type = "message")
      rv$points <- data.frame(x = numeric(), y = numeric()) # reset
    } else {
      showNotification(t("no_points"), type = "error")
    }
  })

  # ---- ADMIN: droits ----
  admin_ok <- reactive({
    # require un mot de passe non vide
    req(input$admin_pwd)
    input$admin_pwd == "monMotDePasse"   # ⚠️ change ici si nécessaire
  })

  # helper to refresh models_list from disk
  refresh_models <- function() {
    models_list(list.files(model_dir, pattern = "\\.rds$", full.names = FALSE))
  }

  # track currently loaded model filename
  current_model_name <- reactiveVal(NULL)

  # ---- TRAIN MODEL (simulé) ----
  observeEvent(input$train_model, {
    req(admin_ok())
    all_files <- list.files(inputs_dir, full.names = TRUE)
    if (length(all_files) == 0) {
      admin_log_val(t("no_files"))
      return()
    }
    data_all <- do.call(rbind, lapply(all_files, read.csv))
    model <- list(timestamp = Sys.time(),
                  data = data_all,
                  mean_x = mean(data_all$x),
                  mean_y = mean(data_all$y))
    # on sauvegarde : 1) un .rds horodaté, 2) on met à jour training.rds
    time_fname <- file.path(model_dir, paste0("model_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rds"))
    saveRDS(model, time_fname)
    saveRDS(model, model_file)
    # refresh
    refresh_models()
    # update select input if present (sélectionne training.rds par défaut)
    tryCatch({
      updateSelectInput(session, "model_choice", choices = models_list(),
                        selected = basename(model_file))
    }, error = function(e) {})
    admin_log_val(t("model_trained"))
  })

  # ---- LIST / CLEAR input files ----
  observeEvent(input$list_files, {
    req(admin_ok())
    files <- list.files(inputs_dir)
    if (length(files) == 0) admin_log_val(t("no_files")) else admin_log_val(paste(files, collapse = "\n"))
  })
  observeEvent(input$clear_files, {
    req(admin_ok())
    file.remove(list.files(inputs_dir, full.names = TRUE))
    admin_log_val(t("all_deleted"))
  })

  # ---- LOAD MODEL: bouton "Recharger modèle (training.rds)" ----
  model <- reactiveVal(NULL) # l'objet modèle chargé utilisé en Prédiction
  observeEvent(input$reload_model, {
    # recharge spécifiquement le training.rds canonique
    if (file.exists(model_file)) {
      model(readRDS(model_file))
      current_model_name(basename(model_file))
      showNotification(paste(t("model_loaded"), basename(model_file)), type = "message")
      admin_log_val(paste(t("model_loaded"), basename(model_file)))
    } else {
      showNotification(t("model_missing"), type = "error")
      admin_log_val(t("model_missing"))
    }
  })

  # ---- LOAD MODEL: changement via le selectInput (menu déroulant) ----
  observeEvent(input$model_choice, {
    req(input$model_choice)
    chosen_file <- file.path(model_dir, input$model_choice)
    if (file.exists(chosen_file)) {
      model(readRDS(chosen_file))
      current_model_name(basename(chosen_file))
      showNotification(paste(t("model_loaded"), input$model_choice), type = "message")
      admin_log_val(paste(t("model_loaded"), input$model_choice))
    } else {
      showNotification(t("model_delete_missing"), type = "error")
      admin_log_val(t("model_delete_missing"))
      # refresh listing si fichier disparu
      refresh_models()
    }
  }, ignoreNULL = TRUE)

  # ---- DELETE MODEL: confirmation modal + suppression ----
  observeEvent(input$delete_model, {
    req(admin_ok())
    files <- models_list()
    if (length(files) == 0) {
      admin_log_val(t("no_models"))
      return()
    }
    if (is.null(input$model_choice) || input$model_choice == "") {
      admin_log_val(t("no_models"))
      return()
    }
    # show confirmation modal
    showModal(modalDialog(
      title = t("delete_confirm_title"),
      t("delete_confirm_text"),
      footer = tagList(
        modalButton(t("confirm_no")),
        actionButton("confirm_delete", t("confirm_yes"), class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_delete, {
    req(admin_ok())
    removeModal()
    chosen <- isolate(input$model_choice)
    if (is.null(chosen) || chosen == "") {
      admin_log_val(t("no_models"))
      return()
    }
    chosen_path <- file.path(model_dir, chosen)
    if (!file.exists(chosen_path)) {
      admin_log_val(t("model_delete_missing"))
      showNotification(t("model_delete_missing"), type = "error")
      refresh_models()
      return()
    }
    ok <- file.remove(chosen_path)
    if (ok) {
      # si c'était le modèle actuellement chargé, on le désactive
      if (!is.null(current_model_name()) && current_model_name() == chosen) {
        model(NULL)
        current_model_name(NULL)
      }
      refresh_models()
      # mettre à jour le select si présent
      tryCatch({
        updateSelectInput(session, "model_choice", choices = models_list(),
                          selected = if (basename(model_file) %in% models_list()) basename(model_file) else if (length(models_list())>0) models_list()[1] else "")
      }, error = function(e) {})
      admin_log_val(paste(t("model_deleted"), chosen))
      showNotification(paste(t("model_deleted"), chosen), type = "message")
    } else {
      admin_log_val(paste("Erreur: impossible de supprimer", chosen))
      showNotification(paste("Error deleting", chosen), type = "error")
      refresh_models()
    }
  })

  # ---- PREDICTION (affichage simple / proxy) ----
  output$pred_summary <- renderPrint({
    m <- model()
    if (is.null(m)) t("no_model") else {
      list(timestamp = m$timestamp, n_points = nrow(m$data), means = c(x = m$mean_x, y = m$mean_y))
    }
  })
  output$pred_plot <- renderPlot({
    m <- model()
    if (is.null(m)) return(NULL)
    ggplot(m$data, aes(x, y)) +
      geom_point(color = "red", size = 2) +
      geom_point(aes(x = m$mean_x, y = m$mean_y),
                 color = "darkgreen", size = 5, shape = 4, stroke = 2) +
      theme_minimal() +
      labs(title = t("summary_title"))
  })
}

shinyApp(ui, server)
