library(shiny)
library(MagmaClustR)
library(dplyr)
library(readr)
library(tibble)
library(ggplot2)

# =========================
# CONFIG & CHARGEMENT
# =========================

# Fichiers possibles (on cherche en priorité dans data/model/, puis à la racine)
df_candidates     <- c("data/model/df_train.csv", "df_train.csv")
model_candidates  <- c("data/model/model_se.rds", "model_se.rds")

df_path    <- df_candidates[file.exists(df_candidates)][1]
model_path <- model_candidates[file.exists(model_candidates)][1]

# Chargement "meilleur effort" (sans stop)
df_init <- tryCatch(
  if (!is.na(df_path)) read_csv(df_path, show_col_types = FALSE) else
    tibble(ID = character(), Input = numeric(), Output = numeric()),
  error = function(e) tibble(ID = character(), Input = numeric(), Output = numeric())
)

model_init <- tryCatch(
  if (!is.na(model_path)) readRDS(model_path) else NULL,
  error = function(e) NULL
)

# =========================
# UI
# =========================
ui <- navbarPage(
  "MagmApp", id = "nav",

  # --- Collecte ---
  tabPanel("Saisie de données",
    fluidRow(
      column(
        width = 4,
        textInput("nameid", "Entrer le nom de l'individu (puis valider) :"),
        actionButton("validationid", "Valider"),
        br(),
        # selectInput("button_samples", "Visualisation de l'incertitude",
        #   choices = c("Bande de crédibilité" = "FALSE", "Échantillons" = "TRUE"),
        #   selected = "TRUE"
        # ),
        actionButton("reset1", "Effacer"),
        actionButton("sendpoints", "Sauvegarder les données")  # n'effectue QUE l'ajout en mémoire
      ),
      column(
        width = 8,
        plotOutput("dataplot", click = "dataplot_click", width = "100%")
      )
    )
  ),

  # --- Prédiction ---
  tabPanel("Prédiction",
    fluidRow(
      column(
        width = 4,
        selectInput("choose_name", "Quel individu souhaitez-vous étudier ?",
                    choices = unique(df_init$ID),
                    selected = if (length(unique(df_init$ID)) > 0) unique(df_init$ID)[-1] else NULL),
        actionButton("reset2", "Effacer les points ajoutés"),
        # selectInput("button_samples2", "Visualisation de l'incertitude",
        #   choices = c("Bande de crédibilité" = "FALSE", "Échantillons" = "TRUE"),
        #   selected = "TRUE"
        # )
      ),
      column(
        width = 8,
        plotOutput("magmaplot", click = "magmaplot_click", width = "100%")
      )
    ),
    tableOutput("info")
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {

  # ---- États réactifs globaux (mémoire) ----
  df_train  <- reactiveVal(df_init)
  model_se  <- reactiveVal(model_init)

  # ---- Grilles / HP réactifs ----
  grid_inputs_collect <- seq(0, 200, 0.5)  # Collecte : échelle démo
  ylim_collect        <- c(0, 500)
  hp_collect          <- tibble(se_variance = 7, se_lengthscale = 5)

  grid_inputs_pred <- reactive({
    dt <- df_train()
    if (!is.null(dt) && nrow(dt) > 0) {
      seq(0, 200, 1) %>% union(unique(dt$Input))
    } else {
      seq(0, 200, 1)
    }
  })

  hyperpost <- reactive({
    m <- model_se()
    if (is.null(m)) return(NULL)
    tryCatch(
      hyperposterior_clust(trained_model = m, grid_inputs = grid_inputs_pred()),
      error = function(e) NULL
    )
  })

  # =========================
  # COLLECTE
  # =========================
  name_id <- reactiveVal(NULL)
  observeEvent(input$validationid, {
    nm <- input$nameid
    if (!is.null(nm) && nzchar(nm)) {
      name_id(nm)
      showNotification(sprintf("Identifiant '%s' validé. Cliquez sur le graphique.", nm), type = "message")
    } else {
      showNotification("Veuillez saisir un identifiant non vide.", type = "error")
    }
  })

  df_click <- reactiveVal(NULL)  # points de collecte (échelle 0..200 / 0..500)

  observeEvent(input$dataplot_click, {
    nm <- name_id(); if (is.null(nm) || !nzchar(nm)) return(NULL)
    cx <- input$dataplot_click$x; cy <- input$dataplot_click$y
    if (is.na(cx) || is.na(cy)) return(NULL)
    newrow <- data.frame(ID = nm, Input = round(cx, 1), Output = round(cy, 2))
    old <- df_click()
    df_click(if (is.null(old)) newrow else rbind(newrow, old))
  })

  observeEvent(input$reset1, { df_click(NULL) })

  # GP live (collecte) : sur les points bruts
  prior <- reactive({
    tryCatch(
      pred_gp(
        data         = df_click(),
        kern         = "SE",
        mean         = 200,
        grid_inputs  = grid_inputs_collect,
        hp           = hp_collect,
        get_full_cov = TRUE,
        plot         = FALSE
      ),
      error = function(e) NULL
    )
  })

  output$dataplot <- renderPlot({
    pr <- prior()
    d  <- df_click()

    if (is.null(d) || nrow(d) == 0 || is.null(pr)) {
      ggplot() +
        theme_minimal() +
        labs(title = "Cliquez pour ajouter des points",
             x = "Nourriture ingérée (g)", y = "Lait produit (mL)") +
        scale_x_continuous(breaks = seq(0, 200, 25), limits = c(0, 200)) +
        scale_y_continuous(breaks = seq(0, 500, 25), limits = ylim_collect)
    } else {
      plot_gp(pred_gp = pr, data = d, samples = TRUE) +
        theme_minimal() + 
        xlab("Nourriture ingérée (g)") + ylab("Lait produit (mL)") +
        scale_x_continuous(breaks = seq(0, 200, 25), limits = c(0, 200)) +
        scale_y_continuous(breaks = seq(0, 500, 25), limits = ylim_collect)
    }
  })

  # Sauvegarder : ajoute simplement (en mémoire) à df_train() — aucune bascule d’onglet, aucune modif de sélection
  observeEvent(input$sendpoints, {
    pts <- df_click()
    nm  <- name_id()
    if (is.null(nm) || is.null(pts) || nrow(pts) == 0) {
      showNotification("Rien à sauvegarder.", type = "error"); return(NULL)
    }

    df_new <- bind_rows(df_train(), pts) %>% distinct(ID, Input, .keep_all = TRUE)
    df_train(df_new)

    # Maintenir la sélection actuelle dans le menu si possible
    current_sel <- isolate(input$choose_name)
    ids <- unique(df_new$ID)
    keep_sel <- if (!is.null(current_sel) && current_sel %in% ids) current_sel else if (length(ids) > 0) ids[1] else NULL
    updateSelectInput(session, "choose_name", choices = ids, selected = keep_sel)

    # reset collecte
    df_click(NULL)

    showNotification(sprintf("Individu '%s' ajouté aux données de prédiction (en mémoire).", nm), type = "message")
  })

  # =========================
  # PRÉDICTION
  # =========================

  # Mise à jour du menu quand df_train change, en conservant si possible la sélection
  observe({
    ids <- unique(df_train()$ID)
    old <- isolate(input$choose_name)
    sel <- if (!is.null(old) && old %in% ids) old else if (length(ids) > 0) ids[1] else NULL
    updateSelectInput(session, "choose_name", choices = ids, selected = sel)
  })

  df_pred_name  <- reactiveVal(NULL)  # données de l’ID choisi
  df_click_pred <- reactiveVal(NULL)  # points ajoutés à droite (s’ajoutent aux données)

  observeEvent(input$choose_name, {
    nm <- input$choose_name
    if (!is.null(nm) && nzchar(nm) && nrow(df_train()) > 0) {
      df_pred_name(
        df_train() %>%
          filter(ID == nm) %>%
          select(any_of(c("ID","Input","Output")))   # ignore Cluster/Proba si présents
      )
    } else {
      df_pred_name(NULL)
    }
    df_click_pred(NULL)
  }, ignoreInit = FALSE)

  # Clics en prédiction : ajout de points qui entreront dans 'data' de pred_magmaclust()
  observeEvent(input$magmaplot_click, {
    nm <- input$choose_name
    if (is.null(nm) || !nzchar(nm)) return(NULL)
    cx <- input$magmaplot_click$x; cy <- input$magmaplot_click$y
    if (is.na(cx) || is.na(cy)) return(NULL)

    newrow <- data.frame(
      ID     = nm,
      Input  = round(cx, 0),
      Output = round(cy, 0)
    )
    old <- df_click_pred()
    df_click_pred(if (is.null(old)) newrow else rbind(newrow, old))
  })

  observeEvent(input$reset2, { df_click_pred(NULL) })

  df_pred <- reactive({
    # 🔑 combine toujours l’ID sélectionné + points ajoutés par clic
    bind_rows(df_pred_name(), df_click_pred())
  })

  # Fallback pour éviter un écran vide en cas d'erreur transitoire
  last_pred <- reactiveVal(NULL)

  pred <- reactive({
    d  <- df_pred()
    m  <- model_se()
    hp <- hyperpost()

    if (is.null(m) || is.null(d) || nrow(d) == 0) return(NULL)
    if (!all(c("ID","Input","Output") %in% names(d))) return(NULL)

    d <- d %>% filter(is.finite(Input), is.finite(Output))

    out <- tryCatch(
      pred_magmaclust(
        data          = d,                 # <-- données de l’ID + points cliqués
        trained_model = m,
        kern          = "SE",
        hyperpost     = hp,
        grid_inputs   = grid_inputs_pred(),
        get_full_cov  = TRUE,
        get_hyperpost = TRUE,
        plot          = FALSE
      ),
      error = function(e) NULL
    )

    if (!is.null(out)) last_pred(out)
    out
  })

  output$magmaplot <- renderPlot({
    pr  <- pred()
    if (is.null(pr)) pr <- isolate(last_pred())
    dft <- df_train()
    d   <- df_pred()

    if (is.null(pr) || is.null(dft) || is.null(d) || nrow(dft) == 0) return(NULL)

    prior_mean <- tryCatch(pr$hyperpost$mean, error = function(e) NULL)

    # 👉 pas d’overlay : 'data' contient déjà les points cliqués, donc plot_magmaclust les affiche
    tryCatch(
      plot_magmaclust(
        pred_clust = pr,
        data       = d,      # inclut les clics
        data_train = dft,
        col_clust  = TRUE,
        prior_mean = prior_mean,
        samples    = TRUE #input$button_samples2
      ) +
      xlab("Nourriture ingérée (g)") + ylab("Lait produit (mL)") +
      scale_color_manual(name = "Couleur vache", 
                         labels = c(
                          "K1" = "Violette",
                          "K2" = "Bleue",
                          "K3" = "Jaune"),
                         values = c(
                           "#ff00eeff",
                           "#00f1fdff", 
                           "#cdc300ff")),
      error = function(e) NULL
    )
  })

  output$info <- renderTable({
    pr  <- pred()
    tibble::tibble(
      "Couleur vache" = c('Violette', 'Bleue', 'Jaune'),
      "Probabilité d'appartenance (%)" = c(
        if (!is.null(pr)) round(pr$mixture$K1 * 100, 2) else NA,
        if (!is.null(pr)) round(pr$mixture$K2 * 100, 2) else NA,
        if (!is.null(pr)) round(pr$mixture$K3 * 100, 2) else NA
      )
    ) 
  })
}

shinyApp(ui, server)
