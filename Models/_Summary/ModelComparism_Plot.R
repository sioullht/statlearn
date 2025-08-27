library(readr)
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

# ------------------------------
# Hilfsfunktion Namen
# ------------------------------
pretty_name <- function(x){
  dplyr::recode(x,
    "XGB"               = "eXtreme Gradient Boosting",
    "XGB_Tuned_"        = "eXtreme Gradient Boosting (Tuned)",
    "RF"                = "Random Forest",
    "RF_Tuned"          = "Random Forest (Tuned)",
    "NN"                = "Neuronales Netz",
    "NN_Tuned"          = "Neuronales Netz (Tuned)",
    "LR"                = "Logistische Regression",
    "RFE_Bootstrapping" = "Logistische Regression (RFE, Tuned)",
    .default = x
  )
}

# ------------------------------
# Tabellenfunktion im LaTeX-Stil
# ------------------------------
make_pdf_table_latex_style <- function(csv_path, out_pdf){
  df <- read_csv(csv_path, show_col_types = FALSE)

  tab <- df %>%
    filter(tolower(Split) == "test") %>%
    transmute(
      Modell       = pretty_name(Model),
      Accuracy     = as.numeric(Accuracy),
      F1_Score     = as.numeric(F1_Score),
      AUC          = as.numeric(AUC),
      `Log Loss`   = as.numeric(Log_Loss),
      `Brier Score`= as.numeric(Brier_Score)
    ) %>%
    arrange(desc(AUC), `Log Loss`, `Brier Score`) %>%
    mutate(
      Accuracy      = sprintf("%.3f", Accuracy),
      F1_Score      = sprintf("%.3f", F1_Score),
      AUC           = sprintf("%.3f", AUC),
      `Log Loss`    = sprintf("%.3f", `Log Loss`),
      `Brier Score` = sprintf("%.3f", `Brier Score`)
    )

  # Tabellen-Theme
  tt <- ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 10),
      hjust = c(0, rep(1, ncol(tab)-1)),
      x     = c(0.02, rep(0.98, ncol(tab)-1))
    ),
    colhead = list(
      fg_params = list(fontsize = 11, fontface = "bold"),
      hjust = c(0, rep(1, ncol(tab)-1)),
      x     = c(0.02, rep(0.98, ncol(tab)-1))
    )
  )

  tg <- tableGrob(tab, rows = NULL, theme = tt)

  # Linien hinzufügen wie in LaTeX
  ncols <- ncol(tg)
  add_line <- function(gt, row){
    line <- segmentsGrob(x0=unit(0,"npc"), x1=unit(1,"npc"),
                         y0=unit(1,"npc"), y1=unit(1,"npc"),
                         gp=gpar(lwd=1.2))
    gtable_add_grob(gt, line, t=row, l=1, r=ncols, z=Inf, name="line")
  }

  # Linie über Header
  tg <- add_line(tg, 1)
  # Linie unter Header
  tg <- add_line(tg, 2)
  # Linie unter der Tabelle (eine Zeile tiefer als letzte Zeile)
  tg <- gtable_add_rows(tg, unit(1,"lines"))  # extra Zeile anhängen
  tg <- add_line(tg, nrow(tg))

  # PDF speichern
  pdf(out_pdf, width=7.5, height=3)
  grid.newpage(); grid.draw(tg)
  dev.off()
}

# ------------------------------
# Aufrufe für beide Tabellen
# ------------------------------
make_pdf_table_latex_style(
  "/Users/louisleicht/Statistical_Learning/Models/_Summary/metrics_standard_models.csv",
  "/Users/louisleicht/Statistical_Learning/Models/_Summary/Table_xTest_Standard.pdf"
)

make_pdf_table_latex_style(
  "/Users/louisleicht/Statistical_Learning/Models/_Summary/metrics_tuned_models.csv",
  "/Users/louisleicht/Statistical_Learning/Models/_Summary/Table_Test_Tuned.pdf"
)