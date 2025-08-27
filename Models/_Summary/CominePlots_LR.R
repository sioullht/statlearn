# combine_roc_side_by_side.R
# Zwei PDFs (jeweils Seite 1) gleich hoch skalieren und nebeneinander als ein neues PDF speichern.

# ---- Pfade anpassen ----
in_left  <- "/Users/louisleicht/Statistical_Learning/Models/LR/plots/ROC_Train_vs_Test.pdf"
in_right <- "/Users/louisleicht/Statistical_Learning/Models/LR_Tuned_Final/Final_ROC_Curves.pdf"
out_pdf  <- "/Users/louisleicht/Statistical_Learning/Models/Summary_ROC_SideBySide.pdf"

# ---- CRAN-Repo setzen (verhindert Spiegel-Abfrage) ----
options(repos = c(CRAN = "https://cloud.r-project.org"))

# ---- Pakete installieren & laden ----
pkgs <- c("magick", "pdftools")  # pdftools nötig, wenn Ghostscript im magick-Build deaktiviert ist
need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(need)) install.packages(need)
suppressPackageStartupMessages({
  library(magick)
  library(pdftools)
})

# ---- Parameter ----
page_index   <- 1L   # 1 = erste Seite
dpi          <- 300  # Render-Auflösung
trim_margins <- FALSE # bei TRUE werden weiße Ränder automatisch beschnitten

# ---- Checks ----
stopifnot(file.exists(in_left), file.exists(in_right))

# ---- PDFs einlesen (als Rasterbilder) ----
img1 <- image_read_pdf(in_left,  density = dpi)[page_index]
img2 <- image_read_pdf(in_right, density = dpi)[page_index]

if (trim_margins) {
  img1 <- image_trim(img1)
  img2 <- image_trim(img2)
}

# ---- Auf gleiche Höhe skalieren ----
info1 <- image_info(img1)
info2 <- image_info(img2)
h_target <- max(info1$height, info2$height)

img1 <- image_resize(img1, paste0("x", h_target))  # skaliert nach Höhe
img2 <- image_resize(img2, paste0("x", h_target))

# ---- Nebeneinander anordnen & als PDF speichern ----
combined <- image_append(c(img1, img2), stack = FALSE)
image_write(combined, path = out_pdf, format = "pdf")

message("Fertig: ", normalizePath(out_pdf))