# heatmaps_differences_and_full.R
# Creates:
#   heatmap_main_differences.jpeg / .pdf
#   heatmap_appendix_full.jpeg    / .pdf

suppressPackageStartupMessages({
  library(data.table)
  library(corrplot)
})

# --------- Load your pre-scaling engineered dataset ---------
path <- "Step2/FE_Step2_Historical_Try2.csv"   # <-- change if needed
dt <- fread(path)

# ============================================================
# 1) MAIN HEATMAP — 9 Difference features
# ============================================================

diff_vars <- c(
  "log_rank_diff", "rankpoints_diff",
  "ace_diff", "df_diff", "in1_pct_diff", "won1_pct_diff",
  "sv_win_pct_diff", "bp_saved_pct_diff", "bp_per_game_diff"
)

use_diff <- intersect(diff_vars, names(dt))
df_diff <- dt[, ..use_diff]
for (j in seq_along(df_diff)) set(df_diff, j = j, value = as.numeric(df_diff[[j]]))

nzv <- sapply(df_diff, function(x) sd(x, na.rm = TRUE))
df_diff <- df_diff[, names(nzv)[nzv > 0], with = FALSE]

corr_main <- cor(df_diff, use = "pairwise.complete.obs", method = "spearman")

# --- Save JPEG ---
jpeg("heatmap_main_differences.jpeg", width = 850, height = 850, quality = 95)
corrplot(corr_main, method="color", type="full", order="hclust",
         addrect=2, tl.cex=0.85, tl.col="black", cl.cex=0.85,
         title="Spearman Correlation Heatmap (9 Difference Features, Pre-Scaling)",
         mar=c(0,0,3,0))
dev.off()

# --- Save PDF ---
pdf("heatmap_main_differences.pdf", width = 8, height = 8)
corrplot(corr_main, method="color", type="full", order="hclust",
         addrect=3, tl.cex=0.85, tl.col="black", cl.cex=0.85,
         title="Spearman Correlation Heatmap (9 Difference Features, Pre-Scaling)",
         mar=c(0,0,3,0))
dev.off()

cat("Saved main heatmap: JPEG and PDF\n")

# ============================================================
# 2) APPENDIX HEATMAP — All numeric, exclude handedness dummies
# ============================================================

num_cols <- names(dt)[vapply(dt, is.numeric, logical(1))]
drop_common <- c("player1_id","player2_id","match_id","y")
hand_dummies <- grep("^p[12]_hand_", names(dt), value = TRUE)
keep_cols <- setdiff(num_cols, c(drop_common, hand_dummies))

df_all <- dt[, ..keep_cols]
nzv2 <- sapply(df_all, function(x) sd(x, na.rm = TRUE))
df_all <- df_all[, names(nzv2)[nzv2 > 0], with = FALSE]

corr_full <- cor(df_all, use = "pairwise.complete.obs", method = "spearman")

w <- if (ncol(df_all) > 40) 14 else 10
h <- w

# --- Save JPEG ---
jpeg("heatmap_appendix_full.jpeg", width = w*100, height = h*100, quality = 95)
corrplot(corr_full, method="color", type="full", order="hclust",
         tl.cex=if (ncol(df_all) > 40) 0.45 else 0.6,
         tl.col="black", cl.cex=0.7,
         title="Appendix: Spearman Correlation Heatmap (All Numeric Features, Pre-Scaling)",
         mar=c(0,0,3,0))
dev.off()

# --- Save PDF ---
pdf("heatmap_appendix_full.pdf", width = w, height = h)
corrplot(corr_full, method="color", type="full", order="hclust",
         tl.cex=if (ncol(df_all) > 40) 0.45 else 0.6,
         tl.col="black", cl.cex=0.7,
         title="Appendix: Spearman Correlation Heatmap (All Numeric Features, Pre-Scaling)",
         mar=c(0,0,3,0))
dev.off()

cat("Saved appendix heatmap: JPEG and PDF\n")
