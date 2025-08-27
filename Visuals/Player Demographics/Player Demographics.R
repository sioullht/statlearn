# 02_demographics.R
library(tidyverse)
library(data.table)

# ---- Load ----
path <- "Step1/DP_Step1_Reordered.csv"
dt <- fread(path)   # ✅ load properly as data.table

# ---- Age & Height: long format across players ----
ages_long <- rbindlist(list(
  dt[, .(who = "Player1", age = as.numeric(player1_age))],
  dt[, .(who = "Player2", age = as.numeric(player2_age))]
)) |> filter(!is.na(age))

heights_long <- rbindlist(list(
  dt[, .(who = "Player1", ht = as.numeric(player1_ht))],
  dt[, .(who = "Player2", ht = as.numeric(player2_ht))]
)) |> filter(!is.na(ht))

# ---- Density: Age ----
p_age <- ggplot(ages_long, aes(x = age, fill = who)) +
  geom_density(alpha = 0.35) +
  labs(
    title = "Age Distribution by Player Role",
    x = "Age (years)", y = "Density", fill = ""
  ) +
  theme_minimal()
ggsave("density_age.jpeg", p_age, width = 7, height = 4.5, dpi = 300)

# ---- Density: Height ----
p_ht <- ggplot(heights_long, aes(x = ht, fill = who)) +
  geom_density(alpha = 0.35) +
  labs(
    title = "Height Distribution by Player Role",
    x = "Height (cm)", y = "Density", fill = ""
  ) +
  theme_minimal()
ggsave("density_height.jpeg", p_ht, width = 7, height = 4.5, dpi = 300)

# ---- Handedness overall ----
hand_p1 <- melt(
  dt[, .(p1_hand_R, p1_hand_L, p1_hand_U)],
  measure.vars = c("p1_hand_R","p1_hand_L","p1_hand_U"),
  variable.name = "flag", value.name = "val"
)[val == 1][, .N, by = flag]

hand_p2 <- melt(
  dt[, .(p2_hand_R, p2_hand_L, p2_hand_U)],
  measure.vars = c("p2_hand_R","p2_hand_L","p2_hand_U"),
  variable.name = "flag", value.name = "val"
)[val == 1][, .N, by = flag]

hand_all <- rbind(hand_p1[,.(hand = sub("^p1_hand_", "", flag), N)],
                  hand_p2[,.(hand = sub("^p2_hand_", "", flag), N)]) |>
  group_by(hand) |>
  summarise(N = sum(N), .groups = "drop") |>
  mutate(hand = recode(hand, "R"="Right", "L"="Left", "U"="Unknown"),
         share = N / sum(N))

p_hand <- ggplot(hand_all, aes(x = hand, y = N)) +
  geom_col() +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
            vjust = -0.3) +
  labs(
    title = "Handedness Frequency (All Player Appearances)",
    x = "Handedness", y = "Count"
  ) +
  theme_minimal()
ggsave("bar_handedness.jpeg", p_hand, width = 6.5, height = 4.5, dpi = 300)

# ---- Matchup differences: Age & Height ----
diffs <- dt[, .(
  age_diff   = as.numeric(player1_age) - as.numeric(player2_age),
  height_diff= as.numeric(player1_ht)  - as.numeric(player2_ht)
)]

p_diffs <- ggplot(diffs |> pivot_longer(everything(),
                                        names_to = "metric",
                                        values_to = "value"),
                  aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ metric, scales = "free_x",
             labeller = as_labeller(c(age_diff="Age difference (P1 - P2)",
                                      height_diff="Height difference (P1 - P2)"))) +
  labs(x = "Difference", y = "Count") +
  theme_minimal()
ggsave("hist_age_height_diff.jpeg", p_diffs, width = 7.5, height = 4.5, dpi = 300)

