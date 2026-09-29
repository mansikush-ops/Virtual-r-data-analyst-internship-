# ============================================================
# Virtual R Internship - Data Cleaning, Preprocessing & EDA
# Dataset: Palmer Penguins (344 rows x 8 columns)
# ============================================================

# 1. Install/load packages
# install.packages(c("palmerpenguins", "dplyr", "ggplot2"))
library(palmerpenguins)
library(dplyr)
library(ggplot2)

# 2. Dataset selection / download
# The CSV is publicly available in the official palmerpenguins repository.
download.file(
  "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/main/inst/extdata/penguins.csv",
  "penguins.csv",
  mode = "wb"
)

penguins_raw <- read.csv("penguins.csv", stringsAsFactors = FALSE)

# Alternatively, the package contains the same curated dataset:
# data("penguins", package = "palmerpenguins")

# 3. Initial inspection
dim(penguins_raw)
str(penguins_raw)
summary(penguins_raw)

# Missing-value count
colSums(is.na(penguins_raw))

# 4. Missing-value handling
# Numeric variables: impute with the median within species.
numeric_vars <- c(
  "bill_length_mm", "bill_depth_mm",
  "flipper_length_mm", "body_mass_g"
)

penguins_clean <- penguins_raw %>%
  mutate(
    species = factor(species),
    island = factor(island),
    sex = factor(sex)
  ) %>%
  group_by(species) %>%
  mutate(
    across(
      all_of(numeric_vars),
      ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)
    )
  ) %>%
  ungroup()

# Categorical variable: impute missing sex with the modal value
# within each species.
mode_value <- function(x) {
  ux <- na.omit(x)
  tab <- table(ux)
  names(tab)[which.max(tab)]
}

penguins_clean <- penguins_clean %>%
  group_by(species) %>%
  mutate(
    sex = ifelse(is.na(sex), mode_value(sex), as.character(sex))
  ) %>%
  ungroup() %>%
  mutate(
    species = factor(species),
    island = factor(island),
    sex = factor(sex)
  )

# Verify that missing values were handled
colSums(is.na(penguins_clean))

# 5. Outlier detection using the 1.5 x IQR rule
iqr_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  sum(x < lower | x > upper, na.rm = TRUE)
}

sapply(penguins_clean[numeric_vars], iqr_outliers)

# 6. Normalization (min-max scaling to [0, 1])
minmax <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

penguins_scaled <- penguins_clean %>%
  mutate(across(all_of(numeric_vars), minmax, .names = "{.col}_scaled"))

# 7. One-hot encoding of categorical variables
encoded <- model.matrix(
  ~ species + island + sex - 1,
  data = penguins_clean
)

encoded <- as.data.frame(encoded)

# 8. Exploratory analysis
# Species counts
penguins_clean %>% count(species)

# Descriptive statistics
penguins_clean %>%
  group_by(species) %>%
  summarise(
    across(
      all_of(c("bill_length_mm", "bill_depth_mm",
               "flipper_length_mm", "body_mass_g")),
      list(mean = ~ mean(.x), median = ~ median(.x), sd = ~ sd(.x))
    )
  )

# Correlations
cor(
  penguins_clean[
    c("bill_length_mm", "bill_depth_mm",
      "flipper_length_mm", "body_mass_g")
  ],
  use = "complete.obs"
)

# 9. Visualizations
ggplot(penguins_clean,
       aes(x = flipper_length_mm, y = body_mass_g,
           colour = species)) +
  geom_point(alpha = 0.75) +
  labs(
    title = "Flipper Length vs Body Mass",
    x = "Flipper length (mm)",
    y = "Body mass (g)"
  ) +
  theme_minimal()

ggplot(penguins_clean, aes(x = species, y = body_mass_g)) +
  geom_boxplot() +
  labs(
    title = "Body Mass Distribution by Species",
    x = "Species",
    y = "Body mass (g)"
  ) +
  theme_minimal()

# 10. Export cleaned data
write.csv(penguins_clean, "palmer_penguins_cleaned.csv", row.names = FALSE)
write.csv(penguins_scaled, "palmer_penguins_processed.csv", row.names = FALSE)
write.csv(encoded, "palmer_penguins_encoded.csv", row.names = FALSE)
