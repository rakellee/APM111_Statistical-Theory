# Given Data
scores <- c(
  88,45,53,86,33,86,85,30,89,53,41,96,56,38,62,
  71,51,86,68,29,28,47,33,37,25,36,33,94,73,46,
  42,34,79,72,88,99,82,62,57,42,28,55,67,62,60,
  96,61,57,75,93,34,75,53,32,28,73,51,69,91,35
)

n <- length(scores)
mean_score <- mean(scores)
sd_score <- sd(scores)

# Mode
get_mode_spss <- function(x) {
  freq <- table(x)
  modes <- as.numeric(names(freq[freq == max(freq)]))
  min(modes)
}
mode_score <- get_mode_spss(scores)

# Skewness (SPSS style)
skewness <- (n / ((n - 1) * (n - 2))) *
  sum(((scores - mean_score) / sd_score)^3)

se_skewness <- sqrt(
  (6 * n * (n - 1)) /
  ((n - 2) * (n + 1) * (n + 3))
)

# Kurtosis (excess kurtosis, SPSS-style)
kurtosis <- ((n * (n + 1)) /
  ((n - 1) * (n - 2) * (n - 3))) *
  sum(((scores - mean_score) / sd_score)^4) -
  (3 * (n - 1)^2) / ((n - 2) * (n - 3))

se_kurtosis <- sqrt(
  (24 * n * (n - 1)^2) /
  ((n - 3) * (n - 2) * (n + 3) * (n + 5))
)

# Percentiles
Q1  <- quantile(scores, 0.25, type = 7)
Q2  <- quantile(scores, 0.50, type = 7)
Q3  <- quantile(scores, 0.75, type = 7)
D9  <- quantile(scores, 0.90, type = 7)
P95 <- quantile(scores, 0.95, type = 7)

# Descriptive table
descriptive_table <- data.frame(
  Measure = c(
    "Valid",
    "Mode                         ᵃ",
    "Median",
    "Mean",
    "Std. Deviation",
    "Variance",
    "Skewness",
    "Std. Error of Skewness",
    "Kurtosis",
    "Std. Error of Kurtosis",
    "Minimum",
    "Maximum",
    "25th percentile(Q1)",
    "50th percentile(Q2)",
    "75th percentile(Q3)",
    "90th percentile(D9)",
    "95th percentile(P95)"
  ),
  Score = c(
    n,
    mode_score,
    Q2,
    mean_score,
    sd_score,
    var(scores),
    skewness,
    se_skewness,
    kurtosis,
    se_kurtosis,
    min(scores),
    max(scores),
    Q1, Q2, Q3, D9, P95
  ),
  stringsAsFactors = FALSE
)

# Format (SPSS style)
descriptive_table$Score <- c(
  formatC(descriptive_table$Score[1], format = "f", digits = 0),
  formatC(descriptive_table$Score[-1], format = "f", digits = 3)
)

cat("Descriptive Statistics\n")
cat(strrep("-", 44), "\n")

# Header
cat(sprintf(" %-30s %9s\n", "Measure", "Score"))
cat(strrep("-", 44), "\n")

# Rows (LEFT-ALIGNED Measure)
for (i in 1:nrow(descriptive_table)) {
  cat(sprintf(" %-30s %10s\n",
    descriptive_table$Measure[i],
    descriptive_table$Score[i]
  ))
}

cat(strrep("-", 44), "\n")
cat("ᵃ More than one mode exists, only the first is reported\n")
