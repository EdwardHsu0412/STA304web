status_levels <- c("Domestic", "International")
satisfaction_levels <- c("Low (1\u20133)", "Moderate (4\u20137)", "High (8\u201310)")

time_labels <- c(
  total_hours = "Total social media use",
  video_hours = "Short-form video",
  text_hours = "Text-based platforms",
  messaging_hours = "Calls and messaging",
  study_hours = "Study time",
  offline_minutes = "Time off social media before bed"
)
time_units <- setNames(
  c(rep("hours per day", 5), "minutes"),
  names(time_labels)
)
predictors <- c("study_hours", "video_hours", "text_hours", "messaging_hours", "offline_minutes")

load_responses <- function(path) {
  responses <- read.csv(path, stringsAsFactors = FALSE)
  responses$status <- factor(responses$status, levels = status_levels)
  responses$total_hours <- with(responses, video_hours + text_hours + messaging_hours)
  responses$satisfaction_band <- cut(
    responses$satisfaction,
    breaks = c(0, 3, 7, 10),
    labels = satisfaction_levels
  )
  responses
}

correlation_result <- function(data, measure) {
  pairs <- data[c(measure, "gpa")]
  pairs <- pairs[complete.cases(pairs), , drop = FALSE]
  if (nrow(pairs) < 4 || any(vapply(pairs, function(x) length(unique(x)) < 2, logical(1)))) {
    return(NULL)
  }
  cor.test(pairs[[measure]], pairs$gpa, method = "pearson")
}

fit_regression <- function(data) {
  data <- droplevels(data)
  terms <- predictors
  if (nlevels(data$status) > 1) {
    terms <- c(terms, "status")
  }
  if (nrow(data) <= length(terms) + 1L) {
    return(NULL)
  }
  fit <- lm(reformulate(terms, response = "gpa"), data = data)
  if (df.residual(fit) < 1 || anyNA(coef(fit))) {
    return(NULL)
  }
  fit
}

regression_test <- function(fit) {
  f <- summary(fit)$fstatistic
  unname(pf(f[1], f[2], f[3], lower.tail = FALSE))
}

satisfaction_summary <- function(data) {
  counts <- as.data.frame(
    table(satisfaction = data$satisfaction_band, status = droplevels(data$status)),
    responseName = "count"
  )
  counts$percent <- 100 * counts$count / ave(counts$count, counts$status, FUN = sum)
  counts
}

dec <- function(x, digits = 2) {
  sub("^-", "\u2212", formatC(x, format = "f", digits = digits))
}

p_label <- function(x) {
  ifelse(x < 0.001, "<0.001", dec(x, 3))
}

interval_label <- function(low, high, digits = 2) {
  paste0("[", dec(low, digits), ", ", dec(high, digits), "]")
}
