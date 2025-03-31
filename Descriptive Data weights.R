library(data.table)
library(ggplot2)

# CSV-Dateien als Data Table einlesen
df <- fread("D:/Daten/AssDataWeightslvl3.csv")
header_df <- fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv")

# Nur die ersten 50 Spaltennamen ??bernehmen
setnames(df, names(df), names(header_df)[1:50])

# 18. Spaltennamen zu "Food" ??ndern
setnames(df, names(df)[18], "Food")

# Sicherstellen, dass alle Spalten numerisch sind
numeric_df <- df[, lapply(.SD, as.numeric), .SDcols = sapply(df, is.numeric)]

# Berechnungen f??r den gesamten Datensatz
overall_mean <- mean(unlist(numeric_df), na.rm = TRUE)
overall_median <- median(unlist(numeric_df), na.rm = TRUE)
overall_variance <- var(unlist(numeric_df), na.rm = TRUE)
overall_coeff_var <- sd(unlist(numeric_df), na.rm = TRUE) / overall_mean
overall_min <- min(unlist(numeric_df), na.rm = TRUE)
overall_max <- max(unlist(numeric_df), na.rm = TRUE)

# Berechnungen f??r jede Spalte
column_stats <- data.table(
  Column = names(numeric_df),
  Mean = sapply(numeric_df, mean, na.rm = TRUE),
  Median = sapply(numeric_df, median, na.rm = TRUE),
  Variance = sapply(numeric_df, var, na.rm = TRUE),
  Coeff_Var = sapply(numeric_df, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)),
  Min = sapply(numeric_df, min, na.rm = TRUE),
  Max = sapply(numeric_df, max, na.rm = TRUE)
)

# Top 3 Spalten mit h??chster Varianz ausw??hlen
top_var_columns <- column_stats[order(-Variance)][1:3, Column]

top_var_data <- df[, ..top_var_columns]
top_var_data$Month <- seq(as.Date("1959-04-01"), as.Date("2024-11-01"), by = "months")

top_var_melted <- melt(top_var_data, id.vars = "Month")

# Mittelwerte der Top-3-Variablen berechnen
column_means <- column_stats[Column %in% top_var_columns, .(Column, Mean)]

# Diagramm erstellen
plot <- ggplot(top_var_melted, aes(x = Month, y = value, color = variable)) +
  geom_line() +
  geom_hline(data = column_means, aes(yintercept = Mean, color = Column), linetype = "dashed") +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
  labs(title = "Three Weights with highest variance", x = "year", y = "weights", color = "Spaltenname") +
  theme_minimal()

# Diagramm anzeigen
print(plot)

# Ergebnisse ausgeben
list(
  Overall_Stats = list(
    Mean = overall_mean,
    Median = overall_median,
    Variance = overall_variance,
    Coeff_Var = overall_coeff_var,
    Min = overall_min,
    Max = overall_max
  ),
  Column_Stats = column_stats,
  Top_Var_Plot = plot
)


ggsave("D:/Daten/weightgraph.csv.pdf", plot = plot, width = 12, height = 6)