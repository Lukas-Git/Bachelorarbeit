library(data.table)
library(ggplot2)
library(moments)  # Für Skewness und Kurtosis

# CSV-Datei als Data Table öffnen
df <- fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv")

# Überprüfung des Datentyps jeder Spalte
str(df)

# Nur numerische Spalten auswählen
numeric_df <- df[, lapply(.SD, as.numeric), .SDcols = sapply(df, is.numeric)]

# Nicht-numerische Werte identifizieren und bereinigen
non_numeric_columns <- names(df)[sapply(df, function(x) any(is.na(as.numeric(as.character(x)))))]
print(non_numeric_columns)

df <- df[, !names(df) %in% non_numeric_columns, with = FALSE]

# Mittelwert des gesamten Data Tables (Durchschnitt aller Werte)
overall_mean <- mean(unlist(numeric_df), na.rm = TRUE)

overall_median <- median(unlist(numeric_df), na.rm = TRUE)

overall_var <- var(unlist(numeric_df), na.rm = TRUE)

var_coeff <- sqrt(overall_var)/overall_mean

# Mittelwert jeder Spalte
column_means <- colMeans(numeric_df, na.rm = TRUE)

# Modus-Funktion definieren
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Modus für jede Spalte berechnen
column_modes <- sapply(numeric_df, get_mode)

# Minimum und Maximum jeder Spalte berechnen
min_max_table <- data.table(
  Column = names(numeric_df),
  Min = sapply(numeric_df, min, na.rm = TRUE),
  Max = sapply(numeric_df, max, na.rm = TRUE)
)

# Anzahl der gesamten Datenpunkte
num_data_points <- sum(!is.na(unlist(numeric_df)))

# Histogramm erstellen
histogram_plot <- ggplot(melt(numeric_df), aes(value)) +
  geom_histogram(binwidth = 0.01, fill = "blue", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.5) +
  scale_x_continuous(limits = c(-2.5, 3), breaks = seq(-2.5, 3, by = 0.25)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Histogramm 3-Month over 3-Month Inflation level 3", x = "Inflation", y = "Absolute Amount")

# Skewness und Kurtosis berechnen
skewness_values <- sapply(numeric_df, skewness, na.rm = TRUE)
kurtosis_values <- sapply(numeric_df, kurtosis, na.rm = TRUE)

# Ergebnisse ausgeben
list(
  Overall_Mean = overall_mean,
  Column_Means = column_means,
  Column_Modes = column_modes,
  Min_Max_Table = min_max_table,
  Num_Data_Points = num_data_points,
  Skewness = skewness_values,
  Kurtosis = kurtosis_values,
  Histogram = histogram_plot
)

ggsave("D:/Daten/histogrambench.csv.pdf", plot = histogram_plot, width = 8, height = 6)