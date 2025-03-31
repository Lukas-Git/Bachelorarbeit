library(ggplot2)
library(data.table)

# Daten laden
coeff_3_12 <- fread("D:/Daten/rank_3_12coeff.csv", quote = "\"")

# Variablen definieren
coeff_names <- seq(1, 100, length.out = 50)  # Werte von 1 bis 100 in 50 Schritten
coeff_weights <- numeric(50)

# Daten umstrukturieren
for (i in 1:50) {
  j <- 51 - i
  coeff_weights[i] <- as.numeric(coeff_3_12[1, ..j])
}

# Gewichte normalisieren (in Prozent umwandeln)
coeff_weights <- (coeff_weights / sum(coeff_weights)) * 100
c <- data.frame(Kategorie = as.numeric(coeff_names), percent_Weights = coeff_weights)

c$show_point <- c$percent_Weights > 0.1  

# Plot erstellen
ggplot(c, aes(x = Kategorie, y = percent_Weights)) + 
  geom_area(fill = "#062F4F", alpha = 0.8) +  # Dunkleres Blau
  geom_line(color = "black", size = 0.5) +  # Dickere schwarze Linie
  geom_point(data = subset(c, show_point), aes(x = Kategorie, y = percent_Weights), 
             color = "black", size = 1.5) +  # Größere schwarze Punkte
  theme_bw() + 
  labs(x = "Albacore", y = "Weights (in %)") + 
  scale_x_continuous(breaks = seq(0, 100, by = 20), labels = paste0(seq(0, 100, by = 20), "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +  # Y-Achse beginnt exakt bei 0
  theme(
    text = element_text(size = 14),
    axis.title.x = element_text(face = "bold", size = 16),  # X-Achsen-Label fett
    axis.title.y = element_text(face = "bold", size = 16),  # Y-Achsen-Label fett
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

ggsave("D:/Daten/Coeff_lvl3_ranks.pdf", height = 5, width = 8)