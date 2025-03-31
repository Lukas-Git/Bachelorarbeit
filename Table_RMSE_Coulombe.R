forecastingperformance <- data.frame(titel=c("Albacore_Comps","Albacore_Ranks","Albacore_Comps","Albacore_Ranks","Albacore_Comps","Albacore_Ranks"),
                                     I1 = c(1.02,1.01,0.99,0.97,1.06,0.98),
                                     I2 = c(1.01,0.97,1.02,0.92,1.00,0.91),
                                     I3 = c(1.08,0.99,1.10,0.93,1.10,0.88),
                                     I4 = c(1.13,0.98,1.14,0.87,1.19,0.84),
                                     I5 = c(1.12,0.87,1.06,0.73,1.16,0.77),
                                     O1 = c(0.90,0.84,0.87,0.81,0.92,0.86),
                                     O2 = c(0.86,0.74,0.84,0.76,0.88,0.82),
                                     O3 = c(0.70,0.57,0.80,0.61,0.84,0.70),
                                     O4 = c(0.63,0.59,0.75,0.56,0.71,0.62),
                                     O5 = c(0.88,0.69,1.01,0.63,0.99,0.67))

colnames(forecastingperformance)[2:11] <- paste0("Var", rep(1:10))
colnames(forecastingperformance)[1] <- "h -->"

# Erstelle die GT-Tabelle
tbl <- gt(forecastingperformance) %>%
  # Spaltennamen f??r Anzeige anpassen
  cols_label(
    Var1 = "1", Var2 = "3", Var3 = "6", Var4 = "12", Var5 = "24",
    Var6 = "1", Var7 = "3", Var8 = "6", Var9 = "12", Var10 = "24"
  ) %>%
  # Gruppierung f??r Zeitperioden mit Tab Spanner
  tab_spanner(label = "2010m1-2019m12", columns = c(Var1, Var2, Var3, Var4, Var5)) %>%
  tab_spanner(label = "2020m1-2023m12", columns = c(Var6, Var7, Var8, Var9, Var10)) %>%
  tab_stubhead(label = "h -->") %>%
  # Zeilen-Gruppen erstellen
  tab_row_group(group = "Level 2(K=15)", rows = 1:2) %>%
  tab_row_group(group = "Level 3(K=50)", rows = 3:4) %>%
  tab_row_group(group = "Level 6(K=215)", rows = 5:6) %>%
  # Stil der Gruppenzeilen
  tab_style(
    style = list(
      cell_fill(color = "lightgray")
    ),
    locations = cells_row_groups()
  ) %>%
  # Trennlinien nach der ersten Spalte
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(1)),
    locations = cells_body(columns = c("h -->"))
  ) %>%
  # D??nne Trennlinie zwischen den Tab-Spannern
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_column_spanners()
  ) %>%
  # D??nne Linien zwischen Zeilen-Gruppen
  tab_options(
    row_group.border.top.color = "black",
    row_group.border.top.width = px(1),
    row_group.border.bottom.color = "black",
    row_group.border.bottom.width = px(1),
    table_body.border.top.color = "black",
    table_body.border.top.width = px(1)
  ) %>%
  # Linie zwischen Spalten??berschriften und Daten
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_column_labels()
  ) %>%
  # Tabelle ohne ??berfl??ssigen Randstil
  tab_options(
    table.border.top.width = px(0),
    table.border.bottom.width = px(0)
  )

# Tabelle anzeigen
tbl <- tbl %>%
  fmt_number(
    columns = everything(),   # Alle numerischen Spalten formatieren
    decimals = 2              # Auf 2 Nachkommastellen runden
  )
print(tbl)
gtsave(tbl, "D:/Daten/table_RMSE_Comparison_coulombe.pdf")