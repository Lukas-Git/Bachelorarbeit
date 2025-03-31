library(here)
library(data.table)
.d <- `[`

weights <- fread("D:/Daten/Table_2_4_6_Level 2 Data long v2.csv",quote = "\"")

mvp <- weights |>
      .d(Item %in% "Motor_vehicles_and_parts") |>
      .d(, "share") 
mvp <- mean(mvp[[1]])      

fadhe <- weights |>
      .d(Item %in% "Furnishings_and_durable_household_equipment")|>
  .d(, "share")
fadhe <-    mean(fadhe[[1]])

rgav <- weights |>
      .d(Item %in% "Recreational_goods_and_vehicles")|>
  .d(, "share")
rgav <- mean(rgav[[1]])

odg <-weights |>
  .d(Item %in% "Other_durable_goods")|>
  .d(, "share")
odg <- mean(odg[[1]])

fbp <- weights |>
  .d(Item %in% "Food_and_beverages_purchased_for_off-premises_consumption")|>
  .d(, "share")
fbp <- mean(fbp[[1]])

caf <- weights |>
  .d(Item %in% "Clothing_and_footwear")|>
  .d(, "share")
caf <- mean(caf[[1]])

gaoeg <- weights |>
  .d(Item %in% "Gasoline_and_other_energy_goods")|>
  .d(, "share")
gaoeg <- mean(gaoeg[[1]])

ong <- weights |>
  .d(Item %in% "Other_nondurable_goods")|>
  .d(, "share")
ong <- mean(ong[[1]])

hau <- weights |>
  .d(Item %in% "Housing_and_utilities")|>
  .d(, "share")
hau <- mean(hau[[1]])
  
hc <- weights |>
  .d(Item %in% "Health_care")|>
  .d(, "share")
hc <- mean(hc[[1]])
  
ts <- weights |>
  .d(Item %in% "Transportation_services")|>
  .d(, "share")
ts <- mean(ts[[1]])
  
rs <- weights |>
  .d(Item %in% "Recreation_services")|>
  .d(, "share")
rs <- mean(rs[[1]])
  
fsaa <- weights |>
  .d(Item %in% "Food_services_and_accommodations")|>
  .d(, "share")
fsaa <- mean(fsaa[[1]])
  
fsai <- weights |>
  .d(Item %in% "Financial_services_and_insurance")|>
  .d(, "share")
fsai <- mean(fsai[[1]])
  
os <- weights |>
  .d(Item %in% "Other_services")|>
  .d(, "share")
os<- mean(os[[1]])

means <-  c(mvp, fadhe, rgav, odg, fbp, caf, gaoeg, ong, hau, hc, ts, rs, fsaa, fsai, os)
namen <- c("Motor_vehicles_and_parts", "Furnishings_and_durable_household_equipment", "Recreational_goods_and_vehicles", "Other_durable_goods", "Food_and_beverages_purchased_for_off-premises_consumption", "Clothing_and_footwear", "Gasoline_and_other_energy_goods", "Other_nondurable_goods", "Housing_and_utilities", "Health_care", "Transportation_services", "Recreation_services", "Food_services_and_accommodations", "Financial_services_and_insurance", "Other_services")

meanstable <- data.table(Name = namen, Gewichte = means)


data.table::fwrite(meanstable, "D:/Daten/weight_avg_lvl2.csv")
print(weights)