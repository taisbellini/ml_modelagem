data <- readxl::read_xlsx("drug_consumption_dataset.xlsx")
head(data)

data <- transform(data, light_drugs = ifelse(
  ((Alcohol == "Never Used" | Alcohol == "Used over a Decade Ago") &
     (Caff == "Never Used" | Caff == "Used over a Decade Ago") &
     (Nicotine == "Never Used" | Nicotine == "Used over a Decade Ago") &
     (Choc == "Never Used" | Choc == "Used over a Decade Ago")), 0, 1))

data <- transform(data, medium_drugs = ifelse(
  ((Cokes == "Never Used" | Cokes == "Used over a Decade Ago") &
     (Ecstasy == "Never Used" | Ecstasy == "Used over a Decade Ago") &
     (LSD == "Never Used" | LSD == "Used over a Decade Ago") &
     (Mushrooms == "Never Used" | Mushrooms == "Used over a Decade Ago")), 0, 1))

data <- transform(data, heavy_drugs = ifelse(
  ((Amphet == "Never Used" | Amphet == "Used over a Decade Ago") &
     (Crack == "Never Used" | Crack == "Used over a Decade Ago") &
     (Heroin == "Never Used" | Heroin == "Used over a Decade Ago") &
     (VSA == "Never Used" | VSA == "Used over a Decade Ago")), 0, 1))


head(data)
table(data$light_drugs)
table(data$medium_drugs)
table(data$heavy_drugs)


