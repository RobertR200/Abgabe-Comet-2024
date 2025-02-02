# Laden benötigter Pakete
library(dplyr)
library(ggplot2)

# --- 1. CSV-Datei einlesen ---
bikeshare_data <- read.csv("/Users/laurin/Documents/RStudio/bike_sharing_data.csv", header = TRUE, sep = ",", dec = ".")

# --- 2. Filtern nach einer bestimmten Station ---
# Station für unsere Gruppe
bikeshare_filtered <- subset(bikeshare_data, station == "E 16 St & Irving Pl")

# --- 3. Prüfung auf fehlende Werte (NAs) ---
anyNA(bikeshare_filtered) # Prüfen, ob es NAs gibt

# Bestimmen, in welchen Spalten NAs sind
apply(bikeshare_filtered, 2, anyNA)

# --- 4. Fehlende Werte ersetzen (Median-Strategie) ---
na_columns <- c("count", "average_temperature", "precipitation", "windspeed", "max_temperature", "min_temperature")

for (col in na_columns) {
  if (col %in% colnames(bikeshare_filtered)) {
    missing_ids <- which(is.na(bikeshare_filtered[[col]]))
    median_value <- median(bikeshare_filtered[[col]], na.rm = TRUE)
    bikeshare_filtered[missing_ids, col] <- median_value
  }
}

# Sicherstellen, dass keine NAs mehr vorhanden sind
apply(bikeshare_filtered, 2, anyNA)

# --- 5. Entfernen von Zeilen mit fehlenden Werten in kritischen Spalten ---
bikeshare_filtered <- bikeshare_filtered[!is.na(bikeshare_filtered$day_of_year), ]
bikeshare_filtered <- bikeshare_filtered[!is.na(bikeshare_filtered$day_of_week), ]
bikeshare_filtered <- bikeshare_filtered[!is.na(bikeshare_filtered$month_of_year), ]

# --- 6. Umrechnung der Temperaturen von Fahrenheit nach Celsius ---
bikeshare_filtered$average_temperature <- (5 / 9) * (bikeshare_filtered$average_temperature - 32)
bikeshare_filtered$max_temperature <- (5 / 9) * (bikeshare_filtered$max_temperature - 32)
bikeshare_filtered$min_temperature <- (5 / 9) * (bikeshare_filtered$min_temperature - 32)

# Überprüfung der Temperatur-Range nach Umrechnung
range(bikeshare_filtered$average_temperature)
range(bikeshare_filtered$max_temperature)
range(bikeshare_filtered$min_temperature)

# --- 7. Überprüfung auf Anomalien & Entfernen von Ausreißern ---
# Entfernen von negativen Werten für nicht-negative Variablen
bikeshare_filtered <- bikeshare_filtered[bikeshare_filtered$count >= 0, ]
bikeshare_filtered <- bikeshare_filtered[bikeshare_filtered$windspeed >= 0, ]
bikeshare_filtered <- bikeshare_filtered[bikeshare_filtered$precipitation >= 0, ]

# --- 8. Bestimmung des Monats mit den meisten Ausleihen ---
monatliche_ausleihen <- bikeshare_filtered %>%
  group_by(month_of_year) %>%
  summarise(gesamt_ausleihen = sum(count, na.rm = TRUE))

# Monat mit den höchsten Ausleihen bestimmen
monat_max <- monatliche_ausleihen %>%
  filter(gesamt_ausleihen == max(gesamt_ausleihen))

# --- 9. Ergebnis ausgeben ---
cat("Monat mit den meisten Ausleihen:", monat_max$month_of_year, 
    "mit insgesamt", monat_max$gesamt_ausleihen, "ausgeliehenen Fahrrädern.\n")


# --- Grafische Darstellung (Aufgabe 4.2) ---

# 1. Grafik: Zusammenhang zwischen Temperatur (Celsius) und Anzahl ausgeliehener Fahrräder
ggplot(data = bikeshare_filtered) +
  geom_point(aes(x = average_temperature, y = count)) +
  ylab("Anzahl ausgeliehener Fahrräder (pro Tag)") +
  xlab("Durchschnittstemperatur (in °C)") +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrräder und Temperatur (°C)")

# 2. Grafik: Zusammenhang zwischen Niederschlagsmenge und Anzahl ausgeliehener Fahrräder
ggplot(data = bikeshare_filtered) +
  geom_point(aes(x = precipitation, y = count)) +
  ylab("Anzahl ausgeliehener Fahrräder (pro Tag)") +
  xlab("Niederschlagsmenge (in Inches)") +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrräder und Niederschlag")

# 3. Grafik: Zusammenhang zwischen Windgeschwindigkeit (km/h) und Anzahl ausgeliehener Fahrräder
ggplot(data = bikeshare_filtered) +
  geom_point(aes(x = windspeed, y = count)) +
  ylab("Anzahl ausgeliehener Fahrräder (pro Tag)") +
  xlab("Windgeschwindigkeit (km/h)") +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrräder und Windgeschwindigkeit")

# 4. Grafik: Zusammenhang zwischen Zeit und Anzahl ausgeliehener Fahrräder
bikeshare_filtered$date <- as.Date(bikeshare_filtered$date, format = "%Y-%m-%d")
ggplot(data = bikeshare_filtered) +
  geom_point(aes(x = date, y = count)) +
  ylab("Anzahl ausgeliehener Fahrräder (pro Tag)") +
  xlab("Datum") +
  ggtitle("Anzahl ausgeliehener Fahrräder im Zeitverlauf")


# --- 4.3: Zusammenhang zwischen Temperatur und Anzahl ausgeliehener Fahrräder nach Regen/Nicht-Regen ---

# An Regentagen (precipitation > 0)
ggplot(data = filter(bikeshare_filtered, precipitation > 0)) +
  geom_point(aes(x = average_temperature, y = count)) +
  ylab("Anzahl ausgeliehener Fahrräder (pro Tag)") +
  xlab("Durchschnittstemperatur (in °C)") +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrräder und Temperatur (Regentage)")

# An nicht-Regentagen (precipitation == 0)
ggplot(data = filter(bikeshare_filtered, precipitation == 0)) +
  geom_point(aes(x = average_temperature, y = count)) +
  ylab("Anzahl ausgeliehener Fahrräder (pro Tag)") +
  xlab("Durchschnittstemperatur (in °C)") +
  ggtitle("Zusammenhang zwischen Anzahl ausgeliehener Fahrräder und Temperatur (Nicht-Regentage)")


# --- 4.4: Grafische Darstellung der Verteilungen ---

# Fahrradausleihen: Balkendiagramm
ggplot(data = bikeshare_filtered, aes(x = date, y = count)) +
  geom_bar(stat = "identity", fill = "blue") + 
  ylab("Anzahl ausgeliehener Fahrräder") +
  xlab("Datum") +
  ggtitle("Tägliche Fahrradausleihen") +
  theme_minimal()

# Temperaturverlauf: Linie mit LOESS-Glättung
ggplot(data = bikeshare_filtered, aes(x = date, y = average_temperature)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "black") +
  ylab("Durchschnittstemperatur (°C)") +
  xlab("Datum") +
  ggtitle("Temperaturverlauf über den Zeitraum") +
  theme_minimal()

# Niederschlagsmenge: Balkendiagramm
ggplot(data = bikeshare_filtered, aes(x = date, y = precipitation)) +
  geom_bar(stat = "identity", fill = "blue") +
  ylab("Niederschlag (in Inches)") +
  xlab("Datum") +
  ggtitle("Täglicher Niederschlag") +
  theme_minimal()

# Windgeschwindigkeit: Boxplot nach Monaten
ggplot(data = bikeshare_filtered, aes(x = format(date, "%m"), y = windspeed)) +
  geom_boxplot(fill = "gray") +
  ylab("Windgeschwindigkeit (km/h)") +
  xlab("Monat") +
  ggtitle("Windgeschwindigkeit über Monate hinweg") +
  theme_minimal()

# --- 4.5: Verteilung der Anzahl ausgeliehener Fahrräder nach Jahreszeiten ---

# Jahreszeiten definieren
bikeshare_filtered$season <- case_when(
  bikeshare_filtered$month_of_year %in% c(12, 1, 2) ~ "Winter",
  bikeshare_filtered$month_of_year %in% c(3, 4, 5) ~ "Frühling",
  bikeshare_filtered$month_of_year %in% c(6, 7, 8) ~ "Sommer",
  bikeshare_filtered$month_of_year %in% c(9, 10, 11) ~ "Herbst"
)

# Kerndichteschätzung für die vier Jahreszeiten
ggplot(data = bikeshare_filtered, aes(x = count, fill = season)) +
  geom_density(alpha = 0.4) +  # Transparenz für überlappende Bereiche
  scale_fill_manual(values = c("Winter" = "steelblue", "Frühling" = "green", 
                               "Sommer" = "orange", "Herbst" = "red")) +
  ggtitle("Verteilung ausgeliehener Fahrräder in verschiedenen Jahreszeiten") +
  xlab("Anzahl ausgeliehener Fahrräder (pro Tag)") +
  ylab("Dichte der Verteilung") +
  theme_minimal()

# --- 4.6: 3D-Scatterplot mit Temperatur, Windgeschwindigkeit und Anzahl ausgeliehener Fahrräder ---

# Laden des Plotly-Pakets
library(plotly)

# Erstellen des 3D-Scatterplots
plot_ly(data = bikeshare_filtered, 
        x = ~average_temperature, 
        y = ~windspeed, 
        z = ~count, 
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 5, opacity = 0.7),
        color = ~count, colors = colorRamp(c("blue", "red"))) %>%
  layout(scene = list(
    xaxis = list(title = "Durchschnittstemperatur (°C)"),
    yaxis = list(title = "Windgeschwindigkeit (km/h)"),
    zaxis = list(title = "Anzahl ausgeliehener Fahrräder")
  ))

