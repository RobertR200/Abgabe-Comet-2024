# Laden des Test-Pakets
library(testthat)

# --- Aufgabe 3.1 ---
# Funktion zur Berechnung der Wahrscheinlichkeit, dass niemand sein eigenes Geschenk erhält
wichtel_perfekte_ziehung <- function(n, iterationen = 1e6) {
  
  # Simulation: Zufällige Permutationen werden generiert
  erfolg <- replicate(iterationen, {
    perm <- sample(1:n)      # Zufällige Zuordnung der Geschenke
    sum(perm == 1:n) == 0    # Prüfen, ob niemand sein eigenes Geschenk erhält
  })
  
  # Berechnung der Wahrscheinlichkeit
  wahrscheinlichkeit <- mean(erfolg)
  
  return(wahrscheinlichkeit)
}

wichtel_perfekte_ziehung(8)

# --- Aufgabe 3.2 ---
# Funktion zur Berechnung der Wahrscheinlichkeit, dass höchstens k Personen ihr Geschenk zurückbekommen
# Funktion 'wichtel_unglueck'
# Diese Funktion simuliert die Wahrscheinlichkeit, dass höchstens 'k' von 'n' Personen ihr eigenes Geschenk zurückbekommen.
# Dabei wird eine zufällige Permutation der Zahlen 1 bis n erzeugt, die die Geschenkverteilung repräsentiert.
#
# Parameter:
#   n          : Anzahl der Personen (muss eine positive ganze Zahl sein)
#   k          : Maximale Anzahl der Personen, die ihr eigenes Geschenk erhalten dürfen
#                (muss zwischen 0 und n liegen, als ganze Zahl)
#   iterationen: Anzahl der Simulationsdurchläufe (muss eine positive ganze Zahl sein, Standard: 1e6)
#
# Rückgabewert:
#   Die geschätzte Wahrscheinlichkeit (als Dezimalzahl zwischen 0 und 1),
#   dass höchstens k Personen ihr eigenes Geschenk erhalten.
wichtel_unglueck <- function(n, k, iterationen = 1e6) {
  
  # --- Sicherheitsprüfungen der Eingabeparameter ---
  
  # Überprüfe, ob 'n' ein numerischer Skalar ist, größer als 0 und eine ganze Zahl
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || floor(n) != n) {
    stop("Fehler: 'n' muss eine positive ganze Zahl sein.")
  }
  
  # Überprüfe, ob 'k' ein numerischer Skalar ist, nicht negativ und eine ganze Zahl
  if (!is.numeric(k) || length(k) != 1 || k < 0 || floor(k) != k) {
    stop("Fehler: 'k' muss eine nicht-negative ganze Zahl sein.")
  }
  
  # Stelle sicher, dass 'k' nicht größer als 'n' ist
  if (k > n) {
    stop("Fehler: 'k' darf nicht größer als 'n' sein.")
  }
  
  # Überprüfe, ob 'iterationen' ein numerischer Skalar ist, größer als 0 und eine ganze Zahl
  if (!is.numeric(iterationen) || length(iterationen) != 1 || iterationen <= 0 || floor(iterationen) != iterationen) {
    stop("Fehler: 'iterationen' muss eine positive ganze Zahl sein.")
  }
  
  # --- Simulationsschleife ---
  
  # 'replicate' führt den folgenden Codeblock 'iterationen'-mal aus und sammelt die Ergebnisse in einem Vektor.
  erfolg <- replicate(iterationen, {
    
    # Erzeuge eine zufällige Permutation der Zahlen 1 bis n, die die Geschenkverteilung darstellt.
    perm <- sample(1:n)
    
    # Vergleiche jedes Element der Permutation mit der Originalreihenfolge 1:n.
    # Der Ausdruck 'perm == 1:n' liefert einen logischen Vektor, der TRUE an Position i liefert,
    # wenn Person i ihr eigenes Geschenk erhält.
    #
    # 'sum(perm == 1:n)' zählt, wie viele Personen ihr eigenes Geschenk erhalten (Fixpunkte).
    #
    # Die Bedingung 'sum(perm == 1:n) <= k' gibt TRUE zurück, wenn höchstens k Personen ihr eigenes Geschenk erhalten.
    sum(perm == 1:n) <= k
  })
  
  # --- Berechnung der Wahrscheinlichkeit ---
  
  # Da TRUE als 1 und FALSE als 0 gewertet werden, entspricht der Mittelwert von 'erfolg'
  # dem Anteil der Simulationen, in denen höchstens k Personen ihr eigenes Geschenk erhalten haben.
  wahrscheinlichkeit <- mean(erfolg)
  
  # Rückgabe der geschätzten Wahrscheinlichkeit (Dezimalzahl zwischen 0 und 1)
  return(wahrscheinlichkeit)
}


# --- Aufgabe 3.3: Kommentare hinzugefügt ---

# Beispiel für n = 8, k = 2 (Aufgabe 3.2)
cat("Wahrscheinlichkeit, dass höchstens 2 Personen ihr Geschenk zurückbekommen (n = 8, k = 2):\n")
print(wichtel_unglueck(n = 8, k = 2))

# --- Aufgabe 3.4: Unit-Tests mit {testthat} ---
test_that("wichtel_perfekte_ziehung gibt Werte zwischen 0 und 1 zurück", {
  result <- wichtel_perfekte_ziehung(8)
  expect_true(result >= 0 && result <= 1)
})

test_that("wichtel_unglueck gibt Werte zwischen 0 und 1 zurück", {
  result <- wichtel_unglueck(8, 2)
  expect_true(result >= 0 && result <= 1)
})

test_that("wichtel_unglueck gibt Fehler für ungültige Eingaben", {
  expect_error(wichtel_unglueck(-1, 2))
  expect_error(wichtel_unglueck(8, -1))
  expect_error(wichtel_unglueck(8, 9))  # k darf nicht größer als n sein
  expect_error(wichtel_unglueck(8, 2, -1000))
})

