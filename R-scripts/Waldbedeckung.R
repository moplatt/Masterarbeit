
library(terra)

setwd("L:/T/95_FG_ENTW_PRJ/30_BAU/2022-2025_Langfristprognose_YETI")
rm(list = ls())

# lade Waldbedeckungs raster
WB <- rast("Daten/Winter_Benjamin/geodaten_gesamtdomain/ses_topo_22_forest_mask_domain_epsg32632.tif")
plot(WB)

# AOIs laden
Achensee <- vect("Daten/GTIF_AOI/01_AOI_Achensee.shp")
Kuehtai <- vect("Daten/GTIF_AOI/02_AOI_Kuehtai.shp")
Kaunertal <- vect("Daten/GTIF_AOI/03_AOI_Kaunertal.shp")

# Funktion zur Berechnung von Waldfläche & Waldanteil
wald_stats <- function(aoi, wb_raster, name) {
  # Vereinige ggf. mehrere Polygone
  aoi_union <- aggregate(aoi)
  
  # Raster auf AOI beschränken
  wb_crop <- crop(wb_raster, aoi_union)
  wb_masked <- mask(wb_crop, aoi_union)
  
  # Werte extrahieren
  raster_vals <- values(wb_masked)
  
  # Zähle nur gültige Wald-Pixel
  wald_pix <- sum(raster_vals == 1, na.rm = TRUE)
  
  # Fläche pro Pixel in m²
  res_pix <- res(wb_raster)
  pix_area_m2 <- res_pix[1] * res_pix[2]
  
  # Waldfläche berechnen
  wald_km2 <- (wald_pix * pix_area_m2) / 1e6
  
  # Gesamtfläche direkt aus Vektorfläche
  ges_km2 <- expanse(aoi_union, unit = "km")
  
  # Prozent berechnen
  waldanteil <- (wald_km2 / ges_km2) * 100
  
  return(data.frame(
    AOI = name,
    Wald_km2 = round(wald_km2, 2),
    Gesamt_km2 = round(ges_km2, 2),
    Waldanteil_prozent = round(waldanteil, 2)
  ))
}

# Berechnung je AOI
result_achensee  <- wald_stats(Achensee, WB, "Achensee")
result_kuehtai   <- wald_stats(Kuehtai, WB, "Kuehtai")
result_kaunertal <- wald_stats(Kaunertal, WB, "Kaunertal")

# Ergebnisse zusammenfassen
wald_gesamt <- rbind(result_achensee, result_kuehtai, result_kaunertal)
print(wald_gesamt)

