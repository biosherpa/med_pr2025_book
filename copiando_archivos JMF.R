archivos <- list.files(pattern = "\\.qmd$", recursive = TRUE)
# Carpeta destino (asegúrate que existe)
destino <- "C://Users//Usuario//Documents//GitHub//med_pr2025_book"

# Crear la carpeta si no existe
if(!dir.exists(destino)) dir.create(destino, recursive = TRUE)

# Copiar archivos
file.copy(from = archivos,
          to   = file.path(destino, basename(archivos)),
          overwrite = TRUE)
archivos
