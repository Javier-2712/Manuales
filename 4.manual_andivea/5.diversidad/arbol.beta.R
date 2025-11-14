## ------------------------------------------------------------
## arbol.beta.R  —  Construcción de árbol filogenético (phylo)
## Para análisis beta (PD) en iNEXTbeta3D
## Requisitos previos:
##   - Objeto `especies` con:
##       * rownames = abreviaturas únicas
##       * columna `LatinName` = nombre científico completo
## ------------------------------------------------------------

suppressPackageStartupMessages({
  library(taxize)
  library(ape)
  library(dplyr)
  library(stringr)
})

# -------- 1) Validación de entrada --------
if (!exists("especies")) stop("⚠️  El objeto `especies` no existe.")
if (!"LatinName" %in% names(especies))
  stop("⚠️  Falta la columna 'LatinName' en el objeto especies.")

# -------- 2) Limpieza básica de nombres --------
latin_raw <- especies$LatinName %>% as.character()
latin_clean <- latin_raw %>%
  trimws() %>%
  str_replace_all("\\s+", " ") %>%
  str_replace("\\bsp\\.?$|\\bspp\\.?$", "") %>%
  str_trim()

latin_clean <- vapply(
  latin_clean,
  function(x) {
    if (!nzchar(x)) return(x)
    parts <- strsplit(x, " ", fixed = TRUE)[[1]]
    if (length(parts) >= 1) parts[1] <- str_to_title(parts[1])
    if (length(parts) >= 2) parts[2:length(parts)] <- tolower(parts[2:length(parts)])
    paste(parts, collapse = " ")
  },
  character(1)
)

ok <- nzchar(latin_clean) & !is.na(latin_clean)
latin_ok <- latin_clean[ok]
abbr_ok  <- rownames(especies)[ok]

message(sprintf("Total especies: %d | válidas: %d | excluidas: %d",
                nrow(especies), length(latin_ok), sum(!ok)))

# -------- 3) Construir árbol (NCBI por defecto) --------
DB <- "ncbi"

consulta_segura <- function(sp, db) {
  for (i in 1:3) {
    out <- try(classification(sp, db = db), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    Sys.sleep(0.6 * i)
  }
  return(setNames(list(NULL), sp))
}

spcla_list <- consulta_segura(latin_ok, DB)

# especies no encontradas
no_encontradas <- names(spcla_list)[vapply(spcla_list, function(x)
  is.null(x) || all(is.na(x)), logical(1))]

if (length(no_encontradas) > 0)
  message("No encontradas: ", paste(no_encontradas, collapse = ", "))

# eliminar vacías
spcla_ok <- spcla_list[!names(spcla_list) %in% no_encontradas]
latin_ok2 <- names(spcla_ok)
abbr_ok2  <- abbr_ok[match(latin_ok2, latin_ok)]

if (length(spcla_ok) < 2)
  stop("❌ No hay suficiente información para construir el árbol (mínimo 2 especies).")

# -------- 4) Crear árbol --------
tree_obj <- taxize::class2tree(spcla_ok)
if (!inherits(tree_obj$phylo, "phylo"))
  stop("❌ No se pudo construir un objeto phylo válido.")

tree <- tree_obj$phylo

# Renombrar tips con abreviaturas
map_a <- setNames(abbr_ok2, latin_ok2)
tree$tip.label <- ifelse(tree$tip.label %in% names(map_a),
                         map_a[tree$tip.label],
                         tree$tip.label)

# -------- 5) Exportar objeto final --------
arbol.filo.beta <- tree
message(sprintf("🌳 Árbol listo: %d tips, %d nodos internos",
                length(tree$tip.label), tree$Nnode))
