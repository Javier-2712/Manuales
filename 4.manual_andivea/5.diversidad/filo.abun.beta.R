# filo.abun.beta.R
# Alinear nombres entre biol (abundancias) y árbol phylo (beta)
# Permite uso de tabla tax1 (Abbrev, LatinName)

filo_abun_beta <- function(biol, tree, tax1 = NULL) {
  stopifnot(inherits(tree, "phylo"))
  if (is.null(rownames(biol)))
    stop("`biol` necesita rownames con nombres de especie o abreviaturas.")
  
  .fix <- function(x) gsub("\\s+", " ", trimws(x))
  rn  <- .fix(rownames(biol))
  tip <- .fix(tree$tip.label)
  
  # Tabla de equivalencias
  if (!is.null(tax1)) {
    tax1 <- tax1 %>% 
      mutate(across(c(Abbrev, LatinName), ~ .fix(as.character(.x))))
    map_abbrev_to_latin <- setNames(tax1$LatinName, tax1$Abbrev)
    map_latin_to_abbrev <- setNames(tax1$Abbrev,  tax1$LatinName)
  } else {
    map_abbrev_to_latin <- map_latin_to_abbrev <- NULL
  }
  
  # Unificar nombres (según formato predominante)
  if (!is.null(map_abbrev_to_latin)) {
    if (mean(rn %in% names(map_abbrev_to_latin)) > 0.7) {
      rn_new <- unname(map_abbrev_to_latin[rn])
      rownames(biol) <- rn_new
      rn <- rn_new
    }
    if (mean(tip %in% names(map_abbrev_to_latin)) > 0.7) {
      tree$tip.label <- unname(map_abbrev_to_latin[tip])
      tip <- tree$tip.label
    }
  }
  
  # Intersección
  comunes <- intersect(rn, tip)
  drop_biol <- setdiff(rn, comunes)
  drop_tree <- setdiff(tip, comunes)
  
  if (length(drop_biol) > 0)
    biol <- biol[!rownames(biol) %in% drop_biol, , drop = FALSE]
  if (length(drop_tree) > 0)
    tree <- ape::drop.tip(tree, drop_tree)
  
  # Reordenar biol
  ord <- match(tree$tip.label, rownames(biol))
  biol <- biol[ord, , drop = FALSE]
  
  list(
    biol = biol,
    tree = tree,
    drop_from_biol = drop_biol,
    drop_from_tree = drop_tree
  )
}