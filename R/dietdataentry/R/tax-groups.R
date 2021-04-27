soki_ruleset <- function(worms_cache_directory = "~/.Rcache") {
    copepoda_rank <- search_worms("Copepoda", cache_directory = worms_cache_directory)$rank
    gammaridea_rank <- search_worms("Gammaridea", cache_directory = worms_cache_directory)$rank
    hyperiidea_rank <- search_worms("Hyperiidea",cache_directory = worms_cache_directory)$rank
    odontoceti_rank <- search_worms("Odontoceti",cache_directory = worms_cache_directory)$rank
    mysticeti_rank <- search_worms("Mysticeti",cache_directory = worms_cache_directory)$rank

    function(worms_record, rule_type, scientific, aphia_id, worms_cache_directory) {
        if (missing(worms_record)) {
            worms_record <- NULL
            if (!missing(aphia_id)) worms_record <- search_worms(aphia_id, cache_directory = worms_cache_directory)
            if (!missing(scientific)) worms_record <- search_worms(scientific, cache_directory = worms_cache_directory)
        }
        if (is.null(worms_record)) {
            ## no rules yet that can be applied without worms_record
            stop("need WoRMS record")
        }
        tmp <- search_worms(ids = worms_record$AphiaID, cache_directory = worms_cache_directory)
        hier <- as.list(tmp$scientificname)
        names(hier) <- tmp$rank
        rule_type <- match.arg(tolower(rule_type), c("predator", "prey"))
        if (nrow(worms_record) < 1) return(NULL)

        get_vernacular <- function(id, cache_directory) {
            vernacular <- worms_common(ids = id)
            if (nrow(vernacular) < 1) {
                vernacular <- NULL
            } else {
                vernacular <- vernacular$vernacular[vernacular$language_code == "eng"]
                if (length(vernacular) < 1) {
                    vernacular <- NULL
                } else {
                    vernacular <- vernacular[1]
                }
                vernacular
            }
        }
        name_and_vernacular <- function(nm, id, cache_directory) {
            if (missing(id)) id <- search_worms(nm, cache_directory = cache_directory)$AphiaID
            if (is.null(id)) {
                nm
            } else {
                cm <- get_vernacular(id, cache_directory = cache_directory)
                if (!is.null(cm)) paste0(nm, " (", cm, ")") else nm
            }
        }

        ## rules common to both types
        if (worms_record$valid_name == "Pleuragramma antarcticum") {
            "<i>Pleuragramma antarcticum</i> (Antarctic silverfish)"
        } else if (worms_record$valid_name == "Champsocephalus gunnari") {
            "<i>Champsocephalus gunnari</i> (mackerel icefish)" ## icefish as species
        } else if (worms_record$genus == "Dissostichus") {
            "<i>Dissostichus</i> spp. (toothfish)"
        } else if (worms_record$valid_name == "Psychroteuthis glacialis") {
            ## squid by family, so leave, except for:
            "<i>Psychroteuthis glacialis</i> (glacial squid)" ## only species in its family
        } else if (worms_record$order == "Salpida") {
            "Salps"
        } else if (worms_record$valid_name == "Euphausia superba") {
            ## separate Ant krill from other euphausiids
            "<i>Euphausia superba</i> (Antarctic krill)"
        } else if (worms_record$order == "Euphausiacea") {
            "Euphausiids (other krill)" ## other euphausiids
        } else if (hier[copepoda_rank] == "Copepoda") {
            "Copepoda (copepods)"
        } else if (hier[gammaridea_rank] == "Gammaridea") {
            "Gammaridea (gammarid amphipods)"
        } else if (hier[hyperiidea_rank] == "Hyperiidea") {
            "Hyperiidea (hyperiid amphipods)"
        } else {
            if (rule_type == "prey") {
                if (worms_record$phylum == "Chordata") {
                    ## these to family
                    name_and_vernacular(worms_record$family, cache_directory = worms_cache_directory)
                } else if (worms_record$kingdom == "Animalia") {
                    ## everything else in Animalia to class
                    name_and_vernacular(worms_record$class, cache_directory = worms_cache_directory)
                } else if (worms_record$class == "Bacillariophyceae") {
                    "Bacillariophyceae (pennate diatoms)"
                } else if (hier$Subphylum == "Dinozoa" && !hier$Class %in% c("Colponemea", "Ellobiopsea", "Myzomonadea", "Perkinsea", "Perkinsasida")) {
                    ## note: Dinozoa alone is not quite right: subphylum dinozoa includes the infraphyla Dinoflagellata and Protalveolata
                    ## but infraphylum is not returned in the hier object
                    ## subset by excluding Protalveolata classes (Colponemea, Ellobiopsea, Myzomonadea, Perkinsea, Perkinsasida)
                    "Dinoflagellata (dinoflagellates)"
                } else if (worms_record$kingdom == "Bacteria") {
                    "Bacteria"
                } else if (worms_record$kingdom %in% c("Chromista", "Plantae", "Protozoa")) {
                    name_and_vernacular(worms_record$phylum, cache_directory = worms_cache_directory)
                } else {
                    stop("uncategorized prey: ", worms_record$scientificname)
                }
            } else if (rule_type == "predator") {
                if (hier[odontoceti_rank] == "Odontoceti") {
                    "Odontoceti (toothed whales)"
                } else if (hier[mysticeti_rank] == "Mysticeti") {
                    "Mysticeti (baleen whales)"
                } else if (worms_record$valid_name %in% c("Arctocephalus gazella", "Arctocephalus tropicalis", "Arctocephalus forsteri")) {
                    "<i>Arctocephalus</i> spp. (Antarctic and subantarctic fur seals)"
                } else if (worms_record$valid_name == "Mirounga leonina") {
                    "<i>Mirounga leonina</i> (southern elephant seals)"
                } else if (worms_record$valid_name %in% c("Lobodon carcinophaga", "Ommatophoca rossii", "Hydrurga leptonyx", "Leptonychotes weddellii")) {
                    "Pack ice seals"
                } else if (worms_record$family == "Spheniscidae") {
                    name_and_vernacular(paste0("<i>", worms_record$valid_name, "</i>"), id = worms_record$AphiaID, cache_directory = worms_cache_directory)
                } else {
                    stop("uncategorized predator: ", worms_record$scientificname)
                }
            }
        }
    }
}
