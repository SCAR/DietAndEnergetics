#' Rule set for determining SOKI groups
#'
#' @param worms_cache_directory string: path to a cache directory
#'
#' @return A function that takes the below parameters, and returns a string giving the SOKI group name
#' * `worms_record` 1-row tibble: as returned by [search_worms()]
#' * `rule_type` string: "prey" or "predator"
#' * `scientific` string: if `worms_record` not provided, the scientific name to search on
#' * `aphia_id` integer: if `worms_record` not provided, the Aphia ID to search on
#' * `worms_cache_directory` string: path to a cache directory
#'
#' @examples
#' \dontrun{
#'   rules <- soki_ruleset()
#'   rules(search_worms("Aptenodytes forsteri"), "predator")
#' }
#'
#' @export
soki_ruleset <- function(worms_cache_directory = worms_cache_dir()) {
    copepoda_rank <- search_worms("Copepoda", cache_directory = worms_cache_directory)$rank ## was Subclass
    gammaridea_rank <- search_worms("Gammaridea", cache_directory = worms_cache_directory)$rank
    hyperiidea_rank <- search_worms("Hyperiidea",cache_directory = worms_cache_directory)$rank
    odontoceti_rank <- search_worms("Odontoceti",cache_directory = worms_cache_directory)$rank
    mysticeti_rank <- search_worms("Mysticeti",cache_directory = worms_cache_directory)$rank

    function(worms_record, rule_type, scientific, aphia_id, worms_cache_directory = worms_cache_dir()) {
        rule_type <- match.arg(tolower(rule_type), c("predator", "prey", "taxon"))
        if (missing(worms_record)) {
            ## special cases
            if ((!missing(aphia_id) && aphia_id %in% 152230) || (!missing(scientific) && scientific %in% "Coelenterata")) {
                return("Coelenterata (comb jellies and cnidarians)")
            }
            if (!missing(scientific) && grepl(" scales$", scientific)) scientific <- sub(" scales$", "", scientific)
            if (!missing(scientific) && tolower(scientific) %eq% "nematocysts") scientific <- "Cnidaria"
            if (!missing(scientific) && tolower(scientific) %eq% "skate egg case") scientific <- "Eggs"
            if (!missing(scientific) && tolower(scientific) %in% c("algae", "carrion", "detritus", "eggs", "eumetazoa", "fish", "fish eggs", "invertebrata", "invertebrate eggs", "macroalgae", "microphytobenthos", "organic matter", "phytoplankton", "phytoplankton and detritus", "sediment", "sediment organic matter", "whale remains")) {
                return(scientific)
            }
            if (rule_type == "taxon" && !missing(scientific) && tolower(scientific) %in% c("benthic biofilm", "epiphytic diatoms", "green algae", "large-fraction particulate organic matter", "particulate organic matter", "plankton", "pygoscelis adeliae guano", "sea ice algae", "sediment", "sediment trap", "small-fraction particulate organic matter", "suspended particulate organic matter", "sympagic algae", "zooplankton")) {
                return(scientific)
            }
            worms_record <- NULL
            ## if we have been given an AphiaID, ignore its status
            if (!missing(aphia_id)) worms_record <- search_worms(ids = aphia_id, cache_directory = worms_cache_directory, acceptable_status = NULL)
            if (!missing(scientific)) {
                ## also catch some special cases
                ## things that either aren't diet (e.g. gravel) or aren't specific prey items ("all prey")
                if (tolower(scientific) %in% c("none", "all", "all prey", "anthropogenic material", "bait", "chilli peppers", "fecal pellet", "filter feeding", "food waste", "gravel", "hair", "hooks", "offal", "paint debris", "synthetic material", "unidentified", "unidentified fecal pellets", "mucus", "netting debris", "parasites", "parasitic worms", "plastic", "residue", "sand", "station refuse", "stones")) return(NA_character_)
                if (scientific == "Euphausiacea moult debris") scientific <- "Euphausiacea"
                if (scientific %in% c("Feathers", "Prions")) scientific <- "Aves"
                ## for the purposes of soki groups, we can ignore subspecies (?)
                scientific <- stringr::str_trim(gsub(" subsp\\.?$", "", scientific))
                if (length(strsplit(scientific, "[[:space:]]+")[[1]]) == 3 && grepl("^(Eudyptes|Pelecanoides|Phalacrocorax)", scientific)) scientific <- paste(strsplit(scientific, "[[:space:]]+")[[1]][-3], collapse = " ")
                worms_record <- search_worms(scientific = scientific, cache_directory = worms_cache_directory)
            }
        }
        if (is.null(worms_record) || nrow(worms_record) != 1) {
            ## no rules yet that can be applied without worms_record
            msg <- "need unique WoRMS record"
            if (!missing(scientific)) msg <- paste0(msg, " for name: ", scientific)
            if (!missing(aphia_id)) msg <- paste0(msg, " for aphia ID: ", aphia_id)
            stop(msg)
        }
        if (rule_type == "taxon") rule_type <- "predator"
        tmp <- worms_hierarchy(id = worms_record$AphiaID, cache_directory = worms_cache_directory)
        hier <- as.list(tmp$scientificname)
        names(hier) <- tmp$rank
        if (nrow(worms_record) < 1) return(NULL)

        get_vernacular <- function(id, cache_directory = worms_cache_directory) {
            vernacular <- worms_common(ids = id, cache_directory = cache_directory)
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
        name_and_vernacular <- function(nm, id, rank, cache_directory = worms_cache_directory) {
            if (missing(id)) {
                if (nm %eq% "Gonostomatidae") {
                    ## there are two Gonostomatidae families!
                    ## can't be bothered fixing this properly: hard code for now
                    return("Gonostomatidae (bristlemouths)")
                }
                srch <- nm
                if (!missing(rank)) srch <- paste0(srch, "@rank:", rank)
                id <- search_worms(srch, cache_directory = cache_directory)$AphiaID
            }
            if (is.null(id)) {
                nm
            } else {
                cm <- get_vernacular(id)
                if (!is.null(cm)) paste0(nm, " (", cm, ")") else nm
            }
        }

        ## rules common to both types
        if (worms_record$valid_name %eq% "Pleuragramma antarctica") {
            "<i>Pleuragramma antarctica</i> (Antarctic silverfish)"
        } else if (worms_record$valid_name %eq% "Champsocephalus gunnari") {
            "<i>Champsocephalus gunnari</i> (mackerel icefish)" ## icefish as species
        } else if (worms_record$genus %eq% "Dissostichus") {
            "<i>Dissostichus</i> spp. (toothfish)"
        } else if (worms_record$valid_name %eq% "Psychroteuthis glacialis") {
            ## squid by family, so leave, except for:
            "<i>Psychroteuthis glacialis</i> (glacial squid)" ## only species in its family
        } else if (worms_record$order %eq% "Salpida") {
            "Salps"
        } else if (worms_record$valid_name %eq% "Euphausia superba") {
            ## separate Ant krill from other euphausiids
            "<i>Euphausia superba</i> (Antarctic krill)"
        } else if (worms_record$order %eq% "Euphausiacea") {
            "Euphausiids (other krill)" ## other euphausiids
        } else if (!is.null(hier[copepoda_rank]) && hier[copepoda_rank] %eq% "Copepoda") {
            "Copepoda (copepods)"
        } else if (!is.null(hier[gammaridea_rank]) && hier[gammaridea_rank] %eq% "Gammaridea") {
            "Gammaridea (gammarid amphipods)"
        } else if (!is.null(hier[hyperiidea_rank]) && hier[hyperiidea_rank] %eq% "Hyperiidea") {
            "Hyperiidea (hyperiid amphipods)"
        } else if (worms_record$order %eq% "Amphipoda") {
            "Amphipoda (amphipods)"
        } else if (worms_record$family %eq% "Hydrobatidae") {
            "Hydrobatidae (storm petrels)"
        } else if (worms_record$family %eq% "Diomedeidae") {
            "Diomedeidae (albatrosses)"
        } else if (worms_record$family %eq% "Pelecanoididae") {
            "Pelecanoididae (diving petrels)"
        } else if (worms_record$order %eq% "Procellariiformes") {
            "Procellariidae (procellariid seabirds)"
        } else {
            if (rule_type == "prey") {
                if (worms_record$phylum %eq% "Chordata") {
                    ## these to family
                    chc <- c("family", "order", "class", "phylum")
                    for (thischc in chc) {
                        if (!is.na(worms_record[[thischc]])) return(name_and_vernacular(worms_record[[thischc]], rank = thischc))
                    }
                } else if (worms_record$kingdom %eq% "Animalia") {
                    ## everything else in Animalia to class
                    chc <- c("class", "phylum")
                    for (thischc in chc) {
                        if (!is.na(worms_record[[thischc]])) return(name_and_vernacular(worms_record[[thischc]], rank = thischc))
                    }
                } else if (worms_record$class %eq% "Bacillariophyceae") {
                    "Bacillariophyceae (pennate diatoms)"
                } else if (!is.null(hier$Subphylum) && hier$Subphylum %eq% "Dinozoa" && !is.null(hier$Class) && !hier$Class %in% c("Colponemea", "Ellobiopsea", "Myzomonadea", "Perkinsea", "Perkinsasida")) {
                    ## note: Dinozoa alone is not quite right: subphylum dinozoa includes the infraphyla Dinoflagellata and Protalveolata
                    ## but infraphylum is not returned in the hier object
                    ## subset by excluding Protalveolata classes (Colponemea, Ellobiopsea, Myzomonadea, Perkinsea, Perkinsasida)
                    "Dinoflagellata (dinoflagellates)"
                } else if (worms_record$kingdom %eq% "Bacteria") {
                    "Bacteria"
                } else if (worms_record$kingdom %in% c("Fungi", "Chromista", "Plantae", "Protozoa")) {
                    chc <- c("phylum", "kingdom")
                    for (thischc in chc) {
                        if (!is.na(worms_record[[thischc]])) return(name_and_vernacular(worms_record[[thischc]], rank = thischc))
                    }
                } else {
                    stop("uncategorized prey: ", worms_record$scientificname)
                }
            } else if (rule_type == "predator") {
                if (!is.null(hier[odontoceti_rank]) && hier[odontoceti_rank] %eq% "Odontoceti") {
                    "Odontoceti (toothed whales)"
                } else if (!is.null(hier[mysticeti_rank]) && hier[mysticeti_rank] %eq% "Mysticeti") {
                    "Mysticeti (baleen whales)"
                } else if (worms_record$valid_name %in% c("Arctocephalus gazella", "Arctocephalus tropicalis", "Arctocephalus forsteri")) {
                    "<i>Arctocephalus</i> spp. (Antarctic and subantarctic fur seals)"
                } else if (worms_record$valid_name %eq% "Mirounga leonina") {
                    "<i>Mirounga leonina</i> (southern elephant seals)"
                } else if (worms_record$valid_name %in% c("Lobodon carcinophaga", "Ommatophoca rossii", "Hydrurga leptonyx", "Leptonychotes weddellii")) {
                    "Pack ice seals"
                } else if (worms_record$family %eq% "Spheniscidae") {
                    name_and_vernacular(paste0("<i>", worms_record$valid_name, "</i>"), id = worms_record$AphiaID)
                ## fallbacks now
                ##} else if (worms_record$phylum %in% c("Chordata", "Echinodermata", "Arthropoda")) {
                ##    ## these to family
                ##    name_and_vernacular(worms_record$family)
                ##} else if (worms_record$kingdom %eq% "Animalia") {
                ##    ## everything else in Animalia to order
                ##    name_and_vernacular(worms_record$order)
                } else if (worms_record$kingdom %in% c("Animalia", "Chromista", "Fungi", "Plantae", "Protozoa")) {
                    ## these might not be "predators" but rule_type "predator" might also be used for e.g. isotope taxon names
                    chc <- c("family", "order", "class", "phylum")
                    for (thischc in chc) {
                        if (!is.na(worms_record[[thischc]])) return(name_and_vernacular(worms_record[[thischc]], rank = thischc))
                    }
                ##} else if (!is.na(worms_record$genus)) {
                ##    name_and_vernacular(worms_record$genus, cache_directory = worms_cache_directory)
                } else {
                    stop("uncategorized predator: ", worms_record$scientificname)
                }
            }
        }
    }
}
