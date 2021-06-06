## define allowed values for entries with restricted vocabularies
vocab <- function(thing) {
    switch(tolower(thing),
           life_stage = c("adult", "adult?", "age class 1", "age class 1+", "age class 2", "age class 2+", "benthic", "c1", "c2", "c3", "c4", "c5", "calf", "calyptopis", "chick", "chick?", "copepod", "copepodite", "copepodite 1", "copepodite 2", "copepodite 3", "copepodite 4", "copepodite 5", "copepodite 6", "cypris larva", "early life stages", "egg", "f1", "f2", "f3", "f4", "f5", "f6", "furcilia", "juvenile", "larva", "mainly adult", "mainly chick", "n1", "n2", "n3", "n4", "n5", "n6", "nauplius", "paralarva", "pelagic", "postlarva", "pre-molt", "pup", "recently fledged", "subadult", "stage 1", "stage 2", "stage 3", "stage 4", "underyearling", "veliger", "yearling", "zoea", "almost mature", "gravid", "gravid?", "mature", "mature?", "maturing", "maturing?", "iia", "iiia", "iiib", "iiid"),
           sex = c("both", "female", "male", "unknown", "female?", "male?", "both?"),
           identification_method = c("bolus content", "caecum content", "foregut DNA content (18S)", "gizzard content", "gastrovascular content", "gut content", "gut content?", "gut pigment", "inferred from predator morphology", "intestine content", "lipid", "nest detritus", "observed predation", "observed predation (experimental)", "regurgitate content", "regurgitate content?", "regurgitated cast content", "scat content", "stable isotope", "stomach content", "stomach DNA content (COI)", "stomach DNA content (18S)", "stomach flushing", "stomach flushing (multiple flushes)", "stomach flushing?", "unknown", "weighing"),
           consumption_rate_units = c("hour", "day", "night", "month", "4 months", "year", "individual", "ng(pigm)", "g", "kg", "t", "mt", "kills", "prey", "pair", "reproductive period", "salps", "percent body dry weight", "percent body wet weight"),
           breeding_stage = c("breeding", "brooding", "chick rearing", "creche", "developing", "early chick rearing", "fasting", "fledging", "gravid", "guard", "incubation", "lactating", "mature (small eggs; some cream)", "moulting", "non breeding", "perinatal", "post-perinatal", "post breeding", "posthatching", "pregnant", "prehatching", "re-developing (post spent)", "running ripe", "spent", "weaning"),
           size_units = c("um", "mm", "cm", "m"),
           mass_units = c("mg", "g", "kg", "t"),
           qualitative_dietary_importance = c("almost exclusive", "exclusive", "incidental", "major", "minor", "none"),
           size_notes = c("arm length", "body length", "body width", "carapace length", "diameter", "disk diameter", "dorsal mantle length", "given as standard length, should be mantle length?", "hood length", "large", "lower hood length", "lower rostral length", "mantle length", "medium", "otolith diameter", "otolith length", "prosome length", "small", "standard length", "total length (standard 1 length)", "test diameter", "total body length", "total length", "uropod length", "valve diameter"),
           mass_notes = c("estimated from standard length", "103 individuals weighed and body mass after regurgitation", "adult body mass before stomach lavage", "mass estimated from length", "mass estimated from length by allometric equation w=3.66l-489.3", "mass excluding stomach contents (mean mass 1.335kg)", "mass, estimated on basis of wet mass", "reconstituted mass", "standard mass estimated from otolith sizes", "wet mass", "wet mass estimated from crest and hood lengths", "wet mass estimated from lower rostral length", "whole wet body mass", "the reconstructed mass of each taxon for each sample was calculated from the average wet body mass for the species in the sample. the value was then multiplied by the number of individu [truncated]", "wet weight"),
           sample_type = c("scat", "stomach content", "foregut"),
           analysis_type = c("high-throughput sequencing"),
           dna_extraction_method = c("Dneasy tissue kit (Qiagen)", "Maxwell 16", "NucleoSpin Tissue system"),
           sequencing_platform = c("Illumina Mi-Seq"),
           target_gene = c("18S", "18S (v7)", "COI"),
           target_food_group = c("all eukaryotes", "multicellular eukaryotes"),
           isotopes_body_part_used=,
           body_part_used = c("anterior nectophores", "anterior region", "aristotle's lantern muscle", "arms", "beaks", "blades", "blood", "body fragments", "body tissue", "body wall", "bone collagen", "buccal mass", "cellular fraction of blood", "central disk test", "collar", "down", "ectoderm", "egg", "egg white", "eggshell", "epidermis", "excluding shell?", "exoskeleton", "faeces", "feather", "feathers", "food sample", "foot muscle", "gonads", "liver", "hair", "holdfasts", "hood", "intestine tissue", "large beaks", "lateral walls", "lower beak rostrums", "lower beaks", "lower body region", "mantle", "medium beaks", "milk protein", "muscle tissue", "muscle tissue or soft body parts", "muscle tissue or whole animal", "ovaries", "peristome", "placenta", "plasma", "podial vesicles", "pooled specimens", "red blood cells", "serum cholesterol", "shell adductor muscle", "siphon muscle", "skin", "small beaks", "spine muscle", "stipes", "stomach caecum", "stomach oil", "tegument", "tentacles", "test", "tissue", "tube feet", "upper beaks", "vibrissae", "white muscle", "whole organism without shell", "whole organism without gut", "whole blood", "whole food sample", "whole or gutted fish", "whole organism", "whole sample", "whole stomach content", "wings"),
           items_included = c("all", "lower beaks", "upper beaks", "all beaks", "otoliths", "needs checking against source"),
           accumulated_hard_parts_treatment = c("included", "excluded", "unknown", "needs checking against source", "excluded for diet assessment, included for prey size assessment"),
           variability_type = c("SD", "SE"),
           c_n_ratio_type = c("atomic", "mass", "unknown"),
           ##isotopes_pretreatment = c("chloroform:ether prewash", "chloroform:methanol prewash", "chloroform:methanol/methanol prewash", "demineralised with acid, then soaked in distilled water", "no prewash used", "Samples were cleaned and preserved in 70% ethanol and dried in an oven at 50C for 6-24 hours; reduced to a fine powder in which 0.30-0.55 mg were encapsulated for analysis.", "Rinsed and oven-dried", "Acidified and oven-dried"),
           isotopes_lipids_treatment = c("none", "chemical delipidation", "mathematical correction", "chemical delipidation and mathematical correction", "unknown"),
           isotopes_carbonates_treatment = c("none", "acidification", "unknown"),
           group=,
           group_soki = c("Ice Diatoms", "Ice Mixotrophs", "Coccolithophores", "Pelagic Picophytoplankton", "Pelagic Diatoms", "Pelagic Dinoflagellates", "Ice Zoobiota", "Microzooplankton", "Mesozooplankton", "Macrozooplankton", "Salps", "Benthic Deposit feeders", "Macrobenthos", "Benthic Filter feeders", "Krill", "Fish Pelagic", "Fish Mesopelagic", "Toothfish", "Icefish", "Cephalopods", "Antarctic Fur Seal", "Elephant Seal", "Crabeater Seal", "Leopard Seal", "Other Seal", "Adelie Penguin", "Emperor Penguin", "Other Penguins", "Albatross", "Skuas", "Other Seabirds", "Minke Whales", "Other Baleen Whales", "Orca A", "Orca B", "Orca C", "Sperm Whales", "Pelagic Bacteria", "Sediment bacteria", "Ice Bacteria", "Labile Detritus", "Refractory Detritus", "Carrion"),
           measurement_name = c("ash content", "ash-free dry weight", "bell diameter", "carapace length", "carbon:nitrogen ratio", "carbohydrate content", "carbon content", "chitin content", "dry weight", "energy content", "lipid content", "lipid-free dry matter", "nitrogen content", "protein content", "sphere diameter", "skeletal ash content", "standard length", "total length", "wet weight", "water content",
                              "free fatty acid content", "triacylglycerol content", "diacylglycerol-ether content", "wax ester content",
                              "8:0 content", "10:0 content", "11:0 content", "12:0 content", "13:0 content", "14:0 content", "14:1 content", "14:1n-7 content", "14:1n-5 content", "15:1 content", "16:0 content", "16:1n-5 content", "16:1n-9 content", "16:2n-4 content", "16:4n-3 content", "17:0 content", "18:0 content", "16:1n-7 content", "16:1n-7t content", "18 1n-9 content", "18 1n-7 content", "18:2n-4 content", "18:3n-6 content", "18:3n-3 content", "20:1n-9 content", "20:1n-7 content", "20:1n-11 content", "22:1n-13+11 content", "22:1n-9 content", "24:1n-11 content", "24:1n-9 content", "18:2n-6 content", "18:4n-3 content", "20:0 content", "20:2n-6 content", "20:3n-6 content", "20:4n-3 content", "20:4n-6 content", "20:5n-3 content", "22:0 content", "22:2 content", "22:5n-3 content", "22:6n-3 content", "22:1n-11 content", "24:1n-7 content", "15:0 content", "i17:0 content", "i18:0 content", "17:1 content", "18:1n-9 content", "18:1n-7 content", "18:1n-5 content", "22:1n-7 content", "23:0 content", "24:0 content", "24:1 content",
                              "other triacylglycerol fatty acid content", "other wax ester fatty alcohol content", "phytanate content",
                              "other wax ester fatty acid content", "other lipid content", "polar lipid content", "sterol content", "saturated fatty acid content", "monounsaturated fatty acid content", "polyunsaturated fatty acid content", "other fatty acid content", "omega3 fatty acid", "omega6 fatty acid", "saturated fatty alcohol content", "monounsaturated fatty alcohol content", "polyunsaturated fatty alcohol content", "other fatty alcohol content", "omega3 fatty acid content", "omega6 fatty acid content" ## lipid measurements
                              ),
           measurement_units = c("%", "%AFDW", "%DW", "%LFDW", "%WW", "cm", "g", "kcal/gDW", "kcal/gWW", "kJ", "kJ/gAFDW", "kJ/gWW", "kJ/gDW", "mm",
                               ## lipid units
                               "%FAW", ## %fatty acids, by weight
                               "%FALCW", ## % fatty alcohols, by weight
                               "%LW", ## %lipids, by weight
                               "%TAGW", ## %triacylglycerols by weight ## for connan et al. 2007, should this be %TAGFAW?
                               "%WEW" ## %wax esters, by weight ## similarly, %WEFAW?
                               ),
           measurement_class = c("fatty acid", "fatty alcohol", "lipid class", "triacylglycerol fatty acid", "wax ester fatty alcohol", "wax ester fatty acid"),
           stop("unrecognised vocabulary name: ", thing)
           )
}

checkvals <- function(vals, vocabclass, msg, quiet = FALSE, noerror = TRUE) {
    if (missing(msg)) msg <- vocabclass
    vocabclass <- gsub("(prey_|predator_|taxon_)", "", vocabclass)
    if (grepl("variability_type", vocabclass)) vocabclass <- "variability_type"
    expected <- vocab(vocabclass)
    vals <- unique(na.omit(vals))
    if (!quiet) cat(sprintf("  checking %s values against allowed entries ... ",msg))
    ok <- TRUE
    if (length(vals)>0) {
        ## some values may be compound e.g. a method of "stomach flushing, observed predation". Split these.
        ## TODO try unsplit first, and split only if it doesn't match
        vals <- switch(vocabclass,
                       energy_req_units=vals, ## don't split this one
                       consumption_rate_units={
                           warning('check that consumption rate units follow the convention of [prey amount unit]/[predator amount unit]/[temporal unit] e.g. g/individual/day');
                           unlist(str_split(vals, "/"))}, ## split on /
                       unlist(str_split(vals, ",")) ## default, split on ,
                       )
        vals <- str_trim(unique(vals))
        if (!all(vals %in% expected)) {
            ok <- FALSE
            tmp <- sprintf("unexpected values in %s: \"%s\" (allowed values: \"%s\")", msg, paste(setdiff(vals, expected), collapse = "\",\""), paste(expected, collapse = "\",\""))
            if (noerror) {
                cat(tmp)
                warning(tmp)
            } else {
                stop(tmp)
            }
        }
    }
    if (!quiet) {
        if (ok) cat("all OK.")
        cat("\n")
    }
}




