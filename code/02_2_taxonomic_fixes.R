# ——————————————————————— #
# ——— Taxonomic Fixes ——— # 
# ——————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com)
# 12.02.2024

taxa[Phylum_division == "nematoda",Subphylum := "unknown (nematoda phylum)"]
taxa[Phylum_division == "nematoda",Superclass := "unknown (nematoda phylum)"]
#nemata are nematodes
taxa[Phylum_division == "nematoda",Phylum_division := "nemata"]
taxa[Phylum_division == "platyhelminthes",Subphylum := "unknown (platyhelminthes phylum)"]
taxa[Phylum_division == "platyhelminthes",Superclass := "unknown (platyhelminthes phylum)"]
taxa[Phylum_division == "nematomorpha",Subphylum := "unknown (nematomorpha phylum)"]
taxa[Phylum_division == "nematomorpha",Superclass := "unknown (nematomorpha phylum)"]
taxa[Phylum_division == "porifera",Subphylum := "unknown (porifera phylum)"]
taxa[Phylum_division == "porifera",Superclass := "unknown (porifera phylum)"]
taxa[Phylum_division == "bryozoa",Subphylum := "unknown (bryozoa phylum)"]
taxa[Phylum_division == "bryozoa",Superclass := "unknown (bryozoa phylum)"]
taxa[Phylum_division == "acanthocephala",Subphylum := "unknown (acanthocephala phylum)"]
taxa[Phylum_division == "acanthocephala",Superclass := "unknown (acanthocephala phylum)"]
taxa[Phylum_division == "nemertea",Subphylum := "unknown (nemertea phylum)"]
taxa[Phylum_division == "nemertea",Superclass := "unknown (nemertea phylum)"]
taxa[Phylum_division == "cnidaria",Subphylum := "unknown (cnidaria phylum)"]
taxa[Phylum_division == "cnidaria",Superclass := "unknown (cnidaria phylum)"]
taxa[Phylum_division == "rotifera",Subphylum := "unknown (rotifera phylum)"]
taxa[Phylum_division == "rotifera",Superclass := "unknown (rotifera phylum)"]
taxa[Phylum_division == "kamptozoa",Subphylum := "unknown (kamptozoa phylum)"]
taxa[Phylum_division == "kamptozoa",Superclass := "unknown (kamptozoa phylum)"]
#collembola not in database at all
taxa[Class == "collembola",Subphylum := "unknown (collembola class)"]
taxa[Class == "collembola",Superclass := "unknown (collembola class)"]
#adenophorea nematodes missing
taxa[Class == "adenophorea",Subphylum := "Missing Subphylum 1"]
taxa[Class == "adenophorea",Superclass := "Missing Superclass 1"]
#taxonomy of cnidaria is different missing subclass rather than blank?
taxa[Class == "anthozoa",Subphylum := "Missing Subphylum 2"]
taxa[Class == "anthozoa",Superclass := "Missing Superclass 3"]
#taxonomy of cnidaria is missing subclass rather than blank?
taxa[Class == "hydrozoa",Subphylum := "Missing Subphylum 9"]
taxa[Class == "hydrozoa",Superclass := "Missing Superclass 15"]
#higher taxonomy of turbellaria is missing subclass rather than blank?
taxa[Class == "turbellaria",Subphylum := "Missing Subphylum 16"]
taxa[Class == "turbellaria",Superclass := "Missing Superclass 26"]
#plumatellida are bryozoa not ectoprocta
taxa[Order == "plumatellida",Phylum_division := "ectoprocta"]
taxa[Order == "plumatellida",Subphylum := "Missing Subphylum 12"]
taxa[Order == "plumatellida",Superclass := "Missing Superclass 22"]
taxa[Order == "plumatellida",Class := "phylactolaemata"]
#oligochaets are a subclass of clitellata
taxa[Order == "haplotaxida",Class := "oligochaeta"]
#spelling mistake
taxa[Order == "unionida",Order := "unionoida"]
#spelling mistake
taxa[Order == "venerida",Order := "veneroida"]
#pisidiidae is now it's own family of sphaeriidae and order of sphaeriida
taxa[Genus == "sphaerium",Family := "pisidiidae"]
taxa[Family == "sphaeriidae",Family := "pisidiidae"]
taxa[Family == "pisidiidae",Order := "veneroida"]
#hydridae is part of the anthoathecata order
taxa[Family == "hydridae",Order := "hydroida"]
#heteroptera is a suborder, actual hemiptera
taxa[Family == "gerridae",Order := "heteroptera"]
taxa[Family == "corixidae",Order := "heteroptera"]
taxa[Family == "naucoridae",Order := "heteroptera"]
taxa[Family == "notonectidae",Order := "heteroptera"]
#aphelocheirus is in aphelocheiridae family and hemiptera
taxa[Family == "aphelocheiridae",Order := "heteroptera"]
taxa[Genus == "aphelocheirus",Family := "naucoridae"]
#snails
taxa[Family == "planorbidae",Order := "basommatophora"]
taxa[Family == "lymnaeidae",Order := "basommatophora"]
taxa[Family == "physidae",Order := "basommatophora"]
#ancylus is actually a planorbidae
taxa[Genus == "ancylus",Family := "ancylidae"]
#snails
taxa[Family == "hydrobiidae",Order := "neotaenioglossa"]
taxa[Family == "thiaridae",Order := "neotaenioglossa"]
taxa[Family == "calyptraeidae",Order := "neotaenioglossa"]
#snails, potamopyrgus is a littorininimorpha order and tateidae  family
taxa[Family == "tateidae",Order := "neotaenioglossa"]
taxa[Genus == "potamopyrgus",Family := "hydrobiidae"]
#snails
taxa[Family == "melanopsidae",Order := "mesogastropoda"]
#zygoptera is a suborder of odonata
taxa[Family == "lestidae",Order := "zygoptera"]
#mussel is in order myida
taxa[Family == "dreissenidae",Order := "veneroida"]
#specific reclassification of mussels
taxa[Genus == "corbicula",Family := "corbiculidae"]
#tubificidae now split into naididae
taxa[Genus == "tubifex",Family := "tubificidae"]
taxa[Genus == "limnodrilus",Family := "tubificidae"]
taxa[Genus == "spirosperma",Family := "tubificidae"]
taxa[Genus == "rhyacodrilus",Family := "tubificidae"]
taxa[Genus == "branchiura",Family := "tubificidae"]
#naididaeare tubificida , oligochaeta
taxa[Family == "naididae",Order := "haplotaxida"]
#tubificidae are tubificida , oligochaeta
taxa[Family == "tubificidae",Order := "haplotaxida"]
taxa[Order == "haplotaxida",Class := "oligochaeta"]
#valvatidae are not classified at order
taxa[Family == "valvatidae",Order := "lower heterobranchia"]
#tachinidae is a chironomid
taxa[Genus == "cricotopus",Family := "chironomidae"]
taxa[Genus == "microtendipes",Family := "chironomidae"]

# - fixed that result from feedback out of the MCMC.newchemical() function in the
# - script 05_hSSD_predict.R
taxa[Genus == "quistadrilus",  ':=' (Family = "tubificidae",
                                     Superclass = "Missing Superclass 20",
                                     Subphylum = "Missing Subphylum 11")]
taxa[Genus == "ilyodrilus",  ':=' (Family = "tubificidae",
                                   Superclass = "Missing Superclass 20",
                                   Subphylum = "Missing Subphylum 11")]
taxa[Genus == "peloscolex",  ':=' (Family = "tubificidae",
                                   Superclass = "Missing Superclass 20",
                                   Subphylum = "Missing Subphylum 11")]
taxa[Family == "cardiidae",  ':=' (Order = "veneroida",
                                   Superclass = "Missing Superclass 6",
                                   Subphylum = "Missing Subphylum 4")]
taxa[Family == "myidae",  ':=' (Order = "myoida",
                                Superclass = "Missing Superclass 6",
                                Subphylum = "Missing Subphylum 4")]
taxa[Family == "mytilidae",  ':=' (Order = "mytiloida",
                                   Superclass = "Missing Superclass 6",
                                   Subphylum = "Missing Subphylum 4")]