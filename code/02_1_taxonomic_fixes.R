# ——————————————————————— #
# ——— Taxonomic Fixes ——— # 
# ——————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com)
# 12.02.2024

hSSD_taxa[Phylum_division == "nematoda", c("Subphylum", "Superclass") := "unknown (nematoda phylum)"] 
hSSD_taxa[Phylum_division == "nematoda", Phylum_division := "nemata"]
hSSD_taxa[Phylum_division == "platyhelminthes", c("Subphylum", "Superclass") := "unknown (platyhelminthes phylum)"] 
hSSD_taxa[Phylum_division == "nematomorpha", c("Subphylum", "Superclass") := "unknown (nematomorpha phylum)"]
hSSD_taxa[Phylum_division == "porifera",Subphylum := "unknown (porifera phylum)"]
hSSD_taxa[Phylum_division == "porifera",Superclass := "unknown (porifera phylum)"]
hSSD_taxa[Phylum_division == "bryozoa",Subphylum := "unknown (bryozoa phylum)"]
hSSD_taxa[Phylum_division == "bryozoa",Superclass := "unknown (bryozoa phylum)"]
hSSD_taxa[Phylum_division == "acanthocephala",Subphylum := "unknown (acanthocephala phylum)"]
hSSD_taxa[Phylum_division == "acanthocephala",Superclass := "unknown (acanthocephala phylum)"]
hSSD_taxa[Phylum_division == "nemertea",Subphylum := "unknown (nemertea phylum)"]
hSSD_taxa[Phylum_division == "nemertea",Superclass := "unknown (nemertea phylum)"]
hSSD_taxa[Phylum_division == "cnidaria",Subphylum := "unknown (cnidaria phylum)"]
hSSD_taxa[Phylum_division == "cnidaria",Superclass := "unknown (cnidaria phylum)"]
hSSD_taxa[Phylum_division == "rotifera",Subphylum := "unknown (rotifera phylum)"]
hSSD_taxa[Phylum_division == "rotifera",Superclass := "unknown (rotifera phylum)"]
hSSD_taxa[Phylum_division == "kamptozoa",Subphylum := "unknown (kamptozoa phylum)"]
hSSD_taxa[Phylum_division == "kamptozoa",Superclass := "unknown (kamptozoa phylum)"]
#collembola not in database at all
hSSD_taxa[Class == "collembola",Subphylum := "unknown (collembola class)"]
hSSD_taxa[Class == "collembola",Superclass := "unknown (collembola class)"]
#adenophorea nematodes missing
hSSD_taxa[Class == "adenophorea",Subphylum := "Missing Subphylum 1"]
hSSD_taxa[Class == "adenophorea",Superclass := "Missing Superclass 1"]
#taxonomy of cnidaria is different missing subclass rather than blank?
hSSD_taxa[Class == "anthozoa",Subphylum := "Missing Subphylum 2"]
hSSD_taxa[Class == "anthozoa",Superclass := "Missing Superclass 3"]
#taxonomy of cnidaria is missing subclass rather than blank?
hSSD_taxa[Class == "hydrozoa",Subphylum := "Missing Subphylum 9"]
hSSD_taxa[Class == "hydrozoa",Superclass := "Missing Superclass 15"]
#higher taxonomy of turbellaria is missing subclass rather than blank?
hSSD_taxa[Class == "turbellaria",Subphylum := "Missing Subphylum 16"]
hSSD_taxa[Class == "turbellaria",Superclass := "Missing Superclass 26"]
#plumatellida are bryozoa not ectoprocta
hSSD_taxa[Order == "plumatellida",Phylum_division := "ectoprocta"]
hSSD_taxa[Order == "plumatellida",Subphylum := "Missing Subphylum 12"]
hSSD_taxa[Order == "plumatellida",Superclass := "Missing Superclass 22"]
hSSD_taxa[Order == "plumatellida",Class := "phylactolaemata"]
#oligochaets are a subclass of clitellata
hSSD_taxa[Order == "haplotaxida",Class := "oligochaeta"]
#spelling mistake
hSSD_taxa[Order == "unionida",Order := "unionoida"]
#spelling mistake
hSSD_taxa[Order == "venerida",Order := "veneroida"]
#pisidiidae is now it's own family of sphaeriidae and order of sphaeriida 
hSSD_taxa[Genus == "sphaerium",Family := "pisidiidae"]
hSSD_taxa[Family == "sphaeriidae",Family := "pisidiidae"]
hSSD_taxa[Family == "pisidiidae",Order := "veneroida"]
#hydridae is part of the anthoathecata order
hSSD_taxa[Family == "hydridae",Order := "hydroida"]
#heteroptera is a suborder, actual hemiptera
hSSD_taxa[Family == "gerridae",Order := "heteroptera"]
hSSD_taxa[Family == "corixidae",Order := "heteroptera"]
hSSD_taxa[Family == "naucoridae",Order := "heteroptera"]
hSSD_taxa[Family == "notonectidae",Order := "heteroptera"]
#aphelocheirus is in aphelocheiridae family and hemiptera
hSSD_taxa[Family == "aphelocheiridae",Order := "heteroptera"]
hSSD_taxa[Genus == "aphelocheirus",Family := "naucoridae"]
#snails
hSSD_taxa[Family == "planorbidae",Order := "basommatophora"]
hSSD_taxa[Family == "lymnaeidae",Order := "basommatophora"]
hSSD_taxa[Family == "physidae",Order := "basommatophora"]
#ancylus is actually a planorbidae 
hSSD_taxa[Genus == "ancylus",Family := "ancylidae"]
#snails
hSSD_taxa[Family == "hydrobiidae",Order := "neotaenioglossa"]
hSSD_taxa[Family == "thiaridae",Order := "neotaenioglossa"]
hSSD_taxa[Family == "calyptraeidae",Order := "neotaenioglossa"]
#snails, potamopyrgus is a littorininimorpha order and tateidae  family
hSSD_taxa[Family == "tateidae",Order := "neotaenioglossa"]
hSSD_taxa[Genus == "potamopyrgus",Family := "hydrobiidae"]
#snails
hSSD_taxa[Family == "melanopsidae",Order := "mesogastropoda"]
#zygoptera is a suborder of odonata
hSSD_taxa[Family == "lestidae",Order := "zygoptera"]
#mussel is in order myida 
hSSD_taxa[Family == "dreissenidae",Order := "veneroida"]
#specific reclassification of mussels
hSSD_taxa[Genus == "corbicula",Family := "corbiculidae"]
#tubificidae now split into naididae
hSSD_taxa[Genus == "tubifex",Family := "tubificidae"]
hSSD_taxa[Genus == "limnodrilus",Family := "tubificidae"]
hSSD_taxa[Genus == "spirosperma",Family := "tubificidae"]
hSSD_taxa[Genus == "rhyacodrilus",Family := "tubificidae"]
hSSD_taxa[Genus == "branchiura",Family := "tubificidae"]
#naididaeare tubificida , oligochaeta
hSSD_taxa[Family == "naididae",Order := "haplotaxida"]
#tubificidae are tubificida , oligochaeta
hSSD_taxa[Family == "tubificidae",Order := "haplotaxida"]
hSSD_taxa[Order == "haplotaxida",Class := "oligochaeta"]
#tricladida are rhabditophora not turbellaria
hSSD_taxa[Order == "tricladida",Class := "turbellaria"]
#valvatidae are not classified at order
hSSD_taxa[Family == "valvatidae",Order := "lower heterobranchia"]
#tachinidae is a chironomid
hSSD_taxa[Genus == "cricotopus",Family := "chironomidae"]
hSSD_taxa[Genus == "microtendipes",Family := "chironomidae"]

# - fixed that result from feedback out of the MCMC.newchemical() function in the 
# - script 05_hSSD_predict.R
hSSD_taxa[Genus == "quistadrilus",  ':=' (Family = "tubificidae",
                                          Superclass = "Missing Superclass 20",
                                          Subphylum = "Missing Subphylum 11")]
hSSD_taxa[Genus == "ilyodrilus",  ':=' (Family = "tubificidae",
                                        Superclass = "Missing Superclass 20",
                                        Subphylum = "Missing Subphylum 11")]
hSSD_taxa[Genus == "peloscolex",  ':=' (Family = "tubificidae",
                                        Superclass = "Missing Superclass 20",
                                        Subphylum = "Missing Subphylum 11")]
hSSD_taxa[Family == "cardiidae",  ':=' (Order = "veneroida",
                                        Superclass = "Missing Superclass 6",
                                        Subphylum = "Missing Subphylum 4")]
hSSD_taxa[Family == "myidae",  ':=' (Order = "myoida",
                                     Superclass = "Missing Superclass 6",
                                     Subphylum = "Missing Subphylum 4")]
hSSD_taxa[Family == "mytilidae",  ':=' (Order = "mytiloida",
                                        Superclass = "Missing Superclass 6",
                                        Subphylum = "Missing Subphylum 4")]