

## ------------------------------------------------------------------------
library(readxl)
library(netmeta)

## ------------------------------------------------------------------------
AcuteMania = read_excel("~/_mydrive/Teaching/Kea NMA Course/Practicals/AcuteMania.xls")
AcuteMania = as.data.frame(AcuteMania)

## ------------------------------------------------------------------------
Leucht = read_excel("~/_mydrive/Teaching/Kea NMA Course/Practicals/Leucht.xls")
Leucht = as.data.frame(Leucht)

## ------------------------------------------------------------------------
AcuteManiaPair = pairwise(treat = treatment, event = r, n = n,
                          data = AcuteMania, studlab = studyid, sm = "OR")
net1 = netmeta(AcuteManiaPair, ref = "PLA", comb.fixed = FALSE, comb.random = TRUE)

## ------------------------------------------------------------------------
net3 = netmeta(effect, se, treat1, treat2, study, data = Leucht,
               sm = "SMD", ref = "PBO", comb.fixed = FALSE, comb.random = TRUE,
               tol.multiarm = 0.075)

## ------------------------------------------------------------------------
net1$d

## ------------------------------------------------------------------------
designs1 = as.character(decomp.design(net1)$Q.het.design$design)
designs1

## ------------------------------------------------------------------------
split1 = netsplit(net1) 
print(split1, showall = FALSE, digits = 2)

## ------------------------------------------------------------------------
decomp.design(net1)

## ------------------------------------------------------------------------
net3$d

## ------------------------------------------------------------------------
designs3 = as.character(decomp.design(net3)$Q.het.design$design)
designs3

## ------------------------------------------------------------------------
split3 = netsplit(net3) 
print(split3, showall = FALSE, digits = 2)

## ------------------------------------------------------------------------
decomp.design(net3)

