
## ------------------------------------------------------------------------

library(readxl)
library(meta)

## ------------------------------------------------------------------------

settings.meta(digits = 2)

## Pairwise meta-analysis

## ------------------------------------------------------------------------

AcuteManiaP = read_excel("~/_mydrive/Teaching/Kea NMA Course/Practicals/AcuteManiaP.xls")
AcuteManiaP = as.data.frame(AcuteManiaP)

## ------------------------------------------------------------------------

str(AcuteManiaP)

## ------------------------------------------------------------------------

pooledSMD1 = metacont(ncont1, mean1, sd1, ncont2, mean2, sd2,
                      data = AcuteManiaP, studlab = studlab, sm = "SMD")
summary(pooledSMD1)

pooledSMD2 = update(pooledSMD1, subset = treat2 == "Risperidone", comb.fixed = FALSE)
summary(pooledSMD2)
                                                      
## ------------------------------------------------------------------------
forest(pooledSMD2, sortvar = n1 + n2)

## ------------------------------------------------------------------------
forest(pooledSMD2, sortvar = ncont1 + ncont2,prediction = TRUE, comb.fixed = FALSE)

## ------------------------------------------------------------------------
pooledOR = metabin(event2, n2, event1, n1,data = AcuteManiaP, studlab = studlab, sm = "OR")
summary(pooledOR)

## ------------------------------------------------------------------------
forest(pooledOR, sortvar = n1 + n2 , layout = "JAMA", allstudies = F,
                                                             fs.study = 10, ff.study = "italic",
                                                             col.diamond = "green", col.diamond.lines = "red",
                                                             col.square = "yellow", col.square.lines = "blue")


## Subgroup analysis and meta-regression

## ------------------------------------------------------------------------
AcuteMania = read_excel("~/_mydrive/Teaching/Kea NMA Course/Practicals/AcuteMania.xls")
AcuteMania = as.data.frame(AcuteMania)
# Select studies with treatments "OLA" and "PLA"
mania.o = AcuteMania[AcuteMania$treatment == "OLA", ]
mania.p = AcuteMania[AcuteMania$treatment == "PLA", ]
# Drop unnecessary variables
mania.o$treatment = mania.o$rob = mania.p$treatment = NULL
# Merge datasets
mania.op = merge(mania.o, mania.p, by = "studyid", suffixes = c(".ola", ".pla"))
# Risk of bias assessment (1 = low risk, 2 = moderate risk)
mania.op$rob = factor(mania.op$rob, levels = 1:2, labels = c("low", "moderate"))

## ------------------------------------------------------------------------
str(mania.op)

## ------------------------------------------------------------------------
Leucht = read_excel("~/_mydrive/Teaching/Kea NMA Course/Practicals/Leucht.xls")
Leucht = as.data.frame(Leucht)

## ------------------------------------------------------------------------
str(Leucht)

## ------------------------------------------------------------------------
leucht.cp = Leucht[Leucht$treat1 == "CPZ" & Leucht$treat2 == "PBO", ]
leucht.cp$year.1990 = leucht.cp$year - 1990
leucht.cp$year.1990

## ------------------------------------------------------------------------
m1 = metabin(r.ola, n.ola, r.pla, n.pla, data = mania.op,studlab = studyid, sm = "OR", method = "Inverse")
summary(m1)

## ------------------------------------------------------------------------
m1.rob = metabin(r.ola, n.ola, r.pla, n.pla, data = mania.op, studlab = studyid,
         sm = "OR", method = "Inverse", byvar = rob)

## ------------------------------------------------------------------------
m1.rob = update(m1, byvar = rob)
summary(m1.rob)

## ------------------------------------------------------------------------
m1.rob.c = update(m1.rob, tau.common = TRUE, comb.fixed = FALSE)
summary(m1.rob.c)

## ------------------------------------------------------------------------
mr1.rob = metareg(m1.rob)

## ------------------------------------------------------------------------
mr1.rob

## ------------------------------------------------------------------------
coef(mr1.rob)

## ------------------------------------------------------------------------
logOR.low = coef(mr1.rob)[1]
round(exp(logOR.low), 2)

## ------------------------------------------------------------------------
logOR.mod = sum(coef(mr1.rob))
round(exp(logOR.mod), 2)

## ------------------------------------------------------------------------
V = vcov(mr1.rob)

## ------------------------------------------------------------------------
metagen(logOR.low, sqrt(V[1, 1]), sm = "OR")

## ------------------------------------------------------------------------
metagen(logOR.mod, sqrt(sum(diag(V)) + 2 * V[1, 2]), sm = "OR")

## ------------------------------------------------------------------------
m2 = metagen(effect, se, data = leucht.cp, studlab = study,sm = "SMD", comb.fixed = FALSE)

## ------------------------------------------------------------------------
mr2 = metareg(m2, year.1990)
mr2

## ------------------------------------------------------------------------
bubble(mr2, col.line = "blue")

## Indirect and mixed comparisons
                                                      
## ------------------------------------------------------------------------
PHOpairwise = read_excel("~/_mydrive/Teaching/Kea NMA Course/Practicals/PHOpairwise.xls")
PHOpairwise = as.data.frame(PHOpairwise)
## ------------------------------------------------------------------------

pooledSMD = metacont(ncont1, mean1, sd1, ncont2, mean2, sd2,
            data = PHOpairwise, studlab = author, sm = "SMD", 
            byvar = contrast, print.byvar = FALSE, comb.fixed = FALSE)
summary(pooledSMD)

## ------------------------------------------------------------------------
directHO = pooledSMD$TE.random.w[pooledSMD$bylevs == "Haloperidol - Olanzapine"]
se.directHO = pooledSMD$seTE.random.w[pooledSMD$bylevs == "Haloperidol - Olanzapine"]

## ------------------------------------------------------------------------
directHP = pooledSMD$TE.random.w[pooledSMD$bylevs == "Haloperidol - Placebo"]
directOP = pooledSMD$TE.random.w[pooledSMD$bylevs == "Olanzapine - Placebo"]

## ------------------------------------------------------------------------
se.directHP = pooledSMD$seTE.random.w[pooledSMD$bylevs == "Haloperidol - Placebo"]
se.directOP = pooledSMD$seTE.random.w[pooledSMD$bylevs == "Olanzapine - Placebo"]

## ------------------------------------------------------------------------
indirectHO = directHP - directOP

## ------------------------------------------------------------------------
se.indirectHO = sqrt(se.directHP**2 + se.directOP**2)

## ------------------------------------------------------------------------
metagen(indirectHO, se.indirectHO, sm = "SMD")
                                                      
## ------------------------------------------------------------------------
DirectIndirectHO = data.frame(source = c("indirect"   , "direct"), 
SMD    = c(indirectHO   , directHO),
seSMD  = c(se.indirectHO, se.directHO))

## ------------------------------------------------------------------------
metagen(SMD, seSMD, data = DirectIndirectHO, studlab = source, comb.random = FALSE)
                                                      