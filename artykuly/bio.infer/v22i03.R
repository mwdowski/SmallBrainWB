# Merge EMAP biological data with standardized taxonomy
library(bio.infer)
options(width = 60)
data(itis.ttable)
data(bcnt.emapw)
bcnt.tax <- get.taxonomic(bcnt.emapw, itis.ttable,
                          outputFile = "sum.tax.table.txt")

# Show excerpt from full taxonomic table
df1 <- read.delim("sum.tax.table.txt")
incvec <- df1[, "FAMILY"] == "EPHEMERIDAE"
incvec[is.na(incvec)] <- F
print(df1[incvec,c("ORDER", "FAMILY", "GENUS", "SPECIES", "TAXANAME")])

# compute taxon-environment relationships for EMAP species data only
data(envdata.emapw)
coef <- taxon.env(form = ~STRMTEMP + STRMTEMP^2, bcnt = bcnt.tax, 
                  envdata = envdata.emapw, bcnt.siteid = "ID.NEW",
                  bcnt.abndid = "ABUND", env.siteid = "ID.NEW",
                  tlevs = "SPECIES", dumpdata = TRUE)

# Echo names of coefficient data
names(coef)

# View taxon-environment relationships
view.te(coef,plotform = "windows")

# Plot histogram of area under ROC values
par(xaxs = "i", yaxs = "i", mar = c(4,4,1,1))
breaks <- seq(from =0.5,to = 1, by = 0.05)
hist(coef[["roc"]], col = "lightgray", breaks =breaks,
     xlab = "", ylab = "", main = "")
mtext("Area under ROC", side = 1, line = 2.3)
mtext("Number of taxa", side = 2, line = 2.3)

# Estimate taxon-environment relationships for all taxa
coef <- taxon.env(form = ~STRMTEMP + STRMTEMP^2, bcnt = bcnt.tax, 
                  envdata = envdata.emapw, bcnt.siteid = "ID.NEW", 
                  bcnt.abndid = "ABUND", env.siteid = "ID.NEW",
                  tlevs = "all", dumpdata = FALSE)

# Assign operational taxonomic units (OTU) to OR data
data(bcnt.OR)
bcnt.tax.OR <- get.taxonomic(bcnt.OR, itis.ttable)
bcnt.otu.OR <- get.otu(bcnt.tax.OR, coef)

# Compute inferences for temperature for one site in OR
# and plot likelihood curve
ss <- makess(bcnt.otu.OR)
inferences <- mlsolve(ss, coef, site.sel = "99046CSR", bruteforce = T)
print(inferences)

# Compute inferences at all sites in OR
inferences <- mlsolve(ss, coef, site.sel = "all", bruteforce = F)

# Compare inferences in OR to measured temperature
data(envdata.OR)
df1 <- merge(envdata.OR, inferences, by.x = "STRM.ID", by.y = "SVN")
par(mar=c(3.4,3.4,1,1), pty = "s")
lim0 <- range(c(df1$STRMTEMP, df1$temp), na.rm = T)
plot(df1$STRMTEMP, df1$temp, xlab = "", ylab = "", xlim = lim0, 
ylim = lim0, axes = F)
axis(1)
axis(2, las = 1)
box(bty = "l")
mtext("Inferred temperature", side = 1, line = 2.3)
mtext("Measured temperature", side = 2, line = 2.3)
abline(0,1, lty = "dashed")
sqdiff <- (df1$temp - df1$STRMTEMP)^2
n <- sum(! is.na(sqdiff))
rmserr <- sqrt(sum(sqdiff, na.rm = T)/n)

# Examine pre-computed taxon-environment relationships
data(coef.west.wt)
view.te(coef.west.wt, plotform = "windows")

# Compute inferences at a single site in OR using \
# pre-computed taxon-environment relationships
bcnt.otu.OR <- get.otu(bcnt.tax.OR, coef.west.wt)
ss <- makess(bcnt.otu.OR)
inference <- mlsolve(ss, coef.west.wt, site.sel = "99046CSR", bruteforce = T)
print(inference)










