load("Albania_WP.RData")
mydata = mydata[mydata$year==2017, ]
dim(mydata)
summary(mydata$weights)

# Descriptive statistics
par(mfrow = c(1, 3))
# GENDER: 1 = Male, 2 = Female
barplot(table(mydata$gender),
        col = c("lightblue", "pink"),
        names.arg = c("Male", "Female"),
        main = "Distribution by Gender",
        ylab = "Number of respondents")

# EDUCATION: 1 = Low, 2 = Medium, 3 = High
barplot(table(mydata$educ),
        col = colorRampPalette(c("#b2f0b2", "#4CAF50"))(3),
        names.arg = c("Low", "Medium", "High"),
        main = "Education Level",
        ylab = "Number of respondents")

# URBAN: 1 = Rural, 2 = Town, 3 = City, 4 = Capital
barplot(table(mydata$urban),
        col = colorRampPalette(c("#FFE5B4", "#FFA500"))(4),
        names.arg = c("Rural", "Town", "City", "Capital"),
        main = "Area of Residence",
        ylab = "Number of respondents")

par(mfrow = c(1,2))
# INCOME QUINTILES: 1 = poorer, ..., 5 = richer 
boxplot(mydata$income_int_dol ~ mydata$income_quint,
        col = "lightblue",
        main = "Income Distribution",
        xlab = "Income Quintile",
        ylab = "Household Income (USD)")

# INCOME
hist(mydata$income_int_dol,
     col = "lightblue",
     main = "Histogram Income",
     xlab = "Income in USD",
     breaks = 30)

# Items of FIES scale
fies = mydata[, c("WORRIED", "HEALTHY", "FEWFOOD","SKIPPED","ATELESS","RUNOUT","HUNGRY","WHLDAY" )]
# NA Analysis
sumNA = function(vec)
  sum(is.na(vec))
apply(fies, 2, sumNA)
data_fies = na.omit(fies)
dim(data_fies)

# Item's Distributions
item_names = colnames(data_fies)
par(mfrow = c(2, 4)) 

# Cycle over all items to create the barplots
for (item_name in item_names) {
  item_counts = table(data_fies[[item_name]])
  item_proportions = prop.table(item_counts)
  
  barplot(item_proportions,
          main = paste("Distribution of \n", item_name), 
          ylab = "Frequency",
          names.arg = c("No", "Yes"), 
          col = c("coral", "lightgreen")) 
}
par(mfrow = c(1, 1)) 

# RAW SCORE DISTRIBUTION
raw_scores = apply(data_fies, 1, sum)
freq_table = table(raw_scores)
bp = barplot(freq_table,
             col = "lightblue",
             main = "Distribution of Raw Scores",
             xlab = "Raw Score (number of positive responses)",
             ylab = "Frequency",
             ylim = c(0, max(freq_table) + 50))
text(x = bp,
     y = freq_table,
     labels = freq_table,
     pos = 3, 
     cex = 0.9)

# Cronbach's Alpha 
# sum variances of individual items
var.j = sum(apply(data_fies, 2, var))
# variance of the total scale score
var.t = var(apply(data_fies, 1, sum))
# Alpha
alpha = (ncol(data_fies)/(ncol(data_fies)-1))*(1-var.j/var.t) 
alpha

# Correlation Matrix
library(corrplot)
M = cor(data_fies) 
corrplot(M, method = "number",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         col = COL2("RdBu", 200))

# Unidimensionality pre-fit
library(psych)
# Bartlett test
cortest.bartlett(data_fies)$p.value

# Parallel Analysis to determine the number of factors
fa.parallel(data_fies, fa = "pc", n.iter = 1000, show.legend = TRUE, cor = "tet")
# 1 factor
fit_efa_unidimensional = fa(data_fies,
                            nfactors = 1,
                            rotate = "none",
                            fm = "minres",
                            cor = "tet")

# Barplot for factorial loads
par(mar = c(9, 4, 4, 2) + 0.1)
loadings = fit_efa_unidimensional$loadings[,1]
loadings_ordered = sort(loadings, decreasing = TRUE)
loadings_rounded = round(loadings_ordered, 2)
par(mar = c(8, 4, 4, 2) + 0.1)
bp2 = barplot(
  height = loadings_ordered,            
  names.arg = names(loadings_ordered),  
  horiz = FALSE,                        
  las = 1,                              
  col = "skyblue",                      
  ylab = "Factorial Load Intenisty", 
  xlab = "Items",       
  main = "Factorial Loads", 
  ylim = c(0, max(loadings_ordered) * 1.1), 
  cex.names = 0.7,                      
  cex.lab = 1.2,                        
  cex.main = 1.4                        
)
text(x = bp2,                             
     y = loadings_ordered + (max(loadings_ordered) * 0.03), 
     labels = loadings_rounded, 
     pos = 3,                           
     cex = 0.6,                         
     col = "black") 

# Rasch model estimation
library(eRm)
fit.rm = RM(data_fies, sum0=TRUE)
print(fit.rm)

old.par = par(no.readonly = TRUE)
par(cex = 0.4, lwd = 1, cex.lab = 1.8, cex.main = 1.8) 
plotjointICC(fit.rm,
             ylab = "Probability of affermative response",
             xlab = "True level of Food Insecurity",
             lwd = 2,
             cex = 0.7) 
abline(h = 0.5)
par(old.par)

# Analysis of individuals
pers.dati = person.parameter(fit.rm) 
theta = round(pers.dati$thetapar$NAgroup1, 3)
length(theta)
mean(theta)

old.par = par(no.readonly = TRUE)
par(lwd = 1)
plot(pers.dati, xlab="Individuals' raw scores",
     ylab="Estimated severity", 
     main="Graph of estimated insecurities as a \n function of observed scores"
)
grid(col = "lightgray", lty = "dotted") 
par(old.par)

# Person Item map
plotPImap(fit.rm, irug=TRUE, sort=T, 
          latdim = expression(paste(theta[i]," , ", beta[j])),
          pplabel = "Distribuzione \n 
          insicurezza 
          alimentare")

# Diagnostic of the mode
LRtest(fit.rm)
itemfit(pers.dati)

# Reliability and separability
theta = pers.dati$thetapar$NAgroup1
SE = pers.dati$se.theta$NAgroup1
valid = !is.na(theta) & !is.na(SE)
theta = theta[valid]
SE = SE[valid]
var_true = var(theta)
mean_sq_error = mean(SE^2)
person_reliability = var_true / (var_true + mean_sq_error)
person_separation_index = sqrt(var_true / mean_sq_error)
cat("Reliability (individuals):", round(person_reliability, 3), "\n")
cat("Separability index (individuals):", round(person_separation_index, 3), "\n")

var_items = diag(vcov(fit.rm))
SE_items = sqrt(var_items)
item_difficulty = -fit.rm$betapar
var_true_item = var(item_difficulty)
mean_sq_error_item = mean(SE_items^2)
item_reliability = var_true_item / (var_true_item + mean_sq_error_item)
item_separation_index = sqrt(var_true_item / mean_sq_error_item)
cat("Reliability (item):", round(item_reliability, 3), "\n")
cat("Separability Index:", round(item_separation_index, 3), "\n")

# DIF
mydata_complete = mydata[complete.cases(fies), ]

# DIF for Gender
group = as.factor(mydata_complete$gender)  
dif_results = LRtest(fit.rm, splitcr = group)
print(dif_results)

# DIF for Urban
group_urban = as.factor(mydata_complete$urban)
dif_urban = LRtest(fit.rm, splitcr = group_urban)
print(dif_urban)

# DIF for Income
group_income = as.factor(mydata_complete$income_quint)
dif_income = LRtest(fit.rm, splitcr = group_income)
print(dif_income)

# DIF for Education
group_educ = as.factor(mydata_complete$educ)
dif_educ = LRtest(fit.rm, splitcr = group_educ)
print(dif_educ)

# Prevalence rates
prevalences = colMeans(mydata_complete[, item_names], na.rm = TRUE)
par(mar = c(5, 8, 4, 2))
# Barplot
barplot(prevalences,
        horiz = TRUE,
        las = 1,
        col = "steelblue",
        main = "Prevalence Rates for each Item",
        xlab = "Prevalence Rates")

barplot_prev = function(group_var, group_name, fies_data, meta_data, legend_labels = NULL) {
  items = colnames(fies_data)
  gruppi = sort(unique(meta_data[[group_var]]))
  n_gruppi = length(gruppi)
  
  preval_matrix = sapply(gruppi, function(g) {
    colMeans(fies_data[meta_data[[group_var]] == g, ], na.rm = TRUE)
  })
  
  rownames(preval_matrix) = items
  
  if (is.null(legend_labels)) {
    legend_labels = as.character(gruppi)
  } else {
    if (length(legend_labels) != n_gruppi) {
      stop("Number of lables must be equal to the number of groups.")
    }
  }
  
  par(mar = c(7, 4, 4, 2))
  barplot(t(preval_matrix),
          beside = TRUE,
          col = rainbow(n_gruppi),
          las = 2,
          ylim = c(0, 1),
          main = paste("Prevalence Rate per item and group -", group_name),
          ylab = "Prevalence rate",
          legend.text = legend_labels,
          args.legend = list(x = "topright", bty = "n", cex = 0.8))
}

# Prevalence rates Gender
barplot_prev(
  "gender", "Gender", data_fies, mydata_complete,
  legend_labels = c("Male", "Female")
)

# Prevalence rates Area of residence
barplot_prev(
  "urban", "Area", data_fies, mydata_complete,
  legend_labels = c("Rural", "Town", "City", "Capital")
)

# Prevalence rates Income 
barplot_prev(
  group_var = "income_quint",
  group_name = "Income Quintiles",
  fies_data = data_fies,
  meta_data = mydata_complete,
  legend_labels = c("Poorer", "Low Income", "Medium Income", "High Income", "Richer")
)

# Prevalence rates education level
barplot_prev(
  group_var = "educ",
  group_name = "Education Level",
  fies_data = data_fies,
  meta_data = mydata_complete,
  legend_labels = c("Low", "Medium", "High")
)

