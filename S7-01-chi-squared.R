##########################################################################
# Jose Cajide - @jrcajide
# Asociación de variables categóricas en tablas de contingencia
##########################################################################


# Import the data
file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
housetasks <- read.delim(file_path, row.names = 1)

# How original data was suposed to be?
dat <- housetasks %>%
  rownames_to_column() %>% 
  pivot_longer(!rowname)

new_dat <- dat[rep.int(1:nrow(dat), dat$value), -3]
rownames(new_dat) <- 1:nrow(new_dat)
colnames(new_dat) <- c('task', 'who')
new_dat

# If there were no relationship, we would expect to see the purple bars reaching to the same length. 
# Are the differences we see here, though, just due to random noise?
new_dat %>% 
  group_by(task, who) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(proportion = n / sum(n)) %>% 
  ggplot(aes(x=task, y= proportion, fill=who)) + 
  geom_bar(position = "fill",stat = "identity") + 
  coord_flip()

tasks_table <- table(new_dat)
# Chi-square test examines whether rows and columns of a contingency table are statistically significantly associated.
# 
# Null hypothesis (H0): the row and the column variables of the contingency table are independent.
# Alternative hypothesis (H1): row and column variables are dependent

# For each cell of the table, we have to calculate the expected value under null hypothesis.

# For a given cell, the expected value is calculated as follow:

# e = ( row.sum∗col.sum ) / grand.total
tmp_table <- cbind(tasks_table, total = rowSums(tasks_table))
tmp_table <- rbind(tmp_table, total = colSums(tmp_table))

# Observed values for Breakfeast and Alternating
(observado <- tmp_table[1,1])
(esperado <- (140 * 254) / 1744)

# The Chi-square statistic is calculated as follow:
# χ2=∑(o−e)2/e
# o is the observed value
# e is the expected value

# We must repeat the same for all values in the table and sum them up

chisq <- chisq.test(tasks_table)
chisq$p.value
# the row and the column variables are statistically significantly associated (p-value = 0). 

chisq$observed
round(chisq$expected,2)

# Which cells contribute the most to the total Chi-square score?
# r=o−e/√e
(observado - esperado)/sqrt(esperado)

round(chisq$residuals, 3)

# Positive residuals: there is association
# Negative residuas: there is negative association or no association
library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)

# The relative contribution of each cell to the total Chi-square score give some indication of 
# the nature of the dependency between rows and columns of the contingency table.
# Contibution in percentage (%)
# contrib=r2/χ2

contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

# printing the p-value
chisq$p.value


