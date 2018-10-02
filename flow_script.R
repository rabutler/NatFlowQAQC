library(tidyverse)
library(CRSSIO)
library(zoo)
source("stat-boxplot-custom.R")

# UI -------------
iFolder <- "C:/alan/NaturalFlow/NFSinput_2016Update/"
new_file <- "2016Data.csv"
old_file <- "2015Data.csv"
oFile <- "LB_NF_2015_vs_2016.pdf"

# setup -------------
lb_nums <- 21:29
nodes <- nf_gage_abbrv()[lb_nums]
nodes_long <- nf_gage_names()[lb_nums]
slots <- c(
  "CoRivPowellToVirgin:PariaGains.Local Inflow",	
  "CoRivPowellToVirgin:LittleCoR.Local Inflow",	
  "CoRivPowellToVirgin:GainsAboveGC.Local Inflow",
  "VirginRiver.Inflow",	
  "CoRivVirginToMead:GainsAboveHoover.Local Inflow",	
  "CoRivMeadToMohave:GainsAboveDavis.Local Inflow",	
  "CoRivMohaveToHavasu:BillWilliamsRiver.Local Inflow",	
  "CoRivMohaveToHavasu:GainsAboveParker.Local Inflow",
  "AboveImperialDamColoradoR:GainsOnColoRAboveImperialDam.Local Inflow"
)
slots <- str_replace_all(slots, ":", ".")
slots <- str_replace_all(slots, " ", ".")
names(nodes) <- slots

# this only includes lb flows
newFlows <- read.csv(file.path(iFolder, new_file))
prevFlows <- read.csv(file.path(iFolder, old_file))

# format data ----------------
format_date_and_gather <- function(zz) {
  zz %>%
    mutate(ym = zoo::as.yearmon(as.character(X))) %>%
    select(-X) %>%
    mutate(
      month = format(ym, "%B"),
      year = as.numeric(format(ym, "%Y"))
    ) %>%
    gather(Variable, Value, -ym, -month, -year) %>%
    mutate(Variable = nodes[Variable])
}

sum_annual <- function(zz) {
  zz %>%
    group_by(Scenario, Variable, year) %>%
    summarise(Value = sum(Value))
}

newFlows <- newFlows %>%
  format_date_and_gather() %>%
  mutate(Scenario = "New")
prevFlows <- prevFlows %>%
  format_date_and_gather() %>%
  mutate(Scenario = "Previous")

newFlowsAnn <- newFlows %>%
  sum_annual()
prevFlowsAnn <- prevFlows %>%
  sum_annual()

monData <- rbind(newFlows, prevFlows)

annData <- rbind(newFlowsAnn, prevFlowsAnn)

# annual ts and annual diffs
annFlowPlot <- ggplot(annData, aes(year, Value, color = Scenario)) + 
  geom_line() +
  facet_wrap(~Variable, nrow = 3, scales = "free_y")

monFlowPlot <- ggplot(monData, aes(ym, Value, color = Scenario)) + 
  geom_line() + 
  facet_wrap(~Variable, nrow = 3, scales = "free_y") + 
  scale_x_yearmon()

ann_bxp <- ggplot(annData, aes(Scenario, Value)) +
  stat_boxplot_custom() +
  facet_wrap(~Variable, nrow = 3, scales = "free_y")

monData <- mutate(monData, month = factor(month, levels = month.name))

mon_bxp <- ggplot(monData, aes(month, Value, fill = Scenario)) +
  stat_boxplot_custom() +
  facet_wrap(~Variable, nrow = 3, scales = "free_y")

pdf(file.path(iFolder, oFile), width = 17, height = 11)
print(annFlowPlot)
print(ann_bxp)
print(monFlowPlot)
print(mon_bxp)
dev.off()
