source(paste(Sys.getenv('GEN_CODE'),'/myboxplot.R',sep = ''))
source(paste(Sys.getenv('GEN_CODE'),'/myboxplot-stats.R',sep = ''))
source(paste(Sys.getenv('GEN_CODE'),'/monthlyBP.R',sep = ''))
library('xlsx')
library(ggplot2)
library(plyr)
library(reshape2)
library(RWDataPlot)
library(zoo)
library(scales)

# ****
# - can get away from needing the myboxplot files by updating to ggplot2
# - can also likely get away from xlsx package by creating a csv file in RiverWare
#   with the necessary slots I need for QA/QC

# compOldTime compares the new data over the old time range
# scenNames should be data in PreviousRun sheet first, then what you want to call
# the scenario shown by compOldTime if applicable, then the name of the new run.

# iFile should only be file names. The function will look in $NATFLOW_DIR$/results
# for the files.
# oFile should be absolute or relative path.
generateNatFlowQCFigs <- function(iFile,oFile)
{
	
	nodes = c('Paria','LittleCO', 'GrandCanyon', 'Virgin', 'Hoover', 'Davis','BillWilliams',
		'Parker', 'Imperial')
	nodes.long =  c('Paria River','Little Colorado River', 'Gains above the Grand Canyon', 
		'Virgin River', 'Gains above Hoover', 'Gains above Davis','Bill Williams River',
		'Gains above Parker', 'Gains above Imperial')
	slots = c('CoRivPowellToVirgin.PariaGains.Local.Inflow',
		'CoRivPowellToVirgin.LittleCoR.Local.Inflow','CoRivPowellToVirgin.GainsAboveGC.Local.Inflow',
		'VirginRiver.Inflow', 'CoRivVirginToMead.GainsAboveHoover.Local.Inflow',
		'CoRivMeadToMohave.GainsAboveDavis.Local.Inflow',
		'CoRivMohaveToHavasu.BillWilliamsRiver.Local.Inflow',
		'CoRivMohaveToHavasu.GainsAboveParker.Local.Inflow',
		'AboveImperialDamColoradoR.GainsOnColoRAboveImperialDam.Local.Inflow')
	slotsAnnSalt = c('Annual.Natural.Salt.PariaRiver_FWAAC',
	              'Annual.Natural.Salt.LittleCORiver_FWAAC',
	              'Annual.Natural.Salt.GainsAboveGrandCanyon_FWAAC',
	              'Annual.Natural.Salt.VirginRiver_FWAAC',
	              'Annual.Natural.Salt.GainsAboveHoover_FWAAC',
	              'Annual.Natural.Salt.GainsAboveDavis_FWAAC',
	              'Annual.Natural.Salt.BillWilliamsRiver_FWAAC',
	              'Annual.Natural.Salt.GainsAboveParker_FWAAC',
	              'Annual.Natural.Salt.GainsAboveImperial_FWAAC')
	slotsMonSalt <- c('CoRivPowellToVirgin.PariaGains.Local.Inflow.Salt.Concentration',
	          'CoRivPowellToVirgin.LittleCoR.Local.Inflow.Salt.Concentration',
	          'CoRivPowellToVirgin.GainsAboveGC.Local.Inflow.Salt.Concentration',
	          'VirginRiver.Inflow.Salt.Concentration', 
	          'CoRivVirginToMead.GainsAboveHoover.Local.Inflow.Salt.Concentration',
	          'CoRivMeadToMohave.GainsAboveDavis.Local.Inflow.Salt.Concentration',
	          'CoRivMohaveToHavasu.BillWilliamsRiver.Local.Inflow.Salt.Concentration',
	          'CoRivMohaveToHavasu.GainsAboveParker.Local.Inflow.Salt.Concentration',
	          'AboveImperialDamColoradoR.GainsOnColoRAboveImperialDam.Local.Inflow.Salt.Concentration')

	# read in 1906-1970 data
  # *** don't think I need this. Keep comparison to those flows that can change
  ##pre71 = as.matrix(read.csv('C:/alan/NaturalFlow/historical/LBIntPre1971.csv',row.names = 1))
	
	# read in the new natural flow data, and the previous update
	newFlows = read.xlsx(paste(Sys.getenv('NATFLOW_DIR'),'/results/',iFile,sep = ''), 
		sheetName = 'LatestRun')
	prevFlows = read.xlsx(paste(Sys.getenv('NATFLOW_DIR'),'/results/',iFile,sep = ''), 
		sheetName = 'PreviousRun')
  	annSaltN <- read.xlsx(paste(Sys.getenv('NATFLOW_DIR'),'/results/', iFile, sep = ''), 
                       sheetName = 'LatestRun-Annual')
	annSaltP <- read.xlsx(paste(Sys.getenv('NATFLOW_DIR'),'/results/', iFile, sep = ''), 
	                      sheetName = 'PreviousRun-Annual')
  
#Setting data types; there is prob a cleaner way to do this.  Need to no ncol for execution and didn't want
#to hardwire length
	dateCol = 1
	newFlows = read.xlsx2(paste(Sys.getenv('NATFLOW_DIR'),'/results/',iFile,sep = ''),sheetName='LatestRun', 
		colClasses = c(rep("Date",dateCol),rep("numeric",ncol(newFlows)-dateCol+1)))
	prevFlows = read.xlsx2(paste(Sys.getenv('NATFLOW_DIR'),'/results/',iFile,sep = ''),sheetName='PreviousRun', 
		colClasses = c(rep("Date",dateCol),rep("numeric",ncol(prevFlows)-dateCol+1)))
	annSaltN <- read.xlsx(paste(Sys.getenv('NATFLOW_DIR'),'/results/', iFile, sep = ''), sheetName = 'LatestRun-Annual',
		colClasses = c(rep("Date",dateCol),rep("numeric",ncol(annSaltN)-dateCol+1)))
	annSaltP <- read.xlsx(paste(Sys.getenv('NATFLOW_DIR'),'/results/', iFile, sep = ''), sheetName = 'PreviousRun-Annual',
		colClasses = c(rep("Date",dateCol),rep("numeric",ncol(annSaltP)-dateCol+1)))
  
  # format data
  # note summing the concentration slots is pointless. will drop them later
  newFlowsAnn <- RWDataPlot::sumMonth2Annual(newFlows[,2:21])
  prevFlowsAnn <- RWDataPlot::sumMonth2Annual(prevFlows[,2:21])
	newFlowsAnn <- cbind(1971:(1970+dim(newFlowsAnn)[1]),newFlowsAnn)
	prevFlowsAnn <- cbind(1971:(1970+dim(prevFlowsAnn)[1]),prevFlowsAnn)
  
  nw <- melt(newFlows, id.vars = 1, measure.vars = 2:21, value.name = 'Value')
	prv <- melt(prevFlows, id.vars = 1, measure.vars = 2:21, value.name = 'Value')
  names(nw)[1] <- 'Date'
  names(prv)[1] <- 'Date'
  nw$ModelRun <- 'NewRun'
  prv$ModelRun <- 'PrevRun'
  
  monData <- rbind(nw, prv)
  monData$Date <- zoo::as.yearmon(monData$Date)
  
  newFlowsAnn <- melt(newFlowsAnn, id.vars = 1, measure.vars = 2:21, value.name = 'Value')
	prevFlowsAnn <- melt(prevFlowsAnn, id.vars = 1, measure.vars = 2:21, value.name = 'Value')
  newFlowsAnn$ModelRun <- 'NewRun'
  prevFlowsAnn$ModelRun <- 'PrevRun'
  annData <- rbind(newFlowsAnn, prevFlowsAnn)
  names(annData)[1] <- 'Year'
  names(annData)[2] <- 'Variable'
  
  # remove the annual salinity
  annData <- annData[annData$Variable %in% slots,]
  annData$Variable <- factor(annData$Variable)
  
  
  # add FWAAC salt
	annSaltN[,1] <- 1971:(1970+dim(annSaltN)[1])
	annSaltP[,1] <- 1971:(1970+dim(annSaltP)[1])
	nw <- melt(annSaltN, id.vars = 1, measure.vars = 2:10, value.name = 'Value')
	prv <- melt(annSaltP, id.vars = 1, measure.vars = 2:10, value.name = 'Value')
	names(nw)[1] <- 'Year'
	names(prv)[1] <- 'Year'
	nw$ModelRun <- 'NewRun'
	prv$ModelRun <- 'PrevRun'
  annSalt <- rbind(nw, prv)
  
  # annual ts and annual diffs
  annSaltPlot <- ggplot(annSalt, aes(Year, Value, color = ModelRun)) + geom_line() +
                  facet_wrap(~variable)
  annFlowPlot <- ggplot(annData, aes(Year, Value, color = ModelRun)) + geom_line() +
	  facet_wrap(~Variable)
  
  monFlowPlot <- ggplot(monData[monData$variable %in% slots,], aes(Date, Value, color = ModelRun)) + 
    geom_line() + facet_wrap(~variable) + scale_x_yearmon()
  
	monSaltPlot <- ggplot(monData[monData$variable %in% slotsMonSalt,], aes(Date, Value, color = ModelRun)) + 
	  geom_line() + facet_wrap(~variable) + scale_x_yearmon()
  
  pdf(oFile, width = 11, height = 17)
  print(annFlowPlot)
	print(annSaltPlot)
	print(monFlowPlot)
	print(monSaltPlot)
  dev.off()
	
# 	# loop over each node
# 	for(i in 1:length(nodes)){
# 		j = match(slots[i],colnames(newFlows))
# 		curNew = newFlows[,j]
# 		j = match(slots[i],colnames(prevFlows))
# 		curPrev = prevFlows[,j]
# 		j = match(slots[i], colnames(pre71))
# 		curPre71 = pre71[,j]
# 		
# 		# create monthly matrix, and annual series from each dataset
# 		curNew = matrix(curNew, ncol = 12, byrow = T)
# 		curPrev = matrix(curPrev, ncol = 12, byrow = T)
# 		curPre71 = matrix(curPre71, ncol = 12, byrow = T)
# 		
# 		curNew.Ann = apply(curNew, 1, sum)
# 		curPrev.Ann = apply(curPrev, 1, sum)
# 		curPre71.Ann = apply(curPre71, 1, sum)
# 		
# 		prevYears = seq(1971,by = 1, length = nrow(curPrev))
# 		curYears = seq(1971, by = 1, length = nrow(curNew))
# 		nMonths = nrow(curPrev)*ncol(curPrev)
# 		
# 		# create the different figures
# 		# ------ ANNUAL TIME SERIES ------------------
# 		par(mai=c(.3,.75,1,.75))
# 		
# 		plot(curYears, curNew.Ann, type = 'l', col = myCols[3], ylim = range(curNew.Ann, 
# 			curPrev.Ann), main = '', ylab = '[AF]')
# 		lines(prevYears, curPrev.Ann, col = myCols[1])
# 		abline(h = mean(curPrev.Ann), col = myCols[1], lty = 2)
# 		
# 		mtext(side = 3, line = .5, 'Annual Volume')
# 		mtext(side = 3, line = 3, nodes.long[i],cex = 1.4)
# 	
# 		par(mai=c(.3,.75,.3,.75))
# 		
# 		# ------ MONTHLY DIFFERENCE SERIES -------------
# 		plot(1:nMonths, matrix(t(curNew)[1:nMonths],ncol = 1, byrow = T) - matrix(t(curPrev),ncol = 1, 
# 			byrow = T), type = 'h', main = 'Monthly Difference Between Previous Run and Current Run', ylab = '[AF]', axes = F)
# 		axis(2)
# 		box()
# 		axis(1, labels = prevYears, at = seq(12,nMonths,12))
# 		
# 		par(mai=c(.3,.75,.3,.3))
# 		
# 		# ------ MONTHLY BOXPLOT -------------
# 		X = list()
# 		if(compOldTime){
# 			X[[1]] = curPrev
# 			X[[2]] = curNew[1:length(prevYears),]
# 			X[[3]] = curNew
# 			tmpCols = myCols
# 		} else{
# 			X[[1]] = curPrev
# 			X[[2]] = curNew
# 			tmpCols = myCols[c(1,3)]
# 		}
# 		monthlyBP(X, 'Monthly Volumes', '[AF]', scenNames, T, myCols = tmpCols, xLabel = month.abb,
# 			legendAndLayout = F)
# 		par(mai=c(.3,.3,.3,.75))
# 		
# 		# ------ ANNUAL BOXPLOT -------------
# 		X = list()
# 		if(compOldTime){
# 			X[[1]] = curPrev.Ann
# 			X[[2]] = curNew.Ann[1:length(prevYears)]
# 			X[[3]] = curNew.Ann
# 			tmpCols = myCols
# 		} else{
# 			X[[1]] = curPrev.Ann
# 			X[[2]] = curNew.Ann
# 			tmpCols = myCols[c(1,3)]
# 		}
# 		myboxplot(X, main = 'Annual Volume', boxcol= tmpCols, outline = T, pch = 19, cex = .6,
# 			axes = F, medcol = tmpCols, whiskcol = tmpCols, staplecol = tmpCols, outcol = tmpCols)
# 		if(compOldTime){
# 			points(1:3, c(mean(X[[1]]), mean(X[[2]]), mean(X[[3]])), pch = 15, col = tmpCols)
# 		} else{
# 			points(1:2, c(mean(X[[1]]), mean(X[[2]])), pch = 15, col = tmpCols)
# 		}
# 		axis(2)
# 		box()
# 		
# 		par(mai=c(.3,.75,.3,.75))
# 		
# 		# ------ MONTHLY AVG -------------
# 		prevMean = apply(curPrev, 2, mean)
# 		newMean = apply(curNew, 2, mean)
# 		if(compOldTime){
# 			newMean1 = apply(curNew[1:length(prevYears),], 2, mean)
# 		} else{
# 			# only so there is valid data in the ylim call below
# 			newMean1 = newMean
# 		}
# 		
# 		plot(newMean, type = 'l', col = myCols[3], main = 'Monthly Average', xlab = '', 
# 			ylab = '[AF]', ylim = range(prevMean, newMean, newMean1), axes = F)
# 		points(prevMean, pch = 4, col = myCols[1])
# 		if(compOldTime){
# 			points(newMean1, pch = 1, col = myCols[2])
# 		}
# 		axis(2)
# 		axis(1, labels = month.abb, at = 1:12)
# 		box()
# 		
# 		# ------ LEGEND -------------
# 		
# 		par(mai=c(.1,.75,.1,.75),xpd = F)
# 		
# 		if(compOldTime){
# 			tmpCols = myCols
# 		} else{
# 			tmpCols = myCols[c(1,3)]
# 		}
# 		barplot(0,0,col = 'white',axes =F)
# 		legend('top', scenNames, fill = tmpCols, bty = 'n', ncol = 3)
# 	}

	
}

# compOldTime compares the new data over the old time range
# scenNames should be data in PreviousRun sheet first, then what you want to call
# the scenario shown by compOldTime if applicable, then the name of the new run.
generateNatSaltQCFigs <- function(iFile,oFile, scenNames, compOldTime = T)
{
	myCols = c('springgreen3','orange','steelblue')
	# first is previous, second is current through end of previous year, and 3rd is entire new 
	
	nodes = c('Paria','LittleCO', 'GrandCanyon', 'Virgin', 'Hoover', 'Davis','BillWilliams',
		'Parker', 'Imperial')
	nodes.long =  c('Paria River','Little Colorado River', 'Gains above the Grand Canyon', 
		'Virgin River', 'Gains above Hoover', 'Gains above Davis','Bill Williams River',
		'Gains above Parker', 'Gains above Imperial')
	slots = c('CoRivPowellToVirgin.PariaGains.Local.Inflow.Salt.Concentration',
		'CoRivPowellToVirgin.LittleCoR.Local.Inflow.Salt.Concentration',
		'CoRivPowellToVirgin.GainsAboveGC.Local.Inflow.Salt.Concentration',
		'VirginRiver.Inflow.Salt.Concentration', 
		'CoRivVirginToMead.GainsAboveHoover.Local.Inflow.Salt.Concentration',
		'CoRivMeadToMohave.GainsAboveDavis.Local.Inflow.Salt.Concentration',
		'CoRivMohaveToHavasu.BillWilliamsRiver.Local.Inflow.Salt.Concentration',
		'CoRivMohaveToHavasu.GainsAboveParker.Local.Inflow.Salt.Concentration',
		'AboveImperialDamColoradoR.GainsOnColoRAboveImperialDam.Local.Inflow.Salt.Concentration')
	slots.Ann = c('Annual.Natural.Salt.PariaRiver_FWAAC',
		'Annual.Natural.Salt.LittleCORiver_FWAAC',
		'Annual.Natural.Salt.GainsAboveGrandCanyon_FWAAC',
		'Annual.Natural.Salt.VirginRiver_FWAAC',
		'Annual.Natural.Salt.GainsAboveHoover_FWAAC',
		'Annual.Natural.Salt.GainsAboveDavis_FWAAC',
		'Annual.Natural.Salt.BillWilliamsRiver_FWAAC',
		'Annual.Natural.Salt.GainsAboveParker_FWAAC',
		'Annual.Natural.Salt.GainsAboveImperial_FWAAC')
	slots.flow = c('CoRivPowellToVirgin.PariaGains.Local.Inflow',
		'CoRivPowellToVirgin.LittleCoR.Local.Inflow','CoRivPowellToVirgin.GainsAboveGC.Local.Inflow',
		'VirginRiver.Inflow', 'CoRivVirginToMead.GainsAboveHoover.Local.Inflow',
		'CoRivMeadToMohave.GainsAboveDavis.Local.Inflow',
		'CoRivMohaveToHavasu.BillWilliamsRiver.Local.Inflow',
		'CoRivMohaveToHavasu.GainsAboveParker.Local.Inflow',
		'AboveImperialDamColoradoR.GainsOnColoRAboveImperialDam.Local.Inflow')
		
	# read in the new natural flow data, and the previous update
	newConc = read.xlsx(paste(Sys.getenv('NATFLOW_DIR'),'/results/',iFile,sep = ''), 
		sheetName = 'LatestRun')
	prevConc = read.xlsx(paste(Sys.getenv('NATFLOW_DIR'),'/results/',iFile,sep = ''), 
		sheetName = 'PreviousRun')
	newAnnConc = read.xlsx(paste(Sys.getenv('NATFLOW_DIR'),'/results/',iFile,sep = ''),
		sheetName = 'LatestRun-Annual')
	prevAnnConc = read.xlsx(paste(Sys.getenv('NATFLOW_DIR'),'/results/',iFile,sep = ''),
		sheetName = 'PreviousRun-Annual')

	pdf(oFile, width = 8.5, height = 11)
	
	# loop over each node
	for(i in 1:length(nodes)){
		layout(matrix(c(1,1,2,2,3,4,5,5,6,6),ncol = 2, byrow = T), widths = c(.66,.34), 
		heights = c(.19,.19,.38,.19,.05))
		
		j = match(slots[i],colnames(newConc))
		curNew = newConc[,j]
		j = match(slots[i],colnames(prevConc))
		curPrev = prevConc[,j]
		
		# create monthly matrix, and annual series from each dataset
		curNew = matrix(curNew, ncol = 12, byrow = T)
		curPrev = matrix(curPrev, ncol = 12, byrow = T)
		
		j = match(slots.Ann[i],colnames(newAnnConc))
		curNew.Ann = newAnnConc[,j]
		j = match(slots.Ann[i],colnames(prevAnnConc))
		curPrev.Ann = prevAnnConc[,j]
		
		prevYears = seq(1971,by = 1, length = nrow(curPrev))
		curYears = seq(1971, by = 1, length = nrow(curNew))
		nMonths = nrow(curPrev)*ncol(curPrev)
	
		# create the different figures
		# ------ ANNUAL TIME SERIES ------------------
		par(mai=c(.3,.75,1,.75))
		
		plot(curYears, curNew.Ann, type = 'l', col = myCols[3], ylim = range(curNew.Ann, 
			curPrev.Ann), main = '', ylab = '[mg/L]')
		lines(prevYears, curPrev.Ann, col = myCols[1])
		abline(h = mean(curPrev.Ann), col = myCols[1], lty = 2)
		
		mtext(side = 3, line = .5, 'Flow Weighted Average Annual Concentration')
		mtext(side = 3, line = 3, nodes.long[i],cex = 1.4)
	
		par(mai=c(.3,.75,.3,.75))
		
		# ------ MONTHLY DIFFERENCE SERIES -------------
		plot(1:nMonths, (matrix(t(curNew)[1:nMonths],ncol = 1, byrow = T) - matrix(t(curPrev),ncol = 1, 
			byrow = T))/matrix(t(curPrev),ncol = 1, byrow = T)*100, type = 'h', 
			main = 'Monthly Percent Difference Between Previous Run and Current Run', 
			ylab = '[%]', axes = F)
		axis(2)
		box()
		axis(1, labels = prevYears, at = seq(12,nMonths,12))
		
		par(mai=c(.3,.75,.3,.3))
		
		# ------ MONTHLY BOXPLOT -------------
		X = list()
		if(compOldTime){
			X[[1]] = curPrev
			X[[2]] = curNew[1:length(prevYears),]
			X[[3]] = curNew
			tmpCols = myCols
		} else{
			X[[1]] = curPrev
			X[[2]] = curNew
			tmpCols = myCols[c(1,3)]
		}
		monthlyBP(X, 'Monthly Concentration', '[mg/L]', scenNames, T, myCols = tmpCols, xLabel = month.abb,
			legendAndLayout = F)
		par(mai=c(.3,.3,.3,.75))
		
		# ------ ANNUAL BOXPLOT -------------
		X = list()
		if(compOldTime){
			X[[1]] = curPrev.Ann
			X[[2]] = curNew.Ann[1:length(prevYears)]
			X[[3]] = curNew.Ann
			tmpCols = myCols
		} else{
			X[[1]] = curPrev.Ann
			X[[2]] = curNew.Ann
			tmpCols = myCols[c(1,3)]
		}
		myboxplot(X, main = 'Flow Weighted Average Annual Concentration', 
			boxcol= tmpCols, outline = T, pch = 19, cex = .6, axes = F, medcol = tmpCols, 
			whiskcol = tmpCols, staplecol = tmpCols, outcol = tmpCols)
		if(compOldTime){
			points(1:3, c(mean(X[[1]]), mean(X[[2]]), mean(X[[3]])), pch = 15, col = tmpCols)
		} else{
			points(1:2, c(mean(X[[1]]), mean(X[[2]])), pch = 15, col = tmpCols)
		}
		axis(2)
		box()
		
		par(mai=c(.3,.75,.3,.75))
		
		# ------ MONTHLY AVG -------------
		prevMean = apply(curPrev, 2, mean)
		newMean = apply(curNew, 2, mean)
		if(compOldTime){
			newMean1 = apply(curNew[1:length(prevYears),], 2, mean)
		} else{
			# only so there is valid data in the ylim call below
			newMean1 = newMean
		}
		
		plot(newMean, type = 'l', col = myCols[3], main = 'Monthly Average Concentration', xlab = '', 
			ylab = '[mg/L]', ylim = range(prevMean, newMean, newMean1), axes = F)
		points(prevMean, pch = 4, col = myCols[1])
		if(compOldTime){
			points(newMean1, pch = 1, col = myCols[2])
		}
		axis(2)
		axis(1, labels = month.abb, at = 1:12)
		box()
		
		# ------ LEGEND -------------
		
		par(mai=c(.1,.75,.1,.75),xpd = F)
		
		if(compOldTime){
			tmpCols = myCols
		} else{
			tmpCols = myCols[c(1,3)]
		}
		barplot(0,0,col = 'white',axes =F)
		legend('top', scenNames, fill = tmpCols, bty = 'n', ncol = 3)
		
		# ****************************************************************
		# ------------------------ NOW WITH MASS -------------------------
		# repeat all above figures, but with mass instead of concentration
		# ----------------------------------------------------------------
		convFactor = 735.474
		
		j = match(slots[i],colnames(newConc))
		curConc = newConc[,j]
		j = match(slots[i],colnames(prevConc))
		curPrevConc = prevConc[,j]
		
		j = match(slots.flow[i],colnames(newConc))
		curFlow = newConc[,j]
		j = match(slots.flow[i],colnames(prevConc))
		curPrevFlow = prevConc[,j]
		
		curMass = curConc * curFlow / convFactor
		prevMass = curPrevConc * curPrevFlow / convFactor
		
		# create monthly matrix, and annual series from each dataset
		curNew = matrix(curMass, ncol = 12, byrow = T)
		curPrev = matrix(prevMass, ncol = 12, byrow = T)
		
		curNew.Ann = apply(curNew, 1, sum)
		curPrev.Ann = apply(curPrev, 1, sum)
		
		prevYears = seq(1971,by = 1, length = nrow(curPrev))
		curYears = seq(1971, by = 1, length = nrow(curNew))
		nMonths = nrow(curPrev)*ncol(curPrev)
		
		# create the different figures
		# ------ ANNUAL TIME SERIES ------------------
		par(mai=c(.3,.75,1,.75))
		
		plot(curYears, curNew.Ann, type = 'l', col = myCols[3], ylim = range(curNew.Ann, 
			curPrev.Ann), main = '', ylab = '[tons]')
		lines(prevYears, curPrev.Ann, col = myCols[1])
		abline(h = mean(curPrev.Ann), col = myCols[1], lty = 2)
		
		mtext(side = 3, line = .5, 'Total Annual Salinity Mass')
		mtext(side = 3, line = 3, nodes.long[i],cex = 1.4)
	
		par(mai=c(.3,.75,.3,.75))
		
		# ------ MONTHLY DIFFERENCE SERIES -------------
		plot(1:nMonths, (matrix(t(curNew)[1:nMonths],ncol = 1, byrow = T) - matrix(t(curPrev),ncol = 1, 
			byrow = T))/matrix(t(curPrev),ncol = 1, byrow = T)*100, type = 'h', 
			main = 'Monthly Percent Difference Between Previous Run and Current Run', 
			ylab = '[%]', axes = F)
		axis(2)
		box()
		axis(1, labels = prevYears, at = seq(12,nMonths,12))
		
		par(mai=c(.3,.75,.3,.3))
		
		# ------ MONTHLY BOXPLOT -------------
		X = list()
		if(compOldTime){
			X[[1]] = curPrev
			X[[2]] = curNew[1:length(prevYears),]
			X[[3]] = curNew
			tmpCols = myCols
		} else{
			X[[1]] = curPrev
			X[[2]] = curNew
			tmpCols = myCols[c(1,3)]
		}
		monthlyBP(X, 'Monthly Mass', '[tons]', scenNames, T, myCols = tmpCols, xLabel = month.abb,
			legendAndLayout = F)
		par(mai=c(.3,.3,.3,.75))
		
		# ------ ANNUAL BOXPLOT -------------
		X = list()
		if(compOldTime){
			X[[1]] = curPrev.Ann
			X[[2]] = curNew.Ann[1:length(prevYears)]
			X[[3]] = curNew.Ann
			tmpCols = myCols
		} else{
			X[[1]] = curPrev.Ann
			X[[2]] = curNew.Ann
			tmpCols = myCols[c(1,3)]
		}
		myboxplot(X, main = 'Total Annual Salinity Mass', 
			boxcol= tmpCols, outline = T, pch = 19, cex = .6, axes = F, medcol = tmpCols, 
			whiskcol = tmpCols, staplecol = tmpCols, outcol = tmpCols)
		if(compOldTime){
			points(1:3, c(mean(X[[1]]), mean(X[[2]]), mean(X[[3]])), pch = 15, col = tmpCols)
		} else{
			points(1:2, c(mean(X[[1]]), mean(X[[2]])), pch = 15, col = tmpCols)
		}
		axis(2)
		box()
		
		par(mai=c(.3,.75,.3,.75))
		
		# ------ MONTHLY AVG -------------
		prevMean = apply(curPrev, 2, mean)
		newMean = apply(curNew, 2, mean)
		if(compOldTime){
			newMean1 = apply(curNew[1:length(prevYears),], 2, mean)
		} else{
			# only so there is valid data in the ylim call below
			newMean1 = newMean
		}
		
		plot(newMean, type = 'l', col = myCols[3], main = 'Monthly Average Mass', xlab = '', 
			ylab = '[tons]', ylim = range(prevMean, newMean, newMean1), axes = F)
		points(prevMean, pch = 4, col = myCols[1])
		if(compOldTime){
			points(newMean1, pch = 1, col = myCols[2])
		}
		axis(2)
		axis(1, labels = month.abb, at = 1:12)
		box()
		
		# ------ LEGEND -------------
		
		par(mai=c(.1,.75,.1,.75),xpd = F)
		
		if(compOldTime){
			tmpCols = myCols
		} else{
			tmpCols = myCols[c(1,3)]
		}
		barplot(0,0,col = 'white',axes =F)
		legend('top', scenNames, fill = tmpCols, bty = 'n', ncol = 3)
		
		# ---------------------------------------------------------------------
		# --------------- MONTHLY PLOTS OF CONCENTRATION AND MASS -------------
		# ---------------------------------------------------------------------

		layout(matrix(c(1,2,3),ncol = 1, byrow = T),heights = c(.33,.33,.34))
		par(mai=c(1,1,1,1))
		nMonths = length(curConc)
		plot(1:nMonths, curConc, type = 'l', col = myCols[3], xlab = '', ylab = '[mg/L]',
			axes = F, main = 'Monthly Concentration')
		axis(2)
		box()
		axis(1, labels = curYears, at = seq(12,nMonths,12))
		plot(1:nMonths, curMass, type = 'l', col = myCols[3], xlab = '', ylab = '[tons]',
			axes = F, main = 'Monthly Mass')
		axis(2)
		box()
		axis(1, labels = curYears, at = seq(12,nMonths,12))
		par(mai=c(.1,.75,.1,.75),xpd = F)
		barplot(0,0,col = 'white',axes =F)
		legend('top', scenNames[3], fill = myCols[3], bty = 'n', ncol = 3)
	}

	
	
	dev.off()
	
}
