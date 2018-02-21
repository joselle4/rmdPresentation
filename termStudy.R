
# load library
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(lubridate)
library(scales)
library(formattable)
library(RColorBrewer)

analyzeCurLineTechs <- function(dt) {
  dt <- formatEmpData(dt)
  dt <- filterJobTitles(dt)
  dt <- dt[Status == "ACTIVE"]
  
  dt <- dt[Schedule == "HOURLY",
           .(SCYearsofService, YearsinJob, TERIEndDate), 
           keyby = .(SupervisorName, JobCode, FullName)]
  
  dt$SupervisorName <- factor(dt$SupervisorName, ordered = TRUE)
  dt$JobCode <- factor(dt$JobCode, ordered = TRUE)
  
  return(dt)
}

analyzeDisLineTechs <- function(dt) {
  dt <- formatEmpData(dt)
  dt <- filterDistJobTitles(dt)
  dt <- dt[Status == "ACTIVE"]
  
  dt <- dt[Schedule == "HOURLY",
           .(SCYearsofService, YearsinJob, TERIEndDate), 
           keyby = .(SupervisorName, JobCode, FullName)]
  
  dt$SupervisorName <- factor(dt$SupervisorName, ordered = TRUE)
  dt$JobCode <- factor(dt$JobCode, ordered = TRUE)
  
  return(dt)
}

termedLineTechs <- function(empData, termData) {
  emp <- formatEmpData(empData)
  emp <- filterJobTitles(emp)
  
  term <- formatTermHistData(termData)
  
  dt <- merge(emp, term, by = "EmployeeNumber")
  dt <- dt[Schedule == "HOURLY", .(EmployeeNumber, FullName.x, JobCode, ActionCode, EffectiveDate,
             LocationCode, LocationDescription, SupervisorName, PA52ReasonCode, PA52ReasonCodeDesc)]
  
  return(dt)
}

termedDisLineTechs <- function(empData, termData) {
  emp <- formatEmpData(empData)
  emp <- filterDistJobTitles(emp)
  
  term <- formatTermHistData(termData)
  
  dt <- merge(emp, term, by = "EmployeeNumber")
  dt <- dt[Schedule == "HOURLY", .(EmployeeNumber, FullName.x, JobCode, ActionCode, EffectiveDate,
                                   LocationCode, LocationDescription, SupervisorName, PA52ReasonCode, PA52ReasonCodeDesc)]
  
  return(dt)
}

termedCrewSupv <- function(empData, termData) {
  emp <- formatEmpData(empData)
  # filter job titles
  emp <- filterSupvJobTitles(emp)
  
  term <- formatTermHistData(termData)
  
  dt <- merge(emp, term, by = "EmployeeNumber")
  dt <- dt[, .(EmployeeNumber, FullName.x, JobCode, ActionCode, EffectiveDate,
               LocationCode, LocationDescription, SupervisorName, PA52ReasonCode, PA52ReasonCodeDesc)]
  
  return(dt)
}

termedSalary <- function(empData, termData) {
  emp <- formatEmpData(empData)
  emp <- filterJobTitles(emp)
  
  term <- formatTermHistData(termData)
  
  dt <- merge(emp, term, by = "EmployeeNumber")
  dt$SCYearsofService <- as.numeric(difftime(as.Date(dt$TerminationDate),as.Date(dt$HireDate)))/365.25
    
  dt <- dt[, .(EmployeeNumber, FullName.x, Schedule, JobCode, AnnualSalary, SCYearsofService, 
               ActionCode, EffectiveDate, LocationCode, LocationDescription, SupervisorName, 
               PA52ReasonCode, PA52ReasonCodeDesc, TerminationYear)]
  
  return(dt)
}

formatEmpData <- function(dt) {
  # remove spaces on colNames
  names(dt) <- gsub(" ", "", names(dt))
  
  # look-up supervisor names
  dt$SupervisorName <- dt$FullName[match(dt$SupervisorEmployeeNumber, dt$Employee)]

  # get 60000 only
  # dt <- dt[Department == "60000"]
  
  dt <- convertDates(dt)
  
  # get hire and term year
  dt$TerminationYear <- year(dt$TerminationDate)
  dt$HireYear <- year(dt$HireDate)
  
  # round numbers
  dt$SCYearsofService <- round(dt$SCYearsofService, 1)
  dt$YearsinJob <- round(dt$YearsinJob, 1)
  
  return(dt)
}

formatCompData <- function(dt) {
  # remove spaces on colNames
  names(dt) <- gsub(" ", "", names(dt))
  
  # recalc hourly rates
  dt$RecalcOldPayRate <- ifelse(dt$Schedule == "SALARY", dt$OldPayRate, dt$OldPayRate*2080)
  dt$RecalcOldPayRate <- ifelse(dt$RecalcOldPayRate < 100, dt$OldPayRate*2080, dt$RecalcOldPayRate)
  dt$RecalcNewPayRate <- ifelse(dt$Schedule == "SALARY", dt$NewPayRate, dt$NewPayRate*2080)
  dt$RecalcNewPayRate <- ifelse(dt$RecalcNewPayRate < 100, dt$NewPayRate*2080, dt$RecalcNewPayRate)
  # dt$RecalcMidpoint <- ifelse(dt$Schedule == "SALARY", dt$Midpoint, dt$Midpoint*2080)
  # dt$RecalcMidpoint <- ifelse(dt$RecalcNewPayRate < 100, dt$Midpoint*2080, dt$RecalcMidpoint)
  dt$RecalcTotalIncrease <- ifelse(dt$Schedule == "SALARY", dt$TotalIncrease, dt$TotalIncrease*2080)
  dt$RecalcTotalIncrease <- ifelse(dt$RecalcTotalIncrease < 100, dt$TotalIncrease*2080, dt$RecalcTotalIncrease)
  dt$PercentRateChange <- round((1 - dt$OldPayRate/dt$NewPayRate)*100, 2) 
  dt$RecalcPercentRateChange <- round((1 - dt$RecalcOldPayRate/dt$RecalcNewPayRate)*100, 2)
    
  dt <- convertDates(dt)
  
  # only include pay effective dates 
  dt <- dt[year(EffectiveDate) >= 2007]
  
  # add effective year
  dt$EffectiveYear <- year(dt$EffectiveDate)
  
  # filter job titles
  dt <- filterJobTitles(dt)
  
  return(dt)
}

formatTermHistData <- function(dt) {
  # remove spaces on colNames
  names(dt) <- gsub(" ", "", names(dt))
  
  dt <- convertDates(dt)

  # only include dates from 5 years ago 
  dt <- dt[year(EffectiveDate) >= 2012]
  
  # get termfin only
  dt <- dt[ActionCode == "TERM-FIN" | ActionCode == "RETIRE"]
  
  return(dt)
}

convertDates <- function(dt) {
  if("BirthDate" %in% colnames(dt)) {
    dt$BirthDate <- sapply(dt$BirthDate, function(dateStamp){
      if(grepl("/", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp, format = "%m/%d/%Y")}
      else if(grepl("-", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp , format = "%Y-%m-%d")}
    })
    dt$BirthDate <- as.Date(dt$BirthDate, origin="1970-01-01")
  }
  
  if("HireDate" %in% colnames(dt)) {
    dt$HireDate <- sapply(dt$HireDate, function(dateStamp){
      if(grepl("/", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp, format = "%m/%d/%Y")}
      else if(grepl("-", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp , format = "%Y-%m-%d")}
    })
    dt$HireDate <- as.Date(dt$HireDate, origin="1970-01-01")
  }
  
  if("TerminationDate" %in% colnames(dt)) {
    dt$TerminationDate <- sapply(dt$TerminationDate, function(dateStamp){
      if(grepl("1700-01-01", dateStamp) == TRUE) {
        dateStamp <- NA} 
      else if(grepl("/", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp, format = "%m/%d/%Y")}
      else if(grepl("-", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp , format = "%Y-%m-%d")}
    })
    dt$TerminationDate <- as.Date(dt$TerminationDate, origin="1970-01-01")
  }
  
  if("TERIEndDate" %in% colnames(dt)) {
    dt$TERIEndDate <- sapply(dt$TERIEndDate, function(dateStamp){
      if(grepl("1700-01-01", dateStamp) == TRUE | is.na(dateStamp) == TRUE) {
        dateStamp <- NA} 
      else if(grepl("/", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp, format = "%m/%d/%Y")}
      else if(grepl("-", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp , format = "%Y-%m-%d")}
    })
    dt$TERIEndDate <- as.Date(dt$TERIEndDate, origin="1970-01-01")
  }
  
  if("EffectiveDate" %in% colnames(dt)) {
    dt$EffectiveDate <- sapply(dt$EffectiveDate, function(dateStamp){
      if(grepl("/", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp, format = "%m/%d/%Y")}
      else if(grepl("-", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp , format = "%Y-%m-%d")}
    })
    dt$EffectiveDate <- as.Date(dt$EffectiveDate, origin="1970-01-01")
  }
  
  if("PayEffectiveDate" %in% colnames(dt)) {
    dt$EffectiveDate <- sapply(dt$EffectiveDate, function(dateStamp){
      if(grepl("/", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp, format = "%m/%d/%Y")}
      else if(grepl("-", dateStamp) == TRUE) {
        dateStamp <- as.Date(dateStamp , format = "%Y-%m-%d")}
    })
    dt$EffectiveDate <- as.Date(dt$EffectiveDate, origin="1970-01-01")
  }

    return(dt)
}

filterLineTechJobTitles <- function(dt) {
  dt <- dt[dt$JobTitle %in% c("LINE TECH A-TRANSMISSION", "LINE TECH B-TRANSMISSION", "LINE TECH C-TRANSMISSION")]
  
  return(dt)
}

filterLineTechDistJobTitles <- function(dt) {
  dt <- dt[dt$JobTitle %in% c("LINE TECH A-DISTRIBUTION", "LINE TECH B-DISTRIBUTION", "LINE TECH C-DISTRIBUTION")]
  
  return(dt)
}

filterJobTitles <- function(dt) {
  dt <- dt[dt$JobTitle %in% c("LINE TECH A-TRANSMISSION", "LINE TECH B-TRANSMISSION", "LINE TECH C-TRANSMISSION", 
                         "CREW SUPV-TRANSMISSION", "SUPV TRANSMISSION LINES")]
  
  return(dt)
}

filterDistJobTitles <- function(dt) {
  dt <- dt[dt$JobTitle %in% c("LINE TECH A-DISTRIBUTION", "LINE TECH B-DISTRIBUTION", "LINE TECH C-DISTRIBUTION", 
                              "CREW SUPV-DISTRIBUTION")]
  
  return(dt)
}

filterSupvJobTitles <- function(dt) {
  dt <- dt[dt$JobTitle %in% c("CREW SUPV-TRANSMISSION", "SUPV TRANSMISSION LINES")]
  
  return(dt)
}

dtActiveEmps <- function(dt, dataRange = 2012:2017) {
  # sample:
  # return(dt[(is.na(dt$TermPeriod) == TRUE | dt$TermPeriod > dt$Period) & dt$HirePeriod <= dt$Period])
  dt <- formatEmpData(dt)
  
  dtActiveEmps <- data.table()
  # get active employees 
  for(termYear in dataRange) {
    dtEmps <- dt[(is.na(dt$TerminationYear) == TRUE | dt$TerminationYear > termYear) & dt$HireYear <= termYear]
    dtEmps$Year <- termYear
    dtActiveEmps <- rbind(dtEmps, dtActiveEmps)
  }
  
  return(dtActiveEmps)
}

subHiresTermsByYear <- function(dt, sinceYear = 2012) {
  # need term count by year, active employees by year
  dtAct <- dtActiveEmps(dt)
  dtAct <- filterLineTechJobTitles(dtAct)
  dtAct <- data.table(dtAct[ , .N, keyby  = Year])
  colnames(dtAct) <- c("Year", "Count")
  dtAct$Status <- "Active"
  
  dt <- formatEmpData(dt)
  dt <- filterLineTechJobTitles(dt)
  dt <- data.table(dt[ , .N, keyby = TerminationYear])
  dt <- dt[dt$TerminationYear >= sinceYear]
  colnames(dt) <- c("Year", "Count")
  dt$Status <- "Terminated"
  
  return(rbind(dtAct, dt))
}


subHiresTermsByYearJobCode <- function(dt, sinceYear = 2012) {
  # need term count by year, active employees by year
  dtAct <- dtActiveEmps(dt)
  dtAct <- filterLineTechJobTitles(dtAct)
  dtAct <- data.table(dtAct[ , .N, keyby  = c("Year", "JobCode")])
  colnames(dtAct) <- c("Year", "JobCode", "Count")
  dtAct$Status <- "Active"
  
  dt <- formatEmpData(dt)
  dt <- filterLineTechJobTitles(dt)
  dt <- data.table(dt[ , .N, keyby = c("TerminationYear", "JobCode")])
  dt <- dt[dt$TerminationYear >= sinceYear]
  colnames(dt) <- c("Year", "JobCode", "Count")
  dt$Status <- "Terminated"
  
  return(rbind(dtAct, dt))
}


subDisHiresTermsByYear <- function(dt, sinceYear = 2012) {
  # need term count by year, active employees by year
  dtAct <- dtActiveEmps(dt)
  dtAct <- filterLineTechDistJobTitles(dtAct)
  dtAct <- data.table(dtAct[ , .N, keyby  = Year])
  colnames(dtAct) <- c("Year", "Count")
  dtAct$Status <- "Active"
  
  dt <- formatEmpData(dt)
  dt <- filterLineTechDistJobTitles(dt)
  dt <- data.table(dt[ , .N, keyby = TerminationYear])
  dt <- dt[dt$TerminationYear >= sinceYear]
  colnames(dt) <- c("Year", "Count")
  dt$Status <- "Terminated"
  
  return(rbind(dtAct, dt))
}

subTurnOverByYear <- function(dt) {
  dt <- subHiresTermsByYear(dt)
  
  dt <- data.table(dt$Year[dt$Status == "Terminated"],
                   round(100*(dt$Count[dt$Status == "Terminated"]/dt$Count[dt$Status == "Active"]), 2))
  colnames(dt) <- c("Year", "TurnOver")
  dt$Year <- as.character(dt$Year)
  dt$Text <- paste0(dt$TurnOver, "%")
  
  return(dt)
}

subDisTurnOverByYear <- function(dt) {
  dt <- subDisHiresTermsByYear(dt)
  
  dt <- data.table(dt$Year[dt$Status == "Terminated"],
                   round(100*(dt$Count[dt$Status == "Terminated"]/dt$Count[dt$Status == "Active"]), 2))
  colnames(dt) <- c("Year", "TurnOver")
  dt$Year <- as.character(dt$Year)
  dt$Text <- paste0(dt$TurnOver, "%")
  
  return(dt)
}

subLineTechCompData <- function(dt) {
  dt <- formatEmpData(dt)
  dt <- filterLineTechJobTitles(dt)
  dt <- dt[is.na(dt$TerminationYear) == TRUE]
  
  dt <- data.table(dt[ , .(.N, 
                           round(mean(AnnualSalary), 0), 
                           min(AnnualSalary), 
                           round(max(AnnualSalary), 0)), 
                       keyby = "JobCode"])
  colnames(dt) <- c("JobCode", "Count", "Avg", "Min", "Max")
  
  dt$AvgTCC <- dt$Avg + 3900
  
  dt <- melt(dt, "JobCode")
  
  return(dt)
}

subLineTechDisCompData <- function(dt) {
  dt <- formatEmpData(dt)
  dt <- filterLineTechDistJobTitles(dt)
  dt <- dt[is.na(dt$TerminationYear) == TRUE]
  
  dt <- data.table(dt[ , .(.N, 
                           round(mean(AnnualSalary), 0), 
                           min(AnnualSalary), 
                           round(max(AnnualSalary), 0)), 
                       keyby = "JobCode"])
  colnames(dt) <- c("JobCode", "Count", "Avg", "Min", "Max")
  
  dt$AvgTCC <- dt$Avg + 3900
  
  dt <- melt(dt, "JobCode")
  
  return(dt)
}

plotCurLineTechs <- function(dt) {
  maxAxis <- ifelse(max(dt$SCYearsofService) > max(dt$YearsinJob), max(dt$SCYearsofService) + 5, max(dt$YearsinJob) + 5)
  
  p1 <- plot_ly(data = dt, x = ~SupervisorName, y = ~SCYearsofService, symbol = ~JobCode,
                text = ~FullName, type = "scatter", mode = "markers", name = "Years of Service") %>%
    layout(yaxis = list(range = c(0,maxAxis), title = "Years"), margin = list(b = 160))
  p2 <- plot_ly(data = dt, x = ~SupervisorName, y = ~YearsinJob, symbol = ~JobCode,
                text = ~FullName, type = "scatter", mode = "markers", name = "Years in Position")  %>%
    layout(yaxis = list(range = c(0,maxAxis), title = "Years"), margin = list(b = 160))
  pp <- subplot(p1, p2)
  
  return(pp)
}

plotCurLineTechsOverlay <- function(dt) {
  
  pp <- plot_ly(data = dt, x = ~SupervisorName, y = ~SCYearsofService, colors = ~SupervisorName, 
                text = ~FullName, type = "scatter", mode = "markers", name = "Years of Service") %>% 
    add_trace(x = ~SupervisorName, y = ~YearsinJob, colors = ~SupervisorName, 
              text = ~FullName, type = "scatter", mode = "markers", name = "Years in Position") %>%
    layout(yaxis = list(title = "Years"), xaxis = list(title = "Supervisor", tickangle = 90), 
           margin = list(b = 160))
  
  return(pp)
}

plotTermLineTechsReasonCode <- function(dt) {
  dt <- data.table(table(dt$PA52ReasonCodeDesc))
  
  p <- plot_ly(data = dt, x = ~N, y = ~V1, colors = ~V1, type = "bar", orientation = "h") %>%
    layout(yaxis = list(title = "Temrination Reason Code"),
           xaxis = list(title = "Count"), 
           margin = list(l = 250), 
           annotations = list(xref = "x", yref = "y", x = ~N + 0.3, y = ~V1, text = ~N, showarrow = FALSE))
  
  return(p)
}

plotTermLineTechsJobCode <- function(dt) {
  dt <- subHiresTermsByYearJobCode(dt)
  
  p <- plot_ly(data = dt[Status == "Terminated"], x = ~Year, y = ~Count, color = ~JobCode, type = "bar", text = ~Count) 
  
  return(p)
}

plotTermLineTechsLocation <- function(dt) {
  dt <- dt[ , .N, c("LocationDescription")]
  
  pp <- plot_ly(data = dt, x = ~N, y = ~LocationDescription, type = "bar", 
                orientation = "h", name = "Work Location") %>%
    layout(margin = list(l = 250), 
           annotations = list(xref = "x", yref = "y", x = ~N + 0.3, y = ~LocationDescription, 
                              text = ~N, showarrow = FALSE))
  
  return(pp)
}

plotTermLineTechsSupv <- function(dt) {
  dt <- dt[is.na(SupervisorName) == FALSE, .N, c("SupervisorName")]
  
  pp <- plot_ly(data = dt, x = ~N, y = ~SupervisorName, type = "bar", 
                orientation = "h", name = "Crew Supervisor") %>%
    layout(margin = list(l = 250), 
           annotations = list(xref = "x", yref = "y", x = ~N + 0.3, y = ~SupervisorName, 
                              text = ~N, showarrow = FALSE))
  
  # pp <- subplot(p1, p2) %>% 
  #   layout(margin = list(l = 250, r = 150), legend = list(orientation = "h", align = "center"))
  
  return(pp)
}

plotTermSupvReasonCode <- function(dt) {
  dt <- data.table(table(dt$PA52ReasonCodeDesc))
  
  p <- plot_ly(data = dt, x = ~V1, y = ~N, colors = ~V1, type = "bar", width = 300) %>%
    layout(xaxis = list(title = "Termination Reason Code"),
           yaxis = list(title = "Count"), 
           annotations = list(xref = "x", yref = "y", y = ~N + 0.3, x = ~V1, text = ~N, showarrow = FALSE))
  
  return(p)
}

plotTermedSalary <- function(dt) {
  dt$Schedule <- factor(dt$Schedule, ordered = TRUE)
  
  p <- plot_ly(data = dt, x = ~JobCode, y = ~AnnualSalary, text = ~FullName.x, 
               color = ~Schedule, type = "scatter", mode = "markers")
  
  return(p)
}

plotTermedSalaryYrs <- function(dt) {
  dt$JobCode <- factor(dt$JobCode, ordered = TRUE)
  
  p <- plot_ly(data = dt, x = ~SCYearsofService, y = ~AnnualSalary, text = ~FullName.x, 
               color = ~JobCode, type = "scatter", mode = "markers", symbol = ~JobCode)
  
  return(p)
}

plotTermedByYear <- function(dt) {
  p <- plot_ly(data = dt, x = ~TerminationYear, y=~AnnualSalary, color = ~JobCode, text = ~FullName.x,
               type = "scatter", mode = "markers", symbol = ~JobCode)
  
  return(p)
}

plotCompByName <- function(dt) {
  dt <- dt[duplicated(FullName)]
  
  p <- plot_ly(data = dt, x = ~EffectiveDate, y = ~PercentRateChange, text = ~Rating, name = "Employee", 
                color = ~FullName, type = "scatter", mode = "lines+markers", alpha = 0.3) %>% 
    layout(xaxis = list(title = "Effective Date"), 
           yaxis = list(title = "Percent Rate Change"))
  
  return(p)
}

plotCompByJob <- function(dt) {
  p <- plot_ly(data = dt, x = ~EffectiveDate, y = ~PercentRateChange, text = ~Rating, name = "Job Code", 
                color = ~JobCode, type = "scatter", mode = "lines+markers", alpha = 0.3) %>% 
    layout(xaxis = list(title = "Effective Date"), 
           yaxis = list(title = "Percent Rate Change"))

  return(p)
}

plotCompByNameAndJob <- function(dt) {
  p1 <- plotCompByName(dt)
  p2 <- plotCompByJob(dt)
  pp <- subplot(p1, p2, nrows = 2) 
  
  return(pp)
}

plotCompByTotalPercMidpoint <- function(dt) {
  p <- plot_ly(data = dt, x = ~`Total%Midpoint`, y = ~PercentRateChange, text = ~Rating, 
                type = "scatter", mode = "markers", alpha = 0.3) %>% 
    layout(xaxis = list(title = "Total % Midpoint"), 
           yaxis = list(title = "Percent Rate Change"))
  
  return(p)
}

plotCompByRating <- function(dt) {
  p <- plot_ly(data = dt, x = ~Rating, y = ~PercentRateChange, text = ~Rating, 
                color = ~Rating, type = "scatter", mode = "markers", alpha = 0.3) %>% 
    layout(xaxis = list(title = "Rating"), 
           yaxis = list(title = "Percent Rate Change"))
  
  return(p)
}

plotCompByTotalPercMidpointAndRating <- function(dt) {
  p <- plot_ly(data = dt, x = ~`Total%Midpoint`, y = ~PercentRateChange, text = ~Rating, 
                color = ~Rating, type = "scatter", mode = "lines+markers", alpha = 0.3) %>% 
    layout(xaxis = list(title = "Total % Midpoint"), 
           yaxis = list(title = "Percent Rate Change"))
  
  return(p)
}

plotCompByPercMidpoint <- function(dt) {
  p1 <- plotCompByTotalPercMidpoint(dt)
  p2 <- plotCompByRating(dt)
  p3 <- plotCompByTotalPercMidpointAndRating(dt)
  pp <- subplot(p1, p2, p3, nrows = 3) 
  
  return(pp)
}

plotCompByNameAndYear <- function(dt) {
  p <- plot_ly(data = dt, x = ~EffectiveYear, y = ~PercentRateChange, text = ~FullName, 
               color = ~Rating, type = "scatter", mode = "markers", alpha = 0.5, symbol = ~Rating)
  
  return(p)
}

plotHireTermByYear <- function(dt) {
  dt <- subHiresTermsByYear(dt)
    
  p <- plot_ly(data = dt[dt$Status == "Active"], x = ~Year, y = ~Count, color = ~Status, type = "bar") %>%
    layout(annotations = list(xref = "x", yref = "x", x = ~Year - .2, y = ~Count + 2, 
                              text = ~Count, showarrow = FALSE)) %>%
    add_trace(data = dt[dt$Status == "Terminated"], x = ~Year, y = ~Count, color = ~Status, type = "bar") %>%
    layout(annotations = list(xref = "x", yref = "x", x = ~Year + .2, y = ~Count + 2, 
                              text = ~Count, showarrow = FALSE)) 
  
  return(p)
}

plotDisHireTermByYear <- function(dt) {
  dt <- subDisHiresTermsByYear(dt)
  
  p <- plot_ly(data = dt[dt$Status == "Active"], x = ~Year, y = ~Count, color = ~Status, type = "bar") %>%
    layout(annotations = list(xref = "x", yref = "x", x = ~Year - .2, y = ~Count + 2, 
                              text = ~Count, showarrow = FALSE)) %>%
    add_trace(data = dt[dt$Status == "Terminated"], x = ~Year, y = ~Count, color = ~Status, type = "bar") %>%
    layout(annotations = list(xref = "x", yref = "x", x = ~Year + .2, y = ~Count + 2, 
                              text = ~Count, showarrow = FALSE)) 
  
  return(p)
}

plotTurnOver <- function(dt) {
  dt <- subTurnOverByYear(dt)
  
  p1 <- plot_ly(dt, x = ~Year, y = ~TurnOver, type = "scatter", mode = "markers", 
                text = ~Text, name = "TurnOver Rate") %>% 
    layout(yaxis = list(range = c(0,30)),
           annotations = list(xref = "x", yref = "y", x = ~Year, y = ~TurnOver, 
                              text = ~Text, showarrow = FALSE))
  
  dt <- dt[dt$Year >= 2014 & dt$Year <= 2016]
  fit <- lm(TurnOver ~ as.integer(Year), data = dt)
  
  p2 <- plot_ly(dt, x = ~Year, y = ~TurnOver, type = "scatter", mode = "markers", 
                text = ~Text, name = "TurnOver Rate") %>%
    add_trace(x = ~Year, y = fitted(fit), mode = "lines", name = "Linear Model") %>% 
    layout(yaxis = list(range = c(0,30)))
  
  p <- subplot(p1, p2, nrows = 2)
  
  return(p)
}


plotDisTurnOver <- function(dt) {
  dt <- subDisTurnOverByYear(dt)
  
  p1 <- plot_ly(dt, x = ~Year, y = ~TurnOver, type = "scatter", mode = "markers", 
                text = ~Text, name = "TUrnOver Rate") %>% 
    layout(yaxis = list(range = c(0,25)),
           annotations = list(xref = "x", yref = "y", x = ~Year, y = ~TurnOver, 
                              text = ~Text, showarrow = FALSE))
  
  dt <- dt[dt$Year >= 2014 & dt$Year <= 2016]
  fit <- lm(TurnOver ~ as.integer(Year), data = dt)
  
  p2 <- plot_ly(dt, x = ~Year, y = ~TurnOver, type = "scatter", mode = "markers", 
                text = ~Text, name = "TurnOver Rate") %>%
    add_trace(x = ~Year, y = fitted(fit), mode = "lines", name = "Linear Model") %>% 
    layout(yaxis = list(range = c(0,25)))
  
  p <- subplot(p1, p2, nrows = 2)
  
  return(p)
}

plotCurLineTechComp <- function(dt, mbp, oc) {
  sdt <- suppressWarnings(subLineTechCompData(dt))

  dt <- formatEmpData(dt)
  dt <- filterLineTechJobTitles(dt)
  dt <- dt[is.na(dt$TerminationYear) == TRUE]
  
  mcast <- dcast(mbp, JobCode + SalaryType ~ DataCut, value.var = "Salary")
  
  mbp$DataType <- paste(mbp$SalaryType, mbp$DataCut)
  mbp <- mbp[mbp$JobCode %in% c("L TECH AT", "L TECH BT", "L TECH CT")]

  p <- plot_ly(data = dt, x = ~JobCode, y = ~AnnualSalary, color =  ~JobCode,
               type = "box", boxpoints = "all", jitter = 0.3) %>%
    add_trace(data = sdt[sdt$variable == "Avg"], x = ~JobCode, y = ~value,
              color = I("black"), type = "scatter", mode = "markers", name = "Avg Salary") %>%
    add_trace(data = sdt[sdt$variable == "AvgTCC"], x = ~JobCode, y = ~value,
              color = I("blue"), type = "scatter", mode = "markers", name = "Avg TCC") %>%
    add_trace(data = mbp[mbp$DataCut == "Avg"], x = ~JobCode, y = ~Salary, text = "MBP Avg",  
              color = ~DataType, type = "scatter", mode = "markers")
    
  # p <- plot_ly(data = dt, x = ~JobCode, y = ~AnnualSalary, color =  ~JobCode,
  #                type = "box", boxpoints = "all", jitter = 0.3) %>%
  #   add_trace(data = sdt[sdt$variable == "Avg"], x = ~JobCode, y = ~value,
  #             color = I("black"), type = "scatter", mode = "markers", name = "SC Avg Salary") %>%
  #   add_trace(data = sdt[sdt$variable == "AvgTCC"], x = ~JobCode, y = ~value,
  #             color = I("blue"), type = "scatter", mode = "markers", name = "SC Avg TCC") %>%
  #   add_trace(data = mbp[mbp$DataCut == "50th"], x = ~JobCode, y = ~Salary, text = "MBP 50th",  
  #             color = ~DataType, type = "scatter", mode = "markers") %>%
  #   add_trace(data = mbp[mbp$DataCut == "Avg"], x = ~JobCode, y = ~Salary, text = "MBP Avg",  
  #             color = ~DataType, type = "scatter", mode = "markers") %>%
  #   add_trace(data = oc, x = ~JobCode, y = ~Salary, color = ~SalaryType, 
  #             type = "scatter", mode = "markers")
  
  # p <- plot_ly(data = dt, x = ~JobCode, y = ~AnnualSalary, color =  ~JobCode,
  #              type = "box", boxpoints = "all", jitter = 0.3) %>%
  #   add_trace(data = sdt[sdt$variable == "Avg"], x = ~JobCode, y = ~value,
  #             color = I("black"), type = "scatter", mode = "markers", name = "SC Avg Salary") %>%
  #   layout(annotations = list(xref = "x", yref = "y", x = ~JobCode, y = ~value,
  #                             text = ~value, xanchor = "left", showarrow = FALSE)) %>%
  #   add_trace(data = sdt[sdt$variable == "AvgTCC"], x = ~JobCode, y = ~value,
  #             color = I("blue"), type = "scatter", mode = "markers", name = "SC Avg TCC") %>%
  #   layout(annotations = list(xref = "x", yref = "y", x = ~JobCode, y = ~value,
  #                             text = ~value, xanchor = "left", showarrow = FALSE)) %>%
  #   add_trace(data = mbp[mbp$DataCut == "50th"], x = ~JobCode, y = ~Salary, text = "MBP 50th",  
  #             color = ~DataType, type = "scatter", mode = "markers") %>%
  #   layout(annotations = list(xref = "x", yref = "y", x = ~JobCode, y = ~Salary, 
  #                             text = ~Salary, xanchor = "left", showarrow = FALSE)) %>%
  #   add_trace(data = mbp[mbp$DataCut == "Avg"], x = ~JobCode, y = ~Salary, text = "MBP Avg",  
  #             color = ~DataType, type = "scatter", mode = "markers") %>%
  #   layout(annotations = list(xref = "x", yref = "y", x = ~JobCode, y = ~Salary, 
  #                             text = ~Salary, xanchor = "right", showarrow = FALSE))
  
  return(p)
}

plotDisLineTechComp <- function(dt, mbp) {
  sdt <- suppressWarnings(subLineTechDisCompData(dt))
  
  dt <- formatEmpData(dt)
  dt <- filterLineTechDistJobTitles(dt)
  dt <- dt[is.na(dt$TerminationYear) == TRUE]
  
  mcast <- dcast(mbp, JobCode + SalaryType ~ DataCut, value.var = "Salary")
  
  mbp$DataType <- paste(mbp$SalaryType, mbp$DataCut)
  mbp <- mbp[mbp$JobCode %in% c("L TECH AD", "L TECH BD", "L TECH CD")]
  
  p <- plot_ly(data = dt, x = ~JobCode, y = ~AnnualSalary, color =  ~JobCode,
               type = "box", boxpoints = "all", jitter = 0.3) %>%
    add_trace(data = sdt[sdt$variable == "Avg"], x = ~JobCode, y = ~value,
              color = I("black"), type = "scatter", mode = "markers", name = "SC Avg Salary") %>%
    add_trace(data = sdt[sdt$variable == "AvgTCC"], x = ~JobCode, y = ~value,
              color = I("blue"), type = "scatter", mode = "markers", name = "SC Avg TCC") %>%
    add_trace(data = mbp[mbp$DataCut == "50th"], x = ~JobCode, y = ~Salary, text = "MBP 50th",  
              color = ~DataType, type = "scatter", mode = "markers") %>%
    add_trace(data = mbp[mbp$DataCut == "Avg"], x = ~JobCode, y = ~Salary, text = "MBP Avg",  
              color = ~DataType, type = "scatter", mode = "markers")
  
  return(p)
}

plotTranvsDisLineTechComp <- function(dt, mbp) {
  sdt <- suppressWarnings(rbind(subLineTechDisCompData(dt), subLineTechCompData(dt)))
  
  dt <- formatEmpData(dt)
  dt <- rbind(filterLineTechDistJobTitles(dt),filterLineTechJobTitles(dt))
  dt <- dt[is.na(dt$TerminationYear) == TRUE]
  
  mcast <- dcast(mbp, JobCode + SalaryType ~ DataCut, value.var = "Salary")
  
  mbp$DataType <- paste(mbp$SalaryType, mbp$DataCut)
  
  p <- plot_ly(data = dt, x = ~JobCode, y = ~AnnualSalary, color =  ~JobCode,
               type = "box", boxpoints = "all", jitter = 0.3) %>%
    add_trace(data = sdt[sdt$variable == "Avg"], x = ~JobCode, y = ~value,
              color = I("black"), type = "scatter", mode = "markers", name = "SC Avg Salary") %>%
    add_trace(data = sdt[sdt$variable == "AvgTCC"], x = ~JobCode, y = ~value,
              color = I("blue"), type = "scatter", mode = "markers", name = "SC Avg TCC") %>%
    add_trace(data = mbp[mbp$DataCut == "50th"], x = ~JobCode, y = ~Salary, text = "MBP 50th",  
              color = ~DataType, type = "scatter", mode = "markers") %>%
    add_trace(data = mbp[mbp$DataCut == "Avg"], x = ~JobCode, y = ~Salary, text = "MBP Avg",  
              color = ~DataType, type = "scatter", mode = "markers")
  
  return(p)
}

# dt$SupervisorName <- factor(dt$SupervisorName, ordered = TRUE)
# dt$SupervisorEmployeeNumber <- factor(dt$SupervisorEmployeeNumber, ordered = TRUE)
# dt$LocationDescription <- factor(dt$LocationDescription, ordered = TRUE)
# dt$JobCode <- factor(dt$JobCode, ordered = TRUE)
# dt$PA52ReasonCode <- factor(dt$PA52ReasonCode, ordered = TRUE)
# dt$PA52ReasonCodeDesc <- factor(dt$PA52ReasonCodeDesc, ordered = TRUE)

