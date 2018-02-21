
# load library
library(devtools)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(anytime)
library(lubridate)
library(scales)
library(formattable)
library(RColorBrewer)
library(DT)

getData <- function() {
  
  dtCDL <- fread("data.csv")
  
  #remove spaces in col names and special chars
  names(dtCDL) <- gsub(" ", "", names(dtCDL))
  names(dtCDL) <- gsub("[^[:alnum:]]", "", names(dtCDL))

  dtCDL$Employeeno <- as.character(dtCDL$Employeeno)
  
  dtCDL$Diabetes[dtCDL$Diabetes == "YES"] <- "Diagnosed Diabetic"
  dtCDL$Diabetes[dtCDL$Diabetes == "NO"] <- "No Diabetes Diagnosis"
  
  dtCDL <- setBPStatus(dtCDL)
  dtCDL <- setHTNTreated(dtCDL)
  dtCDL <- setA1cDiabeticStatus(dtCDL)
  dtCDL <- setBMIStatus(dtCDL)
  
  return(dtCDL)
}


setBPStatus <- function(dt) {
  
  dt$BPStatus[dt$Systolic < 140 | dt$Diastolic < 90] <- "Normal"
  dt$BPStatus[(dt$Systolic >= 140 & dt$Systolic < 160) | (dt$Diastolic >= 90 & dt$Diastolic < 110)] <- "Stage 1"
  dt$BPStatus[(dt$Systolic >= 160 & dt$Systolic <= 180) | (dt$Diastolic >= 100 & dt$Diastolic <= 110)] <- "Stage 2"
  dt$BPStatus[dt$Systolic > 180 | dt$Diastolic > 110] <- "Stage 3"
  
  return(dt)
}

setHTNTreated <- function(dt) {
  
  dt$HTNTreated[dt$HTNTreatedYesNo == "YES"] <- "HTN Treatment"
  dt$HTNTreated[dt$HTNTreatedYesNo == "NO"] <- "No Treatment"
  
  return(dt)
}

setA1cDiabeticStatus <- function(dt) {
  
  dt$A1cDiabeticStatus[dt$A1c < 5.7] <- "Normal"
  dt$A1cDiabeticStatus[dt$A1c >= 5.7 & dt$A1c < 6.4] <- "Pre-Diabetic"
  dt$A1cDiabeticStatus[dt$A1c >= 6.4 & dt$A1c < 10] <- "Diabetic"
  dt$A1cDiabeticStatus[dt$A1c >= 10] <- "Disqualified"
  
  dt$A1cDiabeticStatus <- factor(dt$A1cDiabeticStatus, levels = c("Normal", "Pre-Diabetic", "Diabetic", "Disqualified"))
  
  return(dt)
}

setBMIStatus <- function(dt) {
  
  dt$BMIStatus[dt$BMI < 18.5] <- "Underweight"
  dt$BMIStatus[dt$BMI >= 18.5 & dt$BMI < 25] <- "Normal"
  dt$BMIStatus[dt$BMI >= 25 & dt$BMI < 30] <- "Overweight"
  dt$BMIStatus[dt$BMI >= 30 & dt$BMI < 35] <- "Obese"
  dt$BMIStatus[dt$BMI > 35] <- "Morbidly Obese"
  
  dt$BMIStatus <- factor(dt$BMIStatus, 
                         levels = c("Underweight", "Normal", "Overweight", "Obese", "Morbidly Obese"), 
                         ordered = TRUE)
  
  return(dt)
}

plotBP <- function(dt) {
  
  dt$Label = paste0(dt$Systolic , "/", dt$Diastolic)
  
  p <- plot_ly(dt, y = ~Systolic, x = ~Diastolic, type = "scatter", mode = "markers", marker = list(size = 8), text = ~Label,
               color = ~BPStatus, symbol = ~BPStatus, colors = c("dark green", "gold", "dark orange", "red")) %>%
    layout(shapes = list(
             list(type = "rect", fillcolor = "green", opacity = 0.2, xref = "x", yref = "y", 
                  x0 = 47, x1 = 90, y0 = 85, y1 = 140, line = list(color = "green", opacity = 0)),
             list(type = "rect", fillcolor = "yellow", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = 90, x1 = 100, y0 = 85, y1 = 160, line = list(color = "yellow", opacity = 0)),
             list(type = "rect", fillcolor = "yellow", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = 47, x1 = 90, y0 = 140, y1 = 160, line = list(color = "yellow", opacity = 0)),
             list(type = "rect", fillcolor = "orange", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = 100, x1 = 110, y0 = 85, y1 = 180, line = list(color = "orange", opacity = 0)),
             list(type = "rect", fillcolor = "orange", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = 47, x1 = 100, y0 = 160, y1 = 180, line = list(color = "orange", opacity = 0))
           ), 
           legend = list(orientation = "h", y = 1.1))
    
  return(p)
}

plotBPHTN <- function(dt) {
  
  dt$Label = paste0("BP: ", dt$Systolic , "/", dt$Diastolic)
  countHTN <- dt[dt$HTN == "YES" | dt$HTN == "NO", .N, by = "HTNTreated"]
  dt$HTNTreatedCounted[dt$HTN == "YES"] <- paste0(countHTN[1,1], ": ", countHTN[1,2], " Emps") 
  dt$HTNTreatedCounted[dt$HTN == "NO"] <- paste0(countHTN[2,1], ": ", countHTN[2,2], " Emps") 
  
  p <- plot_ly(dt, y = ~Systolic, x = ~Diastolic, type = "scatter", mode = "markers", marker = list(size = 8), 
               color = ~HTNTreatedCounted, text = ~Label) %>%
    layout(shapes = list(
             list(type = "rect", fillcolor = "green", opacity = 0.2, xref = "x", yref = "y", 
                  x0 = 47, x1 = 90, y0 = 85, y1 = 140, line = list(color = "green", opacity = 0)),
             list(type = "rect", fillcolor = "yellow", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = 90, x1 = 100, y0 = 85, y1 = 160, line = list(color = "yellow", opacity = 0)),
             list(type = "rect", fillcolor = "yellow", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = 47, x1 = 90, y0 = 140, y1 = 160, line = list(color = "yellow", opacity = 0)),
             list(type = "rect", fillcolor = "orange", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = 100, x1 = 110, y0 = 85, y1 = 180, line = list(color = "orange", opacity = 0)),
             list(type = "rect", fillcolor = "orange", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = 47, x1 = 100, y0 = 160, y1 = 180, line = list(color = "orange", opacity = 0))
           ), 
           legend = (list(orientation = "h", y = 1)))
  
  return(p)  
}

plotHTN <- function(dt) {
  
  dt <- dt[, .N, by = "HTN"]
  
  p <- plot_ly(dt[dt$HTN == "YES" | dt$HTN == "NO"], y = ~N, x = ~HTN, type = "bar", color = ~HTN, 
               text = ~N, textposition = "auto") %>%
    layout(yaxis = list(title = "Count"))
  
  return(p)
}

plotA1c <- function(dt) {
  
  xmin <- 0
  xmax <- 290
  
  p <- plot_ly(dt, y = ~A1c, x = ~Employeeno, type = "scatter", mode = "markers", marker = list(size = 8), 
               hoverinfo = "text", text = ~A1c, color = ~A1cDiabeticStatus, 
               colors = c("dark green", "gold", "dark orange", "red")) %>%
    layout(xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE),
      shapes = list(
        list(type = "rect", fillcolor = "green", opacity = 0.2, xref = "x", yref = "y", 
             x0 = xmin, x1 = xmax, y0 = 0, y1 = 5.7, line = list(color = "green", opacity = 0)),
        list(type = "rect", fillcolor = "yellow", opacity = 0.1, xref = "x", yref = "y", 
             x0 = xmin, x1 = xmax, y0 = 5.7, y1 = 6.4, line = list(color = "yellow", opacity = 0)),
        list(type = "rect", fillcolor = "orange", opacity = 0.1, xref = "x", yref = "y", 
             x0 = xmin, x1 = xmax, y0 = 6.4, y1 = 10, line = list(color = "orange", opacity = 0)),
        list(type = "rect", fillcolor = "red", opacity = 0.1, xref = "x", yref = "y", 
             x0 = xmin, x1 = xmax, y0 = 10, y1 = 12, line = list(color = "red", opacity = 0))
    ), 
    legend = list(orientation = "h", y = 0, x = 0.15))
  
  return(p)  
}

plotA1cbar <- function(dt) {
  dt <- dt[ , .N, keyby = c("A1cDiabeticStatus", "Diabetes")]
  dt <- dt[is.na(A1cDiabeticStatus) == FALSE]
  dt$Percent <- percent(dt$N/sum(dt$N))
  dt$Label <- paste(dt$N, "-", dt$Percent)
  
  p <- plot_ly(dt, y = ~N, x = ~A1cDiabeticStatus, type = "bar", color = ~Diabetes, text = ~N, textposition = "auto")
  
  return(p)
}

plotA1cDiabetes <- function(dt) {
  
  xmin <- 0
  xmax <- 290
  
  countDiab <- dt[dt$DiabetesTreatedYesNo == "yes" | dt$DiabetesTreatedYesNo == "no", .N, by = "Diabetes"]
  dt$DiabetesCounted[dt$DiabetesTreatedYesNo == "no"] <- paste0(countDiab[1,1], ": ", countDiab[1,2], " Emps") 
  dt$DiabetesCounted[dt$DiabetesTreatedYesNo == "yes"] <- paste0(countDiab[2,1], ": ", countDiab[2,2], " Emps") 
  
  # palette_rev <- rev(brewer.pal(9, "Set1"))
  
  p <- plot_ly(dt, y = ~A1c, x = ~Employeeno, type = "scatter", mode = "markers", marker = list(size = 8), 
               color = ~DiabetesCounted, colors = "Set1", hoverinfo = "text", text = ~A1c) %>%
    layout(xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE),
           shapes = list(
             list(type = "rect", fillcolor = "green", opacity = 0.2, xref = "x", yref = "y", 
                  x0 = xmin, x1 = xmax, y0 = 0, y1 = 5.7, line = list(color = "green", opacity = 0)),
             list(type = "rect", fillcolor = "yellow", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = xmin, x1 = xmax, y0 = 5.7, y1 = 6.4, line = list(color = "yellow", opacity = 0)),
             list(type = "rect", fillcolor = "orange", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = xmin, x1 = xmax, y0 = 6.4, y1 = 10, line = list(color = "orange", opacity = 0)),
             list(type = "rect", fillcolor = "red", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = xmin, x1 = xmax, y0 = 10, y1 = 12, line = list(color = "red", opacity = 0))
           ), 
           legend = list(orientation = "h", y = 0, x = 0.1))
  
  return(p)  
}

plotDiabetes <- function(dt) {
  
  dt$Diabetes[dt$Diabetes == "Has Diabetes"] <- "YES"
  dt$Diabetes[dt$Diabetes == "No Diabetes"] <- "NO"
  
  dt <- dt[, .N, by = "Diabetes"]
  
  p <- plot_ly(dt[dt$Diabetes == "YES" | dt$Diabetes == "NO"], y = ~N, x = ~Diabetes, type = "bar", color = ~Diabetes, 
               text = ~N, textposition = "auto") %>%
    layout(yaxis = list(title = "Count"))
  
  return(p)
}

plotVisionHearing <- function(dt) {
  
  dtVision <- dt[, .N, by = "vision"]
  colnames(dtVision) <- c("Status", "Count")
  dtVision$Type <- "Vision"
  
  dtHearing <- dt[, .N, by = "hearing"]
  colnames(dtHearing) <- c("Status", "Count")
  dtHearing$Type <- "Hearing"
  
  dt <- rbind(dtVision, dtHearing)
  
  p <- plot_ly(dt[dt$Status != "Not Tested"], y = ~Count, x = ~Type, type = "bar", color = ~Status, 
               text = ~Count, textposition = "auto") 
  
  return(p)
}

plotBMI <- function(dt) {
  
  xmin <- -10
  xmax <- 330
  
  p <- plot_ly(dt, y = ~BMI, x = ~Employeeno, type = "scatter", mode = "markers", marker = list(size = 8), 
               hoverinfo = "text", text = ~BMI, color = ~BMIStatus, 
               colors = c("blue", "dark green", "gold", "dark orange", "red")) %>%
    layout(xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE),
           shapes = list(
             list(type = "rect", fillcolor = "blue", opacity = 0.2, xref = "x", yref = "y", 
                  x0 = xmin, x1 = xmax, y0 = 0, y1 = 18.5, line = list(color = "green", opacity = 0)),
             list(type = "rect", fillcolor = "green", opacity = 0.2, xref = "x", yref = "y", 
                  x0 = xmin, x1 = xmax, y0 = 18.5, y1 = 25, line = list(color = "green", opacity = 0)),
             list(type = "rect", fillcolor = "yellow", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = xmin, x1 = xmax, y0 = 25, y1 = 30, line = list(color = "yellow", opacity = 0)),
             list(type = "rect", fillcolor = "orange", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = xmin, x1 = xmax, y0 = 30, y1 = 35, line = list(color = "orange", opacity = 0)),
             list(type = "rect", fillcolor = "red", opacity = 0.1, xref = "x", yref = "y", 
                  x0 = xmin, x1 = xmax, y0 = 35, y1 = 70, line = list(color = "red", opacity = 0))
           ))
  
  return(p)  
}

plotBMIbar <- function(dt) {
  
  dt <- dt[ , .N, by = "BMIStatus"]
  dt <- dt[is.na(BMIStatus) == FALSE]
  dt$Percent <- percent(dt$N/sum(dt$N))
  
  dt$Label = paste(dt$N, "-", dt$Percent)
  
  p <- plot_ly(dt, y = ~N, x = ~BMIStatus, type = "bar", color = ~BMIStatus, text = ~Label, textposition = "auto") %>%
    layout(yaxis = list(title = "Count"))
  
  return(p)
  
}

plotOSAbar <- function(dt) {
  
  dt <- dt[ , .N, by = "OSAYesNo"]
  
  p <- plot_ly(dt[dt$OSAYesNo != "Not Tested"], y = ~N, x =~OSAYesNo, type = "bar", 
               color = ~OSAYesNo, text = ~N, textposition = "auto") %>%
    layout(xaxis = list(title = "OSA"), yaxis = list(title = "Count"))
  
  return(p)
}

plotComorbidity <- function(dt) {
  dt <- dt[BMI > 35 & ((BPStatus == "Stage 1" | BPStatus == "Stage 2" | BPStatus == "Stage 3") | 
             (A1cDiabeticStatus == "Diabetic" | A1cDiabeticStatus == "Disqualified"))]
  
  dtBP <- dt[BPStatus != "Normal", .N, keyby = c("BMIStatus", "BPStatus")] 
  
  dtA1c <- dt[A1cDiabeticStatus != "Normal", .N, keyby = c("BMIStatus", "A1cDiabeticStatus")]

    
  p1 <- plot_ly(data = dtA1c, y = ~N, x = ~A1cDiabeticStatus, type = "bar", color = ~A1cDiabeticStatus, 
              text = ~N, textposition = "auto")  %>%
    layout(yaxis = list(title = "Count"), xaxis = list(title = "A1c Diabetic Range"))
  p2 <-plot_ly(data = dtBP, y = ~N, x = ~BPStatus, type = "bar", color = ~BPStatus, 
              colors = "Dark2", text = ~N, textposition = "auto") %>%
    layout(yaxis = list(title = ""), xaxis = list(title = "BP Range"))
  
  p <- subplot(p1, p2, titleY = TRUE, titleX = TRUE) %>%
    layout(title = "Morbidly Obese, BMI > 35: A1C and BP Range Count")
             
  return(p)  
}