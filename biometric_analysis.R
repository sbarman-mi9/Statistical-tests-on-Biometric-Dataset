setwd(“~/data")
rm(list = ls())

require(plyr)
require(dplyr)
require(tidyr)
require(psych)
require(ggplot2)
require(shiny)

get.unique.vals <- function(uv.df) {
  cl <- makeCluster(2)
  registerDoParallel(cl)
  df <- foreach(col = iter(uv.df, by="column"), col.name = names(uv.df), .combine="rbind") %dopar% {
    v <- data.frame(col.name, length(unique(col)))
    names(v) <- c("Column", "Unique.values")
    v
  }
  stopCluster(cl)
  df
}

get.missing.columns <- function(df) {
  return(sapply(df, function(x) any(is.na(x)))) 
}

all.missing.columns <- function(df) {
  return(sapply(df, function(x) all(is.na(x))))
}

perc.missing.columns <- function(df) {
  m <- nrow(df)
  return(sapply(df, function(x) as.numeric(sprintf("%.2f", sum(is.na(x))/m * 100)) ))
}

do.transform.column.type <- function(df, columns, FUN = NULL) {
  if (is.null(FUN)) stop("FUN argument is null")
  as.data.frame(apply(df[, columns], 2, FUN))
}

dir()
bio_12 <- read.csv("bio12_masked.csv", header = T, stringsAsFactors = F)
bio_13 <- read.csv("bio13_masked.csv", header = T, stringsAsFactors = F)
bio_14 <- read.csv("bio14_masked.csv", header = T, stringsAsFactors = F)
bio_15 <- read.csv("bio15_masked.csv", header = T, stringsAsFactors = F)

df <- rbind(bio_12, bio_13, bio_14, bio_15)
df <- tbl_df(df)
View(df)

glimpse(df)
get.unique.vals(as.data.frame(df))
get.missing.columns(as.data.frame(df))
all.missing.columns(df)
perc.missing.columns(df)

cols.drop <- which(!names(df) %in% c("UID", "UserID", "CoFN", "CoLN", "Neck", "BFat", "NonfasBS", "HbA1C", "PSA", "Cotinine"))
df <- df %>% select(cols.drop)
View(df)

dob.year <- as.integer(format(as.Date(df$DOB, "%m/%d/%y"), "%Y"))
df$dob.year <- dob.year

df <- df[!is.na(df$dob.year), ]
df$dob.year <- sapply(df$dob.year, function(x) {
  if (x - 100 < 1900){
    return(x)
  }
  x - 100
})
df$age <- sapply(df$dob.year, function(x){
  2015 - x
})
View(df)

names(df)
unique(df$Msubs)
View(df[which(df$Msubs == ""), ])
df <- df %>% filter(Msubs != "")
engaged <- c("MovetoRR", "GraduatetoRRMonthly", "NoPCP", "MedRxMaintenance", 
             "GraduatetoRP", "MedRxActive", "GraduateRP", "Monthly", "movetorrmonthly", 
             "Targeted", "RPhDissmissalPart", "RPhDismissalMD")
participant <- c("AppealFollowUp", "Dismissed", "NotRequired", "OptOut", "Terminated", "Missed")
df$status <- sapply(df$Msubs, function(x) {
  if (x %in% engaged) { return("engaged") }
  return("participant")
})
View(df)

glimpse(df)
cols.drop <- c("DOB", "Mstat", "Msubs", "CoGroup", "HRADate", "HRAStat", "HRAConfig", "QuestC", 
               "BioC", "BioV", "dob.year")
df <- df %>% select(which(!names(df) %in% cols.drop))

df[, c(1, 20)] <- do.transform.column.type(df, c(1,20), as.factor)
df[, c(2,3,5:19)] <- do.transform.column.type(df, c(2,3,5:19), as.numeric)
glimpse(df)

df %>% select(Hei) %>% distinct() %>% View()
df$Hei <- sapply(df$Hei, function(x) {
  v <- extract_numeric(unlist(strsplit(x, "[ ]")))
  if (length(v) == 2) {
    return((v[1]*12 + v[2]) * 2.54)
  }else if(length(v) == 1) {
    return((v[1]*12) * 2.54)
  }
  NA
})
glimpse(df)
View(df)

summary(df$age)
df$age.lvl <- cut(df$age, c(19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74))
glimpse(df)

describe(df[, -c(1, 18, 19, 20, 21)])
while(T) {
  v <- readline("Enter: ")
  x <-  which(names(df.h) %in% v)
  hist(as.data.frame(df[, x])[, 1], freq = F, xlab = v, main = v, breaks = 40, col = "red")
}

names(df)
df %>% filter(!is.na(BMI)) %>% 
  ggplot(aes(x = age.lvl, y = BMI)) + geom_boxplot(stat = "boxplot") + 
  theme_grey(base_size = 15) + scale_y_continuous(breaks = round(seq(min(df$BMI, na.rm = T), max(df$BMI, na.rm = T), by = 5), 1))

df %>% filter(!is.na(A1C)) %>% 
  ggplot(aes(x = Gender, y = A1C, fill = Gender)) + geom_boxplot(stat = "boxplot") +
  theme_grey(base_size = 15) + scale_y_continuous(breaks = round(seq(min(df$A1C, na.rm = T), max(df$A1C, na.rm = T), by = 5), 1))

df %>% filter(!is.na(BMI)) %>% 
  ggplot(aes(x = status, y = BMI, fill = status)) + geom_boxplot(stat = "boxplot") + facet_grid(Gender ~ age.lvl)

df %>% filter(!is.na(BMI)) %>% 
  ggplot(aes(x = age.lvl, y = BMI, fill = Gender)) + geom_boxplot(stat = "boxplot") + 
  theme_grey(base_size = 15) + scale_y_continuous(breaks = round(seq(min(df$BMI, na.rm = T), max(df$BMI, na.rm = T), by = 5), 1))


# summary(aov(BMI ~ Gender, data = df))
# summary(aov(Hei ~ Gender, data = df))
# summary(aov(Wei ~ Gender, data = df))
# summary(aov(Wai ~ Gender, data = df))
# summary(aov(Hip ~ Gender, data = df)) #*
# summary(aov(Sys ~ Gender, data = df))
# summary(aov(Dia ~ Gender, data = df))
# summary(aov(Tcholes ~ Gender, data = df)) #*
# summary(aov(CholesR ~ Gender, data = df)) 
# summary(aov(HDL ~ Gender, data = df))
# summary(aov(LDL ~ Gender, data = df))
# summary(aov(Trig ~ Gender, data = df))
# summary(aov(FasBS ~ Gender, data = df))
# summary(aov(A1C ~ Gender, data = df))
# 
# pairwise.t.test(df$A1C, df$Gender, p.adjust = "bonferroni")
# TukeyHSD(aov(A1C ~ Gender, data = df), conf.level = 0.95)
# bmi.gender <- lm(A1C ~ Gender, data = df)
# summary(bmi.gender)
# 
# summary(aov(BMI ~ status, data = df))
# summary(aov(Hei ~ status, data = df))
# summary(aov(Wei ~ status, data = df))
# summary(aov(Wai ~ status, data = df))
# summary(aov(Hip ~ status, data = df))
# summary(aov(Sys ~ status, data = df))
# summary(aov(Dia ~ status, data = df))
# summary(aov(Tcholes ~ status, data = df))
# summary(aov(CholesR ~ status, data = df))
# summary(aov(HDL ~ status, data = df))
# summary(aov(LDL ~ status, data = df))
# summary(aov(Trig ~ status, data = df))
# summary(aov(FasBS ~ status, data = df))
# summary(aov(A1C ~ status, data = df))
# 
# pairwise.t.test(df$BMI, df$status, p.adjust = "bonferroni")
# TukeyHSD(aov(BMI ~ status, data = df), conf.level = 0.95)
# bmi.status <- lm(BMI ~ status, data = df)
# summary(bmi.status)
# 
# summary(aov(BMI ~ age.lvl, data = df))
# summary(aov(Hei ~ age.lvl, data = df))
# summary(aov(Wei ~ age.lvl, data = df))
# summary(aov(Wai ~ age.lvl, data = df))
# summary(aov(Hip ~ age.lvl, data = df))
# summary(aov(Sys ~ age.lvl, data = df))
# summary(aov(Dia ~ age.lvl, data = df))
# summary(aov(Tcholes ~ age.lvl, data = df))
# summary(aov(CholesR ~ age.lvl, data = df))
# summary(aov(HDL ~ age.lvl, data = df))
# summary(aov(LDL ~ age.lvl, data = df))
# summary(aov(Trig ~ age.lvl, data = df))
# summary(aov(FasBS ~ age.lvl, data = df))
# summary(aov(A1C ~ age.lvl, data = df))
# 
# pairwise.t.test(df$BMI, df$age.lvl, p.adjust = "bonferroni")
# TukeyHSD(aov(BMI ~ age.lvl, data = df), conf.level = 0.95)
# dfc <- within(df, age.lvl <- relevel(age.lvl, ref = "(24,29]"))
# bmi.age <- lm(BMI ~ age.lvl, data = dfc)
# summary(bmi.age)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Gender", sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "gen", label = "Select a measurement", choices = names(df)[2:17],
                    selected = "BMI"),
        numericInput(inputId = "genbreaks", label = "Enter tick breaks", value = 5)
      ),
      mainPanel(
        plotOutput(outputId = "genbox"),
        h3("ANOVA Test"),
        verbatimTextOutput("genanova"),
        h3("Pairwise Multiple T Test"),
        verbatimTextOutput("genttest"),
        h3("Tukey Test"),
        verbatimTextOutput("gentukey"),
        h3("Simple Regression"),
        verbatimTextOutput("genreg")
      )
    )),
    tabPanel("Age", verticalLayout(
      wellPanel(
          selectInput(inputId = "a", label = "Select a measurement", choices = names(df)[2:17],
                      selected = "BMI"),
          numericInput(inputId = "abreaks", label = "Enter tick breaks", value = 5)
      ),
      plotOutput(outputId = "abox"),
      h3("ANOVA Test"),
      verbatimTextOutput("aanova"),
      h3("Pairwise Multiple T Test"),
      verbatimTextOutput("attest"),
      h3("Tukey Test"),
      verbatimTextOutput("atukey"),
      h3("Simple Regression"),
      sidebarLayout(
        sidebarPanel(
          selectInput("alvl", label = "Choose a reference age group", choices = levels(df$age.lvl),
                      selected = "(19,24]")
        ),
        mainPanel(
          verbatimTextOutput("areg")
        )
      )
    )),
    tabPanel("Participation Status", sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "p", label = "Select a measurement", choices = names(df)[2:17],
                    selected = "BMI"),
        numericInput(inputId = "pbreaks", label = "Enter tick breaks", value = 5)
      ),
      mainPanel(
        plotOutput(outputId = "pbox"),
        h3("ANOVA Test"),
        verbatimTextOutput("panova"),
        h3("Pairwise Multiple T Test"),
        verbatimTextOutput("pttest"),
        h3("Tukey Test"),
        verbatimTextOutput("ptukey"),
        h3("Simple Regression"),
        verbatimTextOutput("preg")
      )
    )),
    tabPanel("Descriptive Statistics", verticalLayout(
      verbatimTextOutput("destats")
    ))
  )
)

server <- function(input, output) {
  output$genbox <- renderPlot({
    v <- which(names(df) %in% c(input$gen))
    g <- df %>% filter(!is.na(df[, v])) %>% ggplot(aes_string(x = "Gender", y = input$gen, fill = "Gender")) + geom_boxplot(stat = "boxplot") + 
      theme_grey(base_size = 15) + scale_y_continuous(breaks = round(seq(min(df[, v], na.rm = T), max(df[, v], na.rm = T), by = input$genbreaks), 1))
    print(g)
  })
  output$genanova <- renderPrint({
    v <- which(names(df) %in% c(input$gen))
    summary(aov(as.formula(paste(c(colnames(df)[v], "Gender"), collapse = " ~ ")), data = df))
  })
  output$genttest <- renderPrint({
    v <- which(names(df) %in% c(input$gen))
    pairwise.t.test(as.data.frame(df)[, v], as.data.frame(df)[, 1], p.adjust = "bonferroni")
  })
  output$gentukey <- renderPrint({
    v <- which(names(df) %in% c(input$gen))
    TukeyHSD(aov(as.formula(paste(c(colnames(df)[v], "Gender"), collapse = " ~ ")), data = df), conf.level = 0.95)
  })
  output$genreg <- renderPrint({
    v <- which(names(df) %in% c(input$gen))
    bmi.gender <- lm(as.formula(paste(c(colnames(df)[v], "Gender"), collapse = " ~ ")), data = df)
    summary(bmi.gender)
  })
  
  
  output$pbox <- renderPlot({
    v <- which(names(df) %in% c(input$p))
    g <- df %>% filter(!is.na(df[, v])) %>% ggplot(aes_string(x = "status", y = input$p, fill = "status")) + geom_boxplot(stat = "boxplot") + 
      theme_grey(base_size = 15) + scale_y_continuous(breaks = round(seq(min(df[, v], na.rm = T), max(df[, v], na.rm = T), by = input$pbreaks), 1))
    print(g)
  })
  output$panova <- renderPrint({
    v <- which(names(df) %in% c(input$p))
    summary(aov(as.formula(paste(c(colnames(df)[v], "status"), collapse = " ~ ")), data = df))
  })
  output$pttest <- renderPrint({
    v <- which(names(df) %in% c(input$p))
    pairwise.t.test(as.data.frame(df)[, v], as.data.frame(df)[, 20], p.adjust = "bonferroni")
  })
  output$ptukey <- renderPrint({
    v <- which(names(df) %in% c(input$p))
    TukeyHSD(aov(as.formula(paste(c(colnames(df)[v], "status"), collapse = " ~ ")), data = df), conf.level = 0.95)
  })
  output$preg <- renderPrint({
    v <- which(names(df) %in% c(input$p))
    bmi.part <- lm(as.formula(paste(c(colnames(df)[v], "status"), collapse = " ~ ")), data = df)
    summary(bmi.part)
  })
  
  
  output$abox <- renderPlot({
    v <- which(names(df) %in% c(input$a))
    g <- df %>% filter(!is.na(df[, v])) %>% ggplot(aes_string(x = "age.lvl", y = input$a, fill = "age.lvl")) + geom_boxplot(stat = "boxplot") + 
      theme_grey(base_size = 15) + scale_y_continuous(breaks = round(seq(min(df[, v], na.rm = T), max(df[, v], na.rm = T), by = input$abreaks), 1))
    print(g)
  })
  output$aanova <- renderPrint({
    v <- which(names(df) %in% c(input$a))
    summary(aov(as.formula(paste(c(colnames(df)[v], "age.lvl"), collapse = " ~ ")), data = df))
  })
  output$attest <- renderPrint({
    v <- which(names(df) %in% c(input$a))
    pairwise.t.test(as.data.frame(df)[, v], as.data.frame(df)[, 21], p.adjust = "bonferroni")
  })
  output$atukey <- renderPrint({
    v <- which(names(df) %in% c(input$a))
    TukeyHSD(aov(as.formula(paste(c(colnames(df)[v], "age.lvl"), collapse = " ~ ")), data = df), conf.level = 0.95)
  })
  output$areg <- renderPrint({
    v <- which(names(df) %in% c(input$a))
    dfc <- within(df, age.lvl <- relevel(age.lvl, ref = input$alvl))
    bmi.age <- lm(as.formula(paste(c(colnames(dfc)[v], "age.lvl"), collapse = " ~ ")), data = dfc)
    summary(bmi.age)
  })
  
  output$destats <- renderPrint({
    describe(df[, -c(1, 18, 19, 20, 21)])
  })
}


shinyApp(ui = ui, server = server)








