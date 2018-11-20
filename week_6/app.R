#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("NBA MATCH"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "Team1",
                  label = "Home",
                  choices = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks",  "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "LA Clippers", "Los Angeles Lakers", "Memphis Grizzlies",  "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards")), 
      selectInput(inputId = "Team1.b",
                  label = "ROAD",
                  choices = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks",  "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "LA Clippers", "Los Angeles Lakers", "Memphis Grizzlies",  "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards"))    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      textOutput("score1"),
      
      # Output: HTML table with requested number of observations ----
      textOutput("score2")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  output$score1 <- renderPrint({ 
    df.a <- read.csv("1.csv")
    df.b <- read.csv("2.csv")
    df.c <- read.csv("3.csv")
    df.d <- read.csv("4.csv")
    dfATL <- read.csv("5.csv")
    dfBKN <- read.csv("6.csv")
    dfCAV <- read.csv("7.csv")
    dfCHA <- read.csv("8.csv")
    dfCHI <- read.csv("9.csv")
    dfCLT <- read.csv("10.csv")
    dfDAL <- read.csv("11.csv")
    dfDET <- read.csv("12.csv")
    dfDEN <- read.csv("13.csv")
    dfGSW <- read.csv("14.csv")
    dfHOU <- read.csv("15.csv")
    dfIND <- read.csv("16.csv")
    dfLAC <- read.csv("17.csv")
    dfLAL <- read.csv("18.csv")
    dfMEM <- read.csv("19.csv")
    dfMIA <- read.csv("20.csv")
    dfMIL <- read.csv("21.csv")
    dfMIN <- read.csv("22.csv")
    dfNOR <- read.csv("23.csv")
    dfNYK <- read.csv("24.csv")
    dfOKC <- read.csv("25.csv")
    dfORL <- read.csv("26.csv")
    dfPHI <- read.csv("27.csv")
    dfPHX <- read.csv("28.csv")
    dfPTL <- read.csv("29.csv")
    dfSAC <- read.csv("30.csv")
    dfSAN <- read.csv("31.csv")
    dfTOR <- read.csv("32.csv")
    dfUTA <- read.csv("33.csv")
    dfWAS <- read.csv("34.csv")
    df.a <- df.a[, -1]
    df.b <- df.b[, -1]
    df.c <- df.c[, -1]
    df.d <- df.d[, -1]
    dfATL <- dfATL[, -1]
    dfBKN <- dfBKN[, -1]
    dfCAV <- dfCAV[, -1]
    dfCHA <- dfCHA[, -1]
    dfCHI <- dfCHI[, -1]
    dfCLT <- dfCLT[, -1]
    dfDAL <- dfDAL[, -1]
    dfDET <- dfDET[, -1]
    dfDEN <- dfDEN[, -1]
    dfGSW <- dfGSW[, -1]
    dfHOU <- dfHOU[, -1]
    dfIND <- dfIND[, -1]
    dfLAC <- dfLAC[, -1]
    dfLAL <- dfLAL[, -1]
    dfMEM <- dfMEM[, -1]
    dfMIA <- dfMIA[, -1]
    dfMIL <- dfMIL[, -1]
    dfMIN <- dfMIN[, -1]
    dfNOR <- dfNOR[, -1]
    dfNYK <- dfNYK[, -1]
    dfOKC <- dfOKC[, -1]
    dfORL <- dfORL[, -1]
    dfPHI <- dfPHI[, -1]
    dfPHX <- dfPHX[, -1]
    dfPTL <- dfPTL[, -1]
    dfSAC <- dfSAC[, -1]
    dfSAN <- dfSAN[, -1]
    dfTOR <- dfTOR[, -1]
    dfUTA <- dfUTA[, -1]
    dfWAS <- dfWAS[, -1]
    Team1 <- input$Team1
    Team1.b <- input$Team1.b
    
    AVE <- subset(df.c, Team ==Team1)[, 2]
    AVE <- as.numeric(as.character(AVE))
    Teams <- list(dfATL,dfCLT,dfBKN,dfCHA,dfCHI,dfCAV,dfDAL,dfDEN,dfDET,dfGSW,dfHOU,dfIND,dfLAC,dfLAL,dfMEM,dfMIA,dfMIL,dfMIN,dfNOR,dfNYK,dfOKC,dfORL,dfPHI,dfPHX,dfPTL,dfSAC,dfSAN,dfTOR,dfUTA,dfWAS)
    names(Teams) <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks",  "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "LA Clippers", "Los Angeles Lakers", "Memphis Grizzlies",  "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards") 
    Team1opp <- Teams[[Team1]]
    
    #??Team1???^?kDataFrame
    vectorTeams1 <- as.vector(Team1opp[,"Team"])
    DATA3PT <- data.frame(matrix(NA,0,1))
    DATAFBPS <- data.frame(matrix(NA,0,1))
    DATAOFFTO <- data.frame(matrix(NA,0,1))
    DATAPITP <- data.frame(matrix(NA,0,1))
    DATAPTSDIF <- data.frame(matrix(NA,0,1))
    
    for (i in vectorTeams1){
      Team2 = i
      #?ۨ??T?��y??+?????Q???T?��y???ƶq?M?????T?��y??+?ۨ??Q???T?��y?ƶq???t
      my3PM <- subset(df.b, Team == Team1)[, 10]
      opp3PT <- subset(df.b, Team == Team2)[, 11]
      my3PT <- subset(df.b, Team == Team1)[, 11]
      opp3PM <- subset(df.b, Team == Team2)[, 10]
      X.3PT <- (as.numeric(as.character(my3PM))+as.numeric(as.character(opp3PT)))-(as.numeric(as.character(opp3PM))+as.numeric(as.character(my3PT)))
      #?֧?
      myFBPSM <- subset(df.b, Team == Team1)[, 3]
      oppFBPS <- subset(df.b, Team == Team2)[, 6]
      myFBPS <- subset(df.b, Team == Team1)[, 6]
      oppFBPSM <- subset(df.b, Team == Team2)[, 3]
      X.FBPS <- (as.numeric(as.character(myFBPSM))+as.numeric(as.character(oppFBPS)))-(as.numeric(as.character(myFBPS))+as.numeric(as.character(oppFBPSM)))
      #???~???o??
      myOFFTOM <- subset(df.b, Team ==Team1)[, 2]
      oppOFFTO <- subset(df.b, Team ==Team2)[, 5]
      myOFFTO <-subset(df.b, Team ==Team1)[, 5]
      oppOFFTOM <- subset(df.b, Team == Team2)[, 2]
      X.OFFTO <- (as.numeric(as.character(myOFFTOM))+as.numeric(as.character(oppOFFTO)))-(as.numeric(as.character(myOFFTO))+as.numeric(as.character(oppOFFTOM)))
      #?T?ϱo??
      myPITPM <- subset(df.b, Team ==Team1)[, 4]
      oppPITP <- subset(df.b, Team ==Team2)[, 7]
      myPITP <-subset(df.b, Team ==Team1)[, 7]
      oppPITPM <- subset(df.b, Team == Team2)[, 4]
      X.PITP <- (as.numeric(as.character(myPITPM))+as.numeric(as.character(oppPITP)))-(as.numeric(as.character(myPITP))+as.numeric(as.character(oppPITPM)))
      #?s?X?Ҧ????ԲզX???????o??
      Team1opp.B <- subset(Team1opp, Team== Team2)[, 2]
      Team1opp.B <- as.numeric(as.character(Team1opp.B))
      X.PTSDIF <- Team1opp.B - AVE
      #??DATA
      DATA3PT <- rbind(DATA3PT, X.3PT)
      DATAFBPS <- rbind(DATAFBPS, X.FBPS)
      DATAOFFTO <- rbind(DATAOFFTO, X.OFFTO)
      DATAPITP <- rbind(DATAPITP, X.PITP)
      DATAPTSDIF <- rbind(DATAPTSDIF, X.PTSDIF)
    }  
    
    DATAPTSDIF <- cbind(DATAPTSDIF, DATA3PT, DATAPITP, DATAOFFTO, DATAFBPS)
    names(DATAPTSDIF)[1] <- "DIF"
    names(DATAPTSDIF)[2] <- "PT3"
    names(DATAPTSDIF)[3] <- "PITP"
    names(DATAPTSDIF)[4] <- "OFFTO"
    names(DATAPTSDIF)[5] <- "FBPS"
    
    model3PT <- lm(DIF~PT3, DATAPTSDIF)
    modelPITP <- lm(DIF~PITP, DATAPTSDIF)
    modelOFFTO <- lm(DIF~OFFTO, DATAPTSDIF)
    modelFBPS <- lm(DIF~FBPS, DATAPTSDIF)
    
    
    #???Q???Ӳv
    X.PB1 <-subset(df.d, Team ==Team1)[, 2]
    PTSBONUS1 <- as.numeric(as.character(X.PB1))*10
    X.PB2 <-subset(df.d, Team ==Team1.b)[, 2]
    PTSBONUS2 <- as.numeric(as.character(X.PB2))*10 
    
    #?D?ȳ?
    HOME <-subset(df.c, Team ==Team1)[, 4]
    HOME <- as.numeric(as.character(HOME))
    ROAD <-subset(df.c, Team ==Team1.b)[, 3]
    AVE2 <- subset(df.c, Team ==Team1.b)[, 2]
    ROAD <- as.numeric(as.character(ROAD))
    AVE2 <- as.numeric(as.character(AVE2))
    HOMEPTS <- HOME-AVE
    ROADPTS <- ROAD-AVE2
    
    #??Team2???^?kdata
    
    Team2opp <- Teams[[Team1.b]]
    vectorTeams2 <- as.vector(Team2opp[,"Team"])
    DATA3PT.b <- data.frame(matrix(NA,0,1))
    DATAFBPS.b <- data.frame(matrix(NA,0,1))
    DATAOFFTO.b <- data.frame(matrix(NA,0,1))
    DATAPITP.b <- data.frame(matrix(NA,0,1))
    DATAPTSDIF.b <- data.frame(matrix(NA,0,1))
    
    for (i in vectorTeams2){
      Team2 = i
      #?ۨ??T?��y??+?????Q???T?��y???ƶq?M?????T?��y??+?ۨ??Q???T?��y?ƶq???t
      my3PM.b <- subset(df.b, Team == Team1.b)[, 10]
      opp3PT.b <- subset(df.b, Team == Team2)[, 11]
      my3PT.b <- subset(df.b, Team == Team1.b)[, 11]
      opp3PM.b <- subset(df.b, Team == Team2)[, 10]
      X.3PT.b <- (as.numeric(as.character(my3PM.b))+as.numeric(as.character(opp3PT.b)))-(as.numeric(as.character(opp3PM.b))+as.numeric(as.character(my3PT.b)))
      #?֧?
      myFBPSM.b <- subset(df.b, Team == Team1.b)[, 3]
      oppFBPS.b <- subset(df.b, Team == Team2)[, 6]
      myFBPS.b <- subset(df.b, Team == Team1.b)[, 6]
      oppFBPSM.b <- subset(df.b, Team == Team2)[, 3]
      X.FBPS.b <- (as.numeric(as.character(myFBPSM.b))+as.numeric(as.character(oppFBPS.b)))-(as.numeric(as.character(myFBPS.b))+as.numeric(as.character(oppFBPSM.b)))
      #???~???o??
      myOFFTOM.b <- subset(df.b, Team ==Team1.b)[, 2]
      oppOFFTO.b <- subset(df.b, Team ==Team2)[, 5]
      myOFFTO.b <-subset(df.b, Team ==Team1.b)[, 5]
      oppOFFTOM.b <- subset(df.b, Team == Team2)[, 2]
      X.OFFTO.b <- (as.numeric(as.character(myOFFTOM.b))+as.numeric(as.character(oppOFFTO.b)))-(as.numeric(as.character(myOFFTO.b))+as.numeric(as.character(oppOFFTOM.b)))
      #?T?ϱo??
      myPITPM.b <- subset(df.b, Team ==Team1.b)[, 4]
      oppPITP.b <- subset(df.b, Team ==Team2)[, 7]
      myPITP.b <-subset(df.b, Team ==Team1.b)[, 7]
      oppPITPM.b <- subset(df.b, Team == Team2)[, 4]
      X.PITP.b <- (as.numeric(as.character(myPITPM.b))+as.numeric(as.character(oppPITP.b)))-(as.numeric(as.character(myPITP.b))+as.numeric(as.character(oppPITPM.b)))
      #?s?X?Ҧ????ԲզX???????o??
      Team2opp.B <- subset(Team2opp, Team== Team2)[, 2]
      Team2opp.B <- as.numeric(as.character(Team2opp.B))
      X.PTSDIF.b <- Team2opp.B - AVE2
      #??DATA
      DATA3PT.b <- rbind(DATA3PT.b, X.3PT.b)
      DATAFBPS.b <- rbind(DATAFBPS.b, X.FBPS.b)
      DATAOFFTO.b <- rbind(DATAOFFTO.b, X.OFFTO.b)
      DATAPITP.b <- rbind(DATAPITP.b, X.PITP.b)
      DATAPTSDIF.b <- rbind(DATAPTSDIF.b, X.PTSDIF.b)
    }  
    
    DATAPTSDIF.b <- cbind(DATAPTSDIF.b, DATA3PT.b, DATAPITP.b, DATAOFFTO.b, DATAFBPS.b)
    names(DATAPTSDIF.b)[1] <- "DIF"
    names(DATAPTSDIF.b)[2] <- "PT3"
    names(DATAPTSDIF.b)[3] <- "PITP"
    names(DATAPTSDIF.b)[4] <- "OFFTO"
    names(DATAPTSDIF.b)[5] <- "FBPS"
    
    model3PT.b <- lm(DIF~PT3, DATAPTSDIF.b)
    modelPITP.b <- lm(DIF~PITP, DATAPTSDIF.b)
    modelOFFTO.b <- lm(DIF~OFFTO, DATAPTSDIF.b)
    modelFBPS.b <- lm(DIF~FBPS, DATAPTSDIF.b)
    
    #???ǭ?
    #Team1(?ۤvOFR+????DFR) / 2     
    #Team2(????OFR+?ۤvDFR) / 2
    #final
    my3PM <- subset(df.b, Team == Team1)[, 10]
    opp3PT <- subset(df.b, Team == Team1.b)[, 11]
    my3PT <- subset(df.b, Team == Team1)[, 11]
    opp3PM <- subset(df.b, Team == Team1.b)[, 10]
    X.3PT <- (as.numeric(as.character(my3PM))+as.numeric(as.character(opp3PT)))-(as.numeric(as.character(opp3PM))+as.numeric(as.character(my3PT)))
    #?֧?
    myFBPSM <- subset(df.b, Team == Team1)[, 3]
    oppFBPS <- subset(df.b, Team == Team1.b)[, 6]
    myFBPS <- subset(df.b, Team == Team1)[, 6]
    oppFBPSM <- subset(df.b, Team == Team1.b)[, 3]
    X.FBPS <- (as.numeric(as.character(myFBPSM))+as.numeric(as.character(oppFBPS)))-(as.numeric(as.character(myFBPS))+as.numeric(as.character(oppFBPSM)))
    #???~???o??
    myOFFTOM <- subset(df.b, Team ==Team1)[, 2]
    oppOFFTO <- subset(df.b, Team ==Team1.b)[, 5]
    myOFFTO <-subset(df.b, Team ==Team1)[, 5]
    oppOFFTOM <- subset(df.b, Team == Team1.b)[, 2]
    X.OFFTO <- (as.numeric(as.character(myOFFTOM))+as.numeric(as.character(oppOFFTO)))-(as.numeric(as.character(myOFFTO))+as.numeric(as.character(oppOFFTOM)))
    #?T?ϱo??
    myPITPM <- subset(df.b, Team ==Team1)[, 4]
    oppPITP <- subset(df.b, Team ==Team1.b)[, 7]
    myPITP <-subset(df.b, Team ==Team1)[, 7]
    oppPITPM <- subset(df.b, Team == Team1.b)[, 4]
    X.PITP <- (as.numeric(as.character(myPITPM))+as.numeric(as.character(oppPITP)))-(as.numeric(as.character(myPITP))+as.numeric(as.character(oppPITPM)))
    
    score1 <- as.numeric(as.character(subset(df.a, Team == Team1)[, 2]))+as.numeric(as.character(subset(df.a, Team == Team1.b)[, 3]))
    score2 <- as.numeric(as.character(subset(df.a, Team == Team1.b)[, 2]))+as.numeric(as.character(subset(df.a, Team == Team1)[, 3]))
    score1 <- score1/2
    score2 <- score2/2
    score1 <- score1+X.3PT*model3PT$coefficients[2]+X.FBPS*modelFBPS$coefficients[2]+X.OFFTO*modelOFFTO$coefficients[2]+X.PITP*modelPITP$coefficients[2]+PTSBONUS1+HOMEPTS
    score2 <- score2-X.3PT*model3PT.b$coefficients[2]-X.FBPS*modelFBPS.b$coefficients[2]-X.OFFTO*modelOFFTO.b$coefficients[2]-X.PITP*modelPITP.b$coefficients[2]+PTSBONUS2+ROADPTS
    score1 <- as.numeric(score1)
    score1
  })
  
  # Generate a summary of the dataset ----
  output$score2 <- renderPrint({ 
    df.a <- read.csv("1.csv")
    df.b <- read.csv("2.csv")
    df.c <- read.csv("3.csv")
    df.d <- read.csv("4.csv")
    dfATL <- read.csv("5.csv")
    dfBKN <- read.csv("6.csv")
    dfCAV <- read.csv("7.csv")
    dfCHA <- read.csv("8.csv")
    dfCHI <- read.csv("9.csv")
    dfCLT <- read.csv("10.csv")
    dfDAL <- read.csv("11.csv")
    dfDET <- read.csv("12.csv")
    dfDEN <- read.csv("13.csv")
    dfGSW <- read.csv("14.csv")
    dfHOU <- read.csv("15.csv")
    dfIND <- read.csv("16.csv")
    dfLAC <- read.csv("17.csv")
    dfLAL <- read.csv("18.csv")
    dfMEM <- read.csv("19.csv")
    dfMIA <- read.csv("20.csv")
    dfMIL <- read.csv("21.csv")
    dfMIN <- read.csv("22.csv")
    dfNOR <- read.csv("23.csv")
    dfNYK <- read.csv("24.csv")
    dfOKC <- read.csv("25.csv")
    dfORL <- read.csv("26.csv")
    dfPHI <- read.csv("27.csv")
    dfPHX <- read.csv("28.csv")
    dfPTL <- read.csv("29.csv")
    dfSAC <- read.csv("30.csv")
    dfSAN <- read.csv("31.csv")
    dfTOR <- read.csv("32.csv")
    dfUTA <- read.csv("33.csv")
    dfWAS <- read.csv("34.csv")
    df.a <- df.a[, -1]
    df.b <- df.b[, -1]
    df.c <- df.c[, -1]
    df.d <- df.d[, -1]
    dfATL <- dfATL[, -1]
    dfBKN <- dfBKN[, -1]
    dfCAV <- dfCAV[, -1]
    dfCHA <- dfCHA[, -1]
    dfCHI <- dfCHI[, -1]
    dfCLT <- dfCLT[, -1]
    dfDAL <- dfDAL[, -1]
    dfDET <- dfDET[, -1]
    dfDEN <- dfDEN[, -1]
    dfGSW <- dfGSW[, -1]
    dfHOU <- dfHOU[, -1]
    dfIND <- dfIND[, -1]
    dfLAC <- dfLAC[, -1]
    dfLAL <- dfLAL[, -1]
    dfMEM <- dfMEM[, -1]
    dfMIA <- dfMIA[, -1]
    dfMIL <- dfMIL[, -1]
    dfMIN <- dfMIN[, -1]
    dfNOR <- dfNOR[, -1]
    dfNYK <- dfNYK[, -1]
    dfOKC <- dfOKC[, -1]
    dfORL <- dfORL[, -1]
    dfPHI <- dfPHI[, -1]
    dfPHX <- dfPHX[, -1]
    dfPTL <- dfPTL[, -1]
    dfSAC <- dfSAC[, -1]
    dfSAN <- dfSAN[, -1]
    dfTOR <- dfTOR[, -1]
    dfUTA <- dfUTA[, -1]
    dfWAS <- dfWAS[, -1]
    Team1 <- input$Team1
    Team1.b <- input$Team1.b
    
    AVE <- subset(df.c, Team ==Team1)[, 2]
    AVE <- as.numeric(as.character(AVE))
    Teams <- list(dfATL,dfCLT,dfBKN,dfCHA,dfCHI,dfCAV,dfDAL,dfDEN,dfDET,dfGSW,dfHOU,dfIND,dfLAC,dfLAL,dfMEM,dfMIA,dfMIL,dfMIN,dfNOR,dfNYK,dfOKC,dfORL,dfPHI,dfPHX,dfPTL,dfSAC,dfSAN,dfTOR,dfUTA,dfWAS)
    names(Teams) <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks",  "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "LA Clippers", "Los Angeles Lakers", "Memphis Grizzlies",  "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards") 
    Team1opp <- Teams[[Team1]]
    
    #??Team1???^?kDataFrame
    vectorTeams1 <- as.vector(Team1opp[,"Team"])
    DATA3PT <- data.frame(matrix(NA,0,1))
    DATAFBPS <- data.frame(matrix(NA,0,1))
    DATAOFFTO <- data.frame(matrix(NA,0,1))
    DATAPITP <- data.frame(matrix(NA,0,1))
    DATAPTSDIF <- data.frame(matrix(NA,0,1))
    
    for (i in vectorTeams1){
      Team2 = i
      #?ۨ??T?��y??+?????Q???T?��y???ƶq?M?????T?��y??+?ۨ??Q???T?��y?ƶq???t
      my3PM <- subset(df.b, Team == Team1)[, 10]
      opp3PT <- subset(df.b, Team == Team2)[, 11]
      my3PT <- subset(df.b, Team == Team1)[, 11]
      opp3PM <- subset(df.b, Team == Team2)[, 10]
      X.3PT <- (as.numeric(as.character(my3PM))+as.numeric(as.character(opp3PT)))-(as.numeric(as.character(opp3PM))+as.numeric(as.character(my3PT)))
      #?֧?
      myFBPSM <- subset(df.b, Team == Team1)[, 3]
      oppFBPS <- subset(df.b, Team == Team2)[, 6]
      myFBPS <- subset(df.b, Team == Team1)[, 6]
      oppFBPSM <- subset(df.b, Team == Team2)[, 3]
      X.FBPS <- (as.numeric(as.character(myFBPSM))+as.numeric(as.character(oppFBPS)))-(as.numeric(as.character(myFBPS))+as.numeric(as.character(oppFBPSM)))
      #???~???o??
      myOFFTOM <- subset(df.b, Team ==Team1)[, 2]
      oppOFFTO <- subset(df.b, Team ==Team2)[, 5]
      myOFFTO <-subset(df.b, Team ==Team1)[, 5]
      oppOFFTOM <- subset(df.b, Team == Team2)[, 2]
      X.OFFTO <- (as.numeric(as.character(myOFFTOM))+as.numeric(as.character(oppOFFTO)))-(as.numeric(as.character(myOFFTO))+as.numeric(as.character(oppOFFTOM)))
      #?T?ϱo??
      myPITPM <- subset(df.b, Team ==Team1)[, 4]
      oppPITP <- subset(df.b, Team ==Team2)[, 7]
      myPITP <-subset(df.b, Team ==Team1)[, 7]
      oppPITPM <- subset(df.b, Team == Team2)[, 4]
      X.PITP <- (as.numeric(as.character(myPITPM))+as.numeric(as.character(oppPITP)))-(as.numeric(as.character(myPITP))+as.numeric(as.character(oppPITPM)))
      #?s?X?Ҧ????ԲզX???????o??
      Team1opp.B <- subset(Team1opp, Team== Team2)[, 2]
      Team1opp.B <- as.numeric(as.character(Team1opp.B))
      X.PTSDIF <- Team1opp.B - AVE
      #??DATA
      DATA3PT <- rbind(DATA3PT, X.3PT)
      DATAFBPS <- rbind(DATAFBPS, X.FBPS)
      DATAOFFTO <- rbind(DATAOFFTO, X.OFFTO)
      DATAPITP <- rbind(DATAPITP, X.PITP)
      DATAPTSDIF <- rbind(DATAPTSDIF, X.PTSDIF)
    }  
    
    DATAPTSDIF <- cbind(DATAPTSDIF, DATA3PT, DATAPITP, DATAOFFTO, DATAFBPS)
    names(DATAPTSDIF)[1] <- "DIF"
    names(DATAPTSDIF)[2] <- "PT3"
    names(DATAPTSDIF)[3] <- "PITP"
    names(DATAPTSDIF)[4] <- "OFFTO"
    names(DATAPTSDIF)[5] <- "FBPS"
    
    model3PT <- lm(DIF~PT3, DATAPTSDIF)
    modelPITP <- lm(DIF~PITP, DATAPTSDIF)
    modelOFFTO <- lm(DIF~OFFTO, DATAPTSDIF)
    modelFBPS <- lm(DIF~FBPS, DATAPTSDIF)
    
    
    #???Q???Ӳv
    X.PB1 <-subset(df.d, Team ==Team1)[, 2]
    PTSBONUS1 <- as.numeric(as.character(X.PB1))*10
    X.PB2 <-subset(df.d, Team ==Team1.b)[, 2]
    PTSBONUS2 <- as.numeric(as.character(X.PB2))*10 
    
    #?D?ȳ?
    HOME <-subset(df.c, Team ==Team1)[, 4]
    HOME <- as.numeric(as.character(HOME))
    ROAD <-subset(df.c, Team ==Team1.b)[, 3]
    AVE2 <- subset(df.c, Team ==Team1.b)[, 2]
    ROAD <- as.numeric(as.character(ROAD))
    AVE2 <- as.numeric(as.character(AVE2))
    HOMEPTS <- HOME-AVE
    ROADPTS <- ROAD-AVE2
    
    #??Team2???^?kdata
    
    Team2opp <- Teams[[Team1.b]]
    vectorTeams2 <- as.vector(Team2opp[,"Team"])
    DATA3PT.b <- data.frame(matrix(NA,0,1))
    DATAFBPS.b <- data.frame(matrix(NA,0,1))
    DATAOFFTO.b <- data.frame(matrix(NA,0,1))
    DATAPITP.b <- data.frame(matrix(NA,0,1))
    DATAPTSDIF.b <- data.frame(matrix(NA,0,1))
    
    for (i in vectorTeams2){
      Team2 = i
      #?ۨ??T?��y??+?????Q???T?��y???ƶq?M?????T?��y??+?ۨ??Q???T?��y?ƶq???t
      my3PM.b <- subset(df.b, Team == Team1.b)[, 10]
      opp3PT.b <- subset(df.b, Team == Team2)[, 11]
      my3PT.b <- subset(df.b, Team == Team1.b)[, 11]
      opp3PM.b <- subset(df.b, Team == Team2)[, 10]
      X.3PT.b <- (as.numeric(as.character(my3PM.b))+as.numeric(as.character(opp3PT.b)))-(as.numeric(as.character(opp3PM.b))+as.numeric(as.character(my3PT.b)))
      #?֧?
      myFBPSM.b <- subset(df.b, Team == Team1.b)[, 3]
      oppFBPS.b <- subset(df.b, Team == Team2)[, 6]
      myFBPS.b <- subset(df.b, Team == Team1.b)[, 6]
      oppFBPSM.b <- subset(df.b, Team == Team2)[, 3]
      X.FBPS.b <- (as.numeric(as.character(myFBPSM.b))+as.numeric(as.character(oppFBPS.b)))-(as.numeric(as.character(myFBPS.b))+as.numeric(as.character(oppFBPSM.b)))
      #???~???o??
      myOFFTOM.b <- subset(df.b, Team ==Team1.b)[, 2]
      oppOFFTO.b <- subset(df.b, Team ==Team2)[, 5]
      myOFFTO.b <-subset(df.b, Team ==Team1.b)[, 5]
      oppOFFTOM.b <- subset(df.b, Team == Team2)[, 2]
      X.OFFTO.b <- (as.numeric(as.character(myOFFTOM.b))+as.numeric(as.character(oppOFFTO.b)))-(as.numeric(as.character(myOFFTO.b))+as.numeric(as.character(oppOFFTOM.b)))
      #?T?ϱo??
      myPITPM.b <- subset(df.b, Team ==Team1.b)[, 4]
      oppPITP.b <- subset(df.b, Team ==Team2)[, 7]
      myPITP.b <-subset(df.b, Team ==Team1.b)[, 7]
      oppPITPM.b <- subset(df.b, Team == Team2)[, 4]
      X.PITP.b <- (as.numeric(as.character(myPITPM.b))+as.numeric(as.character(oppPITP.b)))-(as.numeric(as.character(myPITP.b))+as.numeric(as.character(oppPITPM.b)))
      #?s?X?Ҧ????ԲզX???????o??
      Team2opp.B <- subset(Team2opp, Team== Team2)[, 2]
      Team2opp.B <- as.numeric(as.character(Team2opp.B))
      X.PTSDIF.b <- Team2opp.B - AVE2
      #??DATA
      DATA3PT.b <- rbind(DATA3PT.b, X.3PT.b)
      DATAFBPS.b <- rbind(DATAFBPS.b, X.FBPS.b)
      DATAOFFTO.b <- rbind(DATAOFFTO.b, X.OFFTO.b)
      DATAPITP.b <- rbind(DATAPITP.b, X.PITP.b)
      DATAPTSDIF.b <- rbind(DATAPTSDIF.b, X.PTSDIF.b)
    }  
    
    DATAPTSDIF.b <- cbind(DATAPTSDIF.b, DATA3PT.b, DATAPITP.b, DATAOFFTO.b, DATAFBPS.b)
    names(DATAPTSDIF.b)[1] <- "DIF"
    names(DATAPTSDIF.b)[2] <- "PT3"
    names(DATAPTSDIF.b)[3] <- "PITP"
    names(DATAPTSDIF.b)[4] <- "OFFTO"
    names(DATAPTSDIF.b)[5] <- "FBPS"
    
    model3PT.b <- lm(DIF~PT3, DATAPTSDIF.b)
    modelPITP.b <- lm(DIF~PITP, DATAPTSDIF.b)
    modelOFFTO.b <- lm(DIF~OFFTO, DATAPTSDIF.b)
    modelFBPS.b <- lm(DIF~FBPS, DATAPTSDIF.b)
    
    #???ǭ?
    #Team1(?ۤvOFR+????DFR) / 2     
    #Team2(????OFR+?ۤvDFR) / 2
    #final
    my3PM <- subset(df.b, Team == Team1)[, 10]
    opp3PT <- subset(df.b, Team == Team1.b)[, 11]
    my3PT <- subset(df.b, Team == Team1)[, 11]
    opp3PM <- subset(df.b, Team == Team1.b)[, 10]
    X.3PT <- (as.numeric(as.character(my3PM))+as.numeric(as.character(opp3PT)))-(as.numeric(as.character(opp3PM))+as.numeric(as.character(my3PT)))
    #?֧?
    myFBPSM <- subset(df.b, Team == Team1)[, 3]
    oppFBPS <- subset(df.b, Team == Team1.b)[, 6]
    myFBPS <- subset(df.b, Team == Team1)[, 6]
    oppFBPSM <- subset(df.b, Team == Team1.b)[, 3]
    X.FBPS <- (as.numeric(as.character(myFBPSM))+as.numeric(as.character(oppFBPS)))-(as.numeric(as.character(myFBPS))+as.numeric(as.character(oppFBPSM)))
    #???~???o??
    myOFFTOM <- subset(df.b, Team ==Team1)[, 2]
    oppOFFTO <- subset(df.b, Team ==Team1.b)[, 5]
    myOFFTO <-subset(df.b, Team ==Team1)[, 5]
    oppOFFTOM <- subset(df.b, Team == Team1.b)[, 2]
    X.OFFTO <- (as.numeric(as.character(myOFFTOM))+as.numeric(as.character(oppOFFTO)))-(as.numeric(as.character(myOFFTO))+as.numeric(as.character(oppOFFTOM)))
    #?T?ϱo??
    myPITPM <- subset(df.b, Team ==Team1)[, 4]
    oppPITP <- subset(df.b, Team ==Team1.b)[, 7]
    myPITP <-subset(df.b, Team ==Team1)[, 7]
    oppPITPM <- subset(df.b, Team == Team1.b)[, 4]
    X.PITP <- (as.numeric(as.character(myPITPM))+as.numeric(as.character(oppPITP)))-(as.numeric(as.character(myPITP))+as.numeric(as.character(oppPITPM)))
    
    score1 <- as.numeric(as.character(subset(df.a, Team == Team1)[, 2]))+as.numeric(as.character(subset(df.a, Team == Team1.b)[, 3]))
    score2 <- as.numeric(as.character(subset(df.a, Team == Team1.b)[, 2]))+as.numeric(as.character(subset(df.a, Team == Team1)[, 3]))
    score1 <- score1/2
    score2 <- score2/2
    score1 <- score1+X.3PT*model3PT$coefficients[2]+X.FBPS*modelFBPS$coefficients[2]+X.OFFTO*modelOFFTO$coefficients[2]+X.PITP*modelPITP$coefficients[2]+PTSBONUS1+HOMEPTS
    score2 <- score2-X.3PT*model3PT.b$coefficients[2]-X.FBPS*modelFBPS.b$coefficients[2]-X.OFFTO*modelOFFTO.b$coefficients[2]-X.PITP*modelPITP.b$coefficients[2]+PTSBONUS2+ROADPTS
    score2 <- as.numeric(score2)
    score2
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
