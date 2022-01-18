
ui <- fluidPage(
  titlePanel("Student Notenvorhersage"),
  fluidRow(
    
    column(
      width=4,
      plotOutput(outputId = "Verteilung1"),
      h3(textOutput("Prognose1")),
    ),
    column(
      width=4,
      plotOutput(outputId = "Verteilung2"),
      h3(textOutput("Prognose2")),
    ),
    column(
      width=4,
      plotOutput(outputId = "Verteilung3"),
      h3(textOutput("Prognose3")),
    ),
    
    column(
      width=3,
      sliderInput(inputId = "age",
                  label = "Alter:",
                  min = 15,
                  max = 22,
                  value = 15
      ),
      
      radioButtons(inputId="sex", 
                   label="Geschlecht:",
                   choices = list("Männlich" = "M", "Weiblich" = "F")
      ),
      
      radioButtons(inputId="address", 
                   label="Wohngegend:",
                   choices = list("Städtisch" = "U", "Ländlich" = "R")
      ),
      
      sliderInput(inputId = "failures",
                  label = "Anzahl nicht bestandene Kurse:",
                  min = 0,
                  max = 3,
                  value = 0
      ),
      
      radioButtons(inputId="famsize", 
                   label="Familiengröße:",
                   choices = list("Kleinergleich 3" = "LE3", "Größer als 3" = "GT3")
      ),
      
      radioButtons(inputId="Pstatus", 
                   label="Zusammenleben der Eltern:",
                   choices = list("Leben zusammen" = "T", "Leben auseinander" = "A")
      ),
    ),
    column(
      width=3,
      selectInput(inputId="Medu", 
                  label="Bildung von Mutter", 
                  choices = list("Keine" = 0, "4th Grade" = 1, "5 - 9th Grade" = 2, "Secondary Education" = 3, "Higher Education" = 4)
      ),
      
      selectInput(inputId="Fedu", 
                  label="Bildung von Vater", 
                  choices = list("Keine" = 0, "4th Grade" = 1, "5 - 9th Grade" = 2, "Secondary Education" = 3, "Higher Education" = 4)
      ),
      
      selectInput(inputId="reason", 
                  label="Grund für Schulauswahl", 
                  choices = list("Nah von Zuhause" = "home", "Ruf der Schule" = "reputation", "Präferenz der Kurse" = "course")
      ),
      
      selectInput(inputId="guardian", 
                  label="Vormund", 
                  choices = list("Mutter" = "mother", "Vater" = "father")
      ),
      
      selectInput(inputId="traveltime", 
                  label="Schulreisezeit", 
                  choices = list("<15 min" = 1, "15 - 30 min" = 2, "30 min - 1 stunde" = 3, ">1 stunde" = 4)
      ),
      
      selectInput(inputId="studytime", 
                  label="Lernzeit", 
                  choices = list("<2 stunden" = 1, "2 - 5 stunden" = 2, "5-10 stunden" = 3, ">10 stunden" = 4)
      ),
    ),
    
    column(
      width=3,
      radioButtons(inputId="schoolsup", 
                   label="Unterstützung der Schule:",
                   choices = list("Ja" = "yes", "Nein" = "no")
      ),
      
      radioButtons(inputId="famsup", 
                   label="Unterstützung der Eltern:",
                   choices = list("Ja" = "yes", "Nein" = "no")
      ),
      
      radioButtons(inputId="activities", 
                   label="Sportliche Hobbies:",
                   choices = list("Ja" = "yes", "Nein" = "no")
      ),
      
      radioButtons(inputId="higher", 
                   label="Wunsch auf höhere Bildung:",
                   choices = list("Ja" = "yes", "Nein" = "no")
      ),
      
      radioButtons(inputId="internet", 
                   label="Hat Internet:",
                   choices = list("Ja" = "yes", "Nein" = "no")
      ),
    ),
    
    column(
      width=3,
      radioButtons(inputId="romantic", 
                   label="Ist in einer Beziehung:",
                   choices = list("Ja" = "yes", "Nein" = "no")
      ),
      
      sliderInput(inputId = "famrel",
                  label = "Qualität der familiären Beziehung von 1 - 5 (sehr niedrig zu sehr hoch):",
                  min = 1,
                  max = 5,
                  value = 1
      ),
      
      sliderInput(inputId = "freetime",
                  label = "Freizeit von 1 - 5 (sehr niedrig zu sehr hoch):",
                  min = 1,
                  max = 5,
                  value = 1
      ),
      
      sliderInput(inputId = "Dalc",
                  label = "Alkoholkonsum an Wochentagen von 1 - 5 (sehr wenig zu sehr viel):",
                  min = 1,
                  max = 5,
                  value = 1
      ),
      
      sliderInput(inputId = "Walc",
                  label = "Alkoholkonsum an Wochenden von 1 - 5 (sehr wenig zu sehr viel):",
                  min = 1,
                  max = 5,
                  value = 1
      ),
      
      sliderInput(inputId = "health",
                  label = "Gesundheit von 1 - 5 (sehr niedrig zu sehr hoch):",
                  min = 1,
                  max = 5,
                  value = 1
      ),
      sliderInput(inputId = "G1",
                  label = "Note 0 - 20 in G1:",
                  min = 0,
                  max = 20,
                  value = 10
      ),
      sliderInput(inputId = "G2",
                  label = "Note 0 - 20 in G2:",
                  min = 0,
                  max = 20,
                  value = 10
      ),
    ),
  )
)

server <- function(input, output) {
  prognose1 <- reactive({
    Daten.neu <- Daten
    #X <- Daten[,c("sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","guardian","traveltime","studytime","failures","schoolsup","famsup","paid","activities",
    #              "nursery","higher","internet","romantic","famrel","freetime","goout","Dalc","Walc","health","absences")]

    Daten.neu[1, "age"] <- input$age
    Daten.neu[1, "sex"] <- as.factor(input$sex)
    Daten.neu[1, "address"] <- as.factor(input$address)
    Daten.neu[1, "famsize"] <- input$famsize
    Daten.neu[1, "Pstatus"] <- input$Pstatus
    Daten.neu[1, "Medu"] <- as.factor(input$Medu)
    Daten.neu[1, "Fedu"] <- input$Fedu
    Daten.neu[1, "reason"] <- input$reason
    Daten.neu[1, "guardian"] <- input$guardian
    Daten.neu[1, "traveltime"] <- input$traveltime
    Daten.neu[1, "studytime"] <- input$studytime
    Daten.neu[1, "failures"] <- input$failures
    Daten.neu[1, "schoolsup"] <- input$schoolsup
    Daten.neu[1, "famsup"] <- input$famsup
    Daten.neu[1, "activities"] <- input$activities
    Daten.neu[1, "higher"] <- input$higher
    Daten.neu[1, "internet"] <- input$internet
    Daten.neu[1, "romantic"] <- input$romantic
    Daten.neu[1, "famrel"] <- input$famrel
    Daten.neu[1, "freetime"] <- input$freetime
    Daten.neu[1, "Dalc"] <- input$Dalc
    Daten.neu[1, "Walc"] <- input$Walc
    Daten.neu[1, "health"] <- input$health
    
    X.neu <- model.matrix(G1 ~ sex+age+address+famsize+Pstatus+Medu+Fedu+reason+guardian+traveltime+studytime+
                            failures+schoolsup+famsup+activities+higher+internet+romantic+famrel+freetime+Dalc+Walc+health, Daten.neu)
    X.neu <- X.neu[,-1]
    
    prognosevektor1 <- predict(model1, X.neu)$predictions
    prog1 <- prognosevektor1[1]
    prog1 <- round(prog1,digits=2)
    prog1
  })
  
  prognose2 <- reactive({
    
    Daten.neu <- Daten
    #X <- Daten[,c("sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","guardian","traveltime","studytime","failures","schoolsup","famsup","paid","activities",
    #              "nursery","higher","internet","romantic","famrel","freetime","goout","Dalc","Walc","health","absences")]
    
    Daten.neu[1, "age"] <- input$age
    Daten.neu[1, "sex"] <- as.factor(input$sex)
    Daten.neu[1, "address"] <- as.factor(input$address)
    Daten.neu[1, "famsize"] <- input$famsize
    Daten.neu[1, "Pstatus"] <- input$Pstatus
    Daten.neu[1, "Medu"] <- as.factor(input$Medu)
    Daten.neu[1, "Fedu"] <- input$Fedu
    Daten.neu[1, "reason"] <- input$reason
    Daten.neu[1, "guardian"] <- input$guardian
    Daten.neu[1, "traveltime"] <- input$traveltime
    Daten.neu[1, "studytime"] <- input$studytime
    Daten.neu[1, "failures"] <- input$failures
    Daten.neu[1, "schoolsup"] <- input$schoolsup
    Daten.neu[1, "famsup"] <- input$famsup
    Daten.neu[1, "activities"] <- input$activities
    Daten.neu[1, "higher"] <- input$higher
    Daten.neu[1, "internet"] <- input$internet
    Daten.neu[1, "romantic"] <- input$romantic
    Daten.neu[1, "famrel"] <- input$famrel
    Daten.neu[1, "freetime"] <- input$freetime
    Daten.neu[1, "Dalc"] <- input$Dalc
    Daten.neu[1, "Walc"] <- input$Walc
    Daten.neu[1, "health"] <- input$health
    Daten.neu[1, "G1"] <- input$G1
    
    X.neu <- model.matrix(G2 ~ sex+age+address+famsize+Pstatus+Medu+Fedu+reason+guardian+traveltime+studytime+
                            failures+schoolsup+famsup+activities+higher+internet+romantic+famrel+freetime+Dalc+Walc+health+G1, Daten.neu)
    X.neu <- X.neu[,-1]
    
    prognosevektor2 <- predict(model2, X.neu)$predictions
    prog2 <- prognosevektor2[1]
    prog2 <- round(prog2,digits=2)
    prog2
  })
  
  prognose3 <- reactive({
    Daten.neu <- Daten
    #X <- Daten[,c("sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","guardian","traveltime","studytime","failures","schoolsup","famsup","paid","activities",
    #              "nursery","higher","internet","romantic","famrel","freetime","goout","Dalc","Walc","health","absences")]
    
    Daten.neu[1, "age"] <- input$age
    Daten.neu[1, "sex"] <- as.factor(input$sex)
    Daten.neu[1, "address"] <- as.factor(input$address)
    Daten.neu[1, "famsize"] <- input$famsize
    Daten.neu[1, "Pstatus"] <- input$Pstatus
    Daten.neu[1, "Medu"] <- as.factor(input$Medu)
    Daten.neu[1, "Fedu"] <- input$Fedu
    Daten.neu[1, "reason"] <- input$reason
    Daten.neu[1, "guardian"] <- input$guardian
    Daten.neu[1, "traveltime"] <- input$traveltime
    Daten.neu[1, "studytime"] <- input$studytime
    Daten.neu[1, "failures"] <- input$failures
    Daten.neu[1, "schoolsup"] <- input$schoolsup
    Daten.neu[1, "famsup"] <- input$famsup
    Daten.neu[1, "activities"] <- input$activities
    Daten.neu[1, "higher"] <- input$higher
    Daten.neu[1, "internet"] <- input$internet
    Daten.neu[1, "romantic"] <- input$romantic
    Daten.neu[1, "famrel"] <- input$famrel
    Daten.neu[1, "freetime"] <- input$freetime
    Daten.neu[1, "Dalc"] <- input$Dalc
    Daten.neu[1, "Walc"] <- input$Walc
    Daten.neu[1, "health"] <- input$health
    Daten.neu[1, "G1"] <- input$G1
    Daten.neu[1, "G2"] <- input$G2
    
    X.neu <- model.matrix(G3 ~ sex+age+address+famsize+Pstatus+Medu+Fedu+reason+guardian+traveltime+studytime+
                            failures+schoolsup+famsup+activities+higher+internet+romantic+famrel+freetime+Dalc+Walc+health+G1+G2, Daten.neu)
    X.neu <- X.neu[,-1]
    
    prognosevektor3 <- predict(model3, X.neu)$predictions
    prog3 <- prognosevektor3[1]
    prog3 <- round(prog3,digits=2)
    prog3
  })
  
  output$Verteilung1 <- renderPlot({
    prog1 <- prognose1()
    X <- Daten[,c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","guardian","traveltime",
                  "studytime","failures","schoolsup","famsup","paid","activities","nursery","higher","internet","romantic","famrel",
                  "freetime","goout","Dalc","Walc","health","absences")]
    X <- model.matrix(G1 ~ sex+age+address+famsize+Pstatus+Medu+Fedu+reason+guardian+traveltime+studytime+
                            failures+schoolsup+famsup+activities+higher+internet+romantic+famrel+freetime+Dalc+Walc+health, Daten)
    X <- X[,-1]
    y <- Daten[,"G1"]
    abweichungen1 <- y-predict(model1, X)$predictions
    hist(prog1+abweichungen1, col = "blue", main = "Verteilung der Noten G1", xlim=c(0,15))
  })
  
  output$Verteilung2 <- renderPlot({
    prog2 <- prognose2()
    X <- Daten[,c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","guardian","traveltime",
                  "studytime","failures","schoolsup","famsup","paid","activities","nursery","higher","internet","romantic","famrel",
                  "freetime","goout","Dalc","Walc","health","absences","G1")]
    X <- model.matrix(G2 ~ sex+age+address+famsize+Pstatus+Medu+Fedu+reason+guardian+traveltime+studytime+
                        failures+schoolsup+famsup+activities+higher+internet+romantic+famrel+freetime+Dalc+Walc+health+G1, Daten)
    X <- X[,-1]
    y <- Daten[,"G2"]
    abweichungen2 <- y-predict(model2, X)$predictions
    hist(prog2+abweichungen2, col = "blue", main = "Verteilung der Noten G2", xlim=c(0,15))
  })
  
  output$Verteilung3 <- renderPlot({
    prog3 <- prognose3()
    X <- Daten[,c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","guardian","traveltime",
                  "studytime","failures","schoolsup","famsup","paid","activities","nursery","higher","internet","romantic","famrel",
                  "freetime","goout","Dalc","Walc","health","absences","G1","G2")]
    X <- model.matrix(G3 ~ sex+age+address+famsize+Pstatus+Medu+Fedu+reason+guardian+traveltime+studytime+
                        failures+schoolsup+famsup+activities+higher+internet+romantic+famrel+freetime+Dalc+Walc+health+G1+G2, Daten)
    X <- X[,-1]
    y <- Daten[,"G3"]
    abweichungen3 <- y-predict(model3, X)$predictions
    hist(prog3+abweichungen3, col = "blue", main = "Verteilung der Noten G3", xlim=c(0,15))
  })
  
  output$Prognose1 <- renderText({
    prog1 <- prognose1()
    Ausgabe <- paste("Durchschnittliche G1 Note: ", prog1)
  })
  output$Prognose2 <- renderText({
    prog2 <- prognose2()
    Ausgabe <- paste("Durchschnittliche G2 Note: ", prog2)
  })
  output$Prognose3 <- renderText({
    prog3 <- prognose3()
    Ausgabe <- paste("Durchschnittliche G3 Note: ", prog3)
  })
}




shinyApp(ui = ui, server = server)





