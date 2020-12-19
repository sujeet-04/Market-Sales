library(readxl)
valid <- read_excel("D:/Project/Validation Dataset.xlsx",sheet = 1)
head(valid)

##########  Load the training and testing data set  ###########

traindata <- read.csv("D:/project/New folder/Training_Dataset.csv",header = T)
str(traindata)
head(traindata)

testdata <- read.csv("D:/Project/New folder/Testing_Dataset.csv",header = T)
str(testdata)
head(testdata)


#As serial Number column is not required for analysis we should drop it from both train and test dataset.


head(traindata)
head(testdata)

# Use split package to separate out product column into salesman id and product id for both train and test dataset.
library(splitstackshape)

######  For training dataset ######

traindata2 <- cSplit(traindata,"product","-")
head(traindata2)

#Change the newly formed columns names into salesman and product .
colnames(traindata2)[5] <- "Salesman"
colnames(traindata2)[6] <- "Product"

#Change the order of columns 
traindata2 <- traindata2[,c(5,6,1:4)]
head(traindata2)

######  For testing dataset ######

testdata2 <- cSplit(testdata,"product","-")
head(testdata2)

#Change the newly formed columns names into salesman and product .
colnames(testdata2)[5] <- "Salesman"
colnames(testdata2)[6] <- "Product"

#Change the order of columns 
testdata2 <- testdata2[,c(5,6,1:4)]
head(testdata2)

# Lets add another column for the train as well test data where we denote 1 as the salesman achieved his 
# target and 0 represent he didn't.

######### For Train dataset #########
traindata3 <- traindata2
traindata3$achievement <- ifelse(traindata3$Current_Achievement>=traindata3$Previous_Target,"YES","NO")

table(traindata3$achievement)

########## For Test Dataset #########
testdata3 <- testdata2
testdata3$achievement <- ifelse(testdata3$Current_Achievement>=testdata3$Previous_Target,"YES","NO")

table(testdata3$achievement)

# Change the salesman,product and achievement column into factor for both train and test dataset.

traindata3$Salesman <- as.factor(traindata3$Salesman)
traindata3$Product <- as.factor(traindata3$Product)
traindata3$achievement <- as.factor(traindata3$achievement)

testdata3$Salesman <- as.factor(testdata3$Salesman)
testdata3$Product <- as.factor(testdata3$Product)
testdata3$achievement <- as.factor(testdata3$achievement)

traindata3$Salesman <- as.numeric(traindata3$Salesman)
traindata3$Product <- as.numeric(traindata3$Product)
traindata3$Previous_Target <- log(traindata3$Previous_Target+1)
traindata3$Current_Achievement <- log(traindata3$Current_Achievement+1)

testdata3$Salesman <- as.numeric(testdata3$Salesman)
testdata3$Product <- as.numeric(testdata3$Product)

head(traindata3)
head(testdata3)
str(traindata3)
str(testdata3)

#Model to predict january month Target
library(randomForest)
forest_model <- randomForest(Next_Month_Target~.,data = traindata3,
                             ntree=900,mtry=3,importance=TRUE)

#Model to predict February month Target.
final_data <- rbind(traindata3,testdata3)
head(final_data)
str(final_data)

forest_model2 <- randomForest(Next_Month_Target~.,data = final_data,
                             ntree=900,mtry=3,importance=TRUE)


library(shiny)
library(shinydashboard)
library(dashboardthemes)

ui <- shinyUI(
  dashboardPage(skin = "green",
                dashboardHeader(title = shinyDashboardLogo(theme = "onenote",
                                                           boldText = "Taregt Sales",
                                                           mainText = "Prediction")),
                dashboardSidebar(
                  menuItem("Target Prediction",tabName = "Predict",badgeLabel = "JAN",
                           badgeColor = "red",newtab = TRUE),
                  selectInput("Variable",label = "Salesman",
                              choices = c("SLSMAN_1","SLSMAN_2","SLSMAN_3","SLSMAN_4",
                                          "SLSMAN_5","SLSMAN_6","SLSMAN_7","SLSMAN_8","SLSMAN_10",  
                                          "SLSMAN_9","SLSMAN_11","SLSMAN_12","SLSMAN_13","SLSMAN_14",  
                                          "SLSMAN_15","SLSMAN_16","SLSMAN_17","SLSMAN_18","SLSMAN_19","SLSMAN_21",
                                          "SLSMAN_23","SLSMAN_25","SLSMAN_27","SLSMAN_29","SLSMAN_30","SLSMAN_31",
                                          "SLSMAN_32","SLSMAN_33","SLSMAN_34","SLSMAN_35","SLSMAN_36","SLSMAN_37",
                                          "SLSMAN_38","SLSMAN_39","SLSMAN_40","SLSMAN_41","SLSMAN_42","SLSMAN_43",
                                          "SLSMAN_44","SLSMAN_45","SLSMAN_46","SLSMAN_47","SLSMAN_48","SLSMAN_49",
                                          "SLSMAN_50","SLSMAN_51","SLSMAN_52","SLSMAN_53","SLSMAN_54","SLSMAN_56",
                                          "SLSMAN_57","SLSMAN_58","SLSMAN_60","SLSMAN_62","SLSMAN_64","SLSMAN_66", 
                                          "SLSMAN_68","SLSMAN_70","SLSMAN_73","SLSMAN_75","SLSMAN_79","SLSMAN_80",
                                          "SLSMAN_81","SLSMAN_82","SLSMAN_83","SLSMAN_84","SLSMAN_85","SLSMAN_86",
                                          "SLSMAN_87","SLSMAN_88","SLSMAN_89","SLSMAN_90","SLSMAN_91","SLSMAN_92",
                                          "SLSMAN_93","SLSMAN_94","SLSMAN_95","SLSMAN_96","SLSMAN_97","SLSMAN_98",
                                          "SLSMAN_99","SLSMAN_100","SLSMAN_101","SLSMAN_102","SLSMAN_103","SLSMAN_104",
                                          "SLSMAN_105","SLSMAN_106","SLSMAN_107","SLSMAN_108","SLSMAN_109","SLSMAN_110",
                                          "SLSMAN_111","SLSMAN_112","SLSMAN_113","SLSMAN_114","SLSMAN_115","SLSMAN_117",
                                          "SLSMAN_119","SLSMAN_121","SLSMAN_122","SLSMAN_123","SLSMAN_124","SLSMAN_125",
                                          "SLSMAN_126","SLSMAN_127","SLSMAN_128","SLSMAN_129","SLSMAN_130","SLSMAN_131",
                                          "SLSMAN_132","SLSMAN_134","SLSMAN_136","SLSMAN_138","SLSMAN_139","SLSMAN_140",
                                          "SLSMAN_141","SLSMAN_142","SLSMAN_143","SLSMAN_144","SLSMAN_146","SLSMAN_148",
                                          "SLSMAN_150","SLSMAN_152","SLSMAN_154","SLSMAN_156","SLSMAN_158","SLSMAN_159",
                                          "SLSMAN_160","SLSMAN_161","SLSMAN_162","SLSMAN_163","SLSMAN_164","SLSMAN_165",
                                          "SLSMAN_166","SLSMAN_168","SLSMAN_169","SLSMAN_170","SLSMAN_171","SLSMAN_172",
                                          "SLSMAN_173","SLSMAN_174","SLSMAN_176","SLSMAN_178","SLSMAN_180","SLSMAN_181",
                                          "SLSMAN_182","SLSMAN_183","SLSMAN_184","SLSMAN_185","SLSMAN_186","SLSMAN_190",
                                          "SLSMAN_191","SLSMAN_192","SLSMAN_193","SLSMAN_194","SLSMAN_195","SLSMAN_196",
                                          "SLSMAN_197","SLSMAN_198","SLSMAN_204","SLSMAN_206","SLSMAN_208","SLSMAN_209",
                                          "SLSMAN_210","SLSMAN_211","SLSMAN_212","SLSMAN_213","SLSMAN_214","SLSMAN_215",
                                          "SLSMAN_216","SLSMAN_217")),
                  selectInput("Variable2",label = "Product",
                              choices =c("PROD_3","PROD_5","PROD_11","PROD_13","PROD_14","PROD_15",
                                         "PROD_16","PROD_17","PROD_18","PROD_19","PROD_22","PROD_23",
                                         "PROD_25","PROD_26","PROD_27","PROD_31","PROD_32","PROD_33",
                                         "PROD_35","PROD_36","PROD_37","PROD_38","PROD_39","PROD_42",
                                         "PROD_45","PROD_47","PROD_50","PROD_56","PROD_58","PROD_59",
                                         "PROD_61","PROD_62","PROD_80","PROD_81","PROD_20","PROD_21",
                                         "PROD_28","PROD_54","PROD_52","PROD_4","PROD_48","PROD_12","PROD_51")),
                  numericInput("num","Previous Month sale",0),
                
                
                  menuItem(" Target Prediction",tabName = "Predict2",
                           badgeLabel = "FEB",badgeColor = "green"),
                                 collapsed = FALSE,width = 200,
                                 selectInput("Variable3",label = "Salesman",
                                             choices = c("SLSMAN_1","SLSMAN_2","SLSMAN_3","SLSMAN_4",
                                                         "SLSMAN_5","SLSMAN_6","SLSMAN_7","SLSMAN_8","SLSMAN_10",  
                                                         "SLSMAN_9","SLSMAN_11","SLSMAN_12","SLSMAN_13","SLSMAN_14",  
                                                         "SLSMAN_15","SLSMAN_16","SLSMAN_17","SLSMAN_18","SLSMAN_19","SLSMAN_21",
                                                         "SLSMAN_23","SLSMAN_25","SLSMAN_27","SLSMAN_29","SLSMAN_30","SLSMAN_31",
                                                         "SLSMAN_32","SLSMAN_33","SLSMAN_34","SLSMAN_35","SLSMAN_36","SLSMAN_37",
                                                         "SLSMAN_38","SLSMAN_39","SLSMAN_40","SLSMAN_41","SLSMAN_42","SLSMAN_43",
                                                         "SLSMAN_44","SLSMAN_45","SLSMAN_46","SLSMAN_47","SLSMAN_48","SLSMAN_49",
                                                         "SLSMAN_50","SLSMAN_51","SLSMAN_52","SLSMAN_53","SLSMAN_54","SLSMAN_56",
                                                         "SLSMAN_57","SLSMAN_58","SLSMAN_60","SLSMAN_62","SLSMAN_64","SLSMAN_66", 
                                                         "SLSMAN_68","SLSMAN_70","SLSMAN_73","SLSMAN_75","SLSMAN_79","SLSMAN_80",
                                                         "SLSMAN_81","SLSMAN_82","SLSMAN_83","SLSMAN_84","SLSMAN_85","SLSMAN_86",
                                                         "SLSMAN_87","SLSMAN_88","SLSMAN_89","SLSMAN_90","SLSMAN_91","SLSMAN_92",
                                                         "SLSMAN_93","SLSMAN_94","SLSMAN_95","SLSMAN_96","SLSMAN_97","SLSMAN_98",
                                                         "SLSMAN_99","SLSMAN_100","SLSMAN_101","SLSMAN_102","SLSMAN_103","SLSMAN_104",
                                                         "SLSMAN_105","SLSMAN_106","SLSMAN_107","SLSMAN_108","SLSMAN_109","SLSMAN_110",
                                                         "SLSMAN_111","SLSMAN_112","SLSMAN_113","SLSMAN_114","SLSMAN_115","SLSMAN_117",
                                                         "SLSMAN_119","SLSMAN_121","SLSMAN_122","SLSMAN_123","SLSMAN_124","SLSMAN_125",
                                                         "SLSMAN_126","SLSMAN_127","SLSMAN_128","SLSMAN_129","SLSMAN_130","SLSMAN_131",
                                                         "SLSMAN_132","SLSMAN_134","SLSMAN_136","SLSMAN_138","SLSMAN_139","SLSMAN_140",
                                                         "SLSMAN_141","SLSMAN_142","SLSMAN_143","SLSMAN_144","SLSMAN_146","SLSMAN_148",
                                                         "SLSMAN_150","SLSMAN_152","SLSMAN_154","SLSMAN_156","SLSMAN_158","SLSMAN_159",
                                                         "SLSMAN_160","SLSMAN_161","SLSMAN_162","SLSMAN_163","SLSMAN_164","SLSMAN_165",
                                                         "SLSMAN_166","SLSMAN_168","SLSMAN_169","SLSMAN_170","SLSMAN_171","SLSMAN_172",
                                                         "SLSMAN_173","SLSMAN_174","SLSMAN_176","SLSMAN_178","SLSMAN_180","SLSMAN_181",
                                                         "SLSMAN_182","SLSMAN_183","SLSMAN_184","SLSMAN_185","SLSMAN_186","SLSMAN_190",
                                                         "SLSMAN_191","SLSMAN_192","SLSMAN_193","SLSMAN_194","SLSMAN_195","SLSMAN_196",
                                                         "SLSMAN_197","SLSMAN_198","SLSMAN_204","SLSMAN_206","SLSMAN_208","SLSMAN_209",
                                                         "SLSMAN_210","SLSMAN_211","SLSMAN_212","SLSMAN_213","SLSMAN_214","SLSMAN_215",
                                                         "SLSMAN_216","SLSMAN_217")),
                                 selectInput("Variable4",label = "Product",
                                             choices =c("PROD_3","PROD_5","PROD_11","PROD_13","PROD_14","PROD_15",
                                                        "PROD_16","PROD_17","PROD_18","PROD_19","PROD_22","PROD_23",
                                                        "PROD_25","PROD_26","PROD_27","PROD_31","PROD_32","PROD_33",
                                                        "PROD_35","PROD_36","PROD_37","PROD_38","PROD_39","PROD_42",
                                                        "PROD_45","PROD_47","PROD_50","PROD_56","PROD_58","PROD_59",
                                                        "PROD_61","PROD_62","PROD_80","PROD_81","PROD_20","PROD_21",
                                                        "PROD_28","PROD_54","PROD_52","PROD_4","PROD_48","PROD_12","PROD_51")),
                                 numericInput("num2","Previous Month sale",0)),
                
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "Predict",mainPanel(tableOutput(
                      "Predict"
                    ))),
                  tabItem(tabName = "Predict2",
                          mainPanel(tableOutput("Predict2")))
                  
                ))
  )
  
)

server <- function(input,output){
  output$Predict <- renderTable({
    v <- testdata2[which(testdata2$Salesman == input$Variable & 
                       testdata2$Product == input$Variable2),]

    test <- data.frame(Salesman=input$Variable,Product=input$Variable2,PLAN_MONTH=12,
                       Previous_Target=v$Previous_Target,Current_Achievement=input$num,
                       achievement = ifelse(input$num>=v$Previous_Target,"YEs","NO")
)
    
    test$Salesman <- as.numeric(factor(test$Salesman,levels = c("SLSMAN_1","SLSMAN_2","SLSMAN_3","SLSMAN_4",
                                                                "SLSMAN_5","SLSMAN_6","SLSMAN_7","SLSMAN_8","SLSMAN_10",  
                                                                "SLSMAN_9","SLSMAN_11","SLSMAN_12","SLSMAN_13","SLSMAN_14",  
                                                                "SLSMAN_15","SLSMAN_16","SLSMAN_17","SLSMAN_18","SLSMAN_19","SLSMAN_21",
                                                                "SLSMAN_23","SLSMAN_25","SLSMAN_27","SLSMAN_29","SLSMAN_30","SLSMAN_31",
                                                                "SLSMAN_32","SLSMAN_33","SLSMAN_34","SLSMAN_35","SLSMAN_36","SLSMAN_37",
                                                                "SLSMAN_38","SLSMAN_39","SLSMAN_40","SLSMAN_41","SLSMAN_42","SLSMAN_43",
                                                                "SLSMAN_44","SLSMAN_45","SLSMAN_46","SLSMAN_47","SLSMAN_48","SLSMAN_49",
                                                                "SLSMAN_50","SLSMAN_51","SLSMAN_52","SLSMAN_53","SLSMAN_54","SLSMAN_56",
                                                                "SLSMAN_57","SLSMAN_58","SLSMAN_60","SLSMAN_62","SLSMAN_64","SLSMAN_66", 
                                                                "SLSMAN_68","SLSMAN_70","SLSMAN_73","SLSMAN_75","SLSMAN_79","SLSMAN_80",
                                                                "SLSMAN_81","SLSMAN_82","SLSMAN_83","SLSMAN_84","SLSMAN_85","SLSMAN_86",
                                                                "SLSMAN_87","SLSMAN_88","SLSMAN_89","SLSMAN_90","SLSMAN_91","SLSMAN_92",
                                                                "SLSMAN_93","SLSMAN_94","SLSMAN_95","SLSMAN_96","SLSMAN_97","SLSMAN_98",
                                                                "SLSMAN_99","SLSMAN_100","SLSMAN_101","SLSMAN_102","SLSMAN_103","SLSMAN_104",
                                                                "SLSMAN_105","SLSMAN_106","SLSMAN_107","SLSMAN_108","SLSMAN_109","SLSMAN_110",
                                                                "SLSMAN_111","SLSMAN_112","SLSMAN_113","SLSMAN_114","SLSMAN_115","SLSMAN_117",
                                                                "SLSMAN_119","SLSMAN_121","SLSMAN_122","SLSMAN_123","SLSMAN_124","SLSMAN_125",
                                                                "SLSMAN_126","SLSMAN_127","SLSMAN_128","SLSMAN_129","SLSMAN_130","SLSMAN_131",
                                                                "SLSMAN_132","SLSMAN_134","SLSMAN_136","SLSMAN_138","SLSMAN_139","SLSMAN_140",
                                                                "SLSMAN_141","SLSMAN_142","SLSMAN_143","SLSMAN_144","SLSMAN_146","SLSMAN_148",
                                                                "SLSMAN_150","SLSMAN_152","SLSMAN_154","SLSMAN_156","SLSMAN_158","SLSMAN_159",
                                                                "SLSMAN_160","SLSMAN_161","SLSMAN_162","SLSMAN_163","SLSMAN_164","SLSMAN_165",
                                                                "SLSMAN_166","SLSMAN_168","SLSMAN_169","SLSMAN_170","SLSMAN_171","SLSMAN_172",
                                                                "SLSMAN_173","SLSMAN_174","SLSMAN_176","SLSMAN_178","SLSMAN_180","SLSMAN_181",
                                                                "SLSMAN_182","SLSMAN_183","SLSMAN_184","SLSMAN_185","SLSMAN_186","SLSMAN_190",
                                                                "SLSMAN_191","SLSMAN_192","SLSMAN_193","SLSMAN_194","SLSMAN_195","SLSMAN_196",
                                                                "SLSMAN_197","SLSMAN_198","SLSMAN_204","SLSMAN_206","SLSMAN_208","SLSMAN_209",
                                                                "SLSMAN_210","SLSMAN_211","SLSMAN_212","SLSMAN_213","SLSMAN_214","SLSMAN_215",
                                                                "SLSMAN_216","SLSMAN_217")))
    test$Product <-as.numeric(factor(test$Product,levels = c("PROD_3","PROD_5","PROD_11","PROD_13","PROD_14","PROD_15",
                                                             "PROD_16","PROD_17","PROD_18","PROD_19","PROD_22","PROD_23",
                                                             "PROD_25","PROD_26","PROD_27","PROD_31","PROD_32","PROD_33",
                                                             "PROD_35","PROD_36","PROD_37","PROD_38","PROD_39","PROD_42",
                                                             "PROD_45","PROD_47","PROD_50","PROD_56","PROD_58","PROD_59",
                                                             "PROD_61","PROD_62","PROD_80","PROD_81","PROD_20","PROD_21",
                                                             "PROD_28","PROD_54","PROD_52","PROD_4","PROD_48","PROD_12","PROD_51")))
    
    test$achievement <- factor(test$achievement,levels = c("YES","NO"))
    test$Previous_Target <- log(test$Previous_Target+1)
    test$Current_Achievement <- log(test$Current_Achievement+1)
    pred_sales1 <- predict(forest_model,test)
    pred_sales1 <- round(pred_sales1)
    paste(" The next month target of ",input$Variable," For ",input$Variable2," is ",pred_sales1)
    
  })
  output$Predict2 <- renderTable({
    w <- valid[which(valid$SLSMAN_CD == input$Variable3 & 
                           valid$PROD_CD == input$Variable4),]
    
    test2 <- data.frame(Salesman=input$Variable3,Product=input$Variable4,PLAN_MONTH=1,
                       Previous_Target=w$TARGET_IN_EA,Current_Achievement=input$num2,
                       achievement = ifelse(input$num2>=w$TARGET_IN_EA,"YEs","NO")
    )
    
    test2$Salesman <- as.numeric(factor(test2$Salesman,levels = c("SLSMAN_1","SLSMAN_2","SLSMAN_3","SLSMAN_4",
                                                                "SLSMAN_5","SLSMAN_6","SLSMAN_7","SLSMAN_8","SLSMAN_10",  
                                                                "SLSMAN_9","SLSMAN_11","SLSMAN_12","SLSMAN_13","SLSMAN_14",  
                                                                "SLSMAN_15","SLSMAN_16","SLSMAN_17","SLSMAN_18","SLSMAN_19","SLSMAN_21",
                                                                "SLSMAN_23","SLSMAN_25","SLSMAN_27","SLSMAN_29","SLSMAN_30","SLSMAN_31",
                                                                "SLSMAN_32","SLSMAN_33","SLSMAN_34","SLSMAN_35","SLSMAN_36","SLSMAN_37",
                                                                "SLSMAN_38","SLSMAN_39","SLSMAN_40","SLSMAN_41","SLSMAN_42","SLSMAN_43",
                                                                "SLSMAN_44","SLSMAN_45","SLSMAN_46","SLSMAN_47","SLSMAN_48","SLSMAN_49",
                                                                "SLSMAN_50","SLSMAN_51","SLSMAN_52","SLSMAN_53","SLSMAN_54","SLSMAN_56",
                                                                "SLSMAN_57","SLSMAN_58","SLSMAN_60","SLSMAN_62","SLSMAN_64","SLSMAN_66", 
                                                                "SLSMAN_68","SLSMAN_70","SLSMAN_73","SLSMAN_75","SLSMAN_79","SLSMAN_80",
                                                                "SLSMAN_81","SLSMAN_82","SLSMAN_83","SLSMAN_84","SLSMAN_85","SLSMAN_86",
                                                                "SLSMAN_87","SLSMAN_88","SLSMAN_89","SLSMAN_90","SLSMAN_91","SLSMAN_92",
                                                                "SLSMAN_93","SLSMAN_94","SLSMAN_95","SLSMAN_96","SLSMAN_97","SLSMAN_98",
                                                                "SLSMAN_99","SLSMAN_100","SLSMAN_101","SLSMAN_102","SLSMAN_103","SLSMAN_104",
                                                                "SLSMAN_105","SLSMAN_106","SLSMAN_107","SLSMAN_108","SLSMAN_109","SLSMAN_110",
                                                                "SLSMAN_111","SLSMAN_112","SLSMAN_113","SLSMAN_114","SLSMAN_115","SLSMAN_117",
                                                                "SLSMAN_119","SLSMAN_121","SLSMAN_122","SLSMAN_123","SLSMAN_124","SLSMAN_125",
                                                                "SLSMAN_126","SLSMAN_127","SLSMAN_128","SLSMAN_129","SLSMAN_130","SLSMAN_131",
                                                                "SLSMAN_132","SLSMAN_134","SLSMAN_136","SLSMAN_138","SLSMAN_139","SLSMAN_140",
                                                                "SLSMAN_141","SLSMAN_142","SLSMAN_143","SLSMAN_144","SLSMAN_146","SLSMAN_148",
                                                                "SLSMAN_150","SLSMAN_152","SLSMAN_154","SLSMAN_156","SLSMAN_158","SLSMAN_159",
                                                                "SLSMAN_160","SLSMAN_161","SLSMAN_162","SLSMAN_163","SLSMAN_164","SLSMAN_165",
                                                                "SLSMAN_166","SLSMAN_168","SLSMAN_169","SLSMAN_170","SLSMAN_171","SLSMAN_172",
                                                                "SLSMAN_173","SLSMAN_174","SLSMAN_176","SLSMAN_178","SLSMAN_180","SLSMAN_181",
                                                                "SLSMAN_182","SLSMAN_183","SLSMAN_184","SLSMAN_185","SLSMAN_186","SLSMAN_190",
                                                                "SLSMAN_191","SLSMAN_192","SLSMAN_193","SLSMAN_194","SLSMAN_195","SLSMAN_196",
                                                                "SLSMAN_197","SLSMAN_198","SLSMAN_204","SLSMAN_206","SLSMAN_208","SLSMAN_209",
                                                                "SLSMAN_210","SLSMAN_211","SLSMAN_212","SLSMAN_213","SLSMAN_214","SLSMAN_215",
                                                                "SLSMAN_216","SLSMAN_217")))
    test2$Product <-as.numeric(factor(test2$Product,levels = c("PROD_3","PROD_5","PROD_11","PROD_13","PROD_14","PROD_15",
                                                             "PROD_16","PROD_17","PROD_18","PROD_19","PROD_22","PROD_23",
                                                             "PROD_25","PROD_26","PROD_27","PROD_31","PROD_32","PROD_33",
                                                             "PROD_35","PROD_36","PROD_37","PROD_38","PROD_39","PROD_42",
                                                             "PROD_45","PROD_47","PROD_50","PROD_56","PROD_58","PROD_59",
                                                             "PROD_61","PROD_62","PROD_80","PROD_81","PROD_20","PROD_21",
                                                             "PROD_28","PROD_54","PROD_52","PROD_4","PROD_48","PROD_12","PROD_51")))
    
    test2$achievement <- factor(test2$achievement,levels = c("YES","NO"))
    test2$Previous_Target <- log(test2$Previous_Target+1)
    test2$Current_Achievement <- log(test2$Current_Achievement+1)
    pred_sales2 <- predict(forest_model2,test2)
    pred_sales2 <- round(pred_sales2)
    paste(" The next month target of ",input$Variable3," For ",input$Variable4," is ",pred_sales2)
     
  })
}

shinyApp(ui, server)