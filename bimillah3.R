#Set Directory
getwd()
#Set Package and Library
library(shiny)
#install.packages("argonR")
library(argonR)
# devel version
#devtools::install_github("RinteRface/argonDash")
# from CRAN
#install.packages("argonDash")
library(argonDash)
library(htmltools)
library(magrittr)
library(DT)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(purrr)
library(ggplot2)
library(shinydashboardPlus)
library(dashboardthemes)
library(reticulate)
# Use the Python environment
use_python("C:/Users/ACER/anaconda3/python.exe")

# Load the models
rf_model_path <- "D:/SEMESTER 6/DATA MINING DAN VISUAL/FP/fis/rf_model.joblib"
svm_model_path <- "D:/SEMESTER 6/DATA MINING DAN VISUAL/FP/fis/svm_model.joblib"
nn_model_path <- "D:/SEMESTER 6/DATA MINING DAN VISUAL/FP/fis/nn_model.joblib"
#JOBLIB
joblib <- import("joblib")
rf_model <- joblib$load(rf_model_path)
svm_model <- joblib$load(svm_model_path)
nn_model <- joblib$load(nn_model_path)
#Load Datasets
datautama <- read.csv("D:/SEMESTER 6/DATA MINING DAN VISUAL/FP/fis/breast-cancer.csv")
head(datautama)
datautama$concave.points_worst
du <- datautama[c("radius_worst","area_worst","concave.points_worst","diagnosis")]
#Funtion UI
#=================================================================================================Statitics Descriptice
cards_tab <- argonTabItem(
  tabName = "stdesk",
  
  # classic cards
  argonRow(
    argonH1("STATISTICS DESCRIPTIVE", display = 4),
    center = TRUE
  ),
  
  argonRow(
    argonCard(
      width = 12,
      src = NULL,
      icon = icon("cogs"),
      status = "primary",
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      title = "Descriptive",
      argonRow(
        argonColumn(width = 12, plotOutput("piechartdiag")),
        argonColumn(
          radioButtons(
            inputId = "kategori", 
            inline = TRUE,
            label = "Choose Category",
            choices = c("Maligant dan Benign"="M & B","Maligant"="M","Benign"= "B"), 
            selected = "M & B"
          ),
          center = TRUE,
          argonInfoCard(
            value = uiOutput("Jumlahdata"), 
            title = "Jumlah Data (N)", 
            icon = argonIcon("planet"), 
            icon_background = "danger",
            hover_lift = TRUE,
            gradient = TRUE,
            background_color = "primary",
            width=12
          ),
          argonInfoCard(
            value = uiOutput("rata"), 
            title = "Rata-Rata", 
            icon = argonIcon("app"), 
            icon_background = "warning",
            hover_lift = TRUE,
            gradient = TRUE,
            background_color = "info",
            width = 12
          ),
          argonInfoCard(
            value = uiOutput("std"), 
            title = "Standar Deviasi",
            icon = argonIcon("bulb-61"), 
            icon_background = "info",
            hover_lift = TRUE,
            background_color = "success",
            width = 12
          )
        ),
        argonColumn(
          center = TRUE,
          radioButtons(
            inputId = "variabel", 
            inline = TRUE,
            label = "Choose Variable",
            choices = c("Radius Worst"="radius_worst","Area Worst"="area_worst", "Concave Points Worst"="concave.points_worst"), 
            selected = "radius_worst"
          ),
          argonInfoCard(
            value = uiOutput("max"), 
            title = "Maximum",
            gradient = TRUE,
            icon = argonIcon("planet"), 
            icon_background = "danger",
            hover_lift = TRUE,
            background_color = "primary",
            width=12,
          ),
          argonInfoCard(
            value = uiOutput("med"), 
            title = "Median", 
            gradient = TRUE,
            icon = argonIcon("app"), 
            icon_background = "warning",
            hover_lift = TRUE,
            background_color = "info",
            width = 12
          ),
          argonInfoCard(
            value = uiOutput("min"), 
            title = "Minimum", 
            icon = argonIcon("bulb-61"), 
            icon_background = "info",
            hover_lift = TRUE,
            background_color = "success",
            width = 12
          )
        )
      ),
    ),
    br(), br(),
    argonCard(
      width = 12,
      title = "HISTOGRAM",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color ="#230c0c" ,
      gradient = FALSE, 
      floating = FALSE,
      argonRow(
        argonColumn(
          radioButtons(
            "kategori2",
            "Choose Category :",
            c("Maligant And Benign"="M&D",
              "Maligant"="M",
              "Benign"="B")
          ),
          width = 6,
          radioButtons(
            "variabel2", 
            "Choose Variable :",
            c("Radius Worst" = "radius_worst",
              "Area Worst" = "area_worst",
              "Concave Point Worst" = "concave.points_worst"
            )
          )
        ),
        argonColumn(width = 6, plotOutput("histplot"))
      )
    ),    br(), br(),
    argonCard(
      width = 12,
      title = "KORELASI",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color ="#230c0c" ,
      gradient = FALSE, 
      floating = FALSE,
      argonRow(
        argonColumn(
          radioButtons(
            "kategori3",
            "Choose Category :",
            c("Maligant And Benign"="M&D",
              "Maligant"="M",
              "Benign"="B")
          ),
          width = 4,
        ),
        argonColumn(
          radioButtons(
            "variabel3", 
            "Choose Variable :",
            c("Radius Worst" = "radius_worst",
              "Area Worst" = "area_worst",
              "Concave Point Worst" = "concave.points_worst"
            )
          ),
          radioButtons(
            "variabel4", 
            "Choose Variable:",
            c("Radius Worst" = "radius_worst",
              "Area Worst" = "area_worst",
              "Concave Point Worst" = "concave.points_worst"
            )
          )
        ),
        argonColumn(width = 4,plotOutput("corrplot"))
      )
    )  
  ),
  br()
)
#=================================================================================================author
avatarSizes <- c("sm", "md", "lg")
avatarTooltips <- c(NULL, "My avatar", NULL)

items_tab <- argonTabItem(
  tabName = "author",
  argonRow(
    argonH1("OUR TEAM", display = 4),
    center = TRUE
  ),
  argonRow(
    width = 12,
    center = TRUE,
    argonColumn(
      width = 4,
      argonUser(
        title = "NAUFAL LUTHFAN TASBIHI",
        subtitle = "5003211012",
        src = "https://imagetolink.com/ib/GUNPbeVtTr.jpg",
      )
    ),
    argonColumn(
      width = 4,
      argonUser(
        title = "HANIF CHOIRUDDIN",
        subtitle = "5003211063",
        src = "https://imagetolink.com/ib/abaaFy82O0.jpg",
      )
    ),
    argonColumn(
      width = 4,
      argonUser(
        title = "IMAM NUR RIZKY GUSMAN",
        subtitle = "500321121",
        src = "https://imagetolink.com/ib/5ZPJK8BjnS.png",
      )
    )
  )
)
images_tab <- argonTabItem(
  tabName = "medias",
  argonRow(
    argonCard(
      width = 12,
      icon = icon("cogs"),
      status = "success",
      title = argonH1("Input Data",display = 4),
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      argonColumn(argonH1("Enter Your Data",display=4),width = 12,center = TRUE),
      argonRow(
        width = 12,
        center = TRUE,
        argonColumn(            
          width = 4,
          center = TRUE,
          sliderInput(
            "rw", 
            "Radius Worst:",
            min = 0, 
            max = 50, 
            value = 25,
            round = FALSE
          )
        ),
        argonColumn(
          width=4,
          center = TRUE,
          sliderInput(
            "aw", 
            "Area Worst:",
            min = 0, 
            max = 5000, 
            value = 2500
          )
        ),
        argonColumn(
          width=4,
          center = TRUE,
          sliderInput(
            "cpw", 
            "Concave Points Worst:",
            min = 0, 
            max = 1, 
            value = 0.025
          )
        )
      )
      
    ),
    argonCard(
      width = 12,
      icon = icon("cogs"),
      status = "success",
      title = argonH1("Output Data",display = 4),
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      argonColumn(argonH1("Result Classification With Methode",display=4),width = 12,center = TRUE),
      argonRow(
        width = 12,
        center = TRUE,
        argonColumn(
          argonInfoCard(
            width = 12,
            icon = icon("cogs"),
            title = "Support Vector Machine",
            uiOutput("HasilSVM")
          )),
        argonColumn(
          argonInfoCard(
            width = 12,
            icon = icon("cogs"),
            title =  "Neural Network",
            uiOutput("HasilCNN")
          )
        ),
        argonColumn(
          argonInfoCard(
            width = 12,
            icon = icon("cogs"),
            title = "Random Forest",
            uiOutput("HasilRF")
          )
        )
      )
      
    )
  )
)
#=================================================================================================Daaset
Deskripsi <-"Breast cancer is the most common cancer amongst women in the world. It accounts for 25% of all cancer cases,and affected over 2.1 Million people in 2015 alone. It starts when cells in the breast begin to grow out of control. These cells usually form tumors that can be seen via X-ray or felt as lumps in the breast area."
Deskripsi2 <- "The key challenges against it’s detection is how to classify tumors into malignant (cancerous) or benign(non cancerous). We ask you to complete the analysis of classifying these tumors using machine learning (with SVMs) and the Breast Cancer Wisconsin (Diagnostic) Dataset."
tables_tab <- argonTabItem(
  tabName = "dataset",
  argonRow(
    argonH1("Dataset Breast Cancer",display = 4),
    center = TRUE
  ),
  argonCard(
    title = "About Dataset",
    src = "https://www.kaggle.com/datasets/yasserh/breast-cancer-dataset",
    width = 0,
    hover_lift = TRUE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    icon = argonIcon("atom"),
    status = "primary",
    background_color = NULL,
    gradient = FALSE, 
    floating = FALSE,
    Deskripsi,
    br(),br(),
    Deskripsi2,
  ),
  argonColumn(DTOutput('tabel_data'),center = FALSE)
)
#=================================================================================================Description

tabText2 <- "Breast cancer remains one of the most significant health challenges worldwide, particularly affecting women.
            It is the most common cancer among women and is also one of the principal causes of death among cancers. Early 
            detection is crucial as it significantly improves prognosis and survival rates. According to the World Health Organization,
            breast cancer survival rates vary globally, being highest in developed countries and lowest in the developing world, a disparity largely due to the
            availability of adequate diagnosis and treatment options (WHO, 2020). Traditional diagnostic methods like mammography, while effective, are limited 
            by their high costs, the need for specialized equipment and expertise, and the risk of radiation exposure (Smith et al., 2015)."
tabsets_tab <- argonTabItem(
  tabName = "description",
  argonRow(
    argonH1("BREAST CANCER", display = 4),
    center = TRUE
  ),
  argonRow(
    # Horizontal Tabset
    argonColumn(
      width = 6,
      center = TRUE,
      argonTabSet(
        id = "tab-1",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "WHAT IS BREAST CANCER",
          active = TRUE,
          tabText2
        )
      )
    ),
    argonColumn(
      width = 6,
      argonTabSet(
        id = "tab-1",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = lapply(X = 1, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "WHAT IS BREAST CANCER",
          active = TRUE,
          htmltools::tags$iframe(
            src = "https://www.youtube.com/embed/LMRhix52hR8",
            width = "100%",
            height = "400",
            frameborder = "0",
            allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
            allowfullscreen = NA
          )
        )
      )
    )
  )
)
#=================================================================================================Footer
argonFooter <- argonDashFooter(
  copyrights = "@Team 1, 2024",
  src = "",
  argonFooterMenu(
    argonFooterItem("Data Mining and Visualization A", src = ""),
  )
)
#=================================================================================================Header
argonHeader <- argonDashHeader(
  gradient = TRUE,
  color = "blue",
  separator = TRUE,
  separator_color = "secondary",
  argonCard(
    title = "Welcome to our dashboard! ",
    src = "https://www.its.ac.id/statistika/akademik/program-studi/s1-statistika/",
    hover_lift = TRUE,
    shadow = TRUE,
    shadow_size = NULL,
    hover_shadow = FALSE,
    border_level = 0,
    icon = argonIcon("atom"),
    status = "primary",
    background_color = NULL,
    gradient = FALSE, 
    floating = FALSE,
    "This dashboard was created as part of the Data mining and visualization course assignment, Department of Statistics, Faculty of Science and Data Analytics ITS, with the aim of knowing the classification of breast cancer data."
  )
)
#=================================================================================================Sidebar
argonSidebar <- argonDashSidebar(
  vertical = TRUE,
  skin = "light",
  background = "white",
  size = "lg",
  side = "left",
  id = "my_sidebar",
  brand_url = "http://www.google.com",
  brand_logo = "https://i.postimg.cc/wM5pYgYK/MF-ANDAT-removebg-preview.png",
  argonSidebarHeader(title = "Main Menu"),
  argonSidebarMenu(
    argonSidebarItem(
      tabName = "description",
      icon = argonIcon(name = "planet", color = "warning"),
      "BREAST CANCER"
    ),
    argonSidebarItem(
      tabName = "stdesk",
      icon = argonIcon(name = "tv-2", color = "info"),
      "STATISTICS DESCRIPTIVE"
    ),
    argonSidebarItem(
      tabName = "dataset",
      icon = argonIcon(name = "folder-17", color = "green"),
      "DATASET"
    ),
    argonSidebarItem(
      tabName = "medias",
      icon = argonIcon(name = "ui-04", color = "success"),
      "TESTING MODELS"
    ),
    argonSidebarItem(
      tabName = "author",
      icon = argonIcon(name = "circle-08", color = "pink"),
      "AUTHOR"
    ),
  ),
  argonSidebarDivider(),
  argonSidebarHeader(title = "Other Items")
)

