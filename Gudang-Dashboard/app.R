# Import libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(leaflet)
library(readxl)

# Read data from Excel
file <- "../data_pengiriman_gudang.xlsx"

# Read all sheets
lokasi <- read_excel(file, sheet = "Lokasi Gudang")
gudang <- read_excel(file, sheet = "Daftar Gudang")
pengiriman <- read_excel(file, sheet = "Data Pengiriman")

# Join data
GudangData <- gudang %>%
  left_join(lokasi, by = c("KodeLokasi" = "KodeLokasi")) %>%
  left_join(pengiriman, by = "KodeGudang") %>%
  mutate(
    TotalPengiriman = PengirimanLuar + PengirimanDalam,
    TotalPendapatan = BiayaPenanganan * TotalPengiriman
  )

# Dropdown choices
tipe_gudang <- c("All Tipe", unique(GudangData$TipeGudang))
year_list <- c("All Years", sort(unique(GudangData$Tahun)))

# Currency formatting
formatPendapatan <- function(amount) {
  if (amount >= 1e12) {
    return(paste0("Rp ", scales::number(amount / 1e12, accuracy = 0.01), " T"))
  } else if (amount >= 1e9) {
    return(paste0("Rp ", scales::number(amount / 1e9, accuracy = 0.01), " M"))
  } else if (amount >= 1e6) {
    return(paste0("Rp ", scales::number(amount / 1e6, accuracy = 0.01), " Juta"))
  } else {
    return(paste0("Rp ", scales::number(amount, accuracy = 1)))
  }
}

# UI
ui <- shinyUI(
  dashboardPage(
    title = "Dashboard Pengiriman Gudang",
    
    dashboardHeader(title = "Pengiriman Barang", titleWidth = 250),
    
    dashboardSidebar(
      selectInput(
        inputId = "tipegudang",
        label = "Pilih Tipe Gudang",
        choices = tipe_gudang,
        selected = "All Tipe",
        selectize = FALSE
      ),
      selectInput(
        inputId = "year",
        label = "Pilih Tahun",
        choices = year_list,
        selected = "All Years",
        selectize = FALSE
      )
    ),
    
    dashboardBody(
      # Custom Dark Theme CSS
      tags$head(tags$style(HTML("
        body, .content-wrapper, .right-side {
          background-color: #121212;
          color: #ecf0f1;
        }

        .skin-blue .main-header .logo {
          background-color: #34495e;
          color: #ffffff;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #2c3e50;
        }
        .skin-blue .main-header .navbar {
          background-color: #34495e;
        }

        .skin-blue .main-sidebar {
          background-color: #1a1a1a;
        }
        .skin-blue .sidebar-menu > li.active > a {
          background-color: #3498db;
          color: #ffffff;
        }
        .skin-blue .sidebar-menu > li > a {
          color: #bdc3c7;
        }
        .skin-blue .sidebar-menu > li > a:hover {
          background-color: #2c3e50;
          color: #ffffff;
        }

        .box {
          background-color: #1e1e1e;
          border-top: 3px solid #3498db;
          color: #ecf0f1;
        }
        .box.box-primary {
          border-top-color: #3498db;
        }

        .small-box.bg-aqua,
        .small-box.bg-blue,
        .small-box.bg-green,
        .small-box.bg-red,
        .small-box.bg-purple {
          background-color: #3498db !important;
          color: #ffffff !important;
        }

        .plot-container {
          background-color: #1e1e1e;
        }
      "))),
      
      fluidRow(
        valueBoxOutput("total_luar", width = 3),
        valueBoxOutput("total_dalam", width = 3),
        valueBoxOutput("total_pengiriman", width = 3),
        valueBoxOutput("total_pendapatan", width = 3)
      ),
      
      fluidRow(
        box(
          title = "Top 10 Gudang dengan Pengiriman Tertinggi",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          plotOutput("top_gudang")
        ),
        box(
          title = "Distribusi Pengiriman Berdasarkan Provinsi",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          plotOutput("donut_chart")
        )
      ),
      
      fluidRow(
        box(
          title = "Peta Lokasi Gudang",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          leafletOutput("gudang_map", height = 500)
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  showNotification("Dashboard Pengiriman Gudang by Faiza Kailani K", duration = NULL, type = "message")
  
  filtered_data <- reactive({
    res <- GudangData
    if (input$tipegudang != "All Tipe") {
      res <- filter(res, TipeGudang == input$tipegudang)
    }
    if (input$year != "All Years") {
      res <- filter(res, Tahun == as.numeric(input$year))
    }
    res
  })
  
  # VALUE BOXES
  output$total_luar <- renderValueBox({
    total <- sum(filtered_data()$PengirimanLuar, na.rm = TRUE)
    valueBox(
      value = format(total, format = "d", big.mark = ","),
      subtitle = "Pengiriman Luar Provinsi",
      icon = icon("truck"),
      color = "aqua"
    )
  })
  
  output$total_dalam <- renderValueBox({
    total <- sum(filtered_data()$PengirimanDalam, na.rm = TRUE)
    valueBox(
      value = format(total, format = "d", big.mark = ","),
      subtitle = "Pengiriman Dalam Provinsi",
      icon = icon("home"),
      color = "aqua"
    )
  })
  
  output$total_pengiriman <- renderValueBox({
    total <- sum(filtered_data()$TotalPengiriman, na.rm = TRUE)
    valueBox(
      value = format(total, format = "d", big.mark = ","),
      subtitle = "Total Pengiriman",
      icon = icon("boxes"),
      color = "aqua"
    )
  })
  
  output$total_pendapatan <- renderValueBox({
    total <- sum(filtered_data()$TotalPendapatan, na.rm = TRUE)
    valueBox(
      value = formatPendapatan(total),
      subtitle = "Total Pendapatan Penanganan",
      icon = icon("money-bill-wave"),
      color = "aqua"
    )
  })
  
  # BAR CHART
  output$top_gudang <- renderPlot({
    data <- filtered_data()
    top_gudang <- data %>%
      group_by(NamaGudang) %>%
      summarise(
        TotalPengiriman = sum(TotalPengiriman, na.rm = TRUE),
        Luar = sum(PengirimanLuar, na.rm = TRUE),
        Dalam = sum(PengirimanDalam, na.rm = TRUE)
      ) %>%
      arrange(desc(TotalPengiriman)) %>%
      slice_head(n = 10)
    
    stacked_data <- top_gudang %>%
      pivot_longer(cols = c(Luar, Dalam), names_to = "Kategori", values_to = "Jumlah")
    
    ggplot(stacked_data, aes(x = reorder(NamaGudang, -Jumlah), y = Jumlah, fill = Kategori)) +
      geom_bar(stat = "identity") +
      labs(x = "Nama Gudang", y = "Jumlah Pengiriman") +
      scale_fill_manual(values = c("Luar" = "#3498db", "Dalam" = "#2c3e50")) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1e1e1e", color = NA),
        panel.background = element_rect(fill = "#1e1e1e"),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.text = element_text(color = "#ecf0f1"),
        axis.text = element_text(color = "#ecf0f1"),
        axis.title = element_text(color = "#ecf0f1"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(color = "#ecf0f1")
      )
  })
  
  # DONUT CHART
  output$donut_chart <- renderPlot({
    data <- filtered_data() %>%
      group_by(Provinsi) %>%
      summarise(jumlah_pengiriman = sum(TotalPengiriman, na.rm = TRUE)) %>%
      arrange(desc(jumlah_pengiriman)) %>%
      mutate(percentage = jumlah_pengiriman / sum(jumlah_pengiriman) * 100)
    
    ggplot(data, aes(x = 2, y = jumlah_pengiriman, fill = Provinsi)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      labs(fill = "Provinsi") +
      scale_fill_manual(values = c(
        "#3498db", "#2980b9", "#2c3e50", "#5dade2", "#1f618d",
        "#154360", "#2874a6", "#21618c", "#2471a3", "#2e86c1"
      )) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#1e1e1e", color = NA),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.text = element_text(color = "#ecf0f1"),
        legend.title = element_text(color = "#ecf0f1")
      ) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")),
                position = position_stack(vjust = 0.5),
                size = 4, color = "white")
  })
  
  # MAP
  output$gudang_map <- renderLeaflet({
    data <- filtered_data() %>%
      group_by(Kota, Lat, Lon) %>%
      summarise(TotalPengiriman = sum(TotalPengiriman, na.rm = TRUE), .groups = "drop")
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Lon,
        lat = ~Lat,
        popup = ~paste(
          "<strong>Kota: </strong>", Kota,
          "<br><strong>Total Pengiriman: </strong>", format(TotalPengiriman, format = "d", big.mark = ",")
        ),
        radius = ~sqrt(TotalPengiriman) / 50,
        color = ~ifelse(TotalPengiriman > 1e+06, '#3498db', '#2c3e50'),
        fillOpacity = 0.7
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
  