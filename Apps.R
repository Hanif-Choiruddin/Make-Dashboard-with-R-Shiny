#=================================================================================================Shiny
# App
shiny::shinyApp(
  ui = argonDashPage(
    title = "Test",
    author = "David",
    description = "Argon Dash Test",
    sidebar = argonSidebar,
    header = argonHeader,
    body = argonDashBody(
      argonTabItems(
        tabsets_tab,
        cards_tab,
        tables_tab,
        images_tab,
        items_tab
      )
    ),
    footer = argonFooter
  ),
  server = function(input, output) {
    
    
    
    #STATISTICS DESCRIPTIVE
    output$piechartdiag <- renderPlot({
      #BUAT PIE CHART
      frekuensi_tidy <- datautama %>%
        count(diagnosis) %>%
        mutate(prop = n / sum(n) * 100, 
               label = paste0(diagnosis, ": ", round(prop, 1), "%"))
      
      # Membuat diagram pie menggunakan ggplot2 dengan label
      ggplot(frekuensi_tidy, aes(x = "", y = n, fill = diagnosis)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_void() +
        geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
        labs(title = "PIE CHART") +
        theme(legend.position = "right")
    })
    #JUMLAH KOLOM
    output$Jumlahdata <- renderUI(
      {
        count_values <- du %>%
          group_by(diagnosis) %>%
          summarise(count = n())
        jd <- if(input$kategori=="M"){
          print(count_values[1,2])
        }else if(input$kategori=="B"){
          print(count_values[2,2])
        }
        else{
          print(count_values[1,2]+count_values[2,2])
        }
      }
    )
    #MEAN MODUS DKK
    output$rata <- renderUI({
      mean_values <- du %>%
        group_by(diagnosis) %>%
        summarise(across(everything(), mean, na.rm = TRUE))
      rt <-if(input$kategori=="M"){
        if(input$variabel=="radius_worst"){
          #MEAN UNTUK VARIABEL ID
          print(mean_values[1,1]) 
        }else if(input$variabel=="area_worst")
        {
          #MEAN UNTUK VARIABEL radius_mean
          print(mean_values[1,2])
        }else{
          #MEAN UNTUK VARIABELtexture_mean
          print(mean_values[1,3])
        }
      }else if(input$kategori=="B"){
        if(input$variabel=="radius_worst"){
          #MEAN UNTUK VARIABEL ID
          print(mean_values[2,2]) 
        }else if(input$variabel=="area_worst")
        {
          #MEAN UNTUK VARIABEL radius_mean
          print(mean_values[2,3])
        }else{
          #MEAN UNTUK VARIABELtexture_mean
          print(mean_values[2,4])
        }
      }else{
        if(input$variabel=="radius_worst"){
          #MEAN UNTUK VARIABEL ID
          print(mean(du[,1])) 
        }else if(input$variabel=="area_worst")
        {
          #MEAN UNTUK VARIABEL radius_mean
          print(mean(du[,2]))
        }else{
          #MEAN UNTUK VARIABELtexture_mean
          print(mean(du[,3]))
        }
      }
    })
    output$std <- renderUI({
      stdev_values <- du %>%
        group_by(diagnosis) %>%
        summarise(across(everything(), sd, na.rm = TRUE))
      rt <-if(input$kategori=="M"){
        if(input$variabel=="radius_worst"){
          #STDEV UNTUK VARIABEL ID
          print(stdev_values[1,2]) 
        }else if(input$variabel=="area_worst")
        {
          #STDEV UNTUK VARIABEL radius_mean
          print(stdev_values[1,3])
        }else{
          #STDEV UNTUK VARIABELtexture_mean
          print(stdev_values[1,4])
        }
      }else if(input$kategori=="B"){
        if(input$variabel=="radius_worst"){
          #STDEV UNTUK VARIABEL ID
          print(stdev_values[2,2]) 
        }else if(input$variabel=="area_worst")
        {
          #STDEV UNTUK VARIABEL radius_mean
          print(stdev_values[2,3])
        }else{
          #STDEV UNTUK VARIABELtexture_mean
          print(stdev_values[2,4])
        }
      }else{
        if(input$variabel=="radius_worst"){
          #STDEV UNTUK VARIABEL ID
          print(sd(du[,1])) 
        }else if(input$variabel=="area_worst")
        {
          #STDEV UNTUK VARIABEL radius_mean
          print(sd(du[,2]))
        }else{
          #STDEV UNTUK VARIABELtexture_mean
          print(sd(du[,3]))
        }
      }
    })
    output$min <- renderUI({
      min_values <- du %>%
        group_by(diagnosis) %>%
        summarise(across(everything(), min, na.rm = TRUE))
      rt <-if(input$kategori=="M"){
        if(input$variabel=="radius_worst"){
          #min UNTUK VARIABEL ID
          print(min_values[1,2]) 
        }else if(input$variabel=="area_worst")
        {
          #min UNTUK VARIABEL radius_mean
          print(min_values[1,3])
        }else{
          #min UNTUK VARIABELtexture_mean
          print(min_values[1,4])
        }
      }else if(input$kategori=="B"){
        if(input$variabel=="radius_worst"){
          #min UNTUK VARIABEL ID
          print(min_values[2,2]) 
        }else if(input$variabel=="area_worst")
        {
          #min UNTUK VARIABEL radius_mean
          print(min_values[2,3])
        }else{
          #min UNTUK VARIABELtexture_mean
          print(min_values[2,4])
        }
      }else{
        if(input$variabel=="radius_worst"){
          #min UNTUK VARIABEL ID
          print(min(du[,1])) 
        }else if(input$variabel=="area_worst")
        {
          #min UNTUK VARIABEL radius_mean
          print(min(du[,2]))
        }else{
          #min UNTUK VARIABELtexture_mean
          print(min(du[,3]))
        }
      }
    })
    output$max <- renderUI({
      max_values <- du %>%
        group_by(diagnosis) %>%
        summarise(across(everything(), max, na.rm = TRUE))
      rt <-if(input$kategori=="M"){
        if(input$variabel=="radius_worst"){
          #max UNTUK VARIABEL ID
          print(max_values[1,2]) 
        }else if(input$variabel=="area_worst")
        {
          #max UNTUK VARIABEL radius_mean
          print(max_values[1,3])
        }else{
          #max UNTUK VARIABELtexture_mean
          print(max_values[1,4])
        }
      }else if(input$kategori=="B"){
        if(input$variabel=="radius_worst"){
          #max UNTUK VARIABEL ID
          print(max_values[2,2]) 
        }else if(input$variabel=="area_worst")
        {
          #max UNTUK VARIABEL radius_mean
          print(max_values[2,3])
        }else{
          #max UNTUK VARIABELtexture_mean
          print(max_values[2,4])
        }
      }else{
        if(input$variabel=="radius_worst"){
          #max UNTUK VARIABEL ID
          print(max(du[,1])) 
        }else if(input$variabel=="area_worst")
        {
          #max UNTUK VARIABEL radius_mean
          print(max(du[,2]))
        }else{
          #max UNTUK VARIABELtexture_mean
          print(max(du[,3]))
        }
      }
    })
    
    output$med <- renderUI({
      med_values <- du %>%
        group_by(diagnosis) %>%
        summarise(across(everything(), median, na.rm = TRUE))
      rt <-if(input$kategori=="M"){
        if(input$variabel=="radius_worst"){
          #med UNTUK VARIABEL ID
          print(med_values[1,2]) 
        }else if(input$variabel=="area_worst")
        {
          #med UNTUK VARIABEL radius_mean
          print(med_values[1,3])
        }else{
          #med UNTUK VARIABELtexture_mean
          print(med_values[1,4])
        }
      }else if(input$kategori=="B"){
        if(input$variabel=="radius_worst"){
          #med UNTUK VARIABEL ID
          print(med_values[2,2]) 
        }else if(input$variabel=="area_worst")
        {
          #med UNTUK VARIABEL radius_mean
          print(med_values[2,3])
        }else{
          #med UNTUK VARIABELtexture_mean
          print(med_values[2,4])
        }
      }else{
        if(input$variabel=="radius_worst"){
          #med UNTUK VARIABEL ID
          print(median(du[,1])) 
        }else if(input$variabel=="area_worst")
        {
          #med UNTUK VARIABEL radius_mean
          print(median(du[,2]))
        }else{
          #med UNTUK VARIABELtexture_mean
          print(median(du[,3]))
        }
      }
    })
    
    #Histogram
    output$histplot <- renderPlot({
      if(input$kategori2=="M&D"){
        du2 <- du[input$variabel2]
      }else{
        
        data_by_diagnosis <- split(du[input$variabel2], du$diagnosis==input$kategori2)
        du2 <- data_by_diagnosis[[2]] #2 -> TRUE
      }
      
      if(input$variabel2=="radius_worst"){
        hist(du2$radius_worst,
             main = "Histogram of Radius Worst",
             xlab = "Radius Worst",
             ylab = "Frequency",
             col = "skyblue",
             border = "black")
      }else if( input$variabel2=="area_worst"){
        hist(du2$area_worst,
             main = "Histogram of Area Worst",
             xlab = "Area Worst",
             ylab = "Frequency",
             col = "skyblue",
             border = "black")
      }else{
        hist(du2$concave.points_worst,
             main = "Histogram of Concave Points Worst",
             xlab = "Concave Point Worst",
             ylab = "Frequency",
             col = "skyblue",
             border = "black")
      }
      
    })
    
    #KORELASI
    output$corrplot <- renderPlot({
      if(input$kategori3=="M&D"){
        du3<-du[c(input$variabel3,input$variabel4)]
      }else{
        data_by_diagnosis3 <- split(du[c(input$variabel3,input$variabel4)], du$diagnosis==input$kategori3)
        du3 <- data_by_diagnosis3[[2]] #2 -> TRUE
      }
      if(input$variabel3=="radius_worst"){
        yt="Radius Worst"
        yin = du3$radius_worst
      }else if(input$variabel3=="area_worst"){
        yt="Area Worst"
        yin = du3$area_worst
      }else{
        yt="Concave Points Worst"
        yin=du3$concave.points_worst
      }
      if(input$variabel4=="radius_worst"){
        xt="Radius Worst"
        xin=du3$radius_worst
      }else if(input$variabel4=="area_worst"){
        xt="Area Worst"
        xin=du3$area_worst
      }else{
        xt="Concave Points Worst"
        xin=du3$concave.points_worst
      }
      cor_value <- cor(du3[input$variabel3], du3[input$variabel4])
      # Membuat scatter plot
      plot(xin, yin, 
           main = paste0(xt," Vs ",yt," Correlation Value : (",cor_value,")"), 
           xlab = xt, 
           ylab = yt, 
           pch = 19, 
           col = "blue",
           cex = 1.5)
      
    })
    
    #TESTING
    output$HasilSVM<- renderUI({
      input_matrix <- matrix(c(input$aw,input$rw,input$cpw),nrow = 1)
      svm_pred <- svm_model$predict(input_matrix)
      paste("Cancer:", svm_pred)
    })
    output$HasilCNN<- renderUI({
      input_matrix <- matrix(c(input$aw,input$rw,input$cpw),nrow = 1)
      nn_pred <- nn_model$predict(input_matrix)
      paste("Cancer:", nn_pred)
    })
    output$HasilRF<- renderUI({
      input_matrix <- matrix(c(input$aw,input$rw,input$cpw),nrow = 1)
      rf_pred <- rf_model$predict(input_matrix)
      paste("Cancer:", rf_pred)
    })
    # argonTable
    output$tabel_data <- renderDT({du})
    
  }
)


