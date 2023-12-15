library(shiny)
library(ggplot2)
plotly::ggplotly
plotly::renderPlotly
plotly::layout
library(fontawesome)
library(summaryBox)
library(fontawesome)
library(dplyr)

function(input, output, session) {
  
# -------------------------( Overview TAB )------------------------- #
{
# Overview-BTD
{
  #Dynamic Title Headers - UNUSED
  {
    # # result: ov_title -> returns title of Overview input
    # ov_title_btd = reactive({
    #   return(hours %>%
    #            distinct(`Contractor Name`) %>%
    #            dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
    #            pull()
    #   )
    # })
    # 
    # output$contTitle_ctd = renderText({
    #   cont_title_ctd()
    # })
  }
  # ----------------------------------------------------------------------- #   
  #Dynamic Overview Analysis Drop Downs - UNUSED
  {
    # # Filter proj_month_hours based on Job Number user input 
    # choices_year_ptd = reactive({
    #   return(proj_month_hours %>%
    #            dplyr::filter(`Project #` %in% input$projnum_input_ptd)
    #   )
    # })
    # 
    # # Update Year user input based on filtered proj_month_hours  
    # observe({
    #   if (length(choices_year_ptd()) > 0) {
    #     updateSelectInput(session = session, 
    #                       inputId = "year_input_ptd", 
    #                       choices = sort(unique(choices_year_ptd()$Year), decreasing = TRUE)
    #     )
    #   }
    # })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic btd-TRIR df for Tables and Plots
  { 
    # Create btd-trir function to call dataset based on Overview
    
    # TRIR df  
    btd_ov_recKPI = reactive({
      btd_ov_trir = ov_data_trir  %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  Recordables = sum(Recordables)) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeRec = cumsum(Recordables)) %>%
        mutate(TRIR = ((200000 * CumulativeRec) / CumulativeWH)) %>%
        mutate(TRIR = ifelse(is.na(TRIR), 0.00,
                             ifelse(CumulativeWH == 0, 0.00, round(TRIR, 2)))) %>%
        select(Year,Month,TRIR)
      # DART df
      btd_ov_dart = ov_data_dart %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  DaysAwayRec = sum(DaysAwayRec)) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeDart = cumsum(DaysAwayRec)) %>%
        mutate(DART = ((200000 * CumulativeDart) / CumulativeWH)) %>%
        mutate(DART = ifelse(is.na(DART), 0.00, 
                             ifelse(CumulativeWH == 0, 0.00, round(DART, 2)))) %>%
        select(Year,Month,DART)
      # LTIR df  
      btd_ov_ltir = ov_data_ltir %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  LostTimeRec = sum(LostTimeRec)) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeLtir = cumsum(LostTimeRec)) %>%
        mutate(LTIR = ((200000 * CumulativeLtir) / CumulativeWH)) %>%
        mutate(LTIR = ifelse(is.na(LTIR), 0.00, 
                             ifelse(CumulativeWH == 0, 0.00, round(LTIR, 2)))) %>%
        select(Year,Month,LTIR)
      
      # Combine TRIR,DART,LTIR dfs
      btd_combined_Rec_KPI = bind_rows(
        mutate(btd_ov_trir, Metric = "TRIR"),
        mutate(btd_ov_dart, Metric = "DART"),
        mutate(btd_ov_ltir, Metric = "LTIR")
      ) %>%
        mutate(Rates = coalesce(TRIR, DART, LTIR)) %>%
        select(-TRIR, -DART, -LTIR) %>%
        mutate(FullDate = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"),
               Year = factor(Year),
               Month = factor(Month, levels = sprintf("%02d", 1:12)),
               Year.Month = format(FullDate, "%Y.%m"))
      return(btd_combined_Rec_KPI)
    })
    
    # Render TRIR Plots for Contractor Analysis
    
    output$btd_ov_RecKPIplot = plotly::renderPlotly({
        #ptd_recKPI_data = ptd_proj_recKPI()  # Store the reactive value in a variable
        
        gg_btd_ov_recKPI = ggplot(btd_ov_recKPI(), aes(x = Year.Month, 
                                                      y = Rates,
                                                      group = Metric,
                                                      color = Metric,
                                                      text = paste(Metric,": ",Rates, "<br>",
                                                                   Year.Month))) +
          geom_line() +
          labs(title = "Recordable Case Rates", xlab="Month", ylab="Rates") +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1),
                legend.position = "left") +
          scale_y_continuous(breaks = seq(0, (max(btd_ov_recKPI()$Rates)+0.25), by = 1),
                             limits = c(0, (max(btd_ov_recKPI()$Rates)+0.25))) +
          scale_color_manual(values = c("TRIR" = "blue", "DART" = "red", "LTIR" = "green")) 
        # +
        #   scale_x_date(NULL, date_labels = "%m %y", breaks = "Month")
        
        plotly::layout(plotly::ggplotly(gg_btd_ov_recKPI, tooltip = "text"),
                       xaxis = list(range = list(max(as.numeric(btd_ov_recKPI()$Year)),
                                                 max(as.numeric(btd_ov_recKPI()$Year)))
                       )
        )
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic btd-Recordable df for Table of Recordable Cases Counts
  {
    # Create btd-rec function to call dataset based on Overview  
    btd_ov_rec_summary = reactive({
     #Create a Summary Rec Table
      btd_ov_rec_table = claims %>%
        dplyr::filter(`OSHA Classification` %in% c("Medical Only",
                                                   "Restricted Duty", 
                                                   "Lost Time Injury")) %>%
        group_by(`OSHA Classification`) %>%
        summarise(`Project #` = n()) %>%
        select(`OSHA Classification`, `Project #`) %>%
        rename(Recordables = `OSHA Classification`,
               Count = `Project #`) %>%
        arrange(desc(Count))
     
     #Create a Total Row
     btd_ov_total_row = data.frame(
       Recordables = "Total",
       Count = sum(btd_ov_rec_table$Count)
       )
     
     return(rbind(btd_ov_rec_table,
                  btd_ov_total_row)
            )
      
    })
    
    # Render Rec Plot for mtd-Project Analysis
    output$btd_ov_recTable = renderTable({
      btd_ov_rec_summary()
    })
  } 
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-btd-KPIs (Projects)
  {
    # KPI: # of Projects
    ov_btd_proj = reactive({
      return(hours %>%
               distinct(`Project #`) %>%
               summarise(Count = n()) %>%
               pull(Count)
      )
    })
    
    # Render Projects infoBoxOutput for btd-Project Analysis
    output$ov_btd_numProj = renderUI({
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Projects</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(ov_btd_proj() > 999999,
                              paste0((round(ov_btd_proj() / 1000000, digits = 0)),"M"),
                              ifelse(ov_btd_proj() > 999,
                                     paste0((round(ov_btd_proj() / 1000, digits = 0)),"K"),
                                     ov_btd_proj())
                       ),"</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-city","</div>"),
          style = "light"
        )
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-btd-KPIs (Contractors)
  {
    # KPI: # of Contractors
    ov_btd_cont = reactive({
      return(hours %>%
               distinct(`Contractor Name`) %>%
               summarise(Count = n()) %>%
               pull(Count)
      )
    })
    
    # Render Contractors infoBoxOutput for btd-Contractor Analysis
    output$ov_btd_numCont = renderUI({
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Contractors</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(ov_btd_cont() > 999999,
                              paste0((round(ov_btd_cont() / 1000000, digits = 0)),"M"),
                              ifelse(ov_btd_cont() > 999,
                                     paste0((round(ov_btd_cont() / 1000, digits = 0)),"K"),
                                     ov_btd_cont())
                       ),"</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-hard-hat","</div>"),
          style = "light"
        )
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-btd-KPIs (workers)
  {
    #KPI: # of Workers
    ov_btd_Wrkr = reactive({
      return(hours %>%
               summarise(`Worker Count` = sum(`Worker Count`)) %>%
               pull(`Worker Count`)
             )
    })
    
    # Render Workers infoBoxOutput for ptd-Project Analysis
    output$ov_btd_numWrk = renderUI({
        summaryBox2(
          title = HTML("<div style='text-align: right;'>","Workers","</div>"),
          value = HTML("<div style='text-align: right; margin-right: 10px;'>",
                       ifelse(ov_btd_Wrkr() > 999999, 
                              paste0((round(ov_btd_Wrkr() / 1000000, digits = 0)),"M"), 
                              ifelse(ov_btd_Wrkr() > 999,
                                     paste0((round(ov_btd_Wrkr() / 1000, digits = 0)),"K"),
                                     ov_btd_Wrkr())
                       )
                       ,"</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-person-running","</div>"),
          style = "light"
        )
    })  
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-btd-KPIs (hours)
  {
    # KPI: # of hours
    ov_btd_hours = reactive({
      return(ov_month_hours %>%
               ungroup() %>%
               summarise(Workhours = sum(Workhours)) %>%
               pull(Workhours)
      )
    })
    
    # Render hours infoBoxOutput for btd-Overview Analysis
    output$ov_btd_numhour = renderUI({
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Work-Hours</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(ov_btd_hours() > 999999, 
                              paste0((round(ov_btd_hours() / 1000000, digits = 1)),"M"), 
                              ifelse(ov_btd_hours() > 999,
                                     paste0((round(ov_btd_hours() / 1000, digits = 0)),"K"),
                                     ov_btd_hours())
                       ),
                       "</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-clock","</div>"),
          style = "light"
        )
    })  
  }
  # ----------------------------------------------------------------------- #  
  # Dynamic ov-btd-KPIs (TRIR)
  {
    # KPI: TRIR
    ov_btd_trir = reactive({
      return(ov_data_trir  %>%
               ungroup() %>%
               summarise(Workhours = sum(Workhours),
                         Recordables = sum(Recordables)) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
               mutate(TRIR = ifelse(TRIR == 0, as.character("0.00"), round(TRIR, 2))) %>%
               pull(TRIR)
      )
    })
    
    # Render TRIR infoBoxOutput for ctd-Contractor Analysis
    output$ov_btd_trir = renderUI({
        summaryBox3(
          title = HTML("<center style='color: blue;'>TRIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       ov_btd_trir(),
                       "</p></center>"),
          style = "primary",
          width = 4,
          icon = F
        )
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic ov-btd-KPIs (DART)
  {
    # KPI: DART
    ov_btd_dart = reactive({
      return(ov_data_dart  %>%
               ungroup() %>%
               summarise(Workhours = sum(Workhours),
                         DaysAwayRec = sum(DaysAwayRec)) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
               mutate(DART = ifelse(DART == 0, as.character("0.00"), round(DART, 2))) %>%
               pull(DART)
      )
    })
    
    # Render DART infoBoxOutput for btd-Contractor Analysis
    output$ov_btd_dart = renderUI({
        summaryBox3(
          title = HTML("<center style='color: red;'>DART</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       ov_btd_dart(),
                       "</p></center>"),
          style = "danger",
          width = 4,
          icon = F
        )
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic ov-btd-KPIs (LTIR)
  {
    # KPI: LTIR
    ov_btd_ltir = reactive({
      return(ov_data_ltir  %>%
               ungroup() %>%
               summarise(Workhours = sum(Workhours),
                         LostTimeRec = sum(LostTimeRec)) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
               mutate(LTIR = ifelse(LTIR == 0, as.character("0.00"), round(LTIR, 2))) %>%
               pull(LTIR)
      )
    })
    
    # Render LTIR infoBoxOutput for ptd-Project Analysis
    output$ov_btd_ltir = renderUI({
        summaryBox3(
          title = HTML("<center style='color: green;'>LTIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       ov_btd_ltir(),
                       "</p></center>"),
          style = "success",
          width = 4,
          icon = F
        )
    })  
  }  
}

# Overview-YTD
{  
  #Dynamic Title Headers - UNUSED
  {
    # Checkbox to allow multiple project selection
    # multiple_proj_sel = reactive({
    #   if (input$checkbox_input) {
    #     return(TRUE)
    #   } else {
    #     return(FALSE)
    #   }
    # })
    # 
    # observe({
    #   updateSelectInput(session, "projnum_input", multiple = multiple_proj_sel())
    # })
  #   
  #   
  #   # result: cont_title -> returns title of Contractor input
  #   cont_title_ytd = reactive({
  #     return(hours %>%
  #              distinct(`Contractor Name`) %>%
  #              dplyr::filter(`Contractor Name` %in% input$cont_input_ytd) %>%
  #              pull())
  #   })
  #   
  #   output$contTitle_ytd = renderText({
  #     cont_title_ytd()
  #   })
  }
  # ----------------------------------------------------------------------- #   
  #Dynamic Contractor Analysis Drop Downs
  {
    # Filter cont_month_hours based on Contractor Name user input 
    choices_year_ytd_ov = reactive({
      return(ov_month_hours)
    })
    
    # Update Year user input based on filtered cont_month_hours  
    observe({
      if (length(choices_year_ytd_ov()) > 0) {
        updateSelectInput(session = session, 
                          inputId = "year_input_ytd_ov", 
                          choices = sort(unique(choices_year_ytd_ov()$Year), decreasing = TRUE)
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ytd-TRIR df for Tables and Plots
  { 
    # Create ytd-trir function to call dataset based on Overview
    
    # TRIR df  
    ytd_ov_recKPI = reactive({
      ytd_ov_trir = ov_data_trir  %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  Recordables = sum(Recordables)) %>%
        dplyr::filter(Year == input$year_input_ytd_ov) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeRec = cumsum(Recordables)) %>%
        mutate(TRIR = ((200000 * CumulativeRec) / CumulativeWH)) %>%
        mutate(TRIR = ifelse(is.na(TRIR), 0.00,
                             ifelse(CumulativeWH == 0, 0.00, round(TRIR, 2)))) %>%
        select(Year,Month,TRIR)
      # DART df
      ytd_ov_dart = ov_data_dart %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  DaysAwayRec = sum(DaysAwayRec)) %>%
        dplyr::filter(Year == input$year_input_ytd_ov) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeDart = cumsum(DaysAwayRec)) %>%
        mutate(DART = ((200000 * CumulativeDart) / CumulativeWH)) %>%
        mutate(DART = ifelse(is.na(DART), 0.00, 
                             ifelse(CumulativeWH == 0, 0.00, round(DART, 2)))) %>%
        select(Year,Month,DART)
      # LTIR df  
      ytd_ov_ltir = ov_data_ltir %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  LostTimeRec = sum(LostTimeRec)) %>%
        dplyr::filter(Year == input$year_input_ytd_ov) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeLtir = cumsum(LostTimeRec)) %>%
        mutate(LTIR = ((200000 * CumulativeLtir) / CumulativeWH)) %>%
        mutate(LTIR = ifelse(is.na(LTIR), 0.00, 
                             ifelse(CumulativeWH == 0, 0.00, round(LTIR, 2)))) %>%
        select(Year,Month,LTIR)
      
      # Combine TRIR,DART,LTIR dfs
      ytd_ov_combined_Rec_KPI = bind_rows(
        mutate(ytd_ov_trir, Metric = "TRIR"),
        mutate(ytd_ov_dart, Metric = "DART"),
        mutate(ytd_ov_ltir, Metric = "LTIR")
      ) %>%
        mutate(Rates = coalesce(TRIR, DART, LTIR)) %>%
        select(-TRIR, -DART, -LTIR)
      return(ytd_ov_combined_Rec_KPI)
    })
    
    # Render TRIR Plots for Project Analysis
    
    output$ytd_ov_RecKPIplot = plotly::renderPlotly({
      if (length(input$year_input_ytd_ov) > 0) {
        #ytd_recKPI_data = ytd_proj_recKPI()  # Store the reactive value in a variable
        
        gg_ytd_ov_recKPI = ggplot(ytd_ov_recKPI(), aes(x = Month, 
                                                           y = Rates,
                                                           group = Metric,
                                                           color = Metric,
                                                           text = paste(Metric,": ",Rates))) +
          geom_line() +
          labs(title = "Recordable Case Rates", xlab="Month", ylab="Rates") +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1),
                legend.position = "left") +
          scale_y_continuous(breaks = seq(0, (max(ytd_ov_recKPI()$Rates)+0.25), by = 1),
                             limits = c(0, (max(ytd_ov_recKPI()$Rates)+0.25))) +
          scale_color_manual(values = c("TRIR" = "blue", "DART" = "red", "LTIR" = "green"))
      }
      
      plotly::ggplotly(gg_ytd_ov_recKPI, tooltip = "text")
      
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ytd-Recordable df for Table of Recordable Cases Counts
  {
    # Create ytd-rec function to call dataset based on Overview  
    ytd_ov_rec_summary = reactive({
      #Create a Summary Rec Table
      ytd_ov_rec_table = claims %>%
        dplyr::filter(`OSHA Classification` %in% c("Medical Only",
                                                   "Restricted Duty", 
                                                   "Lost Time Injury"),
                      Year == input$year_input_ytd_ov) %>%
        group_by(`OSHA Classification`) %>%
        summarise(`Project #` = n()) %>%
        select(`OSHA Classification`, `Project #`) %>%
        rename(Recordables = `OSHA Classification`,
               Count = `Project #`) %>%
        arrange(desc(Count))
      
      #Create a Total Row
      ytd_ov_total_row = data.frame(
        Recordables = "Total",
        Count = sum(ytd_ov_rec_table$Count)
      )
      
      return(rbind(ytd_ov_rec_table,
                   ytd_ov_total_row)
      )
      
    })
    
    # Render Rec Plot for mtd-Project Analysis
    output$ytd_ov_recTable = renderTable({
      ytd_ov_rec_summary()
    })
  } 
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-ytd-KPIs (Projects)
  {
    # KPI: # of Projects
    ov_ytd_proj = reactive({
      return(hours %>%
               dplyr::filter(Year %in% input$year_input_ytd_ov) %>%
               distinct(`Project #`) %>%
               summarise(Count = n()) %>%
               pull(Count)
      )
    })
    
    # Render Projects infoBoxOutput for ytd-Overview Analysis
    output$ov_ytd_numProj = renderUI({
      if (length(input$year_input_ytd_ov) > 0) {
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Projects</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(ov_ytd_proj() > 999999,
                              paste0((round(ov_ytd_proj() / 1000000, digits = 0)),"M"),
                              ifelse(ov_ytd_proj() > 999,
                                     paste0((round(ov_ytd_proj() / 1000, digits = 0)),"K"),
                                     ov_ytd_proj())
                       ),"</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-city","</div>"),
          style = "light"
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-ytd-KPIs (Contractors)
  {
    # KPI: # of Contractors
    ov_ytd_cont = reactive({
      return(hours %>%
               dplyr::filter(Year %in% input$year_input_ytd_ov) %>%
               distinct(`Contractor Name`) %>%
               summarise(Count = n()) %>%
               pull(Count)
      )
    })
    
    # Render Contractors infoBoxOutput for ytd-Overview Analysis
    output$ov_ytd_numCont = renderUI({
      if (length(input$year_input_ytd_ov) > 0) {
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Contractors</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(ov_ytd_cont() > 999999,
                              paste0((round(ov_ytd_cont() / 1000000, digits = 0)),"M"),
                              ifelse(ov_ytd_cont() > 999,
                                     paste0((round(ov_ytd_cont() / 1000, digits = 0)),"K"),
                                     ov_ytd_cont())
                       ),"</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-hard-hat","</div>"),
          style = "light"
        )
      }
    })
  }  
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-ytd-KPIs (workers)
  {
    #KPI: # of Workers
    ov_ytd_Wrkr = reactive({
      return(hours %>%
               dplyr::filter(Year %in% input$year_input_ytd_ov) %>%
               group_by(Year) %>%
               summarise(`Worker Count` = sum(`Worker Count`)) %>%
               pull(`Worker Count`)
      )
    })
    
    # Render Workers infoBoxOutput for ytd-Overview Analysis
    output$ov_ytd_numWrk = renderUI({
      if (length(input$year_input_ytd_ov) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>","Workers","</div>"),
          value = HTML("<div style='text-align: right; margin-right: 10px;'>",
                       ifelse(ov_ytd_Wrkr() > 999999, 
                              paste0((round(ov_ytd_Wrkr() / 1000000, digits = 0)),"M"), 
                              ifelse(ov_ytd_Wrkr() > 999,
                                     paste0((round(ov_ytd_Wrkr() / 1000, digits = 0)),"K"),
                                     ov_ytd_Wrkr())
                       )
                       ,"</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-person-running","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-ytd-KPIs (hours)
  {
    # KPI: # of hours
    ov_ytd_hours = reactive({
      return(ov_month_hours %>%
               dplyr::filter(Year %in% input$year_input_ytd_ov) %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours)) %>%
               pull(Workhours)
      )
    })
    
    # Render hours infoBoxOutput for ytd-Overview Analysis
    output$ov_ytd_numhour = renderUI({
      if (length(input$year_input_ytd_ov) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Work-Hours</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(ov_ytd_hours() > 999999, 
                              paste0((round(ov_ytd_hours() / 1000000, digits = 1)),"M"), 
                              ifelse(ov_ytd_hours() > 999,
                                     paste0((round(ov_ytd_hours() / 1000, digits = 0)),"K"),
                                     ov_ytd_hours())
                       ),
                       "</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-clock","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #  
  # Dynamic ov-ytd-KPIs (TRIR)
  {
    # KPI: TRIR
    ov_ytd_trir = reactive({
      return(ov_data_trir  %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours),
                         Recordables = sum(Recordables)) %>%
               dplyr::filter(Year == input$year_input_ytd_ov) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
               mutate(TRIR = ifelse(TRIR == 0, as.character("0.00"), round(TRIR, 2))) %>%
               pull(TRIR)
      )
    })
    
    # Render TRIR infoBoxOutput for ytd-Overview Analysis
    output$ov_ytd_trir = renderUI({
      if (length(input$year_input_ytd_ov) > 0) {
        summaryBox3(
          title = HTML("<center style='color: blue;'>TRIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       ov_ytd_trir(),
                       "</p></center>"),
          style = "primary",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic ov-ytd-KPIs (DART)
  {
    # KPI: DART
    ov_ytd_dart = reactive({
      return(ov_data_dart  %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours),
                         DaysAwayRec = sum(DaysAwayRec)) %>%
               dplyr::filter(Year == input$year_input_ytd_ov) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
               mutate(DART = ifelse(DART == 0, as.character("0.00"), round(DART, 2))) %>%
               pull(DART)
      )
    })
    
    # Render DART infoBoxOutput for ytd-Overview Analysis
    output$ov_ytd_dart = renderUI({
      if (length(input$year_input_ytd_ov) > 0) {
        summaryBox3(
          title = HTML("<center style='color: red;'>DART</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       ov_ytd_dart(),
                       "</p></center>"),
          style = "danger",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic ov-ytd-KPIs (LTIR)
  {
    # KPI: LTIR
    ov_ytd_ltir = reactive({
      return(ov_data_ltir  %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours),
                         LostTimeRec = sum(LostTimeRec)) %>%
               dplyr::filter(Year == input$year_input_ytd_ov) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
               mutate(LTIR = ifelse(LTIR == 0, as.character("0.00"), round(LTIR, 2))) %>%
               pull(LTIR)
      )
    })
    
    # Render LTIR infoBoxOutput for ytd-Overview Analysis
    output$ov_ytd_ltir = renderUI({
      if (length(input$year_input_ytd_ov) > 0) {
        summaryBox3(
          title = HTML("<center style='color: green;'>LTIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       ov_ytd_ltir(),
                       "</p></center>"),
          style = "success",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  
}

# Overview-MTD
{  
  #Dynamic Title Headers - UNUSED
  {
    # Checkbox to allow multiple project selection
    # multiple_proj_sel = reactive({
    #   if (input$checkbox_input) {
    #     return(TRUE)
    #   } else {
    #     return(FALSE)
    #   }
    # })
    # 
    # observe({
    #   updateSelectInput(session, "projnum_input", multiple = multiple_proj_sel())
    # })
    
    
    # result: cont_title -> returns title of Contractor input
  #   ov_title_mtd = reactive({
  #     return(hours %>%
  #              distinct(`Contractor Name`) %>%
  #              dplyr::filter(`Contractor Name` %in% input$cont_input_mtd) %>%
  #              pull())
  #   })
  #   
  #   output$contTitle_mtd = renderText({
  #     cont_title_mtd()
  #   })
  }
  # ----------------------------------------------------------------------- #   
  #Dynamic Overview Analysis Drop Downs
  {
    # Filter ov_month_hours based on Overview 
    choices_year_mtd_ov= reactive({
      return(ov_month_hours)
    })
    
    # Update Year user input based on filtered ov_month_hours  
    observe({
      if (length(choices_year_mtd_ov()) > 0) {
        updateSelectInput(session = session, 
                          inputId = "year_input_mtd_ov", 
                          choices = sort(unique(choices_year_mtd_ov()$Year), decreasing = TRUE)
        )
      }
    })
    
    # Filter ov_month_hours based on Overview 
    choices_month_mtd_ov = reactive({
      return(ov_month_hours %>%
               dplyr::filter(Year == input$year_input_mtd_ov)
      )
    })
    
    # Update Month user input based on filtered ov_month_hours  
    observe({
      if (length(choices_month_mtd_ov()) > 0) {
        updateSelectInput(session = session, 
                          inputId = "month_input_mtd_ov", 
                          choices = sort(unique(choices_month_mtd_ov()$Month), decreasing = TRUE)
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic mtd-TRIR df for Tables and Plots
  { 
    # Create ov_mtd-trir function to call dataset based on Overview and Year input
    
    # TRIR df  
    ov_mtd_recKPI = reactive({
      mtd_ov_trir = ov_data_trir  %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  Recordables = sum(Recordables)) %>%
        dplyr::filter(Year == input$year_input_mtd_ov) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
        mutate(TRIR = ifelse(is.na(TRIR), 0.00,
                             ifelse(Workhours == 0, 0.00, round(TRIR, 2)))) %>%
        select(Year,Month,TRIR)
      # DART df
      mtd_ov_dart = ov_data_dart %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  DaysAwayRec = sum(DaysAwayRec)) %>%
        dplyr::filter(Year == input$year_input_mtd_ov) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
        mutate(DART = ifelse(is.na(DART), 0.00,
                             ifelse(Workhours == 0, 0.00, round(DART, 2)))) %>%
        select(Year,Month,DART)
      # LTIR df  
      mtd_ov_ltir = ov_data_ltir %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  LostTimeRec = sum(LostTimeRec)) %>%
        dplyr::filter(Year == input$year_input_mtd_ov) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
        mutate(LTIR = ifelse(is.na(LTIR), 0.00,
                             ifelse(Workhours == 0, 0.00, round(LTIR, 2)))) %>%
        select(Year,Month,LTIR)
      # Combine TRIR,DART,LTIR dfs
      mtd_ov_combined_Rec_KPI = bind_rows(
        mutate(mtd_ov_trir, group = "TRIR"),
        mutate(mtd_ov_dart, group = "DART"),
        mutate(mtd_ov_ltir, group = "LTIR")
      ) %>%
        mutate(Rates = coalesce(TRIR, DART, LTIR)) %>%
        select(-TRIR, -DART, -LTIR)
      return(mtd_ov_combined_Rec_KPI)
    })
    
    # Render TRIR Plots for Overview Analysis
    
    output$mtd_ov_RecKPIplot = plotly::renderPlotly({
      if (length(input$year_input_mtd_ov) > 0 & length(input$month_input_mtd_ov) > 0) {
        gg_mtd_ov_recKPI = ggplot(ov_mtd_recKPI(), aes(x = Month, 
                                                           y = Rates,
                                                           group = group,
                                                           color = group,
                                                           text = paste(group,": ",Rates))) +
          geom_line() +
          labs(title = "Recordable Case Rates", xlab("Month"), ylab("Rates")) +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1),
                legend.position = "top") +
          scale_y_continuous(breaks = seq(0, (max(ov_mtd_recKPI()$Rates)+0.5), by = 1),
                             limits = c(0, (max(ov_mtd_recKPI()$Rates)+0.5))) +
          scale_color_manual(values = c("TRIR" = "blue", "DART" = "red", "LTIR" = "green"))
      }
      
      plotly::ggplotly(gg_mtd_ov_recKPI, tooltip = "text")
      
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic mtd-Recordable df for Table of Recordable Cases Counts
  {
    # Create mtd-rec function to call dataset based on Overview  
    mtd_ov_rec_summary = reactive({
      #Create a Summary Rec Table
      mtd_ov_rec_table = claims %>%
        dplyr::filter(`OSHA Classification` %in% c("Medical Only",
                                                   "Restricted Duty", 
                                                   "Lost Time Injury"),
                      Year == input$year_input_ytd_ov,
                      Month == input$month_input_mtd_ov) %>%
        group_by(`OSHA Classification`) %>%
        summarise(`Project #` = n()) %>%
        select(`OSHA Classification`, `Project #`) %>%
        rename(Recordables = `OSHA Classification`,
               Count = `Project #`) %>%
        arrange(desc(Count))
      
      #Create a Total Row
      mtd_ov_total_row = data.frame(
        Recordables = "Total",
        Count = sum(mtd_ov_rec_table$Count)
      )
      
      return(rbind(mtd_ov_rec_table,
                   mtd_ov_total_row)
      )
      
    })
    
    # Render Rec Plot for mtd-Project Analysis
    output$mtd_ov_recTable = renderTable({
      mtd_ov_rec_summary()
    })
  } 
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-mtd-KPIs (Projects)
  {
    # KPI: # of Projects
    ov_mtd_proj = reactive({
      return(hours %>%
               dplyr::filter(Year %in% input$year_input_mtd_ov,
                             Month %in% input$month_input_mtd_ov) %>%
               distinct(`Project #`) %>%
               summarise(Count = n()) %>%
               pull(Count)
      )
    })
    
    # Render Projects infoBoxOutput for mtd-Overview Analysis
    output$ov_mtd_numProj = renderUI({
      if (length(input$year_input_mtd_ov) > 0 & length(input$month_input_mtd_ov) > 0) {
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Projects</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(ov_mtd_proj() > 999999,
                              paste0((round(ov_mtd_proj() / 1000000, digits = 0)),"M"),
                              ifelse(ov_mtd_proj() > 999,
                                     paste0((round(ov_mtd_proj() / 1000, digits = 0)),"K"),
                                     ov_mtd_proj())
                       ),"</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-city","</div>"),
          style = "light"
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-mtd-KPIs (Contractors)
  {
    # KPI: # of Contractors
    ov_mtd_cont = reactive({
      return(hours %>%
               dplyr::filter(Year %in% input$year_input_mtd_cont,
                             Month %in% input$month_input_mtd_cont) %>%
               distinct(`Contractor Name`) %>%
               summarise(Count = n()) %>%
               pull(Count)
      )
    })
    
    # Render Contractors infoBoxOutput for mtd-Overview Analysis
    output$ov_mtd_numCont = renderUI({
      if (length(input$year_input_mtd_ov) > 0 & length(input$month_input_mtd_ov) > 0) {
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Contractors</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(ov_mtd_cont() > 999999,
                              paste0((round(ov_mtd_cont() / 1000000, digits = 0)),"M"),
                              ifelse(ov_mtd_cont() > 999,
                                     paste0((round(ov_mtd_cont() / 1000, digits = 0)),"K"),
                                     ov_mtd_cont())
                       ),"</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-hard-hat","</div>"),
          style = "light"
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-mtd-KPIs (workers)
  {
    #KPI: # of Workers
    ov_mtd_Wrkr = reactive({
      return(hours %>%
               dplyr::filter(Year %in% input$year_input_mtd_cont,
                             Month %in% input$month_input_mtd_cont) %>%
               summarise(`Worker Count` = sum(`Worker Count`)) %>%
               pull(`Worker Count`)
      )
    })
    
    # Render Workers infoBoxOutput for mtd-Overview Analysis
    output$ov_mtd_numWrk = renderUI({
      if (length(input$year_input_mtd_ov) > 0 & length(input$month_input_mtd_ov) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>","Workers","</div>"),
          value = HTML("<div style='text-align: right; margin-right: 10px;'>",
                       ifelse(ov_mtd_Wrkr() > 999999, 
                              paste0((round(ov_mtd_Wrkr() / 1000000, digits = 0)),"M"), 
                              ifelse(ov_mtd_Wrkr() > 999,
                                     paste0((round(ov_mtd_Wrkr() / 1000, digits = 0)),"K"),
                                     ov_mtd_Wrkr())
                       )
                       ,"</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-person-running","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ov-mtd-KPIs (hours)
  {
    # KPI: # of hours
    ov_mtd_hours = reactive({
      return(ov_month_hours %>%
               dplyr::filter(Year %in% input$year_input_mtd_cont,
                             Month %in% input$month_input_mtd_cont) %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours)) %>%
               pull(Workhours)
      )
    })
    
    # Render hours infoBoxOutput for mtd-Overview Analysis
    output$ov_mtd_numhour = renderUI({
      if (length(input$year_input_mtd_ov) > 0 & length(input$month_input_mtd_ov) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Work-Hours</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(ov_mtd_hours() > 999999, 
                              paste0((round(ov_mtd_hours() / 1000000, digits = 1)),"M"), 
                              ifelse(ov_mtd_hours() > 999,
                                     paste0((round(ov_mtd_hours() / 1000, digits = 0)),"K"),
                                     ov_mtd_hours())
                       ),
                       "</div>"),
          width = 3,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-clock","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #  
  # Dynamic ov-mtd-KPIs (TRIR)
  {
    # KPI: TRIR
    ov_mtd_trir = reactive({
      return(ov_data_trir  %>%
               dplyr::filter(Year %in% input$year_input_mtd_ov) %>%
               group_by(Month) %>%
               summarise(Workhours = sum(Workhours),
                         Recordables = sum(Recordables)) %>%
               dplyr::filter(Month %in% input$month_input_mtd_ov) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
               mutate(TRIR = ifelse(TRIR == 0, as.character("0.00"), round(TRIR, 2))) %>%
               pull(TRIR)
      )
    })
    
    # Render TRIR infoBoxOutput for mtd-Overview Analysis
    output$ov_mtd_trir = renderUI({
      if (length(input$year_input_mtd_ov) > 0 & length(input$month_input_mtd_ov) > 0) {
        summaryBox3(
          title = HTML("<center style='color: blue;'>TRIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       ov_mtd_trir(),
                       "</p></center>"),
          style = "primary",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic ov-mtd-KPIs (DART)
  {
    # KPI: DART
    ov_mtd_dart = reactive({
      return(ov_data_dart  %>%
               dplyr::filter(Year %in% input$year_input_mtd_ov) %>%
               group_by(Month) %>%
               summarise(Workhours = sum(Workhours),
                         DaysAwayRec = sum(DaysAwayRec)) %>%
               dplyr::filter(Month %in% input$month_input_mtd_ov) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
               mutate(DART = ifelse(DART == 0, as.character("0.00"), round(DART, 2))) %>%
               pull(DART)
      )
    })
    
    # Render DART infoBoxOutput for mtd-Overview Analysis
    output$ov_mtd_dart = renderUI({
      if (length(input$year_input_mtd_ov) > 0 & length(input$month_input_mtd_ov) > 0) {
        summaryBox3(
          title = HTML("<center style='color: red;'>DART</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       ov_mtd_dart(),
                       "</p></center>"),
          style = "danger",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic ov-mtd-KPIs (LTIR)
  {
    # KPI: LTIR
    ov_mtd_ltir = reactive({
      return(ov_data_ltir  %>%
               dplyr::filter(Year %in% input$year_input_mtd_ov) %>%
               group_by(Month) %>%
               summarise(Workhours = sum(Workhours),
                         LostTimeRec = sum(LostTimeRec)) %>%
               dplyr::filter(Month %in% input$month_input_mtd_ov) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
               mutate(LTIR = ifelse(LTIR == 0, as.character("0.00"), round(LTIR, 2))) %>%
               pull(LTIR)
      )
    })
    
    # Render LTIR infoBoxOutput for mtd-Project Analysis
    output$ov_mtd_ltir = renderUI({
      if (length(input$year_input_mtd_ov) > 0 & length(input$month_input_mtd_ov) > 0) {
        summaryBox3(
          title = HTML("<center style='color: green;'>LTIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       ov_mtd_ltir(),
                       "</p></center>"),
          style = "success",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  
}  
}
# -------------------------( Project TAB )------------------------- #
{
# Projects-PTD
{
#Dynamic Title Headers
{
  # Checkbox to allow multiple project selection
  # multiple_proj_sel = reactive({
  #   if (input$checkbox_input) {
  #     return(TRUE)
  #   } else {
  #     return(FALSE)
  #   }
  # })
  # 
  # observe({
  #   updateSelectInput(session, "projnum_input", multiple = multiple_proj_sel())
  # })
  
  
  # result: proj_title -> returns title of Project # input
  proj_title_ptd = reactive({
    return(proj_total_hours %>%
             ungroup %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ptd) %>%
             select(`Project Name`) %>%
             pull())
  })
  
  output$projTitle_ptd = renderText({
    proj_title_combined_ptd = paste(proj_title_ptd(), collapse = ", ")
    proj_title_combined_ptd
  })
}
# ----------------------------------------------------------------------- #   
#Dynamic Project Analysis Drop Downs
{
  # # Filter proj_month_hours based on Job Number user input 
  # choices_year_ptd = reactive({
  #   return(proj_month_hours %>%
  #            dplyr::filter(`Project #` %in% input$projnum_input_ptd)
  #   )
  # })
  # 
  # # Update Year user input based on filtered proj_month_hours  
  # observe({
  #   if (length(choices_year_ptd()) > 0) {
  #     updateSelectInput(session = session, 
  #                       inputId = "year_input_ptd", 
  #                       choices = sort(unique(choices_year_ptd()$Year), decreasing = TRUE)
  #     )
  #   }
  # })
}
# ----------------------------------------------------------------------- #   
# Dynamic ptd-TRIR df for Tables and Plots
{ 
  # Create ptd-trir function to call dataset based on user Project # and Year input
 
  # TRIR df  
  ptd_proj_recKPI = reactive({
    ptd_proj_trir = data_trir  %>%
      dplyr::filter(`Project #` %in% input$projnum_input_ptd) %>%
      group_by(Year, Month) %>%
      summarise(Workhours = sum(Workhours),
                Recordables = sum(Recordables)) %>%
      ungroup() %>%
      # group_by(`Project #`, `Project Name`) %>%
      mutate(CumulativeWH = cumsum(Workhours)) %>%
      mutate(CumulativeRec = cumsum(Recordables)) %>%
      mutate(TRIR = ((200000 * CumulativeRec) / CumulativeWH)) %>%
      mutate(TRIR = ifelse(is.na(TRIR), 0.00,
                           ifelse(CumulativeWH == 0, 0.00, round(TRIR, 2)))) %>%
      select(Year,Month,TRIR)
    # DART df
    ptd_proj_dart = data_dart %>%
      dplyr::filter(`Project #` %in% input$projnum_input_ptd) %>%
      group_by(Year, Month) %>%
      summarise(Workhours = sum(Workhours),
                DaysAwayRec = sum(DaysAwayRec)) %>%
      ungroup() %>%
      # group_by(`Project #`, `Project Name`) %>%
      mutate(CumulativeWH = cumsum(Workhours)) %>%
      mutate(CumulativeDart = cumsum(DaysAwayRec)) %>%
      mutate(DART = ((200000 * CumulativeDart) / CumulativeWH)) %>%
      mutate(DART = ifelse(is.na(DART), 0.00, 
                           ifelse(CumulativeWH == 0, 0.00, round(DART, 2)))) %>%
      select(Year,Month,DART)
    # LTIR df  
    ptd_proj_ltir = data_ltir %>%
      dplyr::filter(`Project #` %in% input$projnum_input_ptd) %>%
      group_by(Year, Month) %>%
      summarise(Workhours = sum(Workhours),
                LostTimeRec = sum(LostTimeRec)) %>%
      ungroup() %>%
      # group_by(`Project #`, `Project Name`) %>%
      mutate(CumulativeWH = cumsum(Workhours)) %>%
      mutate(CumulativeLtir = cumsum(LostTimeRec)) %>%
      mutate(LTIR = ((200000 * CumulativeLtir) / CumulativeWH)) %>%
      mutate(LTIR = ifelse(is.na(LTIR), 0.00, 
                           ifelse(CumulativeWH == 0, 0.00, round(LTIR, 2)))) %>%
      select(Year,Month,LTIR)
    
    # Combine TRIR,DART,LTIR dfs
    ptd_combined_Rec_KPI = bind_rows(
      mutate(ptd_proj_trir, Metric = "TRIR"),
      mutate(ptd_proj_dart, Metric = "DART"),
      mutate(ptd_proj_ltir, Metric = "LTIR")
    ) %>%
      mutate(Rates = coalesce(TRIR, DART, LTIR)) %>%
      select(-TRIR, -DART, -LTIR) %>%
      mutate(FullDate = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"),
             Year = factor(Year),
             Month = factor(Month, levels = sprintf("%02d", 1:12)),
             Year.Month = format(FullDate, "%Y.%m"))
    return(ptd_combined_Rec_KPI)
  })
  
  # Render TRIR Plots for Project Analysis
  
  output$ptdRecKPIplot = plotly::renderPlotly({
    if (length(input$projnum_input_ptd) > 0) {
      #ptd_recKPI_data = ptd_proj_recKPI()  # Store the reactive value in a variable
      
      gg_ptd_recKPI = ggplot(ptd_proj_recKPI(), aes(x = Year.Month, 
                                                    y = Rates,
                                                    group = Metric,
                                                    color = Metric,
                                                    text = paste(Metric,": ",Rates, "<br>",
                                                                 Year.Month))) +
        geom_line() +
        labs(title = "Recordable Case Rates", xlab="Month", ylab="Rates") +
        theme_minimal() + 
        theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
              axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1),
              legend.position = "left") +
        scale_y_continuous(breaks = seq(0, (max(ptd_proj_recKPI()$Rates)+0.25), by = 1),
                           limits = c(0, (max(ptd_proj_recKPI()$Rates)+0.25))) +
        scale_color_manual(values = c("TRIR" = "blue", "DART" = "red", "LTIR" = "green")) 
      # +
      #   scale_x_date(NULL, date_labels = "%m %y", breaks = "Month")
    
    plotly::layout(plotly::ggplotly(gg_ptd_recKPI, tooltip = "text"),
                   xaxis = list(range = list(max(as.numeric(ptd_proj_recKPI()$Year)),
                                             max(as.numeric(ptd_proj_recKPI()$Year)))
                                )
                   )
    }
  })
}
# ----------------------------------------------------------------------- #   
# Dynamic ptd-Recordable df for Table and plots
{
  # Create ptd-rec function to call dataset based on user Project # and Year input  
  ptd_proj_rec = reactive({
    return(data_trir  %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ptd) %>%
             group_by(Year, Month) %>%
             summarise(Recordables = sum(Recordables), 
                       `OSHA Classification` = paste(`OSHA Classification`[!is.na(`OSHA Classification`)], collapse = ", ")) %>%
             ungroup() %>%
             mutate(CumulativeRec = cumsum(Recordables)) %>%
             mutate(FullDate = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"),
                    Year = factor(Year),
                    Month = factor(Month, levels = sprintf("%02d", 1:12)),
                    Year.Month = format(FullDate, "%Y.%m"))
    )
  })
  
  # Render Rec Plot for mtd-Project Analysis
  output$ptdrecplot = plotly::renderPlotly({
    if (length(input$projnum_input_ptd) > 0) {
      
      gg_ptd_rec = ggplot(ptd_proj_rec(), aes(x = Year.Month, y = CumulativeRec, group = 1, text = `OSHA Classification`)) +
        geom_line(color = "orange") +
        labs(title = "Recordables", xlab = "Year.Month", ylab = "Recordable Count") +
        theme_minimal() + 
        theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
              axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1)) +
        scale_y_continuous(breaks = seq(0, (max(ptd_proj_rec()$CumulativeRec)+0.5), by = 1),
                           limits = c(0, (max(ptd_proj_rec()$CumulativeRec)+0.5)))
      
      plotly::layout(plotly::ggplotly(gg_ptd_rec, tooltip = "text"),
                     xaxis = list(range = list(max(as.numeric(ptd_proj_rec()$Year)),
                                               max(as.numeric(ptd_proj_rec()$Year)))
      )
  )
    }
  })
} 
# ----------------------------------------------------------------------- #   
# Dynamic proj-ptd-KPIs (contractors)
{
  # KPI: # of Contractors
   ptd_cntr = reactive({
    return(hours %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ptd) %>%
             distinct(`Contractor Name`) %>%
             summarise(Count = n()) %>%
             pull(Count)
    )
  })
  
  # Render Contractors infoBoxOutput for ptd-Project Analysis
  output$ptd_numCon = renderUI({
    if (length(input$projnum_input_ptd) > 0) {
      summaryBox2(
        title = HTML("<div style='text-align: right;'>Contractors</div>"),
        value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                     ifelse(ptd_cntr() > 999999,
                            paste0((round(ptd_cntr() / 1000000, digits = 0)),"M"),
                            ifelse(ptd_cntr() > 999,
                                   paste0((round(ptd_cntr() / 1000, digits = 0)),"K"),
                                   ptd_cntr())
                     ),"</div>"),
        width = 4,
        icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-hard-hat","</div>"),
        style = "light"
      )
    }
  })
}
# ----------------------------------------------------------------------- #   
# Dynamic proj-ptd-KPIs (workers)
{
  #KPI: # of Workers
  ptd_Wrkr = reactive({
    return(hours %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ptd) %>%
             summarise(`Worker Count` = sum(`Worker Count`)) %>%
             pull(`Worker Count`)
    )
  })
  
  # Render Workers infoBoxOutput for ptd-Project Analysis
  output$ptd_numWrk = renderUI({
    if (length(input$projnum_input_ptd) > 0) {
      
      summaryBox2(
        title = HTML("<div style='text-align: right;'>","Workers","</div>"),
        value = HTML("<div style='text-align: right; margin-right: 10px;'>",
                     ifelse(ptd_Wrkr() > 999999, 
                            paste0((round(ptd_Wrkr() / 1000000, digits = 0)),"M"), 
                            ifelse(ptd_Wrkr() > 999,
                                   paste0((round(ptd_Wrkr() / 1000, digits = 0)),"K"),
                                   ptd_Wrkr())
                     )
                     ,"</div>"),
        width = 4,
        icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-person-running","</div>"),
        style = "light"
      )
    }
  })  
}
# ----------------------------------------------------------------------- #   
# Dynamic proj-ptd-KPIs (hours)
{
  # KPI: # of hours
  ptd_hours = reactive({
    return(proj_month_hours %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ptd) %>%
             ungroup() %>%
             summarise(Workhours = sum(Workhours)) %>%
             pull(Workhours)
    )
  })
  
  # Render hours infoBoxOutput for ptd-Project Analysis
  output$ptd_numhour = renderUI({
    if (length(input$projnum_input_ptd) > 0) {
      
      summaryBox2(
        title = HTML("<div style='text-align: right;'>Work-Hours</div>"),
        value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                     ifelse(ptd_hours() > 999999, 
                            paste0((round(ptd_hours() / 1000000, digits = 1)),"M"), 
                            ifelse(ptd_hours() > 999,
                                   paste0((round(ptd_hours() / 1000, digits = 0)),"K"),
                                   ptd_hours())
                     ),
                     "</div>"),
        width = 4,
        icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-clock","</div>"),
        style = "light"
      )
    }
  })  
}
# ----------------------------------------------------------------------- #  
# Dynamic proj-ptd-KPIs (TRIR)
{
  # KPI: TRIR
  ptd_trir = reactive({
    return(data_trir  %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ptd) %>%
             ungroup() %>%
             summarise(Workhours = sum(Workhours),
                       Recordables = sum(Recordables)) %>%
             ungroup() %>%
             # group_by(`Project #`, `Project Name`) %>%
             mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
             mutate(TRIR = ifelse(TRIR == 0, as.character("0.00"), round(TRIR, 2))) %>%
             pull(TRIR)
    )
  })
  
  # Render TRIR infoBoxOutput for ptd-Project Analysis
  output$ptd_trir = renderUI({
    if (length(input$projnum_input_ptd) > 0) {
      summaryBox3(
        title = HTML("<center style='color: blue;'>TRIR</center>"),
        value = HTML("<center><p style='font-size:45px'>",
                     ptd_trir(),
                     "</p></center>"),
        style = "primary",
        width = 4,
        icon = F
      )
    }
  })  
}  
# ----------------------------------------------------------------------- #  
# Dynamic proj-ptd-KPIs (DART)
{
  # KPI: DART
  ptd_dart = reactive({
    return(data_dart  %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ptd) %>%
             ungroup() %>%
             summarise(Workhours = sum(Workhours),
                       DaysAwayRec = sum(DaysAwayRec)) %>%
             ungroup() %>%
             # group_by(`Project #`, `Project Name`) %>%
             mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
             mutate(DART = ifelse(DART == 0, as.character("0.00"), round(DART, 2))) %>%
             pull(DART)
    )
  })
  
  # Render DART infoBoxOutput for ptd-Project Analysis
  output$ptd_dart = renderUI({
    if (length(input$projnum_input_ptd) > 0) {
      summaryBox3(
        title = HTML("<center style='color: red;'>DART</center>"),
        value = HTML("<center><p style='font-size:45px'>",
                     ptd_dart(),
                     "</p></center>"),
        style = "danger",
        width = 4,
        icon = F
      )
    }
  })  
}  
# ----------------------------------------------------------------------- #  
# Dynamic proj-ptd-KPIs (LTIR)
{
  # KPI: LTIR
  ptd_ltir = reactive({
    return(data_ltir  %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ptd) %>%
             ungroup() %>%
             summarise(Workhours = sum(Workhours),
                       LostTimeRec = sum(LostTimeRec)) %>%
             ungroup() %>%
             # group_by(`Project #`, `Project Name`) %>%
             mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
             mutate(LTIR = ifelse(LTIR == 0, as.character("0.00"), round(LTIR, 2))) %>%
             pull(LTIR)
    )
  })
  
  # Render LTIR infoBoxOutput for ptd-Project Analysis
  output$ptd_ltir = renderUI({
    if (length(input$projnum_input_ytd) > 0) {
      summaryBox3(
        title = HTML("<center style='color: green;'>LTIR</center>"),
        value = HTML("<center><p style='font-size:45px'>",
                     ptd_ltir(),
                     "</p></center>"),
        style = "success",
        width = 4,
        icon = F
      )
    }
  })  
}  
}

# Projects-YTD
{  
#Dynamic Title Headers
  {
# Checkbox to allow multiple project selection
  # multiple_proj_sel = reactive({
  #   if (input$checkbox_input) {
  #     return(TRUE)
  #   } else {
  #     return(FALSE)
  #   }
  # })
  # 
  # observe({
  #   updateSelectInput(session, "projnum_input", multiple = multiple_proj_sel())
  # })

    
# result: proj_title -> returns title of Project # input
  proj_title_ytd = reactive({
    return(proj_total_hours %>%
             ungroup %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ytd) %>%
             select(`Project Name`) %>%
             pull())
  })
  
  output$projTitle_ytd = renderText({
    proj_title_combined_ytd = paste(proj_title_ytd(), collapse = ", ")
    proj_title_combined_ytd
  })
  }
# ----------------------------------------------------------------------- #   
#Dynamic Project Analysis Drop Downs
  {
# Filter proj_month_hours based on Job Number user input 
  choices_year_ytd = reactive({
    return(proj_month_hours %>%
      dplyr::filter(`Project #` %in% input$projnum_input_ytd)
      )
  })
  
# Update Year user input based on filtered proj_month_hours  
  observe({
    if (length(choices_year_ytd()) > 0) {
      updateSelectInput(session = session, 
                        inputId = "year_input_ytd", 
                        choices = sort(unique(choices_year_ytd()$Year), decreasing = TRUE)
                        )
    }
  })
}
# ----------------------------------------------------------------------- #   
# Dynamic ytd-TRIR df for Tables and Plots
  { 
# Create ytd-trir function to call dataset based on user Project # and Year input

# TRIR df  
ytd_proj_recKPI = reactive({
  ytd_proj_trir = data_trir  %>%
    dplyr::filter(`Project #` %in% input$projnum_input_ytd) %>%
    group_by(Year, Month) %>%
    summarise(Workhours = sum(Workhours),
              Recordables = sum(Recordables)) %>%
    dplyr::filter(Year == input$year_input_ytd) %>%
    ungroup() %>%
    # group_by(`Project #`, `Project Name`) %>%
    mutate(CumulativeWH = cumsum(Workhours)) %>%
    mutate(CumulativeRec = cumsum(Recordables)) %>%
    mutate(TRIR = ((200000 * CumulativeRec) / CumulativeWH)) %>%
    mutate(TRIR = ifelse(is.na(TRIR), 0.00,
                         ifelse(Workhours == 0, 0.00, round(TRIR, 2)))) %>%
    select(Year,Month,TRIR)
# DART df
  ytd_proj_dart = data_dart %>%
    dplyr::filter(`Project #` %in% input$projnum_input_ytd) %>%
    group_by(Year, Month) %>%
    summarise(Workhours = sum(Workhours),
              DaysAwayRec = sum(DaysAwayRec)) %>%
    dplyr::filter(Year == input$year_input_ytd) %>%
    ungroup() %>%
    # group_by(`Project #`, `Project Name`) %>%
    mutate(CumulativeWH = cumsum(Workhours)) %>%
    mutate(CumulativeDart = cumsum(DaysAwayRec)) %>%
    mutate(DART = ((200000 * CumulativeDart) / CumulativeWH)) %>%
    mutate(DART = ifelse(is.na(DART), 0.00, 
                         ifelse(Workhours == 0, 0.00, round(DART, 2)))) %>%
    select(Year,Month,DART)
# LTIR df  
  ytd_proj_ltir = data_ltir %>%
    dplyr::filter(`Project #` %in% input$projnum_input_ytd) %>%
    group_by(Year, Month) %>%
    summarise(Workhours = sum(Workhours),
              LostTimeRec = sum(LostTimeRec)) %>%
    dplyr::filter(Year == input$year_input_ytd) %>%
    ungroup() %>%
    # group_by(`Project #`, `Project Name`) %>%
    mutate(CumulativeWH = cumsum(Workhours)) %>%
    mutate(CumulativeLtir = cumsum(LostTimeRec)) %>%
    mutate(LTIR = ((200000 * CumulativeLtir) / CumulativeWH)) %>%
    mutate(LTIR = ifelse(is.na(LTIR), 0.00, 
                         ifelse(Workhours == 0, 0.00, round(LTIR, 2)))) %>%
    select(Year,Month,LTIR)
  
# Combine TRIR,DART,LTIR dfs
  ytd_combined_Rec_KPI = bind_rows(
    mutate(ytd_proj_trir, Metric = "TRIR"),
    mutate(ytd_proj_dart, Metric = "DART"),
    mutate(ytd_proj_ltir, Metric = "LTIR")
    ) %>%
    mutate(Rates = coalesce(TRIR, DART, LTIR)) %>%
    select(-TRIR, -DART, -LTIR)
  return(ytd_combined_Rec_KPI)
})

# Render TRIR Plots for Project Analysis
  
  output$ytdRecKPIplot = plotly::renderPlotly({
    if (length(input$projnum_input_ytd) > 0 & length(input$year_input_ytd) > 0) {
      #ytd_recKPI_data = ytd_proj_recKPI()  # Store the reactive value in a variable
      
      gg_ytd_recKPI = ggplot(ytd_proj_recKPI(), aes(x = Month, 
                                                  y = Rates,
                                                  group = Metric,
                                                  color = Metric,
                                                  text = paste(Metric,": ",Rates))) +
        geom_line() +
        labs(title = "Recordable Case Rates", xlab="Month", ylab="Rates") +
        theme_minimal() + 
        theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
              axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1),
              legend.position = "left") +
        scale_y_continuous(breaks = seq(0, (max(ytd_proj_recKPI()$Rates)+0.25), by = 1),
                             limits = c(0, (max(ytd_proj_recKPI()$Rates)+0.25))) +
        scale_color_manual(values = c("TRIR" = "blue", "DART" = "red", "LTIR" = "green"))
      }
      
      plotly::ggplotly(gg_ytd_recKPI, tooltip = "text")
    
  })
}
# ----------------------------------------------------------------------- #   
# Dynamic ytd-Recordable df for Table and plots
  {
    # Create ytd-rec function to call dataset based on user Project # and Year input  
    ytd_proj_rec = reactive({
      return(data_trir  %>%
               dplyr::filter(`Project #` %in% input$projnum_input_ytd) %>%
               group_by(Year, Month) %>%
               summarise(Recordables = sum(Recordables), 
                         `OSHA Classification` = paste(`OSHA Classification`[!is.na(`OSHA Classification`)], collapse = ", ")) %>%
               dplyr::filter(Year == input$year_input_ytd) %>%
               ungroup() %>%
               mutate(CumulativeRec = cumsum(Recordables))
             )
    })
    
    # Render Rec Plot for mtd-Project Analysis
    output$ytdrecplot = plotly::renderPlotly({
      if (length(input$projnum_input_ytd) > 0 & length(input$year_input_ytd) > 0) {
        
        gg_ytd_rec = ggplot(ytd_proj_rec(), aes(x = Month, y = CumulativeRec, group = 1, text = `OSHA Classification`)) +
          geom_line(color = "orange") +
          labs(title = "Recordables", xlab = "Month", ylab = "Recordable Count") +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1)) +
          scale_y_continuous(breaks = seq(0, (max(ytd_proj_rec()$CumulativeRec)+0.5), by = 1),
                               limits = c(0, (max(ytd_proj_rec()$CumulativeRec)+0.5)))
        
        plotly::ggplotly(gg_ytd_rec, tooltip = "text")
      }
    })
} 
# ----------------------------------------------------------------------- #   
# Dynamic proj-ytd-KPIs (contractors)
  {
  # KPI: # of Contractors
    ytd_cntr = reactive({
      return(hours %>%
               dplyr::filter(`Project #` %in% input$projnum_input_ytd, Year %in% input$year_input_ytd) %>%
               distinct(`Contractor Name`) %>%
               summarise(Count = n()) %>%
               pull(Count)
             )
      })
    
  # Render Contractors infoBoxOutput for ytd-Project Analysis
    output$ytd_numCon = renderUI({
      if (length(input$projnum_input_ytd) > 0 & length(input$year_input_ytd) > 0) {
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Contractors</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(ytd_cntr() > 999999,
                       paste0((round(ytd_cntr() / 1000000, digits = 0)),"M"),
                       ifelse(ytd_cntr() > 999,
                              paste0((round(ytd_cntr() / 1000, digits = 0)),"K"),
                              ytd_cntr())
                       ),"</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-hard-hat","</div>"),
          style = "light"
          )
      }
      })
  }
# ----------------------------------------------------------------------- #   
# Dynamic proj-ytd-KPIs (workers)
  {
  #KPI: # of Workers
  ytd_Wrkr = reactive({
    return(hours %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ytd, Year %in% input$year_input_ytd) %>%
      summarise(`Worker Count` = sum(`Worker Count`)) %>%
      pull(`Worker Count`)
      )
  })
  
  # Render Workers infoBoxOutput for ytd-Project Analysis
  output$ytd_numWrk = renderUI({
    if (length(input$projnum_input_ytd) > 0 & length(input$year_input_ytd) > 0) {
      
        summaryBox2(
        title = HTML("<div style='text-align: right;'>","Workers","</div>"),
        value = HTML("<div style='text-align: right; margin-right: 10px;'>",
                     ifelse(ytd_Wrkr() > 999999, 
                            paste0((round(ytd_Wrkr() / 1000000, digits = 0)),"M"), 
                            ifelse(ytd_Wrkr() > 999,
                                   paste0((round(ytd_Wrkr() / 1000, digits = 0)),"K"),
                                   ytd_Wrkr())
                            )
                     ,"</div>"),
        width = 4,
        icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-person-running","</div>"),
        style = "light"
      )
      }
  })  
  }
# ----------------------------------------------------------------------- #   
# Dynamic proj-ytd-KPIs (hours)
  {
# KPI: # of hours
  ytd_hours = reactive({
    return(proj_month_hours %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ytd, Year %in% input$year_input_ytd) %>%
      group_by(Year) %>%
      summarise(Workhours = sum(Workhours)) %>%
      pull(Workhours)
      )
  })
  
# Render hours infoBoxOutput for ytd-Project Analysis
  output$ytd_numhour = renderUI({
    if (length(input$projnum_input_ytd) > 0 & length(input$year_input_ytd) > 0) {
      
      summaryBox2(
        title = HTML("<div style='text-align: right;'>Work-Hours</div>"),
        value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                     ifelse(ytd_hours() > 999999, 
                            paste0((round(ytd_hours() / 1000000, digits = 1)),"M"), 
                            ifelse(ytd_hours() > 999,
                                   paste0((round(ytd_hours() / 1000, digits = 0)),"K"),
                                   ytd_hours())
                            ),
                     "</div>"),
        width = 4,
        icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-clock","</div>"),
        style = "light"
      )
      }
  })  
  }
# ----------------------------------------------------------------------- #  
# Dynamic proj-ytd-KPIs (TRIR)
  {
    # KPI: TRIR
    ytd_trir = reactive({
      return(data_trir  %>%
               dplyr::filter(`Project #` %in% input$projnum_input_ytd) %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours),
                         Recordables = sum(Recordables)) %>%
               dplyr::filter(Year == input$year_input_ytd) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
               mutate(TRIR = ifelse(TRIR == 0, as.character("0.00"), round(TRIR, 2))) %>%
               pull(TRIR)
      )
    })
    
    # Render TRIR infoBoxOutput for ytd-Project Analysis
    output$ytd_trir = renderUI({
      if (length(input$projnum_input_ytd) > 0 & length(input$year_input_ytd) > 0) {
        summaryBox3(
          title = HTML("<center style='color: blue;'>TRIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       ytd_trir(),
                       "</p></center>"),
          style = "primary",
          width = 4,
          icon = F
        )
      }
    })  
  }  
# ----------------------------------------------------------------------- #  
# Dynamic proj-ytd-KPIs (DART)
  {
  # KPI: DART
  ytd_dart = reactive({
    return(data_dart  %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ytd) %>%
             group_by(Year) %>%
             summarise(Workhours = sum(Workhours),
                       DaysAwayRec = sum(DaysAwayRec)) %>%
             dplyr::filter(Year == input$year_input_ytd) %>%
             ungroup() %>%
             # group_by(`Project #`, `Project Name`) %>%
             mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
             mutate(DART = ifelse(DART == 0, as.character("0.00"), round(DART, 2))) %>%
             pull(DART)
    )
  })
  
  # Render DART infoBoxOutput for ytd-Project Analysis
  output$ytd_dart = renderUI({
    if (length(input$projnum_input_ytd) > 0 & length(input$year_input_ytd) > 0) {
      summaryBox3(
        title = HTML("<center style='color: red;'>DART</center>"),
        value = HTML("<center><p style='font-size:45px'>",
                     ytd_dart(),
                     "</p></center>"),
        style = "danger",
        width = 4,
        icon = F
      )
    }
  })  
}  
# ----------------------------------------------------------------------- #  
# Dynamic proj-ytd-KPIs (LTIR)
  {
  # KPI: LTIR
  ytd_ltir = reactive({
    return(data_ltir  %>%
             dplyr::filter(`Project #` %in% input$projnum_input_ytd) %>%
             group_by(Year) %>%
             summarise(Workhours = sum(Workhours),
                       LostTimeRec = sum(LostTimeRec)) %>%
             dplyr::filter(Year == input$year_input_ytd) %>%
             ungroup() %>%
             # group_by(`Project #`, `Project Name`) %>%
             mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
             mutate(LTIR = ifelse(LTIR == 0, as.character("0.00"), round(LTIR, 2))) %>%
             pull(LTIR)
    )
  })
  
  # Render LTIR infoBoxOutput for ytd-Project Analysis
  output$ytd_ltir = renderUI({
    if (length(input$projnum_input_ytd) > 0 & length(input$year_input_ytd) > 0) {
      summaryBox3(
        title = HTML("<center style='color: green;'>LTIR</center>"),
        value = HTML("<center><p style='font-size:45px'>",
                     ytd_ltir(),
                     "</p></center>"),
        style = "success",
        width = 4,
        icon = F
      )
    }
  })  
}  

}

# Projects-MTD
{  
  #Dynamic Title Headers
  {
    # Checkbox to allow multiple project selection
    # multiple_proj_sel = reactive({
    #   if (input$checkbox_input) {
    #     return(TRUE)
    #   } else {
    #     return(FALSE)
    #   }
    # })
    # 
    # observe({
    #   updateSelectInput(session, "projnum_input", multiple = multiple_proj_sel())
    # })
    
    
    # result: proj_title -> returns title of Project # input
    proj_title_mtd = reactive({
      return(proj_total_hours %>%
               ungroup %>%
               dplyr::filter(`Project #` %in% input$projnum_input_mtd) %>%
               select(`Project Name`) %>%
               pull())
    })
    
    output$projTitle_mtd = renderText({
      proj_title_combined_mtd = paste(proj_title_mtd(), collapse = ", ")
      proj_title_combined_mtd
    })
  }
# ----------------------------------------------------------------------- #   
  #Dynamic Project Analysis Drop Downs
  {
    # Filter proj_month_hours based on Job Number user input 
    choices_year_mtd = reactive({
      return(proj_month_hours %>%
               dplyr::filter(`Project #` %in% input$projnum_input_mtd)
      )
    })
    
    # Update Year user input based on filtered proj_month_hours  
    observe({
      if (length(choices_year_mtd()) > 0) {
        updateSelectInput(session = session, 
                          inputId = "year_input_mtd", 
                          choices = sort(unique(choices_year_mtd()$Year), decreasing = TRUE)
        )
      }
    })
    
  # Filter proj_month_hours based on Job Number user input 
  choices_month_mtd = reactive({
    return(proj_month_hours %>%
             dplyr::filter(`Project #` %in% input$projnum_input_mtd) %>%
             dplyr::filter(Year == input$year_input_mtd)
    )
  })
  
  # Update Year user input based on filtered proj_month_hours  
  observe({
    if (length(choices_month_mtd()) > 0) {
      updateSelectInput(session = session, 
                        inputId = "month_input_mtd", 
                        choices = sort(unique(choices_month_mtd()$Month), decreasing = TRUE)
      )
    }
  })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic mtd-TRIR df for Tables and Plots
  { 
    # Create mtd-trir function to call dataset based on user Project # and Year input
    
    
    # TRIR df  
    mtd_proj_recKPI = reactive({
      mtd_proj_trir = data_trir  %>%
        dplyr::filter(`Project #` %in% input$projnum_input_mtd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  Recordables = sum(Recordables)) %>%
        dplyr::filter(Year == input$year_input_mtd) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
        mutate(TRIR = ifelse(is.na(TRIR), 0.00,
                             ifelse(Workhours == 0, 0.00, round(TRIR, 2)))) %>%
        select(Year,Month,TRIR)
    # DART df
      mtd_proj_dart = data_dart %>%
        dplyr::filter(`Project #` %in% input$projnum_input_mtd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  DaysAwayRec = sum(DaysAwayRec)) %>%
        dplyr::filter(Year == input$year_input_mtd) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
        mutate(DART = ifelse(is.na(DART), 0.00,
                             ifelse(Workhours == 0, 0.00, round(DART, 2)))) %>%
        select(Year,Month,DART)
      # LTIR df  
      mtd_proj_ltir = data_ltir %>%
        dplyr::filter(`Project #` %in% input$projnum_input_mtd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  LostTimeRec = sum(LostTimeRec)) %>%
        dplyr::filter(Year == input$year_input_mtd) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
        mutate(LTIR = ifelse(is.na(LTIR), 0.00,
                             ifelse(Workhours == 0, 0.00, round(LTIR, 2)))) %>%
        select(Year,Month,LTIR)
      # Combine TRIR,DART,LTIR dfs
      mtd_combined_Rec_KPI = bind_rows(
        mutate(mtd_proj_trir, group = "TRIR"),
        mutate(mtd_proj_dart, group = "DART"),
        mutate(mtd_proj_ltir, group = "LTIR")
      ) %>%
        mutate(Rates = coalesce(TRIR, DART, LTIR)) %>%
        select(-TRIR, -DART, -LTIR)
      return(mtd_combined_Rec_KPI)
    })
    
    # Render TRIR Plots for Project Analysis
    
    output$mtdRecKPIplot = plotly::renderPlotly({
      if (length(input$projnum_input_mtd) > 0 & length(input$year_input_mtd) > 0) {
        gg_mtd_recKPI = ggplot(mtd_proj_recKPI(), aes(x = Month, 
                                                      y = Rates,
                                                      group = group,
                                                      color = group,
                                                      text = paste(group,": ",Rates))) +
          geom_line() +
          labs(title = "Recordable Case Rates", xlab("Month"), ylab("Rates")) +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1),
                legend.position = "top") +
          scale_y_continuous(breaks = seq(0, (max(mtd_proj_recKPI()$Rates)+0.5), by = 1),
                             limits = c(0, (max(mtd_proj_recKPI()$Rates)+0.5))) +
          scale_color_manual(values = c("TRIR" = "blue", "DART" = "red", "LTIR" = "green"))
      }
      
      plotly::ggplotly(gg_mtd_recKPI, tooltip = "text")
      
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic mtd-Recordable df for Table and plots
  {
    # Create mtd-rec function to call dataset based on user Project # and Year input  
    mtd_proj_rec = reactive({
      return(data_pmr  %>%
               dplyr::filter(`Project #` %in% input$projnum_input_mtd) %>%
               group_by(Year, Month) %>%
               summarise(Recordables = sum(Recordables), 
                         `OSHA Classification` = paste(`OSHA Classification`, collapse = ", ")) %>%
               dplyr::filter(Year == input$year_input_mtd)
      )
    })
    
    # Render Rec Plot for mtd-Project Analysis
    output$mtdrecplot = plotly::renderPlotly({
      if (length(input$projnum_input_mtd) > 0 & length(input$year_input_mtd) >= 0) {
        mtd_data = mtd_proj_rec()
        
        gg_mtd_rec = ggplot(mtd_data, aes(x = Month, y = Recordables, text = `OSHA Classification`)) +
          geom_bar(stat = "identity", fill = "orange", width = 0.75) +
          labs(title = "Recordables", xlab = "Month", ylab = "Recordable Count") +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1))
        
        if (any(!is.na(mtd_data$Recordables))) {
          gg_mtd_rec <- gg_mtd_rec +
            scale_y_continuous(breaks = seq(0, (max(mtd_data$Recordables)+0.5), by = 1),
                               limits = c(0, (max(mtd_data$Recordables)+0.5)))
        }
        
        plotly::ggplotly(gg_mtd_rec, tooltip = "text")
      }
    })
  } 
  # ----------------------------------------------------------------------- #   
  # Dynamic proj-mtd-KPIs (contractors)
  {
    # KPI: # of Contractors
    mtd_cntr = reactive({
      return(hours %>%
               dplyr::filter(`Project #` %in% input$projnum_input_mtd, 
                             Year %in% input$year_input_mtd,
                             Month %in% input$month_input_mtd) %>%
               distinct(`Contractor Name`) %>%
               summarise(Count = n()) %>%
               pull(Count)
      )
    })
    
    # Render Contractors infoBoxOutput for mtd-Project Analysis
    output$mtd_numCon = renderUI({
      if (length(input$projnum_input_mtd) > 0 & length(input$year_input_mtd) > 0) {
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Contractors</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(mtd_cntr() > 999999,
                              paste0((round(mtd_cntr() / 1000000, digits = 0)),"M"),
                              ifelse(mtd_cntr() > 999,
                                     paste0((round(mtd_cntr() / 1000, digits = 0)),"K"),
                                     mtd_cntr())
                       ),"</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-hard-hat","</div>"),
          style = "light"
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic proj-mtd-KPIs (workers)
  {
    #KPI: # of Workers
    mtd_Wrkr = reactive({
      return(hours %>%
               dplyr::filter(`Project #` %in% input$projnum_input_mtd, 
                             Year %in% input$year_input_mtd,
                             Month %in% input$month_input_mtd) %>%
               summarise(`Worker Count` = sum(`Worker Count`)) %>%
               pull(`Worker Count`)
      )
    })
    
    # Render Workers infoBoxOutput for mtd-Project Analysis
    output$mtd_numWrk = renderUI({
      if (length(input$projnum_input_mtd) > 0 & length(input$year_input_mtd) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>","Workers","</div>"),
          value = HTML("<div style='text-align: right; margin-right: 10px;'>",
                       ifelse(mtd_Wrkr() > 999999, 
                              paste0((round(mtd_Wrkr() / 1000000, digits = 0)),"M"), 
                              ifelse(mtd_Wrkr() > 999,
                                     paste0((round(mtd_Wrkr() / 1000, digits = 0)),"K"),
                                     mtd_Wrkr())
                       )
                       ,"</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-person-running","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic proj-mtd-KPIs (hours)
  {
    # KPI: # of hours
    mtd_hours = reactive({
      return(proj_month_hours %>%
               dplyr::filter(`Project #` %in% input$projnum_input_mtd, 
                             Year %in% input$year_input_mtd,
                             Month %in% input$month_input_mtd) %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours)) %>%
               pull(Workhours)
      )
    })
    
    # Render hours infoBoxOutput for mtd-Project Analysis
    output$mtd_numhour = renderUI({
      if (length(input$projnum_input_mtd) > 0 & length(input$year_input_mtd) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Work-Hours</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(mtd_hours() > 999999, 
                              paste0((round(mtd_hours() / 1000000, digits = 1)),"M"), 
                              ifelse(mtd_hours() > 999,
                                     paste0((round(mtd_hours() / 1000, digits = 0)),"K"),
                                     mtd_hours())
                       ),
                       "</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-clock","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #  
  # Dynamic proj-mtd-KPIs (TRIR)
  {
    # KPI: TRIR
    mtd_trir = reactive({
      return(data_trir  %>%
               dplyr::filter(`Project #` %in% input$projnum_input_mtd,
                             Year %in% input$year_input_mtd) %>%
               group_by(Month) %>%
               summarise(Workhours = sum(Workhours),
                         Recordables = sum(Recordables)) %>%
               dplyr::filter(Month %in% input$month_input_mtd) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
               mutate(TRIR = ifelse(TRIR == 0, as.character("0.00"), round(TRIR, 2))) %>%
               pull(TRIR)
      )
    })
    
    # Render TRIR infoBoxOutput for mtd-Project Analysis
    output$mtd_trir = renderUI({
      if (length(input$projnum_input_mtd) > 0 & length(input$year_input_mtd) > 0) {
        summaryBox3(
          title = HTML("<center style='color: blue;'>TRIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       mtd_trir(),
                       "</p></center>"),
          style = "primary",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic proj-mtd-KPIs (DART)
  {
    # KPI: DART
    mtd_dart = reactive({
      return(data_dart  %>%
               dplyr::filter(`Project #` %in% input$projnum_input_mtd,
                             Year %in% input$year_input_mtd) %>%
               group_by(Month) %>%
               summarise(Workhours = sum(Workhours),
                         DaysAwayRec = sum(DaysAwayRec)) %>%
               dplyr::filter(Month %in% input$month_input_mtd) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
               mutate(DART = ifelse(DART == 0, as.character("0.00"), round(DART, 2))) %>%
               pull(DART)
      )
    })
    
    # Render DART infoBoxOutput for mtd-Project Analysis
    output$mtd_dart = renderUI({
      if (length(input$projnum_input_mtd) > 0 & length(input$year_input_mtd) > 0) {
        summaryBox3(
          title = HTML("<center style='color: red;'>DART</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       mtd_dart(),
                       "</p></center>"),
          style = "danger",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic proj-mtd-KPIs (LTIR)
  {
    # KPI: LTIR
    mtd_ltir = reactive({
      return(data_ltir  %>%
               dplyr::filter(`Project #` %in% input$projnum_input_mtd,
                             Year %in% input$year_input_mtd) %>%
               group_by(Month) %>%
               summarise(Workhours = sum(Workhours),
                         LostTimeRec = sum(LostTimeRec)) %>%
               dplyr::filter(Month %in% input$month_input_mtd) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
               mutate(LTIR = ifelse(LTIR == 0, as.character("0.00"), round(LTIR, 2))) %>%
               pull(LTIR)
      )
    })
    
    # Render LTIR infoBoxOutput for mtd-Project Analysis
    output$mtd_ltir = renderUI({
      if (length(input$projnum_input_mtd) > 0 & length(input$year_input_mtd) > 0) {
        summaryBox3(
          title = HTML("<center style='color: green;'>LTIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       mtd_ltir(),
                       "</p></center>"),
          style = "success",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  
}
}
# -------------------------( Contractor TAB )------------------------- #
{
# Contractor-CTD
{
  #Dynamic Title Headers
  {
    # Checkbox to allow multiple project selection
    # multiple_proj_sel = reactive({
    #   if (input$checkbox_input) {
    #     return(TRUE)
    #   } else {
    #     return(FALSE)
    #   }
    # })
    # 
    # observe({
    #   updateSelectInput(session, "projnum_input", multiple = multiple_proj_sel())
    # })

    # result: cont_title -> returns title of Contractor Name input
    cont_title_ctd = reactive({
      return(hours %>%
               distinct(`Contractor Name`) %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
               pull()
             )
    })
    
    output$contTitle_ctd = renderText({
      cont_title_ctd()
    })
  }
  # ----------------------------------------------------------------------- #   
  #Dynamic Project Analysis Drop Downs - UNUSED
  {
    # # Filter proj_month_hours based on Job Number user input 
    # choices_year_ptd = reactive({
    #   return(proj_month_hours %>%
    #            dplyr::filter(`Project #` %in% input$projnum_input_ptd)
    #   )
    # })
    # 
    # # Update Year user input based on filtered proj_month_hours  
    # observe({
    #   if (length(choices_year_ptd()) > 0) {
    #     updateSelectInput(session = session, 
    #                       inputId = "year_input_ptd", 
    #                       choices = sort(unique(choices_year_ptd()$Year), decreasing = TRUE)
    #     )
    #   }
    # })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ctd-TRIR df for Tables and Plots
  { 
    # Create ctd-trir function to call dataset based on user Contractor Name
    
    # TRIR df  
    ctd_cont_recKPI = reactive({
      ctd_cont_trir = cont_data_trir  %>%
        dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  Recordables = sum(Recordables)) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeRec = cumsum(Recordables)) %>%
        mutate(TRIR = ((200000 * CumulativeRec) / CumulativeWH)) %>%
        mutate(TRIR = ifelse(is.na(TRIR), 0.00,
                             ifelse(CumulativeWH == 0, 0.00, round(TRIR, 2)))) %>%
        select(Year,Month,TRIR)
      # DART df
      ctd_cont_dart = cont_data_dart %>%
        dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  DaysAwayRec = sum(DaysAwayRec)) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeDart = cumsum(DaysAwayRec)) %>%
        mutate(DART = ((200000 * CumulativeDart) / CumulativeWH)) %>%
        mutate(DART = ifelse(is.na(DART), 0.00, 
                             ifelse(CumulativeWH == 0, 0.00, round(DART, 2)))) %>%
        select(Year,Month,DART)
      # LTIR df  
      ctd_cont_ltir = cont_data_ltir %>%
        dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  LostTimeRec = sum(LostTimeRec)) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeLtir = cumsum(LostTimeRec)) %>%
        mutate(LTIR = ((200000 * CumulativeLtir) / CumulativeWH)) %>%
        mutate(LTIR = ifelse(is.na(LTIR), 0.00, 
                             ifelse(CumulativeWH == 0, 0.00, round(LTIR, 2)))) %>%
        select(Year,Month,LTIR)
      
      # Combine TRIR,DART,LTIR dfs
      ctd_combined_Rec_KPI = bind_rows(
        mutate(ctd_cont_trir, Metric = "TRIR"),
        mutate(ctd_cont_dart, Metric = "DART"),
        mutate(ctd_cont_ltir, Metric = "LTIR")
      ) %>%
        mutate(Rates = coalesce(TRIR, DART, LTIR)) %>%
        select(-TRIR, -DART, -LTIR) %>%
        mutate(FullDate = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"),
               Year = factor(Year),
               Month = factor(Month, levels = sprintf("%02d", 1:12)),
               Year.Month = format(FullDate, "%Y.%m"))
      return(ctd_combined_Rec_KPI)
    })
    
    # Render TRIR Plots for Contractor Analysis
    
    output$ctdRecKPIplot = plotly::renderPlotly({
      if (length(input$cont_input_ctd) > 0) {
        #ptd_recKPI_data = ptd_proj_recKPI()  # Store the reactive value in a variable
        
        gg_ctd_recKPI = ggplot(ctd_cont_recKPI(), aes(x = Year.Month, 
                                                      y = Rates,
                                                      group = Metric,
                                                      color = Metric,
                                                      text = paste(Metric,": ",Rates, "<br>",
                                                                   Year.Month))) +
          geom_line() +
          labs(title = "Recordable Case Rates", xlab="Month", ylab="Rates") +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1),
                legend.position = "left") +
          scale_y_continuous(breaks = seq(0, (max(ctd_cont_recKPI()$Rates)+0.25), by = 1),
                             limits = c(0, (max(ctd_cont_recKPI()$Rates)+0.25))) +
          scale_color_manual(values = c("TRIR" = "blue", "DART" = "red", "LTIR" = "green")) 
        # +
        #   scale_x_date(NULL, date_labels = "%m %y", breaks = "Month")
        
        plotly::layout(plotly::ggplotly(gg_ctd_recKPI, tooltip = "text"),
                       xaxis = list(range = list(max(as.numeric(ctd_cont_recKPI()$Year)),
                                                 max(as.numeric(ctd_cont_recKPI()$Year)))
                       )
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ctd-Recordable df for Table and plots
  {
    # Create ptd-rec function to call dataset based on user Project # and Year input  
    ctd_cont_rec = reactive({
      return(cont_data_trir  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
               group_by(Year, Month) %>%
               summarise(Recordables = sum(Recordables), 
                         `OSHA Classification` = paste(`OSHA Classification`[!is.na(`OSHA Classification`)], collapse = ", ")) %>%
               ungroup() %>%
               mutate(CumulativeRec = cumsum(Recordables)) %>%
               mutate(FullDate = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"),
                      Year = factor(Year),
                      Month = factor(Month, levels = sprintf("%02d", 1:12)),
                      Year.Month = format(FullDate, "%Y.%m"))
      )
    })
    
    # Render Rec Plot for mtd-Project Analysis
    output$ctdrecplot = plotly::renderPlotly({
      if (length(input$cont_input_ctd) > 0) {
        
        gg_ctd_rec = ggplot(ctd_cont_rec(), aes(x = Year.Month, y = CumulativeRec, group = 1, text = `OSHA Classification`)) +
          geom_line(color = "orange") +
          labs(title = "Recordables", xlab = "Year.Month", ylab = "Recordable Count") +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1)) +
          scale_y_continuous(breaks = seq(0, (max(ctd_cont_rec()$CumulativeRec)+0.5), by = 1),
                             limits = c(0, (max(ctd_cont_rec()$CumulativeRec)+0.5)))
        
        plotly::layout(plotly::ggplotly(gg_ctd_rec, tooltip = "text"),
                       xaxis = list(range = list(max(as.numeric(ctd_cont_rec()$Year)),
                                                 max(as.numeric(ctd_cont_rec()$Year)))
                       )
        )
      }
    })
  } 
  # ----------------------------------------------------------------------- #   
  # Dynamic cont-ctd-KPIs (Projects)
  {
    # KPI: # of Contractors
    cont_ctd_proj = reactive({
      return(hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
               distinct(`Project #`) %>%
               summarise(Count = n()) %>%
               pull(Count)
      )
    })
    
    # Render Contractors infoBoxOutput for ctd-Contractor Analysis
    output$cont_ctd_numProj = renderUI({
      if (length(input$cont_input_ctd) > 0) {
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Projects</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(cont_ctd_proj() > 999999,
                              paste0((round(cont_ctd_proj() / 1000000, digits = 0)),"M"),
                              ifelse(cont_ctd_proj() > 999,
                                     paste0((round(cont_ctd_proj() / 1000, digits = 0)),"K"),
                                     cont_ctd_proj())
                       ),"</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-city","</div>"),
          style = "light"
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic cont-ctd-KPIs (workers)
  {
    #KPI: # of Workers
    cont_ctd_Wrkr = reactive({
      return(hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
               summarise(`Worker Count` = sum(`Worker Count`)) %>%
               pull(`Worker Count`)
      )
    })
    
    # Render Workers infoBoxOutput for ptd-Project Analysis
    output$cont_ctd_numWrk = renderUI({
      if (length(input$cont_input_ctd) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>","Workers","</div>"),
          value = HTML("<div style='text-align: right; margin-right: 10px;'>",
                       ifelse(cont_ctd_Wrkr() > 999999, 
                              paste0((round(cont_ctd_Wrkr() / 1000000, digits = 0)),"M"), 
                              ifelse(cont_ctd_Wrkr() > 999,
                                     paste0((round(cont_ctd_Wrkr() / 1000, digits = 0)),"K"),
                                     cont_ctd_Wrkr())
                       )
                       ,"</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-person-running","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic cont-ctd-KPIs (hours)
  {
    # KPI: # of hours
    cont_ctd_hours = reactive({
      return(cont_month_hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
               ungroup() %>%
               summarise(Workhours = sum(Workhours)) %>%
               pull(Workhours)
      )
    })
    
    # Render hours infoBoxOutput for ptd-Project Analysis
    output$cont_ctd_numhour = renderUI({
      if (length(input$cont_input_ctd) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Work-Hours</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(cont_ctd_hours() > 999999, 
                              paste0((round(cont_ctd_hours() / 1000000, digits = 1)),"M"), 
                              ifelse(cont_ctd_hours() > 999,
                                     paste0((round(cont_ctd_hours() / 1000, digits = 0)),"K"),
                                     cont_ctd_hours())
                       ),
                       "</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-clock","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #  
  # Dynamic cont-ctd-KPIs (TRIR)
  {
    # KPI: TRIR
    cont_ctd_trir = reactive({
      return(cont_data_trir  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
               ungroup() %>%
               summarise(Workhours = sum(Workhours),
                         Recordables = sum(Recordables)) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
               mutate(TRIR = ifelse(TRIR == 0, as.character("0.00"), round(TRIR, 2))) %>%
               pull(TRIR)
      )
    })
    
    # Render TRIR infoBoxOutput for ctd-Contractor Analysis
    output$cont_ctd_trir = renderUI({
      if (length(input$cont_input_ctd) > 0) {
        summaryBox3(
          title = HTML("<center style='color: blue;'>TRIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       cont_ctd_trir(),
                       "</p></center>"),
          style = "primary",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic cont-ctd-KPIs (DART)
  {
    # KPI: DART
    cont_ctd_dart = reactive({
      return(cont_data_dart  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
               ungroup() %>%
               summarise(Workhours = sum(Workhours),
                         DaysAwayRec = sum(DaysAwayRec)) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
               mutate(DART = ifelse(DART == 0, as.character("0.00"), round(DART, 2))) %>%
               pull(DART)
      )
    })
    
    # Render DART infoBoxOutput for ctd-Contractor Analysis
    output$cont_ctd_dart = renderUI({
      if (length(input$cont_input_ctd) > 0) {
        summaryBox3(
          title = HTML("<center style='color: red;'>DART</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       cont_ctd_dart(),
                       "</p></center>"),
          style = "danger",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic cont-ctd-KPIs (LTIR)
  {
    # KPI: LTIR
    cont_ctd_ltir = reactive({
      return(cont_data_ltir  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ctd) %>%
               ungroup() %>%
               summarise(Workhours = sum(Workhours),
                         LostTimeRec = sum(LostTimeRec)) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
               mutate(LTIR = ifelse(LTIR == 0, as.character("0.00"), round(LTIR, 2))) %>%
               pull(LTIR)
      )
    })
    
    # Render LTIR infoBoxOutput for ptd-Project Analysis
    output$cont_ctd_ltir = renderUI({
      if (length(input$cont_input_ctd) > 0) {
        summaryBox3(
          title = HTML("<center style='color: green;'>LTIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       cont_ctd_ltir(),
                       "</p></center>"),
          style = "success",
          width = 4,
          icon = F
        )
      }
    })  
  }  
}

# Contractors-YTD
{  
  #Dynamic Title Headers
  {
    # Checkbox to allow multiple project selection
    # multiple_proj_sel = reactive({
    #   if (input$checkbox_input) {
    #     return(TRUE)
    #   } else {
    #     return(FALSE)
    #   }
    # })
    # 
    # observe({
    #   updateSelectInput(session, "projnum_input", multiple = multiple_proj_sel())
    # })
    
    
    # result: cont_title -> returns title of Contractor input
    cont_title_ytd = reactive({
      return(hours %>%
               distinct(`Contractor Name`) %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ytd) %>%
               pull())
    })
    
    output$contTitle_ytd = renderText({
      cont_title_ytd()
    })
  }
  # ----------------------------------------------------------------------- #   
  #Dynamic Contractor Analysis Drop Downs
  {
    # Filter cont_month_hours based on Contractor Name user input 
    choices_year_ytd_cont = reactive({
      return(cont_month_hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ytd)
      )
    })
    
    # Update Year user input based on filtered cont_month_hours  
    observe({
      if (length(choices_year_ytd_cont()) > 0) {
        updateSelectInput(session = session, 
                          inputId = "year_input_ytd_cont", 
                          choices = sort(unique(choices_year_ytd_cont()$Year), decreasing = TRUE)
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ytd-TRIR df for Tables and Plots
  { 
    # Create ytd-trir function to call dataset based on user Contractor Name and Year input
    
    # TRIR df  
    ytd_cont_recKPI = reactive({
      ytd_cont_trir = cont_data_trir  %>%
        dplyr::filter(`Contractor Name` %in% input$cont_input_ytd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  Recordables = sum(Recordables)) %>%
        dplyr::filter(Year == input$year_input_ytd_cont) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeRec = cumsum(Recordables)) %>%
        mutate(TRIR = ((200000 * CumulativeRec) / CumulativeWH)) %>%
        mutate(TRIR = ifelse(is.na(TRIR), 0.00,
                             ifelse(CumulativeWH == 0, 0.00, round(TRIR, 2)))) %>%
        select(Year,Month,TRIR)
      # DART df
      ytd_cont_dart = cont_data_dart %>%
        dplyr::filter(`Contractor Name` %in% input$cont_input_ytd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  DaysAwayRec = sum(DaysAwayRec)) %>%
        dplyr::filter(Year == input$year_input_ytd_cont) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeDart = cumsum(DaysAwayRec)) %>%
        mutate(DART = ((200000 * CumulativeDart) / CumulativeWH)) %>%
        mutate(DART = ifelse(is.na(DART), 0.00, 
                             ifelse(CumulativeWH == 0, 0.00, round(DART, 2)))) %>%
        select(Year,Month,DART)
      # LTIR df  
      ytd_cont_ltir = cont_data_ltir %>%
        dplyr::filter(`Contractor Name` %in% input$cont_input_ytd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  LostTimeRec = sum(LostTimeRec)) %>%
        dplyr::filter(Year == input$year_input_ytd_cont) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(CumulativeWH = cumsum(Workhours)) %>%
        mutate(CumulativeLtir = cumsum(LostTimeRec)) %>%
        mutate(LTIR = ((200000 * CumulativeLtir) / CumulativeWH)) %>%
        mutate(LTIR = ifelse(is.na(LTIR), 0.00, 
                             ifelse(CumulativeWH == 0, 0.00, round(LTIR, 2)))) %>%
        select(Year,Month,LTIR)
      
      # Combine TRIR,DART,LTIR dfs
      ytd_cont_combined_Rec_KPI = bind_rows(
        mutate(ytd_cont_trir, Metric = "TRIR"),
        mutate(ytd_cont_dart, Metric = "DART"),
        mutate(ytd_cont_ltir, Metric = "LTIR")
      ) %>%
        mutate(Rates = coalesce(TRIR, DART, LTIR)) %>%
        select(-TRIR, -DART, -LTIR)
      return(ytd_cont_combined_Rec_KPI)
    })
    
    # Render TRIR Plots for Contractor Analysis
    
    output$ytd_cont_RecKPIplot = plotly::renderPlotly({
      if (length(input$cont_input_ytd) > 0 & length(input$year_input_ytd_cont) > 0) {
        #ytd_recKPI_data = ytd_proj_recKPI()  # Store the reactive value in a variable
        
        gg_ytd_cont_recKPI = ggplot(ytd_cont_recKPI(), aes(x = Month, 
                                                      y = Rates,
                                                      group = Metric,
                                                      color = Metric,
                                                      text = paste(Metric,": ",Rates))) +
          geom_line() +
          labs(title = "Recordable Case Rates", xlab="Month", ylab="Rates") +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1),
                legend.position = "left") +
          scale_y_continuous(breaks = seq(0, (max(ytd_cont_recKPI()$Rates)+0.25), by = 1),
                             limits = c(0, (max(ytd_cont_recKPI()$Rates)+0.25))) +
          scale_color_manual(values = c("TRIR" = "blue", "DART" = "red", "LTIR" = "green"))
      }
      
      plotly::ggplotly(gg_ytd_cont_recKPI, tooltip = "text")
      
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic ytd-Recordable df for Table and plots
  {
    # Create ytd-rec function to call dataset based on user Contractor input  
    ytd_cont_rec = reactive({
      return(cont_data_trir  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ytd) %>%
               group_by(Year, Month) %>%
               summarise(Recordables = sum(Recordables), 
                         `OSHA Classification` = paste(`OSHA Classification`[!is.na(`OSHA Classification`)], collapse = ", ")) %>%
               dplyr::filter(Year == input$year_input_ytd_cont) %>%
               ungroup() %>%
               mutate(CumulativeRec = cumsum(Recordables))
      )
    })
    
    # Render Rec Plot for mtd-Contractor Analysis
    output$ytd_cont_recplot = plotly::renderPlotly({
      if (length(input$cont_input_ytd) > 0 & length(input$year_input_ytd_cont) > 0) {
        
        gg_ytd_cont_rec = ggplot(ytd_cont_rec(), aes(x = Month, y = CumulativeRec, group = 1, text = `OSHA Classification`)) +
          geom_line(color = "orange") +
          labs(title = "Recordables", xlab = "Month", ylab = "Recordable Count") +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1)) +
          scale_y_continuous(breaks = seq(0, (max(ytd_cont_rec()$CumulativeRec)+0.5), by = 1),
                             limits = c(0, (max(ytd_cont_rec()$CumulativeRec)+0.5)))
        
        plotly::ggplotly(gg_ytd_cont_rec, tooltip = "text")
      }
    })
  } 
  # ----------------------------------------------------------------------- #   
  # Dynamic cont-ytd-KPIs (Projects)
  {
    # KPI: # of Projects
    cont_ytd_proj = reactive({
      return(hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ytd, Year %in% input$year_input_ytd_cont) %>%
               distinct(`Project #`) %>%
               summarise(Count = n()) %>%
               pull(Count)
      )
    })
    
    # Render Contractors infoBoxOutput for ytd-Contractor Analysis
    output$cont_ytd_numProj = renderUI({
      if (length(input$cont_input_ytd) > 0 & length(input$year_input_ytd_cont) > 0) {
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Projects</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(cont_ytd_proj() > 999999,
                              paste0((round(cont_ytd_proj() / 1000000, digits = 0)),"M"),
                              ifelse(cont_ytd_proj() > 999,
                                     paste0((round(cont_ytd_proj() / 1000, digits = 0)),"K"),
                                     cont_ytd_proj())
                       ),"</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-city","</div>"),
          style = "light"
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic cont-ytd-KPIs (workers)
  {
    #KPI: # of Workers
    cont_ytd_Wrkr = reactive({
      return(hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ytd, Year %in% input$year_input_ytd_cont) %>%
               group_by(Year) %>%
               summarise(`Worker Count` = sum(`Worker Count`)) %>%
               pull(`Worker Count`)
      )
    })
    
    # Render Workers infoBoxOutput for ytd-Contractor Analysis
    output$cont_ytd_numWrk = renderUI({
      if (length(input$cont_input_ytd) > 0 & length(input$year_input_ytd_cont) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>","Workers","</div>"),
          value = HTML("<div style='text-align: right; margin-right: 10px;'>",
                       ifelse(cont_ytd_Wrkr() > 999999, 
                              paste0((round(cont_ytd_Wrkr() / 1000000, digits = 0)),"M"), 
                              ifelse(cont_ytd_Wrkr() > 999,
                                     paste0((round(cont_ytd_Wrkr() / 1000, digits = 0)),"K"),
                                     cont_ytd_Wrkr())
                       )
                       ,"</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-person-running","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic cont-ytd-KPIs (hours)
  {
    # KPI: # of hours
    cont_ytd_hours = reactive({
      return(cont_month_hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ytd, Year %in% input$year_input_ytd_cont) %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours)) %>%
               pull(Workhours)
      )
    })
    
    # Render hours infoBoxOutput for ytd-Contractor Analysis
    output$cont_ytd_numhour = renderUI({
      if (length(input$cont_input_ytd) > 0 & length(input$year_input_ytd_cont) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Work-Hours</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(cont_ytd_hours() > 999999, 
                              paste0((round(cont_ytd_hours() / 1000000, digits = 1)),"M"), 
                              ifelse(cont_ytd_hours() > 999,
                                     paste0((round(cont_ytd_hours() / 1000, digits = 0)),"K"),
                                     cont_ytd_hours())
                       ),
                       "</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-clock","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #  
  # Dynamic cont-ytd-KPIs (TRIR)
  {
    # KPI: TRIR
    cont_ytd_trir = reactive({
      return(cont_data_trir  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ytd) %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours),
                         Recordables = sum(Recordables)) %>%
               dplyr::filter(Year == input$year_input_ytd_cont) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
               mutate(TRIR = ifelse(TRIR == 0, as.character("0.00"), round(TRIR, 2))) %>%
               pull(TRIR)
      )
    })
    
    # Render TRIR infoBoxOutput for ytd-Contractor Analysis
    output$cont_ytd_trir = renderUI({
      if (length(input$cont_input_ytd) > 0 & length(input$year_input_ytd_cont) > 0) {
        summaryBox3(
          title = HTML("<center style='color: blue;'>TRIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       cont_ytd_trir(),
                       "</p></center>"),
          style = "primary",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic cont-ytd-KPIs (DART)
  {
    # KPI: DART
    cont_ytd_dart = reactive({
      return(cont_data_dart  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ytd) %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours),
                         DaysAwayRec = sum(DaysAwayRec)) %>%
               dplyr::filter(Year == input$year_input_ytd_cont) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
               mutate(DART = ifelse(DART == 0, as.character("0.00"), round(DART, 2))) %>%
               pull(DART)
      )
    })
    
    # Render DART infoBoxOutput for ytd-Contractor Analysis
    output$cont_ytd_dart = renderUI({
      if (length(input$cont_input_ytd) > 0 & length(input$year_input_ytd_cont) > 0) {
        summaryBox3(
          title = HTML("<center style='color: red;'>DART</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       cont_ytd_dart(),
                       "</p></center>"),
          style = "danger",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic cont-ytd-KPIs (LTIR)
  {
    # KPI: LTIR
    cont_ytd_ltir = reactive({
      return(cont_data_ltir  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_ytd) %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours),
                         LostTimeRec = sum(LostTimeRec)) %>%
               dplyr::filter(Year == input$year_input_ytd_cont) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
               mutate(LTIR = ifelse(LTIR == 0, as.character("0.00"), round(LTIR, 2))) %>%
               pull(LTIR)
      )
    })
    
    # Render LTIR infoBoxOutput for ytd-Contractor Analysis
    output$cont_ytd_ltir = renderUI({
      if (length(input$cont_input_ytd) > 0 & length(input$year_input_ytd_cont) > 0) {
        summaryBox3(
          title = HTML("<center style='color: green;'>LTIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       cont_ytd_ltir(),
                       "</p></center>"),
          style = "success",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  
}

# Contractors-MTD
{  
  #Dynamic Title Headers
  {
    # Checkbox to allow multiple project selection
    # multiple_proj_sel = reactive({
    #   if (input$checkbox_input) {
    #     return(TRUE)
    #   } else {
    #     return(FALSE)
    #   }
    # })
    # 
    # observe({
    #   updateSelectInput(session, "projnum_input", multiple = multiple_proj_sel())
    # })
    
    
    # result: cont_title -> returns title of Contractor input
    cont_title_mtd = reactive({
      return(hours %>%
               distinct(`Contractor Name`) %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_mtd) %>%
               pull())
    })
    
    output$contTitle_mtd = renderText({
      cont_title_mtd()
    })
  }
  # ----------------------------------------------------------------------- #   
  #Dynamic Contractor Analysis Drop Downs
  {
    # Filter cont_month_hours based on JContractor Name user input 
    choices_year_mtd_cont = reactive({
      return(cont_month_hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_mtd)
      )
    })
    
    # Update Year user input based on filtered ov_month_hours  
    observe({
      if (length(choices_year_mtd_cont()) > 0) {
        updateSelectInput(session = session, 
                          inputId = "year_input_mtd_cont", 
                          choices = sort(unique(choices_year_mtd_cont()$Year), decreasing = TRUE)
        )
      }
    })
    
    # Filter proj_month_hours based on Job Number user input 
    choices_month_mtd_cont = reactive({
      return(cont_month_hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_mtd) %>%
               dplyr::filter(Year == input$year_input_mtd_cont)
      )
    })
    
    # Update Month user input based on filtered ov_month_hours  
    observe({
      if (length(choices_month_mtd_cont()) > 0) {
        updateSelectInput(session = session, 
                          inputId = "month_input_mtd_cont", 
                          choices = sort(unique(choices_month_mtd_cont()$Month), decreasing = TRUE)
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic mtd-TRIR df for Tables and Plots
  { 
    # Create mtd-trir function to call dataset based on user Contractor and Year input
    
    
    # TRIR df  
    mtd_cont_recKPI = reactive({
      mtd_cont_trir = cont_data_trir  %>%
        dplyr::filter(`Contractor Name` %in% input$cont_input_mtd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  Recordables = sum(Recordables)) %>%
        dplyr::filter(Year == input$year_input_mtd_cont) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
        mutate(TRIR = ifelse(is.na(TRIR), 0.00,
                             ifelse(Workhours == 0, 0.00, round(TRIR, 2)))) %>%
        select(Year,Month,TRIR)
      # DART df
      mtd_cont_dart = cont_data_dart %>%
        dplyr::filter(`Contractor Name` %in% input$cont_input_mtd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  DaysAwayRec = sum(DaysAwayRec)) %>%
        dplyr::filter(Year == input$year_input_mtd_cont) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
        mutate(DART = ifelse(is.na(DART), 0.00,
                             ifelse(Workhours == 0, 0.00, round(DART, 2)))) %>%
        select(Year,Month,DART)
      # LTIR df  
      mtd_cont_ltir = cont_data_ltir %>%
        dplyr::filter(`Contractor Name` %in% input$cont_input_mtd) %>%
        group_by(Year, Month) %>%
        summarise(Workhours = sum(Workhours),
                  LostTimeRec = sum(LostTimeRec)) %>%
        dplyr::filter(Year == input$year_input_mtd_cont) %>%
        ungroup() %>%
        # group_by(`Project #`, `Project Name`) %>%
        mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
        mutate(LTIR = ifelse(is.na(LTIR), 0.00,
                             ifelse(Workhours == 0, 0.00, round(LTIR, 2)))) %>%
        select(Year,Month,LTIR)
      # Combine TRIR,DART,LTIR dfs
      mtd_cont_combined_Rec_KPI = bind_rows(
        mutate(mtd_cont_trir, group = "TRIR"),
        mutate(mtd_cont_dart, group = "DART"),
        mutate(mtd_cont_ltir, group = "LTIR")
      ) %>%
        mutate(Rates = coalesce(TRIR, DART, LTIR)) %>%
        select(-TRIR, -DART, -LTIR)
      return(mtd_cont_combined_Rec_KPI)
    })
    
    # Render TRIR Plots for Project Analysis
    
    output$mtd_cont_RecKPIplot = plotly::renderPlotly({
      if (length(input$cont_input_mtd) > 0 & length(input$year_input_mtd_cont) > 0) {
        gg_mtd_cont_recKPI = ggplot(mtd_cont_recKPI(), aes(x = Month, 
                                                      y = Rates,
                                                      group = group,
                                                      color = group,
                                                      text = paste(group,": ",Rates))) +
          geom_line() +
          labs(title = "Recordable Case Rates", xlab("Month"), ylab("Rates")) +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1),
                legend.position = "top") +
          scale_y_continuous(breaks = seq(0, (max(mtd_cont_recKPI()$Rates)+0.5), by = 1),
                             limits = c(0, (max(mtd_cont_recKPI()$Rates)+0.5))) +
          scale_color_manual(values = c("TRIR" = "blue", "DART" = "red", "LTIR" = "green"))
      }
      
      plotly::ggplotly(gg_mtd_cont_recKPI, tooltip = "text")
      
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic mtd-Recordable df for Table and plots
  {
    # Create mtd-rec function to call dataset based on user Contractor and Year input  
    mtd_cont_rec = reactive({
      return(data_cmr  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_mtd) %>%
               group_by(Year, Month) %>%
               summarise(Recordables = sum(Recordables), 
                         `OSHA Classification` = paste(`OSHA Classification`, collapse = ", ")) %>%
               dplyr::filter(Year == input$year_input_mtd_cont)
      )
    })
    
    # Render Rec Plot for mtd-Project Analysis
    output$mtd_cont_recplot = plotly::renderPlotly({
      if (length(input$cont_input_mtd) > 0 & length(input$year_input_mtd_cont) >= 0) {
        
        gg_mtd_cont_rec = ggplot(mtd_cont_rec(), aes(x = Month, y = Recordables, text = `OSHA Classification`)) +
          geom_bar(stat = "identity", fill = "orange", width = 0.75) +
          labs(title = "Recordables", xlab = "Month", ylab = "Recordable Count") +
          theme_minimal() + 
          theme(axis.line.x = element_line(color = "black", linewidth = 1, linetype = 1),
                axis.line.y = element_line(color = "black", linewidth = 1, linetype = 1))
        
        if (any(!is.na(mtd_cont_rec()$Recordables))) {
          gg_mtd_cont_rec = gg_mtd_cont_rec +
            scale_y_continuous(breaks = seq(0, (max(mtd_cont_rec()$Recordables)+0.5), by = 1),
                               limits = c(0, (max(mtd_cont_rec()$Recordables)+0.5)))
        }
        
        plotly::ggplotly(gg_mtd_cont_rec, tooltip = "text")
      }
    })
  } 
  # ----------------------------------------------------------------------- #   
  # Dynamic cont-mtd-KPIs (Projects)
  {
    # KPI: # of Contractors
    cont_mtd_proj = reactive({
      return(hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_mtd, 
                             Year %in% input$year_input_mtd_cont,
                             Month %in% input$month_input_mtd_cont) %>%
               distinct(`Project #`) %>%
               summarise(Count = n()) %>%
               pull(Count)
      )
    })
    
    # Render Contractors infoBoxOutput for mtd-Project Analysis
    output$cont_mtd_numProj = renderUI({
      if (length(input$cont_input_mtd) > 0 & length(input$year_input_mtd_cont) > 0) {
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Projects</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(cont_mtd_proj() > 999999,
                              paste0((round(cont_mtd_proj() / 1000000, digits = 0)),"M"),
                              ifelse(cont_mtd_proj() > 999,
                                     paste0((round(cont_mtd_proj() / 1000, digits = 0)),"K"),
                                     cont_mtd_proj())
                       ),"</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-city","</div>"),
          style = "light"
        )
      }
    })
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic cont-mtd-KPIs (workers)
  {
    #KPI: # of Workers
    cont_mtd_Wrkr = reactive({
      return(hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_mtd, 
                             Year %in% input$year_input_mtd_cont,
                             Month %in% input$month_input_mtd_cont) %>%
               summarise(`Worker Count` = sum(`Worker Count`)) %>%
               pull(`Worker Count`)
      )
    })
    
    # Render Workers infoBoxOutput for mtd-Project Analysis
    output$cont_mtd_numWrk = renderUI({
      if (length(input$cont_input_mtd) > 0 & length(input$year_input_mtd_cont) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>","Workers","</div>"),
          value = HTML("<div style='text-align: right; margin-right: 10px;'>",
                       ifelse(cont_mtd_Wrkr() > 999999, 
                              paste0((round(cont_mtd_Wrkr() / 1000000, digits = 0)),"M"), 
                              ifelse(cont_mtd_Wrkr() > 999,
                                     paste0((round(cont_mtd_Wrkr() / 1000, digits = 0)),"K"),
                                     cont_mtd_Wrkr())
                       )
                       ,"</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-person-running","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #   
  # Dynamic cont-mtd-KPIs (hours)
  {
    # KPI: # of hours
    cont_mtd_hours = reactive({
      return(cont_month_hours %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_mtd, 
                             Year %in% input$year_input_mtd_cont,
                             Month %in% input$month_input_mtd_cont) %>%
               group_by(Year) %>%
               summarise(Workhours = sum(Workhours)) %>%
               pull(Workhours)
      )
    })
    
    # Render hours infoBoxOutput for mtd-Project Analysis
    output$cont_mtd_numhour = renderUI({
      if (length(input$cont_input_mtd) > 0 & length(input$year_input_mtd_cont) > 0) {
        
        summaryBox2(
          title = HTML("<div style='text-align: right;'>Work-Hours</div>"),
          value = HTML("<div style='text-align: right; margin-right: 15px;'>",
                       ifelse(cont_mtd_hours() > 999999, 
                              paste0((round(cont_mtd_hours() / 1000000, digits = 1)),"M"), 
                              ifelse(cont_mtd_hours() > 999,
                                     paste0((round(cont_mtd_hours() / 1000, digits = 0)),"K"),
                                     cont_mtd_hours())
                       ),
                       "</div>"),
          width = 4,
          icon = HTML("<div style='text-align: left; margin-left: 20px;'>","fas fa-clock","</div>"),
          style = "light"
        )
      }
    })  
  }
  # ----------------------------------------------------------------------- #  
  # Dynamic cont-mtd-KPIs (TRIR)
  {
    # KPI: TRIR
    cont_mtd_trir = reactive({
      return(cont_data_trir  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_mtd,
                             Year %in% input$year_input_mtd_cont) %>%
               group_by(Month) %>%
               summarise(Workhours = sum(Workhours),
                         Recordables = sum(Recordables)) %>%
               dplyr::filter(Month %in% input$month_input_mtd_cont) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(TRIR = ((200000 * Recordables) / Workhours)) %>%
               mutate(TRIR = ifelse(TRIR == 0, as.character("0.00"), round(TRIR, 2))) %>%
               pull(TRIR)
      )
    })
    
    # Render TRIR infoBoxOutput for mtd-contractor Analysis
    output$cont_mtd_trir = renderUI({
      if (length(input$cont_input_mtd) > 0 & length(input$year_input_mtd_cont) > 0) {
        summaryBox3(
          title = HTML("<center style='color: blue;'>TRIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       cont_mtd_trir(),
                       "</p></center>"),
          style = "primary",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic cont-mtd-KPIs (DART)
  {
    # KPI: DART
    cont_mtd_dart = reactive({
      return(cont_data_dart  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_mtd,
                             Year %in% input$year_input_mtd_cont) %>%
               group_by(Month) %>%
               summarise(Workhours = sum(Workhours),
                         DaysAwayRec = sum(DaysAwayRec)) %>%
               dplyr::filter(Month %in% input$month_input_mtd_cont) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(DART = ((200000 * DaysAwayRec) / Workhours)) %>%
               mutate(DART = ifelse(DART == 0, as.character("0.00"), round(DART, 2))) %>%
               pull(DART)
      )
    })
    
    # Render DART infoBoxOutput for mtd-Project Analysis
    output$cont_mtd_dart = renderUI({
      if (length(input$cont_input_mtd) > 0 & length(input$year_input_mtd_cont) > 0) {
        summaryBox3(
          title = HTML("<center style='color: red;'>DART</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       cont_mtd_dart(),
                       "</p></center>"),
          style = "danger",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  # ----------------------------------------------------------------------- #  
  # Dynamic cont-mtd-KPIs (LTIR)
  {
    # KPI: LTIR
    cont_mtd_ltir = reactive({
      return(cont_data_ltir  %>%
               dplyr::filter(`Contractor Name` %in% input$cont_input_mtd,
                             Year %in% input$year_input_mtd_cont) %>%
               group_by(Month) %>%
               summarise(Workhours = sum(Workhours),
                         LostTimeRec = sum(LostTimeRec)) %>%
               dplyr::filter(Month %in% input$month_input_mtd_cont) %>%
               ungroup() %>%
               # group_by(`Project #`, `Project Name`) %>%
               mutate(LTIR = ((200000 * LostTimeRec) / Workhours)) %>%
               mutate(LTIR = ifelse(LTIR == 0, as.character("0.00"), round(LTIR, 2))) %>%
               pull(LTIR)
      )
    })
    
    # Render LTIR infoBoxOutput for mtd-Project Analysis
    output$cont_mtd_ltir = renderUI({
      if (length(input$cont_input_mtd) > 0 & length(input$year_input_mtd_cont) > 0) {
        summaryBox3(
          title = HTML("<center style='color: green;'>LTIR</center>"),
          value = HTML("<center><p style='font-size:45px'>",
                       cont_mtd_ltir(),
                       "</p></center>"),
          style = "success",
          width = 4,
          icon = F
        )
      }
    })  
  }  
  
}  
}
  
}


