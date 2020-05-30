#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define server logic 
shinyServer(function(input, output) {
    
    # ------------------
    # Data Preview
    data_preview <- reactive({
        switch(input$data_preview,
                 'Confirmed cases' = confirmed,
                 'Deaths'=deaths,
                 'Recovered'=recovered)
    })
    
    output$output_data_preview <- DT::renderDataTable(
        data_preview() %>% filter(date==input$data_preview_date),
        options = list(scrollX = TRUE)
    )
    
    
    # ------------------
    # Geo map
    # total numbers for map tab
    df_conf_date <- reactive({
        confirmed %>% filter(date==input$map_plot_show_date)
    })
    df_death_date <- reactive({
        deaths %>% filter(deaths, date==input$map_plot_show_date)
    })
    output$confirmed_cnt <- renderText({
        paste0('   Total confirmed cases: ', 
               formatC(sum(df_conf_date()$confirmed), big.mark=',')
               )
    })
    output$deaths_cnt <- renderText({
        paste0('   Total deaths: ', 
               formatC(sum(df_death_date()$deaths), big.mark = ',')
               )
    })
    output$deaths_rate <- renderText({
        paste0('   Death rate: ', 
               round(sum(df_death_date()$deaths)/sum(df_conf_date()$confirmed)*100, 2),
               '% (black circles)'
        )
    })
    output$recovered_cnt <- renderText({
        paste0('   Total recovered: ', 
               formatC(sum(filter(recovered, date==input$map_plot_show_date)$recovered), big.mark = ',')
               )
    })
    output$for_date <- renderText({
        format(as.POSIXlt(input$map_plot_show_date),"%d %B %Y")
    })
    
    # update data for plotting
    confirmed_filt <- reactive({
        filter(confirmed_by_state, date==input$map_plot_show_date)
    })
    deaths_filt <- reactive({
        filter(deaths_by_state, date==input$map_plot_show_date)
    })
    
    data_by_state_filt <- reactive({
        data_by_state %>% filter(date==input$map_plot_show_date)
    })
    states_reactive <- reactive({
        states %>%
            sp::merge(data_by_state_filt(),
                      by.x='NAME', by.y='Province_State')
    })
    
    # color pallete
    bins <- c(0, 500, 2000, 5000, 10000, 20000, 50000, 80000, 100000, 200000, Inf)
    pal <- reactive({
        colorBin("YlOrRd", domain=confirmed$confirmed, bins=bins)
    })
    
    # base map
    output$mymap <- renderLeaflet({ 
        leaflet(states_reactive()) %>%
            setView(-120.483330, 38.712046, zoom = 3) %>%  # center of the US
            addTiles() %>%
            addPolygons(
                group='Confirmed',
                stroke = T, smoothFactor = 0.3, 
                fillColor=~pal()(confirmed), 
                weight = 2, opacity = 10,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                label=~lapply(labs, htmltools::HTML),
                highlight = highlightOptions(
                    weight = 3,
                    color = "#4A81CF",
                    dashArray = "",
                    fillOpacity = 0.9,
                    bringToFront = TRUE)) %>%
            addLegend(pal = pal(), values = ~confirmed, opacity = 0.7,
                      position='bottomright', group = 'Confirmed',
                      title=paste0('Confirmed cases as of <br>', input$map_plot_show_date)) %>%
            addCircleMarkers(data=data_by_state_filt() %>% filter(!is.na(death_rate)),
                             lng=~Long, lat=~Lat, 
                             radius=~death_rate*100, color='black', 
                             group='Death_rates', 
                             popup=~paste0(Province_State, ': death rate - ', round(death_rate*100,2), '%')) #%>%
            # addLegend(group = 'Death_rates', color='black', title='Death rate', )
    })
    
    
    
    # -------------
    # Metrics
    
    # motionchart
    output$gvismotion <- renderGvis({
        gvisMotionChart(df_gvis, timevar='date', idvar='Province_State')
    })
    
    # geo chart
    df_gvis_date <- reactive({
        df_gvis %>% 
            filter(date==input$gvis_geo_date)
    })
    
    output$gvisgeo <- renderGvis({
        gvisGeoChart(df_gvis_date(), 'Province_State', input$gvis_geo_var,
                     options=list(region="US", 
                                  displayMode="regions", 
                                  resolution="provinces",
                                  width=600, height=400))
    })
    
    # annotation chart
    output$gvisannotation <- renderGvis({
        gvisAnnotationChart(df_gvis, datevar='date', numvar=input$gvis_ann_var,
                            idvar = 'Province_State',
                            options=list(width=600, height=300))
    })
    
    # merge charts:
    output$gvisplot <- renderGvis({
        # motion
        m <- gvisMotionChart(df_gvis, timevar='date', idvar='Province_State',
                             colorvar='Province_State',
                             options=list(width=300, height=300))
        
        # geo
        g <- gvisGeoChart(df_gvis_date, 'Province_State', 'confirmed',
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=300, height=300))
        
        # annotation
        a <- gvisAnnotationChart(df_gvis, datevar='date', numvar=input$gvisannvar,
                                 idvar = 'Province_State',
                                 options=list(width=1000, height=200))
        
        # merge plots:
        res <- gvisMerge(g, m, horizontal=T)
        res <- gvisMerge(res, a, horizontal=F)
        
        return(res)
    })
    
    
    
    # Metrics panel
    # geo chart filter
    df_gvis_date <- reactive({
        df_gvis %>% 
            filter(date==input$gvis_geo_date)
    })
    
    # states filter
    df_gvis_states <- reactive({
        df_gvis %>%
            filter(Province_State %in% input$gvis_states_picker)
    })
    
    
    # metrics output
    output$test_gvis <- renderGvis({
        switch(input$gvis_type,
               'Geo Map' = gvisGeoChart(df_gvis_date(), 'Province_State', input$gvis_geo_var,
                                        options=list(region="US", 
                                                     displayMode="regions", 
                                                     resolution="provinces",
                                                     width=600, height=500)), 
               'Motion Chart' = gvisMotionChart(df_gvis_states(), 
                                                timevar='date', idvar='Province_State',
                                                sizevar='confirmed',
                                                # colorvar='Province_State',
                                                options=list(width=600, height=500)), 
               'Annotation Chart' = gvisAnnotationChart(df_gvis_states(), 
                                                        datevar='date', 
                                                        numvar=input$gvis_ann_var,
                                                        idvar = 'Province_State',
                                                        options=list(width=600))
               )
    })
    
    
    # -------------
    # Forecast
    df_forc_geo <- reactive({
        confirmed_by_geo %>% filter(Province_State==input$forecast_geo)
    })
    
    # initial lot for selection:
    output$plot_incidence_df <- renderPlot({
        ggplot(df_forc_geo(), aes(date, new_cases)) +
            geom_bar(fill='#9fc2fc', stat='identity') +
            geom_line(color='blue') +
            geom_point(color='blue')
    })
    
    # brushed data plot
    output$brushed_plot <- renderPlotly({
        brushed_data <- brushedPoints(
            df_forc_geo(),
            input$plot_brush,
            xvar='date', yvar='new_cases'
        )
        print(head(brushed_data))
        if (nrow(brushed_data) < 10) return(NULL)
        else {
            p <- ggplot(brushed_data, aes(date, new_cases)) +
                geom_point(color='blue') +
                geom_line(color='blue') +
                geom_bar(stat='identity', fill='#9fc2fc') +
                ggtitle('Brushed data') 
    
            return(hide_legend(ggplotly(p)))
        }
    })
    
    model <- reactive({
        brushed_data <- brushedPoints(
            df_forc_geo(),
            input$plot_brush,
            xvar='date', yvar='new_cases'
        )
        if (nrow(brushed_data) < 10) {
            return(NULL)
        } else {
            df_inc <- as.incidence(brushed_data$new_cases, dates=brushed_data$date)
            df_inc_all <- as.incidence(df_forc_geo()$new_cases, dates=df_forc_geo()$date)
            model <- tryCatch(fit_optim_split(df_inc),
                              error = function(e){
                                  message("An error occurred:\n", e)
                                  fit(df_inc)
                              },
                              warning = function(w){
                                  message("A warning occured:\n", w)
                                  fit(df_inc)
                              },
                              finally = {
                                  message("Finally done!")
                              })
        }
        
        return(model)
    })
    
    output$forecast_plot <- renderPlotly({
        df_inc_all <- as.incidence(df_forc_geo()$new_cases, dates=df_forc_geo()$date)
        ann_text <- '(!) Warning: \nUnable to fit the model on selected data'
        if (!is.null(model())){
            if(class(model())=='incidence_fit') {
                p <- plot(df_inc_all, fit = model(), color = "#9fc2fc")
                info <- model()$info
                ann_text <- paste0('Model results:',
                                  '\n    r_est = ', round(info$r*100, 2), '%',
                                  ' (CI=[', round(info$r.conf[1]*100, 2), '%',
                                  round(info$r.conf[2]*100,2), '%]')
                # print(ann_text)
            } else {
                p <- plot(df_inc_all, fit = model()$fit, color = "#9fc2fc") 
                f <- get_fit(model()$fit)
                ann_text <- paste0('Model results:',
                                   '\n * estimated peak date: ', model()$split,
                                   '\n * "before the peak" model:',
                                   '\n    r_est = ', round(f$before$info$r*100, 2), '%',
                                   ' (CI=[', round(f$before$info$r.conf[1]*100, 2), '%, ',
                                   round(f$before$info$r.conf[2]*100,2), '%]',
                                   '\n * "after the peak" model:',
                                   '\n    r_est = ', round(f$after$info$r*100, 2), '%',
                                   ' (CI=[', round(f$after$info$r.conf[1]*100, 2), '%, ',
                                   round(f$after$info$r.conf[2]*100,2), '%]')
                # print(ann_text)
            }
            p <- p + ggtitle('Model fit:') +
                geom_point(data=df_forc_geo(), aes(date, new_cases), color='blue') +
                geom_line(data=df_forc_geo(), aes(date, new_cases), color='blue') +
                theme(legend.title = element_blank())  +
                ylim(c(0,max(df_forc_geo()$new_cases)*1.5))
        } else {
            p <- ggplot(df_forc_geo(), aes(date, new_cases)) +
                geom_point(color='blue') +
                geom_line(color='blue') +
                geom_bar(stat='identity', fill='#9fc2fc') +
                ylim(c(0, max(df_forc_geo()$new_cases)))
        }
        
        return(hide_legend(ggplotly(p)))
    })
    
    output$ann_text_html <- renderUI({
        if (!is.null(model())){
            if(class(model())=='incidence_fit') {
                info <- model()$info
                s1 <- paste0('   r_est = ', round(info$r*100, 2), '%,')
                s2 <- paste0('----  conf.int: [', round(info$r.conf[1]*100, 2), '%',
                             round(info$r.conf[2]*100,2), '%]')
                res <- paste(s1, s2, sep = '<br/>')
            } else {
                f <- get_fit(model()$fit)
                s2 <- paste0('Est. peak date: ', model()$split)
                s3 <- ' - "before the peak" model:'
                s4 <- paste0('    r_est = ', round(f$before$info$r*100, 2), '% ')
                s44 <- paste0('----  conf.int: [', round(f$before$info$r.conf[1]*100, 2), '%, ',
                             round(f$before$info$r.conf[2]*100,2), '%]')
                s5 <- ' - "after the peak" model:'
                s6 <- paste0('   r_est = ', round(f$after$info$r*100, 2), '%')
                s66 <- paste0('----  conf.int: [', round(f$after$info$r.conf[1]*100, 2), '%, ',
                             round(f$after$info$r.conf[2]*100,2), '%]')
                res <- paste(s2,s3,s4,s44,s5,s6,s66, sep='<br/>')
            }
        } else res <- paste('(!)Warning:','Unable to fit the model on selected data',sep='<br/>')
        
        return(HTML(res))
    })
    
    output$ann_text <- renderText({
        if (!is.null(model())){
            if(class(model())=='incidence_fit') {
                info <- model()$info
                ann_text <- paste0('Model results:',
                                   '\n    r_est = ', round(info$r*100, 2), '%',
                                   ' (CI=[', round(info$r.conf[1]*100, 2), '%',
                                   round(info$r.conf[2]*100,2), '%]')
            } else {
                f <- get_fit(model()$fit)
                paste0('Model results:',
                       '\n * estimated peak date: ', model()$split,
                       '\n * "before the peak" model:',
                       '\n    r_est = ', round(f$before$info$r*100, 2), '%',
                       ' (CI=[', round(f$before$info$r.conf[1]*100, 2), '%, ',
                       round(f$before$info$r.conf[2]*100,2), '%]',
                       '\n * "after the peak" model:',
                       '\n    r_est = ', round(f$after$info$r*100, 2), '%',
                       ' (CI=[', round(f$after$info$r.conf[1]*100, 2), '%, ',
                       round(f$after$info$r.conf[2]*100,2), '%]')
            }
        } else '(!) Warning: \nUnable to fit the model on selected data'
    })
    
    
    output$forecast_plot2 <- renderPlotly({
        brushed_data <- brushedPoints(
            df_forc_geo(),
            input$plot_brush,
            xvar='date', yvar='new_cases'
        )
        model <- NULL
        # print(head(brushed_data))
        if (nrow(brushed_data) < 10) {
            return(NULL)
        } else {
            df_inc <- as.incidence(brushed_data$new_cases, dates=brushed_data$date)
            df_inc_all <- as.incidence(df_forc_geo()$new_cases, dates=df_forc_geo()$date)
            model <- tryCatch(fit_optim_split(df_inc),
                     error = function(e){
                         message("An error occurred:\n", e)
                         fit(df_inc)
                     },
                     warning = function(w){
                         message("A warning occured:\n", w)
                         fit(df_inc)
                     },
                     finally = {
                         message("Finally done!")
                     })
        
            if (!is.null(model)){
                if(class(model)=='incidence_fit') {
                    p <- plot(df_inc_all, fit = model, color = "#9fc2fc") 
                } else {
                    p <- plot(df_inc_all, fit = model$fit, color = "#9fc2fc") 
                }
                p + ggtitle('Some title') +
                    theme(legend.title = element_blank())  +
                    ylim(c(0,max(df$new_cases)*1.5))
            } else {
                p <- ggplot(df_forc_geo(), aes(date, new_cases)) +
                    geom_point(color='red') +
                    geom_line(color='red') +
                    geom_bar(stat='identity', fill='#9fc2fc') +
                    ggtitle('(!) Warning: Unable to fit the model (too few points selected)') +
                    ylim(c(0, max(df_forc_geo()$new_cases)))
            }
            hide_legend(ggplotly(p))
        }
        
        
    })
    
    # model <- reacive({
    #     df_inc <- as.incidence(df_forc_geo()$new_cases, dates=df_forc_geo()$date)
    #     brushed_data <- brushedPoints(
    #         df_forc_geo(), 
    #         input$plot_brush,
    #         xvar='date', yvar='Volume'
    #     )
    #     print(dim(brushed_data))
    #     if (nrow(brushed_data) < 2) return(NULL)
    #     lm(Volume~Girth, brushed_data)
    # })
    
   
    

})
