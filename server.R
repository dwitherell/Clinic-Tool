function(input, output, session) {
    
    #### TAB 2: LOAD AND PREVIEW DATA -------------------------------
    
    ### load data
    lever_percep_corr <- reactive({
        infile <- input$lever_percep_corr_file
        if (is.null(infile)) {
            return(NULL)
        }
        readRDS(infile$datapath)
    })
    
    ### preview raw data
    output$preview_raw <- renderDataTable({
        
        if (is.null(input$lever_percep_corr_file)){
            
            return(NULL)
            
        }
        
        out <- lever_percep_corr() %>% 
            mutate_if(is.numeric, funs(round(., 3))) %>%
            set_names(gsub("_", " ", names(.)))
        
        col_width <- paste0(100 / ncol(out))
        
        datatable(out,
                  rownames = FALSE,
                  options = list(
                      pageLength = 25,
                      columnDefs = list(
                          list(className = 'dt-center',
                               targets = '_all')
                      )
                  )
        )
        
    })
    
    ### preview means
    output$preview_means <- renderDataTable({
        
        if (is.null(input$lever_percep_corr_file)){
            
            return(NULL)
            
        }
        
        means <- lever_percep_corr() %>%
            group_by(Banner, Attribute) %>%
            summarise(Mean = mean(Weight * Rating)) %>%
            mutate_if(is.numeric, funs(round(., 3))) %>%
            spread(Banner, Mean)
        
        atts <- lever_percep_corr() %>% 
            select(starts_with("Att")) %>% 
            group_by(Attribute, Attribute_Code) %>%
            filter(row_number() == 1)
        
        out <- left_join(means, atts, by = "Attribute") %>%
            select(Attribute, Attribute_Code, everything()) %>%
            set_names(gsub("_", " ", names(.)))

        
        datatable(out,
                  rownames = FALSE,
                  options = list(
                      pageLength = 25,
                      columnDefs = list(
                          list(className = 'dt-center',
                               targets = '_all')
                      )
                  )
        )
        
    })
    
    #### TAB 3: LEVER MAP -------------------------------------------
    
    ### select target banner
    output$lvr_select_banner_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        banner_choices <- lever_percep_corr() %$% unique(Banner)
        
        selectInput(inputId = 'lvr_banner',
                    label = 'Target banner',
                    choices = banner_choices,
                    selected = banner_choices[[1]])
    })
    
    ### FILTER #1
    output$lvr_filter1_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        data <- lever_percep_corr()
        
        if (nrow(unique(data[8])) <= 1) {
            
            filter_name <- names(data[8])
            
            # if there is only one option...
            selectInput(inputId = 'lvr_filter1',
                        label = filter_name,
                        choices = "Total",
                        selected = "Total")    
            
        } else {
            
            # if the filter use useful...
            filter_name <- paste(names(data[8]), "(Filter 1)", sep = " ")
            
            filter_choices <- unique(data[8])
            
            selectInput(inputId = 'lvr_filter1',
                        label = filter_name,
                        choices = c("Total", filter_choices),
                        selected = "Total")
            
        }
        
    }) 
    
    ### FILTER #2
    output$lvr_filter2_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        data <- lever_percep_corr()
        
        if (nrow(unique(data[9])) <= 1) {
            
            filter_name <- names(data[9])
            
            # if there is only one option...
            selectInput(inputId = 'lvr_filter2',
                        label = filter_name,
                        choices = "Total",
                        selected = "Total")    
        
        } else {
            
            # if the filter use useful...
            filter_name <- paste(names(data[9]), "(Filter 2)", sep = " ")
            
            filter_choices <- unique(data[9])
            
            selectInput(inputId = 'lvr_filter2',
                        label = filter_name,
                        choices = c("Total", filter_choices),
                        selected = "Total")
            
        }
        
    }) 
    
    ### FILTER #3
    output$lvr_filter3_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        data <- lever_percep_corr()

        if (nrow(unique(data[10])) <= 1) {
            
            filter_name <- names(data[10])
            
            # if there is only one option...
            selectInput(inputId = 'lvr_filter3',
                        label = filter_name,
                        choices = "Total",
                        selected = "Total")    
            
        } else {
            
            # if the filter use useful...
            filter_name <- paste(names(data[10]), "(Filter 3)", sep = " ")
            
            filter_choices <- unique(data[10])
            
            selectInput(inputId = 'lvr_filter3',
                        label = filter_name,
                        choices = c("Total", filter_choices),
                        selected = "Total")
            
        }
        
    }) 
    
    ### choose correlation variable
    output$lvr_corr_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        corr_choices <- lever_percep_corr() %$% unique(Attribute)
        
        selectInput(inputId = 'lvr_corr',
                    label = 'Attribute to correlate',
                    choices = corr_choices)
    })
    
    ### uncheck attribute button
    output$lvr_uncheck_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        actionButton('lvr_uncheck', "Uncheck attributes", icon = icon('check-square'))
    })
    
    ### attribute choices for map
    output$lvr_attributes_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        att_choices <- lever_percep_corr() %$% unique(Attribute)
        
        controls <- tags$div(align = 'left', 
                             class = 'multicol', 
                             checkboxGroupInput(inputId  = 'lvr_attributes', 
                                                label    = "Attributes to include in map", 
                                                choices  = att_choices,
                                                inline   = FALSE))
        observeEvent(input$lvr_uncheck, {
            
            updateCheckboxGroupInput(session, "lvr_attributes", 
                                     choices = att_choices, 
                                     selected = if (input$lvr_uncheck) NULL)
        })
        
        controls
        
    })
    
    ### display number of attributes selected
    output$lvr_atts_selected <- renderText({
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        num <- as.numeric(length(input$lvr_attributes))
        out <- paste("# Attributes selected: ", num)
        out
        
    })
    
    ### display sample sizes
    lvr_sample_size <- eventReactive(input$lvr_submit == 0, {
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        num <- as.numeric(leverdata()[1, 6])  # 6th col is Target Sample Size
        out <- paste("Sample size: ", num)
        out

    }, ignoreInit = TRUE)
    
    output$lvr_sample_size_UI <- renderUI({
        out <- lvr_sample_size()
        print(out)
    })
    
    ### calculate lever data
    leverdata <- eventReactive(input$lvr_submit == 0, {
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        data <- lever_percep_corr()
        
        # filter 1
        if (input$lvr_filter1 == "Total") {
            
            out <- data
            
        } else { out <- data %>% filter(data[8] == input$lvr_filter1) }
        
        # filter 2
        if (input$lvr_filter2 == "Total" | is.null(input$lvr_filter2)) {
            
            out <- out
            
        } else { out <- out %>% filter(out[9] == input$lvr_filter2) }
        

        out <- out %>% select(1:7)
        
        ## sample size calculation
        comps <- length(unique(out$Banner)) - 1
        
        Tgt_SS <- out %>%
            group_by(Serial) %>%
            filter(row_number() == 1) %>%
            nrow(.)
        
        Comp_SS <- Tgt_SS * comps
        
        ## means calculation
        means <- out %>%
            mutate(Group = ifelse(Banner == input$lvr_banner,
                                  "Tgt_Mean", "Comp_Mean")) %>%
            group_by(Group, Attribute) %>%
            summarise(avg = mean(Weight * Rating)) %>%
            spread(Group, avg) %>%
            mutate(Gap = Tgt_Mean - Comp_Mean)
        
        ## stdev calculation
        stdev <- out %>%
            mutate(Group = ifelse(Banner == input$lvr_banner, 
                                  "Tgt_Std", "Comp_Std")) %>%
            group_by(Group, Attribute) %>%
            summarise(std = sqrt(wtd.var(x = Rating, weights = Weight))) %>%
            spread(Group, std)
        
        ## correlation calculation
        corr <- out %>%
            select(-Attribute_Code) %>%
            spread(Attribute, Rating) %>%
            select(-Serial, -c(Banner_Code:Banner)) %$%
            wtd.cors(., weight = Weight) %>%
            as.data.frame() %>%
            rownames_to_column("Attribute") %>%
            gather(Corr_var, Correlation, -Attribute) %>%
            filter(Corr_var == input$lvr_corr) %>%
            select(Attribute, Correlation)
        
        ## combine all
        final <- means %>%
            mutate(Comp_SS = Comp_SS,
                   Tgt_SS  = Tgt_SS) %>%
            left_join(stdev, by = "Attribute") %>%
            left_join(corr, by = "Attribute") %>%
            mutate(
                Tgt_Std2  = Tgt_Std ^ 2,
                Comp_Std2 = Comp_Std ^ 2,
                Pool_Std  = (((Tgt_SS - 1) * Tgt_Std2) + ((Comp_SS - 1) * Comp_Std2)) / (Tgt_SS + Comp_SS - 2),
                Tstat     = Gap / sqrt(Pool_Std * ((1 / Tgt_SS) + (1 / Comp_SS))),
                Sig       = as.factor(ifelse(Gap > 0 & abs(Tstat) > abs(qnorm(0.1)), "Sig higher", 
                                   ifelse(Gap < 0 & abs(Tstat) > abs(qnorm(0.1)), "Sig lower", "Not sig")))
            ) %>%
            select(Attribute, Correlation, Gap, Tgt_Mean, Comp_Mean, Tgt_SS, Comp_SS, Tgt_Std, Comp_Std, Sig) %>%
            set_names(c("Attribute", "Correlation", "Gap", 
                        "Tgt. Mean", "Comp. Mean", 
                        "Tgt. N", "Comp. N",
                        "Tgt. Std", "Comp. Std",
                        "Sig"))
        
        final
        
    }, ignoreNULL = FALSE)
    
    ### generate reactive lever map table
    output$lvr_data_table <- renderDataTable({
        
        if (is.null(input$lvr_attributes)) {return(NULL)}
        
        final <- leverdata() %>%
            filter(Attribute %in% input$lvr_attributes)
        
        # options of the datatable output
        datatable(final, 
                  rownames = FALSE,
                  options = list(
                      order = list(list(2, 'desc')),
                      pageLength = 30,
                      columnDefs = list(
                          list(width = '10%',
                               targets = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
                          list(className = "dt-center", 
                               targets = c(1, 2, 3, 4, 5, 6, 7, 8, 9))))) %>%
            
            formatRound(c(2:5, 8:9), digits = 3) %>%
            formatStyle(c(2:3),
                        color = styleInterval(3.4, c('red', 'white'))) %>%
            formatStyle(c(2:3), fontWeight = styleInterval(5, c('bold', 'weight')))
    })
    
    ### generate reactive lever map TO PRINT
        make_lever <- eventReactive(input$lvr_submit == 0, {
            
            if (is.null(input$lvr_attributes)) {return(NULL)}
            
            point_shape <- (pointList %>% filter(shape == input$lvr_point_select))[[1]]
            
            corr_var <- paste(input$lvr_banner, "mean - Competitive set mean", sep = " ")
            #sig_band <- input$lvr_sig_band
            
            leverdata <- leverdata() %>%
                mutate(Keep = ifelse(Attribute %in% input$lvr_attributes, 1, 0))
            
            basemap <- ggplot(subset(leverdata, Keep == 1),
                              aes(x = Correlation, y = Gap, fill = Sig), 
                              environment = environment()) +
                # geom_rect(aes(ymin = (-1 * sig_band), 
                #               ymax = sig_band,
                #               xmin = -Inf,
                #               xmax = Inf),
                #           fill = input$lvr_nonsig_color, 
                #           alpha = input$lvr_nonsig_alpha) +
                
                geom_vline(aes(xintercept = mean(Correlation)),
                           color    = input$lvr_line_color,
                           size     = input$lvr_line_size,
                           linetype = input$lvr_line_type,
                           alpha    = input$lvr_line_alpha) +
                
                geom_hline(yintercept = 0,
                           color    = input$lvr_line_color,
                           size     = input$lvr_line_size,
                           linetype = input$lvr_line_type,
                           alpha    = input$lvr_line_alpha) +
                # geom_hline(yintercept = input$lvr_sig_band,
                #            color = input$lvr_line_color,
                #            size = input$lvr_line_size,
                #            linetype = input$lvr_line_type,
                #            alpha = input$lvr_line_alpha) +
                # geom_hline(yintercept = (-1 * input$lvr_sig_band),
                #            color = input$lvr_line_color,
                #            size = input$lvr_line_size,
                #            linetype = input$lvr_line_type,
                #            alpha = input$lvr_line_alpha) +
                
                geom_point(shape = point_shape,
                           size  = input$lvr_point_size, 
                           color = "white") +
                
                scale_fill_manual(values = c("Sig lower"  = input$lvr_sig_lower_color,
                                             "Sig higher" = input$lvr_sig_higher_color,
                                             "Not sig"    = input$lvr_not_sig_color),
                                  drop = FALSE) +
                scale_color_manual(values = c("Sig lower"  = input$lvr_sig_lower_color,
                                              "Sig higher" = input$lvr_sig_higher_color,
                                              "Not sig"    = input$lvr_not_sig_color),
                                   drop = FALSE) 
            
            if (input$lvr_label_position == "Best fit") basemap <- basemap + 
                geom_text_repel(aes(label = Attribute,
                                    color = Sig),
                                show.legend   = FALSE,
                                size          = input$lvr_label_size,
                                point.padding = unit(input$lvr_point_padding, 'lines'),
                                box.padding   = unit(input$lvr_box_padding, 'lines'),
                                fontface      = input$lvr_font_face,
                                segment.color = input$lvr_segment_color)
            
            if (input$lvr_label_position == "Corners") basemap <- basemap + 
                geom_text_repel(aes(label = Attribute,
                                    color = Sig),
                                show.legend   = FALSE,
                                size          = input$lvr_label_size,
                                point.padding = unit(input$lvr_point_padding, 'lines'),
                                box.padding   = unit(input$lvr_box_padding, 'lines'),
                                fontface      = input$lvr_font_face,
                                segment.color = input$lvr_segment_color, 
                                nudge_x       = ifelse(leverdata[leverdata$Keep == 1, ]$Correlation < mean(leverdata[leverdata$Keep == 1, ]$Correlation), -0.01, 0.01),
                                nudge_y       = ifelse(leverdata[leverdata$Keep == 1, ]$Gap < 0, -0.01, 0.01))
            
            if (input$lvr_label_position == "Top") basemap <- basemap + 
                geom_text_repel(aes(label = Attribute,
                                    color = Sig),
                                show.legend   = FALSE,
                                size = input$lvr_label_size,
                                point.padding = unit(input$lvr_point_padding, 'lines'),
                                box.padding   = unit(input$lvr_box_padding, 'lines'),
                                fontface      = input$lvr_font_face,
                                segment.color = input$lvr_segment_color, 
                                nudge_y       = 0.01)
            
            if (input$lvr_label_position == "Bottom") basemap <- basemap + 
                geom_text_repel(aes(label = Attribute,
                                    color = Sig),
                                show.legend   = FALSE,
                                size = input$lvr_label_size,
                                point.padding = unit(input$lvr_point_padding, 'lines'),
                                box.padding   = unit(input$lvr_box_padding, 'lines'),
                                fontface      = input$lvr_font_face,
                                segment.color = input$lvr_segment_color, 
                                nudge_y       = -0.01)
            
            if (input$lvr_label_position == "Left") basemap <- basemap + 
                geom_text_repel(aes(label = Attribute,
                                    color = Sig),
                                show.legend   = FALSE,
                                size          = input$lvr_label_size,
                                point.padding = unit(input$lvr_point_padding, 'lines'),
                                box.padding   = unit(input$lvr_box_padding, 'lines'),
                                fontface      = input$lvr_font_face,
                                segment.color = input$lvr_segment_color, 
                                nudge_x       = -0.01)
            
            if (input$lvr_label_position == "Right") basemap <- basemap + 
                geom_text_repel(aes(label = Attribute,
                                    color = Sig),
                                show.legend   = FALSE,
                                size          = input$lvr_label_size,
                                point.padding = unit(input$lvr_point_padding, 'lines'),
                                box.padding   = unit(input$lvr_box_padding, 'lines'),
                                fontface      = input$lvr_font_face,
                                segment.color = input$lvr_segment_color, 
                                nudge_x       = 0.01)
            
            basemap <- basemap + 
                scale_x_continuous(limits = c(input$lvr_xmin, input$lvr_xmax)) +
                scale_y_continuous(limits = c(input$lvr_ymin, input$lvr_ymax)) +
                ggtitle(input$lvr_plot_title) +
                theme(panel.background = element_rect(fill = input$lvr_background_color),
                      plot.background  = element_rect(fill = input$lvr_background_color),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.position  = "bottom",
                      legend.text      = element_text(size = 10),
                      legend.title     = element_blank(),
                      plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"))

            # axis labels
            if (input$lvr_axis_Labels) basemap <- basemap + labs(x = paste0("Correlation with ", input$lvr_corr),
                                                                 y = HTML(paste("Performance\n", corr_var)))

            if (input$lvr_axis_Labels == F) basemap <- basemap + labs(x = NULL, y = NULL)
            
            # axis text and ticks
            if (input$lvr_axis_Text  == F) basemap <- basemap + theme(axis.text  = element_blank())
            if (input$lvr_axis_Ticks == F) basemap <- basemap + theme(axis.ticks = element_blank())
            
            basemap

    }, ignoreNULL = FALSE)
    
    
    ### print out lever map
    output$lever_map <- renderPlot({
        print(make_lever())
    }, height = 640 )
    
    
    ### generate reactive lever map TO SAVE
    save_lever <- eventReactive(input$lvr_submit == 0, {
        
        if (is.null(input$lvr_attributes)) {return(NULL)}
        
        # scalars
        scale4 <- 0.4
        scale6 <- 0.6
        scale8 <- 0.8
        
        point_shape <- (pointList %>% filter(shape == input$lvr_point_select))[[1]]
        
        corr_var <- paste(input$lvr_banner, "mean - Competitive set mean", sep = " ")
        
        #sig_band <- input$lvr_sig_band
        
        leverdata <- leverdata() %>%
            mutate(Keep = ifelse(Attribute %in% input$lvr_attributes, 1, 0))
        
        basemap <- ggplot(subset(leverdata, Keep == 1),
                          aes(x = Correlation, y = Gap, fill = Sig), 
                          environment = environment()) +
            # geom_rect(aes(ymin = (-1 * sig_band), 
            #               ymax = sig_band,
            #               xmin = -Inf,
            #               xmax = Inf),
            #           fill = input$lvr_nonsig_color, 
            #           alpha = input$lvr_nonsig_alpha) +
            geom_vline(aes(xintercept = mean(Correlation)),
                       color    = input$lvr_line_color,
                       size     = input$lvr_line_size * scale8,
                       linetype = input$lvr_line_type,
                       alpha    = input$lvr_line_alpha) +
            geom_hline(yintercept = 0,
                       color    = input$lvr_line_color,
                       size     = input$lvr_line_size * scale8,
                       linetype = input$lvr_line_type,
                       alpha    = input$lvr_line_alpha) +
            # geom_hline(yintercept = input$lvr_sig_band,
            #            color = input$lvr_line_color,
            #            size = input$lvr_line_size,
            #            linetype = input$lvr_line_type,
            #            alpha = input$lvr_line_alpha) +
            # geom_hline(yintercept = (-1 * input$lvr_sig_band),
            #            color = input$lvr_line_color,
            #            size = input$lvr_line_size,
            #            linetype = input$lvr_line_type,
            #            alpha = input$lvr_line_alpha) +
            geom_point(shape = point_shape,
                       size  = input$lvr_point_size * scale6, 
                       color = "white") +
            scale_fill_manual(values = c("Sig lower"  = input$lvr_sig_lower_color,
                                         "Sig higher" = input$lvr_sig_higher_color,
                                         "Not sig"    = input$lvr_not_sig_color),
                              drop = FALSE) +
            scale_color_manual(values = c("Sig lower"  = input$lvr_sig_lower_color,
                                          "Sig higher" = input$lvr_sig_higher_color,
                                          "Not sig"    = input$lvr_not_sig_color),
                               drop = FALSE) 
        
        if (input$lvr_label_position == "Best fit") basemap <- basemap + 
            geom_text_repel(aes(label = Attribute,
                                color = Sig),
                            show.legend   = FALSE,
                            size          = input$lvr_label_size * scale6,
                            point.padding = unit(input$lvr_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$lvr_box_padding * scale6, 'lines'),
                            fontface      = input$lvr_font_face,
                            segment.color = input$lvr_segment_color)
        
        if (input$lvr_label_position == "Corners") basemap <- basemap + 
            geom_text_repel(aes(label = Attribute,
                                color = Sig),
                            show.legend   = FALSE,
                            size          = input$lvr_label_size * scale6,
                            point.padding = unit(input$lvr_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$lvr_box_padding * scale6, 'lines'),
                            fontface      = input$lvr_font_face,
                            segment.color = input$lvr_segment_color, 
                            nudge_x       = ifelse(leverdata[leverdata$Keep == 1, ]$Correlation < mean(leverdata[leverdata$Keep == 1, ]$Correlation), -0.01, 0.01),
                            nudge_y       = ifelse(leverdata[leverdata$Keep == 1, ]$Gap < 0, -0.01, 0.01))
        
        if (input$lvr_label_position == "Top") basemap <- basemap + 
            geom_text_repel(aes(label = Attribute,
                                color = Sig),
                            show.legend   = FALSE,
                            size          = input$lvr_label_size * scale6,
                            point.padding = unit(input$lvr_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$lvr_box_padding * scale6, 'lines'),
                            fontface      = input$lvr_font_face,
                            segment.color = input$lvr_segment_color, 
                            nudge_y       = 0.01)
        
        if (input$lvr_label_position == "Bottom") basemap <- basemap + 
            geom_text_repel(aes(label = Attribute,
                                color = Sig),
                            show.legend   = FALSE,
                            size          = input$lvr_label_size * scale6,
                            point.padding = unit(input$lvr_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$lvr_box_padding * scale6, 'lines'),
                            fontface      = input$lvr_font_face,
                            segment.color = input$lvr_segment_color, 
                            nudge_y       = -0.01)
        
        if (input$lvr_label_position == "Left") basemap <- basemap + 
            geom_text_repel(aes(label = Attribute,
                                color = Sig),
                            show.legend   = FALSE,
                            size          = input$lvr_label_size * scale6,
                            point.padding = unit(input$lvr_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$lvr_box_padding * scale6, 'lines'),
                            fontface      = input$lvr_font_face,
                            segment.color = input$lvr_segment_color, 
                            nudge_x       = -0.01)
        
        if (input$lvr_label_position == "Right") basemap <- basemap + 
            geom_text_repel(aes(label = Attribute,
                                color = Sig),
                            show.legend   = FALSE,
                            size          = input$lvr_label_size * scale6,
                            point.padding = unit(input$lvr_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$lvr_box_padding * scale6, 'lines'),
                            fontface      = input$lvr_font_face,
                            segment.color = input$lvr_segment_color, 
                            nudge_x       = 0.01)
        
        basemap <- basemap + 
            scale_x_continuous(limits = c(input$lvr_xmin, input$lvr_xmax)) +
            scale_y_continuous(limits = c(input$lvr_ymin, input$lvr_ymax)) +
            ggtitle(input$lvr_plot_title) +
            theme(panel.background = element_rect(fill = input$lvr_background_color),
                  plot.background  = element_rect(fill = input$lvr_background_color),
                  axis.title       = element_text(size = 8),
                  axis.text        = element_text(size = 6),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position  = "bottom",
                  legend.text      = element_text(size = 6),
                  legend.title     = element_blank(),
                  plot.title       = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        # axis labels
        if (input$lvr_axis_Labels) basemap <- basemap + labs(x = paste0("Correlation with ", input$lvr_corr),
                                                             y = HTML(paste("Performance\n", corr_var)))
        
        if (input$lvr_axis_Labels == F) basemap <- basemap + labs(x = NULL, y = NULL)
        
        # axis text and ticks
        if (input$lvr_axis_Text  == F) basemap <- basemap + theme(axis.text  = element_blank())
        if (input$lvr_axis_Ticks == F) basemap <- basemap + theme(axis.ticks = element_blank())
        
        basemap
        
    }, ignoreNULL = FALSE)
    
    ### download lever plot object
    output$lvr_download_map <- downloadHandler(
        filename = function() {paste(input$lvr_map_title, '.png', sep = "")},
        content = function(file) {
            png(file,
                width = 8.5,
                height = 6,
                units = 'in',
                res = 1200)
            print(save_lever())
            dev.off()
        }
    )
    
    ### download lever plot data
    output$lvr_download_data <- downloadHandler(
        filename = function() {paste(input$lvr_table_title, '.csv', sep='')},
        content = function(file) {
            write_csv(leverdata(), file)
        }
    )
    
    #### TAB 4: PERCEPTUAL MAP --------------------------------------
    
    ### choose filter
    
    ### FILTER #1
    output$pm_filter1_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        data <- lever_percep_corr()
        
        if (nrow(unique(data[8])) <= 1) {
            
            filter_name <- names(data[8])
            
            # if there is only one option...
            selectInput(inputId = 'pm_filter1',
                        label = filter_name,
                        choices = "Total",
                        selected = "Total")    
            
        } else {
            
            # if the filter use useful...
            filter_name <- paste(names(data[8]), "(Filter 1)", sep = " ")
            
            filter_choices <- unique(data[8])
            
            selectInput(inputId = 'pm_filter1',
                        label = filter_name,
                        choices = c("Total", filter_choices),
                        selected = "Total")
            
        }
        
    }) 
    
    ### FILTER #2
    output$pm_filter2_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        data <- lever_percep_corr()
        
        if (nrow(unique(data[9])) <= 1) {
            
            filter_name <- names(data[9])
            
            # if there is only one option...
            selectInput(inputId = 'pm_filter2',
                        label = filter_name,
                        choices = "Total",
                        selected = "Total")    
            
        } else {
            
            # if the filter use useful...
            filter_name <- paste(names(data[9]), "(Filter 2)", sep = " ")
            
            filter_choices <- unique(data[9])
            
            selectInput(inputId = 'pm_filter2',
                        label = filter_name,
                        choices = c("Total", filter_choices),
                        selected = "Total")
            
        }
        
    }) 
    
    ### FILTER #3
    output$pm_filter3_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        data <- lever_percep_corr()
        
        if (nrow(unique(data[10])) <= 1) {
            
            filter_name <- names(data[10])
            
            # if there is only one option...
            selectInput(inputId = 'pm_filter3',
                        label = filter_name,
                        choices = "Total",
                        selected = "Total")    
            
        } else {
            
            # if the filter use useful...
            filter_name <- paste(names(data[10]), "(Filter 3)", sep = " ")
            
            filter_choices <- unique(data[10])
            
            selectInput(inputId = 'pm_filter3',
                        label = filter_name,
                        choices = c("Total", filter_choices),
                        selected = "Total")
            
        }
        
    }) 
    
    ### uncheck attribute button
    output$pm_uncheck_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        actionButton('pm_uncheck', "Uncheck attributes", icon = icon('check-square'))
    })
    
    ### attribute choices for map
    output$pm_attributes_UI <- renderUI({
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        att_choices <- lever_percep_corr() %$% unique(Attribute)
        
        controls <- tags$div(align = 'left', 
                             class = 'multicol', 
                             checkboxGroupInput(inputId  = 'pm_attributes', 
                                                label    = "Attributes to include in map:", 
                                                choices  = att_choices,
                                                inline   = FALSE))
        observeEvent(input$pm_uncheck, {
            
            updateCheckboxGroupInput(session, "pm_attributes", 
                                     choices = att_choices, 
                                     selected = if (input$pm_uncheck) NULL)
        })
        
        controls
        
    })
    
    ### display number of attributes selected
    output$pm_atts_selected <- renderText({
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        num <- as.numeric(length(input$pm_attributes))
        out <- paste("# Attributes selected: ", num)
        out
        
    })
    
    
    ### display sample sizes
    pm_sample_size <- eventReactive(input$pm_submit == 0, {
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        num <- as.numeric(perceptualdata()[1, 5])  # 5th col is Target Sample Size
        out <- paste("Sample size: ", num)
        out
        
    }, ignoreInit = TRUE)
    
    output$pm_sample_size_UI <- renderUI({
        out <- pm_sample_size()
        print(out)
    })
    
    ### calculate correspondence analysis
    perceptualdata <- eventReactive(input$pm_submit == 0, {
        
        if (is.null(input$lever_percep_corr_file)) return(NULL)
        
        data <- lever_percep_corr()
        
        if (input$pm_filter1 == "Total") {
            
            out <- data
            
        } else { out <- data %>% filter(data[8] == input$pm_filter1) }
        
        if (input$pm_filter2 == "Total") {
            
            out <- out
            
        } else { out <- out %>% filter(data[9] == input$pm_filter2) }
        
        if (input$pm_filter3 == "Total") {
            
            out <- out
            
        } else { out <- out %>% filter(data[10] == input$pm_filter3) }
        
        ## sample size calculation
        SS <- out %>%
            group_by(Serial) %>%
            filter(row_number() == 1) %>%
            nrow(.)
        
        ca.input <- out %>%
            filter(Attribute %in% input$pm_attributes) %>%
            select(Weight, Banner, Attribute, Rating) %>%
            group_by(Banner, Attribute) %>%
            summarise(Avg = weighted.mean(Rating, Weight)) %>%
            spread(Banner, Avg) %>%
            as.data.frame() %>%
            remove_rownames() %>%
            column_to_rownames("Attribute")
        
        ca.output <- FactoMineR::CA(X = ca.input, graph = FALSE)
        
        coords <- ca.output$row$coord %>%
            as.data.frame() %>%
            rownames_to_column("Variable") %>%
            as_tibble() %>%
            select(1:3) %>%
            setNames(c("Variable", "Dim1", "Dim2")) %>%
            mutate(Type = "Attribute") %>%
            rbind(
                ca.output$col$coord %>%
                    as.data.frame() %>%
                    rownames_to_column("Variable") %>%
                    as_tibble() %>%
                    select(1:3) %>%
                    setNames(c("Variable", "Dim1", "Dim2")) %>%
                    mutate(Type = "Banner")) %>%
            mutate(`Sample Size` = SS)
        
        coords 
        
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    ### generate reactive perceptual map table
    output$pm_data_table <- renderDataTable({
        
        if (is.null(perceptualdata())) {return(NULL)}
        
        final <- perceptualdata() %>%
            # drop sample size variable
            select(-5)
        
        # # options of the datatable output
        # datatable(final, rownames = FALSE)
        
        datatable(final, 
                  rownames = FALSE,
                  options = list(
                      pageLength = 15,
                      columnDefs = list(
                          list(width = '25%',
                               targets = c(0, 1, 2, 3)),
                          list(className = "dt-center",
                               targets = c(1, 2))))) %>%
            formatRound(c(2:3), digits = 3)

        
    })
    
    ### generate reactive perceptual map plot TO PRINT
    make_perceptual <- eventReactive(input$pm_submit == 0, {
        
        if (is.null(input$pm_attributes)) {return(NULL)}
        
        perceptual_data <- perceptualdata()
        
        point_shape <- (pointList %>% filter(shape == input$pm_point_select))[[1]]
                    
        
        percep_min <- min(c(perceptual_data$Dim1, perceptual_data$Dim2))
        percep_max <- max(c(perceptual_data$Dim1, perceptual_data$Dim2))
        
        basemap <- perceptualdata() %>%
            ggplot(aes(Dim1, Dim2, fill = Type)) +
            geom_hline(aes(yintercept = 0),
                       color    = input$pm_line_color,
                       size     = input$pm_line_size,
                       linetype = input$pm_line_type,
                       alpha    = input$pm_line_alpha) +
        
            geom_vline(aes(xintercept = 0),
                       color    = input$pm_line_color,
                       size     = input$pm_line_size,
                       linetype = input$pm_line_type,
                       alpha    = input$pm_line_alpha) +
            
            geom_point(shape = point_shape,
                       size  = input$pm_point_size,
                       color = "white") +
            
            xlim(c(percep_min, percep_max)) +
            ylim(c(percep_min, percep_max)) +
            
            guides(color = FALSE) +
            scale_color_manual(values = c("Attribute" = input$pm_attribute_color, 
                                          "Banner"    = input$pm_banner_color)) +
            
            scale_fill_manual(values = c("Attribute" = input$pm_attribute_color, 
                                          "Banner"   = input$pm_banner_color))

        
        if (input$pm_label_position == "Best fit") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size,
                            point.padding = unit(input$pm_point_padding, 'lines'),
                            box.padding   = unit(input$pm_box_padding, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color)
        
        if (input$pm_label_position == "Corners") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size,
                            point.padding = unit(input$pm_point_padding, 'lines'),
                            box.padding   = unit(input$pm_box_padding, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color, 
                            nudge_x       = ifelse(perceptual_data$Dim1 < 0, -0.001, 0.001),
                            nudge_y       = ifelse(perceptual_data$Dim2 < 0, -0.001, 0.001))
        
        if (input$pm_label_position == "Top") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size,
                            point.padding = unit(input$pm_point_padding, 'lines'),
                            box.padding   = unit(input$pm_box_padding, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color, 
                            nudge_y       = 0.001)
        
        if (input$pm_label_position == "Bottom") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size,
                            point.padding = unit(input$pm_point_padding, 'lines'),
                            box.padding   = unit(input$pm_box_padding, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color, 
                            nudge_y       = -0.001)
        
        if (input$pm_label_position == "Left") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size,
                            point.padding = unit(input$pm_point_padding, 'lines'),
                            box.padding   = unit(input$pm_box_padding, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color, 
                            nudge_x       = -0.001)
        
        if (input$pm_label_position == "Right") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size,
                            point.padding = unit(input$pm_point_padding, 'lines'),
                            box.padding   = unit(input$pm_box_padding, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color, 
                            nudge_x       = 0.001)
        
        basemap <- basemap + 
            ggtitle(input$pm_plot_title) +
            theme(panel.background = element_rect(fill = input$pm_background_color),
                  plot.background  = element_rect(fill = input$pm_background_color),
                  axis.title       = element_blank(),
                  axis.text        = element_blank(),
                  axis.ticks       = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position  = "none",
                  plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"))
        
        
        basemap
        
    }, ignoreNULL = FALSE)
    
    ### generate reactive perceptual map plot TO SAVE
    save_perceptual <- eventReactive(input$pm_submit == 0, {
        
        if (is.null(input$pm_attributes)) {return(NULL)}
        
        scale4 <- 0.4
        scale6 <- 0.6
        scale8 <- 0.8

        perceptual_data <- perceptualdata()
        
        point_shape <- (pointList %>% filter(shape == input$pm_point_select))[[1]]
                    
        
        percep_min <- min(c(perceptual_data$Dim1, perceptual_data$Dim2))
        percep_max <- max(c(perceptual_data$Dim1, perceptual_data$Dim2))
        
        basemap <- perceptualdata() %>%
            ggplot(aes(Dim1, Dim2, fill = Type)) +
            geom_hline(aes(yintercept = 0),
                       color    = input$pm_line_color,
                       size     = input$pm_line_size * scale8,
                       linetype = input$pm_line_type,
                       alpha    = input$pm_line_alpha) +
        
            geom_vline(aes(xintercept = 0),
                       color    = input$pm_line_color,
                       size     = input$pm_line_size * scale8,
                       linetype = input$pm_line_type,
                       alpha    = input$pm_line_alpha) +
            
            geom_point(shape = point_shape,
                       size  = input$pm_point_size * scale6,
                       color = "white") +
            
            xlim(c(percep_min, percep_max)) +
            ylim(c(percep_min, percep_max)) +
            
            guides(color = FALSE) +
            scale_color_manual(values = c("Attribute" = input$pm_attribute_color, 
                                          "Banner"    = input$pm_banner_color)) +
            
            scale_fill_manual(values = c("Attribute" = input$pm_attribute_color, 
                                          "Banner"   = input$pm_banner_color))

        
        if (input$pm_label_position == "Best fit") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size * scale6,
                            point.padding = unit(input$pm_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$pm_box_padding * scale6, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color)
        
        if (input$pm_label_position == "Corners") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size * scale6,
                            point.padding = unit(input$pm_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$pm_box_padding * scale6, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color, 
                            nudge_x       = ifelse(perceptual_data$Dim1 < 0, -0.001, 0.001),
                            nudge_y       = ifelse(perceptual_data$Dim2 < 0, -0.001, 0.001))
        
        if (input$pm_label_position == "Top") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size * scale6,
                            point.padding = unit(input$pm_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$pm_box_padding * scale6, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color, 
                            nudge_y       = 0.001)
        
        if (input$pm_label_position == "Bottom") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size * scale6,
                            point.padding = unit(input$pm_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$pm_box_padding * scale6, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color, 
                            nudge_y       = -0.001)
        
        if (input$pm_label_position == "Left") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size * scale6,
                            point.padding = unit(input$pm_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$pm_box_padding * scale6, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color, 
                            nudge_x       = -0.001)
        
        if (input$pm_label_position == "Right") basemap <- basemap + 
            geom_text_repel(aes(label = Variable,
                                color = Type),
                            show.legend   = FALSE,
                            size          = input$pm_label_size * scale6,
                            point.padding = unit(input$pm_point_padding * scale6, 'lines'),
                            box.padding   = unit(input$pm_box_padding * scale6, 'lines'),
                            fontface      = input$pm_font_face,
                            segment.color = input$pm_segment_color, 
                            nudge_x       = 0.001)
        
        basemap <- basemap + 
            ggtitle(input$pm_plot_title) +
            theme(panel.background = element_rect(fill = input$pm_background_color),
                  plot.background  = element_rect(fill = input$pm_background_color),
                  axis.title       = element_blank(),
                  axis.text        = element_blank(),
                  axis.ticks       = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position  = "none",
                  plot.title       = element_text(hjust = 0.5, size = 12, face = "bold"))
        
        
        basemap
        
    }, ignoreNULL = FALSE)
    
    ### print out perceptual map
    output$perceptual_map <- renderPlot({
        print(make_perceptual())
    }, height = 640 )
    
    ### download perceptual map object
    output$pm_download_map <- downloadHandler(
        filename = function() {paste(input$pm_map_title, '.png', sep = "")},
        content = function(file) {
            png(file,
                width = 8.5,
                height = 6,
                units = 'in',
                res = 1200)
            print(save_perceptual())
            dev.off()
        }
    )
    
    ### download perceptual map data
    output$pm_download_data <- downloadHandler(
        filename = function() {paste(input$pm_table_title, '.csv', sep='')},
        content = function(file) {
            write_csv(perceptualdata(), file)
        }
    )
    
    
}





