############## UHR-IonStar 1.3 ################


##0## Please click 'Run App' at topright to start this web app ####

#### Update log is included in the manual ####



### Define server logic ----
server <- function(input, output, session) {
  #### IonStarStat ####
  ## Frame Generation##
  
  output$noticeStat3 <- renderText({
    input$goButtonStat3
    Database_dir <<- isolate(input$fileStat4)
    if (is.null(Database_dir))
      return(NULL)
    
    Sprepo_dir <<- isolate(input$fileStat5)
    if (is.null(Sprepo_dir))
      return(NULL)
    
    print('Submittion completed.')
  })
  

  dbStat <- reactive({dbConnect(dbDriver("SQLite"),dbname = Database_dir$datapath)})
  spStat <- reactive({read.csv(Sprepo_dir$datapath)})
  
  observeEvent(input$goButtonStat4,{
    withProgress(message = 'Frame Generation in Progress...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
        Sys.sleep(0.3)
      }
    })
  })
  
  testStat2 <- eventReactive(input$goButtonStat4,FrameGen(dbStat(),spStat(),input$filenumber,input$scannumber,
                                                          input$proaccession, input$pepseq))
  output$noticeStat5 <- renderText({
    testStat2()$notice
  })
  output$message1 <- renderText({
    testStat2()$message1
  })
  output$message2 <- renderText({
    testStat2()$message2
  })
  
  output$downloadDataStat2 <- downloadHandler(
    filename = function() {
      paste('Annotated_Frames-', gsub(" ","-",date()), '.csv', sep='')
    },
    content = function(con) {
      write.csv(testStat2()$framelist, row.names = FALSE, con)
    }
  )
  output$downloadDataStat3 <- downloadHandler(
    filename = function() {
      paste('Sample_ID-', gsub(" ","-",date()), '.csv', sep='')
    },
    content = function(con) {
      write.csv(testStat2()$sampleid, con)
    }
  )
  
  ## Protein Quantification ##
  
  output$noticeStat6 <- renderText({
    input$goButtonStat5
    FrameList_dir <<- isolate(input$fileStat6)
    if (is.null(FrameList_dir))
      return(NULL)
    
    SampleID_dir <<- isolate(input$fileStat7)
    if (is.null(SampleID_dir))
      return(NULL)
    
    print('Submittion completed.')
  })
  
  framelist <- reactive({read.csv(FrameList_dir$datapath, header = TRUE)})
  sampleid <- reactive({read.csv(SampleID_dir$datapath, header = TRUE, row.names = 1)})
  
  observeEvent(input$goButtonStat6,{
    withProgress(message = 'Protein Quantification in Progress...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
        Sys.sleep(0.3)
      }
    })
  })
  
  testStat3 <- eventReactive(input$goButtonStat6,ProQuant(framelist(),sampleid(),rm = input$Share_rm))
  output$noticeStat8 <- renderText({
    testStat3()$notice
  })

  
  output$downloadDataStat4 <- downloadHandler(
    filename = function() {
      paste('Quan_data-', gsub(" ","-",date()), '.csv', sep='')
    },
    content = function(con) {
      write.csv(testStat3()$quan, con)
    }
  )
  output$downloadDataStat5 <- downloadHandler(
    filename = function() {
      paste('Pep_data-', gsub(" ","-",date()), '.csv', sep='')
    },
    content = function(con) {
      write.csv(testStat3()$pep, con)
    }
  )
  
  
  

  #### Processing ####
  { 
    
    output$downloadManual <- downloadHandler(
      filename <- function(){
        paste("Manual","html",sep = ".")
      },
      
      content <- function(file){
        file.copy("Manual.html",file)
      },
      contentType = "application/html"
    )
    
    
    
    ## input quan_file and group_file
    output$contents <- DT::renderDataTable({
      input$goButton
      quan_file_dir <<- isolate(input$file1)
      if (is.null(quan_file_dir))
        return(NULL)
      
      group_file_dir <<- isolate(input$file2)
      if (is.null(group_file_dir))
        return(NULL)
      
      tmp<-read.csv(group_file_dir$datapath, header = TRUE,row.names = 1)
      tmp2 <- as.matrix(unique(as.character(tmp$GroupID)))
      colnames(tmp2)<- "Groups"
      tmp2
    })
    
    
    quan_file <- reactive({read.csv(quan_file_dir$datapath, header = TRUE)})
    group_file <- reactive({read.csv(group_file_dir$datapath, header = TRUE)})
    
    #   observe({
    # Can also set the label and select items
    #     tmp3 <- reactive({unique(as.character(group_file()$GroupID))})
    #     if(is.null(tmp3())){
    #       return(NULL)
    #     }
    #     updateSelectInput(session, "ctrlGroup",
    #                       label = "Assign control group",
    #                      choices = tmp3(),
    #                       selected = head(tmp3(), 1)
    #    )
    #  })
    
    observeEvent(input$calculate,{
      withProgress(message = 'Calculation in progress...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.3)
                     }
                   })
      decoy_id <<- as.character(input$decoy)
      
    })
    #IonStar_Ratio_p.R

      test <- eventReactive(input$calculate,
                            Ratio_p(quan_file(),group_file(), 
                                    decoy = decoy_id, control_group = input$ctrlGroup, 
                                    is_t_test = input$testing, pro_or_pep = input$qpat))

    output$notice1 <- renderText({
      test()$notice
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('quan_results-', gsub(" ","-",date()), '.csv', sep='')
      },
      content = function(con) {
        write.csv(test()$quan_results, con)
      }
    )
    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste('ave_ratios-', gsub(" ","-",date()), '.csv', sep='')
      },
      content = function(con) {
        write.csv(test()$ave_ratio, con)
      }
    )
    output$downloadData_all <- downloadHandler(
      filename = function() {
        paste('quan&ratios-', gsub(" ","-",date()), '.csv', sep='')
      },
      content = function(con) {
        write.csv(test()$all, con)
      }
    )
    
  }
  #### Visualization ####
  #actionButton("plot",label="Plot",icon=icon("paint-brush"),width="100%"),
  #actionButton("plotexport",label="Export plot",icon=icon("download"),width="100%")
  
  # plot for all
  test2 <- eventReactive(input$plot, plot_for_all(plot_selection=input$plotType, 
                                                  Group1 = input$inter_group1, Group2= input$inter_group2, corr = input$corr))
  output$Plotting <- renderPlot({
    test2()
  })
  #   observeEvent(input$plot,{
  #     output$Plotting <- renderPlot({
  
  #       final_plot <- plot_for_all(plot_selection=input$plotType)
  #       final_plot 
  
  #       })
  # })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste('Plot-', gsub(" ","-",date()), '.png', sep='')
    },
    content = function(con4) {
      ggsave(type="cairo",dpi=320,width=input$sliderwid,
             height=input$sliderhei,con4)
    }
  )
  
  #update seletions
  observe({
    input$calculate
    x <- test()$group_char[test()$group_char != test()$control_group]
    
    if (is.null(x))
      x <- character(0)
    
    updateCheckboxGroupInput(session, "ratio_plot_selection",
                             label = "Choose group/groups you want to show in ratio distribution plot:",
                             choices = x,
                             selected = x[1]
    )
    updateSelectInput(session, inputId = "select_group",
                      label = "Select group",
                      choices = x)
  })
  
  # ratio distribution (density) plot
  test3 <- eventReactive(input$plot2, plot_ratio_distribution(
    selected = input$ratio_plot_selection, dist_corr = input$dist_correction))
  output$Plotting2 <- renderPlot({
    test3()$plotting
  })
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste('ratio_plot-', gsub(" ","-",date()), '.png', sep='')
    },
    content = function(con2) {
      ggsave(type="cairo",dpi=320,width=input$sliderwid2,height=input$sliderhei2,con2)
    }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste('corrected_ratios-', gsub(" ","-",date()), '.csv', sep='')
    },
    content = function(con) {
      write.csv(test3()$correction_ratio, con)
    }
  )
  
  #########Discovery of Changes#############
  # finding significantly changed proteins
  test4 <- eventReactive(input$upload_cutoff, sign_changes(input$select_group,
                                                            input$R_cutoff, input$p_cutoff))
  output$sign_contents <- DT::renderDataTable({
    input$upload_cutoff
    test4()$quan_changed
  })
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste('sign-changed', gsub(" ","-",date()), '.csv', sep='')
    },
    content = function(con3) {
      write.csv(test4()$quan_changed, con3)
    }
  )
  # plot for all 2
  test5 <- eventReactive(input$plot3,plot_for_all2(plot_selection = input$plotType2, 
                                                   TOPx = input$TOPx))
  output$Plotting3 <- renderPlot({
    test5()
  })
  output$downloadPlot3 <- downloadHandler(
    filename = function() {
      paste('vol_plot-', gsub(" ","-",date()), '.png', sep='')
    },
    content = function(con3) {
      ggsave(type="cairo",dpi=320,width=input$sliderwid3,height=input$sliderhei3,con3)
    }
  )
  
  ###############Re-Verification######################
  output$veri_notice1 <- renderText({
    input$veri_submit
    Protein_dir <<- isolate(input$veri_file1)
    if (is.null(Protein_dir))
      return(NULL)
    
    Peptide_dir <<- isolate(input$veri_file2)
    if (is.null(Peptide_dir))
      return(NULL)
    
    print('Submission completed.')
  })
  
  veri_protein <- reactive({read.csv(Protein_dir$datapath, header = TRUE, row.names = 1)})
  veri_peptide <- reactive({read.csv(Peptide_dir$datapath, header = TRUE, row.names = 1)})
  
  #ProteinFilter.R
  observeEvent(input$veri_inge,{
    withProgress(message = 'Integration in progress...', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
        Sys.sleep(0.3)
      }
    })
  })
  
  veri_test1 <- eventReactive(input$veri_inge,ProPepCombine(veri_protein(),veri_peptide()))
  output$veri_notice2 <- renderText({
    veri_test1()$notice
  })
  
  output$veri_downloadData1 <- downloadHandler(
    filename = function() {
      paste('Inte_data-', gsub(" ","-",date()), '.csv', sep='')
    },
    content = function(con) {
      write.csv(veri_test1()$df, con)
    }
  )
  
  veri_test2 <- eventReactive(input$veri_visual,CustomSearchPlot(veri_test1()$df, 
                                                   input$veri_accession))
  output$Veri_plot <- renderPlot({
    veri_test2()$plot2
  })
  
  output$veri_downloadPlot1 <- downloadHandler(
    filename = function() {
      paste('Accession_plot-', gsub(" ","-",date()), '.png', sep='')
    },
    content = function(con3) {
      ggsave(plot = veri_test2()$plot1,type="cairo",dpi=320,width=input$veri_sliderwid1,height=input$veri_sliderhei1,con3)
    }
  )
  
  
  
  
  }