server = function(input, output, session) { 
  library(leaflet)
  library(bnspatial)
  library(igraph)
  library(DT)
    
  observe({ # observeEvent(input$loadButton,{ 
    if(input$selBN == 'conwy') selbn = "extdata/LandUseChange.net"
    raw = system.file(selbn, package = "bnspatial")
    bn <<- loadNetwork(raw)
    output$bnet = renderPlot({
      g = igraph.from.graphNEL(bn$dag, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
      plot(g, vertex.label.dist=2)
    })
    updateSelectInput(session, 'selCPT', choices = bn$universe$nodes)
    updateSelectInput(session, 'evidenceNode', choices = bn$universe$nodes)
  })
  
  observe({ # observeEvent(input$evidenceNode, {
    eviNode = input$evidenceNode
    updateRadioButtons(session, 'evidenceClasses', choices = bn$universe$levels[[eviNode]])
  })
  
  observe({ # observeEvent(input$selCPT, {
    node = input$selCPT
    # updateSelectInput(session, 'editBN', choices = bn$universe$nodes)
    # updateSelectInput(session, 'selTarget', choices = bn$universe$nodes)
    if(node != ''){
      df = as.data.frame.table(bn$cptlist[[node]])
      names(df)[ncol(df)] = 'Condit. prob.'
      # df = DT::datatable(df) %>% formatStyle(df$Freq,
      #        background = styleColorBar(range(df$Freq), 'lightblue'),
      #        backgroundSize = '98% 88%',
      #        backgroundRepeat = 'no-repeat',
      #        backgroundPosition = 'center')
      output$cpt = DT::renderDataTable({ df }, 
        rownames = FALSE, # editable=TRUE, 
        options = list(dom = 't',ordering = TRUE, iDisplayLength=nrow(df), bFilter=0)
      )
    }
  })
  
  evidLog = reactiveVal('') 
  observeEvent(input$evidSet, {
    s = evidLog()
    if(grepl(input$evidenceNode, s )){
      ss = unlist(strsplit(s, ' - '))
      ss[grepl(input$evidenceNode,ss)] = paste0(input$evidenceNode,':', input$evidenceClasses)
      s = paste0(ss, collapse = ' - ')
    } else {
      s = paste0(s, ' - ' ,input$evidenceNode,':', input$evidenceClasses)
    }
    evidLog(s)
  })
  observeEvent(input$evidRm, {
    s = evidLog()
    ss = unlist(strsplit(s, ' - '))
    ss = ss[!grepl(input$evidenceNode,ss)]
    s = paste0(ss, collapse = ' - ')
    evidLog(s)
  })
  observeEvent(input$evidReset, { evidLog('') })
  
  output$evidLog = renderText({ evidLog() })
  
  observe({
    output$marginProb = renderPlot({
      s = evidLog()
      if(nchar(s)>0){
          ss = unlist(strsplit(s, ' - '))[-1]
          sss = strsplit(ss, ':')
          lst = lapply(sss, '[[', 2)
          names(lst) = sapply(sss, '[[', 1)
      } else {
        lst = NULL
      }
      toPlot = querygrain(bn, input$selCPT, type = "marginal", evidence = lst)
      barplot(toPlot[[1]], main= paste(names(toPlot),'probability'))
    })
  })
  
  
  #   observeEvent(input$upClass, {
    #   #classes = importClasses(input$upClass$path)
    #   raw = system.file("extdata/LUclasses.txt", package = "bnspatial")
    #   classes <<- importClasses(raw)
    #   })
    # 
    #   output$downClass = downloadHandler( 'Spatial2BN.txt', content = function(file){
    #   if(exists('classes')){
    #   #classes = importClasses(input$upClass$path)
    #   l1 = lapply(classes, '[[', 1)
    #   l2 = lapply(classes, '[[', 2)
    #   writeLines(paste(names(classes),
    #    lapply(l1, function(x) paste(x, collapse=',')),
    #    lapply(l2, function(x) paste(x, collapse=',')), 
    #     collapse = "\n", sep='\n'), file)
    #   }
    #   })
    #   
    #   output$nothing = renderText(input$selOutput)
    #   
    #   inserted=c()
    #   observeEvent(input$addClass, {
    #   btn = input$addClass
    #   id1 <- paste0('statusName', btn)
    #   id2 <- paste0('probStatus', btn)
    #   
    #   insertUI( selector = '#placeholder',
    #   ui = tagList(fluidRow(column(6,
    #      textInput(id1,"Status:")),
    #     column(3,textInput(id2,"Probability:")))
    #      )
    #   )
    #   inserted <<- c(id1, inserted)
    #   })
    #   
    #   
    #  output$logPrint <- renderPrint({
    #   'log'  })
    # 
    #  
    # 
    #   # btn = nrow(input$upload)
    #   # id = paste0('rst', 1:btn)
    #   # 
    #   # insertUI( selector = '#rasters',
    #   #   ui = tagList(
    #   #   fluidRow(column(6,
    #   #      textInput(id,"Status:")),
    #   #     column(3,textInput(id2,"Probability:")))
    #   #   )
    #   # )
    # 
    #  # observe({
    #  output$candidates <- renderUI(
    #  tagList(
    #    lapply(1:nrow(input$upload), function(idx){
    #    # output[[paste0("rst",idx)]] <- 
    #   fluidRow( 
    #    column(3, renderText( input$upload[idx, 'name'])
    #    ),
    #    column(3, selectInput(paste0("sel",idx), NULL,
    #      choices = '',selected=NULL)
    #    ),
    #    column(1, checkboxInput(paste0("check",idx), NULL, value=TRUE)
    #    ),
    #    column(2, tableOutput(paste0("lab",idx))
    #    ),
    #    column(3, tableOutput(paste0("val",idx))
    #    )
    #    
    #   )
    #    })
    #  )
    #  )
    #  
    #  spTab = reactive({
    #   if(!is.null(input$upload)){
    #    spTab = input$upload
    #  } else {
    #    spTab = data.frame(
    #    name=list.files(system.file("extdata/", package = "bnspatial"),'.tif'),
    #    path=list.files(system.file("extdata/", package = "bnspatial"),'.tif', full.names = TRUE)
    #    )
    #  }
    #  return(spTab)
    #  })
    #   
    #  observe({
    #    lapply(1:nrow(spTab()), function(i){
    #    updateSelectInput(session, paste0("sel",i), choices = bn$universe$nodes)
    #    })
    #    lapply(1:nrow(spTab()), function(i){
    #    output[[paste0("lab",i)]] = renderTable(
    #    data.frame(bn$universe$levels[[ input[[paste0("sel",i)]] ]])
    #    ,colnames = FALSE)
    #    })
    #    # lapply(1:nrow(spTab()), function(i){
    #    #   if(!is.null(input[[paste0("check",i)]])){
    #    #  x = length(bn$universe$levels[[ input[[paste0("sel",i)]] ]])
    #    #  if(input[[paste0("check",i)]]){
    #    #  # output[[paste0("val",i)]] = renderTable( data.frame(Data_value = rep('',5)), colnames=FALSE )
    #    #  df = data.frame(Data_value = rep('',5))
    #    #  output[[paste0("val",i)]] = DT::renderDataTable({
    #    #  df
    #    #  }, editable=TRUE, rownames = FALSE, colnames=FALSE,
    #    #  options = list(dom = 't',ordering = FALSE, iDisplayLength=nrow(df), bFilter=0))
    #    #  } else {
    #    #   output[[paste0("val",i)]] = renderTable( data.frame(From=rep('',5),to=rep('',5)) )
    #    #  }
    #    #   }
    #    # })
    #  
    #  })
    #  
    #  # observe({
    #  #   classes <<- setClasses(
    #  #   input$ node names,
    #  #   input$ node classes, 
    #  #   input$ boundaries)
    #  # })
    #   
    #   library(raster)
    #  observeEvent(input$selPreview, {
    #   
    #  updateSelectInput(session, 'selPreview', choices = spTab()$name)
    #  output$plot_maps = renderPlot({
    #  r = raster(as.character(spTab()$path)[spTab()$name == input$selPreview])
    #  plot(r)
    #  })
    #  
    #   output$plot_expected = renderPlot({
    #   r = raster(nrows=180, ncols=360)
    #   r[] = 1:length(r)
    #   plot(r)
    #   })
    #   output$plot_uncert = renderPlot({
    #   r = raster(nrows=180, ncols=360)
    #   r[] = sample(length(r))
    #   plot(r)
    #   })
    #   })
    #   # observeEvent(input$exeButton, {
    #   #  bnspatial(network, target, lk, NULL, input$selOutput, ) 
    #   # })
    #   
    #   session$onSessionEnded(stopApp)
}
