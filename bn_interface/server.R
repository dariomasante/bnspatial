# Author Dario Masante

server = function(input, output, session) { 
  library(bnspatial)
  library(igraph)
  library(DT)
  library(raster)
    
  observeEvent(c(input$loadButton, input$selBN),{ # load nework----
    if(length(input$loadButton) != 0){
      raw = input$loadButton[['datapath']]
    } else {
      if(input$selBN == 'conwy') selbn = "extdata/LandUseChange.net"
      raw = system.file(selbn, package = "bnspatial")
    }
    bn <<- loadNetwork(raw)
    output$bnet = renderPlot({
      g = igraph.from.graphNEL(bn$dag, name = TRUE, weight = TRUE, unlist.attrs = TRUE)
      plot(g, vertex.label.dist=2)
    })
    updateSelectInput(session, 'selCPT', choices = c('',bn$universe$nodes))
    updateSelectInput(session, 'evidenceNode', choices = bn$universe$nodes)
    updateCheckboxGroupInput(session, 'showBars', choices = bn$universe$nodes)
    updateSelectInput(session, 'evidenceNode2', choices = bn$universe$nodes)
  })
  
  observeEvent(input$evidenceNode, { # evidenceNode ----
    updateRadioButtons(session, 'evidenceClasses', 
                       choices = bn$universe$levels[[input$evidenceNode]])
  })
  observeEvent(input$evidenceNode2, { # evidenceNode2 ----
    updateRadioButtons(session, 'evidenceClasses2', 
                       choices = bn$universe$levels[[input$evidenceNode2]])
  })
  
  observe({ # CPT selCPT ----
    node = input$selCPT
    # updateSelectInput(session, 'editBN', choices = bn$universe$nodes)
    updateSelectInput(session, 'selTarget', choices = c('',bn$universe$nodes)) # Move down
    if(node != ''){
      df = as.data.frame.table(bn$cptlist[[node]])
      nc = ncol(df)
      names(df)[nc] = 'Cond. prob. (%)'
      df[nc] = round(df[nc] * 100, 2)
      if(nc > 2) { df = df[ ,c(1,nc,2:(nc-1))] }
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
  
  evidLog = reactiveVal('') # set evidence ----
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
  observeEvent(input$evidRm, { # remove evidence ----
    s = evidLog()
    ss = unlist(strsplit(s, ' - '))
    ss = ss[!grepl(input$evidenceNode,ss)]
    s = paste0(ss, collapse = ' - ')
    evidLog(s)
  })
  observeEvent(input$evidReset, { evidLog('') })
  
  output$evidLog = renderText({ evidLog() })
  
  observe({
    s = evidLog()
    if(nchar(s)>0){ # Any evidences set?
      ss = unlist(strsplit(s, ' - '))[-1]
      sss = strsplit(ss, ':')
      lst = lapply(sss, '[[', 2)
      names(lst) = sapply(sss, '[[', 1)
    } else { # No evidences provided
      lst = NULL 
    }
    # if(length(input$showBars) > 0){
      output$marginProb = renderPlot({ # margin probs bars ----
        validate(need(input$showBars, 
                      'Select at least one node from the checklist to view its marginals'))
        par(mfrow=c(2,ceiling(length(input$showBars)/2)))
        for(n in input$showBars){
          if(n %in% names(lst)){ # Is evidence on the current node? Set as 1
            toPlot = ifelse(lst[[n]] == bn$universe$levels[[n]], 1, 0)
            names(toPlot) = bn$universe$levels[[n]]
          } else { # Query the network otherwise
            toPlot = gRain::querygrain(bn, n, type = "marginal", evidence = lst)[[1]]
          }
          barplot(toPlot, main= paste(n,'probability'))
        }
      })
        
    # }
  })
  
  observeEvent(input$upClass, { # set classes ----
    #classes = importClasses(input$upClass$path)
    raw = system.file("extdata/LUclasses.txt", package = "bnspatial")
    classes <<- importClasses(raw)
  })

  output$downClass = downloadHandler( 'Spatial2BN.txt', content = function(file){
    if(exists('classes')){
      #classes = importClasses(input$upClass$path)
      l1 = lapply(classes, '[[', 1)
      l2 = lapply(classes, '[[', 2)
      writeLines(paste(names(classes),
        lapply(l1, function(x) paste(x, collapse=',')),
        lapply(l2, function(x) paste(x, collapse=',')),
        collapse = "\n", sep='\n'), file)
    }
  })
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
  output$candidates <- renderUI( # add layers ----
    tagList(
      lapply(1:nrow(input$upload), function(idx){
        fluidRow(
          column(1, actionButton(paste0('rm',idx), 'Remove') ),
          column(3, renderText( input$upload[idx, 'name']) ),
          column(2, selectInput(paste0("sel",idx), NULL,
            choices = '',selected=NULL) ),
          column(1, checkboxInput(paste0("check",idx), NULL, value=TRUE) ),
          column(2, tableOutput(paste0("lab",idx)) ),
          column(3, tableOutput(paste0("val",idx)) )
        )
      })
    )
  )
  
  spTab = reactive({
    if(!is.null(input$upload)){
      tab = input$upload
    } else {
      tab = data.frame(
        name=list.files(system.file("extdata/", package = "bnspatial"),'.tif'),
        datapath=list.files(system.file("extdata/", package = "bnspatial"),'.tif', full.names = TRUE)
      )
    }
    return(tab)
  })
  
  observeEvent(input$upload, {
    dr = 1:nrow(spTab())
    lapply(dr, function(i){
      observeEvent(input[[paste0("rm",i)]], {
        spTab()[-i, ]
      })
    })
    lapply(dr, function(i){
      updateSelectInput(session, paste0("sel",i), choices = bn$universe$nodes)
    })
    lapply(dr, function(i){
      output[[paste0("lab",i)]] = renderTable(
        data.frame(bn$universe$levels[[ input[[paste0("sel",i)]] ]])
          ,colnames = FALSE)
    })
    lapply(dr, function(i){
      if(!is.null(input[[paste0("check",i)]])){
        x = length(bn$universe$levels[[ input[[paste0("sel",i)]] ]])
        if(input[[paste0("check",i)]]){
     # output[[paste0("val",i)]] = renderTable( data.frame(Data_value = rep('',5)), colnames=FALSE )
          df = data.frame(Data_value = rep('',5))
          output[[paste0("val",i)]] = DT::renderDataTable({ 
              df
          }, editable=TRUE, rownames = FALSE, colnames=FALSE,
              options = list(dom = 't',ordering = FALSE, 
              iDisplayLength=nrow(df), bFilter=0))
        } else {
          output[[paste0("val",i)]] = renderTable( data.frame(From=rep('',5),to=rep('',5)) )
        }
      }
    })
  })

     # observe({
     #   classes <<- setClasses(
     #   input$ node names,
     #   input$ node classes,
     #   input$ boundaries)
     # })
      
  observe({ # Inputs view ----
    updateRadioButtons(session, 'selPreview', choices = spTab()$name)
    output$plot_maps = renderLeaflet({ 
      inprev = input$selPreview
      r = raster(as.character(spTab()$datapath)[spTab()$name == inprev])
      leaflet() %>% addTiles() %>% addRasterImage(r) #%>% setView(zoom=2,lng = 0, lat = 0) %>%
    })
  })
  
  evidLog2 = reactiveVal('') # evidence2 for run ----
  observeEvent(input$evidSet2, {
      s = evidLog2()
      if(grepl(input$evidenceNode2, s )){
          ss = unlist(strsplit(s, ' - '))
          ss[grepl(input$evidenceNode2,ss)] = paste0(input$evidenceNode2,':', input$evidenceClasses2)
          s = paste0(ss, collapse = ' - ')
      } else {
          s = paste0(s, ' - ' ,input$evidenceNode2,':', input$evidenceClasses2)
      }
      evidLog2(s)
  })
  observeEvent(input$evidRm2, {
      s = evidLog2()
      ss = unlist(strsplit(s, ' - '))
      ss = ss[!grepl(input$evidenceNode2,ss)]
      s = paste0(ss, collapse = ' - ')
      evidLog2(s)
  })
  observeEvent(input$evidReset2, { evidLog2('') })
  
  output$evidLog2 = renderText({ evidLog2() })
  
  
  output$plot_output = renderLeaflet({ 
      leaflet() %>% addTiles() %>% fitBounds(lng1=-90,lng2=90,lat1=-40,lat2=87)
  })
  
  output$subOut = renderUI({ # Probability output, select which states
      validate(need(input$selOutput, 'Select at least one output type above'))
      if('probability' %in% input$selOutput){
          checkboxGroupInput('targetState','', choices = bn$universe$levels[[input$selTarget]], 
                             selected=bn$universe$levels[[input$selTarget]])
      } else { return(NULL) }
  })

  observeEvent(input$exeButton, { # Query and spatialize ----
    showNotification("Querying the BBN and mapping, please wait.")
    # if(input$selBN == 'conwy') {
    #   lkup = "extdata/LUclasses.txt"
    #   lk = system.file(lkup, package = "bnspatial")
    # # }
    # bnsp = bnspatial(bn, input$selTarget, spTab()$name, lk, what=input$selOutput)
      data(ConwyData)
      list2env(ConwyData, environment())
      
      network <- LandUseChange
      spatialData <- c(ConwyLU, ConwySlope, ConwyStatus)
      lookup <- LUclasses
# Add validation
        output$msg = renderPrint({
            bnsp <<- bnspatial(network, input$selTarget, spatialData, lookup, 
                           what=input$selOutput, targetState=input$targetState, verbose=FALSE)
        })
        output$outSel = renderUI({
          radioButtons('showOutput','Select theme', choices = names(bnsp), inline=TRUE)
        })
      
  })
  
  output$subShow = renderUI({
    if('Probability' == input$showOutput){
      radioButtons('probState','', choices = names(bnsp$Probability), 
                   selected=names(bnsp$Probability)[1])
    } else { return(NULL) }
  })

  observe({
      if(!is.null(input$showOutput)){
        if(input$showOutput == 'Class') {
            rst = bnsp[['Class']]; vals = getValues(rst)
            pal = colorFactor(topo.colors(5), vals, na.color = "transparent")
        }
        if(input$showOutput == 'Entropy') {
            rst = bnsp[['Entropy']]; vals = getValues(rst)
            pal = colorNumeric(c("yellow", "orange", "red"), vals, na.color = "transparent")
        }
        if(input$showOutput == 'ExpectedValue') {
            rst = bnsp[['ExpectedValue']]; vals = getValues(rst)
            pal = colorNumeric(c("yellow", "orange", "red"), vals, na.color = "transparent")
        }
        if(input$showOutput == 'CoefVariation') {
            rst = bnsp[['CoefVariation']]; vals = getValues(rst)
            pal = colorNumeric(c("yellow", "orange", "red"), vals, na.color = "transparent")
        }
        if(input$showOutput == 'Probability'){
          i = ifelse(is.null(input$probState), 1, input$probState)
          rst = bnsp[['Probability']][[i]]; vals = getValues(rst)
          pal = colorNumeric(c("yellow", "orange", "red"), vals, na.color = "transparent")
        } 
        crs(rst) <- crs(ConwyData$ConwySlope) # Put in global.R or get from inputs
        bnd = t(as.matrix(extent(bnsp[[1]])))
        pts = SpatialPoints(bnd, proj4string=crs(ConwyData$ConwySlope))
        bnd = spTransform(pts, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))
        bnd = coordinates(bnd)
        leafletProxy('plot_output') %>% clearImages() %>%
          addRasterImage(rst, colors=pal, opacity = input$opacity) %>% 
          addLegend("bottomright", pal=pal, values=vals, title=input$showOutput,
                    layerId="colorLegend") %>% 
          flyToBounds(lng1=bnd[1],lng2=bnd[2],lat1=bnd[3],lat2=bnd[4])
      }
  })
  
    #   
  session$onSessionEnded(stopApp)
}
