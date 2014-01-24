library(shiny)
library(ChaoSpeciesOnline)
data(Ant)
data(Birds)
#data(Coin)
data(Spider)
data(Seedlings_Pre)
#data(Seedlings_Freq)
#data(Seedlings_Counts)
#Coin <- DataTransform(Coin, "FreqCount")
Seedlings_Pre <- DataTransform(Seedlings_Pre, "MatrixInci")
#Seedlings_Freq <- DataTransform(Seedlings_Freq, "MatrixAbun")
#Seedlings_Counts <- DataTransform(Seedlings_Counts, "InciCount")

shinyServer(function(input, output){
  tempRD11 <- paste(tempfile(), ".RData", sep="") # basic information 1
  tempRD12 <- paste(tempfile(), ".RData", sep="") # basic information 2
  tempRD2 <- paste(tempfile(), ".RData", sep="") # estimation table
  saveList2csv <- function(out, file) {
    for (i in seq_along(out)){
      write.table(names(out)[i], file=file, sep=",", dec=".", 
                  quote=FALSE, col.names=FALSE, row.names=FALSE, append=TRUE)  #writes the name of the list elements
      write.table(out[[i]], file=file, sep=",", dec=".", quote=FALSE, 
                  col.names=NA, row.names=TRUE, append=TRUE)  #writes the data.frames
    }
  }
  loadPaste <- reactive({
    if(input$datatype == 'abundance'){
      text <- input$copyAndPaste_abun
    }else{
      text <- input$copyAndPaste_inci
    }
    Fun <- function(e){
      temp <- lapply(readLines(textConnection(text)), function(x) scan(text = x, what = 'char'))
      out <- list()
      out.name <- 0
      for (i in seq_along(temp)){
        out.name[i] <- temp[[i]][1]
        out[[i]] <- as.numeric(temp[[i]][-1])
      }
      names(out) <- t(data.frame(out.name))
      out
    }
    tryCatch(Fun(e), error = function(e){return()})
  })
  
  getDataName <- reactive({
    Fun <- function(e){
      out <- loadPaste()
      out.name <- names(out)
      if(is.na(names(out)[1]) == TRUE){
        dat <- paste("No data")        
        dat
      }else{
        dat <- out
        for(i in seq_along(out)){
          dat[[i]] <- out.name[i]
        }
        dat
      }
    }
    tryCatch(Fun(e), error = function(e){return()})
  })
  
  #Select data
  output$dataname <- renderUI({
    dat <- getDataName()
    selectInput(inputId="dataset", label="Data", choices=dat, selected = dat[1])
  })
  
  selectedData <- reactive({
    out <- loadPaste()
    selected <- 1
    dataset <- list()
    for(i in seq_along(input$dataset)){
      selected[i] <- which(names(out) == input$dataset[i])
    }
    for(i in seq_along(selected)){
      k <- selected[i]
      dataset[[i]] <- out[[k]]
    }
    names(dataset) <- input$dataset
    return(dataset)
  })
  
  output$datainfo1 <- renderPrint({
    dataset <- selectedData()
    
    if (input$datatype == 'abundance') {
      info1 <- lapply(dataset, function(x) basicAbuntype(x, k=input$cutpt)) 
    } else {
      info1 <- lapply(dataset, function(x) basicIncitype(x, k=input$cutpt))  
    }
    saveRDS(info1, tempRD11)
    return(info1)
  })
  
  #Download summary data
  output$dsummary1 <- downloadHandler(
    filename = function(){ paste('Info_', Sys.Date(), '_[ChaoSpecies].csv', sep='')},
    content = function(file){
      out <- readRDS(tempRD11)
      saveList2csv(out, file)
    }
    )
  
  # Rare/Infrequent Species group
  output$datainfo2 <- renderPrint({
    dataset <- selectedData()
  if (input$datatype == 'abundance') {
    info2 <- lapply(dataset, function(x) RareSpeciesGroupImprove(x, k=input$cutpt)) 
  } else {
    info2 <- lapply(dataset, function(x) InfreqSpeciesGroupImprove(x, k=input$cutpt)) 
  }
  saveRDS(info2, tempRD12)
  return(info2)
})
  
  #Download Rare/Infrequent Species group
  output$dsummary2 <- downloadHandler(
    filename = function(){ paste('group_', Sys.Date(), '_[ChaoSpecies].csv', sep='')},
    content = function(file){
      out <- readRDS(tempRD12)
      saveList2csv(out, file, row.names = F)
    }
  )

  # Estimate the number of species
  output$out <- renderPrint({
    dataset <- selectedData()
    result <- lapply(dataset, function(x)ChaoSpeciesOnline(data=x, datatype=input$datatype, method=input$method, k=input$cutpt, conf=input$conf, detail=F))
    saveRDS(result, tempRD12)
    return(result)
  })
  
  #Download estimation of species richness
  output$dest <- downloadHandler( # dest -> data estimation
    filename = function(){ paste('est_', Sys.Date(), '_[ChaoSpecies].csv', sep='')},
    content = function(file){
      out <- readRDS(tempRD12)
      saveList2csv(out, file)
    }
    )
})