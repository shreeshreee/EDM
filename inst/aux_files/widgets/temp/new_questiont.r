#++++++++++++++++++++++++++++++++++++++++++++++++        Osama Bin Laden         ++++++++++++++++++++++++++++++++++++++++
dad_obl <- reactive({

 aa <- list.files('../../questionbank/afghanistan/OBL',
                  recursive=T,
                  full.names=T)
 aux_1 <- list.files('../../questionbank/afghanistan/OBL',
                     recursive=T)
 b <- strsplit(aux_1,
               '\\/')
 dados <- do.call('rbind',
                  b)

 aux_path <- getwd()
 path <- gsub('shiny-examples/myapp','questionbank/afghanistan/OBL/',aux_path)

 dad <- data.frame(paste('<a href="file:///',path,aux_1,'">',dados[,5],"</a>",sep=''),
                   dados[,1:4],
                   aa)

 names(dad) <- c(tr("narqui"),tr("disci"),tr("assun"),tr("ntipo"),tr("nnivel"),'')
 dad

})

action_obl <- dataTableAjax(session, dad_obl(), rownames = TRUE)

output$questions_obl <- DT::renderDataTable({
 datatable(
           dad_obl()[,-6],
           rownames = TRUE,
           escape = FALSE,
           options = list(
                          ajax = list(url = action_obl)
                          ))
},server=FALSE)
