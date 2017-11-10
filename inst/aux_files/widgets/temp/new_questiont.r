#++++++++++++++++++++++++++++++++++++++++++++++++        Testando da silva         ++++++++++++++++++++++++++++++++++++++++
dad_tst <- reactive({

 aa <- list.files('../../database/afghanistan/TST',
                  recursive=T,
                  full.names=T)
 aux_1 <- list.files('../../database/afghanistan/TST',
                     recursive=T)
 b <- strsplit(aux_1,
               '\\/')
 dados <- do.call('rbind',
                  b)

 aux_path <- getwd()
 path <- gsub('shiny-examples/myapp','database/afghanistan/TST/',aux_path)

 dad <- data.frame(paste('<a href="file:///',path,aux_1,'">',dados[,5],"</a>",sep=''),
                   dados[,1:4],
                   aa)

 names(dad) <- c(tr("narqui"),tr("disci"),tr("assun"),tr("ntipo"),tr("nnivel"),'')
 dad

})

action_tst <- dataTableAjax(session, dad_tst(), rownames = TRUE)

output$questions_tst <- DT::renderDataTable({
 datatable(
           dad_tst()[,-6],
           rownames = TRUE,
           escape = FALSE,
           options = list(
                          ajax = list(url = action_tst)
                          ))
},server=FALSE)
