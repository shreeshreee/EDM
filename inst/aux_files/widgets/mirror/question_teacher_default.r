#++++++++++++++++++++++++++++++++++++++++++++++++        Achim Zeileis         ++++++++++++++++++++++++++++++++++++++++
dad_acz <- reactive({

 aa <- list.files('../../questionbank/austria/ACZ',
                  recursive=T,
                  full.names=T)
 aux_1 <- list.files('../../questionbank/austria/ACZ',
                     recursive=T)
 b <- strsplit(aux_1,
               '\\/')
 dados <- do.call('rbind',
                  b)

 aux_path <- getwd()
 path <- gsub('shiny-examples/myapp','questionbank/austria/ACZ/',aux_path)

 dad <- data.frame(paste('<a href="file:///',path,aux_1,'">',dados[,5],"</a>",sep=''),
                   dados[,1:4],
                   aa)

 names(dad) <- c(tr("narqui"),tr("disci"),tr("assun"),tr("ntipo"),tr("nnivel"),'')
 dad

})

action_acz <- dataTableAjax(session, dad_acz(), rownames = TRUE)

output$questions_acz <- DT::renderDataTable({
 datatable(
           dad_acz()[,-6],
           rownames = TRUE,
           escape = FALSE,
           options = list(
                          ajax = list(url = action_acz)
                          ))
},server=FALSE)
