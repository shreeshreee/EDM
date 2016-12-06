#++++++++++++++++++++++++++++++++++++++++++++++++        Ivan Bezerra Allaman         ++++++++++++++++++++++++++++++++++++++++
dad_iba <- reactive({

 aa <- list.files('../database/IBA',
                  recursive=T,
                  full.names=T)
 aux_1 <- list.files('../database/IBA',
                     recursive=T)
 b <- strsplit(aux_1,
               '\\/')
 dados <- do.call('rbind',
                  b)

 aux_path <- getwd()
 path <- gsub('shiny','database/IBA/',aux_path)

 dad <- data.frame(paste('<a href="file:///',path,aux_1,'">',dados[,5],"</a>",sep=''),
                   dados[,1:4],
                   aa)

 names(dad) <- c(tr("narqui"),tr("disci"),tr("assun"),tr("ntipo"),tr("nnivel"),'')
 dad

})

action_iba <- DT::dataTableAjax(session, dad_iba(), rownames = TRUE)

output$questions_iba <- DT::renderDataTable({
 DT::datatable(
           dad_iba()[,-6],
           rownames = TRUE,
           escape = FALSE,
           options = list(
                          ajax = list(url = action_iba)
                          ))
},server=FALSE)
