#++++++++++++++++++++++++++++++++++++++++++++++++        Achim         ++++++++++++++++++++++++++++++++++++++++
dad_aaa <- reactive({

 aa <- list.files('../database/AAA',
                  recursive=T,
                  full.names=T)
 aux_1 <- list.files('../database/AAA',
                     recursive=T)
 b <- strsplit(aux_1,
               '\\/')
 dados <- do.call('rbind',
                  b)

 aux_path <- getwd()
 path <- gsub('shiny','database/AAA/',aux_path)

 dad <- data.frame(paste('<a href="file:///',path,aux_1,'">',dados[,5],"</a>",sep=''),
                   dados[,1:4],
                   aa)

 names(dad) <- c(tr("narqui"),tr("disci"),tr("assun"),tr("ntipo"),tr("nnivel"),'')
 dad

})

action_aaa <- DT::dataTableAjax(session, dad_aaa(), rownames = TRUE)

output$questions_aaa <- DT::renderDataTable({
 DT::datatable(
           dad_aaa()[,-6],
           rownames = TRUE,
           escape = FALSE,
           options = list(
                          ajax = list(url = action_aaa)
                          ))
})
