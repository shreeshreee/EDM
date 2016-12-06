list.dirss <- function(path=".", pattern=NULL, all.dirs=FALSE,
                       full.names=FALSE, ignore.case=FALSE) {
 # use full.names=TRUE to pass to file.info
 all <- list.files(path, pattern, all.dirs,
                   full.names=TRUE, recursive=FALSE, ignore.case)
 dirs <- all[file.info(all)$isdir]
 # determine whether to return full names or just dir names
 if(isTRUE(full.names))
  return(dirs)
 else
  return(basename(dirs))
}   

load("../aux_files/other/translation.bin")

shinyServer(function(input,output,session){

              options(warn = -1)

             tr <- function(text){ # translates text into current language
              sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES=FALSE)
             }

             output$ini       <- renderText({ tr("inicio")    })
             output$sigla     <- renderText({ tr("gbq")       })
             output$titulo    <- renderText({ tr("titulo")    })
             output$saudacao  <- renderText({ tr("saudacao")  })
             output$criarrem  <- renderText({ tr("criarrem")  })
             output$criar     <- renderText({ tr("criar")     })
             output$compname  <- renderText({ tr("compname")  })
             output$escsigla  <- renderText({ tr("escsigla")  })
             output$ndiscp    <- renderText({ tr("ndiscp")    })
             output$nassun    <- renderText({ tr("nassun")    })
             output$gbanco    <- renderText({ tr("gbanco")    })
             output$remover   <- renderText({ tr("remover")   })
             output$criardisc <- renderText({ tr("criardisc") })
             output$rebanco   <- renderText({ tr("rebanco")   })
             output$gdisc     <- renderText({ tr("gdisc")     })
             output$gassu     <- renderText({ tr("gassu")     })
             output$pquest    <- renderText({ tr("pquest")    })
             output$utbanco   <- renderText({ tr("utbanco")   })
             output$esbanqu   <- renderText({ tr("esbanqu")   })
             output$tquestaoa <- renderText({ tr("tquestaoa") })
             output$tquestaon <- renderText({ tr("tquestaon") })
             output$tquestaom <- renderText({ tr("tquestaom") })
             output$tquestaos <- renderText({ tr("tquestaos") })
             output$associacao<- renderText({ tr("associacao")})
             output$mista     <- renderText({ tr("mista")     })
             output$templ1    <- renderText({ tr("templ1")    })
             output$templ2    <- renderText({ tr("templ2")    })
             output$templ3    <- renderText({ tr("templ3")    })
             output$templ4    <- renderText({ tr("templ4")    })
             output$templ5    <- renderText({ tr("templ5")    })
             output$templ6    <- renderText({ tr("templ6")    })
             output$templ7    <- renderText({ tr("templ7")    })
             output$templ8    <- renderText({ tr("templ8")    })
             output$botpdf    <- renderText({ tr("botpdf")    })
             output$botxml    <- renderText({ tr("botxml")    })
             output$botzip    <- renderText({ tr("botzip")    })
             output$tutorial  <- renderText({ tr("tutorial")  })
             output$sair      <- renderText({ tr("sair")      })

             qq <- observe({
             
               if(input$Sair == 1) stopApp()
             
             })


             output$exeabertat <- renderUI({

              source("../aux_files/other/aberta.R",local=TRUE)$value

             })	

             output$exeabertan <- renderUI({

              source("../aux_files/other/numerica.R",local=TRUE)$value

             })

             output$exemultiplaes <- renderUI({

              source("../aux_files/other/multiplaes.R",local=TRUE)$value

             })

             output$exeunicaes <- renderUI({

              source("../aux_files/other/unicaes.R",local=TRUE)$value

             })

             output$exeassociacao <- renderUI({

              source("../aux_files/other/associacao.R",local=TRUE)$value

             })

             output$exemista <- renderUI({

              source("../aux_files/other/mista.R",local=TRUE)$value

             })

             observe({

              shinyjs::toggleState("gerarb", !is.null(input$ydiscp) && input$ydiscp != "" &&  !is.null(input$yassun) && input$yassun != "")

             })

             # +++++++++++++ Criando banco de questões +++++++++++++++

             dirs <- reactive({

              dirs <- list.dirss('../database')
              dirs

             })

             observeEvent(input$gerarb, {

                           if(any(input$text == dirs())) {

                            shinyjs::info(tr("jtbase"))

                           } 

                           withProgress(message = tr("mgbase"), min=1,max=30, {
                                         Sys.sleep(1)

                                         auxass <- paste('../database/',
                                                         input$text,
                                                         '/',
                                                         input$ydiscp,
                                                         '/',
                                                         input$yassun,
                                                         sep='') 

                                         subsub <- c(tr("pdisc"),tr("pobj"))
                                         subsubsub <- c(tr("pnifa"),tr("pnme"),tr("pndif"))

                                         aux1 <- paste(auxass,
                                                       '/',
                                                       subsub,
                                                       sep='')

                                         aux2 <- sapply(aux1,
                                                        function(x)paste(x,
                                                                         subsubsub,
                                                                         sep='/'))

                                         lapply(aux2,function(x)sapply(x,function(y)dir.create(y,recursive=T)))

                                         namedir <- input$text
                                         namefil <- tolower(namedir)


                                         #             ++++++++++++++ Criando Widgets para o novo usuário ++++++++++++++++ #

                                         your_name <- input$yourname
                                         aux_widg <- file('../aux_files/widgets/mirror/widgets_default.r','r+')
                                         old_widg <- readLines(aux_widg)
                                         old_widg1 <- append(old_widg,',',after=0)
                                         new_widg1 <- gsub('iba',namefil,old_widg1)
                                         new_widg2 <- gsub('Ivan Bezerra Allaman',your_name,new_widg1)
                                         cat(new_widg2,file='../aux_files/widgets/temp/new_Widget.r',sep='\n')
                                         close(aux_widg)

                                         #             ++++++++++++++ Criando Question teacher para o novo usuário ++++++++++++++++ #

                                         aux_qt <- file('../aux_files/widgets/mirror/question_teacher_default.r','r+')
                                         old_qt <- readLines(aux_qt)
                                         new_qt <- gsub('iba',namefil,old_qt)
                                         new_qtt <- gsub('IBA',namedir,new_qt)
                                         new_qt1 <- gsub('Ivan Bezerra Allaman',your_name,new_qtt)
                                         cat(new_qt1,file='../aux_files/widgets/temp/new_questiont.r',sep='\n')
                                         close(aux_qt)

                                         #             ++++++++++++++ Criando Widgets para remoção ++++++++++++++++ #

                                         aux_re <- file('../aux_files/widgets/mirror/remove_widgets_default.r','r+')
                                         old_re <- readLines(aux_re)
                                         old_re1 <- append(old_re,',',after=0) 
                                         new_re <- gsub('iba',namefil,old_re1)
                                         new_re1 <- gsub('Ivan Bezerra Allaman',your_name,new_re)
                                         cat(new_re1,file='../aux_files/widgets/temp/new_remove_widgets.r',sep='\n')
                                         close(aux_re)

                                         #             +++++++++++++ Escrevendo o novo Widget completo para arquivo ui.r +++++++++++++ #

                                         nw <- '../aux_files/widgets/temp/new_Widget.r'
                                         ow <- '../aux_files/widgets/current/widgets.r'

                                         aux_ow <- file(ow,'r+') 
                                         length_ow <- length(readLines(aux_ow))

                                         file.append(ow,nw)
                                         close(aux_ow)

                                         aux_ow1 <- file(ow,'r+')
                                         old_ow1 <- readLines(aux_ow1)
                                         old_ow1 <- old_ow1[-length_ow]
                                         new_ow <- append(old_ow1,')',after=length(old_ow1))
                                         cat(new_ow, file=ow,sep='\n')
                                         close(aux_ow1)

                                         #             +++++++++++++ Escrevendo o novo Question teacher +++++++++++ #

                                         nqt <- '../aux_files/widgets/temp/new_questiont.r'
                                         oqt <- '../aux_files/widgets/current/question_teacher.r'

                                         file.append(oqt,nqt) 

                                         cat(tr("mdelete"),file=paste(aux2[1,2],'/teste.Rnw',sep=''))
                                         cat(tr("mdelete"),file=paste(aux2[1,1],'/teste2.Rnw',sep='')) 

                                         #             +++++++++++++ Escrevendo o novo Widget de remoção +++++++++++++ #

                                         nwr <- '../aux_files/widgets/temp/new_remove_widgets.r'
                                         owr <- '../aux_files/widgets/current/remove_widgets.r'

                                         aux_owr <- file(owr,'r+') 
                                         length_owr <- length(readLines(aux_owr))

                                         file.append(owr,nwr)
                                         close(aux_owr)

                                         aux_ow1r <- file(owr,'r+')
                                         old_ow1r <- readLines(aux_ow1r)
                                         old_ow1r <- old_ow1r[-length_owr]
                                         new_owr <- append(old_ow1r,')',after=length(old_ow1r))

                                         aux_grep <- grep('iba',new_owr)

                                         if(length(aux_grep) != 0){

                                          new_owr <- new_owr[-c(2:6)]

                                         }

                                         cat(new_owr, file=owr,sep='\n')
                                         close(aux_ow1r)
                                         setProgress(1)
})
             })

             listad <- reactive({

              aux_lista <- paste("'",
                                 dirs(),
                                 "'='",
                                 tolower(dirs()),
                                 "'",
                                 sep='',
                                 collapse=',')
              lista <- paste('list(',
                             aux_lista,
                             ')',
                             sep='')
              lista
             })

             output$discip <- renderUI({
              wellPanel(
                        radioButtons('input.dirs',
                                     label = h4(tr("esbanqu")),
                                     choices = eval(parse(text=listad())),
                                     selected = 'iba'),
                        textInput('yourdisci', 
                                  label= h4(tr("condisc")),
                                  value = 'calculo_I') 
                        )
             })       

             observeEvent(input$gerard, {

                           auxdisc <- paste('../database/',
                                            toupper(input$input.dirs),
                                            '/',
                                            input$yourdisci,
                                            sep='') 

                           dir.create(auxdisc, recursive=T) 
             })

             subdirs <- reactive({

              subdirs <- list.dirss(paste('../database/',
                                          toupper(input$input.dirs),
                                          sep=''))
              subdirs
             })

             listaa <- reactive({

              aux_lista <- paste("'",
                                 subdirs(),
                                 "'='",
                                 subdirs(),
                                 "'",
                                 sep='',
                                 collapse=',')
              lista <- paste('list(',
                             aux_lista,
                             ')',
                             sep='')
              lista
             }) 

             output$assunt <- renderUI({

              wellPanel(
                        radioButtons('input.subdirs',
                                     label = h4(tr("esdisc")),
                                     choices = eval(parse(text=listaa())),
                                     selected = 'basica'),
                        textInput('yourass', 
                                  label= h4(tr("conass")),
                                  value = 'amostragem') 
                        ) 

             })

             observeEvent(input$gerara, {

                           #                          if(length(subdirs()) == 0) {

                           #output$alert_as <- renderUI({ sidebarPanel(print("Atenção! Todo assunto deve estar vinculado a uma disciplina. Portanto, gere primeiro uma disciplina!"))}) 

                           #                         } else {

                           output$alert_as <- renderUI({ sidebarPanel(print("OK!"))})   
                           auxass <- paste('../database/',
                                           toupper(input$input.dirs),
                                           '/',
                                           input$input.subdirs,
                                           '/',
                                           input$yourass,
                                           sep='') 

                           subsub <- c(tr("pdisc"),tr("pobj"))
                           subsubsub <- c(tr("pnifa"),tr("pnme"),tr("pndif"))

                           aux1 <- paste(auxass,
                                         '/',
                                         subsub,
                                         sep='')

                           aux2 <- sapply(aux1,
                                          function(x)paste(x,
                                                           subsubsub,
                                                           sep='/'))

                           lapply(aux2,function(x)sapply(x,function(y)dir.create(y,recursive=T)))


                           #}
             })

             # +++++++++++++ Removendo banco de questões +++++++++++++++ 
             output$removee <- renderUI({

              if(length(dirs()) == 1) {

               helpText(h2(tr("mrbanco")))

              } else {

               source('../aux_files/widgets/current/remove_widgets.r',local=TRUE)$value 

              }
             })

             observeEvent(input$removerb, {

                           aux_rem  <- list.dirss(path='../database/')
                           siglasr   <- tolower(aux_rem[aux_rem!='IBA'])

                           aux_siglasr <- paste('input$checkquestionr',siglasr,sep='')
                           aux_siglas1r <- unlist(sapply(aux_siglasr, function(x) eval(parse(text=x))))
                           aux_grepp <- siglasr[aux_siglas1r]

                           unlink(paste('../database/',
                                        toupper(aux_grepp),
                                        sep=''),
                                  recursive=T)

                           # +++++++++++ Removendo no arquivo question_teacher.r ++++++++++++++

                           aux_quest  <- file('../aux_files/widgets/current/question_teacher.r','r+')
                           aux_quest1 <- readLines(aux_quest)
                           aux_quest2 <- sapply(aux_grepp,function(x) grep(x,aux_quest1),simplify=FALSE)
                           aux_quest3 <- lapply(aux_quest2, function(x) x[c(1,5)] + c(-1,+2))
                           aux_quest4 <- lapply(aux_quest3,function(x)paste(x,collapse=':'))
                           aux_quest5 <- paste('c(',paste(aux_quest4,collapse=','),')',sep='')
                           aux_quest6 <- aux_quest1[-eval(parse(text=aux_quest5))]

                           cat(aux_quest6,file='../aux_files/widgets/current/question_teacher.r',sep='\n')
                           close(aux_quest) 

                           # +++++++++++ Removendo no arquivo widgets.r ++++++++++++++

                           aux_widgg  <- file('../aux_files/widgets/current/widgets.r','r+')
                           aux_widgg1 <- readLines(aux_widgg)
                           aux_widgg2 <- sapply(aux_grepp,function(x) grep(x,aux_widgg1),simplify=FALSE)
                           aux_widgg3 <- lapply(aux_widgg2, function(x) x[c(1,3)] + c(-1,+2))
                           aux_widgg4 <- lapply(aux_widgg3,function(x)paste(x,collapse=':'))
                           aux_widgg5 <- paste('c(',paste(aux_widgg4,collapse=','),')',sep='')
                           aux_widgg6 <- aux_widgg1[-eval(parse(text=aux_widgg5))]

                           #if(length(aux_widgg6) == 10) aux_widgg6[10] <- ')'

                           if(aux_widgg6[length(aux_widgg6)] == ',') aux_widgg6[length(aux_widgg6)]  <- ')' 

                           cat(aux_widgg6,file='../aux_files/widgets/current/widgets.r',sep='\n')
                           close(aux_widgg) 

                           # +++++++++++ Removendo no arquivo remove_widgets.r ++++++++++++++

                           aux_widgr  <- file('../aux_files/widgets/current/remove_widgets.r','r+')
                           aux_widgr1 <- readLines(aux_widgr)
                           aux_widgr2 <- sapply(aux_grepp,function(x) grep(x,aux_widgr1),simplify=FALSE)
                           aux_widgr3 <- lapply(aux_widgr2, function(x) x[1] - 1)
                           aux_widgr4 <- lapply(aux_widgr3,function(x)paste(x,x+4,sep=':'))
                           aux_widgr5 <- paste('c(',paste(aux_widgr4,collapse=','),')',sep='')
                           aux_widgr6 <- aux_widgr1[-eval(parse(text=aux_widgr5))]

                           if(length(aux_widgr6) == 1) {

                            aux_aux <- file('../aux_files/widgets/mirror/remove_widgets_default.r','r+')
                            aux_aux1 <- readLines(aux_aux)
                            aux_aux2 <- append(aux_aux1,
                                               'tagList(',
                                               0)
                            aux_widgr6 <- append(aux_aux2,
                                                 ')',
                                                 6)

                            close(aux_aux)

                           }

                           if(aux_widgr6[length(aux_widgr6)] == ',') aux_widgr6[length(aux_widgr6)]  <- ')'

                           cat(aux_widgr6,file='../aux_files/widgets/current/remove_widgets.r',sep='\n')
                           close(aux_widgr) 

             })

             #             +++++++++++++++ Preparando o cabeçalho ++++++++++++++++             

             observe({

              # + Nome da avaliação
              cat(input$textaval,file='../sup/aval.tex')

              # + Nome da unidade
              cat(input$textunid,file='../sup/unid.tex')

              # + Nome da disciplina
              cat(input$textdisc,file='../sup/disc.tex')

              # + Carga horária
              cat(input$textcarg,file='../sup/carg.tex')

              # + Período letivo
              cat(input$textanol,file='../sup/anol.tex')

              # + Nome
              cat(input$textname,file='../sup/nome.tex')

             })            

             #             ++++++++++++++   CARREGANDO A BASE DE DADOS   ++++++++++++++++++++ #

             isolate({
              source('../aux_files/widgets/current/question_teacher.r',local=T)
             })

             #             ++++++++++++++++ CRIANDO O VETOR FINAL COM AS QUESTÕES +++++++++++++++#



             #                       output$teste <- renderPrint({ 

             #                           list(Sys.which('texi2dvi'),
             #							Sys.which("pdflatex"),
             #							Sys.getenv("PATH"),
             #							Sys.setenv(PATH=paste(Sys.getenv("PATH"),"C:\\Program Files\\MiKTeX 2.9\\miktex\\bin\\x64",sep=";")),
             #							Sys.which("pdflatex"))
             # aux_que  <- list.dirs(path='..')
             #                            aux_que1 <- aux_que[aux_que!='app']
             #                            siglas   <- tolower(aux_que1)
             #              
             #                            aux_siglas <- paste('input$checkquestion',siglas,sep='')
             #                            aux_siglas1 <- sapply(aux_siglas, function(x) eval(parse(text=x)))
             #              
             #                            aux_siglas2 <- paste('input$questions_',siglas,'_rows_selected',sep='')
             #                            aux_siglas22 <- sapply(aux_siglas2, function(x) eval(parse(text=x)),simplify=FALSE)
             #              
             #                            aux_que3 <- paste('dad_',siglas,'()[',aux_siglas22,',6]', sep='')  
             #                            aux_que4 <- aux_que3[aux_siglas1]
             #                            q <- unlist(sapply(aux_que4, function(x) eval(parse(text=x))))
             #                            q

             #                          })

             questions <- reactive({

              aux_que  <- list.dirss(path='../database/')
              siglas   <- tolower(aux_que)

              aux_siglas <- paste('input$checkquestion',siglas,sep='')
              aux_siglas1 <- sapply(aux_siglas, function(x) eval(parse(text=x)))

              aux_siglas2 <- paste('input$questions_',siglas,'_rows_selected',sep='')
              aux_siglas22 <- sapply(aux_siglas2, function(x) eval(parse(text=x)),simplify=FALSE)

              aux_que3 <- paste('dad_',siglas,'()[',aux_siglas22,',6]', sep='')  
              aux_que4 <- aux_que3[aux_siglas1]
              que <- unlist(sapply(aux_que4, function(x) eval(parse(text=x))))

              enc <- sapply(que,
                            function(x) detectFileEncoding(x))

              conv <- mapply(function(x,y) writeLines(iconv(readLines(x),  
                                                            from=y,
                                                            to="UTF-8"), 
                                                      file(x, 
                                                           encoding="UTF-8")),
                             que,
                             enc)													
              que 

             })



             #observe({

             #enc <- sapply(questions(),
             #                function(x) detectFileEncoding(x))

             #mapply(function(x,y) writeLines(iconv(readLines(x),  
             #                                        from=y,
             #								to="iso8859-1"), 
             #										         file(paste(Sys.getenv("TMP"),"\\",x,sep=""), 
             #												 encoding="iso8859-1")),
             #                                                           questions(),
             #                                                         enc,
             #												 SIMPLIFY=FALSE)


             #nque  <- sapply(questions,function(x) file(x,"r+"))
             #nque1 <- lapply(nque, function(x) readLines(x))
             #sapply(questions(),function(x) cat(x,file=x,sep='\n'))

             #})

             #            enco <- reactive({
             #            enco <- iconvlist()
             #            enco

             #           })

             #            output$encod <- renderUI({

             #              selectInput("encoding", 
             #                          label = 'Encoding:',
             #                        choices = enco(),
             #                       selected = enco()[enco()=='UTF-8'])

             #         })

             #             +++++++++++ Gerando os arquivos ++++++++++++++ #

             getid <- reactive({

              function(i) paste('Av', 
                                gsub(' ','0',
                                     format(i, width=2)),sep='')
             })

             output$downloadPDF <- downloadHandler(filename = function(){
                                                    paste('av','1.pdf',sep='')
             },
             content = function(file){
              #.Platform
              #Sys.setenv(PATH=paste(Sys.getenv("PATH"),"C:\\Program Files\\MiKTeX 2.9\\miktex\\bin\\x64",sep=";"))
              exams2pdf(questions(),
                        n = input$numquestion,
                        template = c('../template/avaliacao.tex',
                                     '../template/solucao.tex'),
                        header = list(ID=getid(),Date=Sys.Date()),
                        name = c('av','av.gab'),
                        dir = file.path('../'),
                        inputs = file.path(sub('shiny','sup',getwd()),c("aval.tex",
                                                                        "unid.tex",
                                                                        "disc.tex",
                                                                        "carg.tex",
                                                                        "anol.tex",
                                                                        "nome.tex")),
                        encoding = "UTF-8")

              file.copy('av1.pdf',
                        file)		
             },
             contentType = 'application/pdf'

             ) 

             output$downloadXML <- downloadHandler(filename = function(){
                                                    paste('moodlequiz','.xml',sep='')
             },
             content = function(file){

              exams2moodle(questions(),
                           n = input$numquestion,
                           dir = file.path('../'),
                           encoding = "UTF-8")

              file.copy('moodlequiz.xml',
                        file)

             },
             contentType = 'application/xml'

             ) 

             output$downloadQuestions <- downloadHandler(
                                                         filename = function() {
                                                          paste("questions", "zip", sep=".")
                                                         },
                                                         content = function(file) {
                                                          fs <- NULL
                                                          for (i in 1:length(questions())) {
                                                           fs[i] <- questions()[i]
                                                          }
                                                          zip(zipfile=file, files=fs)
                                                         },
                                                         contentType = "application/zip"
                                                         )

             #              session$onSessionEnded(function() {
             #                                      stopApp()
             #                                                          })
                   })
