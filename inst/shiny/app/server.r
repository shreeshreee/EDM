shinyServer(function(input,output,session){           

              tr <- function(text){ # translates text into current language
                sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES=FALSE)
              }

              output$ini       <- renderText({ tr("inicio")    })
              output$sigla     <- renderText({ tr("gbq")       })
              output$titulo    <- renderText({ tr("titulo")    })
              output$saudacao  <- renderText({ tr("saudacao")  })
              output$criarrem  <- renderText({ tr("criarrem")  })
              output$criar     <- renderText({ tr("criar")     })
              output$ncountry  <- renderText({ tr("ncountry")  }) 
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
              output$gdatbanco <- renderText({ tr("gdatbanco") })
              output$gdatb     <- renderText({ tr("gdatb")     }) 
              output$pquest    <- renderText({ tr("pquest")    })
              output$utbanco   <- renderText({ tr("utbanco")   })
              output$esbanqu   <- renderText({ tr("esbanqu")   })
              output$esbdados  <- renderText({ tr("esbdados")  }) 
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

              output$exeabertat <- renderUI({

                source("../../aux_files/other/aberta.R",local=TRUE)$value

              })	

              output$exeabertan <- renderUI({

                source("../../aux_files/other/numerica.R",local=TRUE)$value

              })

              output$exemultiplaes <- renderUI({

                source("../../aux_files/other/multiplaes.R",local=TRUE)$value

              })

              output$exeunicaes <- renderUI({

                source("../../aux_files/other/unicaes.R",local=TRUE)$value

              })

              output$exeassociacao <- renderUI({

                source("../../aux_files/other/associacao.R",local=TRUE)$value

              })

              output$exemista <- renderUI({

                source("../../aux_files/other/mista.R",local=TRUE)$value

              })

              observe({

                shinyjs::toggleState("gerarb", input$yourname!="Achim Zeileis" && input$text!="ACZ" && nchar(input$text)==3)

              })

              #+++++++++++++++ Atualizando banco de questões +++++++++++++++++++++

              observeEvent(input$atubanco,{

                             ori <- getwd()

                             whereEDM <- find.package('EDM')
                             wheredata <- paste(whereEDM,
                                                '/questionbank/',
                                                sep="")

                             download.file("https://github.com/ivanalaman/questionbankEDM/archive/master.zip",
                                           destfile= paste(tempdir(),
                                                           '/master.zip',
                                                           sep=''),
                                           method='auto')

                             unzip(paste(tempdir(),
                                         '/master.zip',
                                         sep=''),
                                   exdir = tempdir())

                             afiless <- list.dirss(paste(tempdir(),
                                                         '/questionbankEDM-master',
                                                         sep=''),
                                                   full.names = TRUE)
                                                            
                             bfiless <- list.dirss(afiless,
                                                   full.names = TRUE)
                             
                             dfiless <- bfiless[bfiless!=paste(tempdir(),
                                                               '/questionbankEDM-master/austria/ACZ',
                                                               sep='')]

                             # preparando objetos para fazer os widgets
                             info_banco <- file(paste(tempdir(),
                                                      '/questionbankEDM-master/CADASTRO.md',
                                                      sep=''),
                                                      'r+') 
                             on.exit(close(info_banco))
                             info_banco1 <- readLines(info_banco)
                             info_banco2 <- info_banco1[-c(1:4)]

                             aux_ncou <- info_banco2[seq(1,length(info_banco2),3)]
                             ncou <- mgsub("\\W",
                                           "",
                                           aux_ncou)

                             aux_dir <- info_banco2[seq(2,length(info_banco2),3)]
                             rdir <- regexec("([A-Z]{3})",aux_dir)
                             ndir <- unique(unlist(regmatches(aux_dir,rdir)))

                             nnames <- mgsub("(?:\\s+\\*\\s+[A-Z]{3}\\s-\\s)",
                                             "",
                                             aux_dir)

                             bc <- cbind(ncou, nnames)
                             row.names(bc) <- ndir

                             ### condição: ou o usuário criou um banco de questões ou não!

                             if(file.exists(paste(whereEDM,
                                                  '/aux_files/.who.txt',
                                                  sep=''))){

                               who <- file(paste(whereEDM,
                                                 '/aux_files/.who.txt',
                                                 sep=''),'r+') 
                               on.exit(close(who))
                               who1 <- readLines(who)

                               whodir <- gsub('\\w+\\/',
                                              '',
                                              who1)

                               efiless <- dfiless[dfiless!=paste(tempdir(),
                                                                 '/questionbankEDM-master/',
                                                                 who1,
                                                                 sep='')]

                               filess <- mgsub('\\/[A-Z]{3}$',
                                               '',
                                               efiless,
                                               perl=TRUE)

                               file.copy(from= filess,
                                         to = wheredata,
                                         recursive=TRUE,
                                         copy.mode=TRUE)

                               # preparando objetos para entrada abaixo

                               ifelse(any(row.names(bc) == whodir),
                                      bc1 <- bc[row.names(bc)!= whodir,],
                                      bc1 <- bc
                                      )

                               namefil <- tolower(row.names(bc1))
                               your_name <- bc1[,2]
                               namedir <- row.names(bc1)
                               namecou <- bc1[,1]

                               #             ++++++++++++++ Criando Widgets para os bancos ++++++++++++++++ #

                               setwd(ori)

                               for(i in 1:length(namedir)){

                                 new_widget(namefil   = namefil[i],
                                            your_name = your_name[i])

                                 #             ++++++++++++++ Criando Question teacher para os bancos ++++++++++++++++ #

                                 new_questiont(namefil   = namefil[i], 
                                               your_name = your_name[i], 
                                               namedir   = namedir[i], 
                                               namecou   = namecou[i]) 

                                 #             ++++++++++++++ Criando Widgets para remoção ++++++++++++++++ #

                                 new_remove_widgets(namefil   = namefil[i], 
                                                    your_name = your_name[i]) 
                               }
                               #             +++++++++++++ Escrevendo o novo Widget completo para arquivo ui.r +++++++++++++ #

                               nw <- '../../aux_files/widgets/temp/new_Widget.r'
                               ow <- '../../aux_files/widgets/current/widgets.r'

                               aux_ow <- file(ow,'r+') 
                               on.exit(close(aux_ow))
                               length_ow <- length(readLines(aux_ow))

                               file.append(ow,nw)

                               aux_ow1 <- file(ow,'r+')
                               on.exit(close(aux_ow1))
                               old_ow1 <- readLines(aux_ow1)
                               old_ow1 <- old_ow1[-length_ow]
                               new_ow <- append(old_ow1,')',after=length(old_ow1))
                               cat(new_ow, file=ow,sep='\n')

                               #             +++++++++++++ Escrevendo o novo Question teacher +++++++++++ #

                               nqt <- '../../aux_files/widgets/temp/new_questiont.r'
                               oqt <- '../../aux_files/widgets/current/question_teacher.r'

                               file.append(oqt,nqt) 

                               #             +++++++++++++ Escrevendo o novo Widget de remoção +++++++++++++ #

                               nwr <- '../../aux_files/widgets/temp/new_remove_widgets.r'
                               owr <- '../../aux_files/widgets/current/remove_widgets.r'

                               aux_owr <- file(owr,'r+') 
                               on.exit(close(aux_owr))
                               length_owr <- length(readLines(aux_owr))

                               file.append(owr,nwr)

                               aux_ow1r <- file(owr,'r+')
                               on.exit(close(aux_ow1r))
                               old_ow1r <- readLines(aux_ow1r)
                               old_ow1r <- old_ow1r[-length_owr]
                               new_owr <- append(old_ow1r,')',after=length(old_ow1r))

                               aux_grep <- grep('acz',new_owr)

                               if(length(aux_grep) != 0){

                                 new_owr <- new_owr[-c(2:6)]

                               }

                               cat(new_owr, file=owr,sep='\n')


                             } else {

                               filess <- mgsub('\\/[A-Z]{3}$',
                                               '',
                                               dfiless,
                                               perl=TRUE) 

                                file.copy(from= filess,
                                         to = wheredata,
                                         recursive=TRUE,
                                         copy.mode=TRUE)
                              
                               # preparando objetos para entrada abaixo

                               namefil <- tolower(row.names(bc))
                               your_name <- bc[,2]
                               namedir <- row.names(bc)
                               namecou <- bc[,1]

                               #             ++++++++++++++ Criando Widgets para os bancos ++++++++++++++++ #

                               setwd(ori)

                               for(i in 1:length(namedir)){

                                 new_widget(namefil   = namefil[i],
                                            your_name = your_name[i])

                                 #             ++++++++++++++ Criando Question teacher para os bancos ++++++++++++++++ #

                                 new_questiont(namefil   = namefil[i], 
                                               your_name = your_name[i], 
                                               namedir   = namedir[i], 
                                               namecou   = namecou[i]) 

                                 #             ++++++++++++++ Criando Widgets para remoção ++++++++++++++++ #

                                 new_remove_widgets(namefil   = namefil[i], 
                                                    your_name = your_name[i]) 
                               }

                               #             +++++++++++++ Escrevendo o novo Widget completo para arquivo ui.r +++++++++++++ #

                               nw <- '../../aux_files/widgets/temp/new_Widget.r'
                               ow <- '../../aux_files/widgets/current/widgets.r'

                               aux_ow <- file(ow,'r+') 
                               on.exit(close(aux_ow))
                               length_ow <- length(readLines(aux_ow))

                               file.append(ow,nw)

                               aux_ow1 <- file(ow,'r+')
                               on.exit(close(aux_ow1))
                               old_ow1 <- readLines(aux_ow1)
                               old_ow1 <- old_ow1[-length_ow]
                               new_ow <- append(old_ow1,')',after=length(old_ow1))
                               cat(new_ow, file=ow,sep='\n')

                               #             +++++++++++++ Escrevendo o novo Question teacher +++++++++++ #

                               nqt <- '../../aux_files/widgets/temp/new_questiont.r'
                               oqt <- '../../aux_files/widgets/current/question_teacher.r'

                               file.append(oqt,nqt) 

                               #             +++++++++++++ Escrevendo o novo Widget de remoção +++++++++++++ #

                               nwr <- '../../aux_files/widgets/temp/new_remove_widgets.r'
                               owr <- '../../aux_files/widgets/current/remove_widgets.r'

                               aux_owr <- file(owr,'r+') 
                               on.exit(close(aux_owr))
                               length_owr <- length(readLines(aux_owr))

                               file.append(owr,nwr)

                               aux_ow1r <- file(owr,'r+')
                               on.exit(close(aux_ow1r))
                               old_ow1r <- readLines(aux_ow1r)
                               old_ow1r <- old_ow1r[-length_owr]
                               new_owr <- append(old_ow1r,')',after=length(old_ow1r))

                               aux_grep <- grep('acz',new_owr)

                               if(length(aux_grep) != 0){

                                 new_owr <- new_owr[-c(2:6)]

                               }

                               cat(new_owr, file=owr,sep='\n')


                             }

                             stopApp()
                             showModal(modalDialog(
                                                   title = "Important message",
                                                   "Restart your application!",
                                                   footer = modalButton("OK!"),
                                                   size='m'))

              })

              # +++++++++++++ Criando banco de questões +++++++++++++++

              dirs <- reactive({

                dirs <- list.dirss('../../questionbank')
                dirs

              })

              sdirs <- reactive({

                sdr <- list.dirss(list.dirss('../../questionbank',
                                             full.names=TRUE))
                sdr

              })

              dcountry <- reactive({

                dcountry <- read.table('../../aux_files/other/countries.txt',h=TRUE) 
                dcountry

              })

              output$ccountry <- renderUI({

                listcountry <- unique(as.character(dcountry()$country)) 

                selectInput('countries',
                            label=h4(uiOutput("ncountry")),
                            listcountry)

              })

              observeEvent(input$gerarb, {

                             if(any(input$text == sdirs())) {

                               shinyjs::info(tr("jtbase"))

                               stopApp()

                             } else {

                               withProgress(message = tr("mgbase"), min=1,max=30, {
                                              Sys.sleep(1)

                                              auxass <- paste('../../questionbank/',
                                                              input$countries,
                                                              '/',
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

                                              namedir <- toupper(input$text)
                                              namefil <- tolower(namedir)

                                              namecou <- input$countries
                                              your_name <- input$yourname

                                              #             ++++++++++++++ Criando Widgets para o novo usuário ++++++++++++++++ #

                                              new_widget(namefil   = namefil,
                                                         your_name = your_name)

                                              #             ++++++++++++++ Criando Question teacher para o novo usuário ++++++++++++++++ #

                                              new_questiont(namefil   = namefil, 
                                                            your_name = your_name, 
                                                            namedir   = namedir, 
                                                            namecou   = namecou) 

                                              #             ++++++++++++++ Criando Widgets para remoção ++++++++++++++++ #

                                              new_remove_widgets(namefil   = namefil, 
                                                                 your_name = your_name) 

                                              #             +++++++++++++ Escrevendo o novo Widget completo para arquivo ui.r +++++++++++++ #

                                              nw <- '../../aux_files/widgets/temp/new_Widget.r'
                                              ow <- '../../aux_files/widgets/current/widgets.r'

                                              aux_ow <- file(ow,'r+') 
                                              on.exit(close(aux_ow))
                                              length_ow <- length(readLines(aux_ow))

                                              file.append(ow,nw)

                                              aux_ow1 <- file(ow,'r+')
                                              on.exit(close(aux_ow1))
                                              old_ow1 <- readLines(aux_ow1)
                                              old_ow1 <- old_ow1[-length_ow]
                                              new_ow <- append(old_ow1,')',after=length(old_ow1))
                                              cat(new_ow, file=ow,sep='\n')

                                              #             +++++++++++++ Escrevendo o novo Question teacher +++++++++++ #

                                              nqt <- '../../aux_files/widgets/temp/new_questiont.r'
                                              oqt <- '../../aux_files/widgets/current/question_teacher.r'

                                              file.append(oqt,nqt) 

                                              cat(tr("mdelete"),
                                                  file=paste(aux2[1,1],
                                                             '/teste1.Rnw',sep=''))  
                                              cat(tr("mdelete"),
                                                  file=paste(aux2[1,2],
                                                             '/teste2.Rnw',sep=''))
                                              cat(tr("mdelete"),
                                                  file=paste(aux2[2,1],
                                                             '/teste3.Rnw',sep=''))
                                              cat(tr("mdelete"),
                                                  file=paste(aux2[2,2],
                                                             '/teste4.Rnw',sep=''))
                                              cat(tr("mdelete"),
                                                  file=paste(aux2[3,1],
                                                             '/teste5.Rnw',sep=''))
                                              cat(tr("mdelete"),
                                                  file=paste(aux2[3,2],
                                                             '/teste6.Rnw',sep=''))

                                              #             +++++++++++++ Escrevendo o novo Widget de remoção +++++++++++++ #

                                              nwr <- '../../aux_files/widgets/temp/new_remove_widgets.r'
                                              owr <- '../../aux_files/widgets/current/remove_widgets.r'

                                              aux_owr <- file(owr,'r+') 
                                              on.exit(close(aux_owr))
                                              length_owr <- length(readLines(aux_owr))

                                              file.append(owr,nwr)

                                              aux_ow1r <- file(owr,'r+')
                                              on.exit(close(aux_ow1r))
                                              old_ow1r <- readLines(aux_ow1r)
                                              old_ow1r <- old_ow1r[-length_owr]
                                              new_owr <- append(old_ow1r,')',after=length(old_ow1r))

                                              aux_grep <- grep('acz',new_owr)

                                              if(length(aux_grep) != 0){

                                                new_owr <- new_owr[-c(2:6)]

                                              }

                                              cat(new_owr, file=owr,sep='\n')

                                              # +++++++++++++ Escrevendo um arquivo oculto que conterará o nome do banco criado +++++++++++++ # 
                                              cat(paste(namecou,
                                                        '/',
                                                        namedir,
                                                        sep=''),
                                                  file='../../aux_files/.who.txt',
                                                  sep='\n')
                                              #js$reset()
                                              stopApp()
                                              showModal(modalDialog(
                                                                    title = "Important message",
                                                                    "Restart your application!",
                                                                    footer = modalButton("OK!"),
                                                                    size='m'
                                                                    )) 
                            })
                             }
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

              ############# Gerar disciplina #########################
              output$discip1 <- renderUI({

                radioButtons('input.dirs',
                             label = h4(tr("esbanqu")),
                             choices = eval(parse(text=listad())),
                             selected = 'austria')

              }) 

              subdirs <- reactive({

                subdirs <- list.dirss(paste('../../questionbank/',
                                            input$input.dirs,
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

              output$discip2 <- renderUI({

                tagList(
                        radioButtons('input.subdirs',
                                     label = h4(tr("esbdados")),
                                     choices = eval(parse(text=listaa()))),
                        textInput('yourdisci', 
                                  label= h4(tr("condisc")),
                                  value = 'calculo_I')
                        )
              }) 

              observeEvent(input$gerard, {

                             auxdisc <- paste('../../questionbank/',
                                              input$input.dirs,
                                              '/',
                                              input$input.subdirs,
                                              '/',
                                              input$yourdisci,
                                              sep='') 

                             dir.create(auxdisc, recursive=T) 

                             stopApp()
                             showModal(modalDialog(
                                                   title = "Important message",
                                                   "Restart your application!",
                                                   footer = modalButton("OK!"), 
                                                   size='m'
                                                   ))  
              })

              subsubdirs <- reactive({

                subsubdirs <- list.dirss(paste('../../questionbank/',
                                               input$input.dirs, # country
                                               '/',
                                               input$input.subdirs,# peaple
                                               sep=''))
                req(subsubdirs) #protecao quando o objeto for NULL ou algo parecido
              })

              listaaa <- reactive({

                aux_lista <- paste("'",
                                   subsubdirs(),
                                   "'='",
                                   subsubdirs(),
                                   "'",
                                   sep='',
                                   collapse=',')
                lista <- paste('list(',
                               aux_lista,
                               ')',
                               sep='')
                lista 
              }) 

              #+++++++++++++ Gerar assunto +++++++++++++++++++

              output$assunt <- renderUI({

                wellPanel(
                          radioButtons('input.subsubdirs',
                                       label = h4(tr("esdisc")),
                                       choices = eval(parse(text=listaaa()))),
                          textInput('yourass', 
                                    label= h4(tr("conass")),
                                    value = 'probability') 
                          )
              })

              ####################################################

              observeEvent(input$gerara, {

                             output$alert_as <- renderUI({ sidebarPanel(print("OK!"))})   
                             auxass <- paste('../../questionbank/',
                                             input$input.dirs,
                                             '/',
                                             input$input.subdirs,
                                             '/',
                                             input$input.subsubdirs,
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

                             stopApp()
                             showModal(modalDialog(
                                                   title = "Important message",
                                                   "Restart your application!",
                                                   footer = modalButton("OK!"), 
                                                   size='m'
                                                   ))   
              })


              # +++++++++++++ Removendo banco de questões +++++++++++++++ 
              output$removee <- renderUI({

                if(length(dirs()) == 1) {

                  helpText(h2(tr("mrbanco")))

                } else {

                  source('../../aux_files/widgets/current/remove_widgets.r',local=TRUE)$value 

                }
              })


              observe({

                shinyjs::toggleState("removerb", length(dirs())!=1)

              })

              observeEvent(input$removerb, {

                             withProgress(message = tr("mgbase"), min=1,max=30, {
                                            Sys.sleep(1) 

                                            aux_rem  <- list.dirss(path='../../questionbank/')
                                            aux_rem1 <- list.dirss(path=paste('../../questionbank/',
                                                                              aux_rem,
                                                                              sep=''))

                                            aux_rem11 <- paste('../../questionbank/',
                                                               aux_rem,
                                                               '/',
                                                               aux_rem1,
                                                               sep='')
                                            names(aux_rem11) <- aux_rem1
                                            aux_rem11 <- aux_rem11[names(aux_rem11)!='ACZ']

                                            siglasr   <- tolower(aux_rem1[aux_rem1!='ACZ'])

                                            aux_siglasr <- paste('input$checkquestionr',siglasr,sep='')
                                            aux_siglas1r <- unlist(sapply(aux_siglasr, function(x) eval(parse(text=x))))

                                            aux_grepp <- siglasr[aux_siglas1r]

                                            aux_grepp1 <-  aux_rem11[names(aux_rem11)==toupper(aux_grepp)]

                                            unlink(aux_grepp1,
                                                   recursive = TRUE)

                                            aux_ch <- mgsub(paste('/',
                                                                  toupper(aux_grepp),
                                                                  sep=''),
                                                            rep('',length(aux_grepp)),
                                                            aux_grepp1) 

                                            aux_ch1 <- lapply(sapply(aux_ch,dir),
                                                              length)

                                            aux_ch2 <- unlist(aux_ch1)

                                            if(any(aux_ch2 == 0)){

                                              aux <- aux_ch2[aux_ch2==0]

                                              aux1 <- aux_ch[names(aux_ch)==names(aux)]

                                              unlink(aux1,
                                                     recursive = TRUE)

                                            }

                                            # +++++++++++ Removendo no arquivo question_teacher.r ++++++++++++++

                                            aux_quest  <- file('../../aux_files/widgets/current/question_teacher.r','r+')
                                            on.exit(close(aux_quest)) 
                                            aux_quest1 <- readLines(aux_quest)
                                            aux_quest2 <- sapply(aux_grepp,function(x) grep(x,aux_quest1),simplify=FALSE)
                                            aux_quest3 <- lapply(aux_quest2, function(x) x[c(1,5)] + c(-1,+2))
                                            aux_quest4 <- lapply(aux_quest3,function(x)paste(x,collapse=':'))
                                            aux_quest5 <- paste('c(',paste(aux_quest4,collapse=','),')',sep='')
                                            aux_quest6 <- aux_quest1[-eval(parse(text=aux_quest5))]

                                            fqt6 <- '../../aux_files/widgets/current/question_teacher.r'
                                            cat(aux_quest6,file=fqt6,sep='\n')

                                            # +++++++++++ Removendo no arquivo widgets.r ++++++++++++++

                                            aux_widgg  <- file('../../aux_files/widgets/current/widgets.r','r+')
                                            on.exit(close(aux_widgg)) 
                                            aux_widgg1 <- readLines(aux_widgg)
                                            aux_widgg2 <- sapply(aux_grepp,function(x) grep(x,aux_widgg1),simplify=FALSE)
                                            aux_widgg3 <- lapply(aux_widgg2, function(x) x[c(1,3)] + c(-1,+2))
                                            aux_widgg4 <- lapply(aux_widgg3,function(x)paste(x,collapse=':'))
                                            aux_widgg5 <- paste('c(',paste(aux_widgg4,collapse=','),')',sep='')
                                            aux_widgg6 <- aux_widgg1[-eval(parse(text=aux_widgg5))]

                                            if(aux_widgg6[length(aux_widgg6)] == ',') {

                                              aux_widgg6[length(aux_widgg6)]  <- ')' 

                                            }

                                            fw6 <- '../../aux_files/widgets/current/widgets.r'
                                            cat(aux_widgg6,file=fw6,sep='\n')

                                            # +++++++++++ Removendo no arquivo remove_widgets.r ++++++++++++++

                                            aux_widgr  <- file('../../aux_files/widgets/current/remove_widgets.r','r+')
                                            on.exit(close(aux_widgr)) 
                                            aux_widgr1 <- readLines(aux_widgr)
                                            aux_widgr2 <- sapply(aux_grepp,function(x) grep(x,aux_widgr1),simplify=FALSE)
                                            aux_widgr3 <- lapply(aux_widgr2, function(x) x[1] - 1)
                                            aux_widgr4 <- lapply(aux_widgr3,function(x)paste(x,x+4,sep=':'))
                                            aux_widgr5 <- paste('c(',paste(aux_widgr4,collapse=','),')',sep='')
                                            aux_widgr6 <- aux_widgr1[-eval(parse(text=aux_widgr5))]

                                            if(length(aux_widgr6) == 1) {

                                              aux_aux <- file('../../aux_files/widgets/mirror/remove_widgets_default.r','r+')
                                              on.exit(close(aux_aux)) 
                                              aux_aux1 <- readLines(aux_aux)
                                              aux_aux2 <- append(aux_aux1,
                                                                 'tagList(',
                                                                 0)
                                              aux_widgr6 <- append(aux_aux2,
                                                                   ')',
                                                                   6)

                                            }

                                            if(aux_widgr6[length(aux_widgr6)] == ',') {

                                              aux_widgr6[length(aux_widgr6)]  <- ')'

                                            }

                                            fr6 <- '../../aux_files/widgets/current/remove_widgets.r'
                                            cat(aux_widgr6,file=fr6,sep='\n')

                                            file.remove('../../aux_files/.who.txt')

                                            stopApp() 
                                            showModal(modalDialog(
                                                                  title = "Important message",
                                                                  "Restart your application!",
                                                                  footer = modalButton("OK!"), 
                                                                  size = 'm'
                                                                  ))

                                            })
              })

              #             +++++++++++++++ Preparando o cabeçalho ++++++++++++++++             

              observe({

                # + Nome da avaliação
                textaval <- file('../../sup/aval.tex',
                                 encoding='UTF-8')
                cat(input$textaval,
                    file=textaval)
                close(textaval)

                # + Nome da unidade
                textunid <- file('../../sup/unid.tex',
                              encoding='UTF-8')
                cat(input$textunid,
                    file=textunid)
                close(textunid)

                # + Nome da disciplina
                textdisc <- file('../../sup/disc.tex',
                              encoding='UTF-8')
                cat(input$textdisc,
                    file=textdisc)
                close(textdisc)
                
                # + Carga horária
                textcarg <- file('../../sup/carg.tex',
                              encoding='UTF-8')
                cat(input$textcarg,
                    file=textcarg)
                close(textcarg)

                # + Período letivo
                textano <- file('../../sup/anol.tex',
                              encoding='UTF-8')
                cat(input$textanol,
                    file=textano)
                close(textano)

                # + Nome
                textname <- file('../../sup/nome.tex',
                              encoding='UTF-8')
                cat(input$textname,
                    file=textname)
                close(textname)

              })            

              #             ++++++++++++++   CARREGANDO A BASE DE DADOS   ++++++++++++++++++++ #

              isolate({
                source('../../aux_files/widgets/current/question_teacher.r',local=T)
              })

              #             ++++++++++++++++ CRIANDO O VETOR FINAL COM AS QUESTÕES +++++++++++++++#

              questions <- reactive({

                aux_que  <- list.dirss(path='../../questionbank',
                                       full.names = TRUE)
                siglas   <- tolower(list.dirss(aux_que))
                aux_siglas <- paste('input$checkquestion',siglas,sep='')
                aux_siglas1 <- sapply(aux_siglas, function(x) eval(parse(text=x)))
                aux_siglas1 
                aux_siglas2 <- paste('input$questions_',siglas,'_rows_selected',sep='')
                aux_siglas22 <- sapply(aux_siglas2, function(x) eval(parse(text=x)),simplify=FALSE)

                aux_que3 <- paste('dad_',siglas,'()[',aux_siglas22,',6]', sep='')  
                aux_que4 <- aux_que3[aux_siglas1]
                que <- unlist(sapply(aux_que4, function(x) eval(parse(text=x))))

                enc <- sapply(que,
                              function(x) suppressWarnings(Ruchardet::detectFileEncoding(x)))

                ifelse(enc == "native.enc", enc <- "", enc) 
                conv <- mapply(function(x,y) {
                                 aux1 <- iconv(readLines(x),
                                               from=y,
                                               to="UTF-8")
                                 aux2 <- file(x, 
                                              encoding="UTF-8")
                                 writeLines(aux1,
                                            aux2)
                                 close(aux2)
                              },
                              que,
                              enc)													
                que
              })

              #             +++++++++++ Gerando os arquivos ++++++++++++++ #

              getid <- reactive({

                function(i) paste('Av', 
                                  gsub(' ','0',
                                       format(i, width=2)),sep='')
              })

              res <- reactive({ 
                exams2pdf(questions(),
                          n = input$numquestion,
                          template = c('../../template/avaliacao.tex',
                                       '../../template/solucao.tex'),
                          header = list(ID=getid(),Date=Sys.Date()),
                          name = c('av','av.gab'),
                          dir = paste(Sys.getenv('HOME'),'/EDM',sep=''),
                          inputs = file.path(sub('shiny/app','sup',getwd()),c("aval.tex",
                                                                              "unid.tex",
                                                                              "disc.tex",
                                                                                     "carg.tex",
                                                                                     "anol.tex",
                                                                                     "nome.tex")),
                                 encoding = "UTF-8")
              })

              output$downloadPDF <- downloadHandler(filename = function(){
                                                      paste('av','1.pdf',sep='')
              },
              content = function(arq){

                res()
                file.copy('av1.pdf',
                          arq)	
              },
              contentType = 'application/pdf'

              )

              output$downloadXML <- downloadHandler(filename = function(){
                                                      paste('moodlequiz','.xml',sep='')
              },
              content = function(file){

                exams2moodle(questions(),
                             n = input$numquestion,
                             dir = paste(Sys.getenv('HOME'),'/EDM',sep=''), 
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
              ## Translate update file!

              output$up_file <- renderUI({
                file <- switch(input$language,
                               br = "../../aux_files/other/update_br.md",
                               en = "../../aux_files/other/update_en.md"
                               )
                html <- markdown::markdownToHTML(file,fragment.only=TRUE)
                Encoding(html) <- 'UTF-8'
                return(HTML(html)) 
              })

              ## Translate update guide!

              output$up_guide <- renderUI({
                file <- switch(input$language,
                               br = "../../aux_files/other/tutorial_br.md",
                               en = "../../aux_files/other/tutorial_en.md"
                               )
                html <- markdown::markdownToHTML(file,fragment.only=TRUE)
                Encoding(html) <- 'UTF-8'
                return(HTML(html))
              })

              session$onSessionEnded(function() {
                                       stopApp()
              })

              ## Debugin app!!
              #output$test <- renderText({ questions() })#é um character!!
              #output$coco <- renderText({subsubdirs()})  
})
