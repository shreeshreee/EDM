HTML(paste('<textarea id="inputTextToSave4" style="width: 700px; height: 250px;" rows="3" cols="40"><<echo=FALSE,results=hide>>= &#13;&#10; questions <- solutions <- NULL  &#13;&#10; questions[1] <-"',
 tr("alta"),
 '" &#13;&#10; solutions[1] <- FALSE &#13;&#10; questions[2] <- "',
 tr("altb"),
 '" &#13;&#10; solutions[2] <- TRUE  &#13;&#10; questions[3] <- "',
 tr("altc"),
 '" &#13;&#10; solutions[3] <- FALSE &#13;&#10; questions[4] <- "',
 tr("altd"),
 '" &#13;&#10; solutions[4] <- FALSE &#13;&#10; questions[5] <- "',
 tr("alte"),
 '" &#13;&#10; solutions[5] <- FALSE &#13;&#10; o <- sample(1:5) # serve para embaralhar as alternativas &#13;&#10; questions <- questions[o] &#13;&#10; solutions <- solutions[o] &#13;&#10;@ &#13;&#10; &#13;&#10;\\begin{question} &#13;&#10; Coloque aqui o cabeçalho de sua questão! &#13;&#10; &#13;&#10;\\begin{answerlist} &#13;&#10; \\item \\Sexpr{questions[1]} &#13;&#10; \\item \\Sexpr{questions[2]} &#13;&#10;  \\item \\Sexpr{questions[3]} &#13;&#10;  \\item \\Sexpr{questions[4]} &#13;&#10; \\item \\Sexpr{questions[5]} &#13;&#10;\\end{answerlist}  &#13;&#10;\\end{question}  &#13;&#10; &#13;&#10;\\begin{solution} &#13;&#10;\\begin{answerlist} &#13;&#10; \\item \\Sexpr{mchoice2text(solutions[1])} &#13;&#10; \\item \\Sexpr{mchoice2text(solutions[2])} &#13;&#10; \\item \\Sexpr{mchoice2text(solutions[3])} &#13;&#10; \\item \\Sexpr{mchoice2text(solutions[4])} &#13;&#10; \\item \\Sexpr{mchoice2text(solutions[5])} &#13;&#10;\\end{answerlist} &#13;&#10;\\end{solution} &#13;&#10; &#13;&#10;%% META-INFORMATION &#13;&#10;%% \\extype{schoice} &#13;&#10;%% \\exsolution{\\Sexpr{mchoice2string(solutions, single=TRUE)}}  </textarea>',
 sep=''))
