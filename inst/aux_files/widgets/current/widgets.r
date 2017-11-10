tagList(
#++++++++++++++++++++++++++++++++++++++++++++++++        Achim Zeileis         ++++++++++++++++++++++++++++++++++++++++
checkboxInput('checkquestionacz', 
              label='Achim Zeileis',
              value = FALSE),
conditionalPanel(
                 condition = 'input.checkquestionacz',
                 DT::dataTableOutput('questions_acz')
                 )
,
#++++++++++++++++++++++++++++++++++++++++++++++++        Testando da silva         ++++++++++++++++++++++++++++++++++++++++
checkboxInput('checkquestiontst', 
              label='Testando da silva',
              value = FALSE),
conditionalPanel(
                 condition = 'input.checkquestiontst',
                 DT::dataTableOutput('questions_tst')
                 )
)
