#++++++++++++++++++++++++++++++++++++++++++++++++        Achim Zeileis         ++++++++++++++++++++++++++++++++++++++++
checkboxInput('checkquestionacz', 
              label='Achim Zeileis',
              value = FALSE),
conditionalPanel(
                 condition = 'input.checkquestionacz',
                 DT::dataTableOutput('questions_acz')
                 )
