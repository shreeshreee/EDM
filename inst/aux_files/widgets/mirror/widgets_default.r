#++++++++++++++++++++++++++++++++++++++++++++++++        Ivan Bezerra Allaman         ++++++++++++++++++++++++++++++++++++++++
checkboxInput('checkquestioniba', 
              label='Ivan Bezerra Allaman',
              value = FALSE),
conditionalPanel(
                 condition = 'input.checkquestioniba',
                 DT::dataTableOutput('questions_iba')
                 )
