,
#++++++++++++++++++++++++++++++++++++++++++++++++        Osama Bin Laden         ++++++++++++++++++++++++++++++++++++++++
checkboxInput('checkquestionobl', 
              label='Osama Bin Laden',
              value = FALSE),
conditionalPanel(
                 condition = 'input.checkquestionobl',
                 DT::dataTableOutput('questions_obl')
                 )
