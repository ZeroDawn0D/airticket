# Link to site
https://zerodawn0d.shinyapps.io/airticket/
## Known bugs
* The plot does not change when all three years are deselected. On testing, I concluded that this was because ```observeEvent()``` does not execute if ```checkboxGroupInput()``` has all options unticked 