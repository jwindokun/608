if (!require(dplyr)) install.packages("dyplr")
library(dplyr)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if(!require(googleVis)) install.packages('googleVis')
library(googleVis)

#*******************************************************************************
# Code that reads in the files that store the documentation information
Title.html = readLines(paste(getwd(),"/data/Title.htm", sep =""))
Background.html = readLines(paste(getwd(),"/data/Background.htm", sep =""))
Constraints.html = readLines(paste(getwd(),"/data/Constraints.htm", sep =""))
Interface.html = readLines(paste(getwd(),"/data/Interface.htm", sep =""))
References.html = readLines(paste(getwd(),"/data/References.htm", sep =""))
#*******************************************************************************




#*******************************************************************************
# Code that reads in all the data files
# CMS data

saveloc = paste(getwd(), "/data/","cmsdata.RData", sep = "")
load(saveloc)

# Enrollees data
saveloc = paste(getwd(), "/data/","enrollees2012.RData", sep = "")
load(saveloc)

# Datamouth Medicare Payments data
saveloc = paste(getwd(), "/data/","medicare_payments2012.RData", sep = "")
load(saveloc)

# CMS population data for statistical analysis
saveloc = paste(getwd(), "/data/","cmspopulationdata.RData", sep ="")
load(saveloc)

#*******************************************************************************



#*******************************************************************************
# code that uses dplyr to obtain required datasets for loaded data
# preloading this at the beging increases responsiveness

cmspopulationdata = cmspopulationdata %>%
  select(payment, population, population_65,male_65, female_65, white, black,
         american_indian, asian, hawaiian,other, hispanic)

mppstate = cmsdata %>%
  group_by(state) %>%
  summarise(Total_Payment = sum(total_medicare_payment_amt), Count = n()) %>%
  arrange(desc(Total_Payment))

mppestate = cmsdata%>%
  group_by(state) %>%
  summarise(statecap = sum(total_medicare_payment_amt)/mean(beneficiaries)) %>%
  arrange(desc(statecap))

ampptype = cmsdata%>%
  filter(gender == "M" | gender == "F") %>%
  group_by(provider_type) %>%
  summarise(Average_Payment = sum(total_medicare_payment_amt)
            /sum(total_unique_benes)) %>%
  arrange(desc(Average_Payment))

mpprovider = cmsdata%>%
  filter(gender == "M" | gender =="F") %>%
  select(first_name, last_name, Specialty = provider_type,
         Total_Payment = total_medicare_payment_amt) %>%
  arrange(desc(Total_Payment))

amppbene = cmsdata%>%
  filter(gender == "M" | gender == "F") %>%
  mutate(Average_Payment = total_medicare_payment_amt/total_unique_benes) %>%
  select(first_name, last_name, Specialty = provider_type, Average_Payment,
         Total_Payment = total_medicare_payment_amt, total_unique_benes) %>%
  arrange(desc(Average_Payment))

percapita = cmsdata%>%
  filter(gender == "M" | gender == "F") %>%
  group_by(state) %>%
  summarise(statecap = sum(total_medicare_payment_amt)/mean(beneficiaries)) %>%
  arrange(desc(statecap))

zip = cmsdata%>%
  filter(gender == "M" | gender == "F") %>%
  group_by(zip) %>%
  summarise(total_payment = sum(total_medicare_payment_amt),
  num_ben = sum(total_unique_benes), Average_Payment =
    (total_payment/num_ben)) %>%
  arrange(desc(Average_Payment))


enrollees2012 = enrollees2012 %>%
                arrange(desc(beneficiaries))

medicare_payments2012 = medicare_payments2012 %>%
  arrange(desc(payments))

#*******************************************************************************
shinyServer(function(input, output, session) {

  progress <- shiny::Progress$new()
  progress$set(message = "Loading data", value = 0)

  for (i in 1:10){
      Sys.sleep(1.0)
      progress$set(value = i/10)
  }
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())


#*******************************************************************************
# code to react to user selections in the first panel's radio buttons
passData = reactive({

    if (input$radio == 1){

      data = mppstate
    }else if (input$radio ==2){

      data = mppestate

    }else if (input$radio ==3){

      data = ampptype

    }else if (input$radio ==4){

      data = mpprovider

    }else if (input$radio ==5){

      data = amppbene

    }else if (input$radio ==6){

      data = zip

    }else {

      data = medicare_payments2012
    }

    return(data)

  })
#*******************************************************************************
# code to react to Panel 4 radio button (the documentation tab)

dochtml = reactive({

  if (input$panel_4_radio == 1){

    data = Title.html
  }else if (input$panel_4_radio ==2){

    data = Background.html

  }else if (input$panel_4_radio ==3){

    data = Interface.html

  }else if (input$panel_4_radio ==4){

    data = Constraints.html

  }else if (input$panel_4_radio ==5){

    data = References.html

  }else if (input$panel_4_radio ==6){

    data = zip

  }else {

    #data = medicare_payments2012
  }

  return(data)

})

#*******************************************************************************
# code to react to Panel 3 radio button (the Statistis tab)

stats = reactive({

  if (input$panel_3_radio == 1){

    model_all = lm(data = cmspopulationdata, payment~.)
    return(summary(model_all))
  }else if (input$panel_3_radio ==2){

    return(confint(model_all, level = 0.95))

  }else if (input$panel_3_radio ==3){

    output$panel_3_plot <- renderPlot({

   cmspopulationdata$predpayment = predict(model_all, data = cmspopulationdata)

   ggplot(data=cmspopulationdata,aes(x=predpayment, y=payment)) +
        geom_point(color="red") +
        geom_smooth(method = "lm", aes(x=predpayment,y=payment),color="black") +
        geom_line(aes(x= predpayment, y = payment),color="blue",linetype=2) +
        labs(x="Predicted Medicare Payment from Model",
             y="Payment from CMS Data", title = "Plot of Fitted Relationship
            between Model and CMS Data, with Fitted line (black)") +
        theme(panel.background = element_rect(fill = 'gray70', colour = 'red'))


    })

  }else if (input$panel_3_radio ==4){
    output$panel_3_plot <-  renderPlot({
  cmspopulationdata$predpayment = predict(model_all, data = cmspopulationdata)
  ggplot(data=cmspopulationdata,aes(x = predpayment, y = predpayment-payment)) +
  geom_point(color="red", size = 5) +
  geom_smooth(method = "loess", aes(x = predpayment, y = predpayment-payment),
                color="black") +
  labs(x="Predict Payment from Model",
  y="Difference bewteen Predicted Payment from Model and Payment from CMS Data",
           title = "Plot of Residuals
  between Model and CMS Data, with Fitted line (black)") +
      theme(panel.background = element_rect(fill = 'white', colour = 'red'))
    })
  }else if (input$panel_3_radio ==5){

    data = References.html

  }else if (input$panel_3_radio ==6){

    data = zip

  }else {

    #data = medicare_payments2012
  }

  #return(data)

})

#*******************************************************************************

# Code to create the slider for Panel 1

output$varslider <- renderUI({


    sliderInput("slider", "Select Number of returns:",step=NULL,animate=FALSE,
            min = 1, max = max(nrow(passData()), 10), value = 10)


  })

output$main_plot <- renderPlot({



    if (input$radio == 1){
        plotdata = passData() [1:input$slider,]
        df<<- plotdata
        ggplot(plotdata, aes(x = reorder(state, -Total_Payment),
        y =Total_Payment, fill = Count)) + geom_bar(stat = "identity") +
        labs(list(title = "Medicare Payments By State", x = "State",
        y = "Payment")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

    } else if (input$radio == 2){
      plotdata = passData() [1:input$slider,]
      df<<- plotdata
      ggplot(plotdata, aes(x = reorder(state, -statecap), y =statecap,
      fill = state)) + geom_bar(stat = "identity") +
      labs(list(title = "Medicare Payment Per Enrolled By State", x = "State",
      y = "Average Payment")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    } else if(input$radio == 3){

      plotdata = passData() [1:input$slider,]
      df<<- plotdata
      ggplot(plotdata, aes(x = reorder(provider_type, -Average_Payment),
      y =Average_Payment, fill = provider_type)) + geom_bar(stat = "identity") +
      labs(list(title = "Average Medicare Payments By Provider Type",
      x = "State", y = "Payment")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    } else if(input$radio == 4){

      plotdata = passData()[1:input$slider, ]
      df<<- plotdata
      ggplot(plotdata, aes(x = reorder(last_name, -Total_Payment),
      y =Total_Payment, fill = Specialty)) + geom_bar(stat = "identity") +
      labs(list(title = "Medicare Payments By Provider ", x = "Provider Name",
      y = "Payment")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    } else if(input$radio == 5)  {

      plotdata = passData()[1:input$slider, ]
      df<<- plotdata
      ggplot(plotdata, aes(x = reorder(last_name, -Average_Payment),
      y =Average_Payment, fill = Specialty)) + geom_bar(stat = "identity") +
      labs(list(title = "Average Payment Per Benificiary By Provider ", x = "Provider Name",
      y = "Average Payment")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))


    } else if (input$radio == 6)  {

      plotdata = passData()[1:input$slider, ]
      df<<- plotdata
      ggplot(plotdata, aes(x = reorder(zip, -Average_Payment),
      y =Average_Payment, fill = num_ben)) + geom_bar(stat = "identity") +
      labs(list(title = "Average Payment Per Benificiary By Zipcode ",
      x = "Zipcode", y = "Average Payment")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))


    } else  {

      plotdata = passData()[1:input$slider, ]
      df<<- plotdata
      ggplot(plotdata, aes(x = reorder(state, -payments),
                           y =payments, fill = state)) + geom_bar(stat = "identity") +
        labs(list(title = "Total Medicare Payment Per Benificiary By State http://www.dartmouthatlas.org/",
                  x = "State", y = "Total")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))


  }

  })


  # Include a downloadable file of the plot in the output list.
#   output$downloadPlot <- downloadHandler(
#     filename = "shinyPlot.pdf",
#     # The argument content below takes filename as a function
#     # and returns what's printed to it.
#     content = function(con) {
#       pdf(con)
#       print(w)
#       dev.off(which=dev.cur())
#     }
#   )
#*******************************************************************************
# output code for Panel 2 visualization

output$panel_2_view <- renderGvis({

  chartoptions = list(
    region="US",
    gvis.editor = "Edit Me",
    displayMode="regions",
    resolution="provinces",
    width=1200, height=800,
    colorAxis="{colors: ['#e7711c', '#4374e0']}",
    #colorAxis="{colors: ['blue', 'red']}",
    backgroundColor = '#D3D3D3'

  )


  if (input$panel_2_radio == 1){

    gvisdata<<-enrollees2012
    gvisplot = "beneficiaries"
    gvistext <<- "Map of Medicare Enrollees By State"

  } else if (input$panel_2_radio == 2){

    gvisdata <<- percapita
    gvisplot = "statecap"
    gvistext <<- "Map of Medicare Payements Per Beneficiary By State"

  } else if(input$panel_2_radio == 3){


    gvisdata <<- medicare_payments2012
    gvisplot = "payments"
    gvistext <<- paste("Total Medicare Payements (Provider and Hospital Payments)"
        ,"Per Beneficiary By State", "Data from: Dartmouth College",
                       sep = " ")

  } else{



  }


  gvisGeoChart(gvisdata, "state", gvisplot,
                            options=chartoptions)



})

output$panel_2_text <- renderText({
  if (input$panel_2_radio<7){
        gvistext
  }

})


#*******************************************************************************
# code for Tab 3 Visualization documentation tab

# output$panel_3_view = renderPrint({
#
#  # stats()
#
# })
#
#
# output$panel_3_text <- renderPrint({
#   if (input$panel_3_radio<7){
#     stats()
#   }
#
# })

output$panel_3_verbatim <- renderPrint({
  if (input$panel_3_radio<7){
    stats()
  }

})


#*******************************************************************************

#*******************************************************************************
# code for Panel 4 Visualization documentation tab

output$panel_4_view = renderText({

  dochtml()

})

#*******************************************************************************
# code to render table data for panel 1 and 2
  output$panel_1_table = renderDataTable({

    if ((input$chkvalue ==TRUE) & (input$radio<8)){
            passData()[1:input$slider, ]   }
    })

output$panel_2_table = renderDataTable({

  if ((input$panel_2_chkvalue ==TRUE) & (input$panel_2_radio<7)){
   gvisdata
  }
})

})
