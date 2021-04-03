#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
readRenviron(".Renviron")
Quandl.api_key(Sys.getenv('Quandl_API_key'))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$rici <- renderPlotly({
        RICI<- Quandl("RICI/RICI")
        ggplotly(ggplot(data = RICI, aes(x = Date, y = Value)) +
                     geom_line() +
                     ylab("Value") +
                     xlab("Date") +
                     ggtitle("RICI Index") +
                     theme_light())
        })
    
    output$ricia <- renderPlotly({
        RICI_A<- Quandl("RICI/RICIA")
        ggplotly(ggplot(data = RICI_A, aes(x = Date, y = Value)) +
                     geom_line() +
                     ylab("Value") +
                     xlab("Date") +
                     ggtitle("RICI Index - Agriculture") +
                     theme_light())
    })
    
    output$ricie <- renderPlotly({
        RICI_E<- Quandl("RICI/RICIE")
        ggplotly(ggplot(data = RICI_E, aes(x = Date, y = Value)) +
                     geom_line() +
                     ylab("Value") +
                     xlab("Date") +
                     ggtitle("RICI Index - Energy") +
                     theme_light())
    })
    
    output$ricim <- renderPlotly({
        RICI_M <- Quandl("RICI/RICIM")
        ggplotly(ggplot(data = RICI_M, aes(x = Date, y = Value)) +
                     geom_line() +
                     ylab("Value") +
                     xlab("Date") +
                     ggtitle("RICI Index - Metals") +
                     theme_light())
    })
    
    output$rici_to_sp <- renderPlotly({
        RICI<- Quandl("RICI/RICI")
        Sp_500<- tq_get("^GSPC", get = "stock.prices", from = "1998-07-31")
        Sp_500 <- Sp_500[, c(2,6)]
        colnames(Sp_500)[1]<- "Date"
        
        RICI_to_SP<- left_join(RICI, Sp_500)
        colnames(RICI_to_SP)[2]<- "RICI"
        colnames(RICI_to_SP)[3]<- "S&P 500"
        RICI_to_SP[, "Ratio"]<- RICI_to_SP[,"RICI"]/RICI_to_SP[, "S&P 500"]
        head(RICI_to_SP$Ratio, 1)
        mean_ratio <- mean(RICI_to_SP$Ratio, na.rm = TRUE)
        
        ggplotly(ggplot(data = RICI_to_SP, aes(x = Date, y = Ratio)) +
            geom_line() +
            theme_light()+
            ylab("Ratio") +
            xlab("Date") +
            ggtitle("RICI to S&P 500 Ratio") +
            geom_hline(yintercept = mean_ratio, linetype = 2))
    })
    
    output$ricia_to_sp <- renderPlotly({
        RICI_A<- Quandl("RICI/RICIA")
        Sp_500<- tq_get("^GSPC", get = "stock.prices", from = "1998-07-31")
        Sp_500 <- Sp_500[, c(2,6)]
        colnames(Sp_500)[1]<- "Date"
        RICIA_to_SP <- left_join(RICI_A, Sp_500)
        colnames(RICIA_to_SP)[2]<- "RICI_A"
        colnames(RICIA_to_SP)[3]<- "S&P 500"
        RICIA_to_SP[, "Ratio"] <- RICIA_to_SP[, "RICI_A"]/RICIA_to_SP[, "S&P 500"]
        RICIA_mean<- mean(RICIA_to_SP$Ratio, na.rm = TRUE)
        
        RICIA_to_sp_plot <- ggplot(data = RICIA_to_SP, aes(x = Date, y = Ratio)) +
            geom_line() +
            theme_light() +
            xlab("Date") +
            ylab("Ratio") +
            ggtitle("RICIA to S&P 500") +
            geom_hline(yintercept = RICIA_mean, linetype = 2)
    })
    
    output$ricie_to_sp <- renderPlotly({
        RICI_E<- Quandl("RICI/RICIE")
        Sp_500<- tq_get("^GSPC", get = "stock.prices", from = "1998-07-31")
        Sp_500 <- Sp_500[, c(2,6)]
        colnames(Sp_500)[1]<- "Date"
        RICIE_to_SP<- left_join(RICI_E, Sp_500)
        colnames(RICIE_to_SP)[2] <- "RICI_E"
        colnames(RICIE_to_SP)[3] <- "S&P 500"
        RICIE_to_SP[, "Ratio"] <- RICIE_to_SP[, "RICI_E"]/RICIE_to_SP[, "S&P 500"]
        RICIE_mean<- mean(RICIE_to_SP$Ratio, na.rm = TRUE)
        
        ggplotly(ggplot(data = RICIE_to_SP, aes(x = Date, y = Ratio)) +
                     geom_line() +
                     theme_light() +
                     xlab("Date")+
                     ylab("Ratio") +
                     ggtitle("RICIE to S&P 500") +
                     geom_hline(yintercept = RICIE_mean, linetype = 2)) 
    })
    
    output$ricim_to_sp <- renderPlotly({
        RICI_M <- Quandl("RICI/RICIM")
        Sp_500<- tq_get("^GSPC", get = "stock.prices", from = "1998-07-31")
        Sp_500 <- Sp_500[, c(2,6)]
        colnames(Sp_500)[1]<- "Date"
        RICIM_to_SP <- left_join(RICI_M, Sp_500)
        colnames(RICIM_to_SP)[2] <- "RICI_M"
        colnames(RICIM_to_SP)[3] <- "S&P 500"
        RICIM_to_SP[, "Ratio"] <- RICIM_to_SP[, "RICI_M"]/RICIM_to_SP[, "S&P 500"]
        RICIM_mean <- mean(RICIM_to_SP$Ratio, na.rm = TRUE)
        
        RICIM_to_sp_plot <- ggplot(data = RICIM_to_SP, aes(x = Date, y = Ratio)) +
            geom_line() +
            theme_light() +
            xlab("Date") +
            ylab("Ratio") +
            ggtitle("RICIM to S&P 500") +
            geom_hline(yintercept = RICIM_mean, linetype = 2)
    })
    
    output$commodityplot <- renderPlotly({
        commodity = Quandl(input$commodities)
        commoditydf = data.frame(commodity)
        Date <- commoditydf[,1]
        Price <- commoditydf[,2]
        
        ggplotly(ggplot(data = commoditydf, aes(x = Date, y = Price))+
                     geom_line()+
                     xlab("Date") +
                     ylab("Price") +
                     theme_light())
    })
    
    # Year-to-Date change
    ytd <- function(df){
        df_monthly <- df %>%
            tq_transmute(select = Value, mutate_fun = periodReturn, period = "yearly", leading = TRUE) %>%
            map_df(rev) 
        
        x <- unlist(head(df_monthly[,2], 1))
        
        label_percent(accuracy = NULL)(x)
    }
    
    
    # Percent off all time high
    poath <- function(df){
        ath <- df$Value[which.max(df$Value)]
        latest <- head(df$Value, 1)
        poath <- (latest - ath)/ath
        
        label_percent(accuracy = NULL)(poath)
    }
    
    # Date that last price refers to.
    last_date <- function(df){
        d <- format(as.Date(head(df$Date, 1)))
    }
    
    output$metalstable <- renderDataTable({

        # METALS PRICES
        # Gold
        gold <- Quandl("LBMA/GOLD") #done
        gold[, "Value"] <- gold[,2]
        latest_gold <- head(gold[,2], 1)
        # Silver
        silver <- Quandl("LBMA/SILVER")#done
        silver[, "Value"] <- silver$USD
        latest_silver <- head(silver$Value, 1)
        # Platinum
        platinum <- Quandl("JOHNMATT/PLAT") #done
        platinum[, "Value"] <- platinum[,5]
        latest_platinum <- head(platinum[,5], 1)
        # Palladium
        palladium <- Quandl("JOHNMATT/PALL") #done
        palladium[ "Value"] <- palladium[,5]
        latest_palladium <- head(palladium[,5], 1)
        # Aluminum
        aluminum <- Quandl("ODA/PALUM_USD") #done
        latest_aluminum <- head(aluminum$Value, 1)
        # Copper
        copper_futures <- Quandl("CHRIS/CME_HG1") #done
        copper_futures[, "Value"] <- copper_futures[, "Last"]
        latest_copper <- head(copper_futures$Value, 1)
        # Iron ore
        iron_ore <- Quandl("ODA/PIORECR_USD") #done
        latest_iron <- head(iron_ore$Value, 1)
        # Lead
        lead <- Quandl("ODA/PLEAD_USD") #done
        latest_lead <- head(lead$Value, 1)
        # Nickel
        nickel <- Quandl("ODA/PNICK_USD") #done
        latest_nickel <- head(nickel$Value, 1)
        # Zinc
        zinc <- Quandl("ODA/PZINC_USD") #done
        latest_zinc <- head(zinc$Value, 1)
        # Tin
        tin <- Quandl("ODA/PTIN_USD") #done
        latest_tin <- head(tin$Value, 1)
        # Uranium
        uranium <- Quandl("ODA/PURAN_USD") #done
        latest_uranium <- head(uranium$Value, 1)

        
        metalstable <- data.frame(Commodity = c("Gold", "Silver", "Platinum", "Palladium", "Aluminum (Monthly)", "Copper", "Iron Ore (Monthly)",
                                                "Lead (Monthly)", "Nickel (Monthly)", "Zinc (Monthly)", "Tin (Monthly)", "Uranium (Monthly)"),
                                  Price = c(latest_gold, latest_silver, latest_platinum, latest_palladium, latest_aluminum,
                                             latest_copper, latest_iron, latest_lead, latest_nickel, latest_zinc, latest_tin,
                                             latest_uranium),
                                  YTD = c(ytd(gold), ytd(silver), ytd(platinum), ytd(palladium), ytd(aluminum), ytd(copper_futures), ytd(iron_ore), 
                                             ytd(lead), ytd(nickel), ytd(zinc), ytd(tin), ytd(uranium)),
                                  POATH =  c(poath(gold), poath(silver), poath(platinum), poath(palladium), poath(aluminum), 
                                             poath(copper), poath(iron_ore), poath(lead), poath(nickel), poath(zinc), poath(tin), poath(uranium)),
                                  Date = c(last_date(gold), last_date(silver), last_date(platinum), last_date(palladium), last_date(aluminum), last_date(copper_futures),
                                           last_date(iron_ore), last_date(lead), last_date(nickel), last_date(zinc), last_date(tin), last_date(uranium)))
                                 
    })
    
    output$oiltable <- renderDataTable({
        # OIL PRICES
        # WTI Oil
        WTI_oil <- Quandl("ODA/POILWTI_USD") #done (monthly)
        latest_WTI <- head(WTI_oil$Value, 1)
        crude_oil_nymex_futures <- Quandl("CHRIS/CME_CL1") #done
        crude_oil_nymex_futures[, "Value"] <- crude_oil_nymex_futures[, "Settle"]
        latest_crude_nymex <- head(crude_oil_nymex_futures$Value, 1)
        Brent_oil <- Quandl("ODA/POILBRE_USD") #done (monthly)
        latest_brent <- head(Brent_oil$Value, 1)
        # NATURAL GAS
        nymex_gas_futures <- Quandl("CHRIS/CME_NG1") #done
        nymex_gas_futures[, "Value"] <- nymex_gas_futures[, "Settle"]
        latest_gasfut <- head(nymex_gas_futures$Value, 1)
        ice_gas_futures <- Quandl("CHRIS/ICE_M1") #done
        ice_gas_futures[, "Value"] <- ice_gas_futures[, "Settle"]
        latest_ice_gas <- head(ice_gas_futures$Value, 1)

        
        oiltable <- data.frame(Commodity = c("WTI Oil (Monthly)", "Nymex Oil", "Brent Oil (Monthly)", "Gas (Nymex)", "Gas (ICE)"),
                               Price = c(latest_WTI, latest_crude_nymex, latest_brent, latest_gasfut, latest_ice_gas),
                               YTD = c(ytd(WTI_oil), ytd(crude_oil_nymex_futures), ytd(Brent_oil), ytd(nymex_gas_futures), ytd(ice_gas_futures)),
                               POATH = c(poath(WTI_oil), poath(crude_oil_nymex_futures), poath(Brent_oil), poath(nymex_gas_futures), poath(ice_gas_futures)),
                               Date = c(last_date(WTI_oil), last_date(crude_oil_nymex_futures), last_date(Brent_oil), last_date(nymex_gas_futures), last_date(ice_gas_futures)))

    })
    
    output$grainstable <- renderDataTable({
        corn_cbot_futures <- Quandl("CHRIS/CME_C1") #done
        corn_cbot_futures[, "Value"] <- corn_cbot_futures[, "Settle"]
        latest_corn <- head(corn_cbot_futures$Value, 1)
        oats_cbot_futures <- Quandl("CHRIS/CME_O1") #done
        oats_cbot_futures[, "Value"] <- oats_cbot_futures[, "Settle"]
        latest_oats <- head(oats_cbot_futures$Value, 1)
        rice_cbot_futures <- Quandl("CHRIS/CME_RR1") #done
        rice_cbot_futures[, "Value"] <- rice_cbot_futures[, "Settle"]
        latest_rice <- head(rice_cbot_futures$Value, 1)
        soybean_cbot_futures <- Quandl("CHRIS/CME_S1") #done
        soybean_cbot_futures[, "Value"] <- soybean_cbot_futures[, "Settle"]
        latest_soybean <- head(soybean_cbot_futures$Value, 1)
        wheat_cbot_futures <- Quandl("CHRIS/CME_W1") #done
        wheat_cbot_futures[, "Value"] <- wheat_cbot_futures[, "Settle"]
        latest_wheat <- head(wheat_cbot_futures$Value, 1)
        

        grainstable <- data.frame(Commodity = c("Corn", "Oats", "Rice", "Soybeans", "Wheat"),
                                  Price = c(latest_corn, latest_oats, latest_rice, latest_soybean, latest_wheat),
                                  YTD = c(ytd(corn_cbot_futures), ytd(oats_cbot_futures), ytd(rice_cbot_futures), ytd(soybean_cbot_futures), ytd(wheat_cbot_futures)),
                                  POATH = c(poath(corn_cbot_futures), poath(oats_cbot_futures), poath(rice_cbot_futures), poath(soybean_cbot_futures), 
                                            poath(wheat_cbot_futures)),
                                  Date = c(last_date(corn_cbot_futures), last_date(oats_cbot_futures), last_date(rice_cbot_futures),
                                           last_date(soybean_cbot_futures), last_date(wheat_cbot_futures)))
        
        
        
    })
    
    output$farmsandfishery <- renderDataTable({
        dairy <- Quandl("CHRIS/CME_DA1") 
        dairy[, "Value"] <- dairy[, "Settle"]
        latest_dairy <- head(dairy$Value, 1)
        cattle_live_futures <- Quandl("CHRIS/CME_LC1") 
        cattle_live_futures[, "Value"] <- cattle_live_futures[, "Settle"]
        latest_cattle <- head(cattle_live_futures$Value, 1)
        poultry <- Quandl("ODA/PPOULT_USD") 
        latest_poultry <- head(poultry$Value, 1)
        pork_cme_futures <- Quandl("CHRIS/CME_LN1") 
        pork_cme_futures[, "Value"] <- pork_cme_futures[, "Settle"]
        latest_pork <- head(pork_cme_futures$Value, 1)
        wool <- Quandl("ODA/PWOOLC_USD") 
        latest_wool <- head(wool$Value, 1)
        salmon <- Quandl("ODA/PSALM_USD") 
        latest_salmon <- head(salmon$Value, 1)
        shrimp <- Quandl("ODA/PSHRI_USD") 
        latest_shrimp <- head(shrimp$Value, 1)
        
        farmtable <- data.frame(Commodity = c("Live Cattle", "Poultry (Monthly)", "Pork", "Salmon (Monthly)", "Shrimp (Monthly)", "Dairy", "Wool (Monthly)"),
                                Price = c(latest_cattle, latest_poultry, latest_pork, latest_salmon, latest_shrimp, latest_dairy, latest_wool),
                                YTD = c(ytd(cattle_live_futures), ytd(poultry), ytd(pork_cme_futures), ytd(salmon), ytd(shrimp), ytd(dairy), ytd(wool)),
                                POATH = c(poath(cattle_live_futures), poath(poultry), poath(pork_cme_futures), poath(salmon), poath(shrimp), poath(dairy), poath(wool)),
                                Date = c(last_date(cattle_live_futures), last_date(poultry), last_date(pork_cme_futures), last_date(salmon), last_date(shrimp), last_date(dairy), last_date(wool)))
                                
    })
    
    output$agriculturesoft <- renderDataTable({
        arabica_coffee <- Quandl("ODA/PCOFFOTM_USD") 
        latest_arabica <- head(arabica_coffee$Value, 1)
        robusta_coffee <- Quandl("ODA/PCOFFROB_USD") 
        latest_robusta <- head(robusta_coffee$Value, 1)
        coffee_futures <- Quandl("CHRIS/ICE_KC1")
        coffee_futures[, "Value"] <- coffee_futures[, "Settle"]
        latest_coffee <- head(coffee_futures$Value, 1)
        cocoa_beans <- Quandl("ODA/PCOCO_USD") 
        latest_cocoa_beans <- head(cocoa_beans$Value, 1)
        cocoa_futures <- Quandl("CHRIS/ICE_CC1") #done
        cocoa_futures[, "Value"] <- cocoa_futures[, "Settle"]
        latest_cocoa <- head(cocoa_futures$Value, 1)
        tea <- Quandl("ODA/PTEA_USD") #done
        latest_tea <- head(tea$Value, 1)
        sugar_futures <- Quandl("CHRIS/ICE_SB1") #done
        sugar_futures[, "Value"] <- sugar_futures[, "Settle"]
        latest_sugar <- head(sugar_futures$Value, 1)
        
        agriculturesoft <- data.frame(Commodity = c("Arabica coffee (Monthly)", "Robusta coffee (Monthly)", "Coffee (futures)",
                                                    "Cocoa beans (Monthly)", "Cocoa (futures)", "Tea (Monthly)", "Sugar"), 
                                      Price = c(latest_arabica, latest_robusta, latest_coffee, latest_cocoa_beans, latest_cocoa, 
                                                latest_tea, latest_sugar),
                                      YTD = c(ytd(arabica_coffee), ytd(robusta_coffee), ytd(coffee_futures), ytd(cocoa_beans), ytd(cocoa_futures), ytd(tea), ytd(sugar_futures)),
                                      POATH = c(poath(arabica_coffee), poath(robusta_coffee), poath(coffee_futures), poath(cocoa_beans), poath(cocoa_futures),
                                                poath(tea), poath(sugar_futures)),
                                      Date = c(last_date(arabica_coffee), last_date(robusta_coffee), last_date(coffee_futures), last_date(cocoa_beans),
                                               last_date(cocoa_futures), last_date(tea), last_date(sugar_futures)))
        
    })
    
    output$fruitandnuts <- renderDataTable({
        peanuts <- Quandl("ODA/PGNUTS_USD") 
        latest_peanuts <- head(peanuts$Value, 1)
        oranges <- Quandl("ODA/PORANG_USD") 
        latest_orange <- head(oranges$Value, 1)
        orange_juice_futures <- Quandl("CHRIS/ICE_OJ1") 
        orange_juice_futures[, "Value"] <- orange_juice_futures[, "Settle"]
        latest_orange_juice <- head(orange_juice_futures$Value, 1)
        bananas <- Quandl("ODA/PBANSOP_USD") 
        latest_bananas <- head(bananas$Value, 1)
        
        fruitandnuts <- data.frame(Commodity = c("Peanuts (Monthly)", "Oranges (Monthly)", "Orange Juice", "Bananas (Monthly)"),
                                   Price = c(latest_peanuts, latest_orange, latest_orange_juice, latest_bananas),
                                   YTD = c(ytd(peanuts), ytd(oranges), ytd(orange_juice_futures), ytd(bananas)),
                                   POATH = c(poath(peanuts), poath(oranges), poath(orange_juice_futures), poath(bananas)),
                                   Date = c(last_date(peanuts), last_date(oranges), last_date(orange_juice_futures), last_date(bananas)))
    })
})


    
