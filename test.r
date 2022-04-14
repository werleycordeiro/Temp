library(bizdays)
vencimento <- "2017-05-15" 
Ano_in <- substr(liquida, 1, 4)
Ano_out <- substr(vencimento, 1, 4)
# A função if-else abaixo define quais serão as datas de pagamento 
# de cupons baseados no ano de vencimento do título
if((as.integer(format( as.POSIXct( vencimento ) ,"%Y")) %% 2) == 0) { 
  
  datas_cupons <- sort( c( paste0(Ano_in:Ano_out,"-02","-15"), 
                           paste0(Ano_in:Ano_out,"-08","-15") 
                           ) 
                        ) # Se ano par
  } else { 
    
    datas_cupons <- sort( c( paste0(Ano_in:Ano_out,"-05","-15"), 
                             paste0(Ano_in:Ano_out,"-11","-15") 
                             ) 
                          ) # Se ano ímpar
    }
datas_cupons <- datas_cupons[as.Date(liquida) < as.Date(datas_cupons)]
datas_cupons <- datas_cupons[!as.Date(vencimento) < as.Date(datas_cupons)]
datas_cupons <- adjust.next(datas_cupons, "Brazil/ANBIMA")
datas_cupons


create.calendar("Brazil/ANBIMA",
                holidaysANBIMA,
                weekdays = c("saturday", "sunday")
                )
dias_uteis <- bizdays(liquida, datas_cupons, "Brazil/ANBIMA")
dias_uteis

