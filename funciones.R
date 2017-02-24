
#Función para crear la prueba chi-cuadrado para varianza

#Argumentos
#variable, es un vector columna, con los datos que representan la variable.
#varianza_h_n, es el valor de la hipotesis nula
#nivel_significancia, es el nivel de significancia
#tipo_de_prueba, representa las desigualdades de la hipotesis alternativa: dos colas, mayor que, menor que

#Funcion que permite generar el texto de la conclusion de la prueba de hipotesis


prueba_hip_varianza <- function(variable, varianza_h_n, nivel_significancia,
                            tipo_de_prueba){
        
        grados_de_libertad <- length(variable) - 1
        
        if(tipo_de_prueba == "two.sided"){
                #Valor de la distribucion chi-cuadrado
                #a partir del cual inicia la region de rechazo en el lado derecho
                #de la distribución
                lado_der <- qchisq(nivel_significancia + (1-nivel_significancia)/2, grados_de_libertad)
                
                #Valor de la distribucion chi-cuadrado
                #a partir del cual inicia la region de rechazo en el lado izquierdo
                #de la distribución
                lado_izq <- qchisq((1-nivel_significancia)/2, grados_de_libertad)
                
                varianza_muestral <- var(variable)
                
                #Estadístico de prueba
                ep <- grados_de_libertad*varianza_muestral/varianza_h_n
                
               bd <- data.frame(Resultados=c("Varianza muestral",
                                             "Estadistico de prueba",
                                             "Nivel de significancia",
                                             "Region critica de rechazo de hipotesis nula",
                                            paste("Intervalo de confianza del", nivel_significancia*100,"%"),
                                            "Conclusion")
                                ,
                                 Valores=c(as.character(round(varianza_muestral,8)),
                                           as.character(round(ep,8)),
                                           as.character(nivel_significancia),
                                           paste("lado inferior = ",as.character(round(lado_izq,4)),
                                                 "lado superior = ", as.character(round(lado_der,4)),
                                                 sep = "  "),
                                           paste("(",
                                                 as.character(
                                                         round(grados_de_libertad * varianza_muestral/lado_der,8)),
                                                 "-",
                                                 as.character(round(grados_de_libertad * varianza_muestral/lado_izq,8)),
                                                 ")"),
                                           if(ep <= lado_izq |
                                              ep >= lado_der){
                                                   paste("El estadistico de prueba es igual a", round(ep,8),
                                                         ". Como el estadistico de prueba se encuentra
                              dentro de la region critica de rechazo entonces se rechaza
                              la hipotesis nula en favor de la hipotesis alternativa.")
                                           }else{
                                                   paste("El estadistico de prueba es igual a", round(ep,8),
                                                         ". Como el estadistico de prueba se encuentra fuera
                              de la region critica de rechazo entonces se concluye que
                              los datos no son suficientes para rechazar la hipotesis nula.")
                                           }
                                           ))
                bd
                
                
        }else{
                if(tipo_de_prueba == "less"){
                        #Valor de la distribucion chi-cuadrado
                        #a partir del cual inicia la region de rechazo en el lado izquierdo
                        #de la distribución
                        lado_izq <- qchisq(1-nivel_significancia,grados_de_libertad)
                        
                        #Valor de la distribucion chi-cuadrado
                        #a partir del cual inicia la region de rechazo en el lado derecho
                        #de la distribución
                        #lado_der <- qchisq((1-nivel_significancia/2), grados_de_libertad)
                        
                        varianza_muestral <- var(variable)
                        
                        #Estadístico de prueba
                        ep <- grados_de_libertad*varianza_muestral/varianza_h_n
                        
                        bd <- data.frame(Resultados=c("Varianza muestral",
                                                      "Estadistico de prueba",
                                                      "Nivel de significancia",
                                                      "Region critica de rechazo de hipotesis nula",
                                                      paste("Intervalo de confianza del", nivel_significancia*100,"%"),
                                                      "Conclusion")
                                         ,
                                         Valores=c(as.character(round(varianza_muestral,8)),
                                                   as.character(round(ep,8)),
                                                   as.character(nivel_significancia),
                                                   paste("lado inferior = ",as.character(round(lado_izq,4)),
                                                         sep = "  "),
                                                   paste("(",
                                                         as.character(
                                                                 round(grados_de_libertad * varianza_muestral/qchisq(nivel_significancia + (1-nivel_significancia)/2, grados_de_libertad),8)),
                                                         "-",
                                                         as.character(
                                                                 round(grados_de_libertad * varianza_muestral/qchisq((1-nivel_significancia)/2, grados_de_libertad),8)),
                                                         ")"),
                                                    if(ep <= lado_izq){
                                                                   paste("El estadistico de prueba es igual a", round(ep,8),
                                                                         ". Como el estadistico de prueba se encuentra
                                                                         dentro de la region critica de rechazo entonces se rechaza
                                                                         la hipotesis nula en favor de la hipotesis alternativa.")
                                                           }else{
                                                                   paste("El estadistico de prueba es igual a", round(ep,8),
                                                                         ". Como el estadistico de prueba se encuentra fuera
                                                                         de la region critica de rechazo entonces se concluye que
                                                                         los datos no son suficientes para rechazar la hipotesis nula.")
                                                           }
                                         ))
                        bd
                        
                }else{
                        if(tipo_de_prueba == "greater"){
                                
                                #Valor de la distribucion chi-cuadrado
                                #a partir del cual inicia la region de rechazo en el lado derecho
                                #de la distribución
                                lado_der <- qchisq(nivel_significancia, grados_de_libertad)
                                
                                varianza_muestral <- var(variable)
                                
                                #Estadístico de prueba
                                ep <- grados_de_libertad*varianza_muestral/varianza_h_n
                                
                                bd <- data.frame(Resultados=c("Varianza muestral",
                                                              "Estadistico de prueba",
                                                              "Nivel de significancia",
                                                              "Region critica de rechazo de hipotesis nula",
                                                              paste("Intervalo de confianza del", nivel_significancia*100,"%"),
                                                              "Conclusion")
                                                 ,
                                                 Valores=c(as.character(round(varianza_muestral,8)),
                                                           as.character(round(ep,8)),
                                                           as.character(nivel_significancia),
                                                           paste("lado superior = ", as.character(round(lado_der,4)),
                                                                 sep = "  "),
                                                           paste("(",
                                                                 as.character(
                                                                         round(grados_de_libertad * varianza_muestral/qchisq(nivel_significancia + (1-nivel_significancia)/2, grados_de_libertad),8)),
                                                                 "-",
                                                                 as.character(
                                                                         round(grados_de_libertad * varianza_muestral/qchisq((1-nivel_significancia)/2, grados_de_libertad),8)),
                                                                 ")"),
                                                           if(ep >= lado_der){
                                                                   paste("El estadistico de prueba es igual a", round(ep,8),
                                                                         ". Como el estadistico de prueba se encuentra
                                                                         dentro de la region critica de rechazo entonces se rechaza
                                                                         la hipotesis nula en favor de la hipotesis alternativa.")
                                                           }else{
                                                                   paste("El estadistico de prueba es igual a", round(ep,8),
                                                                         ". Como el estadistico de prueba se encuentra fuera
                                                                         de la region critica de rechazo entonces se concluye que
                                                                         los datos no son suficientes para rechazar la hipotesis nula.")
                                                           }
                                                 ))
                                bd
                        }else{
                                "Usted debe proporcionar el argumento tipo de prueba. Este asume tres valores:

                                Diferente (dos colas)
                                Menor (una cola)
                                Mayo (una cola)
                                
                                Usted debe elegir uno."
                        }
                }
        }
        
        
}



bd <- read.table("geardata.txt", col.names = c("diametro", "bold number"))
head(bd)
prueba_hip_varianza(bd$diametro,0.01,0.95,"two.sided")
