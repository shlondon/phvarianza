
#Función para crear la prueba chi-cuadrado para varianza

#Argumentos
#variable, es un vector columna, con los datos que representan la variable.
#varianza_h_n, es el valor de la hipotesis nula
#nivel_significancia, es el nivel de significancia
#tipo_de_prueba, representa las desigualdades de la hipotesis alternativa: dos colas, mayor que, menor que

#Funcion que permite generar el texto de la conclusion de la prueba de hipotesis
decision <- function(estadistico_prueba){
        if(estadistico_prueba <= lado_izq |
           estadistico_prueba >= lado_der){
                paste("El estadistico de prueba es igual a", estadistico_prueba,
                      "Como el estadistico de prueba se encuentra
                      dentro de la region critica de rechazo entonces se rechaza
                      la hipotesis nula en favor de la hipotesis alternativa.")
        }else{
                paste("El estadistico de prueba es igual a", estadistico_prueba,
                      "Como el estadistico es de prueba se encuentra fuera
                      de la region critica de rechazo entonces se concluye que
                      los datos no son suficientes para rechazar la hipotesis nula.")
        }
        
        }

prueba_hip_varianza <- function(variable, varianza_h_n, nivel_significancia,
                            tipo_de_prueba){
        
        grados_de_libertad <- length(variable) - 1
        
        if(tipo_de_prueba == "two.sided"){
                #Valor de la distribucion chi-cuadrado
                #a partir del cual inicia la region de rechazo en el lado izquierdo
                #de la distribución
                lado_izq <- qchisq(nivel_significancia/2,grados_de_libertad)
                
                #Valor de la distribucion chi-cuadrado
                #a partir del cual inicia la region de rechazo en el lado derecho
                #de la distribución
                lado_der <- qchisq((1-nivel_significancia/2), grados_de_libertad)
                
                varianza_muestral <- var(variable)
                
                #Estadístico de prueba
                ep <- grados_de_libertad*varianza_muestral/varianza_h_n
                
               bd <- data.frame(Resultados=c("Varianza muestral",
                                             "Estadistico de prueba",
                                             "Nivel de significancia",
                                             "Prueba de hipotesis de dos colas",
                                             "Region critica de rechazo de hipotesis nula",
                                             "    Lado izquierdo",
                                             "    Lado derecho",
                                            paste("Intervalo de confianza del", nivel_significancia*100,"%"),
                                            "Conclusion")
                                ,
                                 Valores=c(as.character(varianza_muestral),
                                           as.character(ep),
                                           as.character(nivel_significancia),
                                           "",
                                           "",
                                           as.character(lado_izq),
                                           as.character(lado_der),
                                           paste("(",
                                                 as.character(grados_de_libertad * varianza_muestral/lado_izq),
                                                 "-",
                                                 as.character(grados_de_libertad * varianza_muestral/lado_der),
                                                 ")"),
                                           decision(ep)
                                           ))
                bd
                
                
        }else{
                if(tipo_de_prueba == "less"){
                        #Valor de la distribucion chi-cuadrado
                        #a partir del cual inicia la region de rechazo en el lado izquierdo
                        #de la distribución
                        lado_izq <- qchisq(nivel_significancia/2,grados_de_libertad)
                        
                        #Valor de la distribucion chi-cuadrado
                        #a partir del cual inicia la region de rechazo en el lado derecho
                        #de la distribución
                        lado_der <- qchisq((1-nivel_significancia/2), grados_de_libertad)
                        
                        varianza_muestral <- var(variable)
                        
                        #Estadístico de prueba
                        ep <- grados_de_libertad*varianza_muestral/varianza_h_n
                        
                        bd <- data.frame(Resultados=c("Varianza muestral",
                                                      "Estadistico de prueba",
                                                      "Nivel de significancia",
                                                      "Prueba de hipotesis de una sola cola (Menor)",
                                                      "Region critica de rechazo de hipotesis nula",
                                                      "    Lado izquierdo",
                                                      paste("Intervalo de confianza del", nivel_significancia*100,"%"),
                                                      "Conclusion")
                                         ,
                                         Valores=c(as.character(varianza_muestral),
                                                   as.character(ep),
                                                   as.character(nivel_significancia),
                                                   "",
                                                   "",
                                                   as.character(lado_izq),
                                                   paste("(",
                                                         as.character(grados_de_libertad * varianza_muestral/lado_izq),
                                                         "-",
                                                         as.character(grados_de_libertad * varianza_muestral/lado_der),
                                                         ")"),
                                                   decision(ep)
                                         ))
                        bd
                }else{
                        if(tipo_de_prueba == "greater"){
                                #Valor de la distribucion chi-cuadrado
                                #a partir del cual inicia la region de rechazo en el lado izquierdo
                                #de la distribución
                                lado_izq <- qchisq(nivel_significancia/2,grados_de_libertad)
                                
                                #Valor de la distribucion chi-cuadrado
                                #a partir del cual inicia la region de rechazo en el lado derecho
                                #de la distribución
                                lado_der <- qchisq((1-nivel_significancia/2), grados_de_libertad)
                                
                                varianza_muestral <- var(variable)
                                
                                #Estadístico de prueba
                                ep <- grados_de_libertad*varianza_muestral/varianza_h_n
                                
                                bd <- data.frame(Resultados=c("Varianza muestral",
                                                              "Estadistico de prueba",
                                                              "Nivel de significancia",
                                                              "Prueba de hipotesis de una sola cola (Mayor)",
                                                              "Region critica de rechazo de hipotesis nula",
                                                              "    Lado derecho",
                                                              paste("Intervalo de confianza del", nivel_significancia*100,"%"),
                                                              "Conclusion")
                                                 ,
                                                 Valores=c(as.character(varianza_muestral),
                                                           as.character(ep),
                                                           as.character(nivel_significancia),
                                                           "",
                                                           "",
                                                           as.character(lado_der),
                                                           paste("(",
                                                                 as.character(grados_de_libertad * varianza_muestral/lado_izq),
                                                                 "-",
                                                                 as.character(grados_de_libertad * varianza_muestral/lado_der),
                                                                 ")"),
                                                           decision(ep)
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



#bd <- read.table("geardata.txt", col.names = c("diametro", "bold number"))
#head(bd)
#prueba_hip_varianza(bd$diametro,0.01,0.05,"two.sided")
