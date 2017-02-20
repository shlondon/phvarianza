

#Función para crear la prueba chi-cuadrado para varianza



#Argumentos
#variable, es un vector columna, con los datos que representan la variable.
#varianza_h_n, es el valor de la hipótesis nula
#nivel_significancia, es el nivel de significancia
#tipo_de_prueba, representa las desigualdades de la hipótesis alternativa: dos colas, mayor que, menor que

prueba_varianza <- function(variable, varianza_h_n, nivel_significancia,
                            tipo_de_prueba){
        grados_de_libertad <- length(variable) - 1
        
        if(tipo_de_prueba == "dosColas"){
                #Valor de la distribuciòn chi-cuadrado
                #a partir del cual inicia la región de rechazo en el lado izquierdo
                #de la distribución
                lado_izq <- qchisq(nivel_significancia/2,grados_de_libertad)
                
                #Valor de la distribuciòn chi-cuadrado
                #a partir del cual inicia la región de rechazo en el lado derecho
                #de la distribución
                lado_der <- qchisq((1-nivel_significancia/2), grados_de_libertad)
                
                varianza_muestral <- var(variable)
                
                #Estadístico de prueba
                ep <- grados_de_libertad*varianza_muestral/varianza_h_n
                
                print(paste("Varianza muestral = ", varianza_muestral),quote=FALSE)
                print(paste("Estadístico de prueba = ", ep),quote=FALSE)
                print(paste("Grados de libertad = ", grados_de_libertad),quote=FALSE)
                print(" ",quote=FALSE)
                print(paste("Nivel de significancia = ", nivel_significancia), quote=FALSE)
                print("Prueba de hipótesis de dos colas",quote=FALSE)
                print(paste("Lado izquierdo = ", lado_izq),quote=FALSE)
                print(paste("Lado derecho = ", lado_der),quote=FALSE)
                print(" ",quote=FALSE)
                print(paste((100 - nivel_significancia*100),"%","Intervalo de confianza"),quote=FALSE)
                print(c(grados_de_libertad * varianza_muestral/lado_izq, 
                        grados_de_libertad * varianza_muestral/lado_der),quote=FALSE)
                
                
        }
        
        
}

prueba_varianza(bd$diameter,0.01,0.05,"dosColas")
