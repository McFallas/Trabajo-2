inp <- read.csv("FDC.csv")


head(inp)

dim(inp)

inp[!complete.cases(inp),]

plot(inp[,2],type= "l", col = "#9900FF",
     
     main = "Volumen de agua por tiempo",
     xlab = ("Rio Estrella"),
     ylab = ("Rio Banano")
)

lines(inp[ ,3], col = "#33CCCC")

summary(inp[,2:3])
hist(inp[,2],
     main = "Histograma Rio Estrella",
     xlab = ("Rango absoluto"),
     ylab = ("Frecuencia"),
     col = "#00CC66"
)
hist(inp[,3],
     main = "Histograma Rio Banano",
     xlab = ("Rango Absoluto"),
     ylab = ("Frecuencia"),
     col = "#CC0099"
)

names(inp) <- c("fecha", "Estrella","Banano")
attach(inp)
plot(Estrella)

Tempdate <- strptime(inp[,1], format= "%d/%m/%Y")


MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN = sum)   
MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%Y"), FUN = sum)   

write.csv(rbind(MAQ_Estrella, MAQ_Banano),file= "MAQ.csv")

plot(MAQ_Banano,ylim = c(100,3000), 
     main = "VALORES ANUALES DE LOS CAUDALES",
     xlab = ("MESES"),
     ylab = ("AÑOS"),
     col = "#FF9900"
)
lines(MAQ_Estrella, col = 2)

MMQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN = sum)   
MMQ_Banano <- tapply(Banano, format(Tempdate, format = "%m"), FUN = sum)  

CORRELACIÓ
#Analisis de correlación

corinp <- cor(inp[,2:3], method= "spearman")

plot(inp[,2], inp[,3],
     main = "COEFICIENTE DE CAUDALES",
     xlab = ("Estrella"),
     ylab = ("Banano"),
     col = "#003333"
)

inp.lm <- lm(inp[,2] ~ inp[,3], data=inp)

summary(inp.lm)

plot(inp.lm)