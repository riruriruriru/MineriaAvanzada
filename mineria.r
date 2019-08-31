library(ggplot2)
library(grid)
library(data.table)
library(C50)
library(caTools)
library(caret)
library(scales)
library(arulesViz)
install.packages('mclust')
library(mclust)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(data.table)
install.packages("factoextra")
install.packages("NbClust")
library(factoextra)
library(NbClust)
install.packages("stargazer")
library(stargazer)

#breastCancer <- read.csv2("C:/Users/espin/Desktop/analisisDeDatos/breast-cancer-wisconsin.csv", sep = ",")
#wpbc.data <- read.table("C:/Users/espin/Desktop/analisisDeDatos/wpbc.data", header = FALSE, sep = ",")
#write.csv(wpbc.data, file = "wpbc2.csv", row.names=FALSE)
wpbc2.data <- read.csv2("C:/Users/espin/Desktop/MineriaAvanzada/wpbc2.csv", sep = ",")
#Se borran las 4 filas con datos faltantes "?"
wpbc2.data <- subset(wpbc2.data, lymph_node_status!="?")
wpbc2.data$lymph_node_status <- as.integer(as.character(wpbc2.data$lymph_node_status))
wpbc2.data$ID<-
#Se pasan los datos a numeric, excepto la clase
for(i in 4:34){
  wpbc2.data[,i] <- as.numeric(as.character(wpbc2.data[,i]))
}

wpbc2.data.melted <- melt(wpbc2.data, id.var = "Outcome")

#data por caracteristicade nucleo junto con su data frame melted correspondiente:
#Radio
wpbc2.data.radius <-data.frame(
  "m" = wpbc2.data$radius_m, 
  "se" = wpbc2.data$radius_se, 
  "w" = wpbc2.data$radius_w,
  "Outcome" = wpbc2.data$Outcome
)
wpbc2.data.melted.radius <- melt(wpbc2.data.radius, id.var = "Outcome")
#Texture
wpbc2.data.texture <-data.frame(
  "m" = wpbc2.data$texture_m, 
  "se" = wpbc2.data$texture_se, 
  "w" = wpbc2.data$texture_w,
  "Outcome" = wpbc2.data$Outcome
)
wpbc2.data.melted.texture <- melt(wpbc2.data.texture, id.var = "Outcome")
#Perimeter
wpbc2.data.perimeter <-data.frame(
  "m" = wpbc2.data$perimeter_m, 
  "se" = wpbc2.data$perimeter_se, 
  "w" = wpbc2.data$perimeter_w,
  "Outcome" = wpbc2.data$Outcome
)
wpbc2.data.melted.perimeter <- melt(wpbc2.data.perimeter, id.var = "Outcome")
#Area
wpbc2.data.area <-data.frame(
  "m" = wpbc2.data$area_m, 
  "se" = wpbc2.data$area_se, 
  "w" = wpbc2.data$area_w,
  "Outcome" = wpbc2.data$Outcome
)
wpbc2.data.melted.area <- melt(wpbc2.data.area, id.var = "Outcome")
#Smoothness
wpbc2.data.smoothness <-data.frame(
  "m" = wpbc2.data$smoothness_m, 
  "se" = wpbc2.data$smoothness_se, 
  "w" = wpbc2.data$smoothness_w,
  "Outcome" = wpbc2.data$Outcome
)
wpbc2.data.melted.smoothness <- melt(wpbc2.data.smoothness, id.var = "Outcome")
#Compactness
wpbc2.data.compactness <-data.frame(
  "m" = wpbc2.data$compactness_m, 
  "se" = wpbc2.data$compactness_se, 
  "w" = wpbc2.data$compactness_w,
  "Outcome" = wpbc2.data$Outcome
)
wpbc2.data.melted.compactness <- melt(wpbc2.data.compactness, id.var = "Outcome")
#Concavity
wpbc2.data.concavity <-data.frame(
  "m" = wpbc2.data$concavity_m, 
  "se" = wpbc2.data$concavity_se, 
  "w" = wpbc2.data$concavity_w,
  "Outcome" = wpbc2.data$Outcome
)
wpbc2.data.melted.concavity <- melt(wpbc2.data.concavity, id.var = "Outcome")
#concave points
wpbc2.data.concave_points <-data.frame(
  "m" = wpbc2.data$concave_points_m, 
  "se" = wpbc2.data$concave_points_se, 
  "w" = wpbc2.data$concave_points_w,
  "Outcome" = wpbc2.data$Outcome
)
wpbc2.data.melted.concave_points <- melt(wpbc2.data.concave_points, id.var = "Outcome")
#symmetry
wpbc2.data.symmetry <-data.frame(
  "m" = wpbc2.data$symmetry_m, 
  "se" = wpbc2.data$symmetry_se, 
  "w" = wpbc2.data$symmetry_w,
  "Outcome" = wpbc2.data$Outcome
)
wpbc2.data.melted.symmetry <- melt(wpbc2.data.symmetry, id.var = "Outcome")
#fractal dimension
wpbc2.data.fractal_dimension <-data.frame(
  "m" = wpbc2.data$fractal_dimension_m, 
  "se" = wpbc2.data$fractal_dimension_se, 
  "w" = wpbc2.data$fractal_dimension_w,
  "Outcome" = wpbc2.data$Outcome
)
wpbc2.data.melted.fractal_dimension <- melt(wpbc2.data.fractal_dimension, id.var = "Outcome")


#GRAFICOS BASICOS (BOXPLOT)
#Time
ggplot(wpbc2.data, aes(x ="Time",y=Time, fill = Time)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Time", breaks = seq(0, 10, 1),limits=c(0, 10))+
  ggtitle("Time")+
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue")+
  stat_summary(fun.y = var,
               geom = "point",
               size = 3,
               color = "red")+ 
  stat_summary(fun.y=mean, colour="blue", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))+
  stat_summary(fun.y=var, colour="red", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))

#Grafico de radios:
ggplot(data = wpbc2.data.melted.radius, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=Outcome)) +
  ggtitle("Radio")+
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue")+
  stat_summary(fun.y = var,
               geom = "point",
               size = 3,
               color = "red")+ 
  stat_summary(fun.y=mean, colour="blue", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))+
  stat_summary(fun.y=var, colour="red", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))

#Grafico de textura:
ggplot(data = wpbc2.data.melted.texture, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=Outcome)) +
  ggtitle("Textura")+
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue")+
  stat_summary(fun.y = var,
               geom = "point",
               size = 3,
               color = "red")+ 
  stat_summary(fun.y=mean, colour="blue", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))+
  stat_summary(fun.y=var, colour="red", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))
#Grafico de perimetro:
ggplot(data = wpbc2.data.melted.perimeter, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=Outcome)) +
  ggtitle("Perimetro")+
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue")+
  stat_summary(fun.y = var,
               geom = "point",
               size = 3,
               color = "red")+ 
  stat_summary(fun.y=mean, colour="blue", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))+
  stat_summary(fun.y=var, colour="red", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))
#Grafico de area:
ggplot(data = wpbc2.data.melted.area, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=Outcome)) +
  ggtitle("Area")+
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue")+
  stat_summary(fun.y = var,
               geom = "point",
               size = 3,
               color = "red")+ 
  stat_summary(fun.y=mean, colour="blue", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))+
  stat_summary(fun.y=var, colour="red", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))
#Grafico de suavidad:
ggplot(data = wpbc2.data.melted.smoothness, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=Outcome)) +
  ggtitle("Suavidad")+
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue")+
  stat_summary(fun.y = var,
               geom = "point",
               size = 3,
               color = "red")+ 
  stat_summary(fun.y=mean, colour="blue", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))+
  stat_summary(fun.y=var, colour="red", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))
#Grafico de compactness:
ggplot(data = wpbc2.data.melted.compactness, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=Outcome)) +
  ggtitle("Compactness")+
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue")+
  stat_summary(fun.y = var,
               geom = "point",
               size = 3,
               color = "red")+ 
  stat_summary(fun.y=mean, colour="blue", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))+
  stat_summary(fun.y=var, colour="red", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))
#Grafico de Concavidad:
ggplot(data = wpbc2.data.melted.concavity, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=Outcome)) +
  ggtitle("Concavidad")+
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue")+
  stat_summary(fun.y = var,
               geom = "point",
               size = 3,
               color = "red")+ 
  stat_summary(fun.y=mean, colour="blue", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))+
  stat_summary(fun.y=var, colour="red", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))
#Grafico de puntos de concavidad:
ggplot(data = wpbc2.data.melted.concave_points, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=Outcome)) +
  ggtitle("Puntos cóncavos")+
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue")+
  stat_summary(fun.y = var,
               geom = "point",
               size = 3,
               color = "red")+ 
  stat_summary(fun.y=mean, colour="blue", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))+
  stat_summary(fun.y=var, colour="red", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))
#Grafico de simetría:
ggplot(data = wpbc2.data.melted.symmetry, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=Outcome)) +
  ggtitle("Simetría")+
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue")+
  stat_summary(fun.y = var,
               geom = "point",
               size = 3,
               color = "red")+ 
  stat_summary(fun.y=mean, colour="blue", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))+
  stat_summary(fun.y=var, colour="red", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))
#Grafico de dimensión fractal:
ggplot(data = wpbc2.data.melted.fractal_dimension, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill=Outcome)) +
  ggtitle("Dimensión Fractal")+
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue")+
  stat_summary(fun.y = var,
               geom = "point",
               size = 3,
               color = "red")+ 
  stat_summary(fun.y=mean, colour="blue", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))+
  stat_summary(fun.y=var, colour="red", geom="text", show_guide = FALSE, 
               vjust=-0.7, aes( label=round(..y.., digits=2)))


#Histogramas

ggplot(wpbc2.data.melted, aes(value, fill = Outcome)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~variable, scales = 'free_x')+
  geom_histogram(position = "identity") +
  #scale_x_continuous(breaks = seq(0,10,1))+
  scale_y_continuous(name = "Número de Muestras")+
  labs(title="Variables según clase",x = "Class", y = "Clump Thickness", colour = "Class")+
  ggtitle("Variables según clase") +
  scale_fill_discrete(name = "Outcome", labels = c("N", "R"))
#Distribucion de clases despues de haber eliminado las filas con datos nulos
nRecur <- nrow(subset(wpbc2.data, Outcome=="R"))
nNonrecur <- nrow(subset(wpbc2.data, Outcome=="N"))
#sumario:
sumario <- summary(wpbc2.data)
#Tabla con datos estadísticos de los valores más grandes/peor caso/w

#Modelo LM
bC <-wpbc2.data
bC$Out <- as.character(bC$Outcome)
#0 = N, 1 = R
bC$Out[bC$Out=="R"] = 1
bC$Out[bC$Out=="N"] = 0
bC$Out<-as.integer(bC$Out)
bC$Outcome<-NULL

#LM stepwise pero solo con valores worst
lmCompletoW<-lm(Out~Time+radius_w+texture_w+perimeter_w+
                 area_w+smoothness_w+compactness_w+
                 concavity_w+concave_points_w+ 
                 symmetry_w+ fractal_dimension_w+
                Tumor_size+ lymph_node_status,data=bC)
stepwiseBreastWorst <- step(lmCompletoW,direction="both")

#LM stepwise con todos los valores
stepwiseBreast <- step(lm(Out~.,data=bC),direction="both")
a<-summary(stepwiseBreast)
a
stargazer(stepwiseBreast, type = "latex")


