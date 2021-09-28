library(tidyverse)
library(foreign)
library(compositions) #datos composicionales
library(gmodels)
library(ggplot2)
library(expss)
library(openxlsx)
library(descr)
library(xtable)
library(ggsci) # colores de graficas
#install.packages("janitor")
library(janitor)
#par(mar=c(5.1, 4.1, 4.1, 2.1)) rangos normales
setwd("/Users/Guieshuba/Dropbox/Tesis")
#+++++++++++++++++++++++++++++++++++++++lectura de información +++++++++++++++++++++++++++++++++++ ####
# Estados de la república 
rlcnedos <- read.csv(file.path("./AnálisisDatos/datos_salud/rlcnestados.csv")) 

# Catálogo de ocupación
cat_ocup <- read.csv(file.path("./AnálisisDatos/datos_salud/cat_ocup.csv")) 

# Catálogo de sexo 
cat_sexo <- read.csv(file.path("./AnálisisDatos/datos_salud/cat_sexo.csv")) 

#catálogo de edades
cat_edad <- read_csv("./AnálisisDatos/defunciones_2018/cat_edad.csv") %>% 
  rename( edad=edadq) %>% 
  select(EDAD_AGRU,edad) %>% 
  group_by(EDAD_AGRU) %>% 
  summarise(edad=mean(edad))

# **************************************************************************************************************####
#  
cat_ceme_agrup <- read.csv(file.path("./AnálisisDatos/defunciones_2018/cat_ceme_agrup.csv"))  

defun_cruzado <- read.csv(file.path("./AnálisisDatos/defunciones_2018/defun_cruzado.csv")) 

defunciones<- defun_cruzado %>% 
  mutate(EDO=abbreviate(ESTADO,3)) %>% 
  merge(rlcnedos,by.x="EDO",by.y="Clave2",all.x = TRUE) %>% 
  #merge(cat_ceme, by.x = "Nombrecorto", by.y = "Nombrecorto", all.x=TRUE) %>% 
  merge(cat_ceme_agrup, by.x = "Nombrecorto", by.y = "Nombrecorto", all.x=TRUE) %>% 
  select(SEXO,EDAD_AGRU,OCUPACION,Nombrecorto,PARENT,ENTIDAD_FEDERATIVA,NombreAgrupado) %>% 
  #select(SEXO,EDAD_AGRU,OCUPACION,Nombrecorto,PARENT,ENTIDAD_FEDERATIVA) %>% 
  mutate(EDAD_AGRU=as.numeric(EDAD_AGRU)) %>%
  merge(cat_ocup) %>%
  merge(cat_sexo) %>% 
  select(-OCUPACION)

#write.csv(defunciones,"./AnálisisDatos/datos_codigo/defunciones.csv",row.names = FALSE)
#write.csv(defunciones,"./AnálisisDatos/datos_codigo/defunciones2.csv",row.names = FALSE)
defunciones <- read.csv(file.path("./AnálisisDatos/datos_codigo/defunciones2.csv")) 

# **************************************************************************************************************####
edadxsexo <- prop.table(with(defunciones,table(EDAD_AGRU,sex), 1)) #porcentajes
## Tesis: cuadro: defxedadsex ####
edadxsexo1 <- as.data.frame(edadxsexo) %>%
  merge(cat_edad,all.x = TRUE) %>% 
  mutate(edadq=ifelse(edad<5,0,edad)) %>% 
  spread(sex,Freq) %>% 
  group_by(edadq) %>% 
  summarize(Hombres=sum(Hombre),Mujeres=sum(Mujer),NoEspecificado=sum(NE)) %>% 
  mutate(grupos_edad =ifelse(as.numeric(edadq) ==0, "Menores de 5 años", 
                             ifelse(as.numeric(edadq)>=5 & as.numeric(edadq)<=10,"5 a 14 años", 
                                    ifelse(as.numeric(edadq)==15,"15 a 19 años", 
                                           ifelse(as.numeric(edadq)>=20 & as.numeric(edadq)<=25, "20 a 29 años",
                                                  ifelse(as.numeric(edadq)>=30 & as.numeric(edadq)<=35, "30 a 39 años",
                                                         ifelse(as.numeric(edadq)>=40 & as.numeric(edadq)<=45, "40 a 49 años",
                                                                ifelse(as.numeric(edadq)>=50 & as.numeric(edadq)<=60, "50 a 64 años",
                                                                       ifelse(as.numeric(edadq)>=65, "65+","No especificado"))))))))) %>% 
  group_by(grupos_edad) %>%
  summarize(Hombres=sum(Hombres),Mujeres=sum(Mujeres),NoEspecificado=sum(NoEspecificado)) %>% 
  adorn_totals(where=c("row","col")) %>% 
  mutate(Hombres=round(Hombres*100,2),Mujeres=round(Mujeres*100,2),NoEspecificado=round(NoEspecificado*100,2),Total=round(Total*100,2))

print(xtable(edadxsexo1 %>% summarise_at(which(sapply(edadxsexo1, is.numeric) & names(edadxsexo1) != 'grupos_edad'), sum),digits=2),include.rownames = FALSE)
print(xtable(edadxsexo1,digits=2),include.rownames=FALSE)

# **************************************************************************************************************####
#### TOP DE DEFUNCIONES DE LA POBLACIÓN TOTAL EN PORCENTAJE ####
defun_top <- defunciones %>%
  #group_by(Nombrecorto,sex) %>%
  group_by(NombreAgrupado,sex) %>%
  summarize(Tot=n()) %>% 
  spread(sex,Tot) %>% 
  mutate(Hombre=ifelse(is.na(Hombre),0,Hombre),Mujer=ifelse(is.na(Mujer),0,Mujer),NE=ifelse(is.na(NE),0,NE)) %>% 
  adorn_totals(where=c("row","col")) %>% 
  adorn_percentages(denominator="col") %>% 
  slice_max(Total,n=11) %>% 
  slice(-1) %>% 
  mutate(Hombre=round(Hombre*100,2),Mujer=round(Mujer*100,2),NE=round(NE*100,2),Total=round(Total*100,2))
#write.csv(defun_top,"./AnálisisDatos/datos_codigo/defun_top.csv",row.names = FALSE)
#write.csv(defun_top,"./AnálisisDatos/datos_codigo/defun_top2.csv",row.names = FALSE)
defun_top <- read.csv(file.path("./AnálisisDatos/datos_codigo/defun_top2.csv")) 

# top defunciones por entidad
defun_ent_tot<- defunciones %>% 
  #group_by(Nombrecorto,ENTIDAD_FEDERATIVA) %>%
  group_by(NombreAgrupado,ENTIDAD_FEDERATIVA) %>% 
  summarize(Total=n()) %>% 
  #filter(!Nombrecorto %in% defun_top$Nombrecorto) %>% 
  filter(!NombreAgrupado %in% defun_top$NombreAgrupado) %>% 
  group_by(ENTIDAD_FEDERATIVA) %>% 
  summarise(Otras=sum(Total))

defun_ent_top <- defunciones %>% 
  # group_by(Nombrecorto,ENTIDAD_FEDERATIVA) %>%
  group_by(NombreAgrupado,ENTIDAD_FEDERATIVA) %>%
  summarize(Total=n()) %>% 
  #filter(Nombrecorto %in% defun_top$Nombrecorto) %>% # %in% = que esté en la lista
  filter(NombreAgrupado %in% defun_top$NombreAgrupado) %>% # %in% = que esté en la lista
  #pivot_wider(names_from = Nombrecorto, values_from= Total) %>% 
  pivot_wider(names_from = NombreAgrupado, values_from= Total) %>% 
  merge(defun_ent_tot)  

#write.csv(defun_ent_top,"./AnálisisDatos/datos_codigo/defun_ent_top.csv",row.names = FALSE)

# top defunciones por edad 
defun_edad_top <- defunciones %>% 
  merge(cat_edad,all.x = TRUE) %>% 
  mutate(edadq=ifelse(edad<5,0,edad)) %>% 
  #group_by(sex,edadq,Nombrecorto) %>%
  group_by(sex,edadq,NombreAgrupado) %>%
  summarize(Total=n()) %>% 
  #filter(Nombrecorto %in% defun_top$Nombrecorto) %>% # %in% = que esté en la lista
  #pivot_wider(names_from = Nombrecorto, values_from= Total)
  filter(NombreAgrupado %in% defun_top$NombreAgrupado) %>% # %in% = que esté en la lista
  pivot_wider(names_from = NombreAgrupado, values_from= Total)

defun_edad_top[26,2] <- 200 #NO ESPECIFICADO
#defun_edad_top[51,2] <- 200
defun_edad_top[52,2] <- 200
#defun_edad_top[66,2] <- 200 
defun_edad_top[68,2] <- 200 

defun_edad_top[is.na(defun_edad_top)] <- 0

#write.csv(defun_edad_top,"./AnálisisDatos/datos_codigo/defun_edad_top.csv",row.names = FALSE)
#write.csv(defun_edad_top,"./AnálisisDatos/datos_codigo/defun_edad_top2.csv",row.names = FALSE)
defun_edad_top <- read.csv(file.path("./AnálisisDatos/datos_codigo/defun_edad_top2.csv")) 

# TOPDEFUNBAR ####
graf <- #ggplot(defun_top,(aes(x=reorder(Nombrecorto,Total),y=Total,fill=Nombrecorto)))+ 
  ggplot(defun_top,(aes(x=reorder(NombreAgrupado,Total),y=Total,fill=NombreAgrupado)))+ 
  geom_bar(stat="identity",show.legend = FALSE)+
  geom_text(aes(label = Total), hjust=1.5, colour="black", size=3.5)+
  labs(x="Causa de defunción",y="Porcentaje")+
  coord_flip() +
  theme_classic()
print(graf)

#ggsave(filename="./TesisTex/Fig/topdefunbar.jpeg",graf)
ggsave(filename="./TesisTex/Fig/topdefunbar_agrup.jpeg",graf)
apply(defun_top[,2:5],2,sum) # suma de la columna
apply(defun_top[1:5,2:5],2,sum) # suma de la columna del top 5 

#### TOP DEFUNCIONES POR SEXO EN PORCENTAJE DIVERGING CHART ####
# TOPDEFUNXSEXO
graf <- defun_top %>% select(-NE,-Total) %>% 
  gather("Sexo","Total",2:3) %>% 
  ggplot( aes(x = ifelse(test = Sexo == "Hombre", yes = Total, no = -Total), 
              y = reorder(NombreAgrupado,Total), fill = Sexo)) +
  #ggplot( aes(x = ifelse(test = Sexo == "Hombre", yes = Total, no = -Total), 
  #                    y = reorder(Nombrecorto,Total), fill = Sexo)) +
  geom_col() +
  geom_text(aes(label = Total), colour="black", size=3.5)+
  scale_x_continuous(labels = abs) +
  labs(x = "Porcentaje", y= "Causa de defunción")
print(graf)
#ggsave(filename="./TesisTex/Fig/topdefunxsexo_agrup.jpeg",graf)

# **************************************************************************************************************####
##### GRÁFICAS DE ENFERMEDADES POR GPOS DE EDAD Y SEXO 
cat_edad <- cat_edad %>% 
  mutate(grupos_edad =ifelse(is.na(edad),"No especificado",
                             ifelse(as.numeric(edad) <=4, "Menores de 5 años",
                                    ifelse(as.numeric(edad)>=5 & as.numeric(edad)<=15,"5 a 19 años", 
                                           ifelse(as.numeric(edad)>=20 & as.numeric(edad)<=60, "20 a 64 años","65+")))))

aux <- defunciones %>% merge(cat_edad,all.x = TRUE) %>% 
  filter(sex=="Hombre") %>% 
  #group_by(grupos_edad,Nombrecorto) %>% 
  group_by(grupos_edad,NombreAgrupado) %>% 
  summarize(Tot=n())

aux2 <- defunciones %>% merge(cat_edad,all.x = TRUE) %>% 
  filter(sex=="Hombre") %>% 
  group_by(grupos_edad,NombreAgrupado) %>% 
  summarize(Tot=n()) %>% 
  adorn_percentages(denominator = "col") %>%
  mutate(Tot=Tot*100) %>% 
  slice_max(Tot,n=5) 

tablah <- aux %>% merge(aux %>%summarize(totedad=sum(Tot))) %>% 
  mutate(Pctg=Tot/totedad*100) %>% 
  group_by(grupos_edad) %>% 
  slice_max(Pctg,n=5) %>% 
  select(-Tot,-totedad) %>% 
  merge(aux2,sort=F) 

aux <- defunciones %>% merge(cat_edad,all.x = TRUE) %>% 
  filter(sex=="Mujer") %>% 
  #group_by(grupos_edad,Nombrecorto) %>% 
  group_by(grupos_edad,NombreAgrupado) %>% 
  summarize(Tot=n()) 

aux2 <- defunciones %>% merge(cat_edad,all.x = TRUE) %>% 
  filter(sex=="Mujer") %>% 
  group_by(grupos_edad,NombreAgrupado) %>% 
  summarize(Tot=n()) %>% 
  adorn_percentages(denominator = "col") %>%
  mutate(Tot=Tot*100) %>% 
  slice_max(Tot,n=5) 

tablam <- aux %>% merge(aux %>%summarize(totedad=sum(Tot))) %>% 
  mutate(Pctg=Tot/totedad*100) %>% 
  group_by(grupos_edad) %>% 
  slice_max(Pctg,n=5) %>% 
  select(-Tot,-totedad) %>% 
  merge(aux2,sort=F)

orden <- data.frame(num=seq(1,25,1),
                    orden= c(seq(11,15,1),seq(6,10,1),seq(16,20,1),seq(1,5,1),seq(21,25,1))) #orden de los grupos de edad

tabla <- bind_cols(tablah,tablam) %>% 
  select(-grupos_edad...5)

tabla <- bind_cols(tabla,orden) %>% 
  arrange(orden)

round(rbind(c(sum(tabla$Pctg...3[1:5]),sum(tabla$Tot...4[1:5]),sum(tabla$Pctg...6[1:5]),sum(tabla$Tot...7[1:5])),# menores 5 años
            c(sum(tabla$Pctg...3[6:10]),sum(tabla$Tot...4[6:10]),sum(tabla$Pctg...6[6:10]),sum(tabla$Tot...7[6:10])), # 5 a 19
            c(sum(tabla$Pctg...3[11:15]),sum(tabla$Tot...4[11:15]),sum(tabla$Pctg...6[11:15]),sum(tabla$Tot...7[11:15])), #
            c(sum(tabla$Pctg...3[16:20]),sum(tabla$Tot...4[16:20]),sum(tabla$Pctg...6[16:20]),sum(tabla$Tot...7[16:20])), 
            c(sum(tabla$Pctg...3[21:25]),sum(tabla$Tot...4[21:25]),sum(tabla$Pctg...6[21:25]),sum(tabla$Tot...7[21:25]))),2) # no esp

print(xtable(tabla[,2:7]),include.rownames=FALSE)

# **************************************************************************************************************####
# Tasas de mortalidad
##### las defunciones son por lugar de ocurrencia en la entidad
#población total de méxico ((INEGI: https://www.inegi.org.mx/app/descarga/?t=123&ag=00) ####
pob18 <- read_csv("./AnálisisDatos/datos_poblacion/pob_2018.csv") # 
pobxgpoedad_aj <- read_csv("./AnálisisDatos/datos_poblacion/pob_2018_gpoedadaj.csv") %>% select(-Pob18_pob10,-factor) 

# pobxgpoedad_aj %>% group_by(desc_entidad) %>% summarize(tot=sum(Pob18ajust)) %>% adorn_totals(where="row")
# pobxgpoedad_aj %>% group_by(Sexo) %>% summarize(tot=sum(Pob18ajust)) %>% adorn_totals(where="row")
# pobxgpoedad_aj %>% group_by(grupos_edad) %>% summarize(tot=sum(Pob18ajust)) %>% adorn_totals(where="row")
# 

# tasa de mortalidad por entidad
aux <- pobxgpoedad_aj %>% group_by(desc_entidad) %>% summarize(pob=sum(Pob18ajust)) 

defun_mort_entidad <- defunciones %>% group_by(ENTIDAD_FEDERATIVA) %>% summarize(defun=n()) %>% 
  merge(aux, by.x="ENTIDAD_FEDERATIVA",by.y = "desc_entidad", all.x = TRUE) %>% 
  mutate(defperc=defun/pob*1000)

graf <- defun_mort_entidad %>% select(ENTIDAD_FEDERATIVA,defperc) %>% 
  ggplot( aes(x = reorder(ENTIDAD_FEDERATIVA,defperc), 
              y = defperc,fill=ENTIDAD_FEDERATIVA)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = round(defperc,2)), colour="black", size=3.5)+
  labs(x ="Estado", y= "Tasa de mortalidad por cada mil habitantes")+
  theme_classic()+
  theme(legend.position = "none")+
  coord_flip()
print(graf)

#write.csv(defun_mort_entidad,"./AnálisisDatos/datos_codigo/defun_mort_entidad.csv",row.names = FALSE)
defun_mort_entidad <- read.csv(file.path("./AnálisisDatos/datos_codigo/defun_mort_entidad.csv")) 

print(xtable(defun_mort_entidad),include.rownames=FALSE) # tasa de mortalidad por entidad
#ggsave(filename="./TesisTex/Fig/mort_ent.jpeg",graf)

# tasa de mortalidad por sexo
aux <- pobxgpoedad_aj %>% group_by(Sexo) %>% summarize(pob=sum(Pob18ajust)) %>% rename(sex="Sexo")

defun_mort_sexo <- defunciones %>% group_by(sex) %>% summarize(defun=n()) %>% 
  merge(aux, all.x = TRUE) %>% 
  mutate(defperc=round(defun/pob*1000,1))

print(xtable(defun_mort_sexo),include.rownames=FALSE) # tasa de mortalidad por sexo
#write.csv(defun_mort_sexo,"./AnálisisDatos/datos_codigo/defun_mort_sexo.csv",row.names = FALSE)
defun_mort_sexo <- read.csv(file.path("./AnálisisDatos/datos_codigo/defun_mort_sexo.csv")) 

# tasa de mortalidad por entidad y sexo
aux <- pobxgpoedad_aj %>% group_by(desc_entidad,Sexo) %>% summarize(pob=sum(Pob18ajust)) %>% rename(sex="Sexo") %>% 
  unite(col="ID",desc_entidad:sex,sep="_") 

defun_mort_entsexo <- defunciones %>% group_by(ENTIDAD_FEDERATIVA,sex) %>% summarize(defun=n()) %>% 
  unite(col="ID",ENTIDAD_FEDERATIVA:sex,sep="_") %>% 
  merge(aux, all.x = TRUE) %>% 
  separate(col="ID",into=c("ENTIDAD_FEDERATIVA","sexo"),sep="_") %>% 
  mutate(defperc=round(defun/pob*1000,1)) %>% 
  slice_max(defperc,n=93)
#write.csv(defun_mort_entsexo,"./AnálisisDatos/datos_codigo/defun_mort_entsexo.csv",row.names = FALSE)
defun_mort_entsexo <- read.csv(file.path("./AnálisisDatos/datos_codigo/defun_mort_entsexo.csv")) 


graf <- defun_mort_entsexo %>% 
  ggplot( aes(x = reorder(ENTIDAD_FEDERATIVA,defperc), 
              y = defperc,fill=sexo)) +
  geom_bar(stat="identity",position=position_dodge())+
  geom_text(aes(label = round(defperc,1)), colour="black", size=3.5)+
  labs(x ="Estado", y= "Tasa de mortalidad por cada mil habitantes")+
  #theme(legend.position = "none")+
  coord_flip()+
  theme_classic()
print(graf)
#ggsave(filename="./TesisTex/Fig/mort_entsexo.jpeg",graf)

# tasa de mortalidad por sexo y grupos de edad 
aux <- pobxgpoedad_aj %>% group_by(Sexo,grupos_edad) %>% summarize(pob=sum(Pob18ajust)) %>% 
  unite(col="ID",Sexo,grupos_edad,sep="_") %>%
  select(ID,pob)

defun_mort_sexogpo <- defunciones %>%  merge(cat_edad,all.x = TRUE) %>% 
  group_by(sex,grupos_edad) %>% 
  summarize(defun=n()) %>% 
  unite(col="ID",sex:grupos_edad,sep="_") %>% 
  merge(aux, all.x = TRUE) %>% 
  separate(col="ID",into=c("sexo","grupos_edad"),sep="_") %>%
  mutate(defperc=round(defun/pob*1000,1)) %>% 
  filter(sexo!="NE")
#write.csv(defun_mort_sexogpo,"./AnálisisDatos/datos_codigo/defun_mort_sexogpo.csv",row.names = FALSE)
defun_mort_sexogpo <- read.csv(file.path("./AnálisisDatos/datos_codigo/defun_mort_sexogpo.csv")) 

graf <- defun_mort_sexogpo %>% mutate(grupos_edad=factor(grupos_edad,levels=c("No especificado","65+","20 a 64 años","5 a 19 años","Menores de 5 años"))) %>% 
  ggplot( aes(x = grupos_edad, 
              y = defperc,fill=sexo)) +
  geom_bar(stat="identity",position=position_dodge())+ #POSITION DODGE PARA QUE SALGA SEPARADO HOMBRE Y MUER
  geom_text(aes(label = round(defperc,2)), colour="black", position = position_dodge(0.9))+
  labs(x ="Grupo_edad", y= "Tasa de mortalidad por cada mil habitantes")+
  #theme(legend.position = "none")+
  coord_flip()+
  theme_bw()
print(graf)
#ggsave(filename="./TesisTex/Fig/mort_sexogpo.jpeg",graf)

orden <- data.frame(num=seq(1,10,1),
                    orden= c(3,2,4,1,5,8,7,9,6,10))
tabla <- bind_cols(defun_mort_sexogpo,orden) %>% 
  arrange(orden) %>% 
  select(-defun,-pob,-num,-orden) %>% 
  pivot_wider(names_from = sexo, values_from= defperc)

print(xtable(tabla))

# tasa de mortalidad por sexo y grupos de edad y enfermedades 
defun_mort_h_gpoenf <- defunciones %>%  merge(cat_edad,all.x = TRUE) %>% 
  filter(sex=="Hombre") %>% 
  #group_by(sex,grupos_edad,Nombrecorto) %>% 
  group_by(sex,grupos_edad,NombreAgrupado) %>% 
  summarize(defun=n()) %>% 
  unite(col="ID",sex:grupos_edad,sep="_") %>% 
  merge(aux, all.x = TRUE) %>% 
  separate(col="ID",into=c("sexo","grupos_edad"),sep="_") %>%
  mutate(defperc=defun/pob*1000) %>% 
  group_by(grupos_edad) %>% 
  slice_max(defperc,n=5)

defun_mort_m_gpoenf <- defunciones %>%  merge(cat_edad,all.x = TRUE) %>% 
  filter(sex=="Mujer") %>% 
  #group_by(sex,grupos_edad,Nombrecorto) %>% 
  group_by(sex,grupos_edad,NombreAgrupado) %>% 
  summarize(defun=n()) %>% 
  unite(col="ID",sex:grupos_edad,sep="_") %>% 
  merge(aux, all.x = TRUE) %>% 
  separate(col="ID",into=c("sexo","grupos_edad"),sep="_") %>%
  mutate(defperc=defun/pob*1000) %>% 
  group_by(grupos_edad) %>% 
  slice_max(defperc,n=5) 

orden <- data.frame(num=seq(1,25,1),
                    orden= c(seq(11,15,1),seq(6,10,1),seq(16,20,1),seq(1,5,1),seq(21,25,1)))
tabla <- bind_cols(defun_mort_h_gpoenf,defun_mort_m_gpoenf) %>% 
  #select(grupos_edad...2,Nombrecorto...3,defperc...6,Nombrecorto...9,defperc...12)
  select(grupos_edad...2,NombreAgrupado...3,defperc...6,NombreAgrupado...9,defperc...12)

tabla <- bind_cols(tabla,orden) %>% 
  arrange(orden) 

print(xtable(tabla[c(2:5)]),include.rownames=FALSE)

round(rbind(c(sum(tabla$defperc...3[1:5]),sum(tabla$defperc...5[1:5])), # menores 5 años 1:5
            c(sum(tabla$defperc...3[6:10]),sum(tabla$defperc...5[6:10])), # 5 a 19  6:10
            c(sum(tabla$defperc...3[11:15]),sum(tabla$defperc...5[11:15])), 
            c(sum(tabla$defperc...3[16:20]),sum(tabla$defperc...5[16:20])),
            c(sum(tabla$defperc...3[21:25]),sum(tabla$defperc...5[21:25]))),2) 

# par(mfrow = c(3,2)) #c(renglones,columnas)
# plot(defun_edad_top$edadq[1:21],hombres_edad_top[1:21,1],pch=1,type="p", xlab="edad",ylab="composición",main="Enf. corazón")
# plot(defun_edad_top$edadq[1:21],hombres_edad_top[1:21,2],pch=1,type="p", xlab="edad",ylab="composición",main="Enf. endocrinas")
# plot(defun_edad_top$edadq[1:21],hombres_edad_top[1:21,3],pch=1,type="p", xlab="edad",ylab="composición",main="Tumores")
# plot(defun_edad_top$edadq[1:21],hombres_edad_top[1:21,4],pch=1,type="p", xlab="edad",ylab="composición",main="Enf. aparato digestivo")
# plot(defun_edad_top$edadq[1:21],hombres_edad_top[1:21,5],pch=1,type="p", xlab="edad",ylab="composición",main="Enf. respiratorias")
# ggsave(filename="./TesisTex/Fig/topdefunbar.jpeg",graf)

# # **************************************************************************************************************####
# CORRER DEFUNCIONES POR ESTADO , CON POBLACIÓN, OBESIDAD, GASTOS SALUD, MEDICOS, ENFERMEROS. 
#gasto en salud (info de transparencia presupuestaria)
gtosalud18 <- read.csv(file.path("./AnálisisDatos/dAtos_salud/gtosalud_2018.csv")) #datos en miles de millones


# #número de médicos por estado (sistema de información de la secretaría de salud ) #### 
medicos <- read_csv("./AnálisisDatos/datos_salud/medicos.csv") # 
personal <- read_csv("./AnálisisDatos/datos_salud/personal.csv") # 
establecimientos <-  read_csv("./AnálisisDatos/datos_salud/establecimientos.csv") # 

medicos_tot18 <- medicos %>%  filter(Año==2018) %>% select(ENTIDAD_FEDERATIVA,Total) %>% 
  rename(Medicos=Total)
enfermeros_18 <- personal %>%  filter(Año==2018) %>% 
  select(ENTIDAD_FEDERATIVA,`Enfermeras generales y especialistas`) %>% 
  rename(Enfermeros=`Enfermeras generales y especialistas`)

establec_18 <- establecimientos %>%  filter(Año==2018) %>% 
  select(ENTIDAD_FEDERATIVA,Total) %>% 
  rename(Establecimientos=Total)

#OBESIDAD
obesidad <- read_csv("./AnálisisDatos/datos_salud/obesidad.csv") # 
ind_obesidad <- obesidad %>% # nùmero de obesos/
  select(ENTIDAD_FEDERATIVA,FactorRiesgo) %>% 
  group_by(ENTIDAD_FEDERATIVA) %>% 
  summarise(Obesidad=sum(FactorRiesgo)/n()) 

ind_obesidad <- obesidad %>% # nùmero de obesos/
  select(rango_edad,FactorRiesgo) %>% 
  group_by(rango_edad) %>% 
  summarise(Obesidad=sum(FactorRiesgo)/n()) 

#maps of mexico diegovalle https://www.diegovalle.net/mxmaps/
install.packages("backports")
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("diegovalle/mxmaps")

## mapas de méxico para indices de obesidad, gasto en salud, proporción de enfermeros, médicos. y defunciones
library("mxmaps")
defun_otrasestad <- defunciones %>%
  merge(cat_edad,all.x = TRUE) %>% 
  group_by(grupos_edad,ENTIDAD_FEDERATIVA) %>% 
  summarize(Tot=n()) %>% 
  pivot_wider(names_from = grupos_edad, values_from= Tot) %>% 
  merge(gtosalud18) %>% rename(Gastosalud="gto") %>% mutate(Gastosalud=Gastosalud*1e6) %>%
  # merge(medicos_tot18) %>% 
  # merge(enfermeros_18) %>% 
  # merge(establec_18) %>%
  # merge(ind_obesidad) %>% 
  merge(pob18) 
#write.csv(defun_otrasestad,"./AnálisisDatos/datos_codigo/defun_otrasestad.csv",row.names = FALSE)

defun_otrasestad <- defun_otrasestad %>%   # la población está en unidades 
  mutate(across(`20 a 64 años`:`No especificado`,~./Pob18*1000)) %>%  #1 por cada 10 mil
  mutate(Gastosalud=Gastosalud/Pob18) %>% #1 por cada 10 mil 
  #mutate(across(Medicos:Establecimientos,~./Pob18*1000)) %>%  #1 por cada 10 mil  
  merge(df_mxstate[c(1:2)],by.x="ENTIDAD_FEDERATIVA",by.y="state_name",all.x =TRUE )

aux <- defun_otrasestad %>% merge(defun_mort_entidad) %>%  slice_max(order_by = Gastosalud,n=32) %>% select(1,7,8,12)
print(xtable(aux), format.args =list(big.mark = ",", decimal.mark = "."))


defun_otrasestad$value <- defun_otrasestad$Pob18/1000000
mxstate_choropleth(defun_otrasestad,
                   num_colors = 9,
                   title = "Total población, por estado",
                   legend= "millones de personas")

defun_otrasestad$value <- defun_otrasestad$Obesidad
mxstate_choropleth(defun_otrasestad,
                   num_colors = 4,
                   title = "índice de obesidad",
                   legend = "%")
# MAPA DE GASTO EN SALUD PER CAPITA ####
defun_otrasestad$value <- defun_otrasestad$Gastosalud
mxstate_choropleth(defun_otrasestad,
                   num_colors = 9,
                   title = "Gasto en salud",
                   legend = "Gasto per cápita",
) 
#theme(legend.justification=c(0,0))

ggsave(filename="./TesisTex/Fig/mapa_gtosalud.jpeg",mapa)

# MAPA DE medicos ####
defun_otrasestad$value <- defun_otrasestad$Medicos
mxstate_choropleth(defun_otrasestad,
                   num_colors = 3,
                   title = "Médicos por persona en cada entidad",
                   legend = "cifras en 1 de cada 10 mil habitantes")
ggsave(filename="./TesisTex/Fig/mapa_.jpeg",mapa)

library(leaflet) # for colorNumeric
magma <- c("#000004FF", "#1D1146FF", "#50127CFF", "#822681FF", "#B63779FF", 
           "#E65163FF", "#FB8761FF", "#FEC387FF", "#FCFDBFFF")
pal <- colorNumeric(palette(), domain = defun_otrasestad$value,reverse = TRUE)
mxstate_leaflet(defun_otrasestad,
                pal,
                ~ pal(value), 
                ~ sprintf("Estado: %s<br/>Gasto: %s",
                          ENTIDAD_FEDERATIVA, round(value,2))) %>%
  addLegend(position = "bottomright", 
            pal = pal, 
            values = defun_otrasestad$value,
            title = "Gasto<br>per cápita",
            labFormat = labelFormat(prefix = "$")) %>%
  addProviderTiles("CartoDB.Positron")

########################################## 
# otras gráficas de enfermeros y medicos
defun_otrasestad <- defunciones %>%
  group_by(ENTIDAD_FEDERATIVA) %>% 
  summarize(Defunciones=n()) %>% 
  merge(gtosalud18) %>% rename(Gastosalud="gto") %>% mutate(Gastosalud=Gastosalud*1e6) %>%
  merge(medicos_tot18) %>% 
  merge(enfermeros_18) %>% 
  merge(establec_18) %>%
  merge(pob18) 

defun_otrasestad %>% adorn_totals(where="row") %>% mutate(razon=Medicos/Enfermeros) %>% slice_min(order_by = razon, n=18) #%>% 

graf <- defun_otrasestad %>% mutate(razon=Medicos/Enfermeros) %>%
  ggplot(aes(x=reorder(ENTIDAD_FEDERATIVA,razon),y=razon)) +
  geom_point()+
  coord_flip()+
  labs(x="Entidad",y="Razón")
print(graf)
ggsave(filename="./TesisTex/Fig/razonmedvsenf.jpeg",graf)


defun_otrasestad <- defun_otrasestad %>% # la población está en unidades 
  adorn_totals(where="row") %>% 
  mutate(across(Defunciones,~./Pob18*1000)) %>%  #1 por cada 1 mil
  mutate(Gastosalud=Gastosalud/Pob18) %>% #1 por cada 1 mil 
  mutate(across(Medicos:Establecimientos,~./Pob18*1000)) %>%   #1 por cada 1 mil  
  select(ENTIDAD_FEDERATIVA,Medicos,Enfermeros,Establecimientos,Defunciones) %>% 
  pivot_longer(cols=c(Medicos:Establecimientos), names_to="RecursosSalud", values_to="valores")

graf <- defun_otrasestad %>% 
  ggplot(mapping=aes(x=reorder(ENTIDAD_FEDERATIVA,valores),y=valores,color=RecursosSalud, size=Defunciones))+
  geom_point()+
  coord_flip() +
  labs(x="Entidad",y="Recursos")

print(graf)
ggsave(filename="./TesisTex/Fig/medicos_defun.jpeg",graf)

#graf gasto en salud
graf <- defun_otrasestad %>% 
  select(ENTIDAD_FEDERATIVA,Gastosalud,Defunciones,Pob18) %>% 
  ggplot(mapping=aes(x=reorder(ENTIDAD_FEDERATIVA,Gastosalud),y=Gastosalud,size=Defunciones))+
  geom_point()+
  coord_flip() +
  labs(x="Entidad",y="Gasto en salud por cada 10 mil habitantes")

print(graf)
ggsave(filename="./TesisTex/Fig/gastosalud.jpeg",graf)

# obesidad
# 
ind_obesidad <- obesidad %>% # nùmero de obesos/
  select(ENTIDAD_FEDERATIVA,quinquenios,FactorRiesgo) %>%
  rename("grupos_edad"="quinquenios") %>% 
  group_by(ENTIDAD_FEDERATIVA,grupos_edad) %>% 
  summarise(Obesidad=sum(FactorRiesgo)/n()) 

defun_otrasestad <- defunciones %>%
  #filter(NombreAgrupado %in% defun_top$NombreAgrupado[1:5]) %>% 
  #filter(NombreAgrupado=="Enf. del corazón") %>% 
  merge(cat_edad,all.x = TRUE) %>% 
  group_by(ENTIDAD_FEDERATIVA,grupos_edad) %>% 
  summarize(Defunciones=n()) %>% 
  merge(pob18) %>% 
  #mutate(Defunciones=Defunciones/Pob18*10000) %>% #1 por cada 10 mil
  merge(ind_obesidad) %>% 
  select(-Pob18)


library(ggrepel)
defun_otrasestad %>% 
  filter(grupos_edad!="Menores de 5 años") %>% 
  filter(grupos_edad!="5 a 19 años") %>% 
  ggplot(aes(x=Obesidad,y=Defunciones,label=ENTIDAD_FEDERATIVA,color=grupos_edad))+
  geom_point()+
  coord_cartesian(ylim=c(0,15000))+
  #geom_text_repel()+
  labs(x="Indice de obesidad",y="Defunciones")

defun_otrasestad %>% 
  filter(grupos_edad=="Menores de 5 años" | grupos_edad== "5 a 19 años") %>% 
  ggplot(aes(x=Obesidad,y=Defunciones,label=ENTIDAD_FEDERATIVA,color=grupos_edad))+
  geom_point()+
  geom_text_repel()+
  labs(x="Indice de obesidad",y="Defunciones")

defun_otrasestad %>% 
  ggplot(aes(x=Obesidad,y=Defunciones,label=ENTIDAD_FEDERATIVA,color=grupos_edad))+
  geom_point()+
  labs(x="Indice de obesidad",y="Defunciones")

