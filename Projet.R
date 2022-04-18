setwd("C:/Users/arnau/OneDrive - De Vinci/ESILV/Semestre 4/Mathématiques/Statistiques sous R")
data = read.csv("license.csv",sep=";",dec=",")
correlation = matrix( ncol = 2, nrow = 52, byrow = FALSE )
correlationFoot = list()
k = 1
TabLicencesPopulation = function(nomFede)
{
  Pop = list()
  Lic = list()
  for (i in 1:length(data$code_postal))
  {
    if (data$federation[i]==nomFede)
    {
      Lic = append(Lic,data$licences_en_2011[i])
      Pop = append(Pop,data$population_totale_2010[i])
    }
  }
  
  out = data.frame(
    Licences = as.numeric(Lic),
    Population = as.numeric(Pop)
  )
  return(out)
}

TabLicencesPopulationAge = function(nomFede,ageSup)
{
  Pop = list()
  Lic = list()
  for (i in 1:length(data$code_postal))
  {
    if (data$federation[i]==nomFede)
    {
      
      if(as.numeric(ageSup)==20)
      {
        Lic = append(Lic,data$moins_de_20_ans[i])
        Pop = append(Pop,data$population_de_moins_de_20_ans[i])
      }
      if(as.numeric(ageSup)==60)
      {
        Lic = append(Lic,data$entre_20_et_60_ans[i])
        Pop = append(Pop,data$population_de_20_a_60_ans[i])
      }
      if(as.numeric(ageSup)==0)
      {
        Lic = append(Lic,data$plus_de_60_ans[i])
        Pop = append(Pop,data$population_de_plus_de_60_ans[i])
      }
    }
  }
  
  out = data.frame(
    Licences = as.numeric(Lic),
    Population = as.numeric(Pop)
  )
  return(out)
}

TabLicencesVilles = function(nomFede)
{
  Ville = list()
  Lic = list()
  for (i in 1:length(data$code_postal))
  {
    if (data$federation[i]==nomFede)
    {
      Lic = append(Lic,data$licences_en_2011[i])
      Ville = append(Ville,data$commune[i])
    }
  }
  
  out = data.frame(
    Licences = as.numeric(Lic),
    Ville = as.character(Ville)
  )
  
  return(out)
}

Foot = TabLicencesPopulation("FF de football")
corFoot = cor(Foot$Licences,Foot$Population)
correlation[k,1] = "Foot"
correlation[k,2] = corFoot
correlationFoot = append(correlationFoot,corFoot)
plot(Foot$Population,Foot$Licences,main="Nombre de Licences de football en fonction du nombre d'habitants",xlab="Population",ylab="Nombre de Licences")

k=k+1
Boxe = TabLicencesPopulation("FF de boxe")
corBoxe = cor(Boxe$Licences,Boxe$Population)
correlation[k,1] = "Boxe"
correlation[k,2] = corBoxe
# correlationBoxe = append(correlationBoxe,corBoxe)
# plot(Boxe$Population,Boxe$Licences,main="Nombre de Licences de boxe en fonction du nombre d'habitants",xlab="Population",ylab="Nombre de Licences")
k=k+1
Golf = TabLicencesPopulation("FF de golf")
corGolf = cor(Golf$Licences,Golf$Population)
correlation[k,1] = "Golf"
correlation[k,2] = corGolf
# correlationGolf = append(correlationGolf,corGolf)
# plot(Golf$Population,Golf$Licences,main="Nombre de Licences de golf en fonction du nombre d'habitants",xlab="Population",ylab="Nombre de Licences")
k=k+1
Natation = TabLicencesPopulation("FF de natation")
corNatation = cor(Natation$Licences,Natation$Population)
correlation[k,1] = "Natation"
correlation[k,2] = corNatation
# correlation = append(correlation,corNatation)
# plot(Natation$Population,Natation$Licences,main="Nombre de Licences de natation en fonction du nombre d'habitants",xlab="Population",ylab="Nombre de Licences")
k=k+1
Aéronautique = TabLicencesPopulation("FF d'aéronautique")
corAéronautique = cor(Aéronautique$Licences,Aéronautique$Population)
correlation[k,1] = "Aéronautique"
correlation[k,2] = corAéronautique

k=k+1
Aïkido = TabLicencesPopulation("FF d'aïkido, d'aïkibudo et affinitaires")
corAïkido = cor(Aïkido$Licences,Aïkido$Population)
correlation[k,1] = "Aïkido"
correlation[k,2] = corAïkido

k=k+1
Muscu = TabLicencesPopulation("FF d'altérophilie, musculation, force athlétique et culturisme")
corMuscu = cor(Muscu$Licences,Muscu$Population)
correlation[k,1] = "Muscu"
correlation[k,2] = corMuscu

k=k+1
Athlétisme = TabLicencesPopulation("FF d'athlétisme")
corAthlétisme = cor(Athlétisme$Licences,Athlétisme$Population)
correlation[k,1] = "Athlétisme"
correlation[k,2] = corAthlétisme

k=k+1
Badminton = TabLicencesPopulation("FF de badminton")
corBadminton = cor(Badminton$Licences,Badminton$Population)
correlation[k,1] = "Badminton"
correlation[k,2] = corBadminton

k=k+1
Baseball = TabLicencesPopulation("FF de baseball et softball")
corBaseball = cor(Baseball$Licences,Baseball$Population)
correlation[k,1] = "Baseball"
correlation[k,2] = corBaseball

k=k+1
Basketball = TabLicencesPopulation("FF de basketball")
corBasketball = cor(Basketball$Licences,Basketball$Population)
correlation[k,1] = "Basketball"
correlation[k,2] = corBasketball

k=k+1
Bowling = TabLicencesPopulation("FF de bowling et de sports de quilles")
corBowling = cor(Bowling$Licences,Bowling$Population)
correlation[k,1] = "Bowling"
correlation[k,2] = corBowling

k=k+1
Boxe = TabLicencesPopulation("FF de boxe")
corBoxe = cor(Boxe$Licences,Boxe$Population)
correlation[k,1] = "Boxe"
correlation[k,2] = corBoxe

k=k+1
Canoë = TabLicencesPopulation("FF de canoë-kayak")
corCanoë = cor(Canoë$Licences,Canoë$Population)
correlation[k,1] = "Canoë"
correlation[k,2] = corCanoë

k=k+1
Danse = TabLicencesPopulation("FF de danse")
corDanse = cor(Danse$Licences,Danse$Population)
correlation[k,1] = "Danse"
correlation[k,2] = corDanse

k=k+1
Gymnastique = TabLicencesPopulation("FF de gymnastique")
corGymnastique = cor(Gymnastique$Licences,Gymnastique$Population)
correlation[k,1] = "Gymnastique"
correlation[k,2] = corGymnastique

k=k+1
Handball = TabLicencesPopulation("FF de handball")
corHandball = cor(Handball$Licences,Handball$Population)
correlation[k,1] = "Handball"
correlation[k,2] = corHandball

k=k+1
Hockey = TabLicencesPopulation("FF de hockey")
corHockey = cor(Hockey$Licences,Hockey$Population)
correlation[k,1] = "Hockey"
correlation[k,2] = corHockey

k=k+1
Hockey = TabLicencesPopulation("FF de hockey sur glace")
corHockey = cor(Hockey$Licences,Hockey$Population)
correlation[k,1] = "Hockey"
correlation[k,2] = corHockey

k=k+1
Judo = TabLicencesPopulation("FF de judo-jujitsu et disciplines associées")
corJudo = cor(Judo$Licences,Judo$Population)
correlation[k,1] = "Judo"
correlation[k,2] = corJudo

k=k+1
Karaté = TabLicencesPopulation("FF de karaté et disciplines associées")
corKaraté = cor(Karaté$Licences,Karaté$Population)
correlation[k,1] = "Karaté"
correlation[k,2] = corKaraté

k=k+1
Escalade = TabLicencesPopulation("FF de la montagne et de l'escalade")
corEscalade = cor(Escalade$Licences,Escalade$Population)
correlation[k,1] = "Escalade"
correlation[k,2] = corEscalade

k=k+1
Retraite = TabLicencesPopulation("FF de la retraite sportive")
corRetraite = cor(Retraite$Licences,Retraite$Population)
correlation[k,1] = "Retraite"
correlation[k,2] = corRetraite

k=k+1
Lutte = TabLicencesPopulation("FF de lutte")
corLutte = cor(Lutte$Licences,Lutte$Population)
correlation[k,1] = "Lutte"
correlation[k,2] = corLutte

k=k+1
Motocyclisme = TabLicencesPopulation("FF de motocyclisme")
corMotocyclisme = cor(Motocyclisme$Licences,Motocyclisme$Population)
correlation[k,1] = "Motocyclisme"
correlation[k,2] = corMotocyclisme

k=k+1
Pêche = TabLicencesPopulation("FF de pêche sportive au coup")
corPêche = cor(Pêche$Licences,Pêche$Population)
correlation[k,1] = "Pêche"
correlation[k,2] = corPêche

k=k+1
Pelote = TabLicencesPopulation("FF de pelote basque")
corPelote = cor(Pelote$Licences,Pelote$Population)
correlation[k,1] = "Pelote"
correlation[k,2] = corPelote

k=k+1
Pentathlon = TabLicencesPopulation("FF de pentathlon moderne")
corPentathlon = cor(Pentathlon$Licences,Pentathlon$Population)
correlation[k,1] = "Pentathlon"
correlation[k,2] = corPentathlon

k=k+1
Planeur = TabLicencesPopulation("FF de planeur ultra léger motorisé")
corPlaneur = cor(Planeur$Licences,Planeur$Population)
correlation[k,1] = "Planeur"
correlation[k,2] = corPlaneur

k=k+1
Roller = TabLicencesPopulation("FF de roller sports")
corRoller = cor(Roller$Licences,Roller$Population)
correlation[k,1] = "Roller"
correlation[k,2] = corRoller

k=k+1
Rugby = TabLicencesPopulation("FF de rugby")
corRugby = cor(Rugby$Licences,Rugby$Population)
correlation[k,1] = "Rugby"
correlation[k,2] = corRugby

k=k+1
SkiNautique = TabLicencesPopulation("FF de ski nautique")
corSkiNautique = cor(SkiNautique$Licences,SkiNautique$Population)
correlation[k,1] = "SkiNautique"
correlation[k,2] = corSkiNautique

k=k+1
Combat = TabLicencesPopulation("FF de sports de contact et disciplines associées")
corCombat = cor(Combat$Licences,Combat$Population)
correlation[k,1] = "Combat"
correlation[k,2] = corCombat

k=k+1
Surf = TabLicencesPopulation("FF de surf")
corSurf = cor(Surf$Licences,Surf$Population)
correlation[k,1] = "Surf"
correlation[k,2] = corSurf

k=k+1
Taekwondo = TabLicencesPopulation("FF de taekwondo et disciplines associées")
corTaekwondo = cor(Taekwondo$Licences,Taekwondo$Population)
correlation[k,1] = "Taekwondo"
correlation[k,2] = corTaekwondo

k=k+1
Tennis = TabLicencesPopulation("FF de tennis")
corTennis = cor(Tennis$Licences,Tennis$Population)
correlation[k,1] = "Tennis"
correlation[k,2] = corTennis

k=k+1
PingPong = TabLicencesPopulation("FF de tennis de table")
corPingPong = cor(PingPong$Licences,PingPong$Population)
correlation[k,1] = "PingPong"
correlation[k,2] = corPingPong

k=k+1
Tir = TabLicencesPopulation("FF de tir")
corTir = cor(Tir$Licences,Tir$Population)
correlation[k,1] = "Tir"
correlation[k,2] = corTir

k=k+1
Arc = TabLicencesPopulation("FF de tir a l'arc")
corArc = cor(Arc$Licences,Arc$Population)
correlation[k,1] = "Arc"
correlation[k,2] = corArc

k=k+1
Triathlon = TabLicencesPopulation("FF de triathlon")
corTriathlon = cor(Triathlon$Licences,Triathlon$Population)
correlation[k,1] = "Triathlon"
correlation[k,2] = corTriathlon

k=k+1
VolVoile = TabLicencesPopulation("FF de vol a voile")
corVolVoile = cor(VolVoile$Licences,VolVoile$Population)
correlation[k,1] = "VolVoile"
correlation[k,2] = corVolVoile

k=k+1
Vol = TabLicencesPopulation("FF de vol libre")
corVol = cor(Vol$Licences,Vol$Population)
correlation[k,1] = "Vol"
correlation[k,2] = corVol

k=k+1
Volley = TabLicencesPopulation("FF de volley-ball")
corVolley = cor(Volley$Licences,Volley$Population)
correlation[k,1] = "Volley"
correlation[k,2] = corVolley

k=k+1
Aviron = TabLicencesPopulation("FF des sociétés d'aviron")
corAviron = cor(Aviron$Licences,Aviron$Population)
correlation[k,1] = "Aviron"
correlation[k,2] = corAviron

k=k+1
Billard = TabLicencesPopulation("FF des sports de billard")
corBillard = cor(Billard$Licences,Billard$Population)
correlation[k,1] = "Billard"
correlation[k,2] = corBillard

k=k+1
Glace = TabLicencesPopulation("FF des sports de glace")
corGlace = cor(Glace$Licences,Glace$Population)
correlation[k,1] = "Glace"
correlation[k,2] = corGlace

k=k+1
Escrime = TabLicencesPopulation("FF d'escrime")
corEscrime = cor(Escrime$Licences,Escrime$Population)
correlation[k,1] = "Escrime"
correlation[k,2] = corEscrime

k=k+1
Adapté = TabLicencesPopulation("FF du sport adapté")
corAdapté = cor(Adapté$Licences,Adapté$Population)
correlation[k,1] = "Adapté"
correlation[k,2] = corAdapté

k=k+1
Automobile = TabLicencesPopulation("FF du sport automobile")
corAutomobile = cor(Automobile$Licences,Automobile$Population)
correlation[k,1] = "Automobile"
correlation[k,2] = corAutomobile

k=k+1
FFSU = TabLicencesPopulation("FF du sport universitaire")
corFFSU = cor(FFSU$Licences,FFSU$Population)
correlation[k,1] = "FFSU"
correlation[k,2] = corFFSU

k=k+1
Handisport = TabLicencesPopulation("FF handisport")
corHandisport = cor(Handisport$Licences,Handisport$Population)
correlation[k,1] = "Handisport"
correlation[k,2] = corHandisport

k=k+1
Maccabi = TabLicencesPopulation("FF Maccabi")
corMaccabi = cor(Maccabi$Licences,Maccabi$Population)
correlation[k,1] = "Maccabi"
correlation[k,2] = corMaccabi

Foot20 = TabLicencesPopulationAge("FF de football",20)
corFoot20 = cor(Foot20$Licences,Foot20$Population)
correlationFoot = append(correlationFoot,corFoot20)
plot(Foot20$Population,Foot20$Licences,main="Nombre de Licences de football en fonction du nombre d'habitants de moins de 20 ans",xlab="Population de 20 ans",ylab="Nombre de Licences")

Foot60 = TabLicencesPopulationAge("FF de football",60)
corFoot60 = cor(Foot60$Licences,Foot60$Population)
correlationFoot = append(correlationFoot,corFoot60)
plot(Foot60$Population,Foot60$Licences,main="Nombre de Licences de football en fonction du nombre d'habitants entre 20 et 60 ans",xlab="Population entre 20 et 60",ylab="Nombre de Licences")

FootPLus60 = TabLicencesPopulationAge("FF de football",0)
corFootPLus60 = cor(FootPLus60$Licences,FootPLus60$Population)
correlationFoot = append(correlationFoot,corFootPLus60)
plot(FootPLus60$Population,FootPLus60$Licences,main="Nombre de Licences de football en fonction du nombre d'habitants de plus de 60",xlab="Population de plus 60",ylab="Nombre de Licences")
correlationFoot = as.numeric(correlationFoot)

summary(Foot)

variancest = function(listeconsideree,x)
{
  retour = 0
  for(i in 1:length(listeconsideree))
  {
    retour = retour + listeconsideree[i]^2
  }
  retour = retour/length(listeconsideree)
  retour = retour - x^2
  return(retour)
}

varlicFoot = variancest(Foot$Licences,mean(Foot$Licences))
varlicFoot = (36/35)*varlicFoot
varpopFoot = variancest(Foot$Population,mean(Foot$Population))
varpopFoot = (36/35)*varpopFoot

etlicFoot = sqrt(varlicFoot)
etpopFoot = sqrt(varpopFoot)

boxplot(Foot$Licences,main = "Boîte à moustache des licenses de football")
boxplot(Foot$Population, main = "Boîte à moustache de la population")

#Distribution foot
ggdensity(Foot, x = "Licences", fill = "lightgray", title = "Licences",add = "mean", rug = TRUE) +
  scale_x_continuous(limits = c(0, 2000)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(Foot$Licences,na.rm = TRUE)
# >0 donc positivement asymétrique
#Foot$Licences = sqrt(Foot$Licences)
ggdensity(Foot, x = "Licences", fill = "lightgray", title = "Licences",add = "mean", rug = TRUE) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")


#test normalisation
Foot$Licences = (Foot$Licences-mean(Foot$Licences))/(sd(Foot$Licences))
skewness(Foot$Licences,na.rm = TRUE)
ggdensity(Foot, x = "Licences", fill = "lightgray", title = "Licences",add = "mean", rug = TRUE) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

