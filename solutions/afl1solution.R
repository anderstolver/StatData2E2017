### R-program til brug ved analyse af afleveringsopgave 1 ###
#############################################################

data<-read.table("afleveringsopgave1.txt",header=T)
data
attach(data)

### 1. boxplot ###

plot(kali~prod.gren)
boxplot(kali~prod.gren)

### 2. model for ensidet variansanalyse ###

### Y_i=alpha(prod.gren_i)+e_i, e_i uafh ~ N(0,sigma^2)

### 3. parameterestimater for ensidet ANOVA ###

model0<-lm(kali~prod.gren-1)
summary(model0) 

### s=1.973

### 4. test af hypotesen om, at der ikke er forskel på behandlingerne ###

model1<-lm(kali~1)
anova(model1,model0)

### 5. konfidensintervaller for parameterestimater ###

confint(model0)

### 6. LSD værdier for sammenligning af behandling A og B ###

### Brug formel s.27 i kompendiet:
###

### LSD=qt(0.975,19)*s*sqrt(1/4+1/4)

qt(0.975,19)*1.973*sqrt(1/4+1/4) ### =2.920

### 7. Forskel på behandling C og D ###

### Forskellige løsningsmetoder:

### a) beregn LSD ved brug af formel s. 27 i kompendiet

### b)

model0refit<-lm(kali~relevel(prod.gren,ref="C"))
summary(model0refit)

### viser, at der ikke er forskel på beh C og D: p=0.159

### 8. Test for om beh B, C og D er ens ###

### Et muligt løsningsforslag er at lave en ny faktor med værdien
### "TRUE" for observationer hørende til behandling A og "FALSE" for
### øvrige behandlingsværdier.

prod.grenA<-(prod.gren=="A")
model2<-lm(kali~prod.grenA)

anova(model2,model0) 

### testet forkastes, så det er ikke muligt at slå grupperne B, C og D sammen.



	
