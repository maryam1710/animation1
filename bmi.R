#farakhani dadeha.
BMI = read.table("c:/Users/a/BMI_IRAN.txt", header = TRUE, sep = " ")
#ghasd darim shakhes bmi ra dar moghable motaghayerhay gosasteh rasm konim.
#ebteda shakhes jensiat ra mored barresi ghar midahim.
#baray rasm nemodar histogram sharti, baste lattice ra farakhani mikonim.
library(lattice)
histogram(~ bmi|factor(sex), data = BMI)
#b hamin tartib motaghayerhay gosaste digar ra mored barresi gharar midahim.
histogram(~ bmi|factor(city), data = BMI)
histogram(~ bmi|factor(time), data = BMI)
histogram(~ bmi|factor(eduf), data = BMI)
histogram(~ bmi|factor(edum), data = BMI)
histogram(~ bmi|factor(obesity), data = BMI)
#hal mikhahim nemodar baravord konandeh kernel ra rasm konim.
densityplot(~ bmi | factor(sex), data = BMI, plot.points = FALSE , ref = TRUE)
densityplot(~ bmi | factor(city), data = BMI, plot.points = FALSE , ref = TRUE)
densityplot(~ bmi | factor(time), data = BMI, plot.points = FALSE , ref = TRUE)
densityplot(~ bmi | factor(eduf), data = BMI, plot.points = FALSE , ref = TRUE)
densityplot(~ bmi | factor(edum), data = BMI, plot.points = FALSE , ref = TRUE)
densityplot(~ bmi | factor(obesity), data = BMI, plot.points = FALSE , ref = TRUE)
#rasm nemodar kernel dar yek ghab.
densityplot(~ bmi, data = BMI, groups = sex, plot.points = FALSE, ref = TRUE ,
auto.key = list(columns = 2))
densityplot(~ bmi, data = BMI, groups = time, plot.points = FALSE, ref = TRUE ,
auto.key = list(columns = 2))
densityplot(~ bmi, data = BMI, groups = city, plot.points = FALSE, ref = TRUE ,
auto.key = list(columns = 2))
densityplot(~ bmi, data = BMI, groups = eduf, plot.points = FALSE, ref = TRUE ,
auto.key = list(columns = 4))
densityplot(~ bmi, data = BMI, groups = edum, plot.points = FALSE, ref = TRUE ,
auto.key = list(columns = 4))
densityplot(~ bmi, data = BMI, groups = obesity, plot.points = FALSE, ref = TRUE ,
auto.key = list(columns = 2))
#rasm nemodar 2 moteghayere. baray rasm in nemodar az tabe "xyplot" estefadeh mikonim.
#in tabe nemodar parakandegi 2 moteghayere ra rasm mikonad.
xyplot(bmi ~ fat | sex , data = BMI, layout = c(1, 2))
xyplot(bmi ~ fat | time , data = BMI, layout = c(1, 4))
xyplot(bmi ~ fat | city , data = BMI, layout = c(1, 2))
xyplot(bmi ~ fat | eduf , data = BMI)
xyplot(bmi ~ fat | edum , data = BMI)
xyplot(bmi ~ fat | obesity , data = BMI, layout = c(1, 2))























