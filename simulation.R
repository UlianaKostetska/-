# Підключення бібліотеки catR для використання функцій для адаптивного тестування
library(catR)

# Генерація банку питань методом чотирипараметричної логістичної моделі з 500 питань
bank = genDichoMatrix(items = 500, model = "4PL")

# Визначення початкового стану тесту
start = list(nrItems = 1, theta = 0)

# Створення списків параметрів для тестування методом БМ та МЛ
test_BM = list(itemSelect = "KL", method = "BM")
test_ML = list(itemSelect = "KL", method = "ML")

# Визначення умови зупинки тесту за критерієм точності 0.3
stop = list(rule = "precision", thr = 0.3)

# Визначення методу завершальної оцінки параметра (МЛ)
final = list(method = "ML")

# Генерація шаблону відповідей 
x = genPattern(th = 1, it = bank, model = NULL)

# Адаптивне тестування методом БМ 
res_BM = randomCAT(trueTheta = 1, itemBank = bank, responses = x, model = NULL,
                   start = start, test = test_BM, stop = stop, final = final)
res_BM

# Адаптивне тестування методом МЛ 
res_ML = randomCAT(trueTheta = 1, itemBank = bank, responses = x, model = NULL,
                   start = start, test = test_ML, stop = stop, final = final)
res_ML

# Повторне адаптивне тестування методом БМ без вказання справжнього значення здібності
res2_BM = randomCAT(itemBank = bank, responses = x, model = NULL,
                    start = start, test = test_BM, stop = stop, final = final)
res2_BM

# Повторне адаптивне тестування методом МЛ без вказання справжнього значення здібності
res2_ML = randomCAT(itemBank = bank, responses = x, model = NULL,
                    start = start, test = test_ML, stop = stop, final = final)
res2_ML

# Визначення послідовності рівнів здібностей учасників тестування 
thetas = seq(from = -2, to = 2, length = 20)

# Вивід шаблону відповідей з результатів адаптивного тестування методом БМ
res2_BM$pattern

# Вивід підмножини шаблону відповідей для питань, вибраних методом БМ
x[res2_BM$testItems]

# Вивід шаблону відповідей з результатів CAT методом МЛ
res2_ML$pattern

# Вивід підмножини шаблону відповідей для питань, вибраних методом МЛ
x[res2_ML$testItems]

# Симуляція тестування для різних рівнів здібностей методом БМ
res3_BM = simulateRespondents(thetas = thetas, itemBank = bank, model = NULL,
                              start = start, test = test_BM, stop = stop, final = final)
res3_BM

# Симуляція тестування для різних рівнів здібностей методом МЛ
res3_ML = simulateRespondents(thetas = thetas, itemBank = bank, model = NULL,
                              start = start, test = test_ML, stop = stop, final = final)
res3_ML

# Повторна симуляція тесту для різних рівнів знань зі збільшеним банком питань
bank2 = genDichoMatrix(items = 5000, model = "4PL")

res3_BM2 = simulateRespondents(thetas = thetas, itemBank = bank2, model = NULL,
                               start = start, test = test_BM, stop = stop, final = final)
res3_ML2 = simulateRespondents(thetas = thetas, itemBank = bank2, model = NULL,
                               start = start, test = test_ML, stop = stop, final = final)

res3_BM2
res3_ML2
