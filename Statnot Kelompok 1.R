# KELOMPOK 1: BINOMIAL & CHI SQUARE TEST

# Binomial Test
roti <- c('coklat', 'keju')
suka <- c(6, 4)
(data_roti = data.frame(roti, suka))

binom.test(data_roti$suka)
binom.test(x=4,n=10,p=0.5,alternative="greater",conf.level = 0.95)

# Uji satu arah
pbinom(q=4,size = 10,prob = 0.5)
# Uji dua rah
pbinom(q=4,size = 10,prob = 0.5)*2

# Menghitung kuantil distribusi binomial
# qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)
qbinom(0.7539, 10, 0.5, FALSE)
qbinom(0.7539, 10, 0.5, TRUE)
