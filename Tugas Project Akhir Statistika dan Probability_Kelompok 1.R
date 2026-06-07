# ============================================================
# Tugas Project Akhir Statistika dan Probability 
# Uji Hipotesis bagi Rata-rata Satu Populasi – Ragam populasi tidak diketahui (Uji t)
# Studi Kasus: Nilai Ujian Mahasiswa
# Mata Kuliah: Probabilitas dan Statistika
# ============================================================

# ---- LANGKAH 1: INPUT DATA ----

nama <- c("Raffi", "Lanang", "Apin", "Ripky", "Fathir",
          "Fitri", "Gilang", "Hana", "Irfan", "Jasmine",
          "Kevin", "Liana", "Miko", "Nadia", "Oscar")

nilai <- c(72, 68, 80, 74, 65, 78, 82, 70, 76, 69, 85, 73, 71, 79, 67)

data_mahasiswa <- data.frame(No = 1:15, Nama = nama, Nilai = nilai)

# Menampilkan data
cat("Data nilai ujian mahasiswa:\n")
print(data_mahasiswa)

# Menampilkan jumlah data
cat("\nJumlah mahasiswa (n):", length(nilai), "\n")

# ---- LANGKAH 2: STATISTIK DESKRIPTIF ----

cat("\n===== STATISTIK DESKRIPTIF =====\n")
cat("Rata-rata (mean)        :", mean(nilai), "\n")
cat("Simpangan baku (sd)     :", round(sd(nilai), 4), "\n")
cat("Varians (var)           :", round(var(nilai), 4), "\n")
cat("Nilai minimum           :", min(nilai), "\n")
cat("Nilai maksimum          :", max(nilai), "\n")
cat("Median                  :", median(nilai), "\n")

# ---- LANGKAH 3: VISUALISASI DATA ----

# Membuat histogram untuk melihat distribusi data
hist(nilai,
     main  = "Distribusi Nilai Ujian Mahasiswa",
     xlab  = "Nilai Ujian",
     ylab  = "Frekuensi",
     col   = "lightblue",
     border= "white",
     breaks= 6)

# Menambahkan garis nilai acuan (μ₀ = 75)
abline(v = 75, col = "red", lwd = 2, lty = 2)
legend("topright",
       legend = "Nilai Acuan (μ₀ = 75)",
       col    = "red",
       lty    = 2,
       lwd    = 2)

# ---- LANGKAH 4: UJI NORMALITAS (Shapiro-Wilk) ----

cat("\n===== UJI NORMALITAS (Shapiro-Wilk) =====\n")
uji_normal <- shapiro.test(nilai)
print(uji_normal)

# Interpretasi uji normalitas
if (uji_normal$p.value > 0.05) {
  cat("Kesimpulan: Data berdistribusi normal (p-value > 0,05)\n")
  cat("Asumsi normalitas terpenuhi. Uji t dapat dilanjutkan.\n")
} else {
  cat("Catatan: Data tidak berdistribusi normal (p-value ≤ 0,05)\n")
}

# ---- LANGKAH 5: UJI t SATU SAMPEL ----

cat("\n===== UJI t SATU SAMPEL =====\n")

# Menetapkan nilai acuan
mu_0 <- 75

# Melakukan uji t satu sampel (dua arah / two-tailed)
hasil_uji <- t.test(nilai,
                    mu          = mu_0,      # Nilai acuan populasi
                    alternative = "two.sided", # Uji dua arah
                    conf.level  = 0.95)       # Tingkat kepercayaan 95%

# Menampilkan hasil lengkap
print(hasil_uji)

# ---- LANGKAH 6: MENGHITUNG t HITUNG SECARA MANUAL ----
# (untuk membuktikan pemahaman konsep)

cat("\n===== PERHITUNGAN MANUAL =====\n")
x_bar  <- mean(nilai)        # Rata-rata sampel
s      <- sd(nilai)          # Simpangan baku sampel
n      <- length(nilai)      # Ukuran sampel
se     <- s / sqrt(n)        # Standard error

t_hitung <- (x_bar - mu_0) / se

cat("Rata-rata sampel (x̄) :", round(x_bar, 4), "\n")
cat("Simpangan baku (s)    :", round(s, 4), "\n")
cat("Ukuran sampel (n)     :", n, "\n")
cat("Standard Error (s/√n) :", round(se, 4), "\n")
cat("t hitung              :", round(t_hitung, 4), "\n")
cat("Derajat bebas (df)    :", n - 1, "\n")

# ---- LANGKAH 7: KESIMPULAN OTOMATIS ----

cat("\n===== KESIMPULAN =====\n")
cat("Nilai acuan (μ₀)      :", mu_0, "\n")
cat("Taraf signifikansi (α):", 0.05, "\n")
cat("p-value               :", round(hasil_uji$p.value, 4), "\n\n")

if (hasil_uji$p.value < 0.05) {
  cat("KEPUTUSAN: Tolak H₀\n")
  cat("INTERPRETASI: Terdapat cukup bukti statistik bahwa rata-rata\n")
  cat("nilai ujian mahasiswa BERBEDA SECARA SIGNIFIKAN dari nilai acuan 75.\n")
} else {
  cat("KEPUTUSAN: Gagal Menolak H₀\n")
  cat("INTERPRETASI: Tidak terdapat cukup bukti statistik bahwa rata-rata\n")
  cat("nilai ujian mahasiswa berbeda dari nilai acuan 75.\n")
}