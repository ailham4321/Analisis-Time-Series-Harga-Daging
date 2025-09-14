# Analisis Time Series Harga Daging Jakarta

Repositori ini berisi skrip untuk analisis deret waktu (time series) harga daging di Jakarta, termasuk pemodelan ARIMA dan Analisis Intervensi (intervention analysis) untuk mendeteksi dan memodelkan perubahan level/struktur akibat suatu kejadian pada titik waktu tertentu.

> Bahasa: Indonesia (dengan beberapa istilah teknis umum dalam bahasa Inggris)

---

## Ringkasan Proyek

- Tujuan: Memodelkan dinamika harga daging, mengevaluasi kestasioneran, memilih model ARIMA yang sesuai, dan mengukur efek intervensi pada waktu t ≈ 66 (sekitar 2021-03-29) menggunakan variabel eksogen (step/pulse) di dalam model ARIMA.
- Metode: EDA, uji ADF untuk stasioneritas, identifikasi ACF/PACF, pemilihan model via `auto.arima` dan grid kecil p,d,q, diagnostik residu (Ljung–Box, KS), peramalan in-sample dan out-of-sample, serta ARIMA dengan intervensi (`xreg`).
- Hasil visual: Tersedia beberapa plot PNG serta output HTML Notebook.

---

## Struktur Repository

```
.
├── 5003211069_Abdillah Ilham_Analisis Time Series.R   # Skrip utama analisis harga daging (ARIMA + intervensi)
├── Analisis Harga Daging Jakarta.Rmd                  # R Notebook (draf/minimal)
├── Analisis Harga Daging Jakarta.nb.html              # Hasil render HTML dari R Notebook
├── Harga Daging Fixed.csv                             # Data harga daging (versi tetap/rapi)
├── Inflation.csv                                      # Contoh data lain (dipakai di skrip referensi intervensi)
├── Processing and ARIMA.ipynb                         # Notebook tambahan (opsional)
├── Sintax Intervensi.R                                # Skrip referensi analisis intervensi (contoh PDB/Inflasi)
├── Plot Awal.png                                      # Visualisasi awal harga daging
├── Plot Insample Outsample.png                        # In-sample & Out-of-sample
├── Plot Intervensi.png                                # Hasil model dengan intervensi
├── Plot Preintervensi.png                             # Segment sebelum intervensi
└── desktop.ini
```

> Catatan: `Sintax Intervensi.R` adalah skrip referensi (contoh PDB/Inflasi 1970–2013) untuk metodologi intervensi; tidak langsung terkait dataset harga daging, namun pola kodenya serupa.

---

## Data

- Sumber file utama: `Harga Daging Fixed.csv` (time series bertanggal). Skrip utama sebelumnya juga merujuk ke `Harga Daging.csv`. Jika Anda hanya memiliki `Harga Daging Fixed.csv`, perbarui path/namanya di skrip atau salin/rename agar sesuai.
- Kolom penting yang digunakan di skrip: `Datetime` (tanggal) dan `Daging` (nilai harga). Tanggal dikonversi dengan `as.Date` memakai format seperti `"%B %d, %Y"`.

---

## Ketergantungan (R Packages)

Skrip utama menggunakan paket-paket berikut (beberapa opsional untuk eksperimen/plot):

- Visualisasi/alat: `ggplot2`, `plotly`
- Time series: `forecast`, `TSA`, `astsa`, `tseries`, `aTSA`
- Uji/diagnostik: `lmtest`, `fBasics`, `strucchange`
- Manipulasi: `reshape` (atau `reshape2` bila diperlukan), `Rmisc`
- Lainnya di skrip referensi: `MASS`, `FitAR`, `tsoutliers`, `stargazer`

> Gunakan R terbaru dan RStudio bila memungkinkan. Instal paket hanya yang diperlukan untuk skrip yang Anda jalankan.

---

## Menjalankan Analisis

1. Clone atau unduh repositori ini.
2. Buka R/RStudio dan set direktori kerja ke folder repo ini.
3. Pastikan `Harga Daging Fixed.csv` ada di akar repo (atau sesuaikan path di skrip).
4. Instal paket yang diperlukan (sekali saja), lalu jalankan skrip utama:

```r
# Instal (jika belum ada)
install.packages(c(
  "ggplot2","forecast","astsa","lmtest","TSA","strucchange",
  "reshape","Rmisc","fBasics","tseries","aTSA","plotly"
))

# (Opsional, dipakai di skrip referensi intervensi)
install.packages(c("MASS","FitAR","tsoutliers","stargazer"))

# Jalankan analisis utama harga daging
source("5003211069_Abdillah Ilham_Analisis Time Series.R")
```

5. (Opsional) Render R Notebook draf menjadi HTML dari RStudio: Knit `Analisis Harga Daging Jakarta.Rmd`.
6. (Opsional) Coba skrip referensi intervensi `Sintax Intervensi.R` dengan menyesuaikan `setwd(...)` dan pastikan `Inflation.csv` tersedia.

---

## Alur Analisis Utama (Ringkas)

- EDA: Plot deret waktu harga daging; inspeksi titik intervensi sekitar `t = 66` (≈ 2021-03-29).
- Kestasioneran: Uji Augmented Dickey–Fuller (ADF). Jika nonstasioner, lakukan diferensiasi (d = 1) dan cek ACF/PACF.
- Pemodelan pre-intervensi: Seleksi ARIMA (contoh yang digunakan pada segmen pra-intervensi: ARIMA(2,1,0)).
- Diagnostik: Uji Ljung–Box untuk white noise residual, uji KS untuk normalitas residual.
- Peramalan: In-sample fit dan out-of-sample forecast; bandingkan dengan data aktual.
- Analisis intervensi: Gunakan variabel langkah (step) mulai `t = 66` sebagai `xreg` dalam `Arima(...)` untuk menangkap lonjakan/level shift; lakukan peramalan ke depan dengan nilai `xreg` masa depan yang konsisten (step = 1 setelah intervensi).

---

## Cuplikan Hasil (Gambar)

- Visualisasi pra-intervensi

  ![Pra-Intervensi](Plot%20Preintervensi.png)

- In-sample vs Out-of-sample

  ![In/Out Sample](Plot%20Insample%20Outsample.png)

- Model dengan Intervensi

  ![Intervensi](Plot%20Intervensi.png)

- Plot awal (eksplorasi)

  ![Plot Awal](Plot%20Awal.png)

> Jika gambar tidak tampil di GitHub, pastikan nama file cocok (case-sensitive) dan path relatif benar.

---

## Catatan & Troubleshooting

- Path file: Skrip contoh awal menggunakan `setwd("C:/Users/Asem/Downloads")`. Ubah agar sesuai lokasi repo Anda atau gunakan path relatif.
- Nama data: Jika skrip mengacu ke `Harga Daging.csv` tetapi repo Anda hanya memiliki `Harga Daging Fixed.csv`, sesuaikan nama file atau buat salinan dengan nama yang diharapkan.
- Frekuensi waktu: Skrip memperlakukan data sebagai deret waktu tak musiman (frequency = 1). Jika Anda ingin menangkap musiman (mingguan/bulanan), sesuaikan `frequency` dan model musiman.
- Versi paket: Perbedaan versi dapat mengubah hasil `auto.arima`. Simpan seed atau spesifikasi manual (p,d,q) untuk replikasi ketat.

---

## Lisensi

Belum ditetapkan

---

## Kredit

- Penulis: Abdillah Ilham
- Kontributor kode dan metodologi: lihat skrip di repositori ini. Jika Anda menggunakan atau mengembangkan proyek ini, mohon cantumkan atribusi yang sesuai.
