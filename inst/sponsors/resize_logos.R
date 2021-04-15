files <- list.files(pattern = ".+_logo\\.png")
for (file in files)
	system(
		sprintf("convert %s -geometry x50 %s",
			file,
			gsub("logo", "logo50", file)
		)
	)
