R := R --vanilla --slave
document:
	@$(R) -e "devtools::document()"

check:
	@$(R) -e "devtools::check()"

install:
	@R CMD INSTALL ./
