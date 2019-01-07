R := R --slave
document:
	@$(R) -e "devtools::document()"

test:
	@$(R) -e "devtools::test()"

check:
	@$(R) -e "devtools::check()"

revcheck:
	@$(R) -e "devtools::use_revdep()"
	@$(R) -f "revdep/check.R"

crancheck:
	@R CMD build .
	@R CMD check *.tar.gz


install:
	@R CMD INSTALL ./


clean:
	@rm -rf *.tar.gz *.Rcheck revdep
