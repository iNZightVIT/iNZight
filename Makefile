R := R --slave
document:
	@$(R) -e "devtools::document()"

FILTER ?= NULL
test:
	@$(R) -e "devtools::test(filter = $(FILTER))"

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
