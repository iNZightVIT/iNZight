PKG_VERSION = $(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME = $(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)
CURRENT_DATE = $(shell date +%Y%m%d)

INST_FILES := $(shell find inst -type f -print)
MAN_FILES := $(wildcard man/*.Rd)
R_FILES := $(wildcard R/*.R)
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES) $(MAN_FILES) $(INST_FILES)

.PHONY: build check install run win release clean

build: $(PKG_NAME)_$(PKG_VERSION)-$(CURRENT_DATE).tar.gz

$(PKG_NAME)_$(PKG_VERSION)-$(CURRENT_DATE).tar.gz: $(PKG_FILES)
	sed -i "s/$(PKG_VERSION)/$(PKG_VERSION)-$(CURRENT_DATE)/" DESCRIPTION
	R CMD build ./
	sed -i "s/$(PKG_VERSION).*/$(PKG_VERSION)/" DESCRIPTION

check: $(PKG_NAME)_$(PKG_VERSION)-$(CURRENT_DATE).tar.gz
	R CMD check $<

install: $(PKG_NAME)_$(PKG_VERSION)-$(CURRENT_DATE).tar.gz
	R CMD INSTALL $<

run: $(PKG_NAME)_$(PKG_VERSION)-$(CURRENT_DATE).tar.gz
	R CMD INSTALL $<
	R

release: $(PKG_NAME).zip $(PKG_NAME).tar.gz

win: $(PKG_NAME)_$(PKG_VERSION)-$(CURRENT_DATE).zip

$(PKG_NAME).tar.gz: $(PKG_NAME)_$(PKG_VERSION)-$(CURRENT_DATE).tar.gz
	cp $< $@

$(PKG_NAME).zip: $(PKG_NAME)_$(PKG_VERSION)-$(CURRENT_DATE).zip
	cp $< $@

$(PKG_NAME)_$(PKG_VERSION)-$(CURRENT_DATE).zip: $(PKG_NAME)_$(PKG_VERSION)-$(CURRENT_DATE).tar.gz
	mkdir tmp
	R CMD INSTALL -l tmp $<
	cd tmp ; zip --quiet -r $@ $(PKG_NAME) ; mv $@ ../
	-rm -rf tmp

clean:
	-rm $(PKG_NAME)*.tar.gz
	-rm -rf $(PKG_NAME).Rcheck
	-rm $(PKG_NAME)*.zip
