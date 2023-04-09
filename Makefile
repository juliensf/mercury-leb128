include Make.options

.PHONY: default
default:
	cd src && $(MAKE) default

.PHONY: install
install:
	cd src && $(MAKE) install

.PHONY: realclean
realclean:
	cd src && $(MAKE) realclean
