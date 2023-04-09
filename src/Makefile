include ../Make.options

.PHONY: default
default:
	$(MMC) --make libmercury_leb128

.PHONY: rebuild
rebuild:
	$(MMC) --rebuild libmercury_leb128

.PHONY: install
install:
	$(MMC) --make libmercury_leb128.install

tags: $(wildcard *.m)
	mtags $^

.PHONY: clean
clean:
	$(MMC) --make mercury_leb128.clean

.PHONY: realclean
realclean:
	$(MMC) --make mercury_leb128.realclean
	/bin/rm -rf Mercury
	/bin/rm -f *.err *.mh
	/bin/rm -f tags
