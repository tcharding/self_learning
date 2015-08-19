SUBDIRS = ch2 ch3 ch4 ch5 ch6

.PHONY: subdirs lib $(SUBDIRS) clean

subdirs: $(SUBDIRS)

$(SUBDIRS): 
	$(MAKE) -C $@

clean:
	for dir in  $(SUBDIRS); do \
	$(MAKE) clean -C $$dir; \
	done
