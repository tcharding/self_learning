CC = gcc
SUBDIRS = daytime sctp echo_IP names sockopts

.PHONY: subdirs lib $(SUBDIRS) clean

subdirs: $(SUBDIRS)

lib:
	$(MAKE) -C $@

$(SUBDIRS): lib
	$(MAKE) -C $@

clean:
	for dir in lib  $(SUBDIRS); do \
	$(MAKE) clean -C $$dir; \
	done
