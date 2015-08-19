CC = gcc
SUBDIRS = tcp_select tcp_simple udp

.PHONY: subdirs $(SUBDIRS) clean

subdirs: $(SUBDIRS)

$(SUBDIRS): 
	$(MAKE) -C $@

clean:
	for dir in $(SUBDIRS); do \
	$(MAKE) clean -C $$dir; \
	done
