# Copyright (c) 2022, Chris Hyndman
# SPDX-License-Identifier: BSD-3-Clause

CONTRIB_EL = $(wildcard contrib/*/*.el)
INIT_EL = init.el

EXPORT_DIR = export/

EXPORT_FILES = $(addprefix $(EXPORT_DIR),$(CONTRIB_EL) $(INIT_EL))
EXPORT_LOG = $(EXPORT_DIR)export.log

export: $(EXPORT_FILES) $(EXPORT_LOG)

$(EXPORT_DIR)%.el: %.el
	mkdir -p $(dir $@)
	cp $^ $@

$(EXPORT_LOG):
	mkdir -p $(dir $@)
	date > $@
	git remote get-url origin >> $@
	git log -n 1 --no-color >> $@

.PHONY: export-clean
export-clean:
	-rm -rf $(EXPORT_DIR)
