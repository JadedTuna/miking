include test-files.mk

.PHONY: all selected $(src_files_all)

all: $(src_files_all)

selected: $(compile_files_typecheck)

$(src_files_all):
	@./make type-check $@