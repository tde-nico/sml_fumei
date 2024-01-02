
all:
#	@ py fumei.py

run: all
	@ clear
	@ cat main.sml | sml

.PHONY: all run