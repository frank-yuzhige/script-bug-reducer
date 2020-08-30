CC=gcc
# Makefile 内容
SRCS:=programA.c programB.c programC.c

target1: target2 target3 target4
	@echo "target1"

target2: target3 target5
	@echo "target2"
	$(error internal error)

target3: target5
	@echo "target3"

target4: target6
	@echo "target4"

target5:
	@echo "target5"

target6:
	@echo "target6"
