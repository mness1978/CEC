DEVPRGPATH=~/tools

CC=gcc
AS=$(DEVPRGPATH)/vasm/vasm
LINK=$(DEVPRGPATH)/vlink/vlink
MAKE=make
ASFLAGS= -m68040
LINKFLAGS=

all: main
	@echo Linking CEC
	$(LINK) $(LINKFLAGS) -o CEC main.o

main: main.s
	@echo Building main.s
	$(AS) -o main.o $(ASFLAGS) -Fhunk main.s
