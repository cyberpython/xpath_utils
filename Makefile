# Copyright 2017 Georgios Migdos
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
# copies of the Software, and to permit persons to whom the Software is 
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in 
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

ifeq ($(LIBXML2_INCLUDE_PATH),)
LIBXML2_INCLUDE_PATH=/usr/include/libxml2
endif

ifneq ($(LIBXML2_LIB_PATH),)
LARGS += -L$(LIBXML2_LIB_PATH)
endif

LARGS +=-Llib -lxpathutils

ifeq ($(LIBXML2_NAME),)
LARGS += -lxml2
else
LARGS += -l$(LIBXML2_NAME)
endif

PATHSEP2=/
PATHSEP=$(strip $(PATHSEP2))
CMDSEP=&&
DEL_DIR_CMD=rm -rf
DEL_FILE_CMD=rm -f
MAKE_DIR_CMD=mkdir -p
COPY_FILE_CMD=cp

ifeq ($(OS),Windows_NT)
CMDSEP=;
PATHSEP2=\\
PATHSEP=$(strip $(PATHSEP2))
DEL_DIR_CMD=rmdir /q /s
DEL_FILE_CMD=del /q /f
MAKE_DIR_CMD=mkdir
COPY_FILE_CMD=copy /y
endif


all: lib examples

clean:
	-$(DEL_DIR_CMD) obj
	-$(DEL_DIR_CMD) lib
	-$(DEL_DIR_CMD) include
	-$(DEL_FILE_CMD) b~*
	-$(DEL_FILE_CMD) example
	-$(DEL_FILE_CMD) example.exe

lib:
	$(MAKE_DIR_CMD) lib
	$(MAKE_DIR_CMD) include
	gcc -c -o lib$(PATHSEP)xpath_proc.o src$(PATHSEP)xpath_proc.c -I$(LIBXML2_INCLUDE_PATH)
	gnatmake -D lib -c src$(PATHSEP)xpath_utils.adb
	cd lib $(CMDSEP) ar rcs libxpathutils.a xpath_proc.o xpath_utils.o
	$(DEL_FILE_CMD) lib$(PATHSEP)*.o
	cd ..
	$(COPY_FILE_CMD) src$(PATHSEP)xpath_utils.ads include$(PATHSEP)

examples: lib
	$(MAKE_DIR_CMD) obj
	gnatmake -D obj -o example examples$(PATHSEP)example.adb -Iinclude -aLlib -largs $(LARGS)