XPath_Utils
===========

XPath_Utils is small Ada library that provides the ability to parse an XML file
and lookup element and attribute values based on XPath expressions (in fact the
actual processing is done by a tiny piece of C code which utilizes 
[libxml2](http://xmlsoft.org/)).

Build dependencies
------------------

* make
* gcc and gnat (preferrably `>= 4.6`)
* ar
* libxml2 (including the headers - this is part of the development
          package in most Linux distributions)

Build instructions
------------------

* To build the library only:
  
      make clean lib
  
  If all goes well, you should have the static library (and .ali files required
  for linking with Ada code) in `lib/` and the Ada specification in `include/`.

* To build the library and example program:

      make clean all
  
  As above, if all goes well, you should have the static library (and .ali files
  required for linking with Ada code) in `lib/` , the Ada specification in 
  `include/` and you should also see the example program in the root directory 
  of the project.

In case you need to set / modify the path of the libxml2 headers (by default 
these are included from `/usr/include/libxml2` in the makefile) you have to pass
the `LIBXML2_INCLUDE_PATH` variable with the correct value as an argument to 
make.

Also, the `LIBXML2_LIB_PATH` variable can be used to set the path where the
linker will search for the libxml2 library and the `LIBXML2_NAME` variable can
be used to change the name of the libxml2 library which used during linking (by
default set to `xml2`).


Performance
-----------

Since all of the XML parsing and XPath processing is done by libxml2 and its
XPath processor needs a DOM tree to work on, the whole XML document is parsed
and the DOM is loaded in memory (resources are deallocated with the call to 
`Free_Resources`). 
For very large XML documents this can be an issue. In such cases you should opt
for another SAX-based solution.

License
-------

The code is licensed under the 
[MIT License](https://opensource.org/licenses/MIT).
