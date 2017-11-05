-- Copyright 2017 Georgios Migdos
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
-- copies of the Software, and to permit persons to whom the Software is 
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in 
-- all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

with System;
with Ada.Strings.Unbounded;

package XPATH_UTILS is

  Empty_String_C : constant Ada.Strings.Unbounded.Unbounded_String := 
    Ada.Strings.Unbounded.To_Unbounded_String("");

  type Context_T is private;

  ------------------------------------------------------------------------------
  -- procedure Initialize
  --
  -- Initializes the package
  -- Should be called only by the main thread and only once prior to any other
  -- calls to any of the routines in this package.
  --
  -- **IMPORTANT**:
  -- Internally initializes the libxml2 parser for
  -- multi-threaded use by calling xmlInitParser().
  -- If another library that initializes the libxml2 parser
  -- or libxml2 calls are directly used for the same purpose,
  -- do not call this procedure.
  --
  procedure Initialize;
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- procedure Finalize
  --
  -- Finalizes the package
  -- Should be called only by the main thread and only once after all 
  -- calls to any of the routines in this package.
  --
  -- **IMPORTANT**:
  -- Internally calls libxml2's xmlCleanupParser() function.
  -- If another library that initializes the libxml2 parser or direct libxml2 
  -- calls are used for the same purpose, do not call this procedure.
  --
  procedure Finalize;
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- procedure Parse_File
  --
  -- Loads the specified file and constructs a DOM representation of its 
  -- contents.
  -- If the 'Silent' parameter is set to False then the XML parser will output 
  -- parsing errors to the standard error stream (stderr).
  -- 
  procedure Parse_File(Filename : in     Ada.Strings.Unbounded.Unbounded_String;
                       Context  :    out Context_T;
                       Success  :    out Boolean;
                       Silent   : in     Boolean := True);
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- procedure Get_Value
  --
  -- Retrieves the value of the node / attribute selected by the specified XPath
  -- expression.
  -- Returns an empty string if no node/attribute matched the XPath expression.
  -- 
  procedure Get_Value(XPath_Expression : in     Ada.Strings.Unbounded.Unbounded_String;
                      Context          : in     Context_T;
                      Value            :    out Ada.Strings.Unbounded.Unbounded_String);
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- procedure Free_Resources
  --
  -- Deallocates the resources used to store the DOM and
  -- the context for the XPath queries.
  -- 
  procedure Free_Resources(Context : in Context_T);
  ------------------------------------------------------------------------------

private

  type Context_T is
    record
      Xml_Doc   : System.Address;
      XPath_Ctx : System.Address;
    end record;

end XPATH_UTILS;