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

with Interfaces.C;
with Interfaces.C.Strings;
package body XPATH_UTILS is

  use type Interfaces.C.int;
  use type Interfaces.C.Strings.chars_ptr;

  XPATH_PROC_RET_CODE_SUCCESS_C                       : constant := 0;
  XPATH_PROC_RET_CODE_FAILURE_C                       : constant := 1;
  XPATH_PROC_RET_CODE_FAILED_TO_PARSE_FILE_C          : constant := 2;
  XPATH_PROC_RET_CODE_XPATH_CONTEXT_CREATION_FAILED_C : constant := 3;
  XPATH_PROC_RET_CODE_INVALID_XPATH_EXPR_C            : constant := 4;

  -- void xpath_proc_initialize()
  procedure XPath_Proc_Initialize;
  pragma Import(C, XPath_Proc_Initialize, "xpath_proc_initialize");

  -- void xpath_proc_finalize()
  procedure XPath_Proc_Finalize;
  pragma Import(C, XPath_Proc_Finalize, "xpath_proc_finalize");

  -- xpath_proc_parse(char* filename, xmlDoc **xml_doc, xmlXPathContext **xpath_context, int *ret_code)
  procedure XPath_Proc_Parse(Filename            : in     Interfaces.C.Strings.chars_ptr;
                             Suppress_Error_Msgs : in     Interfaces.C.int;
                             Xml_Doc             : in out System.Address;
                             XPath_Context       : in out System.Address;
                             Ret_Code            :    out Interfaces.C.int);
  pragma Import(C, XPath_Proc_Parse, "xpath_proc_parse");

  -- void xpath_proc_cleanup(xmlDoc *xml_doc, xmlXPathContext *xpath_context)
  procedure XPath_Proc_Cleanup(Xml_Doc       : in System.Address;
                               XPath_Context : in System.Address);
  pragma Import(C, XPath_Proc_Cleanup, "xpath_proc_cleanup");

  -- void xpath_proc_get_value(const char *xpath_expr, xmlXPathContext *xpath_context, char **result, int *ret_code)
  procedure XPath_Proc_Get_Value(XPath_Expr    : in     Interfaces.C.Strings.chars_ptr;
                                 XPath_Context : in     System.Address;
                                 Result        : in out Interfaces.C.Strings.chars_ptr;
                                 Ret_Code      :    out Interfaces.C.int);
  pragma Import(C, XPath_Proc_Get_Value, "xpath_proc_get_value");


  procedure Initialize
  is
  begin
    XPath_Proc_Initialize;
  end Initialize;

  procedure Finalize
  is
  begin
    XPath_Proc_Finalize;
  end Finalize;

  procedure Parse_File(Filename : in     Ada.Strings.Unbounded.Unbounded_String;
                       Context  :    out Context_T;
                       Success  :    out Boolean;
                       Silent   : in     Boolean := True)
  is
    RC                  : Interfaces.C.int               := XPATH_PROC_RET_CODE_FAILURE_C;
    Fname               : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
    Suppress_Error_Msgs : Interfaces.C.int               := 0;
  begin
    Fname := Interfaces.C.Strings.New_String(Ada.Strings.Unbounded.To_String(Filename));
    if Silent then
      Suppress_Error_Msgs := 1;
    else
      Suppress_Error_Msgs := 0;
    end if;
    XPath_Proc_Parse(Filename             => Fname,
                     Suppress_Error_Msgs  => Suppress_Error_Msgs,
                     Xml_Doc              => Context.Xml_Doc,
                     XPath_Context        => Context.XPath_Ctx,
                     Ret_Code             => RC);
    Success := RC = XPATH_PROC_RET_CODE_SUCCESS_C;
    if Fname /= Interfaces.C.Strings.Null_Ptr then
      Interfaces.C.Strings.Free(Fname);
    end if;
  end Parse_File;

  procedure Get_Value(XPath_Expression : in     Ada.Strings.Unbounded.Unbounded_String;
                      Context          : in     Context_T;
                      Value            :    out Ada.Strings.Unbounded.Unbounded_String)
  is
    XPath_Expr : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
    Result     : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.Null_Ptr;
    RC         : Interfaces.C.int := XPATH_PROC_RET_CODE_FAILURE_C;
  begin
    XPath_Expr := Interfaces.C.Strings.New_String(Ada.Strings.Unbounded.To_String(XPath_Expression));
    XPath_Proc_Get_Value(XPath_Expr    => XPath_Expr,
                         XPath_Context => Context.XPath_Ctx,
                         Result        => Result,
                         Ret_Code      => RC);
    if RC = XPATH_PROC_RET_CODE_SUCCESS_C then
      Value := Ada.Strings.Unbounded.To_Unbounded_String(Interfaces.C.Strings.Value(Result));
    else
      Value := Empty_String_C;
    end if;
    Interfaces.C.Strings.Free(XPath_Expr);
  end Get_Value;

  procedure Free_Resources(Context : in Context_T)
  is
  begin
    XPath_Proc_Cleanup(Xml_Doc       => Context.Xml_Doc,
                       XPath_Context => Context.XPath_Ctx);
  end Free_Resources;

end XPATH_UTILS;
