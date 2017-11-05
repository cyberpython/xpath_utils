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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with XPATH_UTILS;

procedure example
is
  package SU renames Ada.Strings.Unbounded;
  use type Ada.Strings.Unbounded.Unbounded_String;

  Context  : XPATH_UTILS.Context_T;
  Ret_Code : Boolean := False;
  Val      : SU.Unbounded_String := SU.Null_Unbounded_String;

begin

  XPATH_UTILS.Initialize;

  XPATH_UTILS.Parse_File(Filename => SU.To_Unbounded_String("test.xml"),
                          Context  => Context,
                          Success  => Ret_Code);
  
  if Ret_Code then
    
    -- Example of fully qualified XPath to extract the value of an attribute
    XPATH_UTILS.Get_Value(XPath_Expression => SU.To_Unbounded_String("/config/entry/item[@id=12]/@param_x"),
                           Context          => Context,
                           Value            => Val);
    
    Put_Line(SU.To_String(Val));

    -- Example of XPath that matches all 'item' elements with the 'id' attribute
    -- set to '10'.
    -- Note that XPATH_UTILS will return the value for the first match only.
    XPATH_UTILS.Get_Value(XPath_Expression => SU.To_Unbounded_String("//item[@id=10]"),
                           Context          => Context,
                           Value            => Val);
    
    Put_Line(SU.To_String(Val));

    -- List processing example.
    -- Each list entry contains 2 elements: 'path' and 'checksum'
    -- If both are empty, we consider that we have reached the end of the list.
    declare
      Continue  : Boolean             := True;
      I         : Integer             := 1;
      File_Path : SU.Unbounded_String := SU.Null_Unbounded_String;
      Checksum  : SU.Unbounded_String := SU.Null_Unbounded_String;
    begin

      while Continue loop

        XPATH_UTILS.Get_Value(XPath_Expression => SU.To_Unbounded_String("/config/checksums/entry["&I'Img&"]/path"),
                              Context          => Context,
                              Value            => File_Path);
        XPATH_UTILS.Get_Value(XPath_Expression => SU.To_Unbounded_String("/config/checksums/entry["&I'Img&"]/checksum"),
                              Context          => Context,
                              Value            => Checksum);
        
        if File_Path = XPATH_UTILS.Empty_String_C and then Checksum = XPATH_UTILS.Empty_String_C then
          Continue := False;
        else
          if File_Path = XPATH_UTILS.Empty_String_C or else Checksum = XPATH_UTILS.Empty_String_C then
            Put_Line("Invalid list item at index " & I'Img &": both path and checksum need to be provided.");
          else
            Put_Line(SU.To_String(File_Path) & " : " & SU.To_String(Checksum));
          end if;
          I := I + 1;
        end if;
      end loop;

    end;

  else
    Put_Line("**** Could not load the XML file! ****");
  end if;
  
  XPATH_UTILS.Free_Resources(Context => Context);

  XPATH_UTILS.Finalize;
  
end example;
