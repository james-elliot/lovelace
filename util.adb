with Text_Io;
use Text_Io;


package body Util is


function Open_New_Log(Name : String) return boolean is
   begin
      Open(File_Log,In_File,name);
      Close(File_Log);
      return False;
   exception
      when NAME_ERROR =>
         Create(File_Log,Out_File,Name);
         return true;
   end Open_New_Log;

   Name : String(1..4);
   L : Integer;
begin
   for I in 0..9999 loop
      Name := "0000";
      L := Integer'Image(I)'Last-2;
      Name(4-L..4) := Integer'Image(I)(2..L+2);
     if Open_New_Log("log."&Name) then exit; end if;
   end loop;
   Create(Game_Log,Out_File,Name&".pgn");

end Util;
