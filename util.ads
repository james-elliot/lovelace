with Text_Io;
use Text_Io;

package Util is

   function Convert_To_Pos(Tmp:Integer) return String;
   function Open_New_Log(Name : String) return Boolean;
   File_Log : File_Type;
   Game_Log : File_Type;

end Util;
