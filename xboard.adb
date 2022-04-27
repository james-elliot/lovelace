-- LOVELACE version 1
-- Copyright JM Alliot
-- 18 May 2006
-- This is a chess engine xboard compatible written in full ADA
-- with some lines of assembly code
-- It is bitboard based and uses extensively ADA representation clauses
-- Endgames are terrible
-- Draws by repetion are not detected correctly
-- The evaluation function is very bad (and very short..)
-- Use at your own risk...

with Text_Io;
use Text_Io;
with Util;
use Util;

package body Xboard is


   procedure Convert(S:String) is
   begin
      Xboard_From := Character'Pos(S(S'First))-Character'Pos('a')
        +8*(Character'Pos(S(S'First+1))-Character'Pos('1'));
      Xboard_To := Character'Pos(S(S'First+2))-Character'Pos('a')
        +8*(Character'Pos(S(S'First+3))-Character'Pos('1'));
   end Convert;

   function Do_Command(S:String) return Boolean is

      function Find_Next(Curr:Integer) return Integer is
         Tmp : Integer:=S'Last+1;
      begin
         for I in Curr..S'Last loop
            if S(I)=' ' then
               Tmp := I;
               exit;
            end if;
         end loop;
         return Tmp;
      end Find_Next;

      Cut,Ncut : Integer;
   begin
      Cut := Find_Next(S'First);
      if S(S'First..Cut-1)="xboard" then
         Put_Line("");
      elsif S(S'First..S'last)="protover 2" then
         Put_Line("feature playother=0 draw=0 analyze=0 colors=0 ping=0 setboard=1 usermove=1 reuse=0 sigint=0 sigterm=0 setboard=1 done=1");
      elsif S(S'First..S'last)="quit" then
         Put_Line(File_Log,"raising sortie");
         Flush(File_Log);
         raise Sortie;
      elsif S(S'First..Cut-1)="usermove" then
         Convert(S(Cut+1..S'Last));
         Put_Line(File_Log,Integer'Image(Xboard_From)&Integer'Image(Xboard_To));
         Flush(File_log);
         return True;
      elsif S(S'First..Cut-1)="setboard" then
         Put_Line(File_Log,"Fen:"&S(Cut+1..S'Last));
         Flush(File_log);
         Xboard_Fen(1..S'Last-Cut) := S(Cut+1..S'Last);
         Xboard_Fen_Last := S'Last-Cut;
         return true;
      elsif S(S'First..S'last)="go" then
         Xboard_From := -1;Xboard_To := -1;
         Xboard_Force := false;
         Put_Line(File_Log,Integer'Image(Xboard_From)&Integer'Image(Xboard_To));
         Flush(File_log);
         return True;
      elsif S(S'First..S'last)="force" then
         Xboard_Force := True;
         Put_Line(File_Log,"force");
         Flush(File_Log);
         return False;
      elsif S(S'First..Cut-1)="time" then
         Xtime := Integer'Value(S(Cut+1..S'Last));
         Put_Line(File_Log,"time:"&Integer'Image(Xtime));
      elsif S(S'First..Cut-1)="level" then
         Ncut := Find_Next(Cut+1);
         XT1 := Integer'Value(S(Cut+1..Ncut-1));
         Cut := Ncut;
         Ncut := Find_Next(Cut+1);
         XT2 := Integer'Value(S(Cut+1..Ncut-1));
         Cut := Ncut;
         Ncut := Find_Next(Cut+1);
         XT3 := Integer'Value(S(Cut+1..Ncut-1));
         Cut := Ncut;
         Put_Line(File_Log,"t1,t2,t3:"&Integer'Image(XT1)&Integer'Image(XT2)&Integer'Image(XT3));
      elsif S(S'First..Cut-1)="name" then
         Xname(1..S'Last-Cut) := S(Cut+1..S'Last);
         Xname_Last := S'Last-Cut;
         Put_Line(File_Log,"name: "&Xname(1..Xname_Last));
      elsif S(S'First..Cut-1)="rating" then
         Ncut := Find_Next(Cut+1);
         XratingC := Integer'Value(S(Cut+1..Ncut-1));
         Cut := Ncut;
         Ncut := Find_Next(Cut+1);
         XratingO := Integer'Value(S(Cut+1..Ncut-1));
         Put_Line(File_Log,"rating: "&Integer'Image(XratingC)&Integer'Image(XratingO));
      elsif S(S'First..Cut-1)="otim" then
         Xotime := Integer'Value(S(Cut+1..S'Last));
         Put_Line(File_Log,"otime: "&Integer'Image(Xotime));
      elsif S(S'First..Cut-1)="result" then
         Put_Line(File_Log,"result: "&S(Cut+1..S'Last));
         Put_Line(Game_Log,"");
         Put_Line(Game_Log,S(Cut+1..S'Last));
      end if;
      Flush(File_Log);
      Flush(Standard_Output);
      return False;
   end Do_Command;

   procedure Send_Xboard(From:Integer;To:Integer) is
      S:String (1..9):= "move     ";
   begin
      S(6):=Character'Val(Character'Pos('a')+From mod 8);
      S(7):=Character'Val(Character'Pos('1')+From/8);
      S(8):=Character'Val(Character'Pos('a')+To mod 8);
      S(9):=Character'Val(Character'Pos('1')+To/8);
      Put_Line(S);
      Flush(Standard_Output);
   end Send_Xboard;
   
   procedure Resign is
   begin
      Put_Line("resign");
      Flush(Standard_Output);
   end Resign;

   procedure Read_Xboard(From:Integer;To:Integer) is
      Str: String(1..255);
      Last : Natural;
   begin
      if From /= -1 then
         Send_Xboard(From,To);
      end if;
      Xboard_Fen_Last:=0;
      while True loop
         Get_Line(Str,Last);
         if Last>=1 then
            Put_Line(File_Log,Str(1..Last));
            Flush(File_Log);
            if Do_Command(Str(1..Last)) then
               if Xboard_Fen_Last/=0 then Xboard_From:=-3; end if;
               exit;
            end if;
         end if;
      end loop;
   exception
      when others =>
         Put_Line(File_Log,"exception in read_xboard");
         Flush(File_Log);
         raise Sortie;
   end Read_Xboard;

end Xboard;
