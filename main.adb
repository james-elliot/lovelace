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
with Bitboards;
use Bitboards;
with Bitscan;
use Bitscan;
with Move;
use Move;
with Eval;
use Eval;
with Hash;
use Hash;
with Xboard;
use Xboard;
with Calendar;
use Calendar;
with Hashbook;
use Hashbook;
with Ada.Real_Time;
with Adamt19937;
with Interfaces;
use Interfaces;
with Ada.Exceptions;
Use Ada.Exceptions;
with Gnat.Traceback;
use Gnat.Traceback;
with Util;
use Util;

procedure Main is
   
   function Convert_Clock(Tmp:Integer) return String is
      function Trans (I:Integer) return String is
	 T:String(1..2);
      begin
	 T := "00";
	 if I>=10 and I<=99 then 
	    T(1..2):=Integer'Image(I)(2..3);
	 elsif I>0 then
	    T(2):=Integer'Image(I)(2); 
	 end if;
	 return T;
      end Trans;
      S:String(1..8):="        ";
      Hour,Min,Sec:Integer;
   begin
      Hour := Tmp/3600;
      S(1..2):=Trans(Hour);
      S(3):=':';
      Min := (Tmp-3600*Hour)/60;
      S(4..5) := Trans(Min);
      S(6):=':';
      Sec:=Tmp-3600*Hour-60*Min;
      S(7..8):=Trans(Sec);
      return(S);
   end Convert_Clock;
   
   Main_Error : exception;
   type MY_FIXED is delta 0.1 range -9999.9..9999.9;
   package FIXED_IO is new TEXT_IO.FIXED_IO ( MY_FIXED ) ;

   Has_Book : Boolean := False;
   Cast : Castling := (others=>True);
   Col, My_Col  : Color;
   En_Passant :Integer := -1;
   Res : Integer :=0;
   P2 : Piece;
   N_En_Passant : Integer;
   N_Castles: Castling;
   Promote : Boolean;
   Matw, Matb : Integer;
   Valid : Boolean;
   Prof : Integer;
   Dur,Min_Delay : Float:=0.7635432;
   St : State;
   Lcb : array (0..63) of Piece;
   Today,Full_Start_Time,Start_Time,End_Time : Time;
   Full_Start_Time_Real,Start_Time_Real : Ada.Real_Time.Time;
   Have_Time : Float;
   Real_Dur : Ada.Real_Time.Time_Span;
   Max_Delay : Float;
   Tmp: Integer;
   Alpha, Beta:Integer;
   G      : AdaMT19937.Generator;
   Keys   : AdaMT19937.Access_Vector := new AdaMT19937.Vector (0 .. 3);
   Fail_Low,Fail_High : Boolean;
   Old_From:Integer :=-1;
   Old_To : Integer:=-1;
   Old_Xotime : Integer :=0;
   
begin

--   time 20000
--     go
-- Init_Boards("8/pR3pk1/8/2r3PP/Pn6/6K1/8/ w - - 1 40",Col,Cast,En_Passant,Matw,Matb);      
-- This board is the classical starting board
   Init_Boards("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",Col,Cast,En_Passant,Matw,Matb);

   Init_Masks;
   Init_Attacks;
   Has_Book := Load_All;
   Make_Z_I(Col);
   First_Ply := 0;
   Movenum:=0;
   Keys.all := (16#123#, 16#234#, 16#345#, 16#456#);
   AdaMT19937.Reset (G, Keys);

--   for I in 0..63 loop
--      Print_Intboard(King_Vicinity_I(I));
--      Put_Line("");
--   end loop;
--   Print_Chessboard;
--   Tmp := Valeur(Col,Matw,Matb,Cast);
--   raise Main_Error;


   Move_From := -1;Move_To := -1;My_Col := Black;

   while True loop
      <<Do_It_Again>>
      if Col/=My_Col or Xboard_Force then
        if Xboard_Force then Read_Xboard(-1,-1);
        else Read_Xboard(Move_From,Move_To); end if;
	if Movenum /= 0 then --xtime est maintenant a jour
	   Put(Game_Log," {[%eval ");
	   if Res<-9999 then 
	      Res := -(9999-(Res+32766));
	   elsif 
	     Res>9999 then 
	      Res:=9999+(Res-32766); 
	   end if;
	   Fixed_Io.Put(Game_Log,My_Fixed(Float(Res)));
	   Put(Game_Log,"]");
	   Put(Game_Log,"[%emt ");
	   Tmp := Integer(Dur+0.5);
	   Put(Game_Log,Convert_Clock(Tmp));
	   Put(Game_Log,"]");
	   Put(Game_Log,"[%clk ");
	   Put(Game_Log,Convert_Clock((Xtime+50)/100));
	   Put(Game_Log,"]");
	   Put(Game_Log,"}");
	   Flush(Game_Log);
	end if;
	
        if Xboard_From=-3 then
           Init_Boards(Xboard_Fen(1..Xboard_Fen_Last),Col,Cast,En_Passant,Matw,Matb);
           Put_Line(File_Log,"Material Start: "&Integer'Image(Matw)&" "&Integer'Image(Matb));
            Init_Masks;
            Init_Attacks;
            Make_Z_I(Col);
            My_Col := -Col;
            Print_Chessboard(File_Log);
            goto Do_It_Again;
         end if;
         Move_From := Xboard_From;
         Move_To := Xboard_To;
         Res := 0;
         Put_Line(File_Log,"Color:"&Color'Image(Col)&" My_col:"&Color'Image(My_Col)&" xboard_force:"
                  &Boolean'Image(Xboard_Force));
         if Move_From = -1 then My_Col := Col; end if;
         Put_Line(File_Log,"Color:"&Color'Image(Col)&" My_col:"&Color'Image(My_Col)&" xboard_force:"
                  &Boolean'Image(Xboard_Force));
         Print_Chessboard(File_Log);
      end if;
      if Movenum=0 then
         Put_Line(File_Log,"Starting pgn log");
         Put_Line(Game_Log,"[Event ""Freechess test""]");
         Put_Line(Game_Log,"[Site ""Freechess""]");
         Today := Clock;
         Put_Line(Game_Log,"[Date """&
                    Integer'Image(Year(Today))&"."&
                    Integer'Image(Month(Today))&"."&
                    Integer'Image(Day(Today))&
                    """]");
         if XT1/=0 then
            Put_Line(Game_Log,"[TimeControl """&Integer'Image(Xt1)&"/"&Integer'Image(Xt2*60)&
                       """]");
         else
            if Xt3=0 then
               Put_Line(Game_Log,"[TimeControl """&Integer'Image(Xt2*60)&"""]");
            else
               Put_Line(Game_Log,"[TimeControl """&Integer'Image(Xt2*60)&"+"&Integer'Image(Xt3)&"""]");
            end if;
         end if;
         if My_Col=Black then
            Put_Line(Game_Log,"[White """&Xname(1..Xname_Last)&"""]");
            Put_Line(Game_Log,"[Black ""Lovelace""]");
            Put_Line(Game_Log,"[WhiteElo """&Integer'Image(XratingO)&"""]");
            Put_Line(Game_Log,"[BlackElo """&Integer'Image(XratingC)&"""]");
         else
            Put_Line(Game_Log,"[White ""Lovelace""]");
            Put_Line(Game_Log,"[Black """&Xname(1..Xname_Last)&"""]");
            Put_Line(Game_Log,"[WhiteElo """&Integer'Image(XratingC)&"""]");
            Put_Line(Game_Log,"[BlackElo """&Integer'Image(XratingO)&"""]");
         end if;
         Put_Line(Game_Log,"");
         Flush(Game_Log);
      end if;
      if Col = My_Col and not Xboard_Force Then
         begin
            Full_Start_Time_Real := Ada.Real_Time.Clock;
            Full_Start_Time := Clock;
            Move_From := -1;Move_To := -1;Old_From:=-1;Old_To:=-1;
	    Prof := 10;Alpha:=-32767;Beta:=32767;
            Have_Time := Float(Xtime)/100.0;
            if Have_Time<5.0 then Min_Delay:=0.2;
            elsif Have_Time<15.0 then Min_Delay:=0.3;
            elsif Have_Time<30.0 then Min_Delay:=0.5;
	    elsif Have_Time<60.0 then Min_Delay:=0.75;
	    elsif Have_Time<120.0 then Min_Delay:=1.0;
            else Min_Delay := 1.5; end if;
            Put_Line(File_Log,"Xtime="&Integer'Image(Xtime));
            if Has_Book then
               Get_Book(Move_From,Move_To,Col,Cast,En_Passant);
               if Move_From/=-1 then
                  Put_Line(File_Log,"Raising end_thinking in get_book");
                  raise  End_Thinking;
               end if;
            end if;
            loop
               Put_Line(File_Log,"Starting main loop");
               Start_Time_real := Ada.Real_Time.Clock;
               Start_Time := Clock;
               if XT1/=0 then
                  Tmp := (Movenum/2) mod XT1;
                  Tmp := XT1 - Tmp;
                  Max_Delay := Have_Time/Float(Tmp+1);
                  Max_Delay := (4.0*Max_Delay)/3.0;
                  Put_Line(File_Log,"Max_delay="&Float'Image(Max_Delay)
                           &" Tmp="&Integer'Image(Tmp)&" Have_time="&Float'Image(Have_Time)
                          &" Movenum="&Integer'Image(Movenum));
               else
                  Tmp := Integer'Max(30-abs(Matw+Matb)/100,10);
                  Max_Delay := Float(XT3)+Have_Time/Float(Tmp);
                  Put_Line(File_Log,
                           "Max_delay="&Float'Image(Max_Delay)
                             &" Have_time="&Float'Image(Have_Time)
                             &" Movenum="&Integer'Image(Movenum)
                             &" mat="&Integer'Image(Tmp));
               end if;
               if Max_Delay<Min_Delay then Max_Delay := Min_Delay; end if;
               Put_Line(File_Log,"Max_delay="&Float'Image(Max_Delay));
               select
                  delay until (Start_Time+Duration(Max_Delay));
                  Restore_State(St);
                  for I in 0..63 loop Chess_Board(I) := Lcb(I); end loop;
                  Dur := Float(Clock-Start_Time);
                  Put_Line(File_Log,"Raising End_thinking in Interrupted.Dur:"&Float'Image(Dur)
                          &" Max_delay:"&Float'Image(Max_Delay));
                  raise End_Thinking;
               then abort
                  Save_State(St);
                  for I in 0..63 loop Lcb(I):=Chess_Board(I); end loop;
                  Nb_Pos :=0; Nb_Moves_Made := 0;Hash_Hits :=0; Max_Prof :=0;
                  Fail_Low:=False;Fail_High:=False;
                  loop
                     Put_Line(File_Log,"Starting inner loop prof="&Integer'Image(Prof)&" Alpha:"&
                                Integer'Image(Alpha)&" Beta"&Integer'Image(Beta));
                     Res := Gen_Moves(alpha,beta,Col,En_passant,Cast,0,False,Prof,Matw,Matb,False);
                     Put_Line(File_Log,"In loop res:"&Integer'Image(Res));
                     if Res+Prof/10=32766 or Res-Prof/10=-32766 then
			-- Mate
                        Put_Line(File_Log,"Raising end_thinking because res=32766 or res=-32766");
                        raise End_Thinking;
                     end if;
                     if Res>Alpha and Res<Beta then exit; end if;
                     if Res<=Alpha then Alpha:= -32767;Beta:=Res+1;Fail_Low:=True;
                     else Beta:=32767;Alpha:=Res-1; Fail_High:=True;End if;
                     if Fail_Low and Fail_High then
                        Alpha := -32767;Beta:=32767;
                        Put_Line(File_Log,"**************Double Fail****************");
                     end if;
                  end loop;
                  Alpha := Res-10;Beta:=Res+10;

                  End_Time := Clock;
                  Dur := Float(End_Time-Start_Time);
                  Have_Time := Have_Time-Dur;
                  Put_Line(File_log,"Out of loop. Prof:"&Integer'Image(Prof)&" From:"&Integer'Image(Move_From)&" To:"&Integer'Image(Move_To)&" Val:"&Integer'Image(Res)&" Time:"&Float'Image(Dur)&" Have_time:"&Float'Image(Have_Time));
		  Old_From := Move_From; Old_To := Move_To;
                  Flush(File_Log);
                  if (Dur > Max_Delay/3.0) and (Float(End_Time-Full_Start_Time)>Min_Delay) then
                     Put_Line(File_Log,
                              "Raising end_thinking. Dur="&Float'Image(Dur)&
                                " max_delay="&Float'Image(Max_Delay)&
                                " min_delay="&Float'Image(Min_Delay));
                     raise End_Thinking;
                  end if;
                  if Prof>=2000 then
                     Put_Line(File_Log,"Raising End_thinking because prof>=2000");
                     raise End_Thinking;
                  end if;
                  Prof := Prof+10;
                  Put_Line(File_Log,"Adding 10 to prof. New prof:"&Integer'Image(Prof));
               end select;
            end loop;
         exception
            when End_Thinking =>
               Put_Line(File_Log,"End thinking");
               Real_Dur := Ada.Real_Time."-"(Ada.Real_Time.Clock,Full_Start_Time_Real);
               Dur := Float(Clock-Full_Start_Time);
               if Dur<Min_Delay then
                  Put_Line(File_Log,"Duration<Min_Delay: "&Float'Image(Dur)&Float'Image(Min_Delay)&" Adding time");
                  delay Duration(Min_Delay-Dur+(Float(AdaMT19937.Random(G))/Float(Unsigned_32'Last)/2.0));
               end if;
               Dur := Float(Clock-Full_Start_Time);
               Put_Line(File_Log,"Real_time: "&Duration'Image(Ada.Real_Time.To_Duration(Real_Dur))&" Durée: "&Float'Image(Dur));
               Flush(File_Log);
         end;
      end if;
      Movenum := Movenum+1;
      Put_Line(File_log,"Out of main loop. From:"&Integer'Image(Move_From)&
		 " To:"&Integer'Image(Move_To)&" Res:"&Integer'Image(Res));
      Flush(File_Log);
      if Move_From=-1 and Old_From /=-1 then 
	 Move_From := Old_From;Move_To := Old_To;
      end if;
      if Move_From = -1 then 
	 Put_Line(File_Log,"Move_from=-1, exiting loop");
	 Flush(File_Log);
	 exit; 
      end if;

      if Movenum mod 2 = 1 then Put_Line(Game_Log,"");Put(Game_Log,Integer'Image(Movenum/2+1)&".");end if;
      if (Xotime>6000) and (Col=My_Col) and ((My_Col=White and Res<-500) or  (My_Col=Black and Res>500)) then 
	 Resign; 
	 Put(Game_Log," resign");
	 Put_Line(File_Log,"Resigning");
	 exit;
      end if;
      Put(Game_Log," "&San(Move_From,Move_To,En_Passant));
      Really_Do_Move (Move_From,Move_To,Col,Cast,En_Passant,P2,Promote,N_En_Passant,Valid,N_Castles);
      if Is_In_Check(-Col) then Put(Game_Log,"+"); end if;
      
      if Col/=My_Col then
	   Put(Game_Log," {");
	   Put(Game_Log,"[%clk ");
	   Put(Game_Log,Convert_Clock((Xotime+50)/100));
	   Put(Game_Log,"]");
	   Put(Game_Log,"}");
	   Flush(Game_Log);
      end if;
      Put_Line(File_Log,"Piece:"&Piece'Image(P2)&" Value:"&Integer'Image(Piece_Value(P2))&" Promote:"&Boolean'Image(Promote));
      Flush(File_Log);
      if not Valid then
         Put_Line(File_Log,"Error. Not valid");
         Flush(File_Log);
         raise Main_Error;
        end if;
      Print_Chessboard(File_Log);
      Print_Fen(File_Log,-Col,Cast,En_Passant);
      if Promote then
         if Col=White then
            Matw := Matw-(Piece_Value(White_Pawn)-Piece_Value(White_Queen));
            Matb := Matb - Piece_Value(P2);
         else
            Matb := Matb-(Piece_Value(Black_Pawn)-Piece_Value(Black_Queen));
            Matw := Matw - Piece_Value(P2);
         end if;
      else
         if Col=White then
            Matb := Matb-Piece_Value(P2);
         else
            Matw := Matw-Piece_Value(P2);
         end if;
      end if;
      Cast := N_Castles;En_Passant := N_En_Passant;Col := -Col;
      if Promote or P2/= Empty or Chess_Board(Move_To)=Black_Pawn or Chess_Board(Move_To)=White_Pawn then
         First_Ply := Movenum;
      end if;
      Put_Line(File_Log,"Material: "&Integer'Image(Matw)&" "&Integer'Image(Matb));
   end loop;
   while True loop
      Read_Xboard(-1,-1);
   end loop;
exception
   when Sortie =>
      Put_Line(File_Log,"Sortie raised in main, exiting");
      Flush(File_Log);
   when Main_Error =>
      Put_Line(File_Log,Integer'Image(Max_Prof));
      Put_Line(File_Log,"Main error raise. Exiting");
      Flush(File_Log);
   when Error : others =>
      Put_Line(File_Log,Integer'Image(Max_Prof));
      Put_Line(File_Log,"Exception:"&Ada.Exceptions.Exception_Name(Error)&" raised in main. Exiting.");
--      Put_Line(File_Log,GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
      Flush(File_Log);
end Main;

