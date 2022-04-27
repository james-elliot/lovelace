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

with Bitboards;
use Bitboards;
with Bitscan;
use Bitscan;
with Text_Io;
use Text_Io;
with Interfaces;
use Interfaces;
with Hash;
use Hash;
with Util;
use Util;
with Move;
use move;
with Ada.Exceptions;
use Ada.Exceptions;

package body Eval is
   File_Mask_I : array(0..7) of Intboard:=(others=>0);
   White_King_Bonus : array(0..63) of Integer :=
     (15,15,0,0,0,0,15,15,
      10,10,0,-5,-5,0,10,10,
      -5,-5,-10,-20,-20,-10,-5,-5,
      -20,-20,-20,-20,-20,-20,-20,-20,
      -20,-20,-20,-20,-20,-20,-20,-20,
      -20,-20,-20,-20,-20,-20,-20,-20,
      -20,-20,-20,-20,-20,-20,-20,-20,
      -20,-20,-20,-20,-20,-20,-20,-20);
   Black_King_Bonus : array(0..63) of Integer :=
     (-20,-20,-20,-20,-20,-20,-20,-20,
      -20,-20,-20,-20,-20,-20,-20,-20,
      -20,-20,-20,-20,-20,-20,-20,-20,
      -20,-20,-20,-20,-20,-20,-20,-20,
      -20,-20,-20,-20,-20,-20,-20,-20,
      -5,-5,-10,-20,-20,-10,-5,-5,
      10,10,0,-5,-5,0,10,10,
      15,15,0,0,0,0,15,15);

   White_Pawn_Bonus : array (0..63) of Integer := (others=>0);
   Black_Pawn_Bonus : array (0..63) of Integer := (others=>0);
   White_Pawn_Bonus2 : array (0..63) of Integer := (others=>0);
   Black_Pawn_Bonus2 : array (0..63) of Integer := (others=>0);
   Opposing_Black_Pawns : array (0..63) of Intboard := (others=>0);
   Opposing_White_Pawns : array (0..63) of Intboard := (others=>0);

   King_Files_first : array(0..7) of Integer:=(0,0,0,2,2,5,5,6);
   King_Files_Last : array(0..7) of Integer :=(1,2,2,5,5,7,7,7);


   function White_King_Safety(Coefb:Integer;Cast:Castling) return Integer is
      Pos : Integer;
      Open_White,Open_Black,Open_Both,Bad_Rooks,Bad_Rooks_Both : Integer := 0;
      P_Defenders : Integer;
      Castle : Integer :=0;
      To, Danger : Integer;
      Vicinity : Intboard;
      B:Intboard;
   begin
      if Coefb<=0 then Return 0; end if;
      
      if Cast.Wkd then
         castle := 30;
      elsif Cast.Wqd then
         castle := 20;
      elsif (not Cast.Wkc) and then (not Cast.Wqc) then
         castle := -25;
      end if;

      Pos := First_One(White_Kings_I);
      P_Defenders := Count_Bits(White_Pawns_I and White_King_Defenders_I(Pos));
      if Pos/8 = 0 then P_Defenders := P_Defenders*2; end if;
      for I in King_Files_First(Pos mod 8)..King_Files_last(Pos mod 8) loop
         if ((Black_Pawns_I or White_Pawns_I) and File_Mask_I(I)) = 0 then
            Open_Both := Open_Both+1;
            Bad_Rooks_Both := Bad_Rooks_Both+Count_Bits(File_Mask_I(I) and (Black_Rooks_I or Black_Queens_I));
	 else
	    if (White_Pawns_I and File_Mask_I(I))=0 then
	       Open_White := Open_White+1;
	    end if;
	    if (Black_Pawns_I and File_Mask_I(I))=0 then
	       Open_black := Open_Black+1;
	       Bad_Rooks := Bad_Rooks+Count_Bits(File_Mask_I(I) and (Black_Rooks_I or Black_Queens_I));
	    end if;
         end if;
      end loop;
      
      Vicinity := King_Vicinity_I(Pos);
      Danger := 0;
      while Vicinity/= 0 loop
         To := First_One(Vicinity);
         Vicinity := Vicinity xor Set_Mask_I(To);
         Danger := Danger + Count_Bits(Black_Attacks_To(To));
      end loop;
--      Put_Line(Integer'Image(White_King_Bonus(Pos))&Integer'Image(Open_White)&Integer'Image(Open_Black)
--               &Integer'Image(Open_Both)&Integer'Image(P_Defenders)&Integer'Image(Bad_Rooks)&integer'image(danger));
      return (Coefb*(Castle+White_King_Bonus(Pos)-16*(Open_White+Open_Black+2*Open_Both)
		       +16*P_Defenders-16*Bad_Rooks-32*Bad_Rooks_Both-2*Danger))/10;
   end White_King_Safety;

   function Black_King_Safety(Coefw:Integer;Cast:Castling) return Integer is
      Pos : Integer;
      Open_White,Open_Black,Open_Both,Bad_Rooks,Bad_Rooks_Both : Integer := 0;
      P_Defenders : Integer;
      Castle : Integer :=0;
      To, Danger : Integer;
      Vicinity : Intboard;
   begin

      if Coefw<=0 then Return 0; end if;

      if Cast.Bkd then
         castle := 30;
      elsif Cast.Bqd then
         castle := 20;
      elsif (not Cast.Bkc) and then (not Cast.Bqc) then
         castle := -25;
      end if;

      Pos := First_One(Black_Kings_I);
      P_Defenders := Count_Bits(Black_Pawns_I and Black_King_Defenders_I(Pos));
      if Pos/8=7 then P_Defenders := P_Defenders*2; end if;
      for I in King_Files_First(Pos mod 8)..King_Files_last(Pos mod 8) loop
         if ((Black_Pawns_I or White_Pawns_I) and File_Mask_I(I)) = 0 then
            Open_Both := Open_Both+1;
            Bad_Rooks_Both := Bad_Rooks_Both+Count_Bits(File_Mask_I(I) and (White_Rooks_I or White_Queens_I));
	 else
	    if (White_Pawns_I and File_Mask_I(I))=0 then
	       Open_White := Open_White+1;
	       Bad_Rooks := Bad_Rooks+Count_Bits(File_Mask_I(I) and (White_Rooks_I or White_Queens_I));
	    end if;
	    if (Black_Pawns_I and File_Mask_I(I))=0 then
	       Open_black := Open_Black+1;
	    end if;
         end if;
      end loop;

      Vicinity := King_Vicinity_I(Pos);
      Danger := 0;
      while Vicinity/= 0 loop
         To := First_One(Vicinity);
         Vicinity := Vicinity xor Set_Mask_I(To);
         Danger := Danger + Count_Bits(White_Attacks_To(To));
      end loop;
      return -(Coefw*(Castle+Black_King_Bonus(Pos)-16*(Open_White+Open_Black+2*Open_Both)
			+16*P_Defenders-16*Bad_Rooks-32*Bad_Rooks_Both+2*Danger))/10;
   end Black_King_Safety;


   function White_Pawn_Struct(Coefb:Integer) return Integer is
      B : Intboard := White_Pawns_I;
      From : Integer;
      Passed,Doubled,Supported : Boolean;
      Struct,Pos,Advance : Integer := 0;
      File : Integer;
   begin
      while B /=0 loop
         From := First_One(B);
         File := From mod 8;
         B := B xor Set_Mask_I(From);
         Passed := (Opposing_Black_Pawns(From) and Black_Pawns_I) = 0;
         Doubled := ((File_Mask_I(File) and White_Pawns_I) xor Set_Mask_I(From))/=0;
         Supported := (Black_Pawn_Attacks_I(From) and White_Pawns_I) /=0;
         if Passed then
            Struct := Struct+20;
         end if;
         if Supported then
            Struct := Struct+5;
         end if;
         if Passed and Supported then
            Struct := Struct+10;
         end if;
         if Doubled then
            Struct := Struct - 20;
         end if;
         Advance := Advance+White_Pawn_Bonus(From);
         Pos := Pos+White_Pawn_Bonus2(From);
      end loop;
      return Struct+(Pos*Coefb+(10-Coefb)*Advance)/10;
   end White_Pawn_Struct;

   function Black_Pawn_Struct(Coefw:Integer) return Integer is
      B : Intboard := Black_Pawns_I;
      From : Integer;
      Passed,Doubled,Supported : Boolean;
      Struct,Pos,Advance : Integer := 0;
      File : Integer;
   begin
      while B /=0 loop
         From := First_One(B);
         File := From mod 8;
         B := B xor Set_Mask_I(From);
         Passed := (Opposing_White_Pawns(From) and White_Pawns_I) = 0;
         Doubled := ((File_Mask_I(File) and Black_Pawns_I) xor Set_Mask_I(From))/=0;
         Supported := (White_Pawn_Attacks_I(From) and Black_Pawns_I) /=0;
         if Passed then
            Struct := Struct+20;
         end if;
         if Supported then
            Struct := Struct+5;
         end if;
         if Passed and Supported then
            Struct := Struct+10;
         end if;
         if Doubled then
            Struct := Struct - 20;
         end if;
         Advance := Advance+Black_Pawn_Bonus(From);
         Pos := Pos+Black_Pawn_Bonus2(From);
      end loop;
      return -(Struct+(Pos*Coefw+(10-Coefw)*Advance)/10);
   end Black_Pawn_Struct;


   function Piece_Mobility(P : Piece;I : Intboard) return Integer is
      Tmp,From : Integer;
      Mobility : Integer := 0;
      B : Intboard := I;
   begin
      while B/=0 loop
         From := First_One(B);
         B := B xor Set_Mask_I(From);
         if Sliding_Mover(P) then
            if Rook_Mover(P) then
               tmp := Find_Rank(From);
               mobility := Mobility + Rank_Mobility(from,tmp);
               tmp := Find_Rank_90(From);
               Mobility := Mobility + Rank_Mobility_90(from,tmp);
            end if;
            if Bishop_Mover(P) then
               tmp := Find_Rank_45r(From);
               mobility := Mobility + Rank_Mobility_45r(from,tmp);
               tmp := Find_Rank_45l(From);
               mobility := Mobility + Rank_Mobility_45l(from,tmp);
            end if;
         elsif P = Knight then
            Mobility := Knight_Mobility(From);
         end if;
      end loop;
      return Mobility;
   end Piece_Mobility;
   pragma Inline(Piece_Mobility);

   function Valeur(C:Color;Matw:Integer;Matb:Integer;Cast:Castling;Int_Prof:integer) return Integer is
      Mob : Integer :=0;
      Pos : Integer := 0;
      Coefb,Coefw,Tmp,Tmp2,Wks,Bks,Wps,Bps : Integer;
   begin

      if Is_In_Ptable then
         Tmp := Get_From_Ptable;
         if Tmp/=32765 then return Tmp; end if;
      end if;

      Coefb := (-Matb/100-Count_Bits(Black_Pawns_I))-10;
      if Coefb<0 then Coefb:=0; end if;
      if Coefb>10 then Coefb:=10; end if;
      Coefw := (Matw/100-Count_Bits(White_Pawns_I))-10;
      if Coefw<0 then Coefw:=0; end if;
      if Coefw>10 then Coefw:=10; end if;
      Mob := Mob + Piece_Mobility(White_Bishop,White_Bishops_I)*2;
      Mob := Mob + Piece_Mobility(White_Rook,White_Rooks_I)*2;
      Mob := Mob + Piece_Mobility(White_Queen,White_Queens_I)/2;
      Mob := Mob + Piece_Mobility(White_Knight,White_Knights_I)*4;

      Mob := Mob - Piece_Mobility(Black_Bishop,Black_Bishops_I)*2;
      Mob := Mob - Piece_Mobility(Black_Rook,Black_Rooks_I)*2;
      Mob := Mob - Piece_Mobility(Black_Queen,Black_Queens_I)/2;
      Mob := Mob - Piece_Mobility(Black_Knight,Black_Knights_I)*4;
      -- Pair of bishops
      if Count_Bits (White_Bishops_I) >=2 then Pos := Pos+30; end if;
      if Count_Bits (Black_Bishops_I) >=2 then Pos := Pos-30; end if;

      Wks:=White_King_Safety(Coefb,cast);
      Bks:=Black_King_Safety(Coefw,cast);
      Wps:=White_Pawn_Struct(Coefb);
      Bps:=Black_Pawn_Struct(Coefw);
      Tmp := Pos+Mob+Matw+Matb+
        Wks+Bks+
        Wps+Bps+
        (100*(Matw+Matb))/(Matw-Matb+100);
      -- We exclude 0 (draw by repetition) and 1 (draw by pat)
      if Tmp>=0 then Tmp:=Tmp+2; end if;
      Store_To_Ptable(Tmp);
      return Tmp;
   exception
      when Error : others =>
         Put_Line(File_Log,"Exception in eval:"&Ada.Exceptions.Exception_Name(Error)&". Exiting.");
         Flush(File_Log);
         raise;
   end Valeur;


   function SEE (C:Color) return Integer is
      Wa,Ba,Wdone,Wtodo,Bdone,Btodo : Intboard;
      From : Integer;

      procedure Test(A : Intboard; D : in out Intboard; I : Intboard ; B : out Boolean;
                                                                       To_Do : out natural) is
         Tmp : Intboard;
      begin
         Tmp := (A and I);
         if  Tmp /= 0 then
            To_Do := First_One(Tmp);
            D := D xor Set_Mask_I(To_do);
            B := True;
         else
            B := False;
         end if;
      end Test;
      pragma Inline(Test);

      function Test_Black return Integer is
         B : Boolean;
         T : Natural;
         Nwa,Nba : Intboard;
      begin
         Test(Btodo,Bdone,Black_Pawns_I,B,T);
         if B then
            Btodo := Ba xor Bdone;
            return Piece_Value(Black_Pawn);
         end if;
         Test(Btodo,Bdone,Black_Knights_I,B,T);
         if B then
            Btodo := Ba xor Bdone;
            return Piece_Value(Black_Knight);
         end if;
         Test(Btodo,Bdone,Black_Bishops_I,B,T);
         if B then
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(T);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(T);
            All_Diag_Attacks_To(From,Nwa,Nba);
            Wa := Wa or Nwa;
            Ba := Ba or Nba;
            Btodo := Ba xor Bdone;
            return Piece_Value(Black_Bishop);
         end if;
         Test(Btodo,Bdone,Black_Rooks_I,B,T);
         if B then
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(T);
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(T);
            All_Ortho_Attacks_To(From,Nwa,Nba);
            Wa := Wa or Nwa;
            Ba := Ba or Nba;
            Btodo := Ba xor Bdone;
            return Piece_Value(Black_Rook);
         end if;
         Test(Btodo,Bdone,Black_Queens_I,B,T);
         if B then
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(T);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(T);
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(T);
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(T);
            All_Diag_Attacks_To(From,Nwa,Nba);
            Wa := Wa or Nwa;
            Ba := Ba or Nba;
            All_Ortho_Attacks_To(From,Nwa,Nba);
            Wa := Wa or Nwa;
            Ba := Ba or Nba;
            Btodo := Ba xor Bdone;
            return Piece_Value(Black_Queen);
         end if;
         Test(Btodo,Bdone,Black_Kings_I,B,T);
         if B then
            Btodo := Ba xor Bdone;
            return Piece_Value(Black_King);
         end if;
         raise Eval_Error;
      end Test_Black;
      pragma Inline(Test_Black);

      function Test_White return Integer is
         B : Boolean;
         T : Natural;
         Nwa,Nba : Intboard;
      begin
         Test(Wtodo,Wdone,White_Pawns_I,B,T);
         if B then
            Wtodo := Wa xor Wdone;
            return Piece_Value(White_Pawn);
         end if;
         Test(Wtodo,Wdone,White_Knights_I,B,T);
         if B then
            Wtodo := Wa xor Wdone;
            return Piece_Value(White_Knight);
         end if;
         Test(Wtodo,Wdone,White_Bishops_I,B,T);
         if B then
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(T);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(T);
            All_Diag_Attacks_To(From,Nwa,Nba);
            Wa := Wa or Nwa;
            Ba := Ba or Nba;
            Wtodo := Wa xor Wdone;
            return Piece_Value(White_Bishop);
         end if;
         Test(Wtodo,Wdone,White_Rooks_I,B,T);
         if B then
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(T);
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(T);
            All_Ortho_Attacks_To(From,Nwa,Nba);
            Wa := Wa or Nwa;
            Ba := Ba or Nba;
            Wtodo := Wa xor Wdone;
            return Piece_Value(White_Rook);
         end if;
         Test(Wtodo,Wdone,White_Queens_I,B,T);
         if B then
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(T);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(T);
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(T);
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(T);
            All_Diag_Attacks_To(From,Nwa,Nba);
            Wa := Wa or Nwa;
            Ba := Ba or Nba;
            All_Ortho_Attacks_To(From,Nwa,Nba);
            Wa := Wa or Nwa;
            Ba := Ba or Nba;
            Wtodo := Wa xor Wdone;
            return Piece_Value(White_Queen);
         end if;
         Test(Wtodo,Wdone,White_Kings_I,B,T);
         if B then
            Wtodo := Wa xor Wdone;
            return Piece_Value(White_King);
         end if;
         raise Eval_Error;
      end Test_White;
      pragma Inline(Test_White);

      W : Intboard := White_Pieces_I;
      B : Intboard := Black_Pieces_I;
      S1,S2,S3,S4 : Intboard;
      V : Integer;
      I : Integer;
      Nvw, Nvb : Integer;
      Values : array(0..32) of Integer;
      Tot_Val : Integer := 0;

   begin
      if C = Black then
         while (W /= 0) loop
            From := First_One(W);
            W := W xor Set_Mask_I (From);
            All_Attacks_To(From,Wa,Ba);
            if Ba /= 0 then
               S1 := All_Pieces_I;
               S2 := All_Pieces_90_I;
               S3 := All_Pieces_45l_I;
               S4 := All_Pieces_45r_I;
               --                  Put_Line("from:"&Integer'Image(From));
               --                  Put_Line("Wa:");
               --                  Print_Intboard(Wa);
               --                  Put_Line("Ba:");
               --                  Print_Intboard(Ba);
               V := 0;
               Btodo := Ba;
               Wtodo := Wa;
               Bdone := 0;
               Wdone := 0;
               Values(0) := V;
               I := 1;
               Nvw := Piece_Value(Chess_Board(From));
               -- Currently only minimax. Should do alpha/beta
               while Btodo /= 0 loop -- No attack, end SEE
                                     --                     Put_Line("Wtodo:");
                                     --                     Print_Intboard(Wtodo);
                                     --                     Put_Line("Btodo");
                                     --                     Print_Intboard(Btodo);
                  Nvb := Test_Black;
                  --                     Put_Line("nvb:"&Integer'Image(Nvb));
                  --Can't take with the king if retake
                  if Wtodo /= 0 and Nvb=Piece_Value(Black_King) then exit; end if;
                  V := V+Nvw;
                  --                     Put_Line("v:"&Integer'Image(v));
                  Values(I) := V;
                  I := I+1;
                  --No attack -> End SEE
                  if Wtodo = 0 then exit; end if;
                  --White's turn
                  --                   Put_Line("Wtodo2:");
                  --                     Print_Intboard(Wtodo);
                  --                     Put_Line("Btodo2:");
                  --                     Print_Intboard(Btodo);
                  Nvw:= Test_White;
                  --                     Put_Line("nvw:"&Integer'Image(Nvw));
                  --Can't take with the king if retake
                  if Btodo /= 0 and Nvw =Piece_Value(White_King) then exit; end if;
                  V := V+Nvb;
                  --                     Put_Line("v:"&Integer'Image(v));
                  Values(I) := V;
                  I := I+1;
               end loop;
               V := Values(I-1);
               for J in reverse 0..I-2 loop
                  if J mod 2 = 0 then -- MAX
                     V := Integer'Max(V,Values(J));
                  else --MIN
                     V := Integer'Min(V,Values(J));
                  end if;
               end loop;
               Tot_Val := Integer'Max(Tot_Val,V);
               All_Pieces_I :=  S1;
               All_Pieces_90_I := S2;
               All_Pieces_45l_I := S3;
               All_Pieces_45r_I := S4;
            end if;
         end loop;
         return Tot_Val;
      else
         while (B /= 0) loop
            From := First_One(B);
            B := B xor Set_Mask_I (From);
            All_Attacks_To(From,Wa,Ba);
            if Wa /= 0 then
               S1 := All_Pieces_I;
               S2 := All_Pieces_90_I;
               S3 := All_Pieces_45l_I;
               S4 := All_Pieces_45r_I;
               --                  Put_Line("from:"&Integer'Image(From));
               --                  Put_Line("Wa:");
               --                  Print_Intboard(Wa);
               --                  Put_Line("Ba:");
               --                  Print_Intboard(Ba);
               V := 0;
               Btodo := Ba;
               Wtodo := Wa;
               Bdone := 0;
               Wdone := 0;
               Values(0) := V;
               I := 1;
               Nvb := Piece_Value(Chess_Board(From));
               -- Currently only minimax. Should do alpha/beta
               while Wtodo /= 0 loop -- No attack, end SEE
                                     --                    Put_Line("Wtodo:");
                                     --                    Print_Intboard(Wtodo);
                                     --                    Put_Line("Btodo");
                                     --                    Print_Intboard(Btodo);
                  Nvw := Test_White;
                  --                    Put_Line("nvw:"&Integer'Image(Nvw));
                  --Can't take with the king if retake
                  if Btodo /= 0 and Nvw>500 then exit; end if;
                  V := V+Nvb;
                  --                     Put_Line("v:"&Integer'Image(v));
                  Values(I) := V;
                  I := I+1;
                  --No attack -> End SEE
                  if Btodo = 0 then exit; end if;
                  --White's turn
                  --                     Put_Line("Wtodo2:");
                  --                     Print_Intboard(Wtodo);
                  --                     Put_Line("Btodo2:");
                  --                     Print_Intboard(Btodo);
                  Nvb:= Test_Black;
                  --                     Put_Line("nvb:"&Integer'Image(Nvb));
                  --Can't take with the king if retake
                  if Wtodo /= 0 and Nvb <-500 then exit; end if;
                  V := V+Nvw;
                  --                     Put_Line("v:"&Integer'Image(v));
                  Values(I) := V;
                  I := I+1;
               end loop;
               V := Values(I-1);
               for J in reverse 0..I-2 loop
                  if J mod 2 = 0 then -- MIN
                     V := Integer'Min(V,Values(J));
                  else --MAX
                     V := Integer'Max(V,Values(J));
                  end if;
               end loop;
               Tot_Val := Integer'Min(Tot_Val,V);
               All_Pieces_I :=  S1;
               All_Pieces_90_I := S2;
               All_Pieces_45l_I := S3;
               All_Pieces_45r_I := S4;
            end if;
         end loop;
         return Tot_Val;
      end if;
   end SEE;

   Bonus,Bonus2  : Integer;
begin
   for I in 0..7 loop
      for J in 0..7 loop
         File_Mask_I(I) := File_Mask_I(I) or Shift_Left(1,8*J+I);
      end loop;
   end loop;
   for I in 0..7 loop
      for J in 0..7 loop
         for K in -2..2 loop
            if (K>-2 and K<2) or (I=0) or (I=7) then
               if (I+K)<8 and then (I+K)>=0 then
                  for L in -2..2 loop
                     if (J+L)<8 and then (J+L)>=0 then
                        if K/=0 or L/=0 then
                           King_Vicinity_I(8*J+I) := King_Vicinity_I(8*J+I) or Shift_Left(1,8*(J+L)+I+K);
                        end if;
                     end if;
                  end loop;
               end if;
            end if;
         end loop;
      end loop;
   end loop;
   for I in 0..7 loop
      for J in 0..1 loop
         White_King_Defenders_I(8*J+I) := Shift_Left(1,8*(J+1)+I);
         if (I+1)<8 then
            White_King_Defenders_I(8*J+I) := White_King_Defenders_I(8*J+I) or Shift_Left(1,8*(J+1)+I+1);
         end if;
         if (I-1)>=0 then
            White_King_Defenders_I(8*J+I) := White_King_Defenders_I(8*J+I) or Shift_Left(1,8*(J+1)+I-1);
         end if;
      end loop;
   end loop;
   for I in 0..7 loop
      for J in 6..7 loop
         Black_King_Defenders_I(8*J+I) := Shift_Left(1,8*(J-1)+I);
         if (I+1)<8 then
            Black_King_Defenders_I(8*J+I) := Black_King_Defenders_I(8*J+I) or Shift_Left(1,8*(J-1)+I+1);
         end if;
         if (I-1)>=0 then
            Black_King_Defenders_I(8*J+I) := Black_King_Defenders_I(8*J+I) or Shift_Left(1,8*(J-1)+I-1);
         end if;
      end loop;
   end loop;

   for I in 0..7 loop
      Bonus := I*I*I*I*I/128;
      for J in 0..7 loop
         Bonus2 := 0;
         if (J=3 or J=4) then
            if I=2 or I=5 then
               Bonus2 := 5;
            end if;
            if I=3 or I=4 then
               Bonus2 := 10;
            end if;
         end if;
         White_Pawn_Bonus(8*I+J) := Bonus;
         White_Pawn_Bonus2(8*I+J) := Bonus2;
         Black_Pawn_Bonus(8*(7-I)+J) := Bonus;
         Black_Pawn_Bonus2(8*(7-I)+J) := Bonus2;

         for K in I+1..6 loop
            Opposing_Black_Pawns(8*I+J) := Opposing_Black_Pawns(8*I+J) or Shift_Left(1,8*K+J);
            if (J-1)>=0 then
               Opposing_Black_Pawns(8*I+J) := Opposing_Black_Pawns(8*I+J) or Shift_Left(1,8*K+J-1);
            end if;
            if (J+1) <=7 then
               Opposing_Black_Pawns(8*I+J) := Opposing_Black_Pawns(8*I+J) or Shift_Left(1,8*K+J+1);
            end if;
         end loop;

         for K in 1..I-1 loop
            Opposing_White_Pawns(8*I+J) := Opposing_White_Pawns(8*I+J) or Shift_Left(1,8*K+J);
            if (J-1)>=0 then
               Opposing_White_Pawns(8*I+J) := Opposing_White_Pawns(8*I+J) or Shift_Left(1,8*K+J-1);
            end if;
            if (J+1) <=7 then
               Opposing_White_Pawns(8*I+J) := Opposing_White_Pawns(8*I+J) or Shift_Left(1,8*K+J+1);
            end if;
         end loop;

      end loop;
   end loop;


end Eval;
