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
with Eval;
use Eval;
with Hash;
use Hash;
with Util;
use Util;
with Ada.Exceptions;
use Ada.Exceptions;

package body Move is
   Debug : Boolean := False;
   
   Not_Checkmated : exception;
   type Stacks is
      record
         From : Integer;
         To : Integer;
      end record;
   Stack : array (0..1000) of Stacks;

   procedure Dump_Stack(Curr_Prof:Integer) is
   begin
      for I in 0..Curr_Prof loop
         Put(File_Log,"("&Integer'Image(Stack(I).From)&","&Integer'Image(Stack(I).To)&")");
      end loop;
      Put_Line(File_Log,"");
   end Dump_Stack;

   procedure Store_Stack(Int_Prof:Integer;From:Integer;To:Integer) is
   begin
      Stack(Int_Prof).From:=From;
      Stack(Int_Prof).To:=To;
   end Store_Stack;

   procedure Do_Castle(From:Integer;
                          To:Integer;
                          From2:Integer;
                          To2:Integer;
                          P1:Piece;
                          R1:Piece;
                          C:color) is
      begin
      Chess_Board(From) := Empty; Chess_Board(To) := P1; Chess_Board(From2) := Empty; Chess_Board(To2) := R1;
      All_Boards(P1).all := All_Boards(P1).all xor Set_Mask_I(From) xor Set_Mask_I(To);
      All_Boards(R1).all := All_Boards(R1).all xor Set_Mask_I(From2) xor Set_Mask_I(To2);
      All_Color_Boards(C).all := All_Color_Boards(C).all xor Set_Mask_I(From) xor Set_Mask_I(To) xor Set_Mask_I(From2) xor Set_Mask_I(To2);
      All_Pieces_I := All_Pieces_I xor Set_Mask_I(From) xor Set_Mask_I(To) xor Set_Mask_I(From2) xor Set_Mask_I(To2);
      Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_P(From2,R1) xor Z_P(To2,R1) xor Z_C;
      All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From) xor Set_Mask_90_I(To) xor Set_Mask_90_I(From2) xor Set_Mask_90_I(To2);
      All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From) xor Set_Mask_45l_I(To) xor Set_Mask_45l_I(From2) xor Set_Mask_45l_I(To2);
      All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From) xor Set_Mask_45r_I(To) xor Set_Mask_45r_I(From2) xor Set_Mask_45r_I(To2);
   end Do_Castle;
   pragma Inline(Do_Castle);

   procedure UnDo_Castle(From:Integer;
                         To:Integer;
                         From2:Integer;
                         To2:Integer;
                         P1:Piece;
                         R1:Piece;
                         C:color) is
   begin
      Chess_Board(From) := P1; Chess_Board(To) := Empty; Chess_Board(From2) := R1; Chess_Board(To2) := Empty;
      All_Boards(P1).all := All_Boards(P1).all xor Set_Mask_I(From) xor Set_Mask_I(To);
      All_Boards(R1).all := All_Boards(R1).all xor Set_Mask_I(From2) xor Set_Mask_I(To2);
      All_Color_Boards(C).all := All_Color_Boards(C).all xor Set_Mask_I(From) xor Set_Mask_I(To) xor Set_Mask_I(From2) xor Set_Mask_I(To2);
      All_Pieces_I := All_Pieces_I xor Set_Mask_I(From) xor Set_Mask_I(To) xor Set_Mask_I(From2) xor Set_Mask_I(To2);
      Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_P(From2,R1) xor Z_P(To2,R1) xor Z_C;
      All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From) xor Set_Mask_90_I(To) xor Set_Mask_90_I(From2) xor Set_Mask_90_I(To2);
      All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From) xor Set_Mask_45l_I(To) xor Set_Mask_45l_I(From2) xor Set_Mask_45l_I(To2);
      All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From) xor Set_Mask_45r_I(To) xor Set_Mask_45r_I(From2) xor Set_Mask_45r_I(To2);
   end UnDo_Castle;
   pragma Inline(UnDo_Castle);


   function Gen_Moves (Alpha : Integer;
                       Beta : Integer;
                       C:Color;
                       En_Passant : Integer;
                       Castles : Castling;
                       Int_Prof : Integer;
                       Qs : Boolean;
                       Prof:Integer;
                       MatW : Integer;
                       MatB : Integer;
                       Twice : Boolean) return Integer is
      All_Moves : array (0..127) of Move_Struct;
      Nb_Moves : Integer := 1;
      Bad_Moves : array (0..127) of Move_Struct;
      Nb_Bad  : Integer := 0;
      Simple_Moves : array (0..127) of Move_Struct;
      Nb_simple  : Integer := 0;

   procedure Do_Move (From:Integer;
                      To:Integer;
                      C:Color;
                      P2 : out Piece;
                      Promote: out Boolean;
                      N_En_Passant : out Integer;
                      Valid : out Boolean;
                      N_Castles : out Castling;
                      Ms : boolean) is
      P1 : Piece;
      Pos : Integer;
   begin
      Valid := True;
      P1 := Chess_Board(From);
      P2 := Empty;
      Promote := False;
      N_En_Passant := -1;
      N_Castles := Castles;
      if From=4 and then To = 2 and then P1 = White_King then --White Castling Queenside
         Do_Castle(4,2,0,3,White_King,White_Rook,C);
	 N_Castles.Wqd := True;
	 Z_I := Z_I xor Z_Wqd;
      elsif From=4 and then To = 6 and then P1 = White_King then --White Castling Kingside
         Do_Castle(4,6,7,5,White_King,White_Rook,C);
	 N_Castles.Wkd := True;
	 Z_I := Z_I xor Z_Wkd;
      elsif From=60 and then To = 58 and then P1 = Black_King then --Black Castling Queenside
         Do_Castle(60,58,56,59,Black_King,Black_Rook,C);
	 N_Castles.Bqd := True;
	 Z_I := Z_I xor Z_Bqd;
      elsif From=60 and then To = 62 and then P1 = Black_King then --Black Castling Kingside
         Do_Castle(60,62,63,61,Black_King,Black_Rook,C);
	 N_Castles.Bkd := True;
	 Z_I := Z_I xor Z_Bkd;
      elsif To=En_Passant and then ((P1 = White_Pawn) or (P1=Black_Pawn)) then -- En passant
         if C=White then
            P2 := Black_Pawn;
            Chess_Board(From) := Empty;
            Chess_Board(To) := White_Pawn;
            Chess_Board(To-8) := Empty;
            White_Pawns_I := White_Pawns_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            Black_Pawns_I := Black_Pawns_I xor Set_Mask_I(To-8);
            White_Pieces_I  := White_Pieces_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            Black_Pieces_I := Black_Pieces_I xor Set_Mask_I(To-8);
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From) xor Set_Mask_I(To) xor Set_Mask_I(To-8);
            Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_P(To-8,P2) xor Z_C;
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From) xor Set_Mask_90_I(To) xor Set_Mask_90_I(To-8);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From) xor Set_Mask_45l_I(To) xor Set_Mask_45l_I(To-8);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From) xor Set_Mask_45r_I(To) xor Set_Mask_45r_I(To-8);
         else
            P2 := White_Pawn;
            Chess_Board(From) := Empty;
            Chess_Board(To) := Black_Pawn;
            Chess_Board(To+8) := Empty;
            Black_Pawns_I := Black_Pawns_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            White_Pawns_I := White_Pawns_I xor Set_Mask_I(To+8);
            Black_Pieces_I  := Black_Pieces_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            White_Pieces_I := White_Pieces_I xor Set_Mask_I(To+8);
            Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_P(To+8,P2) xor Z_C;
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From) xor Set_Mask_I(To) xor Set_Mask_I(To+8);
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From) xor Set_Mask_90_I(To) xor Set_Mask_90_I(To+8);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From) xor Set_Mask_45l_I(To) xor Set_Mask_45l_I(To+8);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From) xor Set_Mask_45r_I(To) xor Set_Mask_45r_I(To+8);
         end if;
      else --Everything else (nocastling, no en passant)
         P2 := Chess_Board(To);
--	 if P2=White_King or P2=Black_King then 
--	    Put_Line(File_Log,"Move_king_error: From:"&Integer'Image(From)&" To:"&Integer'Image(To)&" Ms:"&Boolean'Image(Ms));
--	    raise Move_King_Error; 
--	 end if;
         Chess_Board(From) := Empty;
         if P1=White_Pawn and then (To/8)=7 then
            White_Pawns_I := White_Pawns_I xor Set_Mask_I(From);
            White_Queens_I := White_Queens_I xor Set_Mask_I(To);
            Chess_Board(To) := White_Queen;
            Z_I := Z_I xor Z_P(From,White_Pawn) xor Z_P(To,White_Queen) xor Z_C;
            if P2/=Empty then Z_I := Z_I xor Z_P(To,P2); end if;
            Promote := True;
         elsif P1=Black_Pawn and then (To/8)=0 then
            Black_Pawns_I := Black_Pawns_I xor Set_Mask_I(From);
            Black_Queens_I := Black_Queens_I xor Set_Mask_I(To);
            Chess_Board(To) := Black_Queen;
            Z_I := Z_I xor Z_P(From,Black_Pawn) xor Z_P(To,Black_Queen) xor Z_C;
            if P2/=Empty then Z_I := Z_I xor Z_P(To,P2); end if;
            Promote := True;
         else
            Chess_Board(To) := P1;
            All_Boards(P1).all := All_Boards(P1).all xor Set_Mask_I(From) xor Set_Mask_I(To);
            Promote := False;
         end if;
         All_Color_Boards(C).all := All_Color_Boards(C).all xor Set_Mask_I(From) xor Set_Mask_I(To);
         if P2 = Empty then
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From) xor Set_Mask_I(To);
            if not Promote then Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_C; end if; --promotion already done above
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From) xor Set_Mask_90_I(To);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From) xor Set_Mask_45l_I(To);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From) xor Set_Mask_45r_I(To);
         else
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From);
            if not Promote then Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_P(To,P2) xor Z_C; end if;
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From);
            All_Boards(P2).all := All_Boards(P2).all xor Set_Mask_I(To);
            All_Color_Boards(-C).all := All_Color_Boards(-C).all xor Set_Mask_I(To);
         end if;
         if P1 = White_Pawn and then (To-From)=16 then N_En_Passant := To-8;
         elsif P1 = Black_Pawn and then (From-To)=16 then N_En_Passant := To+8;
         else N_En_Passant := -1; end if;
      end if;
      if C=White then
         Pos := First_One(White_Kings_I);
         Valid := (Black_Attacks_To (Pos) = 0);
	 if Debug then Put_Line(File_Log,"D1:"&Boolean'Image(Valid)); end if;
         if Valid and then (not Ms) and then Qs and then P2=Empty then
            Pos := Last_One(Black_Kings_I);
            Valid := Valid and (White_Attacks_To(Pos) /= 0);
         end if;
	 if Debug then Put_Line(File_Log,"D2:"&Boolean'Image(Valid)); end if;
         -- Rook/King moved, no castling
         if N_Castles.Wqc and then (From=0 or else From=4) then
            Z_I := Z_I xor Z_Wqc;
            N_Castles.Wqc := False;
         end if;
         -- Rook/King moved, no castling
         if N_Castles.Wkc and then (From=7 or else From=4) then
            Z_I := Z_I xor Z_Wkc;
            N_Castles.Wkc := False;
         end if;
         if N_Castles.Bqc and then To=56 then
            Z_I := Z_I xor Z_Bqc;
            N_Castles.Bqc := False;
         end if;
         if N_Castles.Bkc and then To=63 then
            Z_I := Z_I xor Z_Bkc;
            N_Castles.Bkc := False;
         end if;
      else
         Pos := Last_One(Black_Kings_I);
         Valid := (White_Attacks_To (Pos) = 0);
	 if Debug then Put_Line(File_Log,"D1:"&Boolean'Image(Valid)); end if;
         if Valid and then (not Ms) and then Qs and then P2=Empty then
            Pos := First_One(White_Kings_I);
            Valid := Valid and (Black_Attacks_To(Pos) /= 0);
         end if;
	 if Debug then Put_Line(File_Log,"D2:"&Boolean'Image(Valid)); end if;
         if N_Castles.Bqc and then (From=56 or else From=60) then
            Z_I := Z_I xor Z_Bqc;
            N_Castles.Bqc := False;
         end if;
         if N_Castles.Bkc and then (From=63 or else From=60) then
            Z_I := Z_I xor Z_Bkc;
            N_Castles.Bkc := False;
         end if;
         if N_Castles.Wqc and then To=0 then
            Z_I := Z_I xor Z_Wqc;
            N_Castles.Wqc := False;
         end if;
         if N_Castles.Wkc and then To=7 then
            Z_I := Z_I xor Z_Wkc;
            N_Castles.Wkc := False;
         end if;
      end if;
      Nb_Moves_Made := Nb_Moves_Made+1;
   end Do_Move;
   pragma Inline(Do_Move);

   procedure Undo_Move (
                        From:Integer;
                        To:Integer;
                        C:Color;
                        P2 : Piece;
                        Promote : Boolean;
                        N_Castles : Castling) is
      P1 : Piece;
   begin
      P1 := Chess_Board(To);
      if From=4 and then To = 2 and then P1 = White_King then --White Castling Queenside
         UnDo_Castle(4,2,0,3,White_King,White_Rook,C);
      elsif From=4 and then To = 6 and then P1 = White_King then --White Castling Kingside
         UnDo_Castle(4,6,7,5,White_King,White_Rook,C);
      elsif From=60 and then To = 58 and then P1 = Black_King then --Black Castling Queenside
         UnDo_Castle(60,58,56,59,Black_King,Black_Rook,C);
      elsif From=60 and then To = 62 and then P1 = Black_King then --Black Castling Kingside
         UnDo_Castle(60,62,63,61,Black_King,Black_Rook,C);
      elsif To=En_Passant and then ((P1 = White_Pawn) or (P1=Black_Pawn)) then -- En passant
         if C=White then
            Chess_Board(From) := White_Pawn;
            Chess_Board(To) := Empty;
            Chess_Board(To-8) := Black_Pawn;
            White_Pawns_I := White_Pawns_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            Black_Pawns_I := Black_Pawns_I xor Set_Mask_I(To-8);
            White_Pieces_I  := White_Pieces_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            Black_Pieces_I := Black_Pieces_I xor Set_Mask_I(To-8);
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From) xor Set_Mask_I(To) xor Set_Mask_I(To-8);
            Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_P(To-8,P2) xor Z_C;
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From) xor Set_Mask_90_I(To) xor Set_Mask_90_I(To-8);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From) xor Set_Mask_45l_I(To) xor Set_Mask_45l_I(To-8);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From) xor Set_Mask_45r_I(To) xor Set_Mask_45r_I(To-8);
         else
            Chess_Board(From) := Black_Pawn;
            Chess_Board(To) := Empty;
            Chess_Board(To+8) := White_Pawn;
            Black_Pawns_I := Black_Pawns_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            White_Pawns_I := White_Pawns_I xor Set_Mask_I(To+8);
            Black_Pieces_I  := Black_Pieces_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            White_Pieces_I := White_Pieces_I xor Set_Mask_I(To+8);
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From) xor Set_Mask_I(To) xor Set_Mask_I(To+8);
            Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_P(To+8,P2) xor Z_C;
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From) xor Set_Mask_90_I(To) xor Set_Mask_90_I(To+8);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From) xor Set_Mask_45l_I(To) xor Set_Mask_45l_I(To+8);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From) xor Set_Mask_45r_I(To) xor Set_Mask_45r_I(To+8);
         end if;
      else
         Chess_Board(To) := P2;
         --      Put_line(Piece'Image(P2));
         if Promote then
            if C=White then
               White_Pawns_I := White_Pawns_I xor Set_Mask_I(From);
               White_Queens_I := White_Queens_I xor Set_Mask_I(To);
               Chess_Board(From) := White_Pawn;
               Z_I := Z_I xor Z_P(From,White_Pawn) xor Z_P(To,White_Queen) xor Z_C;
               if P2/=Empty then Z_I := Z_I xor Z_P(To,P2); end if;
            else
               Black_Pawns_I := Black_Pawns_I xor Set_Mask_I(From);
               Black_Queens_I := Black_Queens_I xor Set_Mask_I(To);
               Chess_Board(From) := Black_Pawn;
               Z_I := Z_I xor Z_P(From,Black_Pawn) xor Z_P(To,Black_Queen) xor Z_C;
               if P2/=Empty then Z_I := Z_I xor Z_P(To,P2); end if;
            end if;
         else
            Chess_Board(From) := P1;
            All_Boards(P1).all := All_Boards(P1).all xor Set_Mask_I(From) xor Set_Mask_I(To);
         end if;

         All_Color_Boards(C).all := All_Color_Boards(C).all xor Set_Mask_I(From) xor Set_Mask_I(To);
         if P2 = Empty then
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From) xor Set_Mask_I(To);
            if not Promote then Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_C; end if; --promotion already done above
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From) xor Set_Mask_90_I(To);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From) xor Set_Mask_45l_I(To);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From) xor Set_Mask_45r_I(To);
         else
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From);
            if not Promote then Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_P(To,P2) xor Z_C; end if;
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From);
            All_Boards(P2).all := All_Boards(P2).all xor Set_Mask_I(To);
            All_Color_Boards(-C).all := All_Color_Boards(-C).all xor Set_Mask_I(To);
         end if;
      end if;

      if N_Castles.Wqc/=Castles.Wqc then Z_I := Z_I xor Z_Wqc; end if;
      if N_Castles.Wkc/=Castles.Wkc then Z_I := Z_I xor Z_Wkc; end if;
      if N_Castles.Bqc/=Castles.Bqc then Z_I := Z_I xor Z_Bqc; end if;
      if N_Castles.Bkc/=Castles.Bkc then Z_I := Z_I xor Z_Bkc; end if;

      if N_Castles.Wqd/=Castles.Wqd then Z_I := Z_I xor Z_Wqd; end if;
      if N_Castles.Wkd/=Castles.Wkd then Z_I := Z_I xor Z_Wkd; end if;
      if N_Castles.Bqd/=Castles.Bqd then Z_I := Z_I xor Z_Bqd; end if;
      if N_Castles.Bkd/=Castles.Bkd then Z_I := Z_I xor Z_Bkd; end if;
   end Undo_Move;
   pragma Inline(Undo_Move);


   procedure Gen_Moves_Local (I : Intboard; P : Piece; C : Color ;Ms : boolean) is
      From,To  : Integer;
      Tmp : Integer;
      Attacks,Moves,Crunches : Intboard;
      B : Intboard := I;

      P1,P2 : Piece;
      Promote : Boolean;
      N_En_Passant : Integer;
      Valid : Boolean;
      N_Castles: Castling;
      Res : Integer;
   begin
      while (B /= 0) loop
         Attacks := 0;
         From := First_One (B);
         B := B xor Set_Mask_I(From);
         if P = Pawn then
            if C = White then
               Attacks := White_Pawn_Attacks_I(From);
               if En_Passant =-1 then
                  Crunches := Attacks and All_Color_Boards(-C).all;
               else
                  Crunches := Attacks and (All_Color_Boards(-C).all or Set_Mask_I(En_Passant));
               end if;
               Moves := White_Pawn_Moves1_I(From) and (not All_Pieces_I);
               if Moves /= 0 then
                  Moves := Moves or (White_Pawn_Moves2_I(From) and (not All_Pieces_I));
               end if;
            else
               Attacks := Black_Pawn_Attacks_I(From);
               if En_Passant =-1 then
                  Crunches := Attacks and All_Color_Boards(-C).all;
               else
                  Crunches := Attacks and (All_Color_Boards(-C).all or (Set_Mask_I(En_Passant)));
               end if;
               Moves := Black_Pawn_Moves1_I(From) and (not All_Pieces_I);
               if Moves /= 0 then
                  Moves := Moves or (Black_Pawn_Moves2_I(From) and (not All_Pieces_I));
               end if;
            end if;
         else
            if Sliding_Mover(P) then
               if Rook_Mover(P) then
                  tmp := Find_Rank(From);
                  Attacks := Attacks or Rank_Attacks_I(from,tmp);
                  tmp := Find_Rank_90(From);
                  Attacks := Attacks or Rank_Attacks_90_I(from,tmp);
               end if;
               if Bishop_Mover(P) then
                  tmp := Find_Rank_45r(From);
                  Attacks := Attacks or Rank_Attacks_45r_I(from,tmp);
                  tmp := Find_Rank_45l(From);
                  Attacks := Attacks or Rank_Attacks_45l_I(from,tmp);
               end if;
            elsif P = Knight then Attacks := Knight_Attacks_I(From);
            elsif P = King then
               Attacks := King_Attacks_I(From);
            end if;
            Moves := Attacks and (not All_Pieces_I);
            Crunches := Attacks and All_Color_Boards(-C).all;
         end if;

         while Crunches /= 0 loop
            To := First_One (Crunches);
            Do_Move(From,To,C,P2,Promote,N_En_Passant,Valid,N_Castles,Ms);
            if Valid then
               if Ms then
                  Undo_Move(From,To,C,P2,Promote,N_Castles);
                  raise Not_Checkmated;
               else
                  Res := Piece_Value(P2)+Piece_Value(Chess_Board(To)); -- "To" because the move is done
                  if C=White then
                     if Res >=0 then Res := Piece_Value(P2)+See(-C); end if;
                     Res := -Res;
                  else
                     if Res <=0 then Res := Piece_Value(P2)+See(-C); end if;
                  end if;
                  if Res>=0 then
                     All_Moves(Nb_moves) := (From=>From,To=>To,Valeur=>Res);
                     Nb_moves:= nb_moves+1;
                  else
                     Bad_Moves(Nb_Bad) := (From=>From,To=>To,Valeur=>Res);
                     Nb_Bad := Nb_Bad+1;
                  end if;
                  Undo_Move(From,To,C,P2,Promote,N_castles);
               end if;
            else
               Undo_Move(From,To,C,P2,Promote,N_castles);
            end if;
            Crunches := Crunches xor Set_Mask_I(To);
         end loop;

         while Moves /= 0 loop
            To := First_One (Moves);
            if Ms then
               Do_Move(From,To,C,P2,Promote,N_En_Passant,Valid,N_Castles,Ms);
               Undo_Move(From,To,C,P2,Promote,N_castles);
               if Valid then
                  raise Not_Checkmated;
               end if;
            else
--	       Do_Move(From,To,C,P2,Promote,N_En_Passant,Valid,N_Castles,Ms);
--               Undo_Move(From,To,C,P2,Promote,N_castles);
	       if true then
		  P1 := Chess_Board(From);
		  if (P1=White_Pawn and then (To/8)=7)  or else (P1=Black_Pawn and then (To/8)=0) then
		     All_Moves(Nb_moves) := (From=>From,To=>To,Valeur=>800);
		     Nb_moves:= nb_moves+1;
		  else
		     simple_Moves(Nb_simple) := (From=>From,To=>To,Valeur=>0);
		     Nb_simple := Nb_simple+1;
		  end if;
	       end if;
	    end if;
            Moves := Moves xor Set_Mask_I(To);
         end loop;

         if not Ms then
            if P = King and not Qs then -- Castling ?
               if C=White then
                  if Castles.Wqc and then Chess_Board(1)=Empty and then Chess_Board(2)=Empty
                    and then Chess_Board(3)=Empty and then Black_Attacks_To(4) =0
                    and then Black_Attacks_To(3)=0 and then Black_Attacks_To(2) =0 then
                     All_Moves(Nb_Moves) := (From=>4,To=>2,Valeur=>0);
                     Nb_Moves := Nb_Moves+1;
                  end if;
                  if Castles.Wkc and then Chess_Board(5)=Empty and then Chess_Board(6)=Empty
                    and then Black_Attacks_To(4) =0 and then Black_Attacks_To(5)=0
                    and then Black_Attacks_To(6) =0 then
                     All_Moves(Nb_Moves) := (From=>4,To=>6,Valeur=>0);
                     Nb_Moves := Nb_Moves+1;
                  end if;
               elsif C = Black then --Black Castling ?
                  if Castles.Bqc and then Chess_Board(57)=Empty and then Chess_Board(58)=Empty
                    and then Chess_Board(59)=Empty and then White_Attacks_To(60) =0
                    and then White_Attacks_To(59)=0 and then White_Attacks_To(58) =0 then
                     All_Moves(Nb_Moves) := (From=>60,To=>58,Valeur=>0);
                     Nb_Moves := Nb_Moves+1;
                  end if;
                  if Castles.Bkc and then Chess_Board(61)=Empty and then Chess_Board(62)=Empty
                    and then White_Attacks_To(60) =0 and then White_Attacks_To(61)=0
                    and then White_Attacks_To(62) =0 then
                     All_Moves(Nb_Moves) := (From=>60,To=>62,Valeur=>0);
                     Nb_Moves := Nb_Moves+1;
                  end if;
               end if;
            end if;
         end if;
      end loop;
   end Gen_Moves_Local;
   pragma Inline(Gen_Moves_Local);

   function Checkmated(C:Color) return Integer is
      Pos : Integer;
      T  : Intboard;
   begin
      if Is_In_Ptable then return Get_From_Ptable; end if;

      if C=White then
         Pos := First_One(White_Kings_I);
         T := Black_Attacks_To(Pos);
         if T /=0 then
            begin
               Gen_Moves_Local(White_Pawns_I,Pawn,White,True);
               Gen_Moves_Local(White_Knights_I,Knight,White,True);
               Gen_Moves_Local(White_Bishops_I,Bishop,White,True);
               Gen_Moves_Local(White_Rooks_I,Rook,White,True);
               Gen_Moves_Local(White_Queens_I,Queen,White,True);
               Gen_Moves_Local(White_Kings_I,King,White,True);
               Store_To_Ptable(-32766);
               return -32766;
            exception
               when Not_Checkmated =>
                  Store_To_Ptable(32765);
                  return 0;
            end;
         else
            Store_To_Ptable(32765);
            return 0;
         end if;
      else
         Pos := Last_One(Black_Kings_I);
         T := White_Attacks_To(Pos);
         if T /=0 then
            begin
               Gen_Moves_Local(Black_Pawns_I,Pawn,Black,True);
               Gen_Moves_Local(Black_Knights_I,Knight,Black,True);
               Gen_Moves_Local(Black_Bishops_I,Bishop,Black,True);
               Gen_Moves_Local(Black_Rooks_I,Rook,Black,True);
               Gen_Moves_Local(Black_Queens_I,Queen,Black,True);
               Gen_Moves_Local(Black_Kings_I,King,Black,True);
               Store_To_Ptable(32766);
               return 32766;
            exception
               when Not_Checkmated =>
                  Store_To_Ptable(32765);
                  return 0;
            end;
         else
            Store_To_Ptable(32765);
            return 0;
         end if;
      end if;
   end Checkmated;
   pragma Inline(Checkmated);

   P2 : Piece;
   Promote : Boolean;
   N_En_Passant : Integer;
   Valid : Boolean;
   N_Castles: Castling;
   Res : Integer:=0;
   Pvw,Pvb : Integer;
   Alp :Integer := Alpha;
   Bet :Integer := Beta;
   Alpo : Integer := Alpha;
   Beto : Integer := Beta;
   G : Integer;
   Best_To : Integer := -1;
   Best_From : Integer := -1;
   Low : Integer := -32767;
   High : Integer := 32767;
   Vhash : Hash_Type;
   Start :Integer := 1;
   Tmp : Integer :=0;
   Tmps : Move_Struct;
   Decp : Integer;
   At_Least_Once : Boolean := False;
   Str_From,Str_To:Integer;
   begin
      
      Nb_Pos := Nb_Pos+1;
      Plies(Movenum+Int_Prof) := Z_I;
      if Int_Prof > Max_Prof then Max_Prof := Int_Prof; end if;
      if (Int_Prof>0) and then not Qs Then
         -- Test for move repetition. Check only between current move
         -- and last piece taken or last pawn moved...
         for I in First_Ply..Movenum+Int_Prof-1 loop
            if Z_I = Plies(I) then return 0; end if;
         end loop;
      end if;
      -- Test for checkmate
      Tmp := Checkmated(C);
      if (Tmp=32766) or (Tmp=-32766) then
         G:=Tmp;
	 goto Fin;
      end if;
      if Is_In_Table then
         Hash_Hits := Hash_Hits+1;
         Vhash := Get_From_Table;

         if Integer(Vhash.Prof)>= Prof then
            if Vhash.Low = Vhash.High then
               if Int_Prof=0 and then not Qs then
                  Move_To := Integer(Vhash.To);Move_From:=Integer(Vhash.From);
               end if;
               return Integer(Vhash.Low);
            end if;

            if Integer(Vhash.Low) >=Bet then
               return Integer(Vhash.Low);
            end if;
            if Integer(Vhash.High) <= Alp then
               return Integer(Vhash.High);
            end if;
            Alp := Integer'Max (Alp,Integer(Vhash.Low));
            Bet := Integer'Min (Bet,Integer(Vhash.High));
            Alpo := Alp;
            Beto := Bet;
         end if;

         if Vhash.Valid then
            Best_To := Integer(Vhash.To);
            Best_From := Integer(Vhash.From);
            Start := 0;
            All_Moves(0).From := Best_from;
            All_Moves(0).To := Best_To;
         end if;
      end if;
      
      if C=White then G := -32767; else G:= 32767; end if;
      if Qs 
	--and then not Is_In_Check(C) 
      then
	 G := Valeur(C,Matw,Matb,Castles,Int_Prof-1);
	 if C=White then
	    Alp := Integer'Max(Alp,G);
	    if Alp >= Bet then goto fin; end if;
	 else
	    Bet := Integer'Min(Bet,G);
	    if Bet <= Alp then goto fin; end if;
	 end if;
      end if;
      
      At_Least_Once := False;
      if Start=0 then
         Do_Move(Best_From,Best_To,C,P2,Promote,N_En_Passant,Valid,N_Castles,False);
	 if not Valid then 
	    -- Le coup stocke dans la table l'a ete a une profondeur
	    -- adaptee a une recherche standard alors que l'on est en Qs
	    -- Il n'est pas valide ici
	    if Qs then
	       Undo_Move(Best_From,Best_To,C,P2,Promote,N_castles);
	       Best_From := -1;Best_To:=-1;Start:=1;
	    else
	       -- Ca ne devrait jamais arriver
	       -- Ceci est du debug
	       Put_Line(File_Log,"hash error Qs="&Boolean'Image(Qs));
	       Print_Chessboard(File_Log);
	       Undo_Move(Best_From,Best_To,C,P2,Promote,N_castles);
	       Best_From := -1;Best_To:=-1;Start:=1;
	    end if;
	 else
	    At_Least_Once := True;
	    if Int_Prof=0 then Move_From := Best_From;Move_To := Best_To; end if;
	    Str_From := -Best_From;Str_To:= -Best_To;
	    Store_Stack(Int_Prof,Best_From,Best_To);
	    
	    if Promote then
	       if C=White then
		  Pvw := -Piece_Value(White_Pawn)+Piece_Value(White_Queen);
		  Pvb := -Piece_Value(P2);
	       else
		  Pvb := -Piece_Value(Black_Pawn)+Piece_Value(Black_Queen);
		  Pvw := -Piece_Value(P2);
	       end if;
	    else
	       if C=White then
		  Pvb := -Piece_Value(P2);Pvw := 0;
	       else
		  Pvw := -Piece_Value(P2);Pvb := 0;
	       end if;
	    end if;
	    -- What do we remove when a piece is taken? 10 is the usual. Might try something like 5
	    if Pvw /=0 or Pvb /= 0 then Decp:=5; else Decp:=10; end if;
	    -- When in check only decrease prof by 1
	    if Is_In_Check(-C) then Decp :=1; end if;
	    -- if prof-decp>=0 we stay in normal search if <0 then Quiescent search
	    if Prof-decp>=0 then
	       Res := Gen_Moves(Alp,Bet,-C,N_En_Passant,N_Castles,Int_Prof+1,False,Prof-decp,Matw+Pvw,Matb+Pvb,false);
	    else
	       Res := Gen_Moves(Alp,Bet,-C,N_En_Passant,N_Castles,Int_Prof+1,True,Prof-decp,Matw+Pvw,Matb+Pvb,false);
	    end if;
	    if Res>32700 and Res/=32767 then Res:=Res-1; end if;
	    if Res<-32700 and Res/=-32767 then Res:=Res+1; end if;
	    
	    if Int_Prof=0 then
	       Put_Line(File_Log,"First From:"&Integer'Image(Best_From)&" To:"&Integer'Image(Best_To)&
			  " Alpha:"&Integer'Image(Alp)&" Beta:"&Integer'Image(Bet)&
			  " Res:"&Integer'Image(Res)&" Nb_pos:"&Integer'Image(Nb_Pos));
	       Flush(File_Log);
	    end if;

	    Undo_Move(Best_From,Best_To,C,P2,Promote,N_castles);
	    if C=White then
	       G := Integer'Max(G,Res);
	       if G > Alp then
		  Alp := G;
		  if G >= Bet then goto fin; end if;
	       end if;
	    else
	       G := Integer'Min(G,Res);
	       if G < Bet then
		  Bet := G;
		  if G <= Alp then goto fin; end if;
	       end if;
	    end if;
	 end if;
      end if;
-- Now we generate all possible moves from this position if we were not lucky enough to return above      
      if C = White then
         Gen_Moves_Local(White_Pawns_I,Pawn,White,False);
         Gen_Moves_Local(White_Knights_I,Knight,White,False);
         Gen_Moves_Local(White_Bishops_I,Bishop,White,False);
         Gen_Moves_Local(White_Rooks_I,Rook,White,False);
         Gen_Moves_Local(White_Queens_I,Queen,White,False);
         Gen_Moves_Local(White_Kings_I,King,White,False);
      else
         Gen_Moves_Local(Black_Pawns_I,Pawn,Black,False);
         Gen_Moves_Local(Black_Knights_I,Knight,Black,False);
         Gen_Moves_Local(Black_Bishops_I,Bishop,Black,False);
         Gen_Moves_Local(Black_Rooks_I,Rook,Black,False);
         Gen_Moves_Local(Black_Queens_I,Queen,Black,False);
         Gen_Moves_Local(Black_Kings_I,King,Black,False);
      end if;
      if Int_Prof=0 then
         Put_Line(File_Log,"Int_prof=0 and nb_moves:"&Integer'Image(Nb_Moves)&
                   "  nb_simple:"&Integer'Image(Nb_Simple)&" nb_bad:"&Integer'Image(Nb_Bad));
      end if;
      
      --Horrible bubble sort on the all_moves board However, it contains only good moves (takes 
      --with good SEE value) and there are not many of them
      for J in 1..Nb_Moves-2 loop
         for I in 1..Nb_Moves-1-J loop
            if All_Moves(I).Valeur < All_Moves(I+1).valeur then
               Tmps := All_Moves(I+1);
               All_Moves(I+1):=All_Moves(I);
               All_Moves(I):=Tmps;
            end if;
         end loop;
      end loop;
      --We only search normal moves (no takes) and bad moves (bad takes from SEE value)
      --when not in Quiescent search or when in check
      if (not Qs) or (Is_In_Check(C)) then
         for I in 0..Nb_simple-1 loop
            All_Moves(Nb_Moves) := simple_Moves(I);
            Nb_Moves := Nb_Moves+1;
         end loop;
         for I in 0..Nb_bad-1 loop
            All_Moves(Nb_Moves) := Bad_Moves(I);
            Nb_Moves := Nb_Moves+1;
         end loop;
      end if;
      --If start=0 we remove the move (hash table best) already searched from the list
      if Start =0 then
         for I in 1..Nb_Moves-1 loop
            if All_Moves(I).From=Best_From and All_Moves(I).To =Best_To then
               All_Moves(I).From := -1;
               Start:=1;
               exit;
            end if;
         end loop;
      end if;
      --Now loop over all moves
      for I in Start .. Nb_Moves-1 loop
         if All_Moves(I).From=-1 then goto Suite; end if; --It's the first best move
	 Str_From := All_Moves(I).From;Str_To:= All_Moves(I).To;
         Do_Move(All_Moves(I).From,All_Moves(I).To,C,P2,Promote,N_En_Passant,Valid,N_Castles,False);
	 --Put move in stack to know where we are in the tree for debuggin purposes
         Store_Stack(Int_Prof,All_Moves(I).From,All_Moves(I).To);

         if not Valid then
            Undo_Move(All_Moves(I).From,All_Moves(I).To,C,P2,Promote,N_castles);
            goto Suite;
         else
            At_Least_Once := True;
         end if;
	 -- If promotion static value change
         if Promote then
            if C=White then
               Pvw := -Piece_Value(White_Pawn)+Piece_Value(White_Queen);
               Pvb := -Piece_Value(P2);
            else
               Pvb := -Piece_Value(Black_Pawn)+Piece_Value(Black_Queen);
               Pvw := -Piece_Value(P2);
            end if;
         else
            if C=White then
               Pvb := -Piece_Value(P2);Pvw := 0;
            else
               Pvw := -Piece_Value(P2);Pvb := 0;
            end if;
         end if;
         --Same as above
         if Pvw /=0 or Pvb /= 0 then Decp:=5; else Decp:=10; end if;
         -- WARNING!!! Setting decp to 0 kills the hash tables. decp should be 1 at least!!!
         if Is_In_Check(-C) then Decp :=1; end if;
         if Prof-decp>=0 then
            Res := Gen_Moves(Alp,Bet,-C,N_En_Passant,N_Castles,Int_Prof+1,False,Prof-decp,Matw+Pvw,Matb+Pvb,false);
         else
            Res := Gen_Moves(Alp,Bet,-C,N_En_Passant,N_Castles,Int_Prof+1,True,Prof-decp,Matw+Pvw,Matb+Pvb,false);
         end if;
	 if Res>32700 and Res/=32767 then Res:=Res-1; end if;
	 if Res<-32700 and Res/=-32767 then Res:=Res+1; end if;

         if Int_Prof=0 then
            Put_Line(File_Log,"Prof:"&Integer'Image(Prof)&" From:"&Integer'Image(All_Moves(I).from)&" To:"&Integer'Image(All_Moves(I).To)&" Alpha:"&Integer'Image(Alp)&" Beta"&Integer'Image(Bet)&" Res:"&Integer'Image(Res));
            Flush(File_Log);
         end if;


         Undo_Move(All_Moves(I).From,All_Moves(I).To,C,P2,Promote,N_castles);
         if C=White then
            G := Integer'Max(G,Res);
            if G > Alp then
               Alp := G;
               Best_To := All_Moves(I).To; Best_From := All_Moves(I).From;
               if Int_Prof=0 then Move_From := Best_From;Move_To := Best_To; end if;
               if G >= Bet then exit; end if;
            end if;
         else
            G := Integer'Min(G,Res);
            if G < Bet then
               Bet := G;
               Best_To := All_Moves(I).To; Best_From := All_Moves(I).From;
               if Int_Prof=0 then Move_From := Best_From;Move_To := Best_To; end if;
               if G <= Alp then exit; end if;
            end if;
         end if;
         <<Suite>>
           null;
      end loop;
      <<Fin>>

        if Int_Prof=0 then
            Move_To := Best_To;Move_From:=Best_From;
            Put_Line(File_Log,"Int_prof=0 again and nb_pos:"&Integer'Image(Nb_Pos)&" nb_moves_made:"&Unsigned_64'Image(Nb_Moves_Made)&" Hash hits:"&Integer'Image(Hash_Hits)&" max prof:"&Integer'Image(Max_Prof));
            Flush(File_Log);
        end if;

        -- Cas rare de partie nulle par Pat (pas de coup valide)
        -- On force la valeur a 1 sachant que la fonction d'evaluation
        -- exclue 0 et 1. 0 est reserve a la nulle par repetition
        if (not At_Least_Once) and (not Qs) and (G/=32766) and (G/=-32766) then G:=1; end if;
	-- If G is outside the initial window search then we have either a faillow or a failhigh
        if G<= Alpo then
           High := G;
           Low := -32767;
        elsif G>=Beto then
           Low := G;
           High := 32767;
	   -- Inside the window search the position evaluation is good
        else
           Low := G;
           High := G;
        end if;
        -- We  do not store in Hash when it is a draw by repetition
        -- Because a draw by repetition doesn't depend only on the position
        -- But also on the sequence of moves
        -- This defeats the hash table purpose
        if G/=0 then
	   -- Si en_passant est non nul alors la position actuelle contient un "possible" coup en passant
	   -- Dans ces conditions, il faut la distinguer d'une position obtenue sans le possible en passant
	   -- Avant cette modif on avait des erreurs de hash
	   if En_Passant/=-1 then Z_I := Z_I xor Z_En_Passant(En_Passant); end if;
           if Best_From /= -1 then
              Store_To_Table(Low,High,Best_From,Best_To,True,Prof,Movenum,C);
           else
              Store_To_Table(Low,High,0,0,False,Prof,Movenum,C);
           end if;
	   if En_Passant/=-1 then Z_I := Z_I xor Z_En_Passant(En_Passant); end if;
        end if;
        return G;
   exception
      when Error : others =>
	 Put_Line(File_Log,"From:"&Integer'Image(Str_From)&" To:"&Integer'Image(Str_To));
         Put_Line(File_Log,"Prof:"&Integer'Image(Prof)&" Int_prof:"&Integer'Image(Int_Prof)&" Max prof:"&Integer'Image(Max_Prof)&" Qs:"&Boolean'Image(Qs));
	 Print_Chessboard(File_Log);
         Put_Line(File_Log,"Exception in move:"&Ada.Exceptions.Exception_Name(Error)&". Exiting.");
         Flush(File_Log);
         raise;
   end Gen_Moves;

   function To_San (Pos:Integer) return String is
      S:String(1..2);
   begin
      S(1):=Character'Val (Character'Pos('a') + pos mod 8);
      S(2):=Character'Val (Character'Pos('1') + pos/8);
      return S;
   end To_San;

   function San (From:Integer;
                 To:Integer;
                 En_Passant : Integer) return String is
        P1,P2 : Piece;
        PN : String(1..1):=" ";
   begin
      P1 := Chess_Board(From);
      Put_Line(File_Log,"P1= "&Piece'Image(P1)&Integer'Image(Piece_Value(P1)));
      P2 := Empty;
      if From=4 and then To = 2 and then P1 = White_King then --White Castling Queenside
         return "O-O-O";
      elsif From=4 and then To = 6 and then P1 = White_King then --White Castling Kingside
         return "O-O";
      elsif From=60 and then To = 58 and then P1 = Black_King then --Black Castling Queenside
         return "O-O-O";
      elsif From=60 and then To = 62 and then P1 = Black_King then --Black Castling Kingside
         return "O-O";
      elsif To=En_Passant and then ((P1 = White_Pawn) or (P1=Black_Pawn)) then
         -- En passant. A priori, on se contente de la case de depart et de la case d'arrivee.
         -- A verifier s'il faut mettre un - ou une x
         return (To_San(From)&"-"&To_San(To));
      else --Everything else (nocastling, no en passant)
         P2 := Chess_Board(To);
         if (P1=White_Pawn and then (To/8)=7) or (P1=Black_Pawn and then (To/8)=0) then
            -- Promotion en reine pour les blancs ou les noirs
            if P2=Empty then return (To_San(From)&"-"&To_San(To)&"=Q"); else return (To_San(From)&"x"&To_San(To)&"=Q"); end if;
         end if;
         case P1 is
            when White_Rook =>  PN := "R";
            when Black_Rook =>  PN := "R";
            when White_Knight => PN := "N";
            when Black_Knight => PN := "N";
            when White_Bishop => PN := "B";
            when Black_Bishop => PN := "B";
            when White_Queen => PN := "Q";
            when Black_Queen => PN := "Q";
            when White_King => PN :="K";
            when Black_King => PN :="K";
            when others => PN:=" ";
         end case;

         if P2 = Empty then
            if PN/= " " then return (PN&To_San(From)&"-"&To_San(To));
            else return (To_San(From)&"-"&To_San(To)); end if;
         else
            if PN/= " " then return (PN&To_San(From)&"x"&To_San(To));
            else return (To_San(From)&"x"&To_San(To)); end if;
         end if;
      end if;
   end San;


   function Is_In_Check(C:Color) return Boolean is
      Pos : Integer;
      T : Intboard;
   begin
      if C=White then
         Pos := First_One(White_Kings_I);
         T := Black_Attacks_To(Pos);
      else
         Pos := Last_One(Black_Kings_I);
         T := White_Attacks_To(Pos);
      end if;
      return (T/=0);
   end Is_In_Check;


   procedure Really_Do_Move (From:Integer;
                             To:Integer;
                             C:Color;
                             Castles : Castling;
                             En_Passant : Integer;
                             P2 : out Piece;
                             Promote: out Boolean;
                             N_En_Passant : out Integer;
                             Valid : out Boolean;
                             N_Castles : out Castling) is
      P1 : Piece;
      Pos : Integer;
   begin
      P1 := Chess_Board(From);
      P2 := Empty;
      Promote := False;
      N_En_Passant := -1;
      N_Castles := Castles;
      if From=4 and then To = 2 and then P1 = White_King then --White Castling Queenside
         Do_Castle(4,2,0,3,White_King,White_Rook,C);N_Castles.Wqd := True;
	 Z_I := Z_I xor Z_Wqd;
      elsif From=4 and then To = 6 and then P1 = White_King then --White Castling Kingside
         Do_Castle(4,6,7,5,White_King,White_Rook,C);N_Castles.Wkd := True;
	 Z_I := Z_I xor Z_Wkd;
      elsif From=60 and then To = 58 and then P1 = Black_King then --Black Castling Queenside
         Do_Castle(60,58,56,59,Black_King,Black_Rook,C);N_Castles.Bqd := True;
	 Z_I := Z_I xor Z_Bqd;
      elsif From=60 and then To = 62 and then P1 = Black_King then --Black Castling Kingside
         Do_Castle(60,62,63,61,Black_King,Black_Rook,C);N_Castles.Bkd := True;
	 Z_I := Z_I xor Z_Bkd;
      elsif To=En_Passant and then ((P1 = White_Pawn) or (P1=Black_Pawn)) then -- En passant
         if C=White then
            P2 := Black_Pawn;
            Chess_Board(From) := Empty;
            Chess_Board(To) := White_Pawn;
            Chess_Board(To-8) := Empty;
            White_Pawns_I := White_Pawns_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            Black_Pawns_I := Black_Pawns_I xor Set_Mask_I(To-8);
            White_Pieces_I  := White_Pieces_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            Black_Pieces_I := Black_Pieces_I xor Set_Mask_I(To-8);
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From) xor Set_Mask_I(To) xor Set_Mask_I(To-8);
            Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_P(To-8,P2) xor Z_C;
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From) xor Set_Mask_90_I(To) xor Set_Mask_90_I(To-8);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From) xor Set_Mask_45l_I(To) xor Set_Mask_45l_I(To-8);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From) xor Set_Mask_45r_I(To) xor Set_Mask_45r_I(To-8);
         else
            P2 := White_Pawn;
            Chess_Board(From) := Empty;
            Chess_Board(To) := Black_Pawn;
            Chess_Board(To+8) := Empty;
            Black_Pawns_I := Black_Pawns_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            White_Pawns_I := White_Pawns_I xor Set_Mask_I(To+8);
            Black_Pieces_I  := Black_Pieces_I xor Set_Mask_I(To) xor Set_Mask_I(From);
            White_Pieces_I := White_Pieces_I xor Set_Mask_I(To+8);
            Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_P(To+8,P2) xor Z_C;
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From) xor Set_Mask_I(To) xor Set_Mask_I(To+8);
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From) xor Set_Mask_90_I(To) xor Set_Mask_90_I(To+8);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From) xor Set_Mask_45l_I(To) xor Set_Mask_45l_I(To+8);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From) xor Set_Mask_45r_I(To) xor Set_Mask_45r_I(To+8);
         end if;
      else --Everything else (nocastling, no en passant)
         P2 := Chess_Board(To);
         Chess_Board(From) := Empty;
         if P1=White_Pawn and then (To/8)=7 then
            White_Pawns_I := White_Pawns_I xor Set_Mask_I(From);
            White_Queens_I := White_Queens_I xor Set_Mask_I(To);
            Chess_Board(To) := White_Queen;
            Z_I := Z_I xor Z_P(From,White_Pawn) xor Z_P(To,White_Queen) xor Z_C;
            if P2/=Empty then Z_I := Z_I xor Z_P(To,P2); end if;
            Promote := True;
         elsif P1=Black_Pawn and then (To/8)=0 then
            Black_Pawns_I := Black_Pawns_I xor Set_Mask_I(From);
            Black_Queens_I := Black_Queens_I xor Set_Mask_I(To);
            Chess_Board(To) := Black_Queen;
            Z_I := Z_I xor Z_P(From,Black_Pawn) xor Z_P(To,Black_Queen) xor Z_C;
            if P2/=Empty then Z_I := Z_I xor Z_P(To,P2); end if;
            Promote := True;
         else
            Chess_Board(To) := P1;
            All_Boards(P1).all := All_Boards(P1).all xor Set_Mask_I(From) xor Set_Mask_I(To);
            Promote := False;
         end if;
         All_Color_Boards(C).all := All_Color_Boards(C).all xor Set_Mask_I(From) xor Set_Mask_I(To);
         if P2 = Empty then
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From) xor Set_Mask_I(To);
            if not Promote then Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_C; end if; --promotion already done above
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From) xor Set_Mask_90_I(To);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From) xor Set_Mask_45l_I(To);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From) xor Set_Mask_45r_I(To);
         else
            All_Pieces_I := All_Pieces_I xor Set_Mask_I(From);
            if not Promote then Z_I := Z_I xor Z_P(From,P1) xor Z_P(To,P1) xor Z_P(To,P2) xor Z_C; end if;
            All_Pieces_90_I := All_Pieces_90_I xor Set_Mask_90_I(From);
            All_Pieces_45l_I := All_Pieces_45l_I xor Set_Mask_45l_I(From);
            All_Pieces_45r_I := All_Pieces_45r_I xor Set_Mask_45r_I(From);
            All_Boards(P2).all := All_Boards(P2).all xor Set_Mask_I(To);
            All_Color_Boards(-C).all := All_Color_Boards(-C).all xor Set_Mask_I(To);
         end if;
         if P1 = White_Pawn and then (To-From)=16 then N_En_Passant := To-8;
         elsif P1 = Black_Pawn and then (From-To)=16 then N_En_Passant := To+8;
         else N_En_Passant := -1; end if;
      end if;
      if C=White then
         Pos := First_One(White_Kings_I);
         Valid := (Black_Attacks_To (Pos) = 0);
         -- Rook/King moved, no castling
         if N_Castles.Wqc and then (From=0 or else From=4) then
            Z_I := Z_I xor Z_Wqc;
            N_Castles.Wqc := False;
         end if;
         -- Rook/King moved, no castling
         if N_Castles.Wkc and then (From=7 or else From=4) then
            Z_I := Z_I xor Z_Wkc;
            N_Castles.Wkc := False;
         end if;
         if N_Castles.Bqc and then To=56 then
            Z_I := Z_I xor Z_Bqc;
            N_Castles.Bqc := False;
         end if;
         if N_Castles.Bkc and then To=63 then
            Z_I := Z_I xor Z_Bkc;
            N_Castles.Bkc := False;
         end if;
      else
         Pos := Last_One(Black_Kings_I);
         Valid := (White_Attacks_To (Pos) = 0);
         if N_Castles.Bqc and then (From=56 or else From=60) then
            Z_I := Z_I xor Z_Bqc;
            N_Castles.Bqc := False;
         end if;
         if N_Castles.Bkc and then (From=63 or else From=60) then
            Z_I := Z_I xor Z_Bkc;
            N_Castles.Bkc := False;
         end if;
         if N_Castles.Wqc and then To=0 then
            Z_I := Z_I xor Z_Wqc;
            N_Castles.Wqc := False;
         end if;
         if N_Castles.Wkc and then To=7 then
            Z_I := Z_I xor Z_Wkc;
            N_Castles.Wkc := False;
         end if;
      end if;
   end Really_Do_Move;


end Move;

