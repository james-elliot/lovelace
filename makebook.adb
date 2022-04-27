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

with Hash;
use Hash;
with Hashbook;
use Hashbook;
with Text_Io;
with Bitboards;
use Bitboards;
with Move;
use Move;
with Sequential_Io;


procedure Makebook is
   function To_Move (X,Y:Character) return Integer is
   begin
      return (((Character'Pos(x)-Character'Pos('a'))+8*(Character'Pos(y)-Character'Pos('1'))));
   end To_Move;

   Fp : Text_IO.File_Type;
   S : String(1..4096);
   Last : Natural;
   From,To : Integer;
   Col : Color;
   Cast,N_castles : Castling;
   En_Passant,N_En_Passant : Integer;
   P2 : Piece;
   Promote : Boolean;
   Valid : Boolean;
   Nb_Moves : constant := 20;
   Matw,Matb:Integer;
begin
   Text_IO.Open(Fp,Text_IO.In_File,"opening.san");
   loop
      Init_Boards("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",Col,Cast,En_Passant,Matw,matb);
      Init_Masks;
      Init_Attacks;
      Make_Z_I(Col);
      First_Ply := 0;
      Text_IO.Get_Line(Fp,S,Last);
      if Last > Nb_Moves*5+2 then
         for I in 0..Nb_Moves loop
            From := To_Move(S(3+5*I),S(4+5*I));
            To := To_Move(S(5+5*I),S(6+5*I));
            Put_Book(Z_I,From,To,S(1));
            Really_Do_Move (From,To,Col,Cast,En_Passant,P2,Promote,N_En_Passant,Valid,N_Castles);
            if not Valid then raise Constraint_Error; end if;
            Cast := N_Castles;En_Passant := N_En_Passant;Col := -Col;
         end loop;
      end if;
   end loop;
exception
   when Text_IO.End_Error =>
      Text_IO.Put_Line(Integer'Image(Get_Last));
      Save_All;
end Makebook;
