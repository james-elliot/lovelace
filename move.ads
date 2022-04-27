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
with Interfaces;
use Interfaces;

package Move is

   End_Thinking : exception;
   Move_Error : exception;
   Move_King_Error :exception; 
   
   type Move_Struct is
      record
         From : Integer ;
         To : Integer ;
         Valeur : Integer;
      end record;

   Nb_Moves_Made : Unsigned_64 := 0;
   Nb_Pos : Integer :=0;
   Hash_Hits : Integer  :=0;
   Max_Prof : Integer :=0;

   Move_From : Integer;
   Move_To : Integer;
   Best_Moves : array(0..127) of Move_Struct;

   procedure Dump_Stack(Curr_Prof:Integer);
   function Gen_Moves (Alpha : Integer;
                       Beta : Integer;
                       C:Color;
                       En_Passant : Integer;
                       Castles : Castling;
                       Int_Prof : Integer;
                       Qs : Boolean;
                       Prof:Integer;
                       MatW : Integer;
                       Matb : Integer;
                       Twice : Boolean) return Integer;

   function San (From:Integer;
                 To:Integer;
                 En_Passant : Integer) return String;

   function Is_In_Check(C:Color) return Boolean;

   procedure Really_Do_Move (From:Integer;
                             To:Integer;
                             C:Color;
                             Castles : Castling;
                             En_Passant : Integer;
                             P2 : out Piece;
                             Promote: out Boolean;
                             N_En_Passant : out Integer;
                             Valid : out Boolean;
                             N_Castles : out Castling);

end Move;
