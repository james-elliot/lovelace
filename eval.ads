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


package Eval is
   Eval_Error : exception;
   function See(C:Color) return Integer;
   pragma Inline(See);
   function Valeur(C:Color;Matw:Integer;Matb:Integer;Cast:Castling;Int_Prof:Integer) return Integer;
   pragma Inline(Valeur);
   function Black_King_Safety(Coefw:Integer;Cast:Castling) return Integer;
   function White_King_Safety(Coefb:Integer;Cast:Castling) return Integer;
   function Black_Pawn_Struct(Coefw:Integer) return Integer;
   function White_Pawn_Struct(Coefb:Integer) return Integer;
   Black_King_Defenders_I,White_King_Defenders_I : array (0..63) of Intboard := (others=>0);
   King_Vicinity_I : array (0..63) of Intboard := (others=>0);
end Eval;
