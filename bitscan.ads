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

package Bitscan  is
   Bitscan_Error_First : exception;
   Bitscan_Error_Last : exception;
   -- Never call these functions with A=0 !!! Result is unspecified !
   function First_One (A:Intboard) return Natural;
   function Last_One (A:Intboard) return Natural;
   function Count_Bits(A:Intboard) return Natural;
   Pragma Inline(First_One);
   Pragma Inline(Last_One);
   Pragma Inline(Count_Bits);
end Bitscan ;
