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

with Interfaces;
use Interfaces;
with Bitboards;
use Bitboards;

package Hashbook is

   procedure Put_Book(Z_I : Unsigned_64;From,To : Integer;Res : Character);
   procedure Get_Book(From,To : out Integer;Col : Color; Cast : Castling ; En_Passant : Integer);
   function Get_Last return Integer;
   procedure Save_All;
   function Load_All return Boolean;
end Hashbook;
