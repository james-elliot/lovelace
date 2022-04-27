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

package Xboard is
   procedure Resign;
   procedure Read_Xboard(From:Integer;To:Integer);
   Xboard_From : Integer;
   Xboard_To : Integer;
   Xtime,Xotime,XT1,XT2,XT3 : Integer:=0;
   Xboard_Fen : String(1..256);
   Xname : String(1..256);
   Xboard_Fen_Last :Integer;
   Xname_Last : Integer;
   XratingC, XratingO : Integer;
   Xboard_Force :Boolean := False;
   Sortie : exception;

end Xboard;
