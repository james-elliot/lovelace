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
with Text_Io;
use Text_Io;

package Bitboards is

   Bitboards_Error : exception;
   type Intboard is new Unsigned_64;
   for Intboard'Size use 64;
   type Bit is new Integer range 0..1;
   for Bit'Size use 1;
   type Bitboard is array (0..63) of Bit;
   pragma Pack(Bitboard);
   for Bitboard'Size use 64;
   type Halfboard is
      record
         L : Unsigned_32;
         H : Unsigned_32;
      end record;
   for Halfboard use
      record
         L at 0 range 0..31;
         H at 0 range 32..63;
      end record;
   for Halfboard'Size use 64;

   type Castling is
      record
         Wkc : Boolean;
         Wqc : Boolean;
         Bkc : Boolean;
         Bqc : Boolean;
         Wkd : Boolean;
         Wqd : Boolean;
         Bkd : Boolean;
         Bqd : Boolean;
      end record;

   for Castling use
      record
         Wkc at 0 range 0..0;
         Wqc at 0 range 1..1;
         Bkc at 0 range 2..2;
         Bqc at 0 range 3..3;
         Wkd at 0 range 4..4;
         Wqd at 0 range 5..5;
         Bkd at 0 range 6..6;
         Bqd at 0 range 7..7;
      end record;

   for Castling'Size use 8;

   type State is
      record
         White_Rooks_I : Intboard;
         Black_Rooks_I : Intboard;
         White_Knights_I : Intboard;
         Black_Knights_I : Intboard;
         White_Bishops_I : Intboard;
         Black_Bishops_I : Intboard;
         White_Queens_I : Intboard;
         Black_Queens_I : Intboard;
         White_Kings_I : Intboard;
         Black_Kings_I : Intboard;
         White_Pawns_I : Intboard;
         Black_Pawns_I : Intboard;
         White_Pieces_I : Intboard;
         Black_Pieces_I : Intboard;
         Z_I : Unsigned_64;
         All_Pieces_45r_I  : Intboard;
         All_Pieces_45l_I  : Intboard;
         All_Pieces_90_I  : Intboard;
         All_Pieces_I  : Intboard;
      end record;

   procedure Save_State(S: out State);
   procedure Restore_State(S: State);

   type Color is new Integer;

   Plies : array (0..500) of Unsigned_64 := (others =>0);
   First_Ply : Integer := 0;
   Movenum : Integer:=0;

   White_Rooks_B, Black_Rooks_B,
     White_Knights_B, Black_Knights_B,
     White_Bishops_B, Black_Bishops_B,
     White_Queens_B, Black_Queens_B,
     White_Kings_B, Black_Kings_B,
     White_Pawns_B, Black_Pawns_B,
     White_Pieces_B, Black_Pieces_B,
     All_Pieces_B, All_Pieces_90_B,
     All_Pieces_45r_B, All_Pieces_45l_B : Bitboard;

   White_Rooks_I, Black_Rooks_I,
     White_Knights_I, Black_Knights_I,
     White_Bishops_I, Black_Bishops_I,
     White_Queens_I, Black_Queens_I,
     White_Kings_I, Black_Kings_I,
     White_Pawns_I, Black_Pawns_I,
     White_Pieces_I, Black_Pieces_I,
     All_Pieces_I, All_Pieces_90_I,
     All_Pieces_45r_I, All_Pieces_45l_I : aliased Intboard;

   for White_Rooks_B'Address use White_Rooks_I'Address;
   for Black_Rooks_B'Address use Black_Rooks_I'Address;
   for White_Knights_B'Address use White_Knights_I'Address;
   for Black_Knights_B'Address use Black_Knights_I'Address;
   for White_Bishops_B'Address use White_Bishops_I'Address;
   for Black_Bishops_B'Address use Black_Bishops_I'Address;
   for White_Queens_B'Address use White_Queens_I'Address;
   for Black_Queens_B'Address use Black_Queens_I'Address;
   for White_Kings_B'Address use White_Kings_I'Address;
   for Black_Kings_B'Address use Black_Kings_I'Address;
   for White_Pawns_B'Address use White_Pawns_I'Address;
   for Black_Pawns_B'Address use Black_Pawns_I'Address;
   for White_Pieces_B'address use White_Pieces_I'Address;
   for Black_Pieces_B'Address use Black_Pieces_I'Address;
   for All_Pieces_B'Address use All_Pieces_I'Address;
   for All_Pieces_90_B'Address use All_Pieces_90_I'Address;
   for All_Pieces_45r_B'Address use All_Pieces_45r_I'Address;
   for All_Pieces_45l_B'Address use All_Pieces_45l_I'Address;


   Knight_Attacks_I : array (0..63) of Intboard;
   Knight_Attacks_B : array (0..63) of Bitboard;
   pragma Import(Ada,Knight_Attacks_B);
   King_Attacks_I : array (0..63) of Intboard;
   King_Attacks_B : array (0..63) of Bitboard;
   pragma Import(Ada,King_Attacks_B);
   White_Pawn_Attacks_I : array (0..63) of Intboard;
   White_Pawn_Attacks_B : array (0..63) of Bitboard;
   pragma Import(Ada,White_Pawn_Attacks_B);
   Black_Pawn_Attacks_I : array (0..63) of Intboard;
   Black_Pawn_Attacks_B : array (0..63) of Bitboard;
   pragma Import(Ada,Black_Pawn_Attacks_B);
   White_Pawn_Moves1_I : array (0..63) of Intboard;
   White_Pawn_Moves1_B : array (0..63) of Bitboard;
   pragma Import(Ada,White_Pawn_Moves1_B);
   Black_Pawn_Moves1_I : array (0..63) of Intboard;
   Black_Pawn_Moves1_B : array (0..63) of Bitboard;
   pragma Import(Ada,Black_Pawn_Moves1_B);
   White_Pawn_Moves2_I : array (0..63) of Intboard;
   White_Pawn_Moves2_B : array (0..63) of Bitboard;
   pragma Import(Ada,White_Pawn_Moves2_B);
   Black_Pawn_Moves2_I : array (0..63) of Intboard;
   Black_Pawn_Moves2_B : array (0..63) of Bitboard;
   pragma Import(Ada,Black_Pawn_Moves2_B);

   for Knight_Attacks_B'Address use Knight_attacks_I'Address;
   for King_Attacks_B'Address use King_attacks_I'Address;
   for White_Pawn_Attacks_B'Address use White_Pawn_attacks_I'Address;
   for Black_Pawn_Attacks_B'Address use Black_Pawn_attacks_I'Address;
   for White_Pawn_Moves1_B'Address use White_Pawn_moves1_I'Address;
   for Black_Pawn_Moves1_B'Address use Black_Pawn_moves1_I'Address;
   for White_Pawn_Moves2_B'Address use White_Pawn_moves2_I'Address;
   for Black_Pawn_Moves2_B'Address use Black_Pawn_moves2_I'Address;


   Set_Mask_I : array (0..63) of Intboard;
   Set_Mask_B : array (0..63) of Bitboard;
   pragma Import(Ada,Set_Mask_B);
   for Set_Mask_B'Address use Set_Mask_I'Address;
   Set_Mask_90_I : array (0..63) of Intboard;
   Set_Mask_90_B : array (0..63) of Bitboard;
   pragma Import(Ada,Set_Mask_90_B);
   for Set_Mask_90_B'Address use Set_Mask_90_I'Address;
   Set_Mask_45r_I : array (0..63) of Intboard;
   Set_Mask_45r_B : array (0..63) of Bitboard;
   pragma Import(Ada,Set_Mask_45r_B);
   for Set_Mask_45r_B'Address use Set_Mask_45r_I'Address;
   Set_Mask_45l_I : array (0..63) of Intboard;
   Set_Mask_45l_B : array (0..63) of Bitboard;
   pragma Import(Ada,Set_Mask_45l_B);
   for Set_Mask_45l_B'Address use Set_Mask_45l_I'Address;

   Knight_Mobility : array(0..63) of Integer;
   Rank_Mobility : array(0..63,0..255) of Integer;
   Rank_Mobility_90 : array(0..63,0..255) of Integer;
   Rank_Mobility_45r : array(0..63,0..255) of Integer;
   Rank_Mobility_45l : array(0..63,0..255) of Integer;
   Rank_Attacks_I : array(0..63,0..255) of Intboard;
   Rank_Attacks_B : array(0..63,0..255) of Bitboard;
   pragma Import(Ada,Rank_Attacks_B);
   for Rank_Attacks_B'Address use Rank_Attacks_I'Address;
   Rank_Attacks_90_I : array(0..63,0..255) of Intboard;
   Rank_Attacks_90_B : array(0..63,0..255) of Bitboard;
   pragma Import(Ada,Rank_Attacks_90_B);
   for Rank_Attacks_90_B'Address use Rank_Attacks_90_I'Address;
   Rank_Attacks_45l_I : array(0..63,0..255) of Intboard;
   Rank_Attacks_45l_B : array(0..63,0..255) of Bitboard;
   pragma Import(Ada,Rank_Attacks_45l_B);
   for Rank_Attacks_45l_B'Address use Rank_Attacks_45l_I'Address;
   Rank_Attacks_45r_I : array(0..63,0..255) of Intboard;
   Rank_Attacks_45r_B : array(0..63,0..255) of Bitboard;
   pragma Import(Ada,Rank_Attacks_45r_B);
   for Rank_Attacks_45r_B'Address use Rank_Attacks_45r_I'Address;

   procedure Init_Pieces;
   procedure Init_Boards(S:String;
                         To_Move: out Color;
                         Cast : out Castling;
                         En_Passant : out Integer;
                         Matw : out Integer;
                         Matb : out integer);
   procedure Init_Attacks;
   procedure Init_Masks;
   procedure Print_Bitboard (B : Bitboard);
   procedure Print_Bitboard_45 (B : Bitboard);
   procedure Print_Intboard (I : Intboard);
   procedure Print_Intboard_45 (I : Intboard);
   procedure Print_Chessboard;
   procedure Print_Chessboard(F:File_Type);
   procedure Print_Chessboard2;
   procedure Print_Fen(F : File_Type;Col:Color;Cast:Castling;En_Passant:Integer);
   procedure Check_All(S:String);
   function Find_Rank(B:Natural) return Natural;
   pragma Inline(Find_Rank);
   function Find_Rank_90(B:Natural) return Natural;
   pragma Inline(Find_Rank_90);
   function Find_Rank_45r(B:Natural) return Natural;
   pragma Inline(Find_Rank_45r);
   function Find_Rank_45l(B:Natural) return Natural;
   pragma Inline(Find_Rank_45l);

   Black : constant Color :=  -1;
   White : constant Color := 1;

   type Piece is new Unsigned_8;
   Empty : constant Piece := 0;
   Pawn : constant Piece := 1;
   Knight : constant Piece := 2;
   King : constant Piece := 3;
   Bishop : constant Piece := 5;
   Rook : constant Piece := 6;
   Queen : constant Piece := 7;
   Black_Pawn : constant Piece := 9;
   Black_Knight : constant Piece := 10;
   Black_King : constant Piece := 11;
   Black_Bishop : constant Piece := 13;
   Black_Rook : constant Piece := 14;
   Black_Queen : constant Piece := 15;
   White_Pawn : constant Piece := 1;
   White_Knight : constant Piece := 2;
   White_King : constant Piece := 3;
   White_Bishop : constant Piece := 5;
   White_Rook : constant Piece := 6;
   White_Queen : constant Piece := 7;

   Chess_Board : array (0..63) of Piece;

   Piece_Value : constant array(Piece(0)..Piece(15)) of Integer :=
     (Empty=>0,
      White_Pawn=>100,
      White_Knight=>300,
      White_King=>10000,
      White_Bishop=>300,
      White_Rook=>500,
      White_Queen=>900,
      Black_Pawn=>-100,
      Black_Knight=>-300,
      Black_King=>-10000,
      Black_Bishop=>-300,
      Black_Rook=>-500,
      Black_Queen=>-900,
      others=>0);


   function Piece_Color(P : Piece) return Color;
   pragma Inline(Piece_Color);
   function Sliding_Mover(P : Piece) return Boolean;
   pragma Inline(Sliding_Mover);
   function Bishop_Mover (P : Piece) return Boolean;
   pragma Inline(Bishop_Mover);
   function Rook_Mover (P : Piece) return Boolean;
   pragma Inline(Rook_Mover);
   function White_Attacks_To(P:Integer) return Intboard;
   pragma Inline(White_Attacks_To);
   function Black_Attacks_To(P:Integer) return Intboard;
   pragma Inline(Black_Attacks_To);
   procedure All_Attacks_To(P : Integer;Whites : out Intboard;Blacks : out Intboard);
   pragma Inline(All_Attacks_To);
   procedure All_Ortho_Attacks_To(P : Integer;Whites : out Intboard;Blacks : out Intboard);
   pragma Inline(All_Ortho_Attacks_To);
   procedure All_Diag_Attacks_To(P : Integer;Whites : out Intboard;Blacks : out Intboard);
   pragma Inline(All_Diag_Attacks_To);


   type Addr_Intboard is access all Intboard;

   All_Boards : array (White_Pawn..Black_Queen) of Addr_Intboard :=
     (White_Pawn=>White_Pawns_I'Access,
      White_Knight=>White_Knights_I'Access,
      White_King=>White_Kings_I'Access,
      White_Bishop=>White_Bishops_I'Access,
      White_Rook=>White_Rooks_I'Access,
      White_Queen=>White_Queens_I'Access,
      Black_Pawn=>Black_Pawns_I'Access,
      Black_Knight=>Black_Knights_I'Access,
      Black_King=>Black_Kings_I'Access,
      Black_Bishop=>Black_Bishops_I'Access,
      Black_Rook=>Black_Rooks_I'Access,
      Black_Queen=>Black_Queens_I'Access,
      others=>null);

   All_Color_Boards : array (Black..White) of Addr_Intboard :=
     (Black => Black_Pieces_I'Access,
      White => White_Pieces_I'Access,
      others => null);

   Z_P  : array(0..63,Piece(0)..Piece(15)) of Unsigned_64;
   Z_C : Unsigned_64;
   Z_En_Passant : array(0..63) of Unsigned_64;
   Z_Wqc,Z_Wkc,Z_Bqc,Z_Bkc : Unsigned_64;
   Z_Wqd,Z_Wkd,Z_Bqd,Z_Bkd : Unsigned_64;
   Z_I : Unsigned_64 := 0;


end Bitboards;
