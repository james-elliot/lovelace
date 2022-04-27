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
with Unchecked_Conversion;
with Bitscan;
use Bitscan;
with Xboard;
use Xboard;

package body Bitboards is

   To_45r,From_45r,Start_45r,Start_45r_Rev,
     Length_45r,Pos_45r : array(0..63) of Integer;
   Mask_45r_rev : array(0..63) of Intboard;

   To_45l,From_45l,Start_45l,Start_45l_Rev,
     Length_45l,Pos_45l : array(0..63) of Integer;
   Mask_45l_rev : array(0..63) of Intboard;

   function Intboard_To_Bitboard is new Unchecked_Conversion(Intboard,Bitboard);

   procedure Print_Intboard(I:Intboard) is
   begin
      Print_Bitboard(Intboard_To_Bitboard (I));
   end Print_Intboard;

   procedure Print_Intboard_45(I:Intboard) is
   begin
      Print_Bitboard_45(Intboard_To_Bitboard (I));
   end Print_Intboard_45;

   procedure Zero_Pieces is
   begin
      White_Rooks_I:=0;
      Black_Rooks_I:=0;
      White_Knights_I:=0;
      Black_Knights_I:=0;
      White_Bishops_I:=0;
      Black_Bishops_I:=0;
      White_Queens_I:=0;
      Black_Queens_I:=0;
      White_Kings_I:=0;
      Black_Kings_I:=0;
      White_Pawns_I:=0;
      Black_Pawns_I:=0;
      for I in 0..63 loop Chess_Board(I) := empty; end loop;
   end Zero_Pieces;

   procedure Init_Rotated_Boards is
      K,L:Integer;
   begin
      for I in 0..7 loop
         for J in 0..7 loop
            All_Pieces_90_B(8*I+J) := All_Pieces_B(8*(7-J)+I);
         end loop;
      end loop;

      L:=0;
      All_Pieces_45r_I := 0;
      All_Pieces_45l_I := 0;
      for I in 0 .. 14 loop
         for J in Integer'Max(0,I-7)..Integer'Min(7,I) loop
            K:=I-J;
            All_Pieces_45r_B(L) := All_Pieces_B(J*8+7-K);
            To_45r(J*8+7-K):=L;
            From_45r(L):=J*8+7-K;
            Length_45r(L) := 1+Integer'Min(7,I)-Integer'Max(0,I-7);
            Pos_45r(L) := J-Integer'Max(0,I-7);
            Start_45r(L) := L-Pos_45r(L);
            Start_45r_Rev(J*8+7-K) := Start_45r(L);
            Mask_45r_rev(J*8+7-K):=Shift_Left((2**Length_45r(L)-1),Start_45r(L));

            All_Pieces_45l_B(L) := All_Pieces_B(K*8+J);
--            if (K*8+J=36) then Put(Integer'Image(L));Put(Integer'Image(J));Put_line(Integer'Image(I)); end if;
            To_45l(K*8+J):=L;
            From_45l(L):=K*8+J;
            Length_45l(L) := 1+Integer'Min(7,I)-Integer'Max(0,I-7);
            Pos_45l(L) := J-Integer'Max(0,I-7);
            Start_45l(L) := L-Pos_45l(L);
            Start_45l_Rev(K*8+J) := Start_45l(L);
            Mask_45l_rev(K*8+J):=Shift_Left((2**Length_45l(L)-1),Start_45l(L));

            L:=L+1;
         end loop;
      end loop;
   end Init_Rotated_Boards;

   procedure Init_Pieces is
   begin
      White_Rooks_B(0) := 1;White_Rooks_B(7) := 1;
      Chess_Board(0) := White_Rook; Chess_Board(7) := White_Rook;
      Black_Rooks_B(56) := 1;Black_Rooks_B(63) := 1;
      Chess_Board(56) := Black_Rook; Chess_Board(63) := Black_Rook;
      White_Knights_B(1) := 1;White_Knights_B(6) := 1;
      Chess_Board(1) := White_Knight; Chess_Board(6) := White_Knight;
      Black_Knights_B(57) := 1;Black_Knights_B(62) := 1;
      Chess_Board(57) := Black_Knight; Chess_Board(62) := Black_Knight;
      White_Bishops_B(2) := 1;White_Bishops_B(5) := 1;
      Chess_Board(2) := White_Bishop; Chess_Board(5) := White_Bishop;
      Black_Bishops_B(58) := 1;Black_Bishops_B(61) := 1;
      Chess_Board(58) := Black_Bishop; Chess_Board(61) := Black_Bishop;
      White_Queens_B(3) := 1; Chess_Board(3) := White_Queen;
      Black_Queens_B(59) := 1; Chess_Board(59) := Black_Queen;
      White_Kings_B(4) := 1; Chess_Board(4) := White_King;
      Black_Kings_B(60) := 1; Chess_Board(60) := Black_King;
      for I in 0..7 loop
         White_Pawns_B(I+8):=1; Chess_Board(I+8):=White_Pawn;
         Black_Pawns_B(I+48):=1; Chess_Board(I+48):=Black_Pawn;
      end loop;
   end Init_Pieces;

      procedure Save_State(S: out State) is
      begin
         S.White_Rooks_I:= White_Rooks_I;
         S.Black_Rooks_I:= Black_Rooks_I;
         S.White_Knights_I:= White_Knights_I;
         S.Black_Knights_I:= Black_Knights_I;
         S.White_Bishops_I:= White_Bishops_I;
         S.Black_Bishops_I:= Black_Bishops_I;
         S.White_Queens_I:= White_Queens_I;
         S.Black_Queens_I:= Black_Queens_I;
         S.White_Kings_I:=White_Kings_I;
         S.Black_Kings_I:=Black_Kings_I;
         S.White_Pawns_I:=White_Pawns_I;
         S.Black_Pawns_I:=Black_Pawns_I;
         S.White_Pieces_I := White_Pieces_I;
         S.Black_Pieces_I := Black_Pieces_I;
         S.Z_I:=Z_I;
         S.All_Pieces_45r_I := All_Pieces_45r_I ;
         S.All_Pieces_45l_I := All_Pieces_45l_I ;
         S.All_Pieces_90_I := All_Pieces_90_I ;
         S.All_Pieces_I := All_Pieces_I ;
      end;

      procedure Restore_State(S: State) is
      begin
         White_Rooks_I:= S.White_Rooks_I;
         Black_Rooks_I:= S.Black_Rooks_I;
         White_Knights_I:= S.White_Knights_I;
         Black_Knights_I:= S.Black_Knights_I;
         White_Bishops_I:= S.White_Bishops_I;
         Black_Bishops_I:= S.Black_Bishops_I;
         White_Queens_I:= S.White_Queens_I;
         Black_Queens_I:= S.Black_Queens_I;
         White_Kings_I:= S.White_Kings_I;
         Black_Kings_I:= S.Black_Kings_I;
         White_Pawns_I:= S.White_Pawns_I;
         Black_Pawns_I:= S.Black_Pawns_I;
         White_Pieces_I := S.White_Pieces_I;
         Black_Pieces_I := S.Black_Pieces_I;
         Z_I:= S.Z_I;
         All_Pieces_45r_I := S.All_Pieces_45r_I ;
         All_Pieces_45l_I := S.All_Pieces_45l_I ;
         All_Pieces_90_I := S.All_Pieces_90_I ;
         All_Pieces_I := S.All_Pieces_I ;
      end;


   procedure Init_Pieces(S:String;To_Move: out Color;Cast : out Castling;En_Passant : out Integer) is
      procedure Inc_Ind(Ind: in out Integer;Inc: in Integer) is
         I,J:Integer;
      begin
         I := Ind/8;
         J := (Ind mod 8)+Inc;
         if J>=8 then J:=J-8;I:=I-1;end if;
         Ind := 8*I+J;
      end Inc_Ind;
      Ind : Integer :=56;
      Curr : Integer;
   begin
      Cast := (others=>False);
      for I in S'Range loop
         case S(I) is
            when '/' => null;
            when 'R' => White_Rooks_B(Ind):=1; Chess_Board(Ind):=White_Rook; Inc_Ind(Ind,1);
            when 'r' => Black_Rooks_B(Ind):=1; Chess_Board(Ind):=Black_Rook; Inc_Ind(Ind,1);
            when 'B' => White_Bishops_B(Ind):=1; Chess_Board(Ind):=White_Bishop; Inc_Ind(Ind,1);
            when 'b' => Black_Bishops_B(Ind):=1; Chess_Board(Ind):=Black_Bishop; Inc_Ind(Ind,1);
            when 'N' => White_Knights_B(Ind):=1; Chess_Board(Ind):=White_Knight; Inc_Ind(Ind,1);
            when 'n' => Black_Knights_B(Ind):=1; Chess_Board(Ind):=Black_Knight; Inc_Ind(Ind,1);
            when 'Q' => White_Queens_B(Ind):=1; Chess_Board(Ind):=White_Queen; Inc_Ind(Ind,1);
            when 'q' => Black_Queens_B(Ind):=1; Chess_Board(Ind):=Black_Queen; Inc_Ind(Ind,1);
            when 'K' => White_Kings_B(Ind):=1; Chess_Board(Ind):=White_King; Inc_Ind(Ind,1);
            when 'k' => Black_Kings_B(Ind):=1; Chess_Board(Ind):=Black_King; Inc_Ind(Ind,1);
            when 'P' => White_pawns_B(Ind):=1; Chess_Board(Ind):=White_Pawn; Inc_Ind(Ind,1);
            when 'p' => Black_Pawns_B(Ind):=1; Chess_Board(Ind):=Black_Pawn; Inc_Ind(Ind,1);
            when '1'..'8' => Inc_Ind(Ind,Character'Pos(S(I))-Character'Pos('0'));
            when ' ' => Curr := I+1; exit;
            when others => null;
         end case;
         -- Currently only positions are considered in the FEN. Should do the rest!
      end loop;
      if S(Curr)='w' then To_Move :=White;
      elsif S(Curr)='b' then To_Move := Black;
      else raise Bitboards_Error;
      end if;
      Curr := Curr+2;
      if S(Curr) /='-' then
         while S(Curr)/=' ' loop
            case S(Curr) is
               when 'K' => Cast.Wkc := True;
               when 'k' => Cast.Bkc := True;
               when 'Q' => Cast.Wqc := True;
               when 'q' => Cast.Bqc := True;
               when others => null;
            end case;
            Curr := Curr+1;
         end loop;
         Curr := Curr+1;
      else
         Curr := Curr+2;
      end if;
      if S(Curr)= '-' then En_Passant:=-1;
      else
        En_Passant := -1;
          --(Character'Value(S(Curr))-Character'Value('a'))*8+
           --Character'Value(S(Curr+1))-Character'Value('0');
      end if;

   end Init_Pieces;

   procedure Init_Boards(S:String;To_Move: out Color;Cast : out Castling;En_Passant : out Integer;
                        Matw : out Integer;Matb : out integer) is
      begin
         Zero_Pieces;
         Init_Pieces(S,To_Move,Cast,En_Passant);
         White_Pieces_I := White_Rooks_I or White_Knights_I or White_Bishops_I or
           White_Queens_I or White_Kings_I or White_Pawns_I;
         Black_Pieces_I := Black_Rooks_I or Black_Knights_I or Black_Bishops_I or
           Black_Queens_I or Black_Kings_I or Black_Pawns_I;
         All_Pieces_I := White_Pieces_I or Black_Pieces_I;
         Init_Rotated_Boards;
         Matw:=0;Matb:=0;
         for I in 0..63 loop
            if Chess_Board(I)/=0 then
               if Piece_Color(Chess_Board(I))=Black then
                  Matb := Matb+Piece_Value(Chess_Board(I));
               else
                  Matw := Matw+Piece_Value(Chess_Board(I));
               end if;
            end if;
         end loop;
         Matw := Matw-Piece_Value(White_King);
         Matb := Matb-Piece_Value(Black_King);
      end Init_Boards;

   procedure Init_Masks is
      K,L:Integer;
   begin
      for I in 0..63 loop
        Set_Mask_I(I):=Shift_Left (1,I);
      end loop;

      for I in 0..7 loop
         for J in 0..7 loop
            for N in 0..63 loop
               Set_Mask_90_B(N)(8*I+J) := Set_Mask_B(N)(8*(7-J)+I);
            end loop;
         end loop;
      end loop;

      L:=0;
      for I in 0 .. 14 loop
         for J in 0..Integer'Min(7,I) loop
            K:=I-J;
            if K<=7 then
               for N in 0..63 loop
                  Set_Mask_45l_B(N)(L) := Set_Mask_B(N)(J+K*8);
                  Set_Mask_45r_B(N)(L) := Set_Mask_B(N)(J*8+7-K);
               end loop;
               L:=L+1;
            end if;
         end loop;
      end loop;
   end Init_Masks;

   function Is_In_Bounds(I,J:Integer) return Boolean is
   begin
      return((I<=7) and (I>=0) and (J<=7) and (J>=0));
   end Is_In_Bounds;

   procedure Init_Knight_Attacks is
      Knight_Moves : array (0..7,0..1) of Integer := ((1,2),(2,1),(-1,2),(-2,1),(1,-2),(2,-1),(-1,-2),(-2,-1));
   begin
      for I in 0..63 loop
         Knight_Attacks_I(I) := 0;
         Knight_Mobility(I) := 0;
      end loop;
      for I in 0..7 loop
         for J in 0..7 loop
            for K in 0..7 loop
               if Is_In_Bounds(I+Knight_Moves(K,0),J+Knight_Moves(K,1)) then
                  Knight_Attacks_B(I*8+J)((I+Knight_Moves(K,0))*8+J+Knight_Moves(K,1)) := 1;
                  Knight_Mobility(I*8+J):=Knight_Mobility(I*8+J)+1;
               end if;
            end loop;
         end loop;
      end loop;
   end Init_Knight_Attacks;

   procedure Init_King_Attacks is
      King_Moves : array (0..7,0..1) of Integer := ((1,1),(1,0),(1,-1),(0,1),(0,-1),(-1,1),(-1,0),(-1,-1));
   begin
      for I in 0..63 loop
         King_Attacks_I(I) := 0;
      end loop;
      for I in 0..7 loop
         for J in 0..7 loop
            for K in 0..7 loop
               if Is_In_Bounds(I+King_Moves(K,0),J+King_Moves(K,1)) then
                  King_Attacks_B(I*8+J)((I+King_Moves(K,0))*8+J+King_Moves(K,1)) := 1; end if;
            end loop;
         end loop;
      end loop;
   end Init_King_Attacks;

   procedure Init_White_Pawn_Attacks is
      Pawn_Inc  : array (0..1,0..1) of Integer := ((1,1),(1,-1));
      I : Integer :=0;
      J : Integer :=0;
   begin
      for I in 0..63 loop
         White_Pawn_Attacks_I(I):=0;
      end loop;
      for I in 0..7 loop
         for J in 0..7 loop
            for K in 0..1 loop
               if Is_In_Bounds(I+Pawn_Inc(K,0),J+Pawn_Inc(K,1)) then
                  White_Pawn_Attacks_B(I*8+J)((I+Pawn_Inc(K,0))*8+J+Pawn_Inc(K,1)) := 1;
               end if;
            end loop;
         end loop;
      end loop;
   end Init_White_Pawn_Attacks;

   procedure Init_Black_Pawn_Attacks is
      Pawn_Inc  : array (0..1,0..1) of Integer := ((-1,1),(-1,-1));
   begin
      for I in 0..63 loop
         Black_Pawn_Attacks_I(I):=0;
      end loop;
      for I in 0..7 loop
         for J in 0..7 loop
            for K in 0..1 loop
               if Is_In_Bounds(I+Pawn_Inc(K,0),J+Pawn_Inc(K,1)) then
                  Black_Pawn_Attacks_B(I*8+J)((I+Pawn_Inc(K,0))*8+J+Pawn_Inc(K,1)) := 1; end if;
            end loop;
         end loop;
      end loop;
   end Init_Black_Pawn_Attacks;


   procedure Init_White_Pawn_Moves1 is
   begin
      for I in 0..63 loop
         White_Pawn_Moves1_I(I):=0;
      end loop;
      for I in 1..7 loop
         for J in 0..7 loop
            if Is_In_Bounds(I+1,J) then
               White_Pawn_Moves1_B(I*8+J)((I+1)*8+J) := 1; end if;
         end loop;
      end loop;
   end Init_White_Pawn_Moves1;

   procedure Init_Black_Pawn_Moves1 is
   begin
      for I in 0..63 loop
         Black_Pawn_Moves1_I(I):=0;
      end loop;
      for I in 0..6 loop
         for J in 0..7 loop
            if Is_In_Bounds(I-1,J) then
               Black_Pawn_Moves1_B(I*8+J)((I-1)*8+J) := 1; end if;
         end loop;
      end loop;
   end Init_Black_Pawn_Moves1;

   procedure Init_White_Pawn_Moves2 is
   begin
      for I in 0..63 loop
         White_Pawn_Moves2_I(I):=0;
      end loop;
      for J in 0..7 loop
         White_Pawn_Moves2_B(8+J)(24+J) := 1;
      end loop;
   end Init_White_Pawn_Moves2;

   procedure Init_Black_Pawn_Moves2 is
   begin
      for I in 0..63 loop
         Black_Pawn_Moves2_I(I):=0;
      end loop;
      for J in 0..7 loop
         Black_Pawn_Moves2_B(48+J)(32+J) := 1;
      end loop;
   end Init_Black_Pawn_Moves2;

   --            All_Pieces_90_B(8*I+J) := All_Pieces_B(8*(7-J)+I);

   function Find_Rank(B:Natural) return Natural is
   begin
      return Natural(Shift_Right(All_Pieces_I and (Shift_Left(255,8*(B/8))),8*(B/8)));
   end Find_Rank;

   function Find_Rank_90(B:Natural) return Natural is
   begin
      return Natural(Shift_Right(All_Pieces_90_I and (Shift_Left(255,8*(B mod 8))),8*(B mod 8)));
   end Find_Rank_90;

   function Find_Rank_45r (B:Natural) return Natural is
   begin
      return Natural(Shift_Right((All_Pieces_45r_I and Mask_45r_rev(B)),Start_45r_Rev(B)));
   end Find_Rank_45r;

   function Find_Rank_45l (B:Natural) return Natural is
   begin
      return Natural(Shift_Right((All_Pieces_45l_I and Mask_45l_rev(B)),Start_45l_Rev(B)));
   end Find_Rank_45l;

   procedure Init_Rank_Attacks is
      Base,Ind : Integer;
      R : Unsigned_8;
      Final_I : Intboard;
      Final_B : Bitboard;
      pragma Import(Ada,Final_B);
      for Final_B'Address use Final_I'Address;
      L,D : Integer;
   begin
      for I in 0..63 loop
         Base := I/8;Ind := I mod 8;
         for J in Unsigned_8 (0) .. Unsigned_8(255)  loop
            Rank_Mobility(I,Integer(J)) := 0;
            Rank_Mobility_90(I,Integer(J)) := 0;
            Rank_Mobility_45r(I,Integer(J)) := 0;
            Rank_Mobility_45l(I,Integer(J)) := 0;
            R:=0;
            for K in reverse 0 .. Ind-1 loop
               R := R or Shift_Left(1,K);
               Rank_Mobility(I,Integer(J)) := Rank_Mobility(I,Integer(J))+1;
               exit when (J and Shift_Left(1,K))/=0;
            end loop;
            for K in Ind+1 .. 7 loop
               R := R or Shift_Left(1,K);
               Rank_Mobility(I,Integer(J)) := Rank_Mobility(I,Integer(J))+1;
               exit when (J and Shift_Left(1,K))/=0;
            end loop;
            Rank_Attacks_I(I,Integer(J)) := Shift_Left(Intboard(R),(8*Base));
         end loop;
      end loop;

      for I in 0..7 loop
         for J in 0..7 loop
            for M in 0..255 loop
               for B in 0..7 loop
                  Rank_Attacks_90_B(8*I+J,M)(8*(7-B)+J) := Rank_Attacks_B(7-I,M)(B);
                  if Rank_Attacks_B(7-I,M)(B)=1 then
                     Rank_Mobility_90(8*I+J,M):= Rank_Mobility_90(8*I+J,M)+1;
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;

      for B in 0..63 loop
         L := To_45r(B);
         for J in 0..(2**Length_45r(L)-1) loop
            Rank_Attacks_45r_I(B,J) := 0;
            Final_I := Rank_Attacks_I(Pos_45r(L),J) and (2**Length_45r(L)-1);
            for K in 0 .. Length_45r(L)-1 loop
               D := From_45r(K+Start_45r(L));
               Rank_Attacks_45r_B(B,J)(D) := Final_B(K);
               if Final_B(K)=1 then
                  Rank_Mobility_45r(B,J):=Rank_Mobility_45r(B,J)+1;
               end if;
            end loop;
         end loop;
      end loop;

      for B in 0..63 loop
         L := To_45l(B);
         for J in 0..(2**Length_45l(L)-1) loop
            Rank_Attacks_45l_I(B,J) := 0;
            Final_I := Rank_Attacks_I(Pos_45l(L),J) and (2**Length_45l(L)-1);
            for K in 0 .. Length_45l(L)-1 loop
               D := From_45l(K+Start_45l(L));
               Rank_Attacks_45l_B(B,J)(D) := Final_B(K);
               if Final_B(K)=1 then
                  Rank_Mobility_45l(B,J):=Rank_Mobility_45l(B,J)+1;
               end if;
            end loop;
         end loop;
      end loop;


   end Init_Rank_Attacks;

   procedure Init_Attacks is
   begin
      Init_Knight_Attacks;
      Init_King_Attacks;
      Init_White_Pawn_Attacks;
      Init_Black_Pawn_Attacks;
      Init_White_Pawn_Moves1;
      Init_Black_Pawn_Moves1;
      Init_White_Pawn_Moves2;
      Init_Black_Pawn_Moves2;
      Init_Rank_Attacks;
   end Init_Attacks;

   procedure Print_Bitboard (B:Bitboard) is
   begin
      for I in reverse 0..7 loop
        for J in 0 .. 7 loop
          if B(I*8+J)=1 then Put("X "); else Put(". "); end if;
        end loop;
        Put_Line("");
      end loop;
   end Print_Bitboard;

   procedure Print_Bitboard_45 (B:Bitboard) is
      L : Integer := 1;
      K : Integer := 1;
      Base : Integer := 63;
   begin
      while L <=15 loop
         for I in 1..8-K loop
            Put("  ");
         end loop;
         for I in Base..Base+K-1 loop
            if B(I)=1 then Put("X "); else Put(". "); end if;
            Put("  ");
         end loop;
         Put_Line("");
         L := L+1;
         if L<=8 then K:=L; else K := 16-L; end if;
         Base := Base - K;
      end loop;
   end Print_Bitboard_45;


   procedure Print_Chessboard is
      K : Integer;
   begin
      for I in reverse 0..7 loop
         for J in 0..7 loop
            K := I*8+J;
            if All_Pieces_B(K)=1 then
               begin
                  if White_Rooks_B(K)=1 then Put("R "); end if;
                  if Black_Rooks_B(K)=1 then Put("r "); end if;
                  if White_Knights_B(K)=1 then Put("N "); end if;
                  if Black_Knights_B(K)=1 then Put("n "); end if;
                  if White_Bishops_B(K)=1 then Put("B "); end if;
                  if Black_Bishops_B(K)=1 then Put("b "); end if;
                  if White_Queens_B(K)=1 then Put("Q "); end if;
                  if Black_Queens_B(K)=1 then Put("q "); end if;
                  if White_Kings_B(K)=1 then Put("K "); end if;
                  if Black_Kings_B(K)=1 then Put("k "); end if;
                  if White_Pawns_B(K)=1 then Put("P "); end if;
                  if Black_Pawns_B(K)=1 then Put("p "); end if;
               end;
            else
               Put(". ");
            end if;
         end loop;
         Put_Line("");
      end loop;
   end Print_Chessboard;

   procedure Print_Chessboard(F:File_Type) is
      K : Integer;
   begin
      for I in reverse 0..7 loop
         for J in 0..7 loop
            K := I*8+J;
            if All_Pieces_B(K)=1 then
               begin
                  if White_Rooks_B(K)=1 then Put(F,"R "); end if;
                  if Black_Rooks_B(K)=1 then Put(F,"r "); end if;
                  if White_Knights_B(K)=1 then Put(F,"N "); end if;
                  if Black_Knights_B(K)=1 then Put(F,"n "); end if;
                  if White_Bishops_B(K)=1 then Put(F,"B "); end if;
                  if Black_Bishops_B(K)=1 then Put(F,"b "); end if;
                  if White_Queens_B(K)=1 then Put(F,"Q "); end if;
                  if Black_Queens_B(K)=1 then Put(F,"q "); end if;
                  if White_Kings_B(K)=1 then Put(F,"K "); end if;
                  if Black_Kings_B(K)=1 then Put(F,"k "); end if;
                  if White_Pawns_B(K)=1 then Put(F,"P "); end if;
                  if Black_Pawns_B(K)=1 then Put(F,"p "); end if;
               end;
            else
               Put(F,". ");
            end if;
         end loop;
         Put_Line(F,"");
      end loop;
   end Print_Chessboard;

   procedure Print_Chessboard2 is
      K : Integer;
   begin
      for I in reverse 0..7 loop
         for J in 0..7 loop
            K := I*8+J;
            case Chess_Board(K) is
               when White_Rook =>  Put("R ");
               when Black_Rook => Put("r ");
               when White_Knight => Put("N ");
               when Black_Knight => Put("n ");
               when White_Bishop => Put("B ");
               when Black_Bishop => Put("b ");
               when White_Queen => Put("Q ");
               when Black_Queen => Put("q ");
               when White_King => Put("K ");
               when Black_King => Put("k ");
               when White_Pawn => Put("P ");
               when Black_Pawn => Put("p ");
               when Empty => Put(". ");
               when others => null;
            end case;
         end loop;
         Put_Line("");
      end loop;
   end Print_Chessboard2;

   procedure Print_Fen(F : File_Type;Col : Color;Cast:Castling;En_Passant:Integer) is
      K : Integer;
      Cpt :Integer :=0;
      S : String(1..3) := "   ";
      procedure End_Cpt is
      begin
         if Cpt = 0 then return; end if;
         Put(F,Integer'Image(Cpt)(Integer'Image(Cpt)'First+1..Integer'Image(Cpt)'Last));
         Cpt := 0;
      end End_Cpt;
   begin
      for I in reverse 0..7 loop
         for J in 0..7 loop
            K := I*8+J;
            case Chess_Board(K) is
               when White_Rook =>  End_Cpt;Put(F,"R");
               when Black_Rook => End_Cpt; Put(F,"r");
               when White_Knight => End_Cpt; Put(F,"N");
               when Black_Knight => End_Cpt; Put(F,"n");
               when White_Bishop => End_Cpt; Put(F,"B");
               when Black_Bishop => End_Cpt; Put(F,"b");
               when White_Queen => End_Cpt; Put(F,"Q");
               when Black_Queen => End_Cpt; Put(F,"q");
               when White_King => End_Cpt; Put(F,"K");
               when Black_King => End_Cpt; Put(F,"k");
               when White_Pawn => End_Cpt; Put(F,"P");
               when Black_Pawn => End_Cpt; Put(F,"p");
               when Empty => Cpt := Cpt+1;
               when others => null;
            end case;
         end loop;
         if I/=0 then End_Cpt;Put(F,"/");end if;
      end loop;
      if Col=White then Put(F," w "); else Put(F," b "); end if;
      if not (Cast.Wkc or Cast.Wqc or Cast.Bkc or Cast.Bqc) then Put(F,"-");
      else
         if Cast.WKc then Put(F,"K"); end if;
         if Cast.WQc then Put(F,"Q"); end if;
         if Cast.BKc then Put(F,"k"); end if;
         if Cast.BQc then Put(F,"q"); end if;
      end if;
      if En_Passant=-1 then Put(F," -");
      else
        S(2):=Character'Val(Character'Pos('a')+En_Passant mod 8);
        S(3):=Character'Val(Character'Pos('1')+En_Passant/8);
        Put(F,S);
      end if;
      Put(F,Integer'Image(Movenum-First_Ply));
      Put_line(F,Integer'Image(Movenum/2+1));
      Flush(F);
   end Print_Fen;

   procedure Check_All(S:String) is
      procedure Big_Error(S1:String) is
      begin
         Put_Line(S);
         Put_Line(S1);
         Print_Chessboard;
         Put_Line("");
         Print_Chessboard2;
         Put_Line("White pieces");
         Print_Intboard(White_Pieces_I);
         Put_Line("Black pieces");
         Print_Intboard(Black_Pieces_I);
         Put_Line("All_pieces");
         Print_Intboard(All_Pieces_I);
         for L in Piece(1)..Piece(15) loop
            if L/=4 and L/=8 and L/=12 then
               Put_Line("piece:"&piece'image(l));
               Print_Intboard(All_Boards(L).all);
            end if;
         end loop;
         Put_Line("");
         raise bitboards_error;
      end Big_Error;

      K : Integer;
   begin
      -- Count rooks

      for I in reverse 0..7 loop
         for J in 0..7 loop
            K := I*8+J;
            if Chess_Board(K) /=Empty then
               if (All_Color_Boards(Piece_Color(Chess_Board(K))).all and Set_Mask_I(K)) = 0 then Big_Error("1"); end if;
               if (All_Color_Boards(-Piece_Color(Chess_Board(K))).all and Set_Mask_I(K)) /= 0 then Big_Error("2") ; end if;
               if (All_Pieces_I and Set_Mask_I(K)) =0 then Big_Error("3"); end if;
               if (All_Boards(Chess_Board(K)).all and Set_Mask_I(K)) =0 then Big_Error("4") ; end if;
               for L in Piece(1)..Piece(15) loop
                  if L/=4 and L/=8  and L/= 12 and L /= Chess_Board(K) then
                     if (All_Boards(L).all and Set_Mask_I(K)) /=0 then Big_Error("First"&Piece'Image(L));
                     end if;
                  end if;
               end loop;
            else
               if ((White_Pieces_I or Black_Pieces_I) and Set_Mask_I(K)) /= 0 then Big_Error("6") ; end if;
               if (All_Pieces_I and Set_Mask_I(K)) /=0 then Big_Error("7") ; end if;
               for L in Piece(1)..Piece(15) loop
                  if L/=4 and L/=8 and L/=12 then
                     if (All_Boards(L).all and Set_Mask_I(K)) /=0 then Big_Error("Second"&Piece'Image(L));
                     end if;
                  end if;
               end loop;
            end if;
         end loop;
      end loop;
   end Check_All;

   function Sliding_Mover(P:Piece) return Boolean is
   begin
      return((P and 4)/=0);
   end Sliding_Mover;

   function Bishop_Mover(P:Piece) return Boolean is
   begin
      return((P and 1)/=0);
   end Bishop_Mover;

   function Rook_Mover(P:Piece) return Boolean is
   begin
      return((P and 2)/=0);
   end Rook_Mover;

   function Piece_Color(P : Piece) return Color is
   begin
      if (P and 8)=0 then return White; else return Black; end if;
   end Piece_Color;

   function White_Attacks_To (P:Integer) return Intboard is
      Whites : Intboard;
      Tmp : Intboard;
   begin
      Whites := Black_Pawn_Attacks_I(P) and White_Pawns_I;
      Whites := Whites or (King_Attacks_I(P) and White_Kings_I);
      Whites := Whites or (Knight_Attacks_I(P) and White_Knights_I);
      Tmp := Rank_Attacks_I(P,Find_Rank(P)) or Rank_Attacks_90_I(P,Find_Rank_90(P));
      Whites := Whites or (Tmp and (White_Rooks_I or White_Queens_I));
      Tmp := Rank_Attacks_45l_I(P,Find_Rank_45l(P)) or Rank_Attacks_45r_I(P,Find_Rank_45r(P));
      Whites := Whites or (Tmp and (White_Bishops_I or White_Queens_I));
      return Whites;
   end White_Attacks_To;

   function Black_Attacks_To (P:Integer) return Intboard is
      Blacks : Intboard;
      Tmp : Intboard;
   begin
      Blacks := White_Pawn_Attacks_I(P) and Black_Pawns_I;
      Blacks := Blacks or (King_Attacks_I(P) and Black_Kings_I);
      Blacks := Blacks or (Knight_Attacks_I(P) and Black_Knights_I);
      Tmp := Rank_Attacks_I(P,Find_Rank(P)) or Rank_Attacks_90_I(P,Find_Rank_90(P));
      Blacks := Blacks or (Tmp and (Black_Rooks_I or Black_Queens_I));
      Tmp := Rank_Attacks_45l_I(P,Find_Rank_45l(P)) or Rank_Attacks_45r_I(P,Find_Rank_45r(P));
      Blacks := Blacks or (Tmp and (Black_Bishops_I or Black_Queens_I));
      return Blacks;
   end Black_Attacks_To;


   procedure All_Attacks_To(P : Integer;Whites : out Intboard;Blacks : out Intboard) is
      Tmp : Intboard;
   begin
      Whites := Black_Pawn_Attacks_I(P) and White_Pawns_I;
      Whites := Whites or (King_Attacks_I(P) and White_Kings_I);
      Whites := Whites or (Knight_Attacks_I(P) and White_Knights_I);
      Blacks := White_Pawn_Attacks_I(P) and Black_Pawns_I;
      Blacks := Blacks or (King_Attacks_I(P) and Black_Kings_I);
      Blacks := Blacks or (Knight_Attacks_I(P) and Black_Knights_I);
      Tmp := Rank_Attacks_I(P,Find_Rank(P)) or Rank_Attacks_90_I(P,Find_Rank_90(P));
      Whites := Whites or (Tmp and (White_Rooks_I or White_Queens_I));
      Blacks := Blacks or (Tmp and (Black_Rooks_I or Black_Queens_I));
      Tmp := Rank_Attacks_45l_I(P,Find_Rank_45l(P)) or Rank_Attacks_45r_I(P,Find_Rank_45r(P));
      Whites := Whites or (Tmp and (White_Bishops_I or White_Queens_I));
      Blacks := Blacks or (Tmp and (Black_Bishops_I or Black_Queens_I));
   end All_Attacks_To;

   procedure All_Diag_Attacks_To(P : Integer;Whites : out Intboard;Blacks : out Intboard) is
      Tmp : Intboard;
   begin
      Tmp := Rank_Attacks_45l_I(P,Find_Rank_45l(P)) or Rank_Attacks_45r_I(P,Find_Rank_45r(P));
      Whites := (Tmp and (White_Bishops_I or White_Queens_I));
      Blacks := (Tmp and (Black_Bishops_I or Black_Queens_I));
   end All_Diag_Attacks_To;


   procedure All_Ortho_Attacks_To(P : Integer;Whites : out Intboard;Blacks : out Intboard) is
      Tmp : Intboard;
   begin
      Tmp := Rank_Attacks_I(P,Find_Rank(P)) or Rank_Attacks_90_I(P,Find_Rank_90(P));
      Whites := (Tmp and (White_Rooks_I or White_Queens_I));
      Blacks := (Tmp and (Black_Rooks_I or Black_Queens_I));
   end All_Ortho_Attacks_To;

end Bitboards;
