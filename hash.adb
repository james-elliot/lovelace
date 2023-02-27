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
with Adamt19937;
with Bitboards;
use Bitboards;
with Xboard;
use Xboard;
with Text_Io;
use Text_Io;
with Util;
use Util;

package body Hash is

   G      : AdaMT19937.Generator;
   Keys   : AdaMT19937.Access_Vector := new AdaMT19937.Vector (0 .. 3);


   Nb_Bits : constant Natural :=25;
   Table : array(0..2**Nb_Bits-1) of Hash_Type
     := (others=>(
		  I=>0,
		  Low=>0,
		  High=>0,
		  From=>0,
		  To=>0,
		  Prof=>-128,
		  Movenum=>0,
		  Valid=>False
--		  Board=>(others=>0)
		 ));

   -- PTable: permanent hash table. Stores leaves (positions) scores and checkmated status
   -- 32766 and -32766 are for checkmated status
   -- 32765 is a value reserved for positions already tested as not being checkmated put not evaluated
   -- yet as a leaves
   PTable : array(0..2**Nb_Bits-1) of PHash_Type
     := (others=>(I=>0,Value=>0));

   function Is_In_Table return Boolean is
      Ind : Integer := Integer(Shift_Right(Z_I,64-Nb_Bits));
   begin
      return ((Table(Ind).I=Z_I) and then (Integer(Table(Ind).Prof)/=-128));
   end Is_In_Table;

   -- Warning!!!!!
   -- Only call this function after the above one
   -- You must check that the element is in the table before taking it!
   function Get_From_Table return Hash_Type is
   begin
      return Table(Integer(Shift_Right(Z_I,64-Nb_Bits)));
   end;
   
   procedure Print_Hash(Vhash:Hash_Type) is
      C : Color := White;
      Tmp : Piece;
      K : Integer;
   begin
      Put_Line(File_Log,"low="&Integer_16'Image(Vhash.Low)&" high="&Integer_16'Image(Vhash.High)&" from="&Unsigned_6'Image(Vhash.From)&" to="&Unsigned_6'Image(Vhash.To)&" valid="&Boolean'Image(Vhash.Valid)&" prof="&Integer_10'Image(Vhash.Prof)&" movenum="&Hash.Unsigned_8'Image(Vhash.Movenum));
      
--      for I in reverse 0..7 loop
--	 for J in 0..7 loop
--	    K := I*8+J;
--	    Tmp := Piece(Vhash.Board(K));
--	    if tmp=4 then C := Black; Tmp:=3; end if;
--	    if Tmp /= Chess_Board(K) then raise Hash_Error; end if;
--	    case Tmp is
--	       when White_Rook =>  Put(File_Log,"R ");
--	       when Black_Rook => Put(File_Log,"r ");
--	       when White_Knight => Put(File_Log,"N ");
--	       when Black_Knight => Put(File_Log,"n ");
--	       when White_Bishop => Put(File_Log,"B ");
--	       when Black_Bishop => Put(File_Log,"b ");
--	       when White_Queen => Put(File_Log,"Q ");
--	       when Black_Queen => Put(File_Log,"q ");
--	       when White_King => Put(File_Log,"K ");
--	       when Black_King => Put(File_Log,"k ");
--	       when White_Pawn => Put(File_Log,"P ");
--	       when Black_Pawn => Put(File_Log,"p ");
--	       when Empty => Put(File_Log,". ");
--	       when others => null;
--	    end case;
--	 end loop;
--	 Put_Line(File_Log,"");
--      end loop;
--      Put_Line(File_Log,"color="&Color'Image(C));
   end;
   
   procedure Store_To_Table(Low:Integer;
                            High:Integer;
                            From:Integer;
                            To:Integer;
                            Valid : Boolean;
                            Prof:Integer;
                            Movenum : Integer;
			    C : Color) is
      Ind : Integer;
      plus : Unsigned_4:=0;
      procedure Store is
      begin
         Table(Ind).Low := Integer_16(Low); Table(Ind).High := Integer_16(High);
         Table(Ind).From := Unsigned_6(From); Table(Ind).To := Unsigned_6(To);
         Table(Ind).Prof := Integer_10(Prof); Table(Ind).Movenum := Unsigned_8(Movenum);
         Table(Ind).Valid := Valid;Table(Ind).I := Z_I;
	 
--	 for I in 0..63 loop
--	    Plus := Unsigned_4(Chess_Board(I));
	    --Codage bizarre transformant le codage du roi blanc (3) en 4
	    --Comme le 4 n'existe pas cela permet de stocker la couleur (!)
--	    if Plus = 3 and C=Black then Plus:=4; end if;
--	    Table(Ind).Board(I):=Plus;
--	 end loop;
	 
      end Store;
      pragma Inline(Store);
   begin
      Ind := Integer(Shift_Right(Z_I,64-Nb_Bits));
      if Z_I = Table(Ind).I then
         -- Exactly same position
         if Prof >= Integer(Table(Ind).Prof) then
            --Same position researched deeper
            Store;
         else
           -- Same position, lower depth, store should not happen?
           --           raise Hash_Error;
            null;
         end if;
      else
         -- New position=> collision. Shall we replace the other one?
         if Movenum /= Integer(Table(Ind).Movenum) or else Prof >= Integer(Table(Ind).Prof) then
            --store if different move, or better prof
            Store;
         end if;
      end if;
   end;

   function Is_In_PTable return Boolean is
      Ind : Integer := Integer(Shift_Right(Z_I,64-Nb_Bits));
   begin
      return (PTable(Ind).I=Z_I);
   end Is_In_PTable;

   -- Warning!!!!!
   -- Only call this function after the above one
   -- You must check that the element is in the table before taking it!
   function Get_From_PTable return Integer is
   begin
      return Integer(PTable(Integer(Shift_Right(Z_I,64-Nb_Bits))).Value);
   end;

   procedure Store_To_PTable(Value:Integer) is
      Ind : Integer;
   begin
      Ind := Integer(Shift_Right(Z_I,64-Nb_Bits));
      PTable(Ind).Value := Integer_16(Value);
      PTable(Ind).I := Z_I;
   end;

   procedure Make_Z_I(C:Color) is
   begin
      Z_I :=0;
      for I in 0..63 loop
         if Chess_Board(I) /= Empty then
            Z_I := Z_I xor Z_P(I,Chess_Board(I));
         end if;
      end loop;
      if C=Black then Z_I := Z_I xor Z_C; end if;
   end Make_Z_I;

   procedure Check_Hash(C:Color) is
      Tmp : Unsigned_64 := Z_I;
   begin
      Make_Z_I(C);
      if Z_I/=Tmp then raise Hash_Error; end if;
      Z_I:=Tmp;
   end Check_Hash;


begin


   Keys.all := (16#123#, 16#234#, 16#345#, 16#456#);
   AdaMT19937.Reset (G, Keys);

   Z_C := Unsigned_64(AdaMT19937.Random(G)) +
     Shift_Left(Unsigned_64(AdaMT19937.Random(G)),32);
   
   for I in 0..63 loop
      Z_En_Passant(I) := Unsigned_64(AdaMT19937.Random(G)) +
	Shift_Left(Unsigned_64(AdaMT19937.Random(G)),32);
   end loop;
   
   Z_wqc := Unsigned_64(AdaMT19937.Random(G)) +
     Shift_Left(Unsigned_64(AdaMT19937.Random(G)),32);
   Z_wkc := Unsigned_64(AdaMT19937.Random(G)) +
     Shift_Left(Unsigned_64(AdaMT19937.Random(G)),32);
   Z_bqc := Unsigned_64(AdaMT19937.Random(G)) +
     Shift_Left(Unsigned_64(AdaMT19937.Random(G)),32);
   Z_bkc := Unsigned_64(AdaMT19937.Random(G)) +
     Shift_Left(Unsigned_64(AdaMT19937.Random(G)),32);

   Z_wqd := Unsigned_64(AdaMT19937.Random(G)) +
     Shift_Left(Unsigned_64(AdaMT19937.Random(G)),32);
   Z_wkd := Unsigned_64(AdaMT19937.Random(G)) +
     Shift_Left(Unsigned_64(AdaMT19937.Random(G)),32);
   Z_bqd := Unsigned_64(AdaMT19937.Random(G)) +
     Shift_Left(Unsigned_64(AdaMT19937.Random(G)),32);
   Z_bkd := Unsigned_64(AdaMT19937.Random(G)) +
     Shift_Left(Unsigned_64(AdaMT19937.Random(G)),32);
   
   ---
   -- A priori wqd ne doit pas apparaitre dans la valeur de hash
--   Z_wqc :=Unsigned_64(0);
--   Z_wkc :=Unsigned_64(0);
--   Z_bqc :=Unsigned_64(0);
--   Z_bkc :=Unsigned_64(0);
   Z_wqd :=Unsigned_64(0);
   Z_wkd :=Unsigned_64(0);
   Z_bqd :=Unsigned_64(0);
   Z_bkd :=Unsigned_64(0);
--- end debug
   
   for I in 0..63 loop
      for J in Piece(0)..Piece(15) loop
         Z_P(I,J) := Unsigned_64(AdaMT19937.Random(G)) +
           Shift_Left(Unsigned_64(AdaMT19937.Random(G)),32);
      end loop;
   end loop;

end Hash;
