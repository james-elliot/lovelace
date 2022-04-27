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
with Sequential_Io;
with Bitboards;
use Bitboards;
with Move;
use Move;
with Util;
use Util;
with Text_Io;
use Text_Io;
with AdaMT19937;
with Calendar;

package body Hashbook is

   type Simple_Move is
      record
         From : Unsigned_8;
         To : Unsigned_8;
      end record;

   type Move_Array is array(0..15) of Simple_Move;

   type Hash_Book is
      record
         Z_I : Unsigned_64;
         Moves : Move_Array;
         Won : Unsigned_16;
         Lost : Unsigned_16;
         Draw : Unsigned_16;
         Next : Natural;
      end record;

   G      : AdaMT19937.Generator;
   Keys   : AdaMT19937.Access_Vector := new AdaMT19937.Vector (0 .. 3);


   Nb_Bits : constant Natural := 16;
   Hash_Index : array(0..2**Nb_Bits-1) of Natural := (others=>0);
   Hash_Tab : array (1..130000) of Hash_Book:=(others=>(Z_I=>0,Moves=>(others=>(From=>255,To=>255)),Won=>0,Lost=>0,Draw=>0,Next=>0));
   Last_Elt : Integer := 1;

   package HTIo is new Sequential_Io(Hash_Book);
   package HIIo is new Sequential_Io(Natural);

   procedure Save_All is
      Fp1 : HTIo.File_Type;
      Fp2 : HIIo.File_Type;
   begin
      HTIo.Create(Fp1,Htio.Out_File,"book.bin");
      for I in Hash_Tab'range loop
         Htio.Write(Fp1,Hash_Tab(I));
      end loop;
      Htio.Close(Fp1);
      HIIo.Create(Fp2,Hiio.Out_File,"booki.bin");
      for I in Hash_Index'Range loop
         Hiio.Write(Fp2,Hash_Index(I));
      end loop;
      Hiio.Close(Fp2);
   end Save_All;

   function Load_All return Boolean is
      Fp1 : HTIo.File_Type;
      Fp2 : HIIo.File_Type;
   begin
      HIIo.Open(Fp2,Hiio.In_File,"booki.bin");
      for I in Hash_Index'Range loop
         Hiio.Read(Fp2,Hash_Index(I));
      end loop;
      Hiio.Close(Fp2);
      HTIo.Open(Fp1,Htio.In_File,"book.bin");
      for I in Hash_Tab'range loop
         Htio.Read(Fp1,Hash_Tab(I));
      end loop;
      Htio.Close(Fp1);
      return(True);
   exception
      when others => return(False);
   end Load_All;

   procedure Put_Book(Z_I : Unsigned_64;From,To : Integer;Res : Character) is
      Ind : Integer :=  Integer(Shift_Right(Z_I,64-Nb_Bits));
      T_Ind,Prev : Integer;
   begin
      if Hash_Index(Ind) /= 0 then
         T_Ind := Hash_Index(Ind);
         while (T_Ind /= 0 ) and then (Hash_Tab(T_Ind).Z_I/=Z_I) loop
            Prev := T_Ind;
            T_Ind := Hash_Tab(T_Ind).Next;
         end loop;
         if T_Ind =0 then
            T_Ind := Last_Elt;
            Hash_Tab(Prev).Next := Last_Elt;
            Last_Elt := Last_Elt+1;
         end if;
      else
         T_Ind := Last_Elt;
         Hash_Index(Ind) := T_Ind;
         Last_Elt := Last_Elt+1;
      end if;
      Hash_Tab(T_Ind).Z_I := Z_I;
      if Res = '1' then
         Hash_Tab(T_Ind).won := Hash_Tab(T_Ind).Won+1;
      elsif Res='5' then
         Hash_Tab(T_Ind).draw := Hash_Tab(T_Ind).draw+1;
      else
         Hash_Tab(T_Ind).lost := Hash_Tab(T_Ind).lost+1;
      end if;
      for I in 0..15 loop
         if Hash_Tab(T_Ind).Moves(I).From = Unsigned_8(From) and
           Hash_Tab(T_Ind).Moves(I).To = Unsigned_8(To) then
            exit;
         end if;
         if Hash_Tab(T_Ind).Moves(I).From = 255 then
            Hash_Tab(T_Ind).Moves(I).From := Unsigned_8(From);
            Hash_Tab(T_Ind).Moves(I).To := Unsigned_8(To);
            Exit;
         end if;
      end loop;
   end Put_Book;

   function Get_Ind (Z_I : Unsigned_64) return Integer is
      Ind : Integer :=  Integer(Shift_Right(Z_I,64-Nb_Bits));
      T_Ind : Integer;
   begin
      T_Ind := Hash_Index(Ind);
      while (T_Ind /= 0 ) and then (Hash_Tab(T_Ind).Z_I/=Z_I) loop
         T_Ind := Hash_Tab(T_Ind).Next;
      end loop;
      return T_Ind;
   end Get_Ind;

   procedure Get_Book(From,To : out Integer;Col : Color; Cast : Castling ; En_Passant : Integer) is
      Ind : Integer :=  Integer(Shift_Right(Z_I,64-Nb_Bits));
      T_Ind,N_Ind : Integer;
      St : State;
      Lcb : array (0..63) of Piece;
      Pond : array(0..128) of Float;
      All_Tot : array(0..128) of Integer;
      Moves : array(0..128) of Integer;
      P2 : Piece;
      Promote : Boolean;
      N_En_Passant : Integer;
      Valid : Boolean;
      N_Castles : Castling;
      Res  : Float;
      Sum : Float := 0.0;
      Mv : float := -10000.0;
      T_Max : Integer := 0;
      Best : Integer:=0;
      T2,Tot  : Integer;
      Ind_Max : Integer;
      My_From, My_To : Integer;
   begin
      Put_Line(File_Log,"starting get book");
      From := -1; To := -1;
      My_From := -1; My_To := -1;
      T_Ind := Get_Ind(Z_I);
      if T_Ind = 0 then return; end if;
      Put_Line(File_Log,"Found something");
      Put_Line(File_Log,Unsigned_16'Image(Hash_Tab(T_Ind).won)&
               Unsigned_16'Image(Hash_Tab(T_Ind).lost)&
               Unsigned_16'Image(Hash_Tab(T_Ind).draw));

      -- Code pour choisir le "meilleur" coup
      Save_State(St);
      for J in 0..63 loop Lcb(J):=Chess_Board(J); end loop;
      Ind_Max:=-1;
      for I in Hash_Tab(T_Ind).Moves'Range loop
         My_From := Integer(Hash_Tab(T_Ind).Moves(I).From);
         My_To := Integer(Hash_Tab(T_Ind).Moves(I).To);
         if My_From=255 then exit; end if;
         Really_Do_Move (My_From,My_To,Col,Cast,En_Passant,P2,Promote,N_En_Passant,Valid,N_Castles);
         N_Ind := Get_Ind(Z_I);
         Restore_State(St);
         for J in 0..63 loop Chess_Board(J) := Lcb(J); end loop;

         Tot := 0;
         if N_Ind /=0 then
	    if Col=White then
	       Res := Float(Integer(Hash_Tab(N_Ind).Won))+Float(Integer(Hash_Tab(N_Ind).Draw))/2.0;
	    else
	       Res := Float(Integer(Hash_Tab(N_Ind).Lost))+Float(Integer(Hash_Tab(N_Ind).Draw))/2.0;
	    end if;
            Tot := Integer(Hash_Tab(N_Ind).Draw)+Integer(Hash_Tab(N_Ind).Won)
	      +Integer(Hash_Tab(N_Ind).Lost);
            Put_Line(File_Log,
		     "From:"&Integer'Image(My_From)&" To:"&Integer'Image(My_To)&
		       " won:"&Unsigned_16'Image(Hash_Tab(N_Ind).won)&
		       " lost:"&Unsigned_16'Image(Hash_Tab(N_Ind).lost)&
		       " draw:"&Unsigned_16'Image(Hash_Tab(N_Ind).draw)&
		       " points:"&Float'Image(Res));
            Res := Res/Float(Tot);
         else
            Res := 0.00005;
         end if;
	 Put_Line(File_Log,"From:"&Integer'Image(My_From)&" To:"&Integer'Image(My_To)&" res:"&Float'Image(res)&" Tot:"&Integer'Image(Tot));
	 if Res >0.4 then
	    if Tot > T_Max then T_Max := Tot; end if;
	    Ind_Max := Ind_Max+1;
	   Pond(Ind_Max) := Res;
	   All_Tot(Ind_Max) := Tot;
	   Moves(Ind_Max):=I;
	 end if;
      end loop;
      
      if Ind_Max = -1 then return; end if;
      Put_Line(File_Log,"Computing sum and ponderation");
      for I in 0..Ind_max loop
	 Res := Pond(I) * Float(All_Tot(I))/Float(T_Max);
         if Res>Mv then Mv := Res; Best := I; end if;
	 My_From := Integer(Hash_Tab(T_Ind).Moves(Moves(I)).From);
	 My_To := Integer(Hash_Tab(T_Ind).Moves(Moves(I)).To);
	 Put_Line(File_Log,"From:"&Integer'Image(My_From)&" To:"&Integer'Image(My_To)&" pond:"&Float'Image(Pond(I))&" Tot:"&Integer'Image(All_Tot(I))&" Res:"&Float'Image(Res));
	 Pond(I):=Res;
	 Sum := Sum+Res;
      end loop;

      Put_Line(File_Log,"Probabilities:");
      for I in 0..Ind_max loop
	 Pond(I) := Pond(I)/Sum;
	 Put_Line(File_Log,Float'Image(Pond(I)));
      end loop;
      
      Put_Line(File_Log,"Combined probabilities:");
      for I in 0..Ind_max loop
	 if I /= Hash_Tab(T_Ind).Moves'First then Pond(I) := Pond(I)+Pond(I-1); end if;
	 Put_Line(File_Log,Float'Image(Pond(I)));
      end loop;
      
      Put_Line(File_Log,"Extracting random move with ponderation:");
      Res := Float(AdaMT19937.Random(G) mod 10000)/10000.0;
      for I in 0..Ind_max loop
	 T2:=I;
	 if Res<=Pond(I) then exit; end if;
      end loop;

      From := Integer(Hash_Tab(T_Ind).Moves(Moves(T2)).From);
      To := Integer(Hash_Tab(T_Ind).Moves(Moves(T2)).To);
      Put_Line(File_Log,"Move chosen:"&Integer'Image(From)&Integer'Image(To));
      Flush(File_Log);
   end Get_Book;

   function Get_Last return Integer is
   begin
      return Last_Elt;
   end Get_Last;

   Year,Month,Day : Integer;
   Seconds : Duration;
begin
   Calendar.Split(Calendar.Clock,Year,Month,Day,Seconds);
   Keys.all := (Unsigned_32(Seconds),Unsigned_32(Day),Unsigned_32(Month),Unsigned_32(Year));
   AdaMT19937.Reset (G, Keys);

end Hashbook;
