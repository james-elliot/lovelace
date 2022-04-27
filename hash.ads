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

package Hash is
   Hash_Error : exception;
   type Unsigned_4 is mod 2**4;
   for Unsigned_4'Size use 4;
   type Unsigned_6 is mod 2**6;
   for Unsigned_6'Size use 6;
   type Unsigned_8 is mod 2**8;
   for Unsigned_8'Size use 8;
   type Integer_10 is new Integer range -2**10..2**10-1;
   for Integer_10'Size use 11;
--   type Boards is array(0..63) of Unsigned_4;
--   pragma Pack(Boards);
--   for Boards'Size use 256;
   
   type Hash_Type is
      record
         I : Unsigned_64;
         Low : Integer_16;
         High : Integer_16;
         From : Unsigned_6;
         To : Unsigned_6;
         Valid : Boolean;
         Prof : Integer_10;
         Movenum : Unsigned_8;
--	 Board : Boards;
      end record;
   for Hash_Type use
      record
         I at 0 range 0..63;
         Low at 0 range 64..79;
         High at 0 range 80..95;
         From at 0 range 96..101;
         To at 0 range 102..107;
         Valid at 0 range 108..108;
         Prof at 0 range 109..119;
         Movenum at 0 range 120..127;
--	 Board at 0 range 128..383;
      end record;
   for Hash_Type'Size use 128;
--   for Hash_Type'Size use 384;

   
   function Is_In_Table return Boolean;
   pragma Inline(Is_In_Table);

   -- Warning!!!!!
   -- Only call this function after the above one
   -- You must check that the element is in the table before taking it!

   function Get_From_Table return Hash_Type;
   pragma Inline(Get_From_Table);

   procedure Store_To_Table(Low:Integer;
                            High:Integer;
                            From:Integer;
                            To:Integer;
                            Valid : Boolean;
                            Prof:Integer;
                            Movenum : Integer;
			    C : Color);
   pragma Inline(Store_To_Table);

   procedure Make_Z_I(C:Color);
   procedure Check_Hash(C:Color);


   type PHash_Type is
      record
         I : Unsigned_64;
         Value : Integer_16;
      end record;
   for PHash_Type use
      record
         I at 0 range 0..63;
         Value at 0 range 64..79;
      end record;

   for PHash_Type'Size use 80;

   function Is_In_PTable return Boolean;
   procedure Print_Hash(Vhash:Hash_Type);
   pragma Inline(Is_In_PTable);

   -- Warning!!!!!
   -- Only call this function after the above one
   -- You must check that the element is in the table before taking it!

   function Get_From_PTable return Integer;
   pragma Inline(Get_From_PTable);

   procedure Store_To_PTable(Value:Integer);
   pragma Inline(Store_To_PTable);

end Hash;
