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
with System.Machine_Code;
use System.Machine_Code;
with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;
with Util;
use Util;
with Bitboards;
use Bitboards;
with Text_Io;

package body Bitscan  is
   
-- FirstOne for 32 bits architecture   
--   function First_One(A:Intboard) return Natural  is
--      S:Halfboard;
--      for S'Address use A'Address;
--      R:Integer_32;
--   begin
--      pragma Assert(A/=0,"A equals to 0");
--      Asm(
--          "bsfl %%eax,%%eax"& LF & HT &
--          "jnz 1f"& LF & HT &
--          "bsfl %%edx,%%eax"& LF & HT &
--          "jz 1f"& LF & HT &
--          "addl $32,%%eax"& LF & HT &
--          "1: ",
--          Inputs => (Unsigned_32'Asm_Input ("a",S.L),
--                     Unsigned_32'Asm_Input ("d",S.H)),
--          Outputs => Integer_32'Asm_Output ("=a", R),
--          Clobber => "");
--      return(Natural(R));
--   end First_One;

-- LastOne for 32 bits architecture   
--   function Last_One(A:Intboard) return Natural is
--      S:Halfboard ;
--      for S'Address use A'Address;
--      R:Integer_32;
--   begin
--      pragma Assert(A/=0,"A equals to 0");
--      Asm(
--          "bsrl %%eax,%%eax"& LF & HT &
--          "jz 2f"& LF & HT &
--          "addl $32,%%eax"& LF & HT &
--          "jmp 1f"& LF & HT &
--          "2: bsrl %%edx,%%eax"& LF & HT &
--          "1: ",
--          Inputs => (Unsigned_32'Asm_Input ("d",S.L),
--                     Unsigned_32'Asm_Input ("a",S.H)),
--          Outputs => Integer_32'Asm_Output ("=a", R),
--          Clobber => "");
--      return(Natural(R));
--   end Last_One;
   
   
-- Count bits for 32 bits architecture
--   function Count_Bits(A:Intboard) return Natural is
--      S:Halfboard ;
--      for S'Address use A'Address;
--      R:Integer_32;
--   begin
--         if A=0 then return 0; end if;
--      pragma Assert(A/=0,"A equals to 0");
--      Asm(
--          "xorl %%ecx,%%ecx"& LF & HT &
--          "testl %%eax,%%eax"& LF & HT &
--          "jz 2f"& LF & HT &
--          "1: leal -1(%%eax),%%ebx"& LF & HT &
--          "incl %%ecx"& LF & HT &
--          "andl %%ebx,%%eax"& LF & HT &
--          "jnz 1b"& LF & HT &
--          "2: testl %%edx,%%edx"& LF & HT &
--          "jz 4f"& LF & HT &
--          "3: leal -1(%%edx),%%ebx"& LF & HT &
--          "incl %%ecx"& LF & HT &
--          "andl %%ebx,%%edx"& LF & HT &
--          "jnz 3b"& LF & HT &
--          "4: movl %%ecx,%%eax",
--          Inputs => (Unsigned_32'Asm_Input ("d",S.L),
--                     Unsigned_32'Asm_Input ("a",S.H)),
--          Outputs => Integer_32'Asm_Output ("=a", R),
--          Clobber => "ebx,ecx");
--      return(Natural(R));
--   end Count_Bits;
   
   
-- FirstOne / LastOne for 64 bits arch
   function First_One(A:Intboard) return Natural  is
      R:Integer_64;
   begin
      if A=0 then
	 Text_Io.Put_Line(File_Log,"Error On The Following Chessboard:");
	 Print_Chessboard(File_Log);
	 Text_Io.Put_Line(File_Log,"Chessboard printed");
         raise Bitscan_Error_First;
      end if;
      Asm(
          "bsfq %%rax,%%rax",
          Inputs => Unsigned_64'Asm_Input ("a",Unsigned_64(A)),
          Outputs => Integer_64'Asm_Output ("=a", R),
          Clobber => "");
      return(Natural(R));
   end First_One;

   function Last_One(A:Intboard) return Natural  is
      R:Integer_64;
   begin
      if A=0 then
         Print_Chessboard(File_Log);
         raise Bitscan_Error_Last;
      end if;
      Asm(
          "bsrq %%rax,%%rax",
          Inputs => Unsigned_64'Asm_Input ("a",Unsigned_64(A)),
          Outputs => Integer_64'Asm_Output ("=a", R),
          Clobber => "");
      return(Natural(R));
   end Last_One;

--Count bits for 64 bits architecture without the popcnt instruction
--   function Count_Bits(A:Intboard) return Natural is
--      R:Integer_64;
--   begin
--      if A=0 then return 0; end if;
--      Asm(
--          "xorq %%rcx,%%rcx"& LF & HT &
--	    "testq %%rax,%%rax"& LF & HT &
--	    "jz 2f"& LF & HT &
--	    "1: leaq -1(%%rax),%%rbx"& LF & HT &
--	    "incq %%rcx"& LF & HT &
--	    "andq %%rbx,%%rax"& LF & HT &
--	    "jnz 1b"& LF & HT &
--	    "2: movq %%rcx,%%rax",
--        Inputs => Unsigned_64'Asm_Input ("a",Unsigned_64(A)),
--          Outputs => Integer_64'Asm_Output ("=a", R),
--          Clobber => "rbx,rcx");
--      return(Natural(R));
--   end Count_Bits;
   
--Count bits for 64 bits architecture with the popcnt instruction
   function Count_Bits(A:Intboard) return Natural is
      R:Integer_64;
   begin
      if A=0 then return 0; end if;
      Asm(
          "popcnt %%rax,%%rax",
          Inputs => Unsigned_64'Asm_Input ("a",Unsigned_64(A)),
          Outputs => Integer_64'Asm_Output ("=a", R),
          Clobber => "");
      return(Natural(R));
   end Count_Bits;
   

end Bitscan;
