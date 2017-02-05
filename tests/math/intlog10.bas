# include "fbcu.bi"

# include "fb/math.bi"

using fb.math

namespace fbc_tests.math.intlog10_

sub test cdecl( )

  CU_ASSERT_EQUAL( intlog10( 1ul  ), 0 )
  CU_ASSERT_EQUAL( intlog10( 1ull ), 0 )

  dim as ulongint pow10 = 10

  '' test each power of 10, and each power of 10 minus 1
  for i as integer = 1 to 19
    '' test ulongint param
    CU_ASSERT_EQUAL( intlog10( pow10 - 1 ), i - 1 ) ''  999...
    CU_ASSERT_EQUAL( intlog10( pow10 ), i )         '' 1000...

    '' test ulong param if no overflow
    if pow10 = culng( pow10 ) then
      CU_ASSERT_EQUAL( intlog10( culng(pow10 - 1) ), i - 1 )
      CU_ASSERT_EQUAL( intlog10( culng(pow10) ), i )
    end if

    pow10 *= 10
  next i

  '' test max ulong/ulongint values

  CU_ASSERT_EQUAL( intlog10( 4294967295ul ), 9 ) '' 2^32-1
  CU_ASSERT_EQUAL( intlog10( 18446744073709551615ull ), 19 ) '' 2^64-1

end sub


sub ctor( ) constructor
	fbcu.add_suite( "fbc_tests.math.intlog10_" )
	fbcu.add_test( "test", @test )
end sub

end namespace
