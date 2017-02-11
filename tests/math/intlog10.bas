# include "fbcu.bi"

# include "fb/math.bi"

using fb.math

namespace fbc_tests.math.intlog10_

sub check_intlog10( n as ulongint )

  dim as integer lg = len( str( n ) ) - 1

  CU_ASSERT_EQUAL( intlog10( n ), lg )

  if n <= &hfffffffful then
    CU_ASSERT_EQUAL( intlog10( culng( n ) ), lg )
  end if

end sub

sub test cdecl( )

  '' test 1 explicitly
  CU_ASSERT_EQUAL( intlog10( 1ul ), 0 )
  CU_ASSERT_EQUAL( intlog10( 1ull ), 0 )

  '' test ulong 2^31 and ulongint 2^63 explicitly
  CU_ASSERT_EQUAL( intlog10( &hfffffffful ), 9 )
  CU_ASSERT_EQUAL( intlog10( &hffffffffull ), 9 )
  CU_ASSERT_EQUAL( intlog10( &hffffffffffffffffull ), 19 )

  dim as ulongint pow10 = 10

  '' test (10^n)-1 and 10^n
  for i as integer = 1 to 19
    '' test ulongint param
    check_intlog10( pow10 - 1 ) ''  999...
    check_intlog10( pow10 )     '' 1000...

    pow10 *= 10
  next i

  '' test more arbitrary numbers (rounded powers of 1.5)
  for i as integer = 1 to int(log( 2^64 ) / log( 1.5 ))-1
    check_intlog10( culngint( 1.5^i ) - 1 )
    check_intlog10( culngint( 1.5^i ) )
  next i

end sub


sub ctor( ) constructor
	fbcu.add_suite( "fbc_tests.math.intlog10_" )
	fbcu.add_test( "test", @test )
end sub

end namespace
