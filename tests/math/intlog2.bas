# include "fbcu.bi"

# include "fb/math.bi"

using fb.math

namespace fbc_tests.math.intlog2_

sub check_intlog2( n as ulongint )

  dim as integer lg = len( bin( n ) ) - 1

  CU_ASSERT_EQUAL( intlog2( n ), lg )

  if n <= &hfffffffful then
    CU_ASSERT_EQUAL( intlog2( culng( n ) ), lg )
  end if

end sub

sub test cdecl( )

  '' test 1 explicitly
  CU_ASSERT_EQUAL( intlog2( 1ul ), 0 )
  CU_ASSERT_EQUAL( intlog2( 1ull ), 0 )

  '' test ulong 2^31 and ulongint 2^63 explicitly
  CU_ASSERT_EQUAL( intlog2( &hfffffffful ), 31 )
  CU_ASSERT_EQUAL( intlog2( &hffffffffull ), 31 )
  CU_ASSERT_EQUAL( intlog2( &hffffffffffffffffull ), 63 )

  '' test (2^n)-1 and 2^n
  for i as integer = 1 to 63
    check_intlog2( (1ull shl i) - 1 )
    check_intlog2(  1ull shl i )
  next i

  '' test more arbitrary numbers (rounded powers of 1.5)
  for i as integer = 1 to int(log( 2^64 ) / log( 1.5 ))-1
    check_intlog2( culngint( 1.5^i ) - 1 )
    check_intlog2( culngint( 1.5^i ) )
  next i

end sub


sub ctor( ) constructor
	fbcu.add_suite( "fbc_tests.math.intlog2_" )
	fbcu.add_test( "test", @test )
end sub

end namespace
