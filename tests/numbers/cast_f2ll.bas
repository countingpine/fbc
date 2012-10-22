# include "fbcu.bi"

namespace fbc_tests.numbers.cast_f2ll

	sub testnum( byval n as ulongint )

		'' only run when n has <= 53 significant bits
		if( n and -((n and -n) shl 53) ) then return

		dim x as double = cdbl(n)

		if( n < 1ull shl 63 ) then
			'' make sure that cdbl(ll) concurs with cdbl(ull)
			CU_ASSERT_EQUAL( x, cdbl(clngint(n)) )

			CU_ASSERT_EQUAL( culngint(x), n )
			CU_ASSERT_EQUAL( clngint(x), clngint(n) )
			CU_ASSERT_EQUAL( clngint(-x), -clngint(n) )

			if( n < 1ull shl 52 ) then
				CU_ASSERT_EQUAL( culngint(x + 0.5), n + (n and 1) )
				CU_ASSERT_EQUAL( culngint(x - 0.5), n - (n and 1) )

				CU_ASSERT_EQUAL( clngint(x + 0.5), n + (n and 1) )
				CU_ASSERT_EQUAL( clngint(x - 0.5), n - (n and 1) )

				CU_ASSERT_EQUAL( clngint(-(x + 0.5)), -(n + (n and 1)) )
				CU_ASSERT_EQUAL( clngint(-(x - 0.5)), -(n - (n and 1)) )
			end if

			if( n < 1ull shl 51 ) then
				CU_ASSERT_EQUAL( culngint(x + 0.25), n )
				CU_ASSERT_EQUAL( culngint(x - 0.25), n )

				CU_ASSERT_EQUAL( clngint(x + 0.25), clngint(n) )
				CU_ASSERT_EQUAL( clngint(x - 0.25), clngint(n) )

				CU_ASSERT_EQUAL( clngint(-(x + 0.25)), -clngint(n) )
				CU_ASSERT_EQUAL( clngint(-(x - 0.25)), -clngint(n) )
			end if

		else
			CU_ASSERT_EQUAL( culngint(x), n )
			if( n = 1ull shl 63 ) then
				CU_ASSERT_EQUAL( clngint(-x), -1ll shl 63 )
			end if
		end if

	end sub

	sub test_cast_ll cdecl()

		dim as longint n = 1ll
		dim as double x = 1.0

		dim as integer i, j, k, l

		'' test powers of 2
		for i = 0 to 63
			testnum( 1ull shl i )
		next i

		'' test various bit combinations
		for i = 0 to 63-52
			for j = iif(i=0, 0, 52) to 52
				for k = 0 to j-2
					for l = 0 to k-1
						'' try to cover various different bit patterns
						testnum( (j + k + l) shl i )
						testnum( (j + k - l) shl i )
						testnum( (j - k + l) shl i )
						testnum( (j - k - l) shl i )
					next l
				next k
			next j
		next i

	end sub

	sub ctor () constructor

		fbcu.add_suite("fbc_tests.numbers.cast_f2ll")
		fbcu.add_test("test_cast_ll",  @test_cast_ll)

	end sub

end namespace
