* this code calculates a sum over a few tens to hundreds of terms that
  vary greatly in size

* for M>30 the double float precision seems to be insufficient if the
  sum is calculated in a straight forward way

## attempts for a numerically stable implementation:

1. sort the summands by their absolute values (e.g. 0.01 -0.1 23 340
-405 ... 32044024 -32044025 ...) before adding them

2. use Kahan summation rule with or without sorting the summands

3. use arbitrary precision arithmetic

4. implement using __float128 instead of double float

## details

For 3. I make use of common lisps ratio data type and implemented the
exponential function as well as the square root function using
iterative schemes that can achieve arbitrary precision. This can be
rather slow but should give hundreds of digits in precision with a few
seconds runtime.

For 4. I used the C programming language and the libquadmath library.