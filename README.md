# Advent Of Code 2018

## Disclaimer

This is mostly experimental code. For practice, I try to reuse preexisting
recursion schemes.

## Days

### Day 01

The key insight would be that

Let `sums = [0, ps_1, ps_2, ..., ps_N]` be the partial sums of the input list.
`ps_N` is the total sum. There is either:

1. a repeated number in `sums`
2. `ps_N == 0`. In which case 0 is the repeated value.
3. There exists `i,j,n_i,n_j` such that `ps_i + n_i*ps_N = ps_j + n_j*ps_N`.
4. None of the above and there is no repeating value

If `3` is true then:

`(n_i - n_j)*ps_N = ps_j - ps_i`. (1)

This tell us that:

`(ps_j - ps_i) % ps_N == 0`. (2)

From all the pairs `(ps_j, ps_i)` we only need to consider those that satisfy
(2). For those:

`(n_i - n_j) = (ps_j - ps_i) div ps_N`. (3)

As it can be seen there is only information to compute `(n_i - n_j)`. This means
that the repeated value is one element in `sums`. So we take `n_i = 0`.

`n_j = -(ps_j - ps_i) div ps_N`. (4)

Of all those `n_j` we want the smallest and the one with the minimum `j`. Then the
answer is `sums !! i`.

## References

[Time traveling recursion schemes](https://jtobin.io/time-traveling-recursion)
[Recursion schemes field guide](http://comonad.com/reader/2009/recursion-schemes/)
[Useful recursion schemes](https://medium.com/@jaredtobin/a-tour-of-some-useful-recursive-types-8fa8e423b5b9)
