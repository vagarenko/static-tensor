# Revision history for static-tensor

## Next

* Added `fromHomogenous` and `toHomogenous` vector functions.

## 0.2.0.0  -- 2017-09-20

* **BREAKING:** Changed `PositiveDims` type family to return `Constraint` instead of `Bool`

* Fixed suboptimal Core for `ounzip`, `adjugateMatrix`, `cofactor`, `cofactorMatrix`, `determinant`, 
`inverse`, `minor`, `minorMatrix`, `transpose` functions.

## 0.1.0.0  -- 2017-09-15

* First version. Released on an unsuspecting world.
