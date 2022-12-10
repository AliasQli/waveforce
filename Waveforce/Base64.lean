import Init.Data.String.Basic

namespace Base64

def base64Str := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

def decodeTable : Array UInt32 := Id.run do
  let mut arr := mkArray 128 0
  for ⟨i, c⟩ in base64Str.data.enum do
    arr := arr.set! (c.val.toNat) i.toUInt32
  pure arr

def decode (s : String) : String := Id.run do
  let s := s.dropRightWhile (· == '=')
  let pad := min 2 (4 - s.length % 4)
  let s := s ++ (List.replicate pad 'A').asString
  let mut it := s.iter
  let mut arr := ByteArray.empty
  let mut quadSum := 0
  let mut n := 0
  while not it.atEnd do
    quadSum := quadSum.shiftLeft 6 + (decodeTable.get? it.curr.val.toNat).getD 0
    n := n + 1
    if n >= 4 then do
      let c := quadSum.toUInt8
      quadSum := quadSum.shiftRight 8
      let b := quadSum.toUInt8
      quadSum := quadSum.shiftRight 8
      let a := quadSum.toUInt8
      arr := arr.append ⟨#[a, b, c]⟩
      n := 0
    it := it.next
  pure $ (String.fromUTF8Unchecked arr).dropRight pad

end Base64

abbrev String.decodeBase64 := Base64.decode
