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
  let pad := min 2 ((4 - s.length) % 4)
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

def encodeTable : Array Char := ⟨base64Str.data⟩

def encode (r : String) : String := Id.run do
  let arr := r.toUTF8
  let pad := (3 - arr.size) % 3
  let arr := arr ++ ⟨⟨List.replicate pad 0⟩⟩
  let mut s := ""
  let mut quadSum := 0
  let mut n := 0
  for c in arr do
    quadSum := quadSum.shiftLeft 8 + c.toUInt32
    n := n + 1
    if n >= 3 then do
      let d := (quadSum % 64).toNat
      quadSum := quadSum.shiftRight 6
      let c := (quadSum % 64).toNat
      quadSum := quadSum.shiftRight 6
      let b := (quadSum % 64).toNat
      quadSum := quadSum.shiftRight 6
      let a := (quadSum % 64).toNat
      s := s ++ ⟨[encodeTable[a]!, encodeTable[b]!, encodeTable[c]!, encodeTable[d]!]⟩
      n := 0
  pure $ s.dropRight pad ++ (List.replicate pad '=').asString

def decodeFileName : String → String := Base64.decode ∘ (fun s => s.replace "_" "/")

def encodeFileName : String → String := (fun s => s.replace "/" "_") ∘ Base64.encode

end Base64

abbrev String.decodeBase64 := Base64.decode
abbrev String.encodeBase64 := Base64.encode
abbrev String.decodeBase64FileName := Base64.decodeFileName
abbrev String.encodeBase64FileName := Base64.encodeFileName
