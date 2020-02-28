module Tests

open System
open System.Text
open Expecto
open FsCheck
open FsQuotedPrintable

// Ensure that the input size for tests is large enough to test cases like
// line-splitting.
let private fsCheckConfig =
    { FsCheckConfig.defaultConfig with endSize = 1000 }

[<Tests>]
let tests =
    testList "samples" [
        testCase "Encoding a string with no characters that need to be encoded returns the string" <| fun _ ->
            let original = "hello world"
            Expect.equal (EncodeString(original, Encoding.ASCII)) original "string that doesn't need encoding is unmodified"

        testCase "Encoding a string with an equals sign works" <| fun _ ->
            let original = "E=mc2"
            Expect.equal (EncodeString(original, Encoding.ASCII)) "E=3Dmc2" "string with an equals is encoded correctly"

        testCase "Encoding a string with non-7-bit clean characters encodes those characters" <| fun _ ->
            let original = "Äpfel"
            Expect.equal (EncodeString(original, Encoding.UTF8)) "=C3=84pfel" "non-7-bit clean characters are encoded correctly"

        testCase "Decoding a string with no encoded characters returns the string" <| fun _ ->
            let original = "hello world"
            Expect.equal (DecodeString(original, Encoding.ASCII)) original "string that doesn't need decoding is unmodified"

        testCase "Decoding a string with an equals sign works" <| fun _ ->
            let original = "E=3Dmc2"
            Expect.equal (DecodeString(original, Encoding.ASCII)) "E=mc2" "string containing equals sign"

        testCase "Decoding a string with non-7-bit clean characters decodes those characters" <| fun _ ->
            let original = "=C3=84pfel"
            Expect.equal (DecodeString(original, Encoding.UTF8)) "Äpfel" "string containing non-7-bit clean characters"

        testCase "Decoding a multi-line string" <| fun _ ->
            let original = "abc=" + Environment.NewLine + "def"
            Expect.equal (DecodeString(original, Encoding.UTF8)) "abcdef" "multi-line string"

        testPropertyWithConfig fsCheckConfig "Arbitrary strings can be roundtripped" <| fun (msgNonNull: NonNull<string>) ->
            match msgNonNull with
            | NonNull(msg) ->
                Expect.equal (DecodeString(EncodeString(msg, Encoding.UTF8), Encoding.UTF8)) msg "can roundtrip arbitrary strings"

        testPropertyWithConfig fsCheckConfig "Encoded strings are never greater than 76 characters per line" <| fun (msgNonNull: NonNull<string>) ->
            match msgNonNull with
            | NonNull(msg) ->
                let encoded = EncodeString(msg, Encoding.UTF8)
                let lines = encoded.Split([| Environment.NewLine |], StringSplitOptions.None)
                lines |> Seq.iter (fun line -> Expect.isTrue (line.Length <= 76) "encoded lines must be <= 76 characters in length")
     ]
