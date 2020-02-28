module FsQuotedPrintable

open System
open System.Globalization
open System.IO
open System.Text

//---------------------------------------------------------------------
// Decoding (internal code)

type private DecoderState = 
    | Normal
    | EnteringEscapeSequence
    | FirstCharOfEscapeSequence of char

type private DecodingState = 
    {
        cs: char array
        acc: MemoryStream
        index: int
        decoderState: DecoderState
    }

let rec private decodeInternal state = 
    if state.index >= state.cs.Length then
        state.acc.ToArray()
    else
        match (state.decoderState, state.cs.[state.index]) with
        | (Normal, '=') -> 
            decodeInternal { state with index = state.index + 1; decoderState = DecoderState.EnteringEscapeSequence }
        | (Normal, c) ->
            state.acc.WriteByte(byte(c))
            decodeInternal { state with index = state.index + 1 }
        | (EnteringEscapeSequence, c) ->
            decodeInternal { state with index = state.index + 1; decoderState = DecoderState.FirstCharOfEscapeSequence(c) }
        | (FirstCharOfEscapeSequence(c1), c2) ->
            let charVal = System.Byte.Parse(new string([| c1; c2 |]), NumberStyles.HexNumber)
            let newChar = char(charVal)
            state.acc.WriteByte(byte(newChar))
            decodeInternal { state with index = state.index + 1; decoderState = DecoderState.Normal }


//---------------------------------------------------------------------
// Encoding (internal code)

type private EncodingState =
    {
        source: byte array
        lineDelimiterBytes: byte array
        acc: MemoryStream
        index: int
        outputColumnIndex: int
    }

let rec private encodeUnfold(state: EncodingState) =
    let charNeedsEncoding c =
        ((c < 33uy) || (c > 126uy) || (c = (byte)'=')) && (c <> 9uy) && (c <> 32uy)

    if state.index >= state.source.Length then
        Encoding.ASCII.GetString(state.acc.ToArray())
    else
        match (state.outputColumnIndex, state.source.[state.index]) with
        | (ci, c) when not(charNeedsEncoding(c)) && (ci < 76 - 1) ->
            // Character does not need encoding, and we don't need to line wrap.
            state.acc.WriteByte((byte)c)
            encodeUnfold { state with outputColumnIndex = state.outputColumnIndex + 1; index = state.index + 1 }
        | (_, c) when not(charNeedsEncoding(c)) ->
            // Character does not need encoding, but we need to line wrap.
            state.acc.WriteByte((byte)'=')
            state.acc.Write(state.lineDelimiterBytes, 0, state.lineDelimiterBytes.Length)
            state.acc.WriteByte((byte)c)
            encodeUnfold { state with outputColumnIndex = 1; index = state.index + 1 }
        | (ci, c) when (ci < (76 - 3 - 1)) ->
            // Character needs encoding, and we have enough room for it on this line.
            state.acc.WriteByte((byte)'=')
            state.acc.Write(Encoding.ASCII.GetBytes(c.ToString("X2")), 0, 2)
            encodeUnfold { state with outputColumnIndex = state.outputColumnIndex + 3; index = state.index + 1 }
        | (_, c) ->
            // Character needs encoding, but we need to line wrap.
            state.acc.WriteByte((byte)'=')
            state.acc.Write(state.lineDelimiterBytes, 0, state.lineDelimiterBytes.Length)
            state.acc.WriteByte((byte)'=')
            state.acc.Write(Encoding.ASCII.GetBytes(c.ToString("X2")), 0, 2)
            encodeUnfold { state with outputColumnIndex = 3; index = state.index + 1 }

//---------------------------------------------------------------------
// Public API

let Decode(input: string): byte array =
    use ms = new MemoryStream()
    // Compact into a single line.
    let normalized =
        input
          .Replace("=" + Environment.NewLine, "")
          .Replace(Environment.NewLine, "")

    decodeInternal {
        DecodingState.cs = normalized.ToCharArray()
        acc = ms
        index = 0
        decoderState = DecoderState.Normal  
    }

let DecodeString(input: string, encoding: Encoding): string =
    encoding.GetString(Decode(input))

let Encode(input: byte array): string =
    use ms = new MemoryStream()
    let state =
        {
            EncodingState.source = input
            lineDelimiterBytes = Encoding.ASCII.GetBytes(Environment.NewLine)
            acc = ms
            index = 0
            outputColumnIndex = 0
        }
    encodeUnfold state

let EncodeString(input: string, encoding: Encoding): string =
    Encode(encoding.GetBytes(input))
