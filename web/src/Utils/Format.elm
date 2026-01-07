module Utils.Format exposing (formatScientific)

{-| Number formatting utilities for displaying scientific data

@docs formatScientific

-}


{-| Format a float using smart formatting:
- Regular notation for readable numbers (0.01 to 999999)
- Scientific notation for very small or very large numbers

Examples:
    formatScientific 100 == "100"
    formatScientific 4110 == "4110"
    formatScientific 1.5 == "1.5"
    formatScientific 0.25 == "0.25"
    formatScientific 0.00001 == "1.00e-5"
    formatScientific 1234567890 == "1.23e+9"
    formatScientific 0.0 == "0"

-}
formatScientific : Float -> String
formatScientific value =
    if value == 0.0 then
        "0"

    else if isNaN value then
        "NaN"

    else if isInfinite value then
        if value > 0 then
            "+∞"

        else
            "-∞"

    else
        let
            absValue =
                abs value
        in
        if absValue >= 0.01 && absValue < 1000000 then
            formatRegular value

        else
            formatScientificNotation value


{-| Format a number in regular notation with appropriate decimal places
-}
formatRegular : Float -> String
formatRegular value =
    let
        absValue =
            abs value

        -- Round to a reasonable number of decimal places based on magnitude
        rounded =
            if absValue >= 100 then
                -- Large numbers: no decimals (100, 4110)
                toFloat (round value)

            else if absValue >= 1 then
                -- Medium numbers: 1-2 decimals (1.5, 25.75)
                toFloat (round (value * 100)) / 100

            else
                -- Small numbers: more precision (0.25, 0.001)
                toFloat (round (value * 10000)) / 10000

        -- Convert to string and clean up trailing zeros
        str =
            String.fromFloat rounded
    in
    cleanupTrailingZeros str


{-| Remove unnecessary trailing zeros after decimal point
    "1.50" -> "1.5"
    "100.00" -> "100"
    "0.250" -> "0.25"
-}
cleanupTrailingZeros : String -> String
cleanupTrailingZeros str =
    case String.split "." str of
        [ integer ] ->
            integer

        [ integer, decimal ] ->
            let
                trimmed =
                    String.foldr
                        (\c acc ->
                            if acc == "" && c == '0' then
                                ""

                            else
                                String.cons c acc
                        )
                        ""
                        decimal
            in
            if trimmed == "" then
                integer

            else
                integer ++ "." ++ trimmed

        _ ->
            str


{-| Format a number in scientific notation with 2 decimal places
-}
formatScientificNotation : Float -> String
formatScientificNotation value =
    let
        absValue =
            abs value

        exponent =
            floor (logBase 10 absValue)

        mantissa =
            value / (10 ^ toFloat exponent)

        roundedMantissa =
            toFloat (round (mantissa * 100)) / 100

        mantissaStr =
            String.fromFloat roundedMantissa
                |> ensureTwoDecimals

        exponentStr =
            if exponent >= 0 then
                "+" ++ String.fromInt exponent

            else
                String.fromInt exponent
    in
    mantissaStr ++ "e" ++ exponentStr


{-| Ensure a float string has exactly 2 decimal places
-}
ensureTwoDecimals : String -> String
ensureTwoDecimals str =
    case String.split "." str of
        [ integer ] ->
            integer ++ ".00"

        [ integer, decimal ] ->
            let
                paddedDecimal =
                    if String.length decimal == 0 then
                        "00"

                    else if String.length decimal == 1 then
                        decimal ++ "0"

                    else
                        String.left 2 decimal
            in
            integer ++ "." ++ paddedDecimal

        _ ->
            str
