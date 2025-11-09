module Utils.Format exposing (formatScientific)

{-| Number formatting utilities for displaying scientific data

@docs formatScientific

-}


{-| Format a float in scientific notation with 2 decimal places in the mantissa.

Examples:
    formatScientific 0.0000012302591461717608 == "1.23e-6"
    formatScientific 1.5047776003155534e-14 == "1.50e-14"
    formatScientific 1234.5678 == "1.23e+3"
    formatScientific 0.0 == "0.00"

-}
formatScientific : Float -> String
formatScientific value =
    if value == 0.0 then
        "0.00"

    else if isNaN value then
        "NaN"

    else if isInfinite value then
        if value > 0 then
            "+∞"

        else
            "-∞"

    else
        let
            -- Get the absolute value for calculation
            absValue =
                abs value

            -- Calculate the exponent (power of 10)
            exponent =
                floor (logBase 10 absValue)

            -- Calculate the mantissa (value / 10^exponent)
            mantissa =
                value / (10 ^ toFloat exponent)

            -- Round mantissa to 2 decimal places
            roundedMantissa =
                (toFloat (round (mantissa * 100))) / 100

            -- Format the mantissa with exactly 2 decimal places
            mantissaStr =
                String.fromFloat roundedMantissa
                    |> ensureTwoDecimals

            -- Format the exponent with sign
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
            -- No decimal point, add ".00"
            integer ++ ".00"

        [ integer, decimal ] ->
            -- Has decimal point, ensure 2 digits
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
            -- Shouldn't happen, but return as-is
            str
