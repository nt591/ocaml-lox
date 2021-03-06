open OUnit2
open Scanner
open Token

let testContext1 : Scanner.scanner_context = {
  start = 0;
  current = 0;
  line = 1;
  current_character = None;
  source = "()";
  tokens = [];
  had_error = false;
}

let testContext2 : Scanner.scanner_context = {
  start = 0;
  current = 0;
  line = 1;
  current_character = None;
  source = "";
  tokens = [];
  had_error = false;
}

let testContext3 : Scanner.scanner_context = {
  start = 0;
  current = 1;
  line = 1;
  current_character = None;
  source = "( )";
  tokens = [
    Token.TokenRecord {
      literal = Some Token.IDENTIFIER;
      lexeme = "(";
      line = 1;
      token_type = Token.LEFT_PAREN;
    }
  ];
  had_error = false;
}

let testContext4 : Scanner.scanner_context = {
  start = 0;
  current = 0;
  line = 1;
  current_character = None;
  source = "(";
  tokens = [];
  had_error = false;
}

let testContext5 : Scanner.scanner_context = {
  start = 0;
  current = 0;
  line = 1;
  current_character = None;
  source = ">=";
  tokens = [];
  had_error = false;
}

let test_context_digits : Scanner.scanner_context = {
  start = 0;
  current = 0;
  line = 1;
  current_character = None;
  source = "123 done";
  tokens = [];
  had_error = false;
}

let test_context_float : Scanner.scanner_context = {
  start = 0;
  current = 0;
  line = 1;
  current_character = None;
  source = ".456 done";
  tokens = [];
  had_error = false;
}

let test_context_float2 : Scanner.scanner_context = {
  start = 0;
  current = 0;
  line = 1;
  current_character = None;
  source = "123.456 done";
  tokens = [];
  had_error = false;
}

let tests = "test scanner" >::: [
  "advance - it returns context with current + 1" >:: (fun _ ->
    let expected = 1 in
    let actual = (Scanner.advance testContext1).current
    in assert_equal expected actual
  );

  "advance - it returns context with current_character of the current place" >:: (fun _ ->
    let expected = Some '(' in
    let actual = (Scanner.advance testContext1).current_character
    in assert_equal expected actual
  );

  "advance - it returns context with current_character of None with an empty source" >:: (fun _ ->
    let expected = None in
    let actual = (Scanner.advance testContext2).current_character
    in assert_equal expected actual
  );

  "add_token - it adds a token to context based on a passed in string" >:: (fun _ ->
    let c = Scanner.advance testContext1 in
    let actual = (Scanner.add_token Token.LEFT_PAREN (Some Token.IDENTIFIER) c).tokens in
    let expected = [
      Token.TokenRecord {
        literal = Some Token.IDENTIFIER;
        lexeme = "(";
        line = 1;
        token_type = Token.LEFT_PAREN;
      }
    ] in
    assert_equal expected actual
  );

  "make_token - it adds a new token to context" >:: (fun _ ->
    let expected = [
      Token.TokenRecord {
        literal = Some Token.IDENTIFIER;
        line = 1;
        lexeme = "(";
        token_type = Token.LEFT_PAREN
      }
    ] in
    let actual = (Scanner.make_token Token.LEFT_PAREN "(" (Some Token.IDENTIFIER) testContext1) in
    assert_equal expected actual.tokens
    );

  "is_at_end - returns false when current is less than source length" >:: (fun _ ->
    assert_equal false (Scanner.is_at_end testContext1)
  );

  "is_at_end - returns true when current is equal to source length" >:: (fun _ ->
    let ctx = {
      testContext1 with current = 2
    } in
    assert_equal true (Scanner.is_at_end ctx)
  );

  "scan_token - it gets the next token and adds to the list" >:: (fun _ ->
    let expected = [
      Token.TokenRecord {
        literal = Some Token.IDENTIFIER;
        line = 1;
        lexeme = "(";
        token_type = Token.LEFT_PAREN
      }
    ] in
    let actual = (Scanner.scan_token testContext1).tokens in
    assert_equal expected actual
  );

  "scan_token - it returns back the same list of ctx when there's no token to add" >:: (fun _ ->
    let actual = (Scanner.scan_token testContext3).tokens in
    let expected = testContext3.tokens in
    assert_equal expected actual
  );

  "scan_token - it adds a token to ctx when there's a new token to add" >:: (fun _ ->
    let actual = (Scanner.scan_token testContext1).tokens in
    let expected = [
      Token.TokenRecord {
        literal = Some Token.IDENTIFIER;
        line = 1;
        lexeme = "(";
        token_type = Token.LEFT_PAREN
      }
    ] in
    assert_equal expected actual
  );

  "scan_token - it works for two-letter identifiers" >:: (fun _ ->
    let actual = (Scanner.scan_token testContext5).tokens in
    let expected = [
      Token.TokenRecord {
        literal = Some Token.IDENTIFIER;
        line = 1;
        lexeme = ">=";
        token_type = Token.GREATER_EQUAL
      }
    ] in
    assert_equal expected actual
  );

  "_scan_tokens - it adds EOF for a given context with an empty source into the tokens list" >:: (fun _ ->
    let expected = [
      Token.TokenRecord {
        literal = Some Token.IDENTIFIER;
        line = 1;
        lexeme = "";
        token_type = Token.EOF
      };

    ] in
    let actual = (Scanner._scan_tokens testContext2).tokens in
    assert_equal expected actual
  );

  "_scan_tokens - it adds all tokens for a given context with into the tokens list" >:: (fun _ ->
    let expected = [
      Token.TokenRecord {
        literal = Some Token.IDENTIFIER;
        line = 1;
        lexeme = "(";
        token_type = Token.LEFT_PAREN
      };
      Token.TokenRecord {
        literal = Some Token.IDENTIFIER;
        line = 1;
        lexeme = ")";
        token_type = Token.RIGHT_PAREN
      };
      Token.TokenRecord {
        literal = Some Token.IDENTIFIER;
        line = 1;
        lexeme = ")";
        token_type = Token.EOF
      };
    ] in
    let actual = (Scanner._scan_tokens testContext1).tokens in
    assert_equal expected actual
  );

  "find_digits - it advances until the digits are done" >:: (fun _ ->
    let expected : Scanner.scanner_context = {
      start = 0;
      current = 3;
      line = 1;
      current_character = Some '3';
      source = "123 done";
      tokens = [];
      had_error = false;
    } in
    assert_equal (Scanner.find_digits test_context_digits) expected
  );

  "is_digit - returns true for some digit char" >:: (fun _ ->
    assert_equal (Scanner.is_digit (Some '4')) true
  );

  "is_digit - returns false for some char" >:: (fun _ ->
    assert_equal (Scanner.is_digit (Some 'c')) false
  );

  "is_digit - returns false for none" >:: (fun _ ->
    assert_equal (Scanner.is_digit None) false
  );

  "find_fractional_digits - it consumes all numbers after a period" >:: (fun _ ->
    let expected : Scanner.scanner_context = {
      start = 0;
      current = 4;
      line = 1;
      current_character = Some '6';
      source = ".456 done";
      tokens = [];
      had_error = false;
    } in
    assert_equal (Scanner.find_fractional_digits test_context_float) expected
  );

  "add_number - it adds a token after consuming the entire float" >:: (fun _ ->
    let expected : Scanner.scanner_context = {
      start = 0;
      current = 7;
      line = 1;
      current_character = Some '6';
      source = "123.456 done";
      tokens = [
        Token.TokenRecord {
          literal = Some (Token.NUMBER_LITERAL 123.456);
          line = 1;
          lexeme = "123.456";
          token_type = Token.NUMBER
        }
      ];
      had_error = false;
    } in
    assert_equal (Scanner.add_number test_context_float2) expected
  );
]

let _ = run_test_tt_main tests
