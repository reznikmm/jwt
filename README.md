# JWT

>  JSON Web Token (JWT) implementation in Ada

JSON Web Token (JWT) is a compact, URL-safe means of representing
claims to be transferred between two parties.

This library provides types and subprograms for creation and
validation of JWT. Currently it implements HS256 and RS256
algorithms.

## Install

Run
```
make all install PREFIX=/path/to/install
```

### Dependencies
It depends on [Matreshka](https://forge.ada-ru.org/matreshka).

## Usage

To use as a library, add `with "jwt";` to your project file.

### HS256
The HS256 token requires a secret shared between issuer and validator.
Just create a random Stream_Element_Array and pass it as the `Secret`
parameter into both `Create` and `Validate_Compact_Serialization`
subprograms.

### Create a HS256 signature

```ada
declare
   function "+"
     (Item : Wide_Wide_String) return League.Strings.Universal_String
      renames League.Strings.To_Universal_String;

   Header : JWS.JOSE_Header;
   Secret : Ada.Streams.Stream_Element_Array := (6, 5, 4, 3, 2, 1);
   Signature : JWS.JSON_Web_Signature;
begin
   Header.Set_Algorithm (+"HS256");
   Signature.Create
     (Header  => Header,
      Payload => (1, 2, 3, 4, 5, 6),
      Secret  => Secret);
   Ada.Wide_Wide_Text_IO.Put_Line
     (Signature.Compact_Serialization.To_Wide_Wide_String);
end;
```

### Validate HS256 signature

After validating a compact serialization of a JWN token
you can access `Payload` and `Header` properties of the
`JSON_Web_Signature` object.

```ada
declare
   Token : Wide_Wide_String := "eyJhbGciOiJIUzI1NiJ9.e30" &
     ".c4iSawYTPKKRLOS6VSflG07uxBue3wvnmuAy6j974-E";
   Secret : Ada.Streams.Stream_Element_Array := (6, 5, 4, 3, 2, 1);
   Signature : JWS.JSON_Web_Signature;
   Ok        : Boolean;
begin
   Signature.Validate_Compact_Serialization
     (Value  => +Token,
      Secret => Secret,
      Valid  => Ok);
   if Ok then
      Use_Data (Signature.Payload);
   end if;
end;
```
### RS256

`RS256` is an optional algorithm. To use enable it put next with clause
somewhere in your source code:

```ada
with JWS.RS256;  --  Enable RS256 algorithm
pragma Unreferenced (JWS.RS256);
```

The `RS256` token requires private and public key pair. One can generate
them with `openssl`. Currently the library accepts binary representation
of the keys. To create a private key run:

```
openssl req -x509 -sha256 -nodes -days 365 -newkey rsa:2048 \
 -keyout ./privateKey.key -out ./certificate.crt
grep -v ^- privateKey.key| base64 -d > priv.dat
```

Then put `priv.dat` content into the `Secret` parameter of `Create`.

### Create a RS256 signature

```ada
declare
   function "+"
     (Item : Wide_Wide_String) return League.Strings.Universal_String
      renames League.Strings.To_Universal_String;

   Header    : JWS.JOSE_Header;
   Signature : JWS.JSON_Web_Signature;
begin
   Header.Set_Algorithm (+"RS256");
   Signature.Create
     (Header  => Header,
      Payload => (1, 2, 3, 4, 5, 6),
      Secret  => Secret);  --  Put content of priv.dat here
   Ada.Wide_Wide_Text_IO.Put_Line
     (Signature.Compact_Serialization.To_Wide_Wide_String);
end;
```

### Validate HS256 signature

To get public key from your private key run:
```
openssl rsa  -pubout -in ./privateKey.key > ./publicKey.key
grep -v ^- privateKey.key| base64 -d > pub.dat
```

To validate a JWT use content of the `pub.dat` as the `Secret`
of `Validate_Compact_Serialization` procedure.

```ada
   Signature.Validate_Compact_Serialization
     (Value  => Token,
      Secret => Secret,  --  Put content of the pub.dat here
      Valid  => Ok);
```

## Maintainer

[@MaximReznik](https://github.com/reznikmm).

## Contribute

Feel free to join!
[Open an issue](https://github.com/reznikmm/jwt/issues/new) or submit PRs.

## License

[MIT](LICENSE) © Maxim Reznik
