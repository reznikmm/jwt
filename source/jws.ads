--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;

with League.JSON.Objects;
with League.String_Vectors;
with League.Strings;
with League.Stream_Element_Vectors;

package JWS is

   type JSON_Web_Signature is tagged private;
   --  JSON Web Signature (JWS) represents content secured with digital
   --  signatures or Message Authentication Codes (MACs) using JSON-based data
   --  structures. The JWS cryptographic mechanisms provide integrity
   --  protection for an arbitrary sequence of octets.

   type JOSE_Header;

   not overriding function Header
     (Self : JSON_Web_Signature) return JOSE_Header;

   type JOSE_Header is new League.JSON.Objects.JSON_Object with null record;
   --  JSON Object Signing and Encryption Header

   function Algorithm (Self : JOSE_Header'Class)
     return League.Strings.Universal_String;
   --  The "alg" (algorithm) Header Parameter identifies the cryptographic
   --  algorithm used to secure the JWS.

   procedure Set_Algorithm
     (Self  : in out JOSE_Header'Class;
      Value : League.Strings.Universal_String);

   function Critical (Self : JOSE_Header'Class)
     return League.String_Vectors.Universal_String_Vector;
   --  The "crit" (critical) Header Parameter indicates that extensions to this
   --  specification and/or [JWA] are being used that MUST be understood and
   --  processed.

   procedure Set_Critical
     (Self  : in out JOSE_Header'Class;
      Value : League.String_Vectors.Universal_String_Vector);

   procedure Create
     (Self    : out JSON_Web_Signature'Class;
      Header  : JOSE_Header;
      Payload : Ada.Streams.Stream_Element_Array;
      Secret  : Ada.Streams.Stream_Element_Array);

   function Compact_Serialization
     (Self : JSON_Web_Signature'Class) return League.Strings.Universal_String;
   --  A representation of the JWS as a compact, URL-safe string.

   procedure Validate_Compact_Serialization
     (Self   : out JSON_Web_Signature'Class;
      Value  : League.Strings.Universal_String;
      Secret : Ada.Streams.Stream_Element_Array;
      Valid  : out Boolean);
   --  Validate given compact serialization using Secret

   function Payload
     (Self : JSON_Web_Signature'Class) return Ada.Streams.Stream_Element_Array;
   --  Return the payload from given signature.

   function Payload_Vector
     (Self : JSON_Web_Signature'Class)
      return League.Stream_Element_Vectors.Stream_Element_Vector;
   --  Return the payload from given signature as Stream_Element vector.

   function Payload_Object
     (Self : JSON_Web_Signature'Class)
      return League.JSON.Objects.JSON_Object;
   --  Return the payload from given signature as JSON Object.

private

   type JSON_Web_Signature is tagged record
      Header  : JOSE_Header;
      Payload : League.Stream_Element_Vectors.Stream_Element_Vector;
      Secret  : League.Stream_Element_Vectors.Stream_Element_Vector;
   end record;

   function Compute_Signature
     (Self   : JOSE_Header'Class;
      Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      Secret : Ada.Streams.Stream_Element_Array)
      return League.Stream_Element_Vectors.Stream_Element_Vector;

   function Validate_Signature
     (Self   : JOSE_Header'Class;
      Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      Secret : Ada.Streams.Stream_Element_Array;
      Value  : League.Stream_Element_Vectors.Stream_Element_Vector)
        return Boolean;

   type Signature_Function is access
     function
       (Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
        Secret : Ada.Streams.Stream_Element_Array)
        return League.Stream_Element_Vectors.Stream_Element_Vector;

   type Validate_Signature_Function is access
     function
       (Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
        Secret : Ada.Streams.Stream_Element_Array;
        Value  : League.Stream_Element_Vectors.Stream_Element_Vector)
          return Boolean;

   RS256_Signature_Link : Signature_Function;
   RS256_Validation_Link : Validate_Signature_Function;

end JWS;
