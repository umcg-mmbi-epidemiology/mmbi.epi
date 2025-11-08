# Serialise, Compress, Encrypt, and Transfer Data

Securely serialise, compress, encrypt, and transfer any R object with
full attribute preservation and cross-platform JSON compatibility.

## Usage

``` r
serialise(object, compress = TRUE, as_character = TRUE)

deserialise(object, decompress = TRUE)

encrypt(x, key = Sys.getenv("mmbi_epi_encryption_key"), as_character = TRUE)

decrypt(x, key = Sys.getenv("mmbi_epi_encryption_key"), as_character = TRUE)

post_data(
  object,
  url,
  authorization_header = NULL,
  compress = TRUE,
  encrypt = TRUE,
  key = Sys.getenv("mmbi_epi_encryption_key")
)

create_json_body(
  object,
  compress = TRUE,
  encrypt = TRUE,
  key = Sys.getenv("mmbi_epi_encryption_key")
)

read_json_body(
  object,
  decompress = TRUE,
  decrypt = NULL,
  key = Sys.getenv("mmbi_epi_encryption_key")
)
```

## Arguments

- object:

  Any object of any size, preferably a data set

- compress, decompress:

  Should the serialised object be compressed/decompressed? At least
  allowed: `"gzip"` (or `TRUE`), `"bzip2"`, `"xz"`, see
  [`base::memCompress()`](https://rdrr.io/r/base/memCompress.html). Use
  `FALSE` to not compress/decompress.

- as_character:

  A logical to indicate whether output should be converted to a
  [character](https://rdrr.io/r/base/character.html) string. Note that
  these have a limit of 2,147,483,647 characters (= \\2^{31} - 1\\ bytes
  = ~2 GB in object size), so a [raw](https://rdrr.io/r/base/raw.html)
  vector should be used for very large inputs (i.e.,
  `as_character = FALSE`).

- x:

  A [raw](https://rdrr.io/r/base/raw.html) or
  [character](https://rdrr.io/r/base/character.html) vector

- key:

  A character to be used as the encryption key. Internally, this is
  converted using
  [`openssl::sha256()`](https://jeroen.r-universe.dev/openssl/reference/hash.html)
  to ensure a raw high-entropy key of length `32`, suitable for AES-GCM
  encryption. The default is the [system environment
  variable](https://rdrr.io/r/base/Sys.getenv.html):
  `mmbi_epi_encryption_key`.

- url:

  A character string specifying the target URL for the HTTP POST
  request. Must include the full scheme (e.g., `"https://"` or
  `"http://"`), hostname, and path.

- authorization_header:

  A character string specifying the value of the `Authorization` header
  to include in the POST request, e.g. `"Bearer <token>"`. Use `NULL` to
  omit the header.

- encrypt, decrypt:

  Should the serialised object be encrypted/decrypted? This applies
  AES-GCM via
  [`openssl::aes_gcm_encrypt()`](https://jeroen.r-universe.dev/openssl/reference/aes_cbc.html),
  providing authenticated encryption. This guarantees both
  confidentiality and integrity: the file cannot be read without the
  correct `key`, and any tampering will be detected automatically during
  decryption. The initialization vector (iv) will be a length-12 random
  [raw](https://rdrr.io/r/base/raw.html) vector.

## Details

### Serialisation

`serialise()` converts an arbitrary R object into a transportable format
by wrapping it with metadata, including:

- Object-level attributes (via
  [`attributes()`](https://rdrr.io/r/base/attributes.html)),

- For data frames: per-column attributes, including class (e.g.,
  `factor`, `Date`, `POSIXct`), levels, and time zone information.

The wrapped structure is then converted to JSON using
[`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html),
with consistent handling of `NULL`s, `NA`s, and timestamps. This
structure allows accurate reconstruction of the original object,
including attributes, when passed through `deserialise()`.

The resulting JSON representation is portable and can be decoded in
non-R environments such as Python. This method avoids using base R
[`serialize()`](https://rdrr.io/r/base/serialize.html), which output is
R-specific and unreadable elsewhere.

### Compression

If `compress = TRUE`, `serialise()` uses gzip compression
(`memCompress(type = "gzip")`) by default. Other algorithms ("bzip2",
"xz") are supported. Compression reduces payload size but requires the
same algorithm to be used for decompression. In `deserialise()` and
`read_json_body()` the corresponding
[`memDecompress()`](https://rdrr.io/r/base/memCompress.html) step is
applied when `decompress = TRUE`.

### Encryption (AES-GCM)

`encrypt()` applies AES in Galois/Counter Mode (GCM) via
[`openssl::aes_gcm_encrypt()`](https://jeroen.r-universe.dev/openssl/reference/aes_cbc.html).
AES-GCM provides authenticated encryption: it guarantees
*confidentiality* (content is unreadable without the key) and
*integrity* (any bit-level modification is detected during decryption).
A fresh 12-byte initialisation vector (IV) is generated for each
encryption (`rand_bytes(12)`), which is required for security. Because
the IV is random/unique per call, the ciphertext differs across runs
even for identical inputs; this is expected and desirable. The IV itself
is not secret and is packaged alongside the ciphertext so decryption can
succeed.

### Transport

`post_data()` sends the JSON body with
[`httr::POST()`](https://httr.r-lib.org/reference/POST.html) using
`encode = "json"` and sets the HTTP `Authorization` header if you pass
one (for example a bearer token). The receiving service can be any stack
that can: (1) parse JSON, (2) base64-decode fields, (3) perform AES-GCM
decryption with the same key and IV, (4) gzip decompress, and (5)
deserialise JSON strings.

### Read in R

To decrypt, decompress, and process in R at the receiving side, do:

    library(mmbi.epi)

    # assuming `json_payload` is received
    read_json_body(decompress = TRUE, decrypt = TRUE, key = "my-key")

### Read in Python

To decrypt, decompress, and process in Python at the receiving side, do:

    import json, base64, gzip
    import pandas as pd
    from Crypto.Cipher import AES
    from Crypto.Hash import SHA256

    # assuming `json_payload` is received
    payload = json.loads(json_payload)
    ct = base64.b64decode(payload["data"])
    iv = base64.b64decode(payload["iv"])

    # key derivation (same as openssl::sha256 in R)
    key = SHA256.new(b"my-key").digest()

    # decrypt (AES-GCM)
    cipher = AES.new(key, AES.MODE_GCM, nonce=iv)
    decrypted = cipher.decrypt(ct)

    # decompress and parse
    decompressed = gzip.decompress(decrypted)
    df = pd.read_json(decompressed.decode("utf-8"))

## Examples

``` r
# SERIALISATION AND ENCRYPTION -----------------------------------------

# in essence:
iris2 <- iris |> serialise() |> deserialise()
identical(iris, iris2)
#> [1] TRUE
# and:
iris3 <- iris |> serialise() |> encrypt() |> decrypt() |> deserialise()
identical(iris, iris3)
#> [1] TRUE

# a serialised object is a representation for any type of data
serialise(iris)[1:25]
#>  [1] "78" "9c" "b5" "5a" "4d" "8f" "5b" "37" "0c" "fc" "2f" "ef" "3c" "30" "1e"
#> [16] "45" "52" "1f" "fe" "0d" "3d" "14" "c8" "a1" "87"

# and can be converted back at any time
iris |> serialise() |> deserialise() |> head()
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 6          5.4         3.9          1.7         0.4  setosa


# POSTING DATA ---------------------------------------------------------

# post_data() sends data using POST, after serialising (and encrypting)

if (FALSE) { # \dontrun{

post_data(iris,
          url = "https://some-server:8000/post",
          compress = TRUE,
          encrypt = TRUE)
} # }

# use create_json_body() to make an encrypted JSON of an object, and
# read_json_body() to read it back
iris_json <- iris |> create_json_body(compress = TRUE, encrypt = TRUE)
# (can be sent securely to a server)
# then:
iris4 <- iris_json |> read_json_body(decompress = TRUE, decrypt = TRUE)
identical(iris, iris4)
#> [1] TRUE

# equivalent using curl:
# curl -X POST https://some-server:8000/post
#      -H "Content-Type: application/json"
#      -d '...'

# replace the "..." with the outcome of create_json_body():
iris_json
#> {"data":"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","iv":"oNCG51XdoXN2oDvs"} 
```
