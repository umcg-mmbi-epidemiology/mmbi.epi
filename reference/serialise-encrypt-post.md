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
#> {"data":"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","iv":"5w98z5juJhJfNQ+1"} 
```
