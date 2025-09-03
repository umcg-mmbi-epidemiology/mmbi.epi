# ===================================================================== #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at University Medical Center Groningen (UMCG),             #
#  department of Medical Microbiology & Infection Prevention (MMBI),    #
#  unit Epidemiology & Data Science:                                    #
#  https://github.com/umcg-mmbi-epidemiology                            #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' Serialise, Compress, Encrypt, and Transfer Data
#'
#' Securely serialise, compress, encrypt, and transfer any R object with full attribute preservation and cross-platform JSON compatibility.
#' @param object Any object of any size, preferably a data set
#' @param compress,decompress Should the serialised object be compressed/decompressed? At least allowed: `"gzip"` (or `TRUE`), `"bzip2"`, `"xz"`, see [base::memCompress()]. Use `FALSE` to not compress/decompress.
#' @param encrypt,decrypt Should the serialised object be encrypted/decrypted? This applies AES-GCM via [openssl::aes_gcm_encrypt()], providing authenticated encryption. This guarantees both confidentiality and integrity: the file cannot be read without the correct `key`, and any tampering will be detected automatically during decryption. The initialization vector (iv) will be a length-12 random [raw] vector.
#' @param key A character to be used as the encryption key. Internally, this is converted using [openssl::sha256()] to ensure a raw high-entropy key of length `32`, suitable for AES-GCM encryption. The default is the [system environment variable][Sys.getenv()]: `mmbi_epi_encryption_key`.
#' @param as_character A logical to indicate whether output should be converted to a [character] string. Note that these have a limit of 2,147,483,647 characters (= \eqn{2^{31} - 1} bytes = ~2 GB in object size), so a [raw] vector should be used for very large inputs (i.e., `as_character = FALSE`).
#' @param x A [raw] or [character] vector
#' @param url A character string specifying the target URL for the HTTP POST request. Must include the full scheme (e.g., `"https://"` or `"http://"`), hostname, and path.
#' @param authorization_header A character string specifying the value of the `Authorization` header to include in the POST request, e.g. `"Bearer <token>"`. Use `NULL` to omit the header.
#' @details
#' ## Serialisation
#' [serialise()] converts an arbitrary R object into a transportable
#' format by wrapping it with metadata, including:
#' * Object-level attributes (via `attributes()`),
#' * For data frames: per-column attributes, including class (e.g., `factor`,
#'   `Date`, `POSIXct`), levels, and time zone information.
#'
#' The wrapped structure is then converted to JSON using [jsonlite::toJSON()],
#' with consistent handling of `NULL`s, `NA`s, and timestamps. This structure
#' allows accurate reconstruction of the original object, including attributes,
#' when passed through [deserialise()].
#'
#' The resulting JSON representation is portable and can be decoded in non-R
#' environments such as Python. This method avoids using base R `serialize()`,
#' which output is R-specific and unreadable elsewhere.
#'
#' ## Compression
#' If `compress = TRUE`, [serialise()] uses gzip compression
#' (`memCompress(type = "gzip")`) by default. Other algorithms ("bzip2",
#' "xz") are supported. Compression reduces payload size but requires the same
#' algorithm to be used for decompression. In [deserialise()] and
#' [read_json_body()] the corresponding [memDecompress()] step is
#' applied when `decompress = TRUE`.
#'
#' ## Encryption (AES-GCM)
#' [encrypt()] applies AES in Galois/Counter Mode (GCM) via
#' [openssl::aes_gcm_encrypt()]. AES-GCM provides authenticated encryption:
#' it guarantees _confidentiality_ (content is unreadable without the key)
#' and _integrity_ (any bit-level modification is detected during
#' decryption). A fresh 12-byte initialisation vector (IV) is generated for each
#' encryption (`rand_bytes(12)`), which is required for security. Because
#' the IV is random/unique per call, the ciphertext differs across runs even for
#' identical inputs; this is expected and desirable. The IV itself is not
#' secret and is packaged alongside the ciphertext so decryption can succeed.
#'
#' ## Transport
#' [post_data()] sends the JSON body with [httr::POST()] using
#' `encode = "json"` and sets the HTTP `Authorization` header if you
#' pass one (for example a bearer token). The receiving service can be any
#' stack that can: (1) parse JSON, (2) base64-decode
#' fields, (3) perform AES-GCM decryption with the same key and IV, (4) gzip
#' decompress, and (5) deserialise JSON strings.
#'
#' ## Read in R
#' To decrypt, decompress, and process in R at the receiving side, do:
#'
#' ```r
#' library(mmbi.epi)
#'
#' # assuming `json_payload` is received
#' read_json_body(decompress = TRUE, decrypt = TRUE, key = "my-key")
#' ```
#'
#' ## Read in Python
#' To decrypt, decompress, and process in Python at the receiving side, do:
#'
#' ```python
#' import json, base64, gzip
#' import pandas as pd
#' from Crypto.Cipher import AES
#' from Crypto.Hash import SHA256
#'
#' # assuming `json_payload` is received
#' payload = json.loads(json_payload)
#' ct = base64.b64decode(payload["data"])
#' iv = base64.b64decode(payload["iv"])
#'
#' # key derivation (same as openssl::sha256 in R)
#' key = SHA256.new(b"my-key").digest()
#'
#' # decrypt (AES-GCM)
#' cipher = AES.new(key, AES.MODE_GCM, nonce=iv)
#' decrypted = cipher.decrypt(ct)
#'
#' # decompress and parse
#' decompressed = gzip.decompress(decrypted)
#' df = pd.read_json(decompressed.decode("utf-8"))
#' ```
#' @importFrom jsonlite toJSON
#' @name serialise-encrypt-post
#' @rdname serialise-encrypt-post
#' @export
#' @examples
#' # SERIALISATION AND ENCRYPTION -----------------------------------------
#'
#' # in essence:
#' iris2 <- iris |> serialise() |> deserialise()
#' identical(iris, iris2)
#' # and:
#' iris3 <- iris |> serialise() |> encrypt() |> decrypt() |> deserialise()
#' identical(iris, iris3)
#'
#' # a serialised object is a representation for any type of data
#' serialise(iris)[1:25]
#'
#' # and can be converted back at any time
#' iris |> serialise() |> deserialise() |> head()
serialise <- function(object, compress = TRUE, as_character = TRUE) {
  # Extract class and levels info if object is a data.frame
  meta <- list()
  meta$obj_attributes <- attributes(object)
  if (is.data.frame(object)) {
    meta$col_attributes <- lapply(object, attributes)
  }

  # Wrap object with metadata
  wrapped <- list(
    object = object,
    meta = meta
  )

  out <- wrapped |>
    toJSON(dataframe = ifelse(is.data.frame(object), "rows", "columns"),
           auto_unbox = TRUE,
           null = "null",
           na = "null",
           POSIXt = "ISO8601",
           digits = NA,
           force = TRUE) |>
    charToRaw()
  if (isTRUE(compress)) {
    compress <- "gzip"
  }
  if (!isFALSE(compress)) {
    out <- memCompress(out, type = compress)
  }
  if (isTRUE(as_character)) {
    out <- as.character(out)
  }
  out
}

#' @importFrom jsonlite fromJSON
#' @rdname serialise-encrypt-post
#' @export
deserialise <- function(object, decompress = TRUE) {
  if (isTRUE(decompress)) {
    decompress <- "gzip"
  }
  if (is.character(object)) {
    object <- object |> strtoi(base = 16L) |> as.raw()
  }
  if (!isFALSE(decompress)) {
    tryCatch(object <- memDecompress(object, type = decompress),
             error = function(e) stop("Problem during decompressing, do you need `decompress = FALSE` or is an encryption key invalid?\n-> ", conditionMessage(e), call. = FALSE))
  }
  if (is.raw(object)) {
    object <- rawToChar(object)
  }
  parsed <- fromJSON(object, simplifyDataFrame = TRUE)
  x <- parsed$object
  meta <- parsed$meta

  # restore data structure (classes, attributes, etc)
  if (!is.null(meta$obj_attributes)) {
    if ("factor" %in% meta$obj_attributes$class) {
      x <- factor(x, levels = meta$obj_attributes$levels)
    }
    if ("Date" %in% meta$obj_attributes$class) {
      x <- as.Date(x)
    }
    if ("POSIXct" %in% meta$obj_attributes$class) {
      if (!is.null(meta$obj_attributes$tzone)) {
        x <- as.POSIXct(gsub("T", " ", x), tz = meta$obj_attributes$tzone)
      } else {
        x <- as.POSIXct(gsub("T", " ", x))
      }
    }
    attributes(x) <- meta$obj_attributes
  }
  if (is.data.frame(x) && !is.null(meta$col_attributes)) {
    for (nm in names(meta$col_attributes)) {
      if (is.null(x[[nm]])) next
      if ("factor" %in% meta$col_attributes[[nm]]$class) {
        x[[nm]] <- factor(x[[nm]], levels = meta$col_attributes[[nm]]$levels)
      }
      if ("Date" %in% meta$col_attributes[[nm]]$class) {
        x[[nm]] <- as.Date(x[[nm]])
      }
      if ("POSIXct" %in% meta$col_attributes[[nm]]$class) {
        if (!is.null(meta$col_attributes[[nm]]$tzone)) {
          x[[nm]] <- as.POSIXct(gsub("T", " ", x[[nm]]), tz = meta$col_attributes[[nm]]$tzone)
        } else {
          x[[nm]] <- as.POSIXct(gsub("T", " ", x[[nm]]))
        }
      }
      attributes(x[[nm]]) <- meta$col_attributes[[nm]]
    }
  }

  x
}

#' @importFrom openssl sha256 aes_gcm_encrypt rand_bytes
#' @rdname serialise-encrypt-post
#' @export
encrypt <- function(x, key = Sys.getenv("mmbi_epi_encryption_key"), as_character = TRUE) {
  if (is.character(x)) {
    x <- x |> strtoi(base = 16L) |> as.raw()
  } else if (!is.raw(x)) {
    stop("The input for `encrypt()` must be of data type 'raw' or 'character', not ", paste0("'", class(x), "'", collapse = "/"), ". Use `serialise() first.")
  }
  key <- sha256(charToRaw(key))
  x <- aes_gcm_encrypt(x, key = key, iv = rand_bytes(12))
  if (isTRUE(as_character)) {
    iv <- attr(x, "iv", exact = TRUE)
    if (isTRUE(as_character)) {
      iv <- as.character(iv)
    }
    x <- as.character(x)
    attr(x, "iv") <- iv
  }
  x
}

#' @importFrom openssl sha256 aes_gcm_decrypt
#' @rdname serialise-encrypt-post
#' @export
decrypt <- function(x, key = Sys.getenv("mmbi_epi_encryption_key"), as_character = TRUE) {
  iv <- attr(x, "iv", exact = TRUE)
  if (is.character(x)) {
    x <- x |> strtoi(base = 16L) |> as.raw()
  }
  if (is.character(iv)) {
    iv <- iv |> strtoi(base = 16L) |> as.raw()
  }
  key <- sha256(charToRaw(key))
  x <- aes_gcm_decrypt(x, key = key, iv = iv)
  if (isTRUE(as_character)) {
    x <- as.character(x)
  }
  x
}

#' @importFrom httr POST add_headers stop_for_status
#' @rdname serialise-encrypt-post
#' @export
#' @examples
#'
#'
#' # POSTING DATA ---------------------------------------------------------
#'
#' # post_data() sends data using POST, after serialising (and encrypting)
#'
#' \dontrun{
#'
#' post_data(iris,
#'           url = "https://some-server:8000/post",
#'           compress = TRUE,
#'           encrypt = TRUE)
#' }
#'
#' # use create_json_body() to make an encrypted JSON of an object, and
#' # read_json_body() to read it back
#' iris_json <- iris |> create_json_body(compress = TRUE, encrypt = TRUE)
#' # (can be sent securely to a server)
#' # then:
#' iris4 <- iris_json |> read_json_body(decompress = TRUE, decrypt = TRUE)
#' identical(iris, iris4)
#'
#' # equivalent using curl:
#' # curl -X POST https://some-server:8000/post
#' #      -H "Content-Type: application/json"
#' #      -d '...'
#'
#' # replace the "..." with the outcome of create_json_body():
#' iris_json
post_data <- function(object,
                      url,
                      authorization_header = NULL,
                      compress = TRUE,
                      encrypt = TRUE,
                      key = Sys.getenv("mmbi_epi_encryption_key")) {

  json_body <- create_json_body(object = object,
                                compress = compress,
                                encrypt = encrypt)

  resp <- POST(url = url,
               body = json_body,
               encode = "json",
               config = add_headers(Authorization = authorization_header))
  stop_for_status(resp)
  invisible(resp)
}

#' @importFrom jsonlite toJSON
#' @rdname serialise-encrypt-post
#' @export
create_json_body <- function(object,
                             compress = TRUE,
                             encrypt = TRUE,
                             key = Sys.getenv("mmbi_epi_encryption_key")) {

  payload <- serialise(object, compress = compress, as_character = FALSE)

  if (isTRUE(encrypt)) {
    payload <- encrypt(payload, key = key, as_character = FALSE)
    iv <- attr(payload, "iv", exact = TRUE)
  } else {
    iv <- NULL
  }

  if (is.null(iv)) {
    toJSON(x = list(data = payload), auto_unbox = TRUE)
  } else {
    toJSON(x = list(data = payload, iv = iv), auto_unbox = TRUE)
  }
}

#' @importFrom jsonlite fromJSON base64_dec
#' @rdname serialise-encrypt-post
#' @export
read_json_body <- function(object,
                           decompress = TRUE,
                           decrypt = NULL,
                           key = Sys.getenv("mmbi_epi_encryption_key")) {
  if (is.character(object)) {
    object <- gsub("\n", "", object, fixed = TRUE)
  }
  payload <- fromJSON(object)
  out <- base64_dec(payload$data)
  if (!is.null(payload$iv)) {
    # initialization vector
    attr(out, "iv") <- base64_dec(payload$iv)
  }

  if (isTRUE(decrypt) || (is.null(decrypt) && !is.null(payload$iv))) {
    out <- decrypt(out, key = key, as_character = FALSE)
  }
  deserialise(out, decompress = decompress)
}
