Build
-----
::
    brew install openssl
    cabal sandbox init
    cabal install hsOpenSSL --extra-lib-dirs=/usr/local/lib --extra-include-dirs=/usr/local/include
    cabal install --dependencies
    cabal build

Usage
-----
::
    APNPush --help
    Usage: apnspush [-tmcks] [tokens file, message, cert file, key file, sandbox]
      -m          Message to send
      -t          File containing tokens of devices
      -s          Use sandbox APNS
      -c          Path to cert file
      -k          Path to key file
          --help  Print this help message
