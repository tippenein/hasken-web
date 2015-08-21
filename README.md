Hasken Web
-----

The api for storing hasken documents

To build and run the server:
```
stack build && stack exec hasken-web
```

Doc's have only 2 attributes:

```
url :: String
tags :: [String]
```

`get /docs/<tag>` - searches for docs with `<tag>`
