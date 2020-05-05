unit CloudAPI.Enum;

interface

type
{$SCOPEDENUMS ON}
  TcaParameterType = (Cookie, GetOrPost, UrlSegment, HttpHeader, RequestBody, QueryString, QueryStringWithoutEncode);
  TcaMethod = (GET, POST, PUT, DELETE, HEAD, OPTIONS, PATCH, MERGE, COPY);
{$SCOPEDENUMS OFF}

implementation

end.
