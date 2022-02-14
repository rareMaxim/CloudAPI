unit CloudAPI.Response.Printer;

interface

uses
  System.Json.Serializers,
  System.Net.HttpClient,
  System.Net.URLClient,
  CloudAPI.Response;

type
  TrpRequest = class
  private
    [JsonName('Url')]
    FUrl: string;
    [JsonName('Method')]
    FMethod: string;
    [JsonName('Headers')]
    FHeaders: TArray<string>;
    [JsonName('Content')]
    FContent: string;
  protected

  public
    constructor Create(AHttpRequest: IHTTPRequest);
    class function TNetHeadersToStrings(AHeaders: TNetHeaders): TArray<string>;
  public
    property Url: string read FUrl;
    property Method: string read FMethod;
    property Headers: TArray<string> read FHeaders;
    property Content: string read FContent;
  end;

  TrpResponse = class
  private
    [JsonName('StatusCode')]
    FStatusCode: Integer;
    [JsonName('StatusText')]
    FStatusText: string;
    [JsonName('Headers')]
    FHeaders: TArray<string>;
    [JsonName('Content')]
    FContent: string;
  public
    constructor Create(AHttpResponse: IHTTPResponse);
  public
    property StatusCode: Integer read FStatusCode;
    property StatusText: string read FStatusText;
    property Content: string read FContent;
  end;

  TcaResponsePrinter = class
  private
    [JsonName('Request')]
    FRequest: TrpRequest;
    [JsonName('Response')]
    FResponse: TrpResponse;
  protected
  public
    function AsJson: string;
    constructor Create();

    procedure ParseResponse(AResponse: TcaResponseBase);
    procedure FreeData;
    destructor Destroy; override;
    property Request: TrpRequest read FRequest;
    property Response: TrpResponse read FResponse;
    class procedure ToConsole(AResponse: TcaResponseBase);
  end;

implementation

uses
  System.Json.Types,
  System.SysUtils,
  System.Classes;

function StreamToString(aStream: TStream): string;
var
  SS: TStringStream;
begin
  if aStream <> nil then
  begin
    SS := TStringStream.Create('');
    try
      SS.CopyFrom(aStream, 0); // No need to position at 0 nor provide size
      Result := SS.DataString;
    finally
      SS.Free;
    end;
  end
  else
  begin
    Result := '';
  end;
end;
{ TcaPrintResponse }

function TcaResponsePrinter.AsJson: string;
var
  lSerializer: TJsonSerializer;
begin
  lSerializer := TJsonSerializer.Create;
  try
    lSerializer.Formatting := TJsonFormatting.Indented;
    lSerializer.StringEscapeHandling := TJsonStringEscapeHandling.EscapeNonAscii;
    Result := lSerializer.Serialize<TcaResponsePrinter>(Self);
  finally
    lSerializer.Free;
  end;
end;

constructor TcaResponsePrinter.Create();
begin
  FRequest := nil;
  FResponse := nil;
end;

destructor TcaResponsePrinter.Destroy;
begin
  FreeData;
  inherited;
end;

procedure TcaResponsePrinter.FreeData;
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  if Assigned(FResponse) then
    FreeAndNil(FResponse);
end;

procedure TcaResponsePrinter.ParseResponse(AResponse: TcaResponseBase);
begin
  FreeData;
  if Assigned(AResponse.HttpRequest) then
    FRequest := TrpRequest.Create(AResponse.HttpRequest);
  if Assigned(AResponse.HttpResponse) then
    FResponse := TrpResponse.Create(AResponse.HttpResponse);
end;

class procedure TcaResponsePrinter.ToConsole(AResponse: TcaResponseBase);
var
  lSelf: TcaResponsePrinter;
begin
  lSelf := TcaResponsePrinter.Create();
  try
    lSelf.ParseResponse(AResponse);
    Writeln(lSelf.AsJson);
  finally
    lSelf.Free;
  end;
end;

{ TrpRequest }

constructor TrpRequest.Create(AHttpRequest: IHTTPRequest);
begin
  FUrl := AHttpRequest.Url.ToString;
  FMethod := AHttpRequest.MethodString;
  FHeaders := TNetHeadersToStrings(AHttpRequest.Headers);
  FContent := StreamToString(AHttpRequest.SourceStream);
end;

class function TrpRequest.TNetHeadersToStrings(AHeaders: TNetHeaders): TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, length(AHeaders));
  for i := Low(AHeaders) to High(AHeaders) do
    Result[i] := AHeaders[i].Name + ' = ' + AHeaders[i].Value;
end;

{ TrpResponse }

constructor TrpResponse.Create(AHttpResponse: IHTTPResponse);
begin
  FStatusCode := AHttpResponse.StatusCode;
  FStatusText := AHttpResponse.StatusText;
  FHeaders := TrpRequest.TNetHeadersToStrings(AHttpResponse.Headers);
  if AHttpResponse.HeaderValue['Content-Type'] = 'application/json' then
    FContent := AHttpResponse.ContentAsString();
end;

end.
