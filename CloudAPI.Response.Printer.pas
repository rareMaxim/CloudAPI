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
  protected

  public
    constructor Create(AHttpRequest: IHTTPRequest);
    class function TNetHeadersToStrings(AHeaders: TNetHeaders): TArray<string>;
  public
    property Url: string read FUrl;
    property Method: string read FMethod;
    property Headers: TArray<string> read FHeaders;
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
  System.SysUtils;
{ TcaPrintResponse }

function TcaResponsePrinter.AsJson: string;
var
  lSerializer: TJsonSerializer;
begin
  lSerializer := TJsonSerializer.Create;
  try
    lSerializer.Formatting := TJsonFormatting.Indented;
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
  FRequest := TrpRequest.Create(AResponse.HttpRequest);
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
  FContent := AHttpResponse.ContentAsString();
end;

end.
