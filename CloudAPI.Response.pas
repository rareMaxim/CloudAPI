unit CloudAPI.Response;

interface

uses
  CloudAPI.Exceptions,
  CloudAPI.Request,
  System.JSON,
  System.JSON.Serializers,
  System.Net.HttpClient,
  System.SysUtils;

type
  TcaTiming = record
  private
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    function GetDuration: Integer;
  public
    class function Create(const AStartTime, AEndTime: TDateTime): TcaTiming; static;
    property StartTime: TDateTime read FStartTime;
    property EndTime: TDateTime read FEndTime;
    property Duration: Integer read GetDuration;
  end;

  IcaResponseBase = interface
    ['{D577F707-054A-449C-BE42-015B7EF03CDC}']
    // private
    function GetHttpRequest: IHTTPRequest;
    function GetHttpResponse: IHTTPResponse;
    procedure SetHttpRequest(const Value: IHTTPRequest);
    procedure SetHttpResponse(const Value: IHTTPResponse);
    function GetTiming: TcaTiming;
    function GetException: ECloudApiException;
    procedure SetException(const Value: ECloudApiException);
    // public
    function AsJson: TJSONValue;
    function RawBytes: TBytes;
    property HttpRequest: IHTTPRequest read GetHttpRequest write SetHttpRequest;
    property HttpResponse: IHTTPResponse read GetHttpResponse write SetHttpResponse;
    property Timing: TcaTiming read GetTiming;
    property Exception: ECloudApiException read GetException write SetException;
  end;

  TcaResponseBase = class(TInterfacedObject, IcaResponseBase)
  private
    FHttpRequest: IHTTPRequest;
    FHttpResponse: IHTTPResponse;
    FTiming: TcaTiming;
    fException: ECloudApiException;
    FJson: TJSONValue;
    function GetHttpRequest: IHTTPRequest;
    function GetHttpResponse: IHTTPResponse;
    procedure SetHttpRequest(const Value: IHTTPRequest);
    procedure SetHttpResponse(const Value: IHTTPResponse);
    function GetTiming: TcaTiming;
    function GetException: ECloudApiException;
    procedure SetException(const Value: ECloudApiException);
  protected
    procedure TryLoadJSON(AHttpResponse: IHTTPResponse);
  public
    function AsJson: TJSONValue;
    function RawBytes: TBytes;
    constructor Create(ACloudRequest: IcaRequest; AHttpRequest: IHTTPRequest; AHttpResponse: IHTTPResponse;
      AException: ECloudApiException);
    destructor Destroy; override;
    property HttpRequest: IHTTPRequest read GetHttpRequest write SetHttpRequest;
    property HttpResponse: IHTTPResponse read GetHttpResponse write SetHttpResponse;
    property Timing: TcaTiming read GetTiming;
    property Exception: ECloudApiException read GetException write SetException;
  end;

  IcaResponse<T> = interface(IcaResponseBase)
    // private
    function GetData: T;
    function GetSerializer: TJsonSerializer;
    procedure SetData(const Value: T);
    procedure SetSerializer(const Value: TJsonSerializer);
    // public
    property Data: T read GetData write SetData;
    property Serializer: TJsonSerializer read GetSerializer write SetSerializer;
  end;

  TcaResponse<T> = class(TcaResponseBase, IcaResponse<T>)
  private
    FSerializer: TJsonSerializer;
    FData: T;
    function GetData: T;
    function GetSerializer: TJsonSerializer;
    procedure SetData(const Value: T);
    procedure SetSerializer(const Value: TJsonSerializer);
  protected
    procedure DoUpdateData(const AData: string);
    function TestHtml(const AData: string): Boolean;
  public
    constructor Create(ACloudRequest: IcaRequest; AHttpRequest: IHTTPRequest; AHttpResponse: IHTTPResponse;
      ASerializer: TJsonSerializer; AException: ECloudApiException); reintroduce;
    property Data: T read GetData write SetData;
    property Serializer: TJsonSerializer read GetSerializer write SetSerializer;
  end;

implementation

uses
  CloudAPI.Types;

function TcaResponseBase.AsJson: TJSONValue;
begin
  Result := FJson;
end;

constructor TcaResponseBase.Create(ACloudRequest: IcaRequest; AHttpRequest: IHTTPRequest; AHttpResponse: IHTTPResponse;
  AException: ECloudApiException);
begin
  inherited Create();
  FHttpRequest := AHttpRequest;
  FHttpResponse := AHttpResponse;
  FTiming := TcaTiming.Create(ACloudRequest.StartAt, Now);
  fException := AException;
  if Assigned(AHttpResponse) then
    TryLoadJSON(AHttpResponse);
end;

destructor TcaResponseBase.Destroy;
begin
  if Assigned(FJson) then
    FreeAndNil(FJson);
  inherited;
end;

function TcaResponseBase.GetException: ECloudApiException;
begin
  Result := fException;
end;

function TcaResponseBase.GetHttpRequest: IHTTPRequest;
begin
  Result := FHttpRequest;
end;

function TcaResponseBase.GetHttpResponse: IHTTPResponse;
begin
  Result := FHttpResponse;
end;

function TcaResponseBase.GetTiming: TcaTiming;
begin
  Result := FTiming;
end;

function TcaResponseBase.RawBytes: TBytes;
begin
  FHttpResponse.ContentStream.Position := 0;
  SetLength(Result, FHttpResponse.ContentStream.Size);
  FHttpResponse.ContentStream.Read(Result[0], FHttpResponse.ContentStream.Size);
end;

procedure TcaResponseBase.SetException(const Value: ECloudApiException);
begin
  fException := Value;
end;

procedure TcaResponseBase.SetHttpRequest(const Value: IHTTPRequest);
begin
  FHttpRequest := Value;
end;

procedure TcaResponseBase.SetHttpResponse(const Value: IHTTPResponse);
begin
  FHttpResponse := Value;
end;

procedure TcaResponseBase.TryLoadJSON(AHttpResponse: IHTTPResponse);
var
  lJsonStr: string;
begin
  if Assigned(FJson) then
    FreeAndNil(FJson);
  if AHttpResponse.HeaderValue['Content-Type'] <> 'application/json' then
    Exit;
  try
    lJsonStr := AHttpResponse.ContentAsString(TEncoding.UTF8);
    FJson := TJSONObject.ParseJSONValue(lJsonStr);
  except
    on E: System.SysUtils.Exception do
      fException := ECloudApiException.Create(E.ToString);
  end;
end;

constructor TcaResponse<T>.Create(ACloudRequest: IcaRequest; AHttpRequest: IHTTPRequest; AHttpResponse: IHTTPResponse;
  ASerializer: TJsonSerializer; AException: ECloudApiException);
var
  lContentAsString: string;
begin
  inherited Create(ACloudRequest, AHttpRequest, AHttpResponse, AException);
  FSerializer := ASerializer;
  if Assigned(FHttpResponse) then
  begin
    lContentAsString := FHttpResponse.ContentAsString(TEncoding.UTF8);
    if TestHtml(lContentAsString) then
      fException := ECloudApiException.Create('600', 'Server return Html text')
    else if not Assigned(fException) then
      DoUpdateData(lContentAsString);
  end;
end;

procedure TcaResponse<T>.DoUpdateData(const AData: string);
begin
  try
    FData := FSerializer.Deserialize<T>(AData);
  except
    on E: System.SysUtils.Exception do
    begin
      fException := ECloudApiException.Create(E.ClassName, E.ToString);
    end;
  end;
end;

function TcaResponse<T>.GetData: T;
begin
  Result := FData;
end;

function TcaResponse<T>.GetSerializer: TJsonSerializer;
begin
  Result := FSerializer;
end;

procedure TcaResponse<T>.SetData(const Value: T);
begin
  FData := Value;
end;

procedure TcaResponse<T>.SetSerializer(const Value: TJsonSerializer);
begin
  FSerializer := Value;
end;

function TcaResponse<T>.TestHtml(const AData: string): Boolean;
begin
  Result := AData.Substring(0, 5).TrimLeft.ToLower = '<html';
end;

{ TcaTiming }

class function TcaTiming.Create(const AStartTime, AEndTime: TDateTime): TcaTiming;
begin
  Result.FStartTime := AStartTime;
  Result.FEndTime := AEndTime;
end;

function TcaTiming.GetDuration: Integer;
begin
  Result := TcaRequestLimit.DatesDuration(EndTime, StartTime);
end;

end.
