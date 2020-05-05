unit CloudAPI.Response;

interface

uses
  CloudAPI.Request,
  System.JSON.Serializers,
  System.Net.HttpClient,
  System.SysUtils;

type
  IcaResponseBase = interface
    ['{D577F707-054A-449C-BE42-015B7EF03CDC}']
    // private
    function GetHttpRequest: IHTTPRequest;
    function GetHttpResponse: IHTTPResponse;
    procedure SetHttpRequest(const Value: IHTTPRequest);
    procedure SetHttpResponse(const Value: IHTTPResponse);
    // public
    property HttpRequest: IHTTPRequest read GetHttpRequest write SetHttpRequest;
    property HttpResponse: IHTTPResponse read GetHttpResponse write SetHttpResponse;
  end;

  TcaResponseBase = class(TInterfacedObject, IcaResponseBase)
  private
    FHttpRequest: IHTTPRequest;
    FHttpResponse: IHTTPResponse;
    function GetHttpRequest: IHTTPRequest;
    function GetHttpResponse: IHTTPResponse;

    procedure SetHttpRequest(const Value: IHTTPRequest);
    procedure SetHttpResponse(const Value: IHTTPResponse);

  public
    constructor Create(ACloudRequest: IcaRequest; AHttpRequest: IHTTPRequest; AHttpResponse: IHTTPResponse);
    property HttpRequest: IHTTPRequest read GetHttpRequest write SetHttpRequest;
    property HttpResponse: IHTTPResponse read GetHttpResponse write SetHttpResponse;

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
    FDataJson: string;
    function GetData: T;
    function GetSerializer: TJsonSerializer;
    procedure SetData(const Value: T);
    procedure SetSerializer(const Value: TJsonSerializer);
  protected
    procedure DoUpdateData;
  public
    constructor Create(ACloudRequest: IcaRequest; AHttpRequest: IHTTPRequest; AHttpResponse: IHTTPResponse;
      ASerializer: TJsonSerializer); reintroduce;
    property Data: T read GetData write SetData;
    property Serializer: TJsonSerializer read GetSerializer write SetSerializer;
  end;

implementation

constructor TcaResponseBase.Create(ACloudRequest: IcaRequest; AHttpRequest: IHTTPRequest; AHttpResponse: IHTTPResponse);
begin
  inherited Create();
  FHttpRequest := AHttpRequest;
  FHttpResponse := AHttpResponse;
end;

function TcaResponseBase.GetHttpRequest: IHTTPRequest;
begin
  Result := FHttpRequest;
end;

function TcaResponseBase.GetHttpResponse: IHTTPResponse;
begin
  Result := FHttpResponse;
end;

procedure TcaResponseBase.SetHttpRequest(const Value: IHTTPRequest);
begin
  FHttpRequest := Value;
end;

procedure TcaResponseBase.SetHttpResponse(const Value: IHTTPResponse);
begin
  FHttpResponse := Value;
end;

constructor TcaResponse<T>.Create(ACloudRequest: IcaRequest; AHttpRequest: IHTTPRequest; AHttpResponse: IHTTPResponse;
  ASerializer: TJsonSerializer);
begin
  inherited Create(ACloudRequest, AHttpRequest, AHttpResponse);
  FSerializer := ASerializer;
  DoUpdateData;
end;

procedure TcaResponse<T>.DoUpdateData;
begin
  FDataJson := GetHttpResponse.ContentAsString(TEncoding.UTF8);
  SetData(FSerializer.Deserialize<T>(FDataJson));
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

end.
