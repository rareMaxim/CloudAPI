unit CloudAPI.Client.Base;

interface

uses
  CloudAPI.Ext.MethodLimits,
  CloudAPI.IAuthenticator,
  CloudAPI.Parameter,
  CloudAPI.Request,
  CloudAPI.Response,
  System.Generics.Collections,
  System.JSON.Serializers,
  System.Net.HttpClient,
  System.Net.Mime,
  System.Net.URLClient,
  System.SysUtils;

type
  TCloudApiClientBase = class
  private
    FAuthenticator: IAuthenticator;
    FBaseUrl: string;
    FHttpClient: THTTPClient;
    FVersion: string;
    FDefaultParams: TList<TcaParameter>;
    FRequestLimitManager: TcaRequestLimitManager;
    class var FSerializer: TJsonSerializer;
    function GetAuthenticator: IAuthenticator;
    function GetBaseUrl: string;
    procedure SetAuthenticator(const Value: IAuthenticator);
    procedure SetBaseUrl(const Value: string);
  protected
    procedure AuthenticateIfNeeded(ARequest: IcaRequest);
    function GetSerializer: TJsonSerializer;
    function InternalExecute(ARequest: IcaRequest): IcaResponseBase;
    procedure WriteLimitInfo(ARequest: IcaRequest);
    procedure DoOnLimit(const ATimeLimit: Int64);
    function BuildHttpRequestAndSetupHttp(ARequest: IcaRequest; AFormData: TMultipartFormData): IHTTPRequest;
    function BuildUrl(ARequest: IcaRequest): TUri;
  public
    constructor Create; overload;
    constructor Create(const ABaseUrl: string); overload;
    destructor Destroy; override;
    property Authenticator: IAuthenticator read GetAuthenticator write SetAuthenticator;
    property BaseUrl: string read GetBaseUrl write SetBaseUrl;
    property HttpClient: THTTPClient read FHttpClient write FHttpClient;
    class property Serializer: TJsonSerializer read FSerializer;
    property Version: string read FVersion;
    property DefaultParams: TList<TcaParameter> read FDefaultParams;
    property RequestLimitManager: TcaRequestLimitManager read FRequestLimitManager write FRequestLimitManager;
  end;

implementation

uses
  CloudAPI.Enum,
  CloudAPI.Types,
  System.Classes,
  System.Rtti;

{ TCloudApiClientBase }

procedure TCloudApiClientBase.AuthenticateIfNeeded(ARequest: IcaRequest);
begin
  if Assigned(FAuthenticator) then
    FAuthenticator.Authenticate(ARequest);
end;

constructor TCloudApiClientBase.Create;
begin
  FHttpClient := THTTPClient.Create;
  FHttpClient.UserAgent := 'CloudAPI for Delphi v 2.0.1';
  FHttpClient.ResponseTimeout := 5000;
  FSerializer := TJsonSerializer.Create;
  FDefaultParams := TList<TcaParameter>.Create;
  FRequestLimitManager := TcaRequestLimitManager.Create;
end;

constructor TCloudApiClientBase.Create(const ABaseUrl: string);
begin
  self.Create;
  FBaseUrl := ABaseUrl;
end;

destructor TCloudApiClientBase.Destroy;
begin
  FRequestLimitManager.Free;
  FDefaultParams.Free;
  FSerializer.Free;
  FHttpClient.Free;
  inherited;
end;

procedure TCloudApiClientBase.DoOnLimit(const ATimeLimit: Int64);
begin
  if ATimeLimit > 0 then
    if Assigned(FRequestLimitManager.OnLimit) then
      FRequestLimitManager.OnLimit(ATimeLimit);
end;

function TCloudApiClientBase.InternalExecute(ARequest: IcaRequest): IcaResponseBase;
var
  LHttpRequest: IHTTPRequest;
  LHttpResponse: IHTTPResponse;
  LResponseContent: TStream;
  LMultiFormData: TMultipartFormData;
  I: Integer;
begin
  AuthenticateIfNeeded(ARequest);
  for I := 0 to FDefaultParams.Count - 1 do
    ARequest.AddParam(FDefaultParams[I]);
  LResponseContent := nil;
  LMultiFormData := TMultipartFormData.Create;
  try
    LHttpRequest := BuildHttpRequestAndSetupHttp(ARequest, LMultiFormData);
    WriteLimitInfo(ARequest);
    LHttpResponse := FHttpClient.Execute(LHttpRequest, LResponseContent, LHttpRequest.Headers);
  finally
    LMultiFormData.Free;
  end;

  Result := TcaResponseBase.Create(ARequest, LHttpRequest, LHttpResponse);
end;

function TCloudApiClientBase.GetAuthenticator: IAuthenticator;
begin
  Result := FAuthenticator;
end;

function TCloudApiClientBase.GetBaseUrl: string;
begin
  Result := FBaseUrl;
end;

function TCloudApiClientBase.GetSerializer: TJsonSerializer;
begin
  Result := FSerializer;
end;

procedure TCloudApiClientBase.SetAuthenticator(const Value: IAuthenticator);
begin
  FAuthenticator := Value;
end;

procedure TCloudApiClientBase.SetBaseUrl(const Value: string);
begin
  FBaseUrl := Value;
end;

function TCloudApiClientBase.BuildHttpRequestAndSetupHttp(ARequest: IcaRequest; AFormData: TMultipartFormData)
  : IHTTPRequest;
var
  LParam: TcaParameter;
  LFile: TcaFileToSend;
  LUrl: TUri;
  LCookie: TCookie;
  LMethodString: string;
begin
  LMethodString := TRttiEnumerationType.GetName<TcaMethod>(ARequest.Method);
  LUrl := BuildUrl(ARequest);
  Result := FHttpClient.GetRequest(LMethodString, LUrl);
  for LFile in ARequest.Files do
  begin
    case LFile.Tag of
      TcaFileToSendTag.FromFile:
        AFormData.AddFile(LFile.Name, LFile.Data);
      TcaFileToSendTag.FromStream:
        AFormData.AddStream(LFile.Name, LFile.Content, LFile.Data);
    end;
  end;
  for LParam in ARequest.GetOrPosts do
  begin
    if ARequest.IsMultipartFormData then
    begin
      AFormData.AddField(LParam.Name, LParam.ValueAsString);
    end
    else
    begin
      LUrl.AddParameter(LParam.Name, LParam.ValueAsString);
    end;
  end;

  for LParam in ARequest.HttpHeaders do
  begin
    Result.AddHeader(LParam.Name, LParam.ValueAsString);
  end;
  for LParam in ARequest.Cookies do
  begin
      { TODO -oMaxim Sysoev -cGeneral : Протестировать куки }
    LCookie.Name := LParam.Name;
    LCookie.Value := LParam.ValueAsString;
    FHttpClient.CookieManager.AddServerCookie(LCookie, LUrl);
  end;
  if ARequest.IsMultipartFormData then
  begin
    AFormData.Stream.Position := 0;
    Result.SourceStream := AFormData.Stream;
    Result.AddHeader('Content-Type', AFormData.MimeTypeHeader);
  end;

end;

function TCloudApiClientBase.BuildUrl(ARequest: IcaRequest): TUri;
var
  LFullUrl: string;
  LParam: TcaParameter;
begin
  LFullUrl := FBaseUrl + '/' + ARequest.Resource;
  for LParam in ARequest.UrlSegments do
  begin
    LFullUrl := LFullUrl.Replace('{' + LParam.Name + '}', LParam.ValueAsString);
  end;
  Result := TUri.Create(LFullUrl);

  for LParam in ARequest.QueryParameters do
  begin
    Result.AddParameter(LParam.Name, LParam.ValueAsString);
  end;
end;

procedure TCloudApiClientBase.WriteLimitInfo(ARequest: IcaRequest);
var
  LTotalWait: Int64;
begin
  LTotalWait := FRequestLimitManager.LocalWait(ARequest.LimitInfo.Name);
  DoOnLimit(LTotalWait);
  FRequestLimitManager.Add(ARequest.LimitInfo.Limit, ARequest.LimitInfo.Name, ARequest.LimitInfo.IsGlobal);
end;

end.
