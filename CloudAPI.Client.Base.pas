unit CloudAPI.Client.Base;

interface

uses
  CloudAPI.Exceptions,
  CloudAPI.Ext.MethodLimits,
  CloudAPI.IAuthenticator,
  CloudAPI.Parameter,
  CloudAPI.Request,
  CloudAPI.Response,
  CloudAPI.Response.Printer,
  System.Classes,
  System.Generics.Collections,
  System.JSON.Serializers,
  System.Net.HttpClient,
  System.Net.Mime,
  System.Net.URLClient,
  System.SysUtils;

type
  TCloudApiClientBase = class(TPersistent)
  public const
    LIB_VERSION = '4.3.0';
  private
    FAuthenticator: IAuthenticator;
    FBaseUrl: string;
    FHttpClient: THTTPClient;
    FVersion: string;
    FDefaultParams: TList<TcaParameter>;
    FRequestLimitManager: TcaRequestLimitManager;
    FResponseStream: TStream;
    FSerializer: TJsonSerializer;
    fExceptionManager: TcaExceptionManager;
    FResponsePrinter: TcaResponsePrinter;
    FOnExcecuteCallback: TProc<IcaResponseBase>;
    function GetAuthenticator: IAuthenticator;
    function GetBaseUrl: string;
    procedure SetAuthenticator(const Value: IAuthenticator);
    procedure SetBaseUrl(const Value: string);
  protected
    procedure AuthenticateIfNeeded(ARequest: IcaRequest);
    function GetSerializer: TJsonSerializer;
    function TryInternalExcecute(ARequest: IcaRequest; var AResp: IcaResponseBase): Boolean;
    procedure WriteLimitInfo(ARequest: IcaRequest);
    procedure DoOnLimit(const ATimeLimit: Int64);
    procedure DoOnExcecute(AcaResponse: IcaResponseBase);
  public
    constructor Create; overload;
    constructor Create(const ABaseUrl: string); overload;
    destructor Destroy; override;
  public
    procedure Assign(Source: TPersistent); override;
    property Authenticator: IAuthenticator read GetAuthenticator write SetAuthenticator;
    property BaseUrl: string read GetBaseUrl write SetBaseUrl;
    property DefaultParams: TList<TcaParameter> read FDefaultParams;
    property HttpClient: THTTPClient read FHttpClient;
    property RequestLimitManager: TcaRequestLimitManager read FRequestLimitManager write FRequestLimitManager;
    property ResponseStream: TStream read FResponseStream write FResponseStream;
    property Version: string read FVersion;
    property Serializer: TJsonSerializer read FSerializer;
    property ExceptionManager: TcaExceptionManager read fExceptionManager write fExceptionManager;
    property ResponsePrinter: TcaResponsePrinter read FResponsePrinter write FResponsePrinter;
    property OnExcecuteCallback: TProc<IcaResponseBase> read FOnExcecuteCallback write FOnExcecuteCallback;
  end;

implementation

uses
  CloudAPI.Core.RequestBuilder,
  CloudAPI.Types,
  System.Rtti;
{ TCloudApiClientBase }

procedure TCloudApiClientBase.Assign(Source: TPersistent);
begin
  if Source is TCloudApiClientBase then
  begin
    FAuthenticator := TCloudApiClientBase(Source).Authenticator;
    FOnExcecuteCallback := TCloudApiClientBase(Source).FOnExcecuteCallback;
  end
  else
    inherited Assign(Source);
end;

procedure TCloudApiClientBase.AuthenticateIfNeeded(ARequest: IcaRequest);
begin
  if Assigned(FAuthenticator) then
    FAuthenticator.Authenticate(ARequest);
end;

constructor TCloudApiClientBase.Create;
begin
  FHttpClient := THTTPClient.Create;
  FHttpClient.AllowCookies := True;
  FHttpClient.AutomaticDecompression := [THTTPCompressionMethod.Any];
  FSerializer := TJsonSerializer.Create;
  FHttpClient.UserAgent := 'CloudAPI for Delphi v.' + LIB_VERSION;
  FHttpClient.ResponseTimeout := 5000;
  FDefaultParams := TList<TcaParameter>.Create;
  FRequestLimitManager := TcaRequestLimitManager.Create;
  FResponseStream := nil;
  fExceptionManager := TcaExceptionManager.Current;
  FResponsePrinter := TcaResponsePrinter.Create();
end;

constructor TCloudApiClientBase.Create(const ABaseUrl: string);
begin
  self.Create;
  FBaseUrl := ABaseUrl;
end;

destructor TCloudApiClientBase.Destroy;
begin
  FSerializer.Free;
  FRequestLimitManager.Free;
  FDefaultParams.Free;
  FHttpClient.Free;
  // fExceptionManager.Free;
  FResponsePrinter.Free;
  inherited;
end;

procedure TCloudApiClientBase.DoOnExcecute(AcaResponse: IcaResponseBase);
begin
  if Assigned(OnExcecuteCallback) then
    OnExcecuteCallback(AcaResponse);
end;

procedure TCloudApiClientBase.DoOnLimit(const ATimeLimit: Int64);
begin
  if ATimeLimit > 0 then
    if Assigned(FRequestLimitManager.OnLimit) then
      FRequestLimitManager.OnLimit(ATimeLimit);
end;

function TCloudApiClientBase.TryInternalExcecute(ARequest: IcaRequest; var AResp: IcaResponseBase): Boolean;
var
  I: Integer;
  lHttpRequest: IHTTPRequest;
  lHttpResponse: IHTTPResponse;
  lException: ECloudApiException;
begin
  lException := nil;
  if not Assigned(ARequest) then
    ARequest := TcaRequest.Create;
  AuthenticateIfNeeded(ARequest);
  for I := 0 to FDefaultParams.Count - 1 do
    ARequest.AddParam(FDefaultParams[I]);

  lHttpRequest := TRequestBuilder.Build(self, ARequest);
  WriteLimitInfo(ARequest);
  ARequest.StartAt := Now;
  try
    lHttpResponse := FHttpClient.Execute(lHttpRequest, FResponseStream, lHttpRequest.Headers);
    Result := True;
  except
    on E: Exception do
    begin
      lException := ECloudApiException.Create(E.ClassName, E.ToString);
      ExceptionManager.Alert(lException);
      lHttpResponse := nil;
      Result := False;
    end;
  end;
  AResp := TcaResponseBase.Create(ARequest, lHttpRequest, lHttpResponse, lException);
  FResponsePrinter.ParseResponse(AResp as TcaResponseBase);
  DoOnExcecute(AResp);
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

procedure TCloudApiClientBase.WriteLimitInfo(ARequest: IcaRequest);
var
  LTotalWait: Int64;
begin
  LTotalWait := FRequestLimitManager.LocalWait(ARequest.LimitInfo.Name);
  DoOnLimit(LTotalWait);
  FRequestLimitManager.Add(ARequest.LimitInfo.Limit, ARequest.LimitInfo.Name, ARequest.LimitInfo.IsGlobal);
end;

end.
