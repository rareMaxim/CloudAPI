unit CoreAPI.Indy;

interface

uses
  IdHTTPHeaderInfo,
  IdHTTP,
  IdSSLOpenSSL,
  IdMultipartFormData,
  CoreAPI.Base, CoreAPI;

type
  TtgCoreApi = class(TtgCoreApiBase, ItgRequestAPI)
  private
    FProxySettings: TIdProxyConnectionInfo;
    FHttp: TIdHTTP;
    FSSL: TIdSSLIOHandlerSocketOpenSSL;
    function GetProxySettings: TIdProxyConnectionInfo;
    procedure SetProxySettings(const Value: TIdProxyConnectionInfo);
  protected
    function DoPost: string;
    function DoGet: string;
    procedure FillFormData(var AForm: TIdMultiPartFormDataStream);
  public
    function Execute: string; override;
    constructor Create(const AProxy: TIdProxyConnectionInfo);
    destructor Destroy; override;
    property ProxySettings: TIdProxyConnectionInfo read GetProxySettings write SetProxySettings;
  end;

implementation

uses
  CoreAPI.Parameter,
  CoreAPI.ParameterConverter.Indy,
  TelegaPi.Exceptions,
  TelegaPi.Types.ReplyMarkups,
  REST.Json,
  System.SysUtils;

{ TtgCoreApiSysNet }

constructor TtgCoreApi.Create(const AProxy: TIdProxyConnectionInfo);
begin
  inherited Create;
  FHttp := TIdHTTP.Create;
  FProxySettings := AProxy;
  FSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  FHttp.IOHandler := FSSL;
end;

destructor TtgCoreApi.Destroy;
begin
  FSSL.Free;
  FHttp.Free;
  inherited;
end;

function TtgCoreApi.DoGet: string;
begin
  try
    Result := FHttp.Get(Url);
    if Assigned(OnSend) then
      OnSend(Url, '');
  except
    on E: Exception do
    begin
      Result := '';
      DoHaveException(E);
    end;
  end;
end;

function TtgCoreApi.DoPost: string;
var
  PostData: TIdMultiPartFormDataStream;
begin
  PostData := TIdMultiPartFormDataStream.Create;
  try
    try
      FillFormData(PostData);
      Result := FHttp.Post(Url, PostData);
      if Assigned(OnSend) then
        OnSend(Url, StreamToString(PostData));
    except
      on E: Exception do
      begin
        Result := '';
        DoHaveException(E);
      end;
    end;
  finally
    PostData.Free;
  end;
end;

function TtgCoreApi.Execute: string;
begin
  if FProxySettings <> nil then
    FHttp.ProxyParams := FProxySettings;
  if Parameters.Count > 0 then
  begin
    Result := DoPost;
    ClearParameter;
  end
  else
    Result := DoGet;
  if Result = '' then
    Exit;
  LastRequestIsOk := True;
  if Assigned(OnReceive) then
    OnReceive(Result);
  if Assigned(DataExtractor) then
    Result := DataExtractor(Result);
end;

procedure TtgCoreApi.FillFormData(var AForm: TIdMultiPartFormDataStream);
var
  LParam: TtgApiParameter;
  ParamConverter: TtgParamConverter;
begin
  ParamConverter := TtgParamConverter.Create;
  try
    for LParam in Parameters do
    begin
        // skip all empty params
      if LParam.Skip then
        Continue;
      if LParam.Required and (LParam.IsDefaultValue or LParam.Value.IsEmpty) then
        DoHaveException(ETelegramException.Create('Not assigned required data [TtgApiRequest.FillFormData]'));
      if ParamConverter.IsSupported(LParam) then
        ParamConverter.ApplyParamToFormData(LParam, AForm)
      else if LParam.Value.IsType<IReplyMarkup>then
        AForm.AddFormField(LParam.Key, TJson.ObjectToJsonString(LParam.Value.AsObject))
      else
        DoHaveException(ETelegramException.Create('Check parameter type ' + LParam.Value.ToString + ' [TtgApiRequest.FillFormData]'));
    end;
  finally
    ParamConverter.Free;
  end;
end;

function TtgCoreApi.GetProxySettings: TIdProxyConnectionInfo;
begin
  Result := FProxySettings;
end;

procedure TtgCoreApi.SetProxySettings(const Value: TIdProxyConnectionInfo);
begin
  FProxySettings := Value;
end;

end.

