unit CoreAPI.SysNet;

interface

uses
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.Mime,
  CoreAPI.Base, CoreAPI;

type
  TtgCoreApi = class(TtgCoreApiBase, ItgRequestAPI)
  private
    FProxySettings: TProxySettings;
    FHttp: THTTPClient;
    function GetProxySettings: TProxySettings;
    procedure SetProxySettings(const Value: TProxySettings);
  protected
    function DoPost: string;
    function DoGet: string;
    procedure FillFormData(var AForm: TMultipartFormData);
  public
    function Execute: string; override;
    constructor Create(const Proxy: TProxySettings);
    destructor Destroy; override;
    property ProxySettings: TProxySettings read GetProxySettings write SetProxySettings;
  end;

implementation

uses
  CoreAPI.Parameter,
  CoreAPI.ParameterConverter.SystemNet,
  TelegaPi.Exceptions,
  TelegaPi.Types.ReplyMarkups,
  REST.Json,
  System.SysUtils;

{ TtgCoreApiSysNet }

constructor TtgCoreApi.Create(const Proxy: TProxySettings);
begin
  inherited Create;
  FHttp := THTTPClient.Create;
  FProxySettings:=Proxy;
end;

destructor TtgCoreApi.Destroy;
begin
  FHttp.Free;
  inherited;
end;

function TtgCoreApi.DoGet: string;
begin
  try
    Result := FHttp.Get(Url).ContentAsString;
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
  PostData: TMultipartFormData;
begin
  PostData := TMultipartFormData.Create;
  try
    try
      FillFormData(PostData);
      Result := FHttp.Post(Url, PostData).ContentAsString;
      if Assigned(OnSend) then
        OnSend(Url, StreamToString(PostData.Stream));
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
  FHttp.ProxySettings := FProxySettings;
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

procedure TtgCoreApi.FillFormData(var AForm: TMultipartFormData);
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
        AForm.AddField(LParam.Key, TJson.ObjectToJsonString(LParam.Value.AsObject))
      else
        DoHaveException(ETelegramException.Create('Check parameter type ' + LParam.Value.ToString + ' [TtgApiRequest.FillFormData]'));
    end;
  finally
    ParamConverter.Free;
  end;
end;

function TtgCoreApi.GetProxySettings: TProxySettings;
begin
  Result := FProxySettings;
end;

procedure TtgCoreApi.SetProxySettings(const Value: TProxySettings);
begin
  FProxySettings := Value;
end;

end.

