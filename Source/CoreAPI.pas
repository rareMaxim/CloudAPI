unit CoreAPI;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
{$IFDEF USE_INDY}
  IdHTTPHeaderInfo,
  IdHTTP,
  IdSSLOpenSSL,
  IdMultipartFormData,
{$ELSE}
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.Mime,
{$ENDIF}
  CoreAPI.Parameter,
  System.Classes,
  TelegAPI.Utils.Json;

type
  ItgRequestAPI = interface
    // private
    function GetOnError: TProc<Exception>;
    procedure SetOnError(const Value: TProc<Exception>);
{$IFDEF USE_INDY}
    function GetProxySettings: TIdProxyConnectionInfo;
    procedure SetProxySettings(const Value: TIdProxyConnectionInfo);
{$ELSE}
    function GetProxySettings: TProxySettings;
    procedure SetProxySettings(const Value: TProxySettings);
{$ENDIF}
    function GetOnSend: TProc<string, string>;
    procedure SetOnSend(const Value: TProc<string, string>);
    function GetDataExtractor: TFunc<string, string>;
    procedure SetDataExtractor(const Value: TFunc<string, string>);
    // public
    function SetToken(const AToken: string): ItgRequestAPI;
    function SetMethod(const AMethod: string): ItgRequestAPI;
    function AddParameter(const AKey: string; AValue, ADefaultValue: TValue; ARequired: Boolean = False): ItgRequestAPI;
    function ClearParameter: ItgRequestAPI;
    function Execute: string;
    function ExecuteAsBool: Boolean;
    function ExecuteAndReadValue: string;
    // props
{$IFDEF USE_INDY}
    property ProxySettings: TIdProxyConnectionInfo read GetProxySettings write SetProxySettings;
{$ELSE}
    property ProxySettings: TProxySettings read GetProxySettings write SetProxySettings;
{$ENDIF}
    property DataExtractor: TFunc<string, string> read GetDataExtractor write SetDataExtractor;
    // events
    property OnError: TProc<Exception> read GetOnError write SetOnError;
    property OnSend: TProc<string, string> read GetOnSend write SetOnSend;
  end;

  TtgRequestAPI = class(TInterfacedObject, ItgRequestAPI)
  private
    const
      SERVER = 'https://api.telegram.org/bot';
  private
    FToken: string;
    FMethod: string;
    FParameters: TObjectList<TtgApiParameter>;
    FOnError: TProc<Exception>;
{$IFDEF USE_INDY}
    FProxySettings: TIdProxyConnectionInfo;
    FHttp: TIdHTTP;
    FSSL: TIdSSLIOHandlerSocketOpenSSL;
{$ELSE}
    FProxySettings: TProxySettings;
    FHttp: THTTPClient;
{$ENDIF}
    FGetOnSend: TProc<string, string>;
    FDataExtractor: TFunc<string, string>;
    FOnReceive: TProc<string>;
    FLastRequestIsOk: Boolean;
    function GetOnError: TProc<Exception>;
    procedure SetOnError(const Value: TProc<Exception>);
{$IFDEF USE_INDY}
    function GetProxySettings: TIdProxyConnectionInfo;
    procedure SetProxySettings(const Value: TIdProxyConnectionInfo);
{$ELSE}
    function GetProxySettings: TProxySettings;
    procedure SetProxySettings(const Value: TProxySettings);
{$ENDIF}
    function GetOnSend: TProc<string, string>;
    procedure SetOnSend(const Value: TProc<string, string>);
    function GetOnReceive: TProc<string>;
    procedure SetOnReceive(const Value: TProc<string>);
    function GetUrl: string;
    function GetDataExtractor: TFunc<string, string>;
    procedure SetDataExtractor(const Value: TFunc<string, string>);
  protected
    procedure DoHaveException(const AException: Exception);
    function StreamToString(Stream: TStream): string;
{$IFDEF USE_INDY}
    function DoPost: string;
    function DoGet: string;
    procedure FillFormData(var AForm: TIdMultiPartFormDataStream);
{$ELSE}
    function DoPost: string;
    function DoGet: string;
    procedure FillFormData(var AForm: TMultipartFormData);
{$ENDIF}
  public
    function SetToken(const AToken: string): ItgRequestAPI;
    function SetMethod(const AMethod: string): ItgRequestAPI;
    function AddParameter(const AKey: string; AValue, ADefaultValue: TValue; ARequired: Boolean = False): ItgRequestAPI;
    function ClearParameter: ItgRequestAPI;
    function Execute: string;
    function ExecuteAsBool: Boolean;
    function ExecuteAndReadValue: string;
    constructor Create;
    destructor Destroy; override;
    property Url: string read GetUrl;
{$IFDEF USE_INDY}
    property ProxySettings: TIdProxyConnectionInfo read GetProxySettings write SetProxySettings;
{$ELSE}
    property ProxySettings: TProxySettings read GetProxySettings write SetProxySettings;
{$ENDIF}
    property DataExtractor: TFunc<string, string> read GetDataExtractor write SetDataExtractor;
    property OnSend: TProc<string, string> read GetOnSend write SetOnSend;
    property OnReceive: TProc<string> read GetOnReceive write SetOnReceive;
    property OnError: TProc<Exception> read GetOnError write SetOnError;
  end;

implementation

uses
{$IFDEF USE_INDY}
  CoreAPI.ParameterConverter.Indy,
{$ELSE}
  CoreAPI.ParameterConverter.SystemNet,
{$ENDIF}
  TelegAPI.Types.ReplyMarkups,
  TelegAPI.Exceptions,
  REST.Json,
  System.Json;

{ TtgRequestAPI }

function TtgRequestAPI.AddParameter(const AKey: string; AValue, ADefaultValue: TValue; ARequired: Boolean): ItgRequestAPI;
begin
  FParameters.Add(TtgApiParameter.Create(AKey, AValue, ADefaultValue, ARequired));
  Result := Self;
end;

function TtgRequestAPI.ClearParameter: ItgRequestAPI;
begin
  FParameters.Clear;
  Result := Self;
end;

constructor TtgRequestAPI.Create;
begin
  FParameters := TObjectList<TtgApiParameter>.Create;
{$IFDEF USE_INDY}
  FHttp := TIdHTTP.Create;
  FSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  FHttp.IOHandler := FSSL;
{$ELSE}
  FHttp := THTTPClient.Create;
{$ENDIF}
end;

destructor TtgRequestAPI.Destroy;
begin
  FHttp.Free;
  FSSL.Free;
  FParameters.Free;
  inherited;
end;

procedure TtgRequestAPI.DoHaveException(const AException: Exception);
begin
  if Assigned(OnError) then
    OnError(AException)
  else
    raise AException;
end;

function TtgRequestAPI.Execute: string;
begin
  FLastRequestIsOk := False;
{$IFDEF USE_INDY}
  if FProxySettings <> nil then
    FHttp.ProxyParams := FProxySettings;
{$ELSE}
  FHttp.ProxySettings := FProxySettings;
{$ENDIF}
  if FParameters.Count > 0 then
  begin
    Result := DoPost;
    ClearParameter;
  end
  else
    Result := DoGet;
  if Result = '' then
    Exit;
  FLastRequestIsOk := True;
  if Assigned(OnReceive) then
    OnReceive(Result);
  if Assigned(DataExtractor) then
    Result := DataExtractor(Result);
end;

function TtgRequestAPI.ExecuteAndReadValue: string;
var
  LJson: TJSONValue;
begin
  LJson := TJSONObject.ParseJSONValue(Execute);
  try
    Result := LJson.Value;
  finally
    LJson.Free;
  end;
end;

function TtgRequestAPI.ExecuteAsBool: Boolean;
var
  LJson: TJSONValue;
begin
  LJson := TJSONObject.ParseJSONValue(Execute);
  try
    Result := LJson is TJSONTrue;
  finally
    LJson.Free;
  end;
end;

function TtgRequestAPI.GetDataExtractor: TFunc<string, string>;
begin
  Result := FDataExtractor;
end;

function TtgRequestAPI.GetOnError: TProc<Exception>;
begin
  Result := FOnError;
end;

function TtgRequestAPI.GetOnReceive: TProc<string>;
begin
  Result := FOnReceive;
end;

function TtgRequestAPI.GetOnSend: TProc<string, string>;
begin
  Result := FGetOnSend;
end;

function TtgRequestAPI.GetUrl: string;
begin
  Result := SERVER + FToken + '/' + FMethod;
end;

procedure TtgRequestAPI.SetDataExtractor(const Value: TFunc<string, string>);
begin
  FDataExtractor := Value;
end;

function TtgRequestAPI.SetMethod(const AMethod: string): ItgRequestAPI;
begin
  FMethod := AMethod;
  Result := Self;
end;

procedure TtgRequestAPI.SetOnError(const Value: TProc<Exception>);
begin
  FOnError := Value;
end;

procedure TtgRequestAPI.SetOnReceive(const Value: TProc<string>);
begin
  FOnReceive := Value;
end;

procedure TtgRequestAPI.SetOnSend(const Value: TProc<string, string>);
begin
  FGetOnSend := Value;
end;

{$IFDEF USE_INDY}

procedure TtgRequestAPI.SetProxySettings(const Value: TIdProxyConnectionInfo);
begin
  FProxySettings := Value;
end;

function TtgRequestAPI.DoGet: string;
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

function TtgRequestAPI.DoPost: string;
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

procedure TtgRequestAPI.FillFormData(var AForm: TIdMultiPartFormDataStream);
var
  LParam: TtgApiParameter;
  ParamConverter: TtgParamConverter;
begin
  ParamConverter := TtgParamConverter.Create;
  try
    for LParam in FParameters do
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

function TtgRequestAPI.GetProxySettings: TIdProxyConnectionInfo;
begin
  Result := FProxySettings;
end;
{$ELSE}

procedure TtgRequestAPI.FillFormData(var AForm: TMultipartFormData);
var
  LParam: TtgApiParameter;
  ParamConverter: TtgParamConverter;
begin
  ParamConverter := TtgParamConverter.Create;
  try
    for LParam in FParameters do
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

function TtgRequestAPI.DoPost: string;
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

function TtgRequestAPI.DoGet: string;
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

procedure TtgRequestAPI.SetProxySettings(const Value: TProxySettings);
begin
  FProxySettings := Value;
end;

function TtgRequestAPI.GetProxySettings: TProxySettings;
begin
  Result := FProxySettings;
end;
{$ENDIF}

function TtgRequestAPI.SetToken(const AToken: string): ItgRequestAPI;
begin
  FToken := AToken;
  Result := Self;
end;

function TtgRequestAPI.StreamToString(Stream: TStream): string;
var
  LStrings: TStringList;
begin
  LStrings := TStringList.Create;
  try
    Stream.Position := 0;
    LStrings.LoadFromStream(Stream);
    Result := LStrings.Text;
  finally
    LStrings.Free;
  end;
end;

end.

