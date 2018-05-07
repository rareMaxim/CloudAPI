unit CoreAPI;

interface

uses
  TelegAPI.Base,
  CrossUrl.HttpClient,
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  System.Generics.Collections,
  CoreAPI.Parameter,
  System.Classes;

type
  ItgRequestAPI = interface
    ['{3DC5A653-F52D-4A31-87AD-0C008AFA7111}']
    // private
    function GetOnError: TProc<Exception>;
    procedure SetOnError(const Value: TProc<Exception>);
    function GetOnSend: TProc<string, string>;
    procedure SetOnSend(const Value: TProc<string, string>);
    function GetOnReceive: TProc<string>;
    procedure SetOnReceive(const Value: TProc<string>);
    function GetDataExtractor: TFunc<string, string>;
    procedure SetDataExtractor(const Value: TFunc<string, string>);
    // public
    function SetToken(const AToken: string): ItgRequestAPI;
    function SetMethod(const AMethod: string): ItgRequestAPI;
    function AddParameter(const AKey: string; AValue, ADefaultValue: TValue;
      ARequired: Boolean = False): ItgRequestAPI;
    function ClearParameters: ItgRequestAPI;
    function Execute: string;
    function ExecuteAsBool: Boolean;
    function ExecuteAndReadValue: string;
    // props
    property DataExtractor: TFunc<string, string> read GetDataExtractor write
      SetDataExtractor;
    // events
    property OnError: TProc<Exception> read GetOnError write SetOnError;
    property OnSend: TProc<string, string> read GetOnSend write SetOnSend;
    property OnReceive: TProc<string> read GetOnReceive write SetOnReceive;
  end;

  TtgCoreApiBase = class(TtgAbstractComponent, ItgRequestAPI)
  protected
    const
      SERVER = 'https://api.telegram.org/bot';
  private
    FParameters: TObjectList<TtgApiParameter>;
    FGetOnSend: TProc<string, string>;
    FDataExtractor: TFunc<string, string>;
    FOnReceive: TProc<string>;
    FLastRequestIsOk: Boolean;
    FToken: string;
    FMethod: string;
    FOnError: TProc<Exception>;
    function GetOnError: TProc<Exception>;
    procedure SetOnError(const Value: TProc<Exception>);
    function GetOnSend: TProc<string, string>;
    procedure SetOnSend(const Value: TProc<string, string>);
    function GetOnReceive: TProc<string>;
    procedure SetOnReceive(const Value: TProc<string>);
    function GetDataExtractor: TFunc<string, string>;
    procedure SetDataExtractor(const Value: TFunc<string, string>);
    function GetParameters: TObjectList<TtgApiParameter>;
    procedure SetParameters(const Value: TObjectList<TtgApiParameter>);
    function GetUrl: string;
  protected
    procedure DoHaveException(const AException: Exception);
    function StreamToString(Stream: TStream): string;
  public
    function SetToken(const AToken: string): ItgRequestAPI;
    function SetMethod(const AMethod: string): ItgRequestAPI;
    function AddParameter(const AKey: string; AValue, ADefaultValue: TValue;
      ARequired: Boolean = False): ItgRequestAPI;
    function ClearParameters: ItgRequestAPI;
    function Execute: string; virtual; abstract;
    function ExecuteAsBool: Boolean;
    function ExecuteAndReadValue: string;
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  // props
    property LastRequestIsOk: Boolean read FLastRequestIsOk write FLastRequestIsOk;
    property DataExtractor: TFunc<string, string> read GetDataExtractor write
      SetDataExtractor;
    property Url: string read GetUrl;
    property Parameters: TObjectList<TtgApiParameter> read GetParameters write
      SetParameters;
  // events
    property OnError: TProc<Exception> read GetOnError write SetOnError;
    property OnSend: TProc<string, string> read GetOnSend write SetOnSend;
    property OnReceive: TProc<string> read GetOnReceive write SetOnReceive;
  end;

  TtgCoreApi = class(TtgCoreApiBase, ItgRequestAPI)
  private
    FHttp: IcuHttpClient;
  protected
    function DoPost: string;
    function DoGet: string;
    procedure FillFormData(var AForm: IcuMultipartFormData);
  public
    function Execute: string; override;
    constructor Create;
    destructor Destroy; override;
    property HttpCore: IcuHttpClient read FHttp write FHttp;
  end;

type
  /// <summary>
  /// This class is used by func. TTelegramBotCore.ParamsToFormData to locate
  /// suitable param loader for API request parameters preparation.
  /// </summary>
  TtgParamConverter = class
  public
    type
    // parameter loader method
      TLoader = procedure(var AFormData: IcuMultipartFormData; AParam:
        TtgApiParameter) of object;
  protected
    procedure AddInteger(var AFormData: IcuMultipartFormData; AParam: TtgApiParameter);
    procedure AddTDateTime(var AFormData: IcuMultipartFormData; AParam: TtgApiParameter);
    procedure AddString(var AFormData: IcuMultipartFormData; AParam: TtgApiParameter);
    procedure AddInt64(var AFormData: IcuMultipartFormData; AParam: TtgApiParameter);
    procedure AddBoolean(var AFormData: IcuMultipartFormData; AParam: TtgApiParameter);
    procedure AddClass_TtgFileToSend(var AFormData: IcuMultipartFormData; AParam:
      TtgApiParameter);
  public
    ParamLoaders: TDictionary<PTypeInfo, TtgParamConverter.TLoader>;
    constructor Create;
    destructor Destroy; override;
    function IsSupported(Param: TtgApiParameter): Boolean;
    function ApplyParamToFormData(const AParam: TtgApiParameter; var Form:
      IcuMultipartFormData): Boolean;
  end;

implementation

uses
  System.DateUtils,
  System.JSON,
  REST.Json,
  TelegAPI.Exceptions,
  TelegAPI.Types.ReplyMarkups,
  TelegAPI.Types;

{ TtgCoreApiBase }

function TtgCoreApiBase.AddParameter(const AKey: string; AValue, ADefaultValue:
  TValue; ARequired: Boolean): ItgRequestAPI;
begin
  FParameters.Add(TtgApiParameter.Create(AKey, AValue, ADefaultValue, ARequired));
  Result := Self;
end;

function TtgCoreApiBase.ClearParameters: ItgRequestAPI;
begin
  FParameters.Clear;
  Result := Self;
end;

constructor TtgCoreApiBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParameters := TObjectList<TtgApiParameter>.Create;
end;

destructor TtgCoreApiBase.Destroy;
begin
  FParameters.Free;
  inherited;
end;

procedure TtgCoreApiBase.DoHaveException(const AException: Exception);
begin
  if Assigned(OnError) then
    OnError(AException)
  else
    raise AException;
end;

function TtgCoreApiBase.ExecuteAndReadValue: string;
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

function TtgCoreApiBase.ExecuteAsBool: Boolean;
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

function TtgCoreApiBase.GetDataExtractor: TFunc<string, string>;
begin
  Result := FDataExtractor;
end;

function TtgCoreApiBase.GetOnError: TProc<Exception>;
begin
  Result := FOnError;
end;

function TtgCoreApiBase.GetOnReceive: TProc<string>;
begin
  Result := FOnReceive;
end;

function TtgCoreApiBase.GetOnSend: TProc<string, string>;
begin
  Result := FGetOnSend;
end;

function TtgCoreApiBase.GetParameters: TObjectList<TtgApiParameter>;
begin
  Result := FParameters;
end;

function TtgCoreApiBase.GetUrl: string;
begin
  Result := SERVER + FToken + '/' + FMethod;
end;

procedure TtgCoreApiBase.SetDataExtractor(const Value: TFunc<string, string>);
begin
  FDataExtractor := Value;
end;

function TtgCoreApiBase.SetMethod(const AMethod: string): ItgRequestAPI;
begin
  FMethod := AMethod;
  Result := Self;
end;

procedure TtgCoreApiBase.SetOnError(const Value: TProc<Exception>);
begin
  FOnError := Value;
end;

procedure TtgCoreApiBase.SetOnReceive(const Value: TProc<string>);
begin
  FOnReceive := Value;
end;

procedure TtgCoreApiBase.SetOnSend(const Value: TProc<string, string>);
begin
  FGetOnSend := Value;
end;

procedure TtgCoreApiBase.SetParameters(const Value: TObjectList<TtgApiParameter>);
begin
  FParameters := Value;
end;

function TtgCoreApiBase.SetToken(const AToken: string): ItgRequestAPI;
begin
  FToken := AToken;
  Result := Self;
end;

function TtgCoreApiBase.StreamToString(Stream: TStream): string;
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

{ TtgCoreApiSysNet }

constructor TtgCoreApi.Create;
begin
  inherited Create(nil);
end;

destructor TtgCoreApi.Destroy;
begin
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
  PostData: IcuMultipartFormData;
begin
  PostData := FHttp.CreateMultipartFormData;
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

end;

function TtgCoreApi.Execute: string;
begin
  if Parameters.Count > 0 then
  begin
    Result := DoPost;
    ClearParameters;
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

procedure TtgCoreApi.FillFormData(var AForm: IcuMultipartFormData);
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
        DoHaveException(ETelegramException.Create('Check parameter type ' +
          LParam.Value.ToString + ' [TtgApiRequest.FillFormData]'));
    end;
  finally
    ParamConverter.Free;
  end;
end;

procedure TtgParamConverter.AddInteger(var AFormData: IcuMultipartFormData;
  AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsInteger.ToString);
end;

procedure TtgParamConverter.AddString(var AFormData: IcuMultipartFormData;
  AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsString);
end;

procedure TtgParamConverter.AddTDateTime(var AFormData: IcuMultipartFormData;
  AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, DateTimeToUnix(AParam.Value.AsType<TDateTime>,
    False).ToString);
end;

function TtgParamConverter.ApplyParamToFormData(const AParam: TtgApiParameter;
  var Form: IcuMultipartFormData): Boolean;
begin
  Result := ParamLoaders.ContainsKey(AParam.Value.TypeInfo);
  if not Result then
    Exit;
  ParamLoaders[AParam.Value.TypeInfo](Form, AParam);
end;

constructor TtgParamConverter.Create;
begin
  // init type-lookup dictionary
  ParamLoaders := TDictionary<PTypeInfo, TtgParamConverter.TLoader>.Create;
  // primitive types
  ParamLoaders.Add(PTypeInfo(TypeInfo(Integer)), AddInteger);
  ParamLoaders.Add(PTypeInfo(TypeInfo(string)), AddString);
  ParamLoaders.Add(PTypeInfo(TypeInfo(Int64)), AddInt64);
  ParamLoaders.Add(PTypeInfo(TypeInfo(Boolean)), AddBoolean);
  ParamLoaders.Add(PTypeInfo(TypeInfo(TDateTime)), AddTDateTime);
  // class types
  ParamLoaders.Add(PTypeInfo(TypeInfo(TtgFileToSend)), AddClass_TtgFileToSend);
end;

destructor TtgParamConverter.Destroy;
begin
  ParamLoaders.Free;
  inherited;
end;

function TtgParamConverter.IsSupported(Param: TtgApiParameter): Boolean;
begin
  Result := ParamLoaders.ContainsKey(Param.Value.TypeInfo);
end;

procedure TtgParamConverter.AddInt64(var AFormData: IcuMultipartFormData; AParam:
  TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsInt64.ToString);
end;

procedure TtgParamConverter.AddBoolean(var AFormData: IcuMultipartFormData;
  AParam: TtgApiParameter);
begin
  AFormData.AddField(AParam.Key, AParam.Value.AsBoolean.ToString(TUseBoolStrs.True));
end;

procedure TtgParamConverter.AddClass_TtgFileToSend(var AFormData:
  IcuMultipartFormData; AParam: TtgApiParameter);
var
  LFileToSent: TtgFileToSend;
begin
  LFileToSent := AParam.Value.AsType<TtgFileToSend>;
  case LFileToSent.Tag of
    TtgFileToSendTag.FromStream:
      AFormData.AddStream(AParam.Key, LFileToSent.Content, LFileToSent.Data);
    TtgFileToSendTag.FromFile:
      AFormData.AddFile(AParam.Key, LFileToSent.Data);
    TtgFileToSendTag.ID, TtgFileToSendTag.FromURL:
      AFormData.AddField(AParam.Key, LFileToSent.Data);
  else
    raise Exception.Create('Cant convert TTgFileToSend: Unknown prototype tag');
  end;
end;

end.

