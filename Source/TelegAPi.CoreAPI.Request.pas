unit TelegAPi.CoreAPI.Request;

interface

uses
  System.Rtti,
  System.Net.HttpClient,
  System.Generics.Collections,
  System.Net.URLClient,
  System.Net.Mime,
  System.SysUtils,
  TelegAPi.CoreAPI.Parameter,
  TelegAPi.Bot.Impl,
  System.Classes;

type
  TtgApiRequest = class
  private
    FParams: TObjectList<TtgApiParameter>;
    FHttp: THTTPClient;
    FOnReceive: TProc<string>;
    FOnSend: TProc<string, string>;
    FOnError: TProc<Exception>;
    FProxySettings: TProxySettings;
    FUrl: string;
  protected
    procedure DoHaveException(const AException: Exception);
    function DoPost: IHTTPResponse;
    function DoGet: IHTTPResponse;
    procedure FillFormData(var AForm: TMultipartFormData);
    function StreamToString(Stream: TMemoryStream): string;
  public
    constructor Create(const AURL: string);
    function Execute(out Return: IHTTPResponse): Boolean; overload;
    function Execute(out Return: string): Boolean; overload;
    destructor Destroy; override;
    property Parameters: TObjectList<TtgApiParameter> read FParams write FParams;
    property ProxySettings: TProxySettings read FProxySettings write FProxySettings;
    property Url: string read FUrl write FUrl;
    property OnReceive: TProc<string> read FOnReceive write FOnReceive;
    property OnSend: TProc<string, string> read FOnSend write FOnSend;
    property OnError: TProc<Exception> read FOnError write FOnError;
  end;

implementation

uses
  TelegAPi.Types,
  TelegAPi.Types.ReplyMarkups,
  REST.Json,
  TelegAPi.Exceptions,
  TelegAPi.CoreAPI.ParameterConverter;

{ TtgApiRequest }

constructor TtgApiRequest.Create(const AURL: string);
begin
  FUrl := AURL;
  FParams := TObjectList<TtgApiParameter>.Create;
  FHttp := THTTPClient.Create;
end;

destructor TtgApiRequest.Destroy;
begin
  FHttp.Free;
  FParams.Free;
  inherited;
end;

function TtgApiRequest.DoGet: IHTTPResponse;
begin
  try
    Result := FHttp.Get(FUrl);
    if Assigned(OnSend) then
      OnSend(FUrl, '');
  except
    on E: Exception do
    begin
      Result := nil;
      DoHaveException(E);
    end;
  end;
end;

procedure TtgApiRequest.DoHaveException(const AException: Exception);
begin
  if Assigned(OnError) then
    OnError(AException)
  else
    raise AException;
end;

function TtgApiRequest.DoPost: IHTTPResponse;
var
  PostData: TMultipartFormData;
begin
  PostData := TMultipartFormData.Create;
  try
    try
      FillFormData(PostData);
      Result := FHttp.Post(FUrl, PostData);
      if Assigned(OnSend) then
        OnSend(FUrl, StreamToString(PostData.Stream));
    except
      on E: Exception do
      begin
        Result := nil;
        DoHaveException(E);
      end;
    end;
  finally
    PostData.Free;
  end;
end;

function TtgApiRequest.Execute(out Return: IHTTPResponse): Boolean;
begin
  Result := False;
  FHttp.ProxySettings := FProxySettings;
  if Parameters.Count > 0 then
    Return := DoPost
  else
    Return := DoGet;
  if Return = nil then
    Exit;
  Result := True;
  if Assigned(OnReceive) then
    OnReceive(Return.ContentAsString);
end;

function TtgApiRequest.Execute(out Return: string): Boolean;
var
  LResponse: IHTTPResponse;
begin
  Result := Execute(LResponse);
  if Result then
    Return := LResponse.ContentAsString(TEncoding.UTF8);
end;

procedure TtgApiRequest.FillFormData(var AForm: TMultipartFormData);
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

function TtgApiRequest.StreamToString(Stream: TMemoryStream): string;
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

