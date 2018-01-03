unit CoreAPI.Request.Indy;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  CoreAPI.Parameter,
  TelegAPi.Bot.Impl,
  IdHTTP,
  IdSSLOpenSSL,
  IdHTTPHeaderInfo,
  IdMultipartFormData;

type
  TtgApiRequest = class
  private
    FParams: TObjectList<TtgApiParameter>;
    FHttp: TIdHTTP;
    FSSL: TIdSSLIOHandlerSocketOpenSSL;
    FOnReceive: TProc<string>;
    FOnSend: TProc<string, string>;
    FOnError: TProc<Exception>;
    FProxySettings: TIdProxyConnectionInfo;
    FUrl: string;
  protected
    procedure DoHaveException(const AException: Exception);
    function DoPost: string;
    function DoGet: string;
    procedure FillFormData(var AForm: TIdMultiPartFormDataStream);
    function StreamToString(Stream: TStream): string;
  public
    constructor Create(const AURL: string);
  //  function Execute(out Return: IHTTPResponse): Boolean; overload;
    function Execute(out Return: string): Boolean; overload;
    destructor Destroy; override;
    property Parameters: TObjectList<TtgApiParameter> read FParams write FParams;
    property ProxySettings: TIdProxyConnectionInfo read FProxySettings write FProxySettings;
    property Url: string read FUrl write FUrl;
    property OnReceive: TProc<string> read FOnReceive write FOnReceive;
    property OnSend: TProc<string, string> read FOnSend write FOnSend;
    property OnError: TProc<Exception> read FOnError write FOnError;
  end;

implementation

uses
{$IFDEF  USE_INDY}
  CoreAPI.ParameterConverter.Indy,
{$ELSE}
  CoreAPI.ParameterConverter.SystemNet,
{$ENDIF}
  TelegAPi.Types,
  TelegAPi.Types.ReplyMarkups,
  REST.Json,
  TelegAPi.Exceptions;

{ TtgApiRequest }

constructor TtgApiRequest.Create(const AURL: string);
begin
  FUrl := AURL;
  FParams := TObjectList<TtgApiParameter>.Create;
  FHttp := TIdHTTP.Create;
  FSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
  FHttp.IOHandler := FSSL;
end;

destructor TtgApiRequest.Destroy;
begin
  FHttp.Free;
  FSSL.Free;
  FParams.Free;
  inherited;
end;

function TtgApiRequest.DoGet: string;
begin
  try
    Result := FHttp.Get(FUrl);
    if Assigned(OnSend) then
      OnSend(FUrl, '');
  except
    on E: Exception do
    begin
      Result := '';
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

function TtgApiRequest.DoPost: string;
var
  PostData: TIdMultiPartFormDataStream;
begin
  PostData := TIdMultiPartFormDataStream.Create;
  try
    try
      FillFormData(PostData);
      Result := FHttp.Post(FUrl, PostData);
      if Assigned(OnSend) then
        OnSend(FUrl, StreamToString(PostData));
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

function TtgApiRequest.Execute(out Return: string): Boolean;
begin
  Result := False;
  if FProxySettings <> nil then
    FHttp.ProxyParams := FProxySettings;
  if Parameters.Count > 0 then
    Return := DoPost
  else
    Return := DoGet;
  if Return = '' then
    Exit;
  Result := True;
  if Assigned(OnReceive) then
    OnReceive(Return);
end;

procedure TtgApiRequest.FillFormData(var AForm: TIdMultiPartFormDataStream);
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

function TtgApiRequest.StreamToString(Stream: TStream): string;
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

