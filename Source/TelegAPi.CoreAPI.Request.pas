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
    const
      SERVER_URL = 'https://api.telegram.org/bot';
  private
    FMethod: string;
    FParams: TObjectList<TtgApiParameter>;
    FHttp: THTTPClient;
    FOnReceive: TProc<string>;
    FTelega: TTelegramBot;
    FOnSend: TProc<string, string>;
    FOnError: TProc<Exception>;
  protected
    function DoPost: IHTTPResponse;
    function DoGet: IHTTPResponse;
    procedure FillFormData(var AForm: TMultipartFormData);
    function StreamToString(Stream: TMemoryStream): string;
  public
    constructor Create(Sender: TTelegramBot; const AMethod: string);
    function Execute(out Return: IHTTPResponse): Boolean; overload;
    function Execute(out Return: string): Boolean; overload;
    function GetUrl: string;
    destructor Destroy; override;
    property Parameters: TObjectList<TtgApiParameter> read FParams write FParams;
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

constructor TtgApiRequest.Create(Sender: TTelegramBot; const AMethod: string);
begin
  FMethod := AMethod;
  FTelega := Sender;
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
  Result := FHttp.Get(GetUrl);
  if Assigned(OnSend) then
    OnSend(GetUrl, '');
end;

function TtgApiRequest.DoPost: IHTTPResponse;
var
  PostData: TMultipartFormData;
begin
  PostData := TMultipartFormData.Create;
  try
    FillFormData(PostData);
    Result := FHttp.Post(GetUrl, PostData);
    if Assigned(OnSend) then
      OnSend(GetUrl, StreamToString(PostData.Stream));
  finally
    PostData.Free;
  end;
end;

function TtgApiRequest.Execute(out Return: IHTTPResponse): Boolean;
begin
  Result := False;
  FHttp.ProxySettings := FTelega.ProxySettings;
  try
    if Parameters.Count > 0 then
      Return := DoPost
    else
      Return := DoGet;
    if Return = nil then
      Exit;
    Result := True;
    if Assigned(OnReceive) then
      OnReceive(Return.ContentAsString);
  except
    on E: Exception do
    begin
      Result := False;
      if Assigned(OnError) then
        OnError(E);
    end;
  end;
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
        FTelega.ExceptionManager.HaveGlobalExeption('TtgApiRequest.FillFormData', ETelegramException.Create('Not assigned required data'));
      if ParamConverter.IsSupported(LParam) then
        ParamConverter.ApplyParamToFormData(LParam, AForm)
      else if LParam.Value.Kind = tkClass then        // last variant to search
      begin
        { TODO -oOwner -cGeneral : Проверить че за херня тут твориться }
        if not LParam.Value.IsEmpty then
        begin
          if LParam.Value.IsType<IReplyMarkup>then
            AForm.AddField(LParam.Key, TJson.ObjectToJsonString(LParam.Value.AsObject))
          else
            FTelega.ExceptionManager.HaveGlobalExeption('TTelegramBot.ParamsToFormData', Exception.Create('Unknown object'));
        end;
      end
      else
        FTelega.ExceptionManager.HaveGlobalExeption('ParamsToFormData', ETelegramDataConvert.Create('Check parameter type ' + LParam.Value.ToString))
    end;
  finally
    ParamConverter.Free;
  end;
end;

function TtgApiRequest.GetUrl: string;
begin
  Result := SERVER_URL + FTelega.Token + '/' + FMethod;
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

