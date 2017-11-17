unit TelegAPI.Bot.Impl;

interface

uses
  System.Rtti,
  System.Classes,
  System.TypInfo,
  System.SysUtils,
  System.Net.Mime,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Generics.Collections,
  TelegAPI.Base,
  TelegAPI.Bot,
  TelegAPI.Types,
  TelegAPI.Types.Impl,
  TelegAPI.Types.Enums,
  TelegAPI.Types.ReplyMarkups,
  TelegAPI.Types.InlineQueryResults,
  TelegAPI.Exceptions,
  TelegAPI.Utils.Params,
  TelegAPI.Utils.Json,
  System.Json;

type
  TtgOnReceiveRawData = procedure(ASender: TObject; const AData: string) of object;

  TTelegramBot = class(TtgAbstractComponent, ITelegramBot)
  private
    FToken: string;
    FProxySettings: TProxySettings;
    FOnRawData: TtgOnReceiveRawData;
    FParamLoader: TtgParamLoader;
    FExceptionManager: ItgExceptionHandler;
    function GetToken: string;
    procedure SetToken(const Value: string);
    // Create TBaseJ
    function CreateBaseJSONClass<T: TBaseJson>(const Params: string): T;

    // Returns TJSONValue as method request result
    function GetJSONFromMethod(const Method: string; Parameters: TDictionary<string, TValue>): TJSONValue;

    // Returns TJSONArray as method request result
    function GetJSONArrayFromMethod(const Method: string; Parameters: TDictionary<string, TValue>): TJSONArray;

    // Returns true when given Method executed successfully
    function ExecuteMethod(const Method: string; Parameters: TDictionary<string, TValue>): Boolean;

    // Returns response JSON from server as result of request

    function GetValueFromMethod(const Method: string; Parameters: TDictionary<string, TValue>): string;
    function GetExceptionManager: ItgExceptionHandler;
    procedure SetExceptionManager(const Value: ItgExceptionHandler);

    // function GetArrayFromMethod<T:TBaseJson; TI:IInterface>(const Method: string; Parameters: TDictionary<string, TValue>):TArray<TI>;

  protected
    /// <summary>
    /// Мастер-функция для запросов на сервак
    /// </summary>
    function RequestAPI(const Method: string; Parameters: TDictionary<string, TValue>): string;
    function SendDataToServer(const Method: string; Parameters: TDictionary<string, TValue>): string;
    function ParamsToFormData(Parameters: TDictionary<string, TValue>): TMultipartFormData;
  public
    function ApiTest(const ARequest: string; Parameters: TDictionary<string, TValue> = nil): string;
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
{$REGION 'Getting updates'}
    function GetUpdates(const Offset: Int64 = 0; const Limit: Int64 = 100; const Timeout: Int64 = 0; AllowedUpdates: TAllowedUpdates = UPDATES_ALLOWED_ALL): TArray<ItgUpdate>;
    function SetWebhook(const Url: string; Certificate: TtgFileToSend = nil; MaxConnections: Int64 = 40; AllowedUpdates: TAllowedUpdates = UPDATES_ALLOWED_ALL): Boolean;
    function DeleteWebhook: Boolean;
    function GetWebhookInfo: ItgWebhookInfo;
{$ENDREGION}
{$REGION 'Basic methods'}
    function GetMe: ItgUser;
    function SendMessage(const ChatId: TValue; const Text: string; ParseMode: TtgParseMode = TtgParseMode.Default; DisableWebPagePreview: Boolean = False; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function ForwardMessage(ChatId: TValue; FromChatId: TValue; MessageId: Int64; DisableNotification: Boolean = False): ITgMessage;
    function SendPhoto(ChatId: TValue; Photo: TValue; const Caption: string = ''; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendAudio(ChatId: TValue; Audio: TValue; const Caption: string = ''; Duration: Int64 = 0; const Performer: string = ''; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendDocument(ChatId: TValue; Document: TValue; const Caption: string = ''; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVideo(ChatId: TValue; Video: TValue; const Caption: string = ''; Duration: Int64 = 0; Width: Int64 = 0; Height: Int64 = 0; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVoice(ChatId: TValue; Voice: TValue; const Caption: string = ''; Duration: Int64 = 0; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVideoNote(ChatId: TValue; //
      VideoNote: TValue; //
      Duration: Int64 = 0; //
      Length: Int64 = 0; //
      DisableNotification: Boolean = False; //
      ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil //
    ): ITgMessage;
    function SendLocation(ChatId: TValue; Location: TtgLocation; LivePeriod: Int64 = 0; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVenue(ChatId: TValue; Venue: TtgVenue; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendContact(ChatId: TValue; Contact: TtgContact; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendChatAction(ChatId: TValue; const Action: TtgSendChatAction): Boolean;
    function GetUserProfilePhotos(ChatId: TValue; Offset: Int64; Limit: Int64 = 100): TtgUserProfilePhotos;
    function GetFile(const FileId: string): ItgFile;
    function KickChatMember(ChatId: TValue; UserId: Int64; UntilDate: Int64 = 0): Boolean;
    function UnbanChatMember(ChatId: TValue; UserId: Int64): Boolean;
    function LeaveChat(ChatId: TValue): Boolean;
    function GetChat(const ChatId: TValue): ItgChat;
    function GetChatAdministrators(const ChatId: TValue): TArray<ItgChatMember>;
    function GetChatMembersCount(const ChatId: TValue): Int64;
    function GetChatMember(ChatId: TValue; UserId: Int64): ItgChatMember;
    function AnswerCallbackQuery(const CallbackQueryId: string; const Text: string = ''; ShowAlert: Boolean = False; const Url: string = ''; CacheTime: Int64 = 0): Boolean;
{$ENDREGION}
{$REGION 'Updating messages'}
    function EditMessageText(ChatId: TValue; MessageId: Int64; const Text: string; ParseMode: TtgParseMode = TtgParseMode.Default; DisableWebPagePreview: Boolean = False; ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function EditMessageText(const InlineMessageId: string; const Text: string; ParseMode: TtgParseMode = TtgParseMode.Default; DisableWebPagePreview: Boolean = False; ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function EditMessageCaption(ChatId: TValue; MessageId: Int64; const Caption: string; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function EditMessageCaption(const InlineMessageId: string; const Caption: string; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function editMessageLiveLocation(ChatId: TValue; MessageId: Int64; Location: TtgLocation; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function editMessageLiveLocation(const InlineMessageId: string; Location: TtgLocation; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function stopMessageLiveLocation(ChatId: TValue; MessageId: Int64; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function stopMessageLiveLocation(const InlineMessageId: string; ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function EditMessageReplyMarkup(ChatId: TValue; MessageId: Int64; ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function EditMessageReplyMarkup(const InlineMessageId: string; ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function DeleteMessage(ChatId: TValue; MessageId: Int64): Boolean;
{$ENDREGION}
{$REGION 'Inline mode'}
    function AnswerInlineQuery(const InlineQueryId: string; Results: TArray<TtgInlineQueryResult>; CacheTime: Int64 = 300; IsPersonal: Boolean = False; const NextOffset: string = ''; const SwitchPmText: string = ''; const SwitchPmParameter: string = ''): Boolean;
{$ENDREGION}
{$REGION 'Payments'}
    function SendInvoice(ChatId: Int64; const title: string; const Description: string; const Payload: string; const ProviderToken: string; const StartParameter: string; const Currency: string; Prices: TArray<TtgLabeledPrice>; const PhotoUrl: string = ''; PhotoSize: Int64 = 0; PhotoWidth: Int64 = 0; PhotoHeight: Int64 = 0; NeedName: Boolean = False; NeedPhoneNumber: Boolean = False; NeedEmail: Boolean = False; NeedShippingAddress: Boolean = False; IsFlexible: Boolean = False; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function AnswerShippingQuery(const ShippingQueryId: string; Ok: Boolean; ShippingOptions: TArray<TtgShippingOption>; const ErrorMessage: string): Boolean;
    function AnswerPreCheckoutQuery(const PreCheckoutQueryId: string; Ok: Boolean; const ErrorMessage: string = ''): Boolean;
{$ENDREGION}
{$REGION 'Games'}
    function SendGame(ChatId: Int64; const GameShortName: string; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SetGameScore(UserId: Int64; Score: Int64; Force: Boolean = False; DisableEditMessage: Boolean = False; ChatId: Int64 = 0; MessageId: Int64 = 0; const InlineMessageId: string = ''): ITgMessage;
    function GetGameHighScores(UserId: Int64; ChatId: Int64 = 0; MessageId: Int64 = 0; const InlineMessageId: string = ''): TArray<ItgGameHighScore>;
{$ENDREGION}
{$REGION 'Manage groups and channels'}
    function DeleteChatPhoto(ChatId: TValue): Boolean;
    function ExportChatInviteLink(ChatId: TValue): string;
    function PinChatMessage(ChatId: TValue; MessageId: Int64; DisableNotification: Boolean = False): Boolean;
    function SetChatDescription(ChatId: TValue; const Description: string): Boolean;
    function SetChatPhoto(ChatId: TValue; Photo: TtgFileToSend): Boolean;
    function SetChatTitle(ChatId: TValue; const title: string): Boolean;
    function UnpinChatMessage(ChatId: TValue): Boolean;
{$ENDREGION}
{$REGION 'Manage users and admins'}
    function RestrictChatMember(ChatId: TValue; UserId: Int64; UntilDate: Int64 = 0; CanSendMessages: Boolean = False; CanSendMediaMessages: Boolean = False; CanSendOtherMessages: Boolean = False; CanAddWebPagePreviews: Boolean = False): Boolean;
    function PromoteChatMember(ChatId: TValue; UserId: Int64; CanChangeInfo: Boolean = False; CanPostMessages: Boolean = False; CanEditMessages: Boolean = False; CanDeleteMessages: Boolean = False; CanInviteUsers: Boolean = False; CanRestrictMembers: Boolean = False; CanPinMessages: Boolean = False; CanPromoteMembers: Boolean = False): Boolean;
{$ENDREGION}
{$REGION 'Strickers'}
    function SendSticker(ChatId: TValue; Sticker: TValue; DisableNotification: Boolean = False; ReplyToMessageId: Int64 = 0; ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function getStickerSet(const Name: string): TtgStickerSet;
    function uploadStickerFile(UserId: Int64; PngSticker: TtgFileToSend): ItgFile;
    function createNewStickerSet(UserId: Int64; const Name, title: string; PngSticker: TValue; const Emojis: string; ContainsMasks: Boolean = False; MaskPosition: TtgMaskPosition = nil): Boolean;
    function addStickerToSet(UserId: Int64; const Name: string; PngSticker: TValue; const Emojis: string; MaskPosition: TtgMaskPosition = nil): Boolean;
    function setStickerPositionInSet(const Sticker: string; Position: Int64): Boolean;
    function deleteStickerFromSet(const Sticker: string): Boolean;
    function setChatStickerSet(ChatId: TValue; const StickerSetName: string): Boolean;
    function deleteChatStickerSet(ChatId: TValue): Boolean;
{$ENDREGION}
  published
{$REGION 'Property|Свойства'}
    /// <summary>
    /// Proxy Settings to be used by the client.
    /// </summary>
    property ProxySettings: TProxySettings read FProxySettings write FProxySettings;
    /// <summary>
    /// <para>
    /// List the types of updates you want your bot to receive.
    /// </para>
    /// <para>
    /// Типы принимаемых сообщений
    /// </para>
    /// </summary>

    /// <summary>
    /// Токен вашего бота.
    /// </summary>
    /// <remarks>
    /// Создать бота и получить токен можно у @BotFather
    /// </remarks>
    /// <example>
    /// 283107813:AAG4hEElAvIogTSHNHXI6rZtE46A7XQvIH
    /// </example>
    property Token: string read GetToken write SetToken;
    property ExceptionManager: ItgExceptionHandler read GetExceptionManager write SetExceptionManager;
{$ENDREGION}
{$REGION 'События|Events'}
    property OnReceiveRawData: TtgOnReceiveRawData read FOnRawData write FOnRawData;
{$ENDREGION}
  end;

implementation

uses
  REST.Json,
  TelegAPI.Helpers;

{ TTelegramBot }
{$REGION 'Core'}

constructor TTelegramBot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParamLoader := TtgParamLoader.Create;
end;

destructor TTelegramBot.Destroy;
begin
  FParamLoader.Free;
  inherited;
end;

function TTelegramBot.RequestAPI(const Method: string; Parameters: TDictionary<string, TValue>): string;
var
  LTextResponse: string;
begin
  LTextResponse := SendDataToServer(Method, Parameters);
  if Assigned(OnReceiveRawData) then
    OnReceiveRawData(Self, LTextResponse);
  if LTextResponse.IsEmpty and Assigned(ExceptionManager) then
  begin
    ExceptionManager.HaveGlobalExeption('RequestAPI', ETelegramUnknownData.Create('Can''t parse response'));
  end
  else
    Result := ApiTest(LTextResponse, Parameters);
end;

function TTelegramBot.ApiTest(const ARequest: string; Parameters: TDictionary<string, TValue>): string;
var
  FJSON: TJSONObject;
  FResult: TJSONValue;
begin
  FJSON := TJSONObject.ParseJSONValue(ARequest) as TJSONObject;
  try
    if FJSON.GetValue('ok') is TJSONFalse then
      if ExceptionManager <> nil then
      begin
        ExceptionManager.HaveApiExeption('TTelegramBot.ApiTest', EApiRequestException.Create(ARequest, 0, Parameters));
        Exit;
      end;
   //   raise Exception.Create(ARequest);

    FResult := FJSON.GetValue('result');
    Result := FResult.ToJSON;
  finally
    FJSON.Free;
  end;
end;

function TTelegramBot.ParamsToFormData(Parameters: TDictionary<string, TValue>): TMultipartFormData;
var
  LParameter: TPair<string, TValue>;
  LAddProc: TtgParamLoader.TLoader;
  LTest: string;
begin
  Result := TMultipartFormData.Create;
  for LParameter in Parameters do
  begin
    // skip all empty params
    if LParameter.Value.IsEmpty then
      Continue;
    // look for the given parameter type
    if FParamLoader.ParamLoaders.TryGetValue(LParameter.Value.TypeInfo, LAddProc) then
    begin
      LAddProc(Result, LParameter.Value.TypeInfo, LParameter.Key, LParameter.Value);
    end
    else if LParameter.Value.Kind = tkClass then
    // last variant to search
    begin
      { TODO -oOwner -cGeneral : Проверить че за херня тут твориться }
      if not LParameter.Value.IsEmpty then
      begin
        if LParameter.Value.IsType<IReplyMarkup>then
        begin
          LTest :=TJson.ObjectToJsonString(LParameter.Value.AsObject);
          Result.AddField(LParameter.Key, LTest);
        end;

        //raise Exception.Create('Error Message');
        // Result.AddField(LParameter.Key, dj.From(LParameter.Value.AsObject).ToJson);
        // LTest := dj.From(LParameter.Value, TJsonUtils.DJsonConfig).ToJSON;
        // Result.AddField(LParameter.Key, LTest);
      end
    end
    else if Assigned(ExceptionManager) then
      ExceptionManager.HaveGlobalExeption('ParamsToFormData', ETelegramDataConvert.Create('Check parameter type ' + LParameter.Value.ToString))
    else
      raise ETelegramDataConvert.Create('Check parameter type ' + LParameter.Value.ToString);
  end;
end;

function TTelegramBot.SendDataToServer(const Method: string; Parameters: TDictionary<string, TValue>): string;
var
  LHttp: THTTPClient;
  LHttpResponse: IHTTPResponse;
  LFullUrl: string;
  LParamToDate: TMultipartFormData;
begin
  LHttp := THTTPClient.Create;
  LParamToDate := nil;
  try
    LHttp.ProxySettings := FProxySettings;
    LFullUrl := 'https://api.telegram.org/bot' + FToken + '/' + Method;
    try
      if Assigned(Parameters) then
      begin
        LParamToDate := ParamsToFormData(Parameters);
        LHttpResponse := LHttp.Post(LFullUrl, LParamToDate);
      end
      else
        LHttpResponse := LHttp.Get(LFullUrl);
      Result := LHttpResponse.ContentAsString(TEncoding.UTF8);
    except
      on E: Exception do
      begin
        if Assigned(ExceptionManager) then
          ExceptionManager.HaveGlobalExeption('SendDataToServer', E)
        else
          raise E;
        Result := string.Empty;
      end;
    end;
  finally
    FreeAndNil(LParamToDate);
    FreeAndNil(LHttp);
  end;
end;

procedure TTelegramBot.SetToken(const Value: string);
begin
  FToken := Value;
end;



// procedure TTelegramBot.ErrorHandlerGeneral(AException: Exception);
// begin
// if Assigned(OnReceiveGeneralError) then
// TThread.Synchronize(nil,
// procedure
// begin
// OnReceiveGeneralError(Self, AException)
// end)
// else
// raise Exception.Create(AException.Message);
// if Assigned(AException) then
// FreeAndNil(AException);
// end;

{$ENDREGION}
{$REGION 'Getting updates'}

function TTelegramBot.SetWebhook(const Url: string; Certificate: TtgFileToSend; MaxConnections: Int64; AllowedUpdates: TAllowedUpdates): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('url', Url);
    Parameters.Add('certificate', Certificate);
    Parameters.Add('max_connections', MaxConnections);
    Parameters.Add('allowed_updates', AllowedUpdates.ToString);
    Result := ExecuteMethod('setWebhook', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetJSONFromMethod(const Method: string; Parameters: TDictionary<string, TValue>): TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(RequestAPI(Method, Parameters));
end;

function TTelegramBot.GetJSONArrayFromMethod(const Method: string; Parameters: TDictionary<string, TValue>): TJSONArray;
begin
  Result := TJSONObject.ParseJSONValue(RequestAPI(Method, Parameters)) as TJSONArray;
end;

function TTelegramBot.ExecuteMethod(const Method: string; Parameters: TDictionary<string, TValue>): Boolean;
var
  LJson: TJSONValue;
begin
  LJson := GetJSONFromMethod(Method, Parameters);
  try
    Result := LJson is TJSONTrue;
  finally
    LJson.Free;
  end;
end;

function TTelegramBot.GetValueFromMethod(const Method: string; Parameters: TDictionary<string, TValue>): string;
var
  LJson: TJSONValue;
begin
  LJson := GetJSONFromMethod(Method, Parameters);
  try
    Result := LJson.Value;
  finally
    LJson.Free;
  end;
end;

function TTelegramBot.stopMessageLiveLocation(ChatId: TValue; MessageId: Int64; ReplyMarkup: IReplyMarkup): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));

    Result := ExecuteMethod('stopMessageLiveLocation', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.stopMessageLiveLocation(const InlineMessageId: string; ReplyMarkup: IReplyMarkup): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('inline_message_id', InlineMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));

    Result := ExecuteMethod('stopMessageLiveLocation', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetWebhookInfo: ItgWebhookInfo;
var
  LJson: TJSONValue;
begin
  LJson := GetJSONFromMethod('getWebhookInfo', nil);
  try
    Result := TBaseJsonClass(TtgWebhookInfo).Create(LJson.ToJSON) as TtgWebhookInfo;
  finally
    LJson.Free;
  end;
end;

function TTelegramBot.CreateBaseJSONClass<T>(const Params: string): T;
var
  c: TRttiContext;
  rt: TRttiType;
  v: TValue;
begin
  // Invoke RTTI
  c := TRttiContext.Create;
  rt := c.GetType(T);
  v := rt.GetMethod('Create').Invoke(rt.AsInstance.MetaclassType, [Params]);
  Result := T(v.AsObject);
  // free RttiContext record
  c.Free;
end;

// function TTelegramBot.GetArrayFromMethod<T, TI>(BJClass:TBaseJsonClass; const Method: string; Parameters: TDictionary<string, TValue>): TArray<TI>;
// var LJsonArr: TJSONArray;
// I: Integer;
// begin
// LJsonArr := GetJSONArrayFromMethod('getUpdates', Parameters);
// try
// SetLength(Result, LJsonArr.Count);
// for I := 0 to High(Result) do
// begin
// Result[I] := BJClass.Create(LJsonArr.Items[I].ToJSON);
// end;
// finally
// LJsonArr.Free;
// end;
// end;

function TTelegramBot.GetUpdates(const Offset, Limit, Timeout: Int64; AllowedUpdates: TAllowedUpdates): TArray<ItgUpdate>;
var
  Parameters: TDictionary<string, TValue>;
  LJson: TJSONArray;
  I: Integer;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('offset', Offset);
    Parameters.Add('limit', Limit);
    Parameters.Add('timeout', Timeout);
    Parameters.Add('allowed_updates', AllowedUpdates.ToString);
    LJson := TJSONObject.ParseJSONValue(RequestAPI('getUpdates', Parameters)) as TJSONArray;
    try
      SetLength(Result, LJson.Count);
      for I := 0 to High(Result) do
        Result[I] := TtgUpdate.Create(LJson.Items[I].ToJSON);
    finally
      LJson.Free;
    end;
    // Result:=GetArrayFromMethod<ItgUpdate>('getUpdates', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.DeleteWebhook: Boolean;
begin
  Result := ExecuteMethod('deleteWebhook', nil);
end;

{$ENDREGION}
{$REGION 'Basic methods'}

function TTelegramBot.UnbanChatMember(ChatId: TValue; UserId: Int64): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('user_id', UserId);
    Result := ExecuteMethod('unbanChatMember', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendLocation(ChatId: TValue; Location: TtgLocation; LivePeriod: Int64; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('latitude', Location.Latitude);
    Parameters.Add('longitude', Location.Longitude);
    Parameters.Add('live_period', LivePeriod);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendLocation', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendPhoto(ChatId, Photo: TValue; const Caption: string; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('photo', Photo);
    Parameters.Add('caption', Caption);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendPhoto', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendSticker(ChatId, Sticker: TValue; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('sticker', Sticker);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendSticker', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendMessage(const ChatId: TValue; const Text: string; ParseMode: TtgParseMode; DisableWebPagePreview, DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('text', Text);
    Parameters.Add('parse_mode', ParseMode.ToString);
    Parameters.Add('disable_web_page_preview', DisableWebPagePreview);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendMessage', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendVenue(ChatId: TValue; Venue: TtgVenue; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('latitude', Venue.Location.Latitude);
    Parameters.Add('longitude', Venue.Location.Longitude);
    Parameters.Add('title', Venue.title);
    Parameters.Add('address', Venue.Address);
    Parameters.Add('foursquare_id', Venue.FoursquareId);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendVenue', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendVideo(ChatId, Video: TValue; const Caption: string; Duration, Width, Height: Int64; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('video', Video);
    Parameters.Add('duration', Duration);
    Parameters.Add('width', Width);
    Parameters.Add('height', Height);
    Parameters.Add('caption', Caption);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendVideo', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendVideoNote(ChatId, VideoNote: TValue; Duration, Length: Int64; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  LParameters: TDictionary<string, TValue>;
begin
  LParameters := TDictionary<string, TValue>.Create;
  try
    LParameters.Add('chat_id', ChatId);
    LParameters.Add('video_note', VideoNote);
    LParameters.Add('duration', Duration);
    LParameters.Add('length', Length);
    LParameters.Add('disable_notification', DisableNotification);
    LParameters.Add('reply_to_message_id', ReplyToMessageId);
    LParameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendVoice', LParameters));
  finally
    LParameters.Free;
  end;
end;

function TTelegramBot.SendVoice(ChatId, Voice: TValue; const Caption: string; Duration: Int64; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('voice', Voice);
    Parameters.Add('caption', Caption);
    Parameters.Add('duration', Duration);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendVoice', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendAudio(ChatId, Audio: TValue; const Caption: string; Duration: Int64; const Performer: string; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('audio', Audio);
    Parameters.Add('duration', Duration);
    Parameters.Add('performer', Performer);
    Parameters.Add('caption', Caption);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendAudio', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendChatAction(ChatId: TValue; const Action: TtgSendChatAction): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('action', Action.ToString);
    Result := ExecuteMethod('sendChatAction', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendContact(ChatId: TValue; Contact: TtgContact; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('phone_number', Contact.PhoneNumber);
    Parameters.Add('first_name', Contact.FirstName);
    Parameters.Add('last_name', Contact.LastName);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendContact', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendDocument(ChatId, Document: TValue; const Caption: string; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('document', Document);
    Parameters.Add('caption', Caption);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendDocument', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.KickChatMember(ChatId: TValue; UserId, UntilDate: Int64): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('user_id', UserId);
    Parameters.Add('until_date', UntilDate);
    Result := ExecuteMethod('kickChatMember', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.LeaveChat(ChatId: TValue): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Result := ExecuteMethod('leaveChat', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetUserProfilePhotos(ChatId: TValue; Offset, Limit: Int64): TtgUserProfilePhotos;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('offset', Offset);
    Parameters.Add('limit', Limit);
    Result := TtgUserProfilePhotos.Create(RequestAPI('getUserProfilePhotos', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetMe: ItgUser;
begin
  Result := TtgUser.Create(RequestAPI('getMe', nil));
end;

function TTelegramBot.GetToken: string;
begin
  Result := FToken;
end;

function TTelegramBot.getStickerSet(const Name: string): TtgStickerSet;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('name', Name);
    Result := TtgStickerSet.Create(RequestAPI('getStickerSet', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.ForwardMessage(ChatId, FromChatId: TValue; MessageId: Int64; DisableNotification: Boolean): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('from_chat_id', FromChatId);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('message_id', MessageId);
    Result := TTgMessage.Create(RequestAPI('forwardMessage', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetChat(const ChatId: TValue): ItgChat;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Result := TtgChat.Create(RequestAPI('getChat', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetChatAdministrators(const ChatId: TValue): TArray<ItgChatMember>;
var
  Parameters: TDictionary<string, TValue>;
  LJson: TJSONArray;
  I: Integer;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);

    LJson := TJSONObject.ParseJSONValue(RequestAPI('getChatAdministrators', Parameters)) as TJSONArray;
    try
      SetLength(Result, LJson.Count);
      for I := 0 to High(Result) do
        Result[I] := TtgChatMember.Create(LJson.Items[I].ToJSON);
    finally
      LJson.Free;
    end;
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetChatMember(ChatId: TValue; UserId: Int64): ItgChatMember;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('user_id', UserId);
    Result := TtgChatMember.Create(RequestAPI('getChatMember', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetChatMembersCount(const ChatId: TValue): Int64;
var
  Parameters: TDictionary<string, TValue>;
  LJson: TJSONValue;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    LJson := TJSONObject.ParseJSONValue(RequestAPI('getChatMembersCount', Parameters));
    try
      if not LJson.TryGetValue<Int64>(Result) then
        Result := 0;
    finally
      LJson.Free;
    end;
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetExceptionManager: ItgExceptionHandler;
begin
  Result := FExceptionManager;
end;

function TTelegramBot.GetFile(const FileId: string): ItgFile;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('file_id', FileId);
    Result := TtgFile.Create(RequestAPI('getFile', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.addStickerToSet(UserId: Int64; const Name: string; PngSticker: TValue; const Emojis: string; MaskPosition: TtgMaskPosition): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('user_id', UserId);
    Parameters.Add('name', Name);
    Parameters.Add('png_sticker', PngSticker);
    Parameters.Add('emojis', Emojis);
    Parameters.Add('mask_position', MaskPosition);

    Result := ExecuteMethod('addStickerToSet', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.AnswerCallbackQuery(const CallbackQueryId, Text: string; ShowAlert: Boolean; const Url: string; CacheTime: Int64): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('callback_query_id', CallbackQueryId);
    Parameters.Add('text', Text);
    Parameters.Add('show_alert', ShowAlert);
    Parameters.Add('url', Url);
    Parameters.Add('cache_time', CacheTime);

    Result := ExecuteMethod('answerCallbackQuery', Parameters);
  finally
    Parameters.Free;
  end;
end;
{$ENDREGION}
{$REGION 'Updating messages'}

function TTelegramBot.EditMessageText(const InlineMessageId, Text: string; ParseMode: TtgParseMode; DisableWebPagePreview: Boolean; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('inline_message_id', InlineMessageId);
    Parameters.Add('text', Text);
    Parameters.Add('parse_mode', ParseMode.ToString);
    Parameters.Add('disable_web_page_preview', DisableWebPagePreview);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('editMessageText', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.EditMessageText(ChatId: TValue; MessageId: Int64; const Text: string; ParseMode: TtgParseMode; DisableWebPagePreview: Boolean; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('text', Text);
    Parameters.Add('parse_mode', ParseMode.ToString);
    Parameters.Add('disable_web_page_preview', DisableWebPagePreview);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('editMessageText', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.DeleteMessage(ChatId: TValue; MessageId: Int64): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);

    Result := ExecuteMethod('deleteMessage', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.deleteStickerFromSet(const Sticker: string): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('sticker', Sticker);
    Result := ExecuteMethod('deleteStickerFromSet', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.EditMessageCaption(ChatId: TValue; MessageId: Int64; const Caption: string; ReplyMarkup: IReplyMarkup): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('caption', Caption);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));

    Result := ExecuteMethod('editMessageCaption', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.EditMessageCaption(const InlineMessageId, Caption: string; ReplyMarkup: IReplyMarkup): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('caption', Caption);
    Parameters.Add('inline_message_id', InlineMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));

    Result := ExecuteMethod('editMessageCaption', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.editMessageLiveLocation(ChatId: TValue; MessageId: Int64; Location: TtgLocation; ReplyMarkup: IReplyMarkup): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('latitude', Location.Latitude);
    Parameters.Add('longitude', Location.Longitude);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));

    Result := ExecuteMethod('editMessageLiveLocation', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.editMessageLiveLocation(const InlineMessageId: string; Location: TtgLocation; ReplyMarkup: IReplyMarkup): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('inline_message_id', InlineMessageId);
    Parameters.Add('latitude', Location.Latitude);
    Parameters.Add('longitude', Location.Longitude);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := ExecuteMethod('editMessageLiveLocation', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.EditMessageReplyMarkup(ChatId: TValue; MessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('editMessageReplyMarkup', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.EditMessageReplyMarkup(const InlineMessageId: string; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('inline_message_id', InlineMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('editMessageReplyMarkup', Parameters));
  finally
    Parameters.Free;
  end;
end;

{$ENDREGION}
{$REGION 'Inline mode'}

function TTelegramBot.AnswerInlineQuery(const InlineQueryId: string; Results: TArray<TtgInlineQueryResult>; CacheTime: Int64; IsPersonal: Boolean; const NextOffset, SwitchPmText, SwitchPmParameter: string): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('inline_query_id', InlineQueryId);
    Parameters.Add('results', TJsonUtils.ArrayToJString<TtgInlineQueryResult>(Results));
    Parameters.Add('cache_time', CacheTime);
    Parameters.Add('is_personal', IsPersonal);
    Parameters.Add('next_offset', NextOffset);
    Parameters.Add('switch_pm_text', SwitchPmText);
    Parameters.Add('switch_pm_parameter', SwitchPmParameter);

    Result := ExecuteMethod('answerInlineQuery', Parameters);
  finally
    Parameters.Free;
  end;
end;
{$ENDREGION}
{$REGION 'Payments'}

function TTelegramBot.SendInvoice(ChatId: Int64; const title: string; const Description: string; const Payload: string; const ProviderToken: string; const StartParameter: string; const Currency: string; Prices: TArray<TtgLabeledPrice>; const PhotoUrl: string; PhotoSize: Int64; PhotoWidth: Int64; PhotoHeight: Int64; NeedName: Boolean; NeedPhoneNumber: Boolean; NeedEmail: Boolean; NeedShippingAddress: Boolean; IsFlexible: Boolean; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  LParameters: TDictionary<string, TValue>;
begin
  LParameters := TDictionary<string, TValue>.Create;
  try
    LParameters.Add('chat_id', ChatId);
    LParameters.Add('title', title);
    LParameters.Add('description', Description);
    LParameters.Add('payload', Payload);
    LParameters.Add('provider_token', ProviderToken);
    LParameters.Add('start_parameter', StartParameter);
    LParameters.Add('currency', Currency);
    LParameters.Add('prices', TJsonUtils.ArrayToJString<TtgLabeledPrice>(Prices));
    LParameters.Add('photo_url', PhotoUrl);
    LParameters.Add('photo_size', PhotoSize);
    LParameters.Add('photo_width', PhotoWidth);
    LParameters.Add('photo_height', PhotoHeight);
    LParameters.Add('need_name', NeedName);
    LParameters.Add('need_phone_number', NeedPhoneNumber);
    LParameters.Add('need_email', NeedEmail);
    LParameters.Add('need_shipping_address', NeedShippingAddress);
    LParameters.Add('is_flexible', IsFlexible);
    LParameters.Add('disable_notification', DisableNotification);
    LParameters.Add('reply_to_message_id', ReplyToMessageId);
    LParameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendInvoice', LParameters));
  finally
    LParameters.Free;
  end;
end;

function TTelegramBot.AnswerPreCheckoutQuery(const PreCheckoutQueryId: string; Ok: Boolean; const ErrorMessage: string): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('Pre_checkout_query_id', PreCheckoutQueryId);
    Parameters.Add('Ok', Ok);
    Parameters.Add('Error_message', ErrorMessage);

    Result := ExecuteMethod('AnswerPreCheckoutQuery', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.AnswerShippingQuery(const ShippingQueryId: string; Ok: Boolean; ShippingOptions: TArray<TtgShippingOption>; const ErrorMessage: string): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('Shipping_query_id', ShippingQueryId);
    Parameters.Add('Ok', Ok);
    Parameters.Add('Shipping_options', TJsonUtils.ArrayToJString<TtgShippingOption>(ShippingOptions));
    Parameters.Add('Error_message', ErrorMessage);

    Result := ExecuteMethod('answerShippingQuery', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.createNewStickerSet(UserId: Int64; const Name, title: string; PngSticker: TValue; const Emojis: string; ContainsMasks: Boolean; MaskPosition: TtgMaskPosition): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('user_id', UserId);
    Parameters.Add('name', Name);
    Parameters.Add('title', title);
    Parameters.Add('png_sticker', PngSticker);
    Parameters.Add('emojis', Emojis);
    Parameters.Add('contains_masks', ContainsMasks);
    Parameters.Add('mask_position', MaskPosition);

    Result := ExecuteMethod('createNewStickerSet', Parameters);
  finally
    Parameters.Free;
  end;

end;

{$ENDREGION}
{$REGION 'Games'}

function TTelegramBot.SetGameScore(UserId, Score: Int64; Force, DisableEditMessage: Boolean; ChatId, MessageId: Int64; const InlineMessageId: string): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('user_id', UserId);
    Parameters.Add('score', Score);
    Parameters.Add('force', Force);
    Parameters.Add('disable_edit_message', DisableEditMessage);
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('inline_message_id', InlineMessageId);
    Result := TTgMessage.Create(RequestAPI('setGameScore', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.setStickerPositionInSet(const Sticker: string; Position: Int64): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('sticker', Sticker);
    Parameters.Add('position', Position);

    Result := ExecuteMethod('setStickerPositionInSet', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendGame(ChatId: Int64; const GameShortName: string; DisableNotification: Boolean; ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('game_short_name', GameShortName);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := TTgMessage.Create(RequestAPI('sendGame', Parameters));
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetGameHighScores(UserId, ChatId, MessageId: Int64; const InlineMessageId: string): TArray<ItgGameHighScore>;
var
  Parameters: TDictionary<string, TValue>;
  LJson: TJSONArray;
  I: Integer;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('user_id', UserId);
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('inline_message_id', InlineMessageId);
    LJson := TJSONObject.ParseJSONValue(RequestAPI('getGameHighScores', Parameters)) as TJSONArray;
    try
      SetLength(Result, LJson.Count);
      for I := 0 to High(Result) do
        Result[I] := TtgGameHighScore.Create(LJson.Items[I].ToJSON);
    finally
      LJson.Free;
    end;
  finally
    Parameters.Free;
  end;
end;

{$ENDREGION}
{$REGION 'Manage groups and channels'}

function TTelegramBot.DeleteChatPhoto(ChatId: TValue): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);

    Result := ExecuteMethod('deleteChatPhoto', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.deleteChatStickerSet(ChatId: TValue): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);

    Result := ExecuteMethod('deleteChatStickerSet', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.ExportChatInviteLink(ChatId: TValue): string;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);

    Result := GetValueFromMethod('exportChatInviteLink', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.PinChatMessage(ChatId: TValue; MessageId: Int64; DisableNotification: Boolean): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('disable_notification', DisableNotification);

    Result := ExecuteMethod('pinChatMessage', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SetChatDescription(ChatId: TValue; const Description: string): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('description', Description);

    Result := ExecuteMethod('setChatDescription', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SetChatPhoto(ChatId: TValue; Photo: TtgFileToSend): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('photo', Photo);

    Result := ExecuteMethod('setChatPhoto', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.setChatStickerSet(ChatId: TValue; const StickerSetName: string): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('sticker_set_name', StickerSetName);

    Result := ExecuteMethod('setChatStickerSet', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SetChatTitle(ChatId: TValue; const title: string): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('title', title);

    Result := ExecuteMethod('setChatTitle', Parameters);
  finally
    Parameters.Free;
  end;
end;

procedure TTelegramBot.SetExceptionManager(const Value: ItgExceptionHandler);
begin
  FExceptionManager := Value;
end;

function TTelegramBot.UnpinChatMessage(ChatId: TValue): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);

    Result := ExecuteMethod('unpinChatMessage', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.uploadStickerFile(UserId: Int64; PngSticker: TtgFileToSend): ItgFile;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('user_id', UserId);
    Parameters.Add('png_sticker', PngSticker);
    Result := TtgFile.Create(RequestAPI('uploadStickerFile', Parameters));
  finally
    Parameters.Free;
  end;
end;

{$ENDREGION}
{$REGION 'Manage users and admins'}

function TTelegramBot.PromoteChatMember(ChatId: TValue; UserId: Int64; CanChangeInfo, CanPostMessages, CanEditMessages, CanDeleteMessages, CanInviteUsers, CanRestrictMembers, CanPinMessages, CanPromoteMembers: Boolean): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('user_id', UserId);
    Parameters.Add('can_change_info', CanChangeInfo);
    Parameters.Add('can_post_messages', CanPostMessages);
    Parameters.Add('can_edit_messages', CanEditMessages);
    Parameters.Add('can_delete_messages', CanDeleteMessages);
    Parameters.Add('can_invite_users', CanInviteUsers);
    Parameters.Add('can_restrict_members', CanRestrictMembers);
    Parameters.Add('can_pin_messages', CanPinMessages);
    Parameters.Add('can_promote_members', CanPromoteMembers);

    Result := ExecuteMethod('promoteChatMember', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.RestrictChatMember(ChatId: TValue; UserId, UntilDate: Int64; CanSendMessages, CanSendMediaMessages, CanSendOtherMessages, CanAddWebPagePreviews: Boolean): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('user_id', UserId);
    Parameters.Add('until_date', UntilDate);
    Parameters.Add('can_send_messages', CanSendMessages);
    Parameters.Add('can_send_media_messages', CanSendMediaMessages);
    Parameters.Add('can_send_other_messages', CanSendOtherMessages);
    Parameters.Add('can_add_web_page_previews', CanAddWebPagePreviews);

    Result := ExecuteMethod('restrictChatMember', Parameters);
  finally
    Parameters.Free;
  end;
end;
{$ENDREGION}

end.

