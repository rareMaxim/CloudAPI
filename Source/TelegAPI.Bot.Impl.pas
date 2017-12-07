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
  TelegAPI.CoreAPI.Parameter,
  TelegAPI.Bot,
  TelegAPI.Types,
  TelegAPI.Types.Impl,
  TelegAPI.Types.Enums,
  TelegAPI.Types.ReplyMarkups,
  TelegAPI.Types.InlineQueryResults,
  TelegAPI.Exceptions,
  TelegAPI.Utils.Json,
  System.Json;

type
  TtgOnReceiveRawData = procedure(ASender: TObject; const AData: string) of object;

  TTelegramBot = class(TtgAbstractComponent, ITelegramBot)
  private
    FToken: string;
    FProxySettings: TProxySettings;
    FOnRawData: TtgOnReceiveRawData;
    FExceptionManager: ItgExceptionHandler;
    function GetToken: string;
    procedure SetToken(const Value: string);
    // Returns TJSONValue as method request result
    function GetJSONFromMethod(const AValue: string): TJSONValue;
    // Returns TJSONArray as method request result
    function GetJSONArrayFromMethod(const AValue: string): TJSONArray;
    // Returns true when given Method executed successfully
    function ExtractBool(const AValue: string): Boolean;
    // Returns response JSON from server as result of request
    function GetArrayFromMethod<TI: IInterface>(const TgClass: TBaseJsonClass; const AValue: string): TArray<TI>;
    function GetValueFromMethod(const AValue: string): string;
    function GetExceptionManager: ItgExceptionHandler;
    procedure SetExceptionManager(const Value: ItgExceptionHandler);
  protected
    function RequestAPI(const Method: string; const Parameters: TArray<TtgApiParameter>): string;
    function ApiTest(const ARequest: string; const Parameters: TArray<TtgApiParameter> = nil): string;
  public
{$REGION 'Getting updates'}
    function GetUpdates(//
      const Offset: Int64 = 0; //
      const Limit: Int64 = 100; //
      const Timeout: Int64 = 0; //
      const AllowedUpdates: TAllowedUpdates = UPDATES_ALLOWED_ALL): TArray<ItgUpdate>;
    function SetWebhook(//
      const Url: string; //
      const Certificate: TtgFileToSend = nil; //
      const MaxConnections: Int64 = 40; //
      const AllowedUpdates: TAllowedUpdates = UPDATES_ALLOWED_ALL): Boolean;
    function DeleteWebhook: Boolean;
    function GetWebhookInfo: ItgWebhookInfo;
{$ENDREGION}

{$REGION 'Basic methods'}
    function GetMe: ItgUser;
    function SendMessage(//
      const ChatId: TValue; //
      const Text: string; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const DisableWebPagePreview: Boolean = False; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function ForwardMessage(//
      const ChatId, FromChatId: TValue; //
      const MessageId: Int64; //
      const DisableNotification: Boolean = False): ITgMessage;
    function SendPhoto(//
      const ChatId: TValue; //
      const Photo: TtgFileToSend; //
      const Caption: string = ''; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendAudio(//
      const ChatId: TValue; //
      const Audio: TtgFileToSend; //
      const Caption: string = ''; //
      const Duration: Int64 = 0; //
      const Performer: string = ''; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendDocument(//
      const ChatId: TValue; //
      const Document: TtgFileToSend; //
      const Caption: string = ''; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVideo(//
      const ChatId: TValue; //
      const Video: TtgFileToSend; //
      const Caption: string = ''; //
      const Duration: Int64 = 0; //
      const Width: Int64 = 0; //
      const Height: Int64 = 0; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVoice(//
      const ChatId: TValue; //
      const Voice: TtgFileToSend; //
      const Caption: string = ''; //
      const Duration: Int64 = 0; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVideoNote(//
      const ChatId: TValue; //
      const VideoNote: TtgFileToSend; //
      const Duration: Int64 = 0; //
      const Length: Int64 = 0; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendLocation(//
      const ChatId: TValue; //
      const Location: TtgLocation; //
      const LivePeriod: Int64 = 0; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVenue(//
      const ChatId: TValue; //
      const Venue: TtgVenue; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendContact(//
      const ChatId: TValue; //
      const Contact: TtgContact; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendChatAction(//
      const ChatId: TValue; //
      const Action: TtgSendChatAction): Boolean;
    function GetUserProfilePhotos(//
      const ChatId: TValue; //
      const Offset: Int64; //
      const Limit: Int64 = 100): ItgUserProfilePhotos;
    function GetFile(const FileId: string): ItgFile;
    function KickChatMember(//
      const ChatId: TValue; //
      const UserId: Int64; //
      const UntilDate: Int64 = 0): Boolean;
    function UnbanChatMember(//
      const ChatId: TValue; //
      const UserId: Int64): Boolean;
    function LeaveChat(const ChatId: TValue): Boolean;
    function GetChat(const ChatId: TValue): ItgChat;
    function GetChatAdministrators(const ChatId: TValue): TArray<ItgChatMember>;
    function GetChatMembersCount(const ChatId: TValue): Int64;
    function GetChatMember(//
      const ChatId: TValue; //
      const UserId: Int64): ItgChatMember;
    function AnswerCallbackQuery(//
      const CallbackQueryId: string; //
      const Text: string = ''; //
      const ShowAlert: Boolean = False; //
      const Url: string = ''; //
      const CacheTime: Int64 = 0): Boolean;
{$ENDREGION}

{$REGION 'Updating messages'}
    function EditMessageText(//
      const ChatId: TValue; //
      const MessageId: Int64; //
      const Text: string; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const DisableWebPagePreview: Boolean = False; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function EditMessageText(//
      const InlineMessageId: string; //
      const Text: string; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const DisableWebPagePreview: Boolean = False; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function EditMessageCaption(//
      const ChatId: TValue; //
      const MessageId: Int64; //
      const Caption: string; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function EditMessageCaption(//
      const InlineMessageId: string; //
      const Caption: string; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function editMessageLiveLocation(//
      const ChatId: TValue; //
      const MessageId: Int64; //
      const Location: TtgLocation; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function editMessageLiveLocation(//
      const InlineMessageId: string; //
      const Location: TtgLocation; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function stopMessageLiveLocation(//
      const ChatId: TValue; //
      const MessageId: Int64; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function stopMessageLiveLocation(//
      const InlineMessageId: string; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function EditMessageReplyMarkup(//
      const ChatId: TValue; //
      const MessageId: Int64; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function EditMessageReplyMarkup(//
      const InlineMessageId: string; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function DeleteMessage(//
      const ChatId: TValue; //
      const MessageId: Int64): Boolean;
{$ENDREGION}

{$REGION 'Inline mode'}
    function AnswerInlineQuery(//
      const InlineQueryId: string; //
      const Results: TArray<TtgInlineQueryResult>; //
      const CacheTime: Int64 = 300; //
      const IsPersonal: Boolean = False; //
      const NextOffset: string = ''; //
      const SwitchPmText: string = ''; //
      const SwitchPmParameter: string = ''): Boolean;
{$ENDREGION}

{$REGION 'Payments'}
    function SendInvoice(//
      const ChatId: Int64; //
      const Title: string; //
      const Description: string; //
      const Payload: string; //
      const ProviderToken: string; //
      const StartParameter: string; //
      const Currency: string; //
      const Prices: TArray<TtgLabeledPrice>; //
      const ProviderData: string = ''; //
      const PhotoUrl: string = ''; //
      const PhotoSize: Int64 = 0; //
      const PhotoWidth: Int64 = 0; //
      const PhotoHeight: Int64 = 0; //
      const NeedName: Boolean = False; //
      const NeedPhoneNumber: Boolean = False; //
      const NeedEmail: Boolean = False; //
      const NeedShippingAddress: Boolean = False; //
      const IsFlexible: Boolean = False; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function AnswerShippingQueryGood(//
      const ShippingQueryId: string; //
      const ShippingOptions: TArray<TtgShippingOption>): Boolean;
    function AnswerShippingQueryBad(//
      const ShippingQueryId: string; //
      const ErrorMessage: string): Boolean;
    function AnswerPreCheckoutQueryGood(//
      const PreCheckoutQueryId: string): Boolean;
    function AnswerPreCheckoutQueryBad(//
      const PreCheckoutQueryId: string; //
      const ErrorMessage: string): Boolean;
{$ENDREGION}

{$REGION 'Games'}
    function SendGame(//
      const ChatId: Int64; //
      const GameShortName: string; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SetGameScore(//
      const UserId: Int64; //
      const Score: Int64; //
      const InlineMessageId: string; //
      const Force: Boolean = False; //
      const DisableEditMessage: Boolean = False): ITgMessage; overload;
    function SetGameScore(//
      const UserId: Int64; //
      const Score: Int64; //
      const ChatId: Int64; //
      const MessageId: Int64; //
      const Force: Boolean = False; //
      const DisableEditMessage: Boolean = False): ITgMessage; overload;
    function GetGameHighScores(//
      const UserId: Int64; //
      const InlineMessageId: string = ''): TArray<ItgGameHighScore>; overload;
    function GetGameHighScores(//
      const UserId: Int64; //
      const ChatId: Int64 = 0; //
      const MessageId: Int64 = 0): TArray<ItgGameHighScore>; overload;
{$ENDREGION}

{$REGION 'Manage groups and channels'}
    function DeleteChatPhoto(const ChatId: TValue): Boolean;
    function ExportChatInviteLink(const ChatId: TValue): string;
    function PinChatMessage(//
      const ChatId: TValue; //
      const MessageId: Int64; //
      const DisableNotification: Boolean = False): Boolean;
    function SetChatDescription(const ChatId: TValue; const Description: string): Boolean;
    function SetChatPhoto(const ChatId: TValue; const Photo: TtgFileToSend): Boolean;
    function SetChatTitle(const ChatId: TValue; const Title: string): Boolean;
    function UnpinChatMessage(const ChatId: TValue): Boolean;
{$ENDREGION}

{$REGION 'Manage users and admins'}
    function RestrictChatMember(//
      const ChatId: TValue; //
      const UserId: Int64; //
      const UntilDate: Int64 = 0; //
      const CanSendMessages: Boolean = False; //
      const CanSendMediaMessages: Boolean = False; //
      const CanSendOtherMessages: Boolean = False; //
      const CanAddWebPagePreviews: Boolean = False): Boolean;
    function PromoteChatMember(//
      const ChatId: TValue; //
      const UserId: Int64; //
      const CanChangeInfo: Boolean = False; //
      const CanPostMessages: Boolean = False; //
      const CanEditMessages: Boolean = False; //
      const CanDeleteMessages: Boolean = False; //
      const CanInviteUsers: Boolean = False; //
      const CanRestrictMembers: Boolean = False; //
      const CanPinMessages: Boolean = False; //
      const CanPromoteMembers: Boolean = False): Boolean;
{$ENDREGION}

{$REGION 'Strickers'}
    function SendSticker(//
      const ChatId: TValue; //
      const Sticker: TValue; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function getStickerSet(const Name: string): TtgStickerSet;
    function uploadStickerFile(const UserId: Int64; const PngSticker: TtgFileToSend): ItgFile;
    function createNewStickerSet(//
      const UserId: Int64; //
      const Name, Title: string; //
      const PngSticker: TValue; //
      const Emojis: string; //
      const ContainsMasks: Boolean = False; //
      const MaskPosition: TtgMaskPosition = nil): Boolean;
    function addStickerToSet(//
      const UserId: Int64; //
      const Name: string; //
      const PngSticker: TValue; //
      const Emojis: string; //
      const MaskPosition: TtgMaskPosition = nil): Boolean;
    function setStickerPositionInSet(const Sticker: string; const Position: Int64): Boolean;
    function deleteStickerFromSet(const Sticker: string): Boolean;
    function setChatStickerSet(const ChatId: TValue; const StickerSetName: string): Boolean;
    function deleteChatStickerSet(const ChatId: TValue): Boolean;
    function sendMediaGroup(//
      const ChatId: TValue; //
      const AMedia: TArray<TtgInputMedia>; //
      const ADisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0): TArray<ITgMessage>;
{$ENDREGION}
  published
{$REGION 'Property|Свойства'}
    property ProxySettings: TProxySettings read FProxySettings write FProxySettings;
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
  FMX.Types,
  TelegAPI.Helpers,
  TelegAPI.CoreAPI.Request;
{ TTelegramBot }
{$REGION 'Core'}

function TTelegramBot.RequestAPI(const Method: string; const Parameters: TArray<TtgApiParameter>): string;
var
  LTgRequest: TtgApiRequest;
begin
  LTgRequest := TtgApiRequest.Create(Self, Method);
  try
    LTgRequest.OnSend :=
      procedure(Url, Data: string)
      begin
        Log.d('%s - %s', [Url, Data]);
      end;
    LTgRequest.OnReceive :=
      procedure(Data: string)
      begin
        Log.d('%s', [Data]);
      end;
    LTgRequest.Parameters.AddRange(Parameters);
    Result := LTgRequest.Execute.ContentAsString(TEncoding.UTF8);
    if Assigned(OnReceiveRawData) then
      OnReceiveRawData(Self, Result);
    if Result.IsEmpty then
      ExceptionManager.HaveGlobalExeption('RequestAPI', ETelegramUnknownData.Create('Can''t parse response'))
    else
      Result := ApiTest(Result, Parameters);
  finally
    LTgRequest.Free;
  end;
end;

function TTelegramBot.ApiTest(const ARequest: string; const Parameters: TArray<TtgApiParameter>): string;
var
  FJSON: TJSONObject;
begin
  Result := '';
  if ARequest.IsEmpty then
    Exit;
  FJSON := TJSONObject.ParseJSONValue(ARequest) as TJSONObject;
  try
    if FJSON.GetValue('ok') is TJSONFalse then
      ExceptionManager.HaveApiExeption('TTelegramBot.ApiTest', EApiRequestException.Create(ARequest, 0))
    else
      Result := FJSON.GetValue('result').ToJSON;
  finally
    FJSON.Free;
  end;
end;

function TTelegramBot.GetJSONFromMethod(const AValue: string): TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(AValue);
end;

function TTelegramBot.GetJSONArrayFromMethod(const AValue: string): TJSONArray;
begin
  Result := TJSONObject.ParseJSONValue(AValue) as TJSONArray;
end;

function TTelegramBot.ExtractBool(const AValue: string): Boolean;
var
  LJson: TJSONValue;
begin
  LJson := TJSONObject.ParseJSONValue(AValue);
  try
    Result := LJson is TJSONTrue;
  finally
    LJson.Free;
  end;
end;

function TTelegramBot.GetValueFromMethod(const AValue: string): string;
var
  LJson: TJSONValue;
begin
  LJson := GetJSONFromMethod(AValue);
  try
    Result := LJson.Value;
  finally
    LJson.Free;
  end;
end;

function TTelegramBot.GetArrayFromMethod<TI>(const TgClass: TBaseJsonClass; const AValue: string): TArray<TI>;
var
  LJsonArr: TJSONArray;
  I: Integer;
  GUID: TGUID;
begin
  // stage 1: type checking
  // cache value fot further use
  GUID := GetTypeData(TypeInfo(TI))^.GUID;
  // check for TI interface support
  if TgClass.GetInterfaceEntry(GUID) = nil then
    raise Exception.Create('GetArrayFromMethod: unsupported interface for ' + TgClass.ClassName);
  // stage 2: proceed data
  LJsonArr := GetJSONArrayFromMethod(AValue);
  if (not Assigned(LJsonArr)) or LJsonArr.Null then
    Exit(nil);
  try
    SetLength(Result, LJsonArr.Count);
    for I := 0 to High(Result) do
      TgClass.GetTgClass.Create(LJsonArr.Items[I].ToJSON).GetInterface(GUID, Result[I]);
  finally
    LJsonArr.Free;
  end;
end;

function TTelegramBot.GetToken: string;
begin
  Result := FToken;
end;

procedure TTelegramBot.SetToken(const Value: string);
begin
  FToken := Value;
end;

function TTelegramBot.GetExceptionManager: ItgExceptionHandler;
begin
  if FExceptionManager = nil then
    FExceptionManager := TtgExceptionManagerConsole.Create;
  Result := FExceptionManager;
end;

procedure TTelegramBot.SetExceptionManager(const Value: ItgExceptionHandler);
begin
  FExceptionManager := Value;
end;


{$ENDREGION}
{$REGION 'Getting updates'}

function TTelegramBot.SetWebhook(const Url: string; const Certificate: TtgFileToSend; const MaxConnections: Int64; const AllowedUpdates: TAllowedUpdates): Boolean;
begin
  Result := ExtractBool(RequestAPI('setWebhook', [//
    TtgApiParameter.Create('url', Url, '', True), //
    TtgApiParameter.Create('certificate', Certificate, nil, False), //
    TtgApiParameter.Create('max_connections', MaxConnections, 0, False), //
    TtgApiParameter.Create('allowed_updates', AllowedUpdates.ToString, '[]', False) //
    ]));
end;

function TTelegramBot.GetWebhookInfo: ItgWebhookInfo;
begin
  Result := TtgWebhookInfo.Create(RequestAPI('getWebhookInfo', nil));
end;

function TTelegramBot.GetUpdates(const Offset, Limit, Timeout: Int64; const AllowedUpdates: TAllowedUpdates): TArray<ItgUpdate>;
begin
  Result := GetArrayFromMethod<ItgUpdate>(TtgUpdate, RequestAPI('getUpdates', [//
    TtgApiParameter.Create('offset', Offset, 0, False), //
    TtgApiParameter.Create('limit', Limit, 100, False), //
    TtgApiParameter.Create('timeout', Timeout, 0, False), //
    TtgApiParameter.Create('allowed_updates', AllowedUpdates.ToString, '[]', False) //
    ]));
end;

function TTelegramBot.DeleteWebhook: Boolean;
begin
  Result := ExtractBool(RequestAPI('deleteWebhook', nil));
end;
{$ENDREGION}
{$REGION 'Basic methods'}

function TTelegramBot.stopMessageLiveLocation(const ChatId: TValue; const MessageId: Int64; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := ExtractBool(RequestAPI('stopMessageLiveLocation', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('message_id', MessageId, 0, True), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.stopMessageLiveLocation(const InlineMessageId: string; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := ExtractBool(RequestAPI('stopMessageLiveLocation', [//
    TtgApiParameter.Create('inline_message_id', InlineMessageId, '', True), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.UnbanChatMember(const ChatId: TValue; const UserId: Int64): Boolean;
begin
  Result := ExtractBool(RequestAPI('unbanChatMember', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('user_id', UserId, 0, True) //
    ]));
end;

function TTelegramBot.SendLocation(const ChatId: TValue; const Location: TtgLocation; const LivePeriod: Int64; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendLocation', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('latitude', Location.Latitude, 0.0, True), //
    TtgApiParameter.Create('longitude', Location.Longitude, 0.0, True), //
    TtgApiParameter.Create('live_period', LivePeriod, 0, False), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.sendMediaGroup(const ChatId: TValue; const AMedia: TArray<TtgInputMedia>; const ADisableNotification: Boolean; const ReplyToMessageId: Int64): TArray<ITgMessage>;
begin
  Result := GetArrayFromMethod<ITgMessage>(TTgMessage, RequestAPI('sendMediaGroup', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('media', TJsonUtils.ArrayToJString<TtgInputMedia>(AMedia), '[]', True), //
    TtgApiParameter.Create('disable_notification', ADisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False) //
    ]));
end;

function TTelegramBot.SendPhoto(const ChatId: TValue; const Photo: TtgFileToSend; const Caption: string; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendPhoto', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('photo', Photo, nil, True), //
    TtgApiParameter.Create('caption', Caption, '', False), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.SendMessage(const ChatId: TValue; const Text: string; const ParseMode: TtgParseMode; const DisableWebPagePreview, DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendMessage', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('text', Text, '', True), //
    TtgApiParameter.Create('parse_mode', ParseMode.ToString, '', False), //
    TtgApiParameter.Create('disable_web_page_preview', DisableWebPagePreview, False, False), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.SendVenue(const ChatId: TValue; const Venue: TtgVenue; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendVenue', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('latitude', Venue.Location.Latitude, '', True), //
    TtgApiParameter.Create('longitude', Venue.Location.Longitude, '', True), //
    TtgApiParameter.Create('title', Venue.Title, '', True), //
    TtgApiParameter.Create('address', Venue.Address, '', True), //
    TtgApiParameter.Create('foursquare_id', Venue.FoursquareId, False, False), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.SendVideo(const ChatId: TValue; const Video: TtgFileToSend; const Caption: string; const Duration, Width, Height: Int64; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendVideo', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('video', Video, '', True), //
    TtgApiParameter.Create('duration', Duration, 0, False), //
    TtgApiParameter.Create('width', Width, 0, False), //
    TtgApiParameter.Create('height', Height, 0, False), //
    TtgApiParameter.Create('caption', Caption, '', False), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.SendVideoNote(const ChatId: TValue; const VideoNote: TtgFileToSend; const Duration, Length: Int64; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendVideoNote', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('video_note', VideoNote, '', True), //
    TtgApiParameter.Create('duration', Duration, 0, False), //
    TtgApiParameter.Create('length', Length, 0, False), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.SendVoice(const ChatId: TValue; const Voice: TtgFileToSend; const Caption: string; const Duration: Int64; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendVoice', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('voice', Voice, '', True), //
    TtgApiParameter.Create('caption', Caption, '', False), //
    TtgApiParameter.Create('duration', Duration, 0, False), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.SendAudio(const ChatId: TValue; const Audio: TtgFileToSend; const Caption: string; const Duration: Int64; const Performer: string; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendAudio', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('audio', Audio, nil, True), //
    TtgApiParameter.Create('duration', Duration, 0, False), //
    TtgApiParameter.Create('performer', Performer, '', False), //
    TtgApiParameter.Create('caption', Caption, 0, False), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.SendChatAction(const ChatId: TValue; const Action: TtgSendChatAction): Boolean;
begin
  Result := ExtractBool(RequestAPI('sendChatAction', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('action', Action.ToString, '', True)//
    ]));
end;

function TTelegramBot.SendContact(const ChatId: TValue; const Contact: TtgContact; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendContact', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('phone_number', Contact.PhoneNumber, '', True), //
    TtgApiParameter.Create('first_name', Contact.FirstName, '', True), //
    TtgApiParameter.Create('last_name', Contact.LastName, '', False), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.SendDocument(const ChatId: TValue; const Document: TtgFileToSend; const Caption: string; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendDocument', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('document', Document, nil, True), //
    TtgApiParameter.Create('caption', Caption, '', False), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.KickChatMember(const ChatId: TValue; const UserId, UntilDate: Int64): Boolean;
begin
  Result := ExtractBool(RequestAPI('kickChatMember', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('user_id', UserId, 0, True), //
    TtgApiParameter.Create('until_date', UntilDate, '', False)//
    ]));
end;

function TTelegramBot.LeaveChat(const ChatId: TValue): Boolean;
begin
  Result := ExtractBool(RequestAPI('leaveChat', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True)]));
end;

function TTelegramBot.GetUserProfilePhotos(const ChatId: TValue; const Offset, Limit: Int64): ItgUserProfilePhotos;
begin
  Result := TtgUserProfilePhotos.Create(RequestAPI('getUserProfilePhotos', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('offset', Offset, 0, True), //
    TtgApiParameter.Create('limit', Limit, 100, False) //
    ]));
end;

function TTelegramBot.GetMe: ItgUser;
begin
  Result := TtgUser.Create(RequestAPI('getMe', nil));
end;

function TTelegramBot.ForwardMessage(const ChatId, FromChatId: TValue; const MessageId: Int64; const DisableNotification: Boolean): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('forwardMessage', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('from_chat_id', FromChatId, 0, True), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('message_id', MessageId, 0, False)]));
end;

function TTelegramBot.GetChat(const ChatId: TValue): ItgChat;
begin
  Result := TtgChat.Create(RequestAPI('getChat', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True)]));
end;

function TTelegramBot.GetChatAdministrators(const ChatId: TValue): TArray<ItgChatMember>;
begin
  Result := GetArrayFromMethod<ItgChatMember>(TtgChatMember,  //
    RequestAPI('getChatAdministrators', //
    [TtgApiParameter.Create('chat_id', ChatId, 0, True)]));
end;

function TTelegramBot.GetChatMember(const ChatId: TValue; const UserId: Int64): ItgChatMember;
begin
  Result := TtgChatMember.Create(RequestAPI('getChatMember', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('user_id', UserId, 0, True)]));
end;

function TTelegramBot.GetChatMembersCount(const ChatId: TValue): Int64;
var
  LJson: TJSONValue;
begin
  LJson := TJSONObject.ParseJSONValue(RequestAPI('getChatMembersCount', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True)]));
  try
    if not LJson.TryGetValue<Int64>(Result) then
      Result := 0;
  finally
    LJson.Free;
  end;
end;

function TTelegramBot.GetFile(const FileId: string): ItgFile;
begin
  Result := TtgFile.Create(RequestAPI('getFile', [//
    TtgApiParameter.Create('file_id', FileId, '', True)]));
end;

function TTelegramBot.AnswerCallbackQuery(const CallbackQueryId, Text: string; const ShowAlert: Boolean; const Url: string; const CacheTime: Int64): Boolean;
begin
  Result := ExtractBool(RequestAPI('answerCallbackQuery', [//
    TtgApiParameter.Create('callback_query_id', CallbackQueryId, '', True), //
    TtgApiParameter.Create('text', Text, '', True), //
    TtgApiParameter.Create('show_alert', ShowAlert, False, False), //
    TtgApiParameter.Create('url', Url, '', False), //
    TtgApiParameter.Create('cache_time', CacheTime, 0, False)]));
end;
{$ENDREGION}
{$REGION 'Updating messages'}

function TTelegramBot.EditMessageText(const InlineMessageId, Text: string; const ParseMode: TtgParseMode; const DisableWebPagePreview: Boolean; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage(RequestAPI('editMessageText', [//
    TtgApiParameter.Create('inline_message_id', InlineMessageId, 0, True), //
    TtgApiParameter.Create('text', Text, 0, True), //
    TtgApiParameter.Create('parse_mode', ParseMode.ToString, False, False), //
    TtgApiParameter.Create('disable_web_page_preview', DisableWebPagePreview, False, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), 0, False)]));
end;

function TTelegramBot.EditMessageText(const ChatId: TValue; const MessageId: Int64; const Text: string; const ParseMode: TtgParseMode; const DisableWebPagePreview: Boolean; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('editMessageText', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('message_id', MessageId, nil, True), //
    TtgApiParameter.Create('text', Text, '', False), //
    TtgApiParameter.Create('parse_mode', ParseMode.ToString, False, False), //
    TtgApiParameter.Create('disable_web_page_preview', DisableWebPagePreview, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.DeleteMessage(const ChatId: TValue; const MessageId: Int64): Boolean;
begin
  Result := ExtractBool(RequestAPI('deleteMessage', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('message_id', MessageId, nil, True) //
    ]));
end;

function TTelegramBot.EditMessageCaption(const ChatId: TValue; const MessageId: Int64; const Caption: string; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := ExtractBool(RequestAPI('editMessageCaption', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('message_id', MessageId, nil, True), //
    TtgApiParameter.Create('caption', Caption, '', False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.EditMessageCaption(const InlineMessageId, Caption: string; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := ExtractBool(RequestAPI('editMessageCaption', [//
    TtgApiParameter.Create('inline_message_id', InlineMessageId, nil, True), //
    TtgApiParameter.Create('caption', Caption, '', False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.editMessageLiveLocation(const ChatId: TValue; const MessageId: Int64; const Location: TtgLocation; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := ExtractBool(RequestAPI('editMessageLiveLocation', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('message_id', MessageId, 0, True), //
    TtgApiParameter.Create('latitude', Location.Latitude, '', False), //
    TtgApiParameter.Create('longitude', Location.Longitude, False, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.editMessageLiveLocation(const InlineMessageId: string; const Location: TtgLocation; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := ExtractBool(RequestAPI('editMessageLiveLocation', [//
    TtgApiParameter.Create('inline_message_id', InlineMessageId, 0, True), //
    TtgApiParameter.Create('latitude', Location.Latitude, '', False), //
    TtgApiParameter.Create('longitude', Location.Longitude, False, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.EditMessageReplyMarkup(const ChatId: TValue; const MessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('editMessageReplyMarkup', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('message_id', MessageId, '', False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.EditMessageReplyMarkup(const InlineMessageId: string; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('editMessageReplyMarkup', [//
    TtgApiParameter.Create('inline_message_id', InlineMessageId, 0, True), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;
{$ENDREGION}



{$REGION 'Manage groups and channels'}

function TTelegramBot.DeleteChatPhoto(const ChatId: TValue): Boolean;
begin
  Result := ExtractBool(RequestAPI('deleteChatPhoto', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True)]));
end;

function TTelegramBot.deleteChatStickerSet(const ChatId: TValue): Boolean;
begin
  Result := ExtractBool(RequestAPI('deleteChatStickerSet', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True)]));
end;

function TTelegramBot.ExportChatInviteLink(const ChatId: TValue): string;
begin
  Result := GetValueFromMethod(RequestAPI('exportChatInviteLink', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True)]));
end;

function TTelegramBot.PinChatMessage(const ChatId: TValue; const MessageId: Int64; const DisableNotification: Boolean): Boolean;
begin
  Result := ExtractBool(RequestAPI('pinChatMessage', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('message_id', MessageId, 0, True), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False)]));
end;

function TTelegramBot.SetChatDescription(const ChatId: TValue; const Description: string): Boolean;
begin
  Result := ExtractBool(RequestAPI('setChatDescription', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('description', Description, '', True)]));
end;

function TTelegramBot.SetChatPhoto(const ChatId: TValue; const Photo: TtgFileToSend): Boolean;
begin
  Result := ExtractBool(RequestAPI('setChatPhoto', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('photo', Photo, nil, True)]));
end;

function TTelegramBot.setChatStickerSet(const ChatId: TValue; const StickerSetName: string): Boolean;
begin
  Result := ExtractBool(RequestAPI('setChatStickerSet', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('sticker_set_name', StickerSetName, '', True)]));
end;

function TTelegramBot.SetChatTitle(const ChatId: TValue; const Title: string): Boolean;
begin
  Result := ExtractBool(RequestAPI('setChatTitle', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('title', Title, '', True)]));
end;

function TTelegramBot.UnpinChatMessage(const ChatId: TValue): Boolean;
begin
  Result := ExtractBool(RequestAPI('unpinChatMessage', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True)]));
end;


{$ENDREGION}
{$REGION 'Manage users and admins'}

function TTelegramBot.PromoteChatMember(const ChatId: TValue; const UserId: Int64; const CanChangeInfo, CanPostMessages, CanEditMessages, CanDeleteMessages, CanInviteUsers, CanRestrictMembers, CanPinMessages, CanPromoteMembers: Boolean): Boolean;
begin
  Result := ExtractBool(RequestAPI('promoteChatMember', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('user_id', UserId, 0, True), //
    TtgApiParameter.Create('can_change_info', CanChangeInfo, False, False), //
    TtgApiParameter.Create('can_post_messages', CanPostMessages, False, False), //
    TtgApiParameter.Create('can_edit_messages', CanEditMessages, False, False), //
    TtgApiParameter.Create('can_delete_messages', CanDeleteMessages, False, False), //
    TtgApiParameter.Create('can_invite_users', CanInviteUsers, False, False), //
    TtgApiParameter.Create('can_restrict_members', CanRestrictMembers, False, False), //
    TtgApiParameter.Create('can_pin_messages', CanPinMessages, False, False), //
    TtgApiParameter.Create('can_promote_members', CanPromoteMembers, False, False)]));
end;

function TTelegramBot.RestrictChatMember(const ChatId: TValue; const UserId, UntilDate: Int64; const CanSendMessages, CanSendMediaMessages, CanSendOtherMessages, CanAddWebPagePreviews: Boolean): Boolean;
begin
  Result := ExtractBool(RequestAPI('restrictChatMember', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('user_id', UserId, 0, True), //
    TtgApiParameter.Create('until_date', UntilDate, 0, False), //
    TtgApiParameter.Create('can_send_messages', CanSendMessages, False, False), //
    TtgApiParameter.Create('can_send_media_messages', CanSendMediaMessages, False, False), //
    TtgApiParameter.Create('can_send_other_messages', CanSendOtherMessages, False, False), //
    TtgApiParameter.Create('can_add_web_page_previews', CanAddWebPagePreviews, False, False)]));
end;
{$ENDREGION}
{$REGION 'Stickers'}

function TTelegramBot.addStickerToSet(const UserId: Int64; const Name: string; const PngSticker: TValue; const Emojis: string; const MaskPosition: TtgMaskPosition): Boolean;
begin
  Result := ExtractBool(RequestAPI('addStickerToSet', [//
    TtgApiParameter.Create('user_id', UserId, 0, True), //
    TtgApiParameter.Create('name', Name, 0, True), //
    TtgApiParameter.Create('png_sticker', PngSticker, False, False), //
    TtgApiParameter.Create('emojis', Emojis, False, False), //
    TtgApiParameter.Create('mask_position', MaskPosition, 0, False)]));
end;

function TTelegramBot.createNewStickerSet(const UserId: Int64; const Name, Title: string; const PngSticker: TValue; const Emojis: string; const ContainsMasks: Boolean; const MaskPosition: TtgMaskPosition): Boolean;
begin
  Result := ExtractBool(RequestAPI('createNewStickerSet', [//
    TtgApiParameter.Create('user_id', UserId, 0, True), //
    TtgApiParameter.Create('name', Name, nil, True), //
    TtgApiParameter.Create('title', Title, 0, True), //
    TtgApiParameter.Create('png_sticker', PngSticker, 0, True), //
    TtgApiParameter.Create('emojis', Emojis, nil, True), //
    TtgApiParameter.Create('contains_masks', ContainsMasks, 0, True), //
    TtgApiParameter.Create('mask_position', MaskPosition, '', False)]));
end;

function TTelegramBot.deleteStickerFromSet(const Sticker: string): Boolean;
begin
  Result := ExtractBool(RequestAPI('deleteStickerFromSet', [//
    TtgApiParameter.Create('sticker', Sticker, 0, True)]));
end;

function TTelegramBot.getStickerSet(const Name: string): TtgStickerSet;
begin
  Result := TtgStickerSet.Create(RequestAPI('getStickerSet', [//
    TtgApiParameter.Create('name', Name, 0, True)]));
end;

function TTelegramBot.SendSticker(const ChatId, Sticker: TValue; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendSticker', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('sticker', Sticker, '', True), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, 0, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.setStickerPositionInSet(const Sticker: string; const Position: Int64): Boolean;
begin
  Result := ExtractBool(RequestAPI('setStickerPositionInSet', [//
    TtgApiParameter.Create('sticker', Sticker, 0, True), //
    TtgApiParameter.Create('position', Position, nil, True)]));
end;

function TTelegramBot.uploadStickerFile(const UserId: Int64; const PngSticker: TtgFileToSend): ItgFile;
begin
  Result := TtgFile.Create(RequestAPI('uploadStickerFile', [//
    TtgApiParameter.Create('user_id', UserId, 0, True), //
    TtgApiParameter.Create('png_sticker', PngSticker, nil, True)]));
end;
{$ENDREGION}
{$REGION 'Inline mode'}

function TTelegramBot.AnswerInlineQuery(const InlineQueryId: string; const Results: TArray<TtgInlineQueryResult>; const CacheTime: Int64; const IsPersonal: Boolean; const NextOffset, SwitchPmText, SwitchPmParameter: string): Boolean;
begin
  Result := ExtractBool(RequestAPI('answerInlineQuery', [//
    TtgApiParameter.Create('inline_query_id', InlineQueryId, '', True), //
    TtgApiParameter.Create('results', TJsonUtils.ArrayToJString<TtgInlineQueryResult>(Results), nil, True), //
    TtgApiParameter.Create('cache_time', CacheTime, 0, False), //
    TtgApiParameter.Create('is_personal', IsPersonal, False, False), //
    TtgApiParameter.Create('next_offset', NextOffset, '', False), //
    TtgApiParameter.Create('switch_pm_text', SwitchPmText, '', False), //
    TtgApiParameter.Create('switch_pm_parameter', SwitchPmParameter, '', False) //
    ]));
end;


{$ENDREGION}
{$REGION 'Payments'}

function TTelegramBot.SendInvoice(const ChatId: Int64; const title: string; const Description: string; const Payload: string; const ProviderToken: string; const StartParameter: string; const Currency: string; const Prices: TArray<TtgLabeledPrice>; const ProviderData: string; const PhotoUrl: string; const PhotoSize: Int64; const PhotoWidth: Int64; const PhotoHeight: Int64; const NeedName: Boolean; const NeedPhoneNumber: Boolean; const NeedEmail: Boolean; const NeedShippingAddress: Boolean; const IsFlexible: Boolean; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendInvoice', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('title', title, '', True), //
    TtgApiParameter.Create('description', Description, '', True), //
    TtgApiParameter.Create('payload', Payload, '', True), //
    TtgApiParameter.Create('provider_token', ProviderToken, '', True), //
    TtgApiParameter.Create('start_parameter', StartParameter, '', True), //
    TtgApiParameter.Create('currency', Currency, '', True), //
    TtgApiParameter.Create('prices', TJsonUtils.ArrayToJString<TtgLabeledPrice>(Prices), nil, True), //
    TtgApiParameter.Create('provider_data', ProviderData, '', False), //
    TtgApiParameter.Create('photo_url', PhotoUrl, '', False), //
    TtgApiParameter.Create('photo_size', PhotoSize, 0, False), //
    TtgApiParameter.Create('photo_width', PhotoWidth, 0, False), //
    TtgApiParameter.Create('photo_height', PhotoHeight, 0, False), //
    TtgApiParameter.Create('need_name', NeedName, False, False), //
    TtgApiParameter.Create('need_phone_number', NeedPhoneNumber, False, False), //
    TtgApiParameter.Create('need_email', NeedEmail, False, False), //
    TtgApiParameter.Create('need_shipping_address', NeedShippingAddress, False, False), //
    TtgApiParameter.Create('is_flexible', IsFlexible, False, False), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, False, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    ]));
end;

function TTelegramBot.AnswerPreCheckoutQueryBad(const PreCheckoutQueryId, ErrorMessage: string): Boolean;
begin
  Result := ExtractBool(RequestAPI('AnswerPreCheckoutQuery', [//
    TtgApiParameter.Create('Pre_checkout_query_id', PreCheckoutQueryId, 0, True), //
    TtgApiParameter.Create('Ok', False, True, False), //
    TtgApiParameter.Create('Error_message', ErrorMessage, '', True)]));
end;

function TTelegramBot.AnswerPreCheckoutQueryGood(const PreCheckoutQueryId: string): Boolean;
begin
  Result := ExtractBool(RequestAPI('AnswerPreCheckoutQuery', [//
    TtgApiParameter.Create('Pre_checkout_query_id', PreCheckoutQueryId, 0, True), //
    TtgApiParameter.Create('Ok', True, False, False)]));
end;

function TTelegramBot.AnswerShippingQueryBad(const ShippingQueryId, ErrorMessage: string): Boolean;
begin
  Result := ExtractBool(RequestAPI('answerShippingQuery', [//
    TtgApiParameter.Create('Shipping_query_id', ShippingQueryId, 0, True), //
    TtgApiParameter.Create('Ok', False, False, False), //
    TtgApiParameter.Create('Error_message', ErrorMessage, '', False) //
    ]));
end;

function TTelegramBot.AnswerShippingQueryGood(const ShippingQueryId: string; const ShippingOptions: TArray<TtgShippingOption>): Boolean;
begin
  Result := ExtractBool(RequestAPI('answerShippingQuery', [//
    TtgApiParameter.Create('Shipping_query_id', ShippingQueryId, 0, True), //
    TtgApiParameter.Create('Ok', True, False, False), //
    TtgApiParameter.Create('Shipping_options', TJsonUtils.ArrayToJString<TtgShippingOption>(ShippingOptions), nil, True)]));
end;

{$ENDREGION}

{$REGION 'Games'}

function TTelegramBot.GetGameHighScores(const UserId: Int64; const InlineMessageId: string): TArray<ItgGameHighScore>;
begin
  Result := GetArrayFromMethod<ItgGameHighScore>(TtgGameHighScore, RequestAPI('getGameHighScores', [//
    TtgApiParameter.Create('user_id', UserId, 0, True), //
    TtgApiParameter.Create('inline_message_id', InlineMessageId, 0, True)]))
end;

function TTelegramBot.GetGameHighScores(const UserId, ChatId, MessageId: Int64): TArray<ItgGameHighScore>;
begin
  Result := GetArrayFromMethod<ItgGameHighScore>(TtgGameHighScore, RequestAPI('getGameHighScores', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('user_id', UserId, 0, True), //
    TtgApiParameter.Create('message_id', MessageId, 0, True)]))
end;

function TTelegramBot.SendGame(const ChatId: Int64; const GameShortName: string; const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('sendGame', [//
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('game_short_name', GameShortName, '', True), //
    TtgApiParameter.Create('disable_notification', DisableNotification, False, False), //
    TtgApiParameter.Create('reply_to_message_id', ReplyToMessageId, False, False), //
    TtgApiParameter.Create('reply_markup', TInterfacedObject(ReplyMarkup), nil, False)]));
end;

function TTelegramBot.SetGameScore(const UserId, Score: Int64; const InlineMessageId: string; const Force, DisableEditMessage: Boolean): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('setGameScore', [//
    TtgApiParameter.Create('user_id', UserId, 0, True), //
    TtgApiParameter.Create('score', Score, 0, True), //
    TtgApiParameter.Create('force', Force, False, False), //
    TtgApiParameter.Create('disable_edit_message', DisableEditMessage, False, False), //
    TtgApiParameter.Create('inline_message_id', InlineMessageId, 0, True)]));
end;

function TTelegramBot.SetGameScore(const UserId, Score, ChatId, MessageId: Int64; const Force, DisableEditMessage: Boolean): ITgMessage;
begin
  Result := TTgMessage.Create(RequestAPI('setGameScore', [//
    TtgApiParameter.Create('user_id', UserId, 0, True), //
    TtgApiParameter.Create('score', Score, nil, True), //
    TtgApiParameter.Create('force', Force, False, False), //
    TtgApiParameter.Create('disable_edit_message', DisableEditMessage, False, False), //
    TtgApiParameter.Create('chat_id', ChatId, 0, True), //
    TtgApiParameter.Create('message_id', MessageId, 0, True)]));
end;
{$ENDREGION}

end.

