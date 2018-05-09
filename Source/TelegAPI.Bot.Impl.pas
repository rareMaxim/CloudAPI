unit TelegAPI.Bot.Impl;

{$I config.inc}

interface

uses
  CoreAPI,
  CoreAPI.Parameter,
  CrossUrl.HttpClient,
  System.Rtti,
  System.Classes,
  System.TypInfo,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  TelegAPI.Base,
  TelegAPI.Bot,
  TelegAPI.Types,
  TelegAPI.Types.Impl,
  TelegAPI.Types.Enums,
  TelegAPI.Types.ReplyMarkups,
  TelegAPI.Types.InlineQueryResults,
  TelegAPI.Exceptions,
  TelegAPI.Utils.JSON;

type
  TtgOnReceiveRawData = procedure(ASender: TObject; const AData: string) of object;

  TtgOnSendData = procedure(ASender: TObject; const AUrl, AData: string) of object;

  TTelegramBot = class(TtgAbstractComponent, ITelegramBot)
  private
    FToken: string;
    FRequest: TtgCoreApi;
    FOnRawData: TtgOnReceiveRawData;
    FExceptionManager: ItgExceptionHandler;
  //  FRequest: ItgRequestAPI;
    FOnSendData: TtgOnSendData;
    function GetToken: string;
    procedure SetToken(const Value: string);
    // Returns TJSONArray as method request result
    function GetJSONArrayFromMethod(const AValue: string): TJSONArray;
    // Returns response JSON from server as result of request
    function GetArrayFromMethod<TI: IInterface>(const TgClass: TBaseJsonClass;
      const AValue: string): TArray<TI>;
    function GetExceptionManager: ItgExceptionHandler;
    procedure SetExceptionManager(const Value: ItgExceptionHandler);
    function GetHttpCore: IcuHttpClient;
    procedure SetHttpCore(const Value: IcuHttpClient);
  protected
  public
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const AToken: string); overload;
    constructor Create(const AToken: string; ACore: IcuHttpClient); overload;
    destructor Destroy; override;
{$REGION 'Getting updates'}
    function GetUpdates( //
      const Offset: Int64 = 0; //
      const Limit: Int64 = 100; //
      const Timeout: Int64 = 0; //
      const AllowedUpdates: TAllowedUpdates = UPDATES_ALLOWED_ALL): TArray<
      ItgUpdate>; overload;
    function GetUpdates( //
      const JSON: string): TArray<ItgUpdate>; overload;
    function SetWebhook( //
      const Url: string; //
      const Certificate: TtgFileToSend = nil; //
      const MaxConnections: Int64 = 40; //
      const AllowedUpdates: TAllowedUpdates = UPDATES_ALLOWED_ALL): Boolean;
    function DeleteWebhook: Boolean;
    function GetWebhookInfo: ItgWebhookInfo;
{$ENDREGION}
{$REGION 'Basic methods'}
    function GetMe: ItgUser;
    function SendMessage( //
      const ChatId: TtgUserLink; //
      const Text: string; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const DisableWebPagePreview: Boolean = False; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function ForwardMessage( //
      const ChatId, FromChatId: TtgUserLink; //
      const MessageId: Int64; //
      const DisableNotification: Boolean = False): ITgMessage;
    function SendPhoto( //
      const ChatId: TtgUserLink; //
      const Photo: TtgFileToSend; //
      const Caption: string = ''; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendAudio( //
      const ChatId: TtgUserLink; //
      const Audio: TtgFileToSend; //
      const Caption: string = ''; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const Duration: Int64 = 0; //
      const Performer: string = ''; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendDocument( //
      const ChatId: TtgUserLink; //
      const Document: TtgFileToSend; //
      const Caption: string = ''; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVideo( //
      const ChatId: TtgUserLink; //
      const Video: TtgFileToSend; //
      const Caption: string = ''; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const SupportsStreaming: Boolean = True; //
      const Duration: Int64 = 0; //
      const Width: Int64 = 0; //
      const Height: Int64 = 0; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVoice( //
      const ChatId: TtgUserLink; //
      const Voice: TtgFileToSend; //
      const Caption: string = ''; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const Duration: Int64 = 0; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVideoNote( //
      const ChatId: TtgUserLink; //
      const VideoNote: TtgFileToSend; //
      const Duration: Int64 = 0; //
      const Length: Int64 = 0; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendLocation( //
      const ChatId: TtgUserLink; //
      const Location: TtgLocation; //
      const LivePeriod: Int64 = 0; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVenue( //
      const ChatId: TtgUserLink; //
      const Venue: TtgVenue; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendContact( //
      const ChatId: TtgUserLink; //
      const Contact: TtgContact; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendChatAction( //
      const ChatId: TtgUserLink; //
      const Action: TtgSendChatAction): Boolean;
    function GetUserProfilePhotos( //
      const ChatId: TtgUserLink; //
      const Offset: Int64; //
      const Limit: Int64 = 100): ItgUserProfilePhotos;
    function GetFile(const FileId: string): ItgFile;
    function KickChatMember( //
      const ChatId: TtgUserLink; //
      const UserId: Int64; //
      const UntilDate: TDateTime = 0): Boolean;
    function UnbanChatMember( //
      const ChatId: TtgUserLink; //
      const UserId: Int64): Boolean;
    function LeaveChat(const ChatId: TtgUserLink): Boolean;
    function GetChat(const ChatId: TtgUserLink): ItgChat;
    function GetChatAdministrators(const ChatId: TtgUserLink): TArray<ItgChatMember>;
    function GetChatMembersCount(const ChatId: TtgUserLink): Int64;
    function GetChatMember( //
      const ChatId: TtgUserLink; //
      const UserId: Int64): ItgChatMember;
    function AnswerCallbackQuery( //
      const CallbackQueryId: string; //
      const Text: string = ''; //
      const ShowAlert: Boolean = False; //
      const Url: string = ''; //
      const CacheTime: Int64 = 0): Boolean;
{$ENDREGION}
{$REGION 'Updating messages'}
    function EditMessageText( //
      const ChatId: TtgUserLink; //
      const MessageId: Int64; //
      const Text: string; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const DisableWebPagePreview: Boolean = False; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function EditMessageText( //
      const InlineMessageId: string; //
      const Text: string; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const DisableWebPagePreview: Boolean = False; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function EditMessageCaption( //
      const ChatId: TtgUserLink; //
      const MessageId: Int64; //
      const Caption: string; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function EditMessageCaption( //
      const InlineMessageId: string; //
      const Caption: string; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function editMessageLiveLocation( //
      const ChatId: TtgUserLink; //
      const MessageId: Int64; //
      const Location: TtgLocation; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function editMessageLiveLocation( //
      const InlineMessageId: string; //
      const Location: TtgLocation; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function stopMessageLiveLocation( //
      const ChatId: TtgUserLink; //
      const MessageId: Int64; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function stopMessageLiveLocation( //
      const InlineMessageId: string; //
      ReplyMarkup: IReplyMarkup = nil): Boolean; overload;
    function EditMessageReplyMarkup( //
      const ChatId: TtgUserLink; //
      const MessageId: Int64; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function EditMessageReplyMarkup( //
      const InlineMessageId: string; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage; overload;
    function DeleteMessage( //
      const ChatId: TtgUserLink; //
      const MessageId: Int64): Boolean;
{$ENDREGION}
{$REGION 'Inline mode'}
    function AnswerInlineQuery( //
      const InlineQueryId: string; //
      const Results: TArray<TtgInlineQueryResult>; //
      const CacheTime: Int64 = 300; //
      const IsPersonal: Boolean = False; //
      const NextOffset: string = ''; //
      const SwitchPmText: string = ''; //
      const SwitchPmParameter: string = ''): Boolean;
{$ENDREGION}
{$REGION 'Payments'}
    function SendInvoice( //
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
    function AnswerShippingQueryGood( //
      const ShippingQueryId: string; //
      const ShippingOptions: TArray<TtgShippingOption>): Boolean;
    function AnswerShippingQueryBad( //
      const ShippingQueryId: string; //
      const ErrorMessage: string): Boolean;
    function AnswerPreCheckoutQueryGood( //
      const PreCheckoutQueryId: string): Boolean;
    function AnswerPreCheckoutQueryBad( //
      const PreCheckoutQueryId: string; //
      const ErrorMessage: string): Boolean;
{$ENDREGION}
{$REGION 'Games'}
    function SendGame( //
      const ChatId: Int64; //
      const GameShortName: string; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SetGameScore( //
      const UserId: Int64; //
      const Score: Int64; //
      const InlineMessageId: string; //
      const Force: Boolean = False; //
      const DisableEditMessage: Boolean = False): ITgMessage; overload;
    function SetGameScore( //
      const UserId: Int64; //
      const Score: Int64; //
      const ChatId: Int64; //
      const MessageId: Int64; //
      const Force: Boolean = False; //
      const DisableEditMessage: Boolean = False): ITgMessage; overload;
    function GetGameHighScores( //
      const UserId: Int64; //
      const InlineMessageId: string = ''): TArray<ItgGameHighScore>; overload;
    function GetGameHighScores( //
      const UserId: Int64; //
      const ChatId: Int64 = 0; //
      const MessageId: Int64 = 0): TArray<ItgGameHighScore>; overload;
{$ENDREGION}
{$REGION 'Manage groups and channels'}
    function DeleteChatPhoto(const ChatId: TtgUserLink): Boolean;
    function ExportChatInviteLink(const ChatId: TtgUserLink): string;
    function PinChatMessage( //
      const ChatId: TtgUserLink; //
      const MessageId: Int64; //
      const DisableNotification: Boolean = False): Boolean;
    function SetChatDescription(const ChatId: TtgUserLink; const Description:
      string): Boolean;
    function SetChatPhoto(const ChatId: TtgUserLink; const Photo: TtgFileToSend): Boolean;
    function SetChatTitle(const ChatId: TtgUserLink; const Title: string): Boolean;
    function UnpinChatMessage(const ChatId: TtgUserLink): Boolean;
{$ENDREGION}
{$REGION 'Manage users and admins'}
    function RestrictChatMember( //
      const ChatId: TtgUserLink; //
      const UserId: Int64; //
      const UntilDate: TDateTime = 0; //
      const CanSendMessages: Boolean = False; //
      const CanSendMediaMessages: Boolean = False; //
      const CanSendOtherMessages: Boolean = False; //
      const CanAddWebPagePreviews: Boolean = False): Boolean;
    function PromoteChatMember( //
      const ChatId: TtgUserLink; //
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
    function SendSticker( //
      const ChatId: TtgUserLink; //
      const Sticker: TtgFileToSend; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function getStickerSet(const Name: string): TtgStickerSet;
    function uploadStickerFile(const UserId: Int64; const PngSticker:
      TtgFileToSend): ItgFile;
    function createNewStickerSet( //
      const UserId: Int64; //
      const Name, Title: string; //
      const PngSticker: TtgFileToSend; //
      const Emojis: string; //
      const ContainsMasks: Boolean = False; //
      const MaskPosition: TtgMaskPosition = nil): Boolean;
    function addStickerToSet( //
      const UserId: Int64; //
      const Name: string; //
      const PngSticker: TtgFileToSend; //
      const Emojis: string; //
      const MaskPosition: TtgMaskPosition = nil): Boolean;
    function setStickerPositionInSet(const Sticker: string; const Position:
      Int64): Boolean;
    function deleteStickerFromSet(const Sticker: string): Boolean;
    function setChatStickerSet(const ChatId: TtgUserLink; const StickerSetName:
      string): Boolean;
    function deleteChatStickerSet(const ChatId: TtgUserLink): Boolean;
    function sendMediaGroup( //
      const ChatId: TtgUserLink; //
      const AMedia: TArray<TtgInputMedia>; //
      const ADisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0): TArray<ITgMessage>;

{$ENDREGION}
  published
{$REGION 'Property|Свойства'}
    property HttpCore: IcuHttpClient read GetHttpCore write SetHttpCore;
    property Token: string read GetToken write SetToken;
    property ExceptionManager: ItgExceptionHandler read GetExceptionManager
      write SetExceptionManager;
{$ENDREGION}
{$REGION 'События|Events'}
    property OnReceiveRawData: TtgOnReceiveRawData read FOnRawData write FOnRawData;
    property OnSendData: TtgOnSendData read FOnSendData write FOnSendData;
{$ENDREGION}
  end;

implementation

uses
  REST.JSON,
  TelegAPI.Helpers;
{ TTelegramBot }
{$REGION 'Core'}

constructor TTelegramBot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRequest := TtgCoreApi.Create();
  FRequest.OnError :=
    procedure(E: Exception)
    begin
      ExceptionManager.HaveGlobalException('RequestAPI', E)
    end;
  FRequest.OnReceive :=
    procedure(AData: string)
    begin
      if Assigned(OnReceiveRawData) then
        OnReceiveRawData(Self, AData);
    end;
  FRequest.OnSend :=
    procedure(AURL, AData: string)
    begin
      if Assigned(OnSendData) then
        OnSendData(Self, AURL, AData);
    end;
  FRequest.DataExtractor :=
    function(AInput: string): string
    var
      LJSON: TJSONObject;
      LException: EApiRequestException;
      LExcCode: Integer;
      LExcDesc: string;
    begin
      Result := '';
      if AInput.IsEmpty or AInput.StartsWith('<html') then
        Exit;
      LJSON := TJSONObject.ParseJSONValue(AInput) as TJSONObject;
      try
        if LJSON.GetValue('ok') is TJSONFalse then
        begin
          LExcCode := (LJSON.GetValue('error_code') as TJSONNumber).AsInt;
          LExcDesc := (LJSON.GetValue('description') as TJSONString).Value;
          LException := EApiRequestException.Create(LExcDesc, LExcCode);
          try
            ExceptionManager.HaveApiException('TTelegramBot.ApiTest', LException)
          finally
            LException.Free;
          end;
        end
        else
          Result := LJSON.GetValue('result').ToString;
      finally
        LJSON.Free;
      end;
    end;
end;

constructor TTelegramBot.Create(const AToken: string);
begin
  Self.Create(nil);
  SetToken(AToken);
end;

destructor TTelegramBot.Destroy;
begin
  FRequest.Free;
  FRequest := nil;
  FExceptionManager := nil;
  inherited;
end;

function TTelegramBot.GetJSONArrayFromMethod(const AValue: string): TJSONArray;
begin
  Result := TJSONObject.ParseJSONValue(AValue) as TJSONArray;
end;

function TTelegramBot.GetArrayFromMethod<TI>(const TgClass: TBaseJsonClass;
  const AValue: string): TArray<TI>;
var
  LJsonArr: TJSONArray;
  I: Integer;
  GUID: TGUID;
  LException: Exception;
begin
  // stage 1: type checking
  // cache value fot further use
  GUID := GetTypeData(TypeInfo(TI))^.GUID;
  // check for TI interface support
  if TgClass.GetInterfaceEntry(GUID) = nil then
  begin
    LException := Exception.Create('GetArrayFromMethod: unsupported interface for '
      + TgClass.ClassName);
    try
      ExceptionManager.HaveGlobalException('GetArrayFromMethod', LException);
    finally
      LException.Free;
    end;
  end;
  // stage 2: proceed data
  LJsonArr := GetJSONArrayFromMethod(AValue);
  if (not Assigned(LJsonArr)) or LJsonArr.Null then
    Exit(nil);
  try
    SetLength(Result, LJsonArr.Count);
    for I := 0 to High(Result) do
      TgClass.GetTgClass.Create(LJsonArr.Items[I].ToString).GetInterface(GUID, Result[I]);
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
  FRequest.SetToken(Token)
end;

function TTelegramBot.GetExceptionManager: ItgExceptionHandler;
begin
  if FExceptionManager = nil then
    FExceptionManager := TtgExceptionManagerConsole.Create(nil);
  Result := FExceptionManager;
end;

procedure TTelegramBot.SetExceptionManager(const Value: ItgExceptionHandler);
begin
  FExceptionManager := Value;
end;

{$ENDREGION}
{$REGION 'Getting updates'}

function TTelegramBot.SetWebhook(const Url: string; const Certificate:
  TtgFileToSend; const MaxConnections: Int64; const AllowedUpdates:
  TAllowedUpdates): Boolean;
begin
  Result := FRequest.SetMethod('setWebhook') //
    .AddParameter('url', Url, '', True) //
    .AddParameter('certificate', Certificate, nil, False) //
    .AddParameter('max_connections', MaxConnections, 0, False) //
    .AddParameter('allowed_updates', AllowedUpdates.ToString, '[]', False) //
    .ExecuteAsBool;
end;

function TTelegramBot.GetWebhookInfo: ItgWebhookInfo;
begin
  Result := TtgWebhookInfo.Create(FRequest.SetMethod('getWebhookInfo').Execute);
end;

function TTelegramBot.GetUpdates(const Offset, Limit, Timeout: Int64; const
  AllowedUpdates: TAllowedUpdates): TArray<ItgUpdate>;
begin
  Result := GetArrayFromMethod<ItgUpdate>(TtgUpdate, FRequest.SetMethod('getUpdates') //
    .AddParameter('offset', Offset, 0, False) //
    .AddParameter('limit', Limit, 100, False) //
    .AddParameter('timeout', Timeout, 0, False) //
    .AddParameter('allowed_updates', AllowedUpdates.ToString, '[]', False) //
    .Execute);
end;

function TTelegramBot.GetUpdates(const JSON: string): TArray<ItgUpdate>;
begin
  Result := GetArrayFromMethod<ItgUpdate>(TtgUpdate, JSON);
end;

function TTelegramBot.DeleteWebhook: Boolean;
begin
  Result := FRequest.SetMethod('deleteWebhook').ExecuteAsBool;
end;

{$ENDREGION}
{$REGION 'Basic methods'}

function TTelegramBot.stopMessageLiveLocation(const ChatId: TtgUserLink; const
  MessageId: Int64; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := FRequest.SetMethod('stopMessageLiveLocation') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('message_id', MessageId, 0, True) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.stopMessageLiveLocation(const InlineMessageId: string;
  ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := FRequest.SetMethod('stopMessageLiveLocation') //
    .AddParameter('inline_message_id', InlineMessageId, 0, True) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.UnbanChatMember(const ChatId: TtgUserLink; const UserId:
  Int64): Boolean;
begin
  Result := FRequest.SetMethod('unbanChatMember') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('user_id', UserId, 0, True) //
    .ExecuteAsBool;
end;

function TTelegramBot.SendLocation(const ChatId: TtgUserLink; const Location:
  TtgLocation; const LivePeriod: Int64; const DisableNotification: Boolean;
  const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('unbanChatMember') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('latitude', Location.Latitude, 0.0, True) //
    .AddParameter('longitude', Location.Longitude, 0.0, True) //
    .AddParameter('live_period', LivePeriod, 0, False) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.sendMediaGroup(const ChatId: TtgUserLink; const AMedia:
  TArray<TtgInputMedia>; const ADisableNotification: Boolean; const
  ReplyToMessageId: Int64): TArray<ITgMessage>;
var
  LRequest: ItgRequestAPI;
  LMedia: TtgInputMedia;
begin
 // TBaseJson.UnSupported;
  LRequest := FRequest.SetMethod('sendMediaGroup') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('media', TJsonUtils.ArrayToJString<TtgInputMedia>(AMedia),
    '[]', True) //
    .AddParameter('disable_notification', ADisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False);
  for LMedia in AMedia do
  begin
    case LMedia.GetFileToSend.Tag of
      TtgFileToSendTag.FromFile:
        LRequest.AddRawFile(LMedia.Media, LMedia.GetFileToSend.Data);
      TtgFileToSendTag.FromStream:
        LRequest.AddRawStream(LMedia.Media, LMedia.GetFileToSend.Content);
    end;

  end;
  Result := GetArrayFromMethod<ITgMessage>(TTgMessage, LRequest//
    .Execute);

end;

function TTelegramBot.SendPhoto(const ChatId: TtgUserLink; const Photo:
  TtgFileToSend; const Caption: string; const ParseMode: TtgParseMode; const
  DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup:
  IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendPhoto') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('photo', Photo, nil, True) //
    .AddParameter('caption', Caption, '', False) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.SendMessage(const ChatId: TtgUserLink; const Text: string;
  const ParseMode: TtgParseMode; const DisableWebPagePreview,
  DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup:
  IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendMessage') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('text', Text, '', True) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False) //
    .AddParameter('disable_web_page_preview', DisableWebPagePreview, False, False) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.SendVenue(const ChatId: TtgUserLink; const Venue: TtgVenue;
  const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup:
  IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendVenue') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('latitude', Venue.Location.Latitude, 0, True) //
    .AddParameter('longitude', Venue.Location.Longitude, 0, True) //
    .AddParameter('title', Venue.Title, '', True) //
    .AddParameter('address', Venue.Address, '', True) //
    .AddParameter('foursquare_id', Venue.FoursquareId, '', False) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.SendVideo(const ChatId: TtgUserLink; const Video:
  TtgFileToSend; const Caption: string; const ParseMode: TtgParseMode; const
  SupportsStreaming: Boolean; const Duration, Width, Height: Int64; const
  DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup:
  IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendVideo') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('video', Video, nil, True) //
    .AddParameter('duration', Duration, 0, False) //
    .AddParameter('width', Width, 0, False) //
    .AddParameter('height', Height, 0, False) //
    .AddParameter('caption', Caption, '', False) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False) //
    .AddParameter('supports_streaming', SupportsStreaming, False, False) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.SendVideoNote(const ChatId: TtgUserLink; const VideoNote:
  TtgFileToSend; const Duration, Length: Int64; const DisableNotification:
  Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendVideoNote') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('video_note', VideoNote, nil, True) //
    .AddParameter('duration', Duration, 0, False) //
    .AddParameter('length', Length, 0, False) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.SendVoice(const ChatId: TtgUserLink; const Voice:
  TtgFileToSend; const Caption: string; const ParseMode: TtgParseMode; const
  Duration: Int64; const DisableNotification: Boolean; const ReplyToMessageId:
  Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendVoice') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('voice', Voice, nil, True) //
    .AddParameter('duration', Duration, 0, False) //
    .AddParameter('caption', Caption, '', False) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.SendAudio(const ChatId: TtgUserLink; const Audio:
  TtgFileToSend; const Caption: string; const ParseMode: TtgParseMode; const
  Duration: Int64; const Performer: string; const DisableNotification: Boolean;
  const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendAudio') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('audio', Audio, nil, True) //
    .AddParameter('duration', Duration, 0, False) //
    .AddParameter('performer', Performer, '', False) //
    .AddParameter('caption', Caption, '', False) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.SendChatAction(const ChatId: TtgUserLink; const Action:
  TtgSendChatAction): Boolean;
begin
  Result := FRequest.SetMethod('sendChatAction') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('action', Action.ToString, '', True) //
    .ExecuteAsBool;
end;

function TTelegramBot.SendContact(const ChatId: TtgUserLink; const Contact:
  TtgContact; const DisableNotification: Boolean; const ReplyToMessageId: Int64;
  ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendContact') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('phone_number', Contact.PhoneNumber, '', True) //
    .AddParameter('first_name', Contact.FirstName, '', True) //
    .AddParameter('last_name', Contact.LastName, '', False) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.SendDocument(const ChatId: TtgUserLink; const Document:
  TtgFileToSend; const Caption: string; const ParseMode: TtgParseMode; const
  DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup:
  IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendDocument') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('document', Document, nil, True) //
    .AddParameter('caption', Caption, '', False) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.KickChatMember(const ChatId: TtgUserLink; const UserId:
  Int64; const UntilDate: TDateTime): Boolean;
begin
  Result := FRequest.SetMethod('kickChatMember') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('until_date', UntilDate, 0, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.LeaveChat(const ChatId: TtgUserLink): Boolean;
begin
  Result := FRequest.SetMethod('leaveChat') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .ExecuteAsBool;
end;

function TTelegramBot.GetUserProfilePhotos(const ChatId: TtgUserLink; const
  Offset, Limit: Int64): ItgUserProfilePhotos;
begin
  Result := TtgUserProfilePhotos.Create(FRequest.SetMethod('getUserProfilePhotos') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('offset', Offset, 0, True) //
    .AddParameter('limit', Limit, 100, False) //
    .Execute);
end;

function TTelegramBot.GetMe: ItgUser;
begin
  Result := TtgUser.Create(FRequest.SetMethod('getMe').Execute);
end;

function TTelegramBot.ForwardMessage(const ChatId, FromChatId: TtgUserLink;
  const MessageId: Int64; const DisableNotification: Boolean): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('forwardMessage') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('from_chat_id', FromChatId, 0, True) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('message_id', MessageId, 0, False) //
    .Execute);
end;

function TTelegramBot.GetChat(const ChatId: TtgUserLink): ItgChat;
begin
  Result := TtgChat.Create(FRequest.SetMethod('getChat').Execute);
end;

function TTelegramBot.GetChatAdministrators(const ChatId: TtgUserLink): TArray<
  ItgChatMember>;
begin
  Result := GetArrayFromMethod<ItgChatMember>(TtgChatMember, FRequest.SetMethod('getChatAdministrators')
    //
    .AddParameter('chat_id', ChatId, 0, True).Execute);
end;

function TTelegramBot.GetChatMember(const ChatId: TtgUserLink; const UserId:
  Int64): ItgChatMember;
begin
  Result := TtgChatMember.Create(FRequest.SetMethod('getChatMember') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('user_id', UserId, 0, True) //
    .Execute);
end;

function TTelegramBot.GetChatMembersCount(const ChatId: TtgUserLink): Int64;
var
  LJson: TJSONValue;
begin
  LJson := TJSONObject.ParseJSONValue(FRequest.SetMethod('getChatMembersCount')
    //
    .AddParameter('chat_id', ChatId, 0, True).Execute);
  try
    if not LJson.TryGetValue<Int64>(Result) then
      Result := 0;
  finally
    LJson.Free;
  end;
end;

function TTelegramBot.GetFile(const FileId: string): ItgFile;
begin
  Result := TtgFile.Create(FRequest.SetMethod('getFile') //
    .AddParameter('file_id', FileId, '', True).Execute);
end;

function TTelegramBot.AnswerCallbackQuery(const CallbackQueryId, Text: string;
  const ShowAlert: Boolean; const Url: string; const CacheTime: Int64): Boolean;
begin
  Result := FRequest.SetMethod('answerCallbackQuery') //
    .AddParameter('callback_query_id', CallbackQueryId, '', True) //
    .AddParameter('text', Text, '', True) //
    .AddParameter('show_alert', ShowAlert, False, False) //
    .AddParameter('url', Url, '', False) //
    .AddParameter('cache_time', CacheTime, 0, False) //
    .ExecuteAsBool;
end;
{$ENDREGION}
{$REGION 'Updating messages'}

function TTelegramBot.EditMessageText(const InlineMessageId, Text: string; const
  ParseMode: TtgParseMode; const DisableWebPagePreview: Boolean; ReplyMarkup:
  IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('editMessageText') //
    .AddParameter('inline_message_id', InlineMessageId, 0, True) //
    .AddParameter('text', Text, '', True) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False) //
    .AddParameter('disable_web_page_preview', DisableWebPagePreview, False, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.EditMessageText(const ChatId: TtgUserLink; const MessageId:
  Int64; const Text: string; const ParseMode: TtgParseMode; const
  DisableWebPagePreview: Boolean; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('editMessageText') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('message_id', MessageId, 0, True) //
    .AddParameter('text', Text, '', True) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False) //
    .AddParameter('disable_web_page_preview', DisableWebPagePreview, False, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.DeleteMessage(const ChatId: TtgUserLink; const MessageId:
  Int64): Boolean;
begin
  Result := FRequest.SetMethod('deleteMessage') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('message_id', MessageId, 0, True) //
    .ExecuteAsBool;
end;

function TTelegramBot.EditMessageCaption(const ChatId: TtgUserLink; const
  MessageId: Int64; const Caption: string; const ParseMode: TtgParseMode;
  ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := FRequest.SetMethod('editMessageText') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('message_id', MessageId, 0, True) //
    .AddParameter('caption', Caption, '', True) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.EditMessageCaption(const InlineMessageId, Caption: string;
  const ParseMode: TtgParseMode; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := FRequest.SetMethod('editMessageCaption') //
    .AddParameter('inline_message_id', InlineMessageId, 0, True) //
    .AddParameter('caption', Caption, '', True) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.editMessageLiveLocation(const ChatId: TtgUserLink; const
  MessageId: Int64; const Location: TtgLocation; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := FRequest.SetMethod('editMessageLiveLocation') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('message_id', MessageId, 0, True) //
    .AddParameter('latitude', Location.Latitude, 0, False) //
    .AddParameter('longitude', Location.Longitude, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.editMessageLiveLocation(const InlineMessageId: string;
  const Location: TtgLocation; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := FRequest.SetMethod('editMessageLiveLocation') //
    .AddParameter('inline_message_id', InlineMessageId, 0, True) //
    .AddParameter('latitude', Location.Latitude, 0, False) //
    .AddParameter('longitude', Location.Longitude, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.EditMessageReplyMarkup(const ChatId: TtgUserLink; const
  MessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('editMessageReplyMarkup') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('message_id', MessageId, 0, True) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.EditMessageReplyMarkup(const InlineMessageId: string;
  ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('editMessageReplyMarkup') //
    .AddParameter('inline_message_id', InlineMessageId, 0, True) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;
{$ENDREGION}
{$REGION 'Manage groups and channels'}

function TTelegramBot.DeleteChatPhoto(const ChatId: TtgUserLink): Boolean;
begin
  Result := FRequest.SetMethod('deleteChatPhoto') //
    .AddParameter('chat_id', ChatId, 0, True).ExecuteAsBool;
end;

function TTelegramBot.deleteChatStickerSet(const ChatId: TtgUserLink): Boolean;
begin
  Result := FRequest.SetMethod('deleteChatStickerSet') //
    .AddParameter('chat_id', ChatId, 0, True).ExecuteAsBool;
end;

function TTelegramBot.ExportChatInviteLink(const ChatId: TtgUserLink): string;
begin
  Result := FRequest.SetMethod('deleteChatStickerSet') //
    .AddParameter('chat_id', ChatId, 0, True).ExecuteAndReadValue;
end;

function TTelegramBot.PinChatMessage(const ChatId: TtgUserLink; const MessageId:
  Int64; const DisableNotification: Boolean): Boolean;
begin
  Result := FRequest.SetMethod('pinChatMessage') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('message_id', MessageId, 0, True) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.SetChatDescription(const ChatId: TtgUserLink; const
  Description: string): Boolean;
begin
  Result := FRequest.SetMethod('setChatDescription') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('description', Description, '', True) //
    .ExecuteAsBool;
end;

function TTelegramBot.SetChatPhoto(const ChatId: TtgUserLink; const Photo:
  TtgFileToSend): Boolean;
begin
  Result := FRequest.SetMethod('setChatDescription') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('photo', Photo, nil, True) //
    .ExecuteAsBool;
end;

function TTelegramBot.setChatStickerSet(const ChatId: TtgUserLink; const
  StickerSetName: string): Boolean;
begin
  Result := FRequest.SetMethod('setChatStickerSet') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('sticker_set_name', StickerSetName, '', True) //
    .ExecuteAsBool;
end;

function TTelegramBot.SetChatTitle(const ChatId: TtgUserLink; const Title:
  string): Boolean;
begin
  Result := FRequest.SetMethod('setChatTitle') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('title', Title, '', True) //
    .ExecuteAsBool;
end;

function TTelegramBot.UnpinChatMessage(const ChatId: TtgUserLink): Boolean;
begin
  Result := FRequest.SetMethod('unpinChatMessage') //
    .AddParameter('chat_id', ChatId, 0, True).ExecuteAsBool;
end;

{$ENDREGION}
{$REGION 'Manage users and admins'}

function TTelegramBot.PromoteChatMember(const ChatId: TtgUserLink; const UserId:
  Int64; const CanChangeInfo, CanPostMessages, CanEditMessages,
  CanDeleteMessages, CanInviteUsers, CanRestrictMembers, CanPinMessages,
  CanPromoteMembers: Boolean): Boolean;
begin
  Result := FRequest.SetMethod('promoteChatMember') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('can_change_info', CanChangeInfo, False, False) //
    .AddParameter('can_post_messages', CanPostMessages, False, False) //
    .AddParameter('can_edit_messages', CanEditMessages, False, False) //
    .AddParameter('can_delete_messages', CanDeleteMessages, False, False) //
    .AddParameter('can_invite_users', CanInviteUsers, False, False) //
    .AddParameter('can_restrict_members', CanRestrictMembers, False, False) //
    .AddParameter('can_pin_messages', CanPinMessages, False, False) //
    .AddParameter('can_promote_members', CanPromoteMembers, False, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.RestrictChatMember(const ChatId: TtgUserLink; const UserId:
  Int64; const UntilDate: TDateTime; const CanSendMessages, CanSendMediaMessages,
  CanSendOtherMessages, CanAddWebPagePreviews: Boolean): Boolean;
begin
  Result := FRequest.SetMethod('restrictChatMember') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('until_date', UntilDate, 0, False) //
    .AddParameter('can_send_messages', CanSendMessages, False, False) //
    .AddParameter('can_send_media_messages', CanSendMediaMessages, False, False)
  //
    .AddParameter('can_send_other_messages', CanSendOtherMessages, False, False)
  //
    .AddParameter('can_add_web_page_previews', CanAddWebPagePreviews, False, False) //
    .ExecuteAsBool;
end;
{$ENDREGION}
{$REGION 'Stickers'}

function TTelegramBot.addStickerToSet(const UserId: Int64; const Name: string;
  const PngSticker: TtgFileToSend; const Emojis: string; const MaskPosition:
  TtgMaskPosition): Boolean;
begin
  Result := FRequest.SetMethod('addStickerToSet') //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('name', Name, '', False) //
    .AddParameter('png_sticker', PngSticker, nil, False) //
    .AddParameter('emojis', Emojis, '', False) //
    .AddParameter('mask_position', MaskPosition, nil, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.createNewStickerSet(const UserId: Int64; const Name, Title:
  string; const PngSticker: TtgFileToSend; const Emojis: string; const
  ContainsMasks: Boolean; const MaskPosition: TtgMaskPosition): Boolean;
begin
  Result := FRequest.SetMethod('createNewStickerSet') //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('name', Name, '', False) //
    .AddParameter('title', Title, '', False) //
    .AddParameter('png_sticker', PngSticker, nil, False) //
    .AddParameter('emojis', Emojis, '', False) //
    .AddParameter('contains_masks', ContainsMasks, False, False) //
    .AddParameter('mask_position', MaskPosition, nil, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.deleteStickerFromSet(const Sticker: string): Boolean;
begin
  Result := FRequest.SetMethod('deleteStickerFromSet') //
    .AddParameter('sticker', Sticker, '', False) //
    .ExecuteAsBool;
end;

function TTelegramBot.getStickerSet(const Name: string): TtgStickerSet;
begin
  Result := TtgStickerSet.Create(FRequest.SetMethod('deleteStickerFromSet') //
    .AddParameter('name', Name, '', True).Execute);
end;

function TTelegramBot.SendSticker(const ChatId: TtgUserLink; const Sticker:
  TtgFileToSend; const DisableNotification: Boolean; const ReplyToMessageId:
  Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendSticker') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('sticker', Sticker, nil, True) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.setStickerPositionInSet(const Sticker: string; const
  Position: Int64): Boolean;
begin
  Result := FRequest.SetMethod('deleteStickerFromSet') //
    .AddParameter('sticker', Sticker, '', True) //
    .AddParameter('position', Position, 0, True) //
    .ExecuteAsBool;
end;

function TTelegramBot.uploadStickerFile(const UserId: Int64; const PngSticker:
  TtgFileToSend): ItgFile;
begin
  Result := TtgFile.Create(FRequest.SetMethod('uploadStickerFile') //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('png_sticker', PngSticker, nil, True) //
    .Execute);
end;
{$ENDREGION}
{$REGION 'Inline mode'}

function TTelegramBot.AnswerInlineQuery(const InlineQueryId: string; const
  Results: TArray<TtgInlineQueryResult>; const CacheTime: Int64; const
  IsPersonal: Boolean; const NextOffset, SwitchPmText, SwitchPmParameter: string):
  Boolean;
begin
  Result := FRequest.SetMethod('answerInlineQuery') //
    .AddParameter('inline_query_id', InlineQueryId, '', True) //
    .AddParameter('results', TJsonUtils.ArrayToJString<TtgInlineQueryResult>(Results),
    '[]', True) //
    .AddParameter('cache_time', CacheTime, 0, False) //
    .AddParameter('is_personal', IsPersonal, False, False) //
    .AddParameter('next_offset', NextOffset, '', False) //
    .AddParameter('switch_pm_text', SwitchPmText, '', False) //
    .AddParameter('switch_pm_parameter', SwitchPmParameter, '', False) //
    .ExecuteAsBool;
end;

{$ENDREGION}
{$REGION 'Payments'}

function TTelegramBot.SendInvoice(const ChatId: Int64; const Title: string;
  const Description: string; const Payload: string; const ProviderToken: string;
  const StartParameter: string; const Currency: string; const Prices: TArray<
  TtgLabeledPrice>; const ProviderData: string; const PhotoUrl: string; const
  PhotoSize: Int64; const PhotoWidth: Int64; const PhotoHeight: Int64; const
  NeedName: Boolean; const NeedPhoneNumber: Boolean; const NeedEmail: Boolean;
  const NeedShippingAddress: Boolean; const IsFlexible: Boolean; const
  DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup:
  IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendInvoice') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('title', Title, '', True) //
    .AddParameter('description', Description, '', True) //
    .AddParameter('payload', Payload, '', True) //
    .AddParameter('provider_token', ProviderToken, '', True) //
    .AddParameter('start_parameter', StartParameter, '', True) //
    .AddParameter('currency', Currency, '', True) //
    .AddParameter('prices', TJsonUtils.ArrayToJString<TtgLabeledPrice>(Prices),
    '', True) //
    .AddParameter('provider_data', ProviderData, '', False) //
    .AddParameter('photo_url', PhotoUrl, '', False) //
    .AddParameter('photo_size', PhotoSize, 0, False) //
    .AddParameter('photo_width', PhotoWidth, 0, False) //
    .AddParameter('photo_height', PhotoHeight, 0, False) //
    .AddParameter('need_name', NeedName, False, False) //
    .AddParameter('need_phone_number', NeedPhoneNumber, False, False) //
    .AddParameter('need_email', NeedEmail, False, False) //
    .AddParameter('need_shipping_address', NeedShippingAddress, False, False) //
    .AddParameter('is_flexible', IsFlexible, False, False) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.AnswerPreCheckoutQueryBad(const PreCheckoutQueryId,
  ErrorMessage: string): Boolean;
begin
  Result := FRequest.SetMethod('answerPreCheckoutQuery') //
    .AddParameter('pre_checkout_query_id', PreCheckoutQueryId, 0, True) //
    .AddParameter('ok', False, True, False) //
    .AddParameter('error_message', ErrorMessage, '', True) //
    .ExecuteAsBool;
end;

function TTelegramBot.AnswerPreCheckoutQueryGood(const PreCheckoutQueryId:
  string): Boolean;
begin
  Result := FRequest.SetMethod('answerPreCheckoutQuery') //
    .AddParameter('pre_checkout_query_id', PreCheckoutQueryId, 0, True) //
    .AddParameter('ok', True, False, False) //
    .ExecuteAsBool;
end;

function TTelegramBot.AnswerShippingQueryBad(const ShippingQueryId, ErrorMessage:
  string): Boolean;
begin
  Result := FRequest.SetMethod('answerShippingQuery') //
    .AddParameter('Shipping_query_id', ShippingQueryId, 0, True) //
    .AddParameter('ok', False, False, False) //
    .AddParameter('error_message', ErrorMessage, '', False) //
    .ExecuteAsBool;
end;

function TTelegramBot.AnswerShippingQueryGood(const ShippingQueryId: string;
  const ShippingOptions: TArray<TtgShippingOption>): Boolean;
begin
  Result := FRequest.SetMethod('answerShippingQuery') //
    .AddParameter('Shipping_query_id', ShippingQueryId, 0, True) //
    .AddParameter('ok', True, False, False) //
    .AddParameter('Shipping_options', TJsonUtils.ArrayToJString<
    TtgShippingOption>(ShippingOptions), '[]', True) //
    .ExecuteAsBool;
end;

procedure TTelegramBot.AssignTo(Dest: TPersistent);
begin
  if not (Assigned(Dest) or (Dest is TTelegramBot)) then
    Exit;
  (Dest as TTelegramBot).Token := Self.Token;
  (Dest as TTelegramBot).HttpCore := Self.HttpCore;
  (Dest as TTelegramBot).FExceptionManager := Self.FExceptionManager;
  (Dest as TTelegramBot).OnReceiveRawData := Self.OnReceiveRawData;
  (Dest as TTelegramBot).OnSendData := Self.OnSendData;
 // inherited AssignTo(Dest);
end;

{$ENDREGION}
{$REGION 'Games'}

function TTelegramBot.GetGameHighScores(const UserId: Int64; const
  InlineMessageId: string): TArray<ItgGameHighScore>;
begin
  Result := GetArrayFromMethod<ItgGameHighScore>(TtgGameHighScore, FRequest.SetMethod
    ('getGameHighScores') //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('inline_message_id', InlineMessageId, 0, True) //
    .Execute);
end;

function TTelegramBot.GetGameHighScores(const UserId, ChatId, MessageId: Int64):
  TArray<ItgGameHighScore>;
begin
  Result := GetArrayFromMethod<ItgGameHighScore>(TtgGameHighScore, FRequest.SetMethod
    ('getGameHighScores') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('message_id', MessageId, 0, True) //
    .Execute);
end;

function TTelegramBot.GetHttpCore: IcuHttpClient;
begin
  Result := FRequest.HttpCore;
end;

function TTelegramBot.SendGame(const ChatId: Int64; const GameShortName: string;
  const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup:
  IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('sendGame') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('game_short_name', GameShortName, '', True) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .Execute);
end;

function TTelegramBot.SetGameScore(const UserId, Score: Int64; const
  InlineMessageId: string; const Force, DisableEditMessage: Boolean): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('setGameScore') //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('score', Score, 0, True) //
    .AddParameter('force', Force, False, False) //
    .AddParameter('disable_edit_message', DisableEditMessage, False, False) //
    .AddParameter('inline_message_id', InlineMessageId, 0, True) //
    .Execute);
end;

function TTelegramBot.SetGameScore(const UserId, Score, ChatId, MessageId: Int64;
  const Force, DisableEditMessage: Boolean): ITgMessage;
begin
  Result := TTgMessage.Create(FRequest.SetMethod('setGameScore') //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('score', Score, 0, True) //
    .AddParameter('force', Force, False, False) //
    .AddParameter('disable_edit_message', DisableEditMessage, False, False) //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('message_id', MessageId, 0, True) //
    .Execute);
end;

procedure TTelegramBot.SetHttpCore(const Value: IcuHttpClient);
begin
  FRequest.HttpCore := Value;
end;

{$ENDREGION}

constructor TTelegramBot.Create(const AToken: string; ACore: IcuHttpClient);
begin
  Self.Create(AToken);
  HttpCore := ACore;
end;

end.

