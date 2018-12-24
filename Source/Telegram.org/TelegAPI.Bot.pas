{***************************************************************************}
{                                                                           }
{           CloudApi for Delphi                                             }
{                                                                           }
{           Copyright (c) 2014-2018 Maxim Sysoev                            }
{                                                                           }
{           https://t.me/CloudAPI                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit TelegAPI.Bot;

interface

uses
  CloudAPI.BaseComponent,
  CloudAPI.Exception,
  CloudAPI.Request,
  CloudAPI.Types,
  CloudAPI.Utils.JSON,
  System.Classes,
  System.JSON,
  System.SysUtils,
  System.TypInfo,
  TelegAPI.Types,
  TelegAPI.Types.Enums,
  TelegAPI.Types.Impl,
  TelegAPI.Types.InlineQueryResults,
  TelegAPI.Types.ReplyMarkups;

type
  TTelegramBot = class(TCloudApiBaseComponent)
  private
    FToken: string;
    FDomain: string;
    function GetToken: string;
    procedure SetToken(const Value: string);
  protected
    procedure DoInitApiCore; override;
  public
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(const AToken: string); reintroduce; overload;
{$REGION 'Getting updates'}
    function GetUpdates( //
      const Offset: Int64 = 0; //
      const Limit: Int64 = 100; //
      const Timeout: Int64 = 0; //
      const AllowedUpdates: TAllowedUpdates = UPDATES_ALLOWED_ALL): TArray<ItgUpdate>; overload;
    function GetUpdates( //
      const JSON: string): TArray<ItgUpdate>; overload;
    function SetWebhook( //
      const Url: string; //
      const Certificate: TFileToSend; //
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
      const Photo: TFileToSend; //
      const Caption: string = ''; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendAudio( //
      const ChatId: TtgUserLink; //
      const Audio: TFileToSend; //
      const Thumb: TFileToSend; //
      const Caption: string = ''; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const Duration: Int64 = 0; //
      const Performer: string = ''; //
      const Title: string = ''; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendDocument( //
      const ChatId: TtgUserLink; //
      const Document: TFileToSend; //
      const Caption: string = ''; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVideo( //
      const ChatId: TtgUserLink; //
      const Video: TFileToSend; //
      const Caption: string = ''; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const SupportsStreaming: Boolean = True; //
      const Duration: Int64 = 0; //
      const Width: Int64 = 0; //
      const Height: Int64 = 0; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendAnimation( //
      const ChatId: TtgUserLink; //
      const Animation: TFileToSend; //
      const Thumb: TFileToSend; //
      const Caption: string = ''; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const Duration: Int64 = 0; //
      const Width: Int64 = 0; //
      const Height: Int64 = 0; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVoice( //
      const ChatId: TtgUserLink; //
      const Voice: TFileToSend; //
      const Caption: string = ''; //
      const ParseMode: TtgParseMode = TtgParseMode.Default; //
      const Duration: Int64 = 0; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function SendVideoNote( //
      const ChatId: TtgUserLink; //
      const VideoNote: TFileToSend; //
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
      const Offset: Int64 = 0; //
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
    function SetChatDescription(const ChatId: TtgUserLink; const Description: string): Boolean;
    function SetChatPhoto(const ChatId: TtgUserLink; const Photo: TFileToSend): Boolean;
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
      const Sticker: TFileToSend; //
      const DisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0; //
      ReplyMarkup: IReplyMarkup = nil): ITgMessage;
    function getStickerSet(const Name: string): TtgStickerSet;
    function uploadStickerFile(const UserId: Int64; const PngSticker: TFileToSend): ItgFile;
    function createNewStickerSet( //
      const UserId: Int64; //
      const Name, Title: string; //
      const PngSticker: TFileToSend; //
      const Emojis: string; //
      const ContainsMasks: Boolean = False; //
      const MaskPosition: TtgMaskPosition = nil): Boolean;
    function addStickerToSet( //
      const UserId: Int64; //
      const Name: string; //
      const PngSticker: TFileToSend; //
      const Emojis: string; //
      const MaskPosition: TtgMaskPosition = nil): Boolean;
    function setStickerPositionInSet(const Sticker: string; const Position: Int64): Boolean;
    function deleteStickerFromSet(const Sticker: string): Boolean;
    function setChatStickerSet(const ChatId: TtgUserLink; const StickerSetName: string): Boolean;
    function deleteChatStickerSet(const ChatId: TtgUserLink): Boolean;
    function sendMediaGroup( //
      const ChatId: TtgUserLink; //
      const AMedia: TArray<TtgInputMedia>; //
      const ADisableNotification: Boolean = False; //
      const ReplyToMessageId: Int64 = 0): TArray<ITgMessage>;

{$ENDREGION}
  published
{$REGION 'Property|Свойства'}
    property Domain read FDomain write FDomain;
    property Token: string read GetToken write SetToken;
{$ENDREGION}
{$REGION 'События|Events'}
    property OnReceiveRawData;
    property OnSendData;
    property OnError;
{$ENDREGION}
  end;

implementation

uses
  REST.JSON,
  TelegAPI.Helpers;

{ TTelegramBot }
{$REGION 'Core'}

procedure TTelegramBot.AssignTo(Dest: TPersistent);
begin
  if not(Assigned(Dest) or (Dest is TTelegramBot)) then
    Exit;
  (Dest as TTelegramBot).Token := Self.Token;
  (Dest as TTelegramBot).Proxy := Self.Proxy;
  (Dest as TTelegramBot).OnReceiveRawData := Self.OnReceiveRawData;
  (Dest as TTelegramBot).OnSendData := Self.OnSendData;
  (Dest as TTelegramBot).OnError := Self.OnError;
  // inherited AssignTo(Dest);
end;

constructor TTelegramBot.Create(const AToken: string);
begin
  inherited Create(nil);
  SetToken(AToken);
end;

procedure TTelegramBot.DoInitApiCore;
begin
  inherited;
  Domain := 'https://api.telegram.org/bot';
  GetRequest.OnDataReceiveAsString := function(AData: string): string
    var
      LJSON: TJSONObject;
      LExcCode: Integer;
      LExcDesc: string;
    begin
      if Assigned(OnReceiveRawData) then
        OnReceiveRawData(Self, AData);
      Result := '';
      if AData.IsEmpty or AData.StartsWith('<html') then
        Exit;
      LJSON := TJSONObject.ParseJSONValue(AData) as TJSONObject;
      try
        if LJSON.GetValue('ok') is TJSONFalse then
        begin
          LExcCode := (LJSON.GetValue('error_code') as TJSONNumber).AsInt;
          LExcDesc := (LJSON.GetValue('description') as TJSONString).Value;
          DoCallLogEvent(ECloudApiException.Create(LExcCode, LExcDesc), True);
        end
        else
        begin
          Result := LJSON.GetValue('result').ToString;
          Result := Result.Replace('\n', #13#10);
          Result := Result.Replace('</br>', #13#10);
          Result := Result.Replace('<br>', #13#10);
        end;
      finally
        LJSON.Free;
      end;
    end;
end;

function TTelegramBot.GetToken: string;
begin
  Result := FToken;
end;

procedure TTelegramBot.SetToken(const Value: string);
begin
  FToken := Value;
  GetRequest.Domain := FDomain + Token;
end;

{$ENDREGION}
{$REGION 'Getting updates'}

function TTelegramBot.SetWebhook(const Url: string; const Certificate: TFileToSend; const MaxConnections: Int64;
  const AllowedUpdates: TAllowedUpdates): Boolean;
begin
  with (GetRequest) do
  begin
    SetMethod('setWebhook');
    AddParameter('url', Url, '', True, TStoreFormat.InFormData);
    AddParameter('certificate', Certificate, TFileToSend.Empty, False, TStoreFormat.InFormData);
    AddParameter('max_connections', MaxConnections, 0, False, TStoreFormat.InFormData);
    AddParameter('allowed_updates', AllowedUpdates.ToString, '[]', False, TStoreFormat.InFormData);
    Result := ExecuteAsBool;
  end;
end;

function TTelegramBot.GetWebhookInfo: ItgWebhookInfo;
begin
  Result := TtgWebhookInfo.Create(GetRequest.SetMethod('getWebhookInfo').ExecuteAsString);
end;

function TTelegramBot.GetUpdates(const Offset, Limit, Timeout: Int64; const AllowedUpdates: TAllowedUpdates)
  : TArray<ItgUpdate>;
begin
  Result := TBaseJson.AsArray<ItgUpdate>(TtgUpdate, GetRequest.SetMethod('getUpdates') //
    .AddParameter('offset', Offset, 0, False, TStoreFormat.InUrl) //
    .AddParameter('limit', Limit, 100, False, TStoreFormat.InUrl) //
    .AddParameter('timeout', Timeout, 0, False, TStoreFormat.InUrl) //
    .AddParameter('allowed_updates', AllowedUpdates.ToString, '[]', False, TStoreFormat.InUrl) //
    .ExecuteAsString);
end;

function TTelegramBot.GetUpdates(const JSON: string): TArray<ItgUpdate>;
begin
  Result := TBaseJson.AsArray<ItgUpdate>(TtgUpdate, JSON);
end;

function TTelegramBot.DeleteWebhook: Boolean;
begin
  Result := GetRequest.SetMethod('deleteWebhook').ExecuteAsBool;
end;

{$ENDREGION}
{$REGION 'Basic methods'}

function TTelegramBot.stopMessageLiveLocation(const ChatId: TtgUserLink; const MessageId: Int64;
  ReplyMarkup: IReplyMarkup): Boolean;
begin
  with (GetRequest) do
  begin
    SetMethod('stopMessageLiveLocation');
    AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData);
    AddParameter('message_id', MessageId, 0, True, TStoreFormat.InFormData);
    AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData);
    Result := ExecuteAsBool;
  end;
end;

function TTelegramBot.stopMessageLiveLocation(const InlineMessageId: string; ReplyMarkup: IReplyMarkup): Boolean;
begin
  with (GetRequest) do
  begin
    SetMethod('stopMessageLiveLocation');
    AddParameter('inline_message_id', InlineMessageId, '', True, TStoreFormat.InFormData);
    AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData);
    Result := ExecuteAsBool;
  end;
end;

function TTelegramBot.UnbanChatMember(const ChatId: TtgUserLink; const UserId: Int64): Boolean;
begin
  with (GetRequest) do
  begin
    SetMethod('unbanChatMember');
    AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl);
    AddParameter('user_id', UserId, 0, True, TStoreFormat.InUrl);
    Result := ExecuteAsBool;
  end;
end;

function TTelegramBot.SendLocation(const ChatId: TtgUserLink; const Location: TtgLocation; const LivePeriod: Int64;
  const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('unbanChatMember') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData)
    //
    .AddParameter('latitude', Location.Latitude, 0.0, True, TStoreFormat.InFormData) //
    .AddParameter('longitude', Location.Longitude, 0.0, True, TStoreFormat.InFormData) //
    .AddParameter('live_period', LivePeriod, 0, False, TStoreFormat.InFormData)
    //
    .AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InFormData) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.sendMediaGroup(const ChatId: TtgUserLink; const AMedia: TArray<TtgInputMedia>;
  const ADisableNotification: Boolean; const ReplyToMessageId: Int64): TArray<ITgMessage>;
var
  LRequest: IApiRequest;
  LMedia: TtgInputMedia;
  LTmpJson: string;
begin
  LTmpJson := TJsonUtils.ArrayToJString<TtgInputMedia>(AMedia);
  LRequest := (GetRequest.SetMethod('sendMediaGroup') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData)
    //
    .AddParameter('media', LTmpJson, '[]', True, TStoreFormat.InFormData) //
    .AddParameter('disable_notification', ADisableNotification, False, False, TStoreFormat.InFormData) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData));
  for LMedia in AMedia do
  begin
    case LMedia.GetFileToSend.Tag of
      TFileToSendTag.FromFile:
        LRequest.StoreMultipartForm.AddFile(ExtractFileName(LMedia.GetFileToSend.Data), LMedia.GetFileToSend.Data);
      TFileToSendTag.FromStream:
        LRequest.StoreMultipartForm.AddStream(LMedia.GetFileToSend.Data, LMedia.GetFileToSend.Content,
          LMedia.GetFileToSend.Data);
    end;
  end;
  Result := TBaseJson.AsArray<ITgMessage>(TTgMessage, LRequest.ExecuteAsString);
end;

function TTelegramBot.SendPhoto(const ChatId: TtgUserLink; const Photo: TFileToSend; const Caption: string;
  const ParseMode: TtgParseMode; const DisableNotification: Boolean; const ReplyToMessageId: Int64;
  ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('sendPhoto') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData)
    //
    .AddParameter('photo', Photo, TFileToSend.Empty, True, TStoreFormat.InFormData) //
    .AddParameter('caption', Caption, '', False, TStoreFormat.InFormData) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False, TStoreFormat.InFormData) //
    .AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InFormData) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.SendMessage(const ChatId: TtgUserLink; const Text: string; const ParseMode: TtgParseMode;
  const DisableWebPagePreview, DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup)
  : ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('sendMessage') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('text', Text, '', True, TStoreFormat.InUrl) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False, TStoreFormat.InUrl) //
    .AddParameter('disable_web_page_preview', DisableWebPagePreview, False, False, TStoreFormat.InUrl) //
    .AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InUrl) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InUrl) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.SendVenue(const ChatId: TtgUserLink; const Venue: TtgVenue; const DisableNotification: Boolean;
  const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  with GetRequest do
  begin
    SetMethod('sendVenue');
    AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData);
    AddParameter('latitude', Venue.Location.Latitude, 0, True, TStoreFormat.InFormData);
    AddParameter('longitude', Venue.Location.Longitude, 0, True, TStoreFormat.InFormData);
    AddParameter('title', Venue.Title, '', True, TStoreFormat.InFormData);
    AddParameter('address', Venue.Address, '', True, TStoreFormat.InFormData);
    AddParameter('foursquare_id', Venue.FoursquareId, '', False, TStoreFormat.InFormData);
    AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InFormData);
    AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData);
    AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData);
    Result := TTgMessage.Create(ExecuteAsString);
  end;
end;

function TTelegramBot.SendVideo(const ChatId: TtgUserLink; const Video: TFileToSend; const Caption: string;
  const ParseMode: TtgParseMode; const SupportsStreaming: Boolean; const Duration, Width, Height: Int64;
  const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('sendVideo') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('video', Video, TFileToSend.Empty, True, TStoreFormat.InFormData) //
    .AddParameter('duration', Duration, 0, False, TStoreFormat.InFormData) //
    .AddParameter('width', Width, 0, False, TStoreFormat.InFormData) //
    .AddParameter('height', Height, 0, False, TStoreFormat.InFormData) //
    .AddParameter('caption', Caption, '', False, TStoreFormat.InFormData) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False, TStoreFormat.InFormData) //
    .AddParameter('supports_streaming', SupportsStreaming, False, False, TStoreFormat.InFormData) //
    .AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InFormData) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.SendVideoNote(const ChatId: TtgUserLink; const VideoNote: TFileToSend;
  const Duration, Length: Int64; const DisableNotification: Boolean; const ReplyToMessageId: Int64;
  ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('sendVideoNote') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('video_note', VideoNote, TFileToSend.Empty, True, TStoreFormat.InFormData) //
    .AddParameter('duration', Duration, 0, False, TStoreFormat.InFormData) //
    .AddParameter('length', Length, 0, False, TStoreFormat.InFormData) //
    .AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InFormData) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.SendVoice(const ChatId: TtgUserLink; const Voice: TFileToSend; const Caption: string;
  const ParseMode: TtgParseMode; const Duration: Int64; const DisableNotification: Boolean;
  const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('sendVoice') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('voice', Voice, TFileToSend.Empty, True, TStoreFormat.InFormData) //
    .AddParameter('duration', Duration, 0, False, TStoreFormat.InFormData) //
    .AddParameter('caption', Caption, '', False, TStoreFormat.InFormData) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False, TStoreFormat.InFormData) //
    .AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InFormData) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.SendAnimation(const ChatId: TtgUserLink; const Animation, Thumb: TFileToSend;
  const Caption: string; const ParseMode: TtgParseMode; const Duration, Width, Height: Int64;
  const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('sendAnimation') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('animation', Animation, TFileToSend.Empty, True, TStoreFormat.InFormData) //
    .AddParameter('thumb', Thumb, TFileToSend.Empty, False, TStoreFormat.InFormData) //
    .AddParameter('duration', Duration, 0, False, TStoreFormat.InFormData) //
    .AddParameter('width', Width, 0, False, TStoreFormat.InFormData) //
    .AddParameter('height', Height, 0, False, TStoreFormat.InFormData) //
    .AddParameter('caption', Caption, '', False, TStoreFormat.InFormData) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False, TStoreFormat.InFormData) //
    .AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InFormData) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.SendAudio(const ChatId: TtgUserLink; const Audio: TFileToSend; const Thumb: TFileToSend;
  const Caption: string; const ParseMode: TtgParseMode; const Duration: Int64; const Performer, Title: string;
  const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  with GetRequest do
  begin
    SetMethod('sendAudio');
    AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl);
    AddParameter('audio', Audio, TFileToSend.Empty, True, TStoreFormat.InFormData);
    AddParameter('thumb', Thumb, TFileToSend.Empty, True, TStoreFormat.InFormData);
    AddParameter('caption', Caption, '', False, TStoreFormat.InUrl);
    AddParameter('parse_mode', ParseMode.ToString, '', False, TStoreFormat.InUrl);
    AddParameter('duration', Duration, 0, False, TStoreFormat.InUrl);
    AddParameter('performer', Performer, '', False, TStoreFormat.InUrl);
    AddParameter('title', Title, '', False, TStoreFormat.InUrl);
    AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InUrl);
    AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InUrl);
    AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InUrl);
    Result := TTgMessage.Create(ExecuteAsString);
  end;
end;

function TTelegramBot.SendChatAction(const ChatId: TtgUserLink; const Action: TtgSendChatAction): Boolean;
begin
  Result := GetRequest.SetMethod('sendChatAction') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('action', Action.ToString, '', True, TStoreFormat.InUrl) //
    .ExecuteAsBool;
end;

function TTelegramBot.SendContact(const ChatId: TtgUserLink; const Contact: TtgContact;
  const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  with GetRequest do
  begin
    SetMethod('sendContact');
    AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData);
    AddParameter('phone_number', Contact.PhoneNumber, '', True, TStoreFormat.InFormData);
    AddParameter('first_name', Contact.FirstName, '', True, TStoreFormat.InFormData);
    AddParameter('last_name', Contact.LastName, '', False, TStoreFormat.InFormData);
    AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InFormData);
    AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData);
    AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData);
    Result := TTgMessage.Create(ExecuteAsString);
  end;
end;

function TTelegramBot.SendDocument(const ChatId: TtgUserLink; const Document: TFileToSend; const Caption: string;
  const ParseMode: TtgParseMode; const DisableNotification: Boolean; const ReplyToMessageId: Int64;
  ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  with GetRequest do
  begin
    SetMethod('sendDocument');
    AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData);
    AddParameter('document', Document, TFileToSend.Empty, True, TStoreFormat.InFormData);
    AddParameter('caption', Caption, '', False, TStoreFormat.InFormData);
    AddParameter('parse_mode', ParseMode.ToString, '', False, TStoreFormat.InFormData);
    AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InFormData);
    AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData);
    AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData);
    Result := TTgMessage.Create(ExecuteAsString);
  end;
end;

function TTelegramBot.KickChatMember(const ChatId: TtgUserLink; const UserId: Int64;
  const UntilDate: TDateTime): Boolean;
begin
  Result := GetRequest.SetMethod('kickChatMember') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('user_id', UserId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('until_date', UntilDate, 0, False, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.LeaveChat(const ChatId: TtgUserLink): Boolean;
begin
  Result := GetRequest.SetMethod('leaveChat') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .ExecuteAsBool;
end;

function TTelegramBot.GetUserProfilePhotos(const ChatId: TtgUserLink; const Offset, Limit: Int64): ItgUserProfilePhotos;
begin
  Result := TtgUserProfilePhotos.Create(GetRequest.SetMethod('getUserProfilePhotos') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('offset', Offset, 0, False, TStoreFormat.InUrl) //
    .AddParameter('limit', Limit, 100, False, TStoreFormat.InUrl) //
    .ExecuteAsString);
end;

function TTelegramBot.GetMe: ItgUser;
begin
  Result := TtgUser.Create(GetRequest.SetMethod('getMe').ExecuteAsString);
end;

function TTelegramBot.ForwardMessage(const ChatId, FromChatId: TtgUserLink; const MessageId: Int64;
  const DisableNotification: Boolean): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('forwardMessage') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('from_chat_id', FromChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InUrl) //
    .AddParameter('message_id', MessageId, 0, False, TStoreFormat.InUrl) //
    .ExecuteAsString);
end;

function TTelegramBot.GetChat(const ChatId: TtgUserLink): ItgChat;
begin
  Result := TtgChat.Create(GetRequest.SetMethod('getChat').//
    AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl).ExecuteAsString);
end;

function TTelegramBot.GetChatAdministrators(const ChatId: TtgUserLink): TArray<ItgChatMember>;
begin
  Result := TBaseJson.AsArray<ItgChatMember>(TtgChatMember, //
    GetRequest.SetMethod('getChatAdministrators') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl).ExecuteAsString);
end;

function TTelegramBot.GetChatMember(const ChatId: TtgUserLink; const UserId: Int64): ItgChatMember;
begin
  Result := TtgChatMember.Create(GetRequest.SetMethod('getChatMember') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('user_id', UserId, 0, True, TStoreFormat.InUrl) //
    .ExecuteAsString);
end;

function TTelegramBot.GetChatMembersCount(const ChatId: TtgUserLink): Int64;
var
  LJSON: TJSONValue;
begin
  LJSON := TJSONObject.ParseJSONValue(GetRequest.SetMethod('getChatMembersCount') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl).ExecuteAsString);
  try
    if not LJSON.TryGetValue<Int64>(Result) then
      Result := 0;
  finally
    LJSON.Free;
  end;
end;

function TTelegramBot.GetFile(const FileId: string): ItgFile;
begin
  Result := TtgFile.Create(GetRequest.SetMethod('getFile') //
    .AddParameter('file_id', FileId, '', True, TStoreFormat.InUrl).ExecuteAsString);
end;

function TTelegramBot.AnswerCallbackQuery(const CallbackQueryId, Text: string; const ShowAlert: Boolean;
  const Url: string; const CacheTime: Int64): Boolean;
begin
  Result := GetRequest.SetMethod('answerCallbackQuery') //
    .AddParameter('callback_query_id', CallbackQueryId, '', True, TStoreFormat.InUrl) //
    .AddParameter('text', Text, '', False, TStoreFormat.InUrl) //
    .AddParameter('show_alert', ShowAlert, False, False, TStoreFormat.InUrl) //
    .AddParameter('url', Url, '', False, TStoreFormat.InUrl) //
    .AddParameter('cache_time', CacheTime, 0, False, TStoreFormat.InUrl) //
    .ExecuteAsString = 'true';
end;
{$ENDREGION}
{$REGION 'Updating messages'}

function TTelegramBot.EditMessageText(const InlineMessageId, Text: string; const ParseMode: TtgParseMode;
  const DisableWebPagePreview: Boolean; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('editMessageText') //
    .AddParameter('inline_message_id', InlineMessageId, '', True, TStoreFormat.InFormData) //
    .AddParameter('text', Text, '', True, TStoreFormat.InFormData) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False, TStoreFormat.InFormData) //
    .AddParameter('disable_web_page_preview', DisableWebPagePreview, False, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.EditMessageText(const ChatId: TtgUserLink; const MessageId: Int64; const Text: string;
  const ParseMode: TtgParseMode; const DisableWebPagePreview: Boolean; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('editMessageText') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('message_id', MessageId, 0, True, TStoreFormat.InFormData) //
    .AddParameter('text', Text, '', True, TStoreFormat.InFormData) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False, TStoreFormat.InFormData) //
    .AddParameter('disable_web_page_preview', DisableWebPagePreview, False, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.DeleteMessage(const ChatId: TtgUserLink; const MessageId: Int64): Boolean;
begin
  Result := GetRequest.SetMethod('deleteMessage') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('message_id', MessageId, 0, True, TStoreFormat.InUrl) //
    .ExecuteAsBool;
end;

function TTelegramBot.EditMessageCaption(const ChatId: TtgUserLink; const MessageId: Int64; const Caption: string;
  const ParseMode: TtgParseMode; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := GetRequest.SetMethod('editMessageText') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('message_id', MessageId, 0, True, TStoreFormat.InFormData) //
    .AddParameter('caption', Caption, '', True, TStoreFormat.InFormData) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.EditMessageCaption(const InlineMessageId, Caption: string; const ParseMode: TtgParseMode;
  ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := GetRequest.SetMethod('editMessageCaption') //
    .AddParameter('inline_message_id', InlineMessageId, '', True, TStoreFormat.InFormData) //
    .AddParameter('caption', Caption, '', True, TStoreFormat.InFormData) //
    .AddParameter('parse_mode', ParseMode.ToString, '', False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.editMessageLiveLocation(const ChatId: TtgUserLink; const MessageId: Int64;
  const Location: TtgLocation; ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := GetRequest.SetMethod('editMessageLiveLocation') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('message_id', MessageId, 0, True, TStoreFormat.InFormData) //
    .AddParameter('latitude', Location.Latitude, 0, False, TStoreFormat.InFormData) //
    .AddParameter('longitude', Location.Longitude, 0, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.editMessageLiveLocation(const InlineMessageId: string; const Location: TtgLocation;
  ReplyMarkup: IReplyMarkup): Boolean;
begin
  Result := GetRequest.SetMethod('editMessageLiveLocation') //
    .AddParameter('inline_message_id', InlineMessageId, '', True, TStoreFormat.InFormData) //
    .AddParameter('latitude', Location.Latitude, 0, False, TStoreFormat.InFormData) //
    .AddParameter('longitude', Location.Longitude, 0, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.EditMessageReplyMarkup(const ChatId: TtgUserLink; const MessageId: Int64;
  ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('editMessageReplyMarkup') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('message_id', MessageId, 0, True, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.EditMessageReplyMarkup(const InlineMessageId: string; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('editMessageReplyMarkup') //
    .AddParameter('inline_message_id', InlineMessageId, '', True, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;
{$ENDREGION}
{$REGION 'Manage groups and channels'}

function TTelegramBot.DeleteChatPhoto(const ChatId: TtgUserLink): Boolean;
begin
  Result := GetRequest.SetMethod('deleteChatPhoto') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl).ExecuteAsBool;
end;

function TTelegramBot.deleteChatStickerSet(const ChatId: TtgUserLink): Boolean;
begin
  Result := GetRequest.SetMethod('deleteChatStickerSet') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl).ExecuteAsBool;
end;

function TTelegramBot.ExportChatInviteLink(const ChatId: TtgUserLink): string;
begin
  Result := GetRequest.SetMethod('deleteChatStickerSet') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl).ExecuteAndReadValue;
end;

function TTelegramBot.PinChatMessage(const ChatId: TtgUserLink; const MessageId: Int64;
  const DisableNotification: Boolean): Boolean;
begin
  Result := GetRequest.SetMethod('pinChatMessage') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('message_id', MessageId, 0, True, TStoreFormat.InUrl) //
    .AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InUrl) //
    .ExecuteAsBool;
end;

function TTelegramBot.SetChatDescription(const ChatId: TtgUserLink; const Description: string): Boolean;
begin
  Result := GetRequest.SetMethod('setChatDescription') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('description', Description, '', True, TStoreFormat.InUrl) //
    .ExecuteAsBool;
end;

function TTelegramBot.SetChatPhoto(const ChatId: TtgUserLink; const Photo: TFileToSend): Boolean;
begin
  Result := GetRequest.SetMethod('setChatDescription') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('photo', Photo, TFileToSend.Empty, True, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.setChatStickerSet(const ChatId: TtgUserLink; const StickerSetName: string): Boolean;
begin
  Result := GetRequest.SetMethod('setChatStickerSet') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('sticker_set_name', StickerSetName, '', True, TStoreFormat.InUrl) //
    .ExecuteAsBool;
end;

function TTelegramBot.SetChatTitle(const ChatId: TtgUserLink; const Title: string): Boolean;
begin
  Result := GetRequest.SetMethod('setChatTitle') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('title', Title, '', True, TStoreFormat.InUrl) //
    .ExecuteAsBool;
end;

function TTelegramBot.UnpinChatMessage(const ChatId: TtgUserLink): Boolean;
begin
  Result := GetRequest.SetMethod('unpinChatMessage') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl).ExecuteAsBool;
end;

{$ENDREGION}
{$REGION 'Manage users and admins'}

function TTelegramBot.PromoteChatMember(const ChatId: TtgUserLink; const UserId: Int64;
  const CanChangeInfo, CanPostMessages, CanEditMessages, CanDeleteMessages, CanInviteUsers, CanRestrictMembers,
  CanPinMessages, CanPromoteMembers: Boolean): Boolean;
begin
  Result := GetRequest.SetMethod('promoteChatMember') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('user_id', UserId, 0, True, TStoreFormat.InUrl) //
    .AddParameter('can_change_info', CanChangeInfo, False, False, TStoreFormat.InUrl) //
    .AddParameter('can_post_messages', CanPostMessages, False, False, TStoreFormat.InUrl) //
    .AddParameter('can_edit_messages', CanEditMessages, False, False, TStoreFormat.InUrl) //
    .AddParameter('can_delete_messages', CanDeleteMessages, False, False, TStoreFormat.InUrl) //
    .AddParameter('can_invite_users', CanInviteUsers, False, False, TStoreFormat.InUrl) //
    .AddParameter('can_restrict_members', CanRestrictMembers, False, False, TStoreFormat.InUrl) //
    .AddParameter('can_pin_messages', CanPinMessages, False, False, TStoreFormat.InUrl) //
    .AddParameter('can_promote_members', CanPromoteMembers, False, False, TStoreFormat.InUrl) //
    .ExecuteAsBool;
end;

function TTelegramBot.RestrictChatMember(const ChatId: TtgUserLink; const UserId: Int64; const UntilDate: TDateTime;
  const CanSendMessages, CanSendMediaMessages, CanSendOtherMessages, CanAddWebPagePreviews: Boolean): Boolean;
begin
  Result := GetRequest.SetMethod('restrictChatMember') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InUrl) //
    .AddParameter('user_id', UserId, 0, True, TStoreFormat.InUrl) //
    .AddParameter('until_date', UntilDate, 0, False, TStoreFormat.InUrl) //
    .AddParameter('can_send_messages', CanSendMessages, False, False, TStoreFormat.InUrl) //
    .AddParameter('can_send_media_messages', CanSendMediaMessages, False, False, TStoreFormat.InUrl) //
    .AddParameter('can_send_other_messages', CanSendOtherMessages, False, False, TStoreFormat.InUrl) //
    .AddParameter('can_add_web_page_previews', CanAddWebPagePreviews, False, False, TStoreFormat.InUrl) //
    .ExecuteAsBool;
end;
{$ENDREGION}
{$REGION 'Stickers'}

function TTelegramBot.addStickerToSet(const UserId: Int64; const Name: string; const PngSticker: TFileToSend;
  const Emojis: string; const MaskPosition: TtgMaskPosition): Boolean;
begin
  Result := GetRequest.SetMethod('addStickerToSet') //
    .AddParameter('user_id', UserId, 0, True, TStoreFormat.InUrl) //
    .AddParameter('name', Name, '', False, TStoreFormat.InUrl) //
    .AddParameter('png_sticker', PngSticker, TFileToSend.Empty, False, TStoreFormat.InUrl) //
    .AddParameter('emojis', Emojis, '', False, TStoreFormat.InUrl) //
    .AddParameter('mask_position', MaskPosition, nil, False, TStoreFormat.InUrl) //
    .ExecuteAsBool;
end;

function TTelegramBot.createNewStickerSet(const UserId: Int64; const Name, Title: string; const PngSticker: TFileToSend;
  const Emojis: string; const ContainsMasks: Boolean; const MaskPosition: TtgMaskPosition): Boolean;
begin
  Result := GetRequest.SetMethod('createNewStickerSet') //
    .AddParameter('user_id', UserId, 0, True, TStoreFormat.InFormData) //
    .AddParameter('name', Name, '', False, TStoreFormat.InFormData) //
    .AddParameter('title', Title, '', False, TStoreFormat.InFormData) //
    .AddParameter('png_sticker', PngSticker, TFileToSend.Empty, False, TStoreFormat.InFormData) //
    .AddParameter('emojis', Emojis, '', False, TStoreFormat.InFormData) //
    .AddParameter('contains_masks', ContainsMasks, False, False, TStoreFormat.InFormData) //
    .AddParameter('mask_position', MaskPosition, nil, False, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.deleteStickerFromSet(const Sticker: string): Boolean;
begin
  Result := GetRequest.SetMethod('deleteStickerFromSet') //
    .AddParameter('sticker', Sticker, '', False, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.getStickerSet(const Name: string): TtgStickerSet;
begin
  Result := TtgStickerSet.Create(GetRequest.SetMethod('deleteStickerFromSet') //
    .AddParameter('name', Name, '', True, TStoreFormat.InFormData).ExecuteAsString);
end;

function TTelegramBot.SendSticker(const ChatId: TtgUserLink; const Sticker: TFileToSend;
  const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('sendSticker') //
    .AddParameter('chat_id', ChatId.ToString, '', True, TStoreFormat.InFormData) //
    .AddParameter('sticker', Sticker, TFileToSend.Empty, True, TStoreFormat.InFormData) //
    .AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InFormData) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.setStickerPositionInSet(const Sticker: string; const Position: Int64): Boolean;
begin
  Result := GetRequest.SetMethod('deleteStickerFromSet') //
    .AddParameter('sticker', Sticker, '', True, TStoreFormat.InFormData) //
    .AddParameter('position', Position, 0, True, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.uploadStickerFile(const UserId: Int64; const PngSticker: TFileToSend): ItgFile;
begin
  Result := TtgFile.Create(GetRequest.SetMethod('uploadStickerFile') //
    .AddParameter('user_id', UserId, 0, True, TStoreFormat.InFormData) //
    .AddParameter('png_sticker', PngSticker, TFileToSend.Empty, True, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;
{$ENDREGION}
{$REGION 'Inline mode'}

function TTelegramBot.AnswerInlineQuery(const InlineQueryId: string; const Results: TArray<TtgInlineQueryResult>;
  const CacheTime: Int64; const IsPersonal: Boolean; const NextOffset, SwitchPmText, SwitchPmParameter: string)
  : Boolean;
begin
  Result := GetRequest.SetMethod('answerInlineQuery') //
    .AddParameter('inline_query_id', InlineQueryId, '', True, TStoreFormat.InFormData) //
    .AddParameter('results', TJsonUtils.ArrayToJString<TtgInlineQueryResult>(Results), '[]', True,
    TStoreFormat.InFormData) //
    .AddParameter('cache_time', CacheTime, 0, False, TStoreFormat.InFormData) //
    .AddParameter('is_personal', IsPersonal, False, False, TStoreFormat.InFormData) //
    .AddParameter('next_offset', NextOffset, '', False, TStoreFormat.InFormData) //
    .AddParameter('switch_pm_text', SwitchPmText, '', False, TStoreFormat.InFormData) //
    .AddParameter('switch_pm_parameter', SwitchPmParameter, '', False, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

{$ENDREGION}
{$REGION 'Payments'}

function TTelegramBot.SendInvoice(const ChatId: Int64; const Title: string; const Description: string;
  const Payload: string; const ProviderToken: string; const StartParameter: string; const Currency: string;
  const Prices: TArray<TtgLabeledPrice>; const ProviderData: string; const PhotoUrl: string; const PhotoSize: Int64;
  const PhotoWidth: Int64; const PhotoHeight: Int64; const NeedName: Boolean; const NeedPhoneNumber: Boolean;
  const NeedEmail: Boolean; const NeedShippingAddress: Boolean; const IsFlexible: Boolean;
  const DisableNotification: Boolean; const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('sendInvoice') //
    .AddParameter('chat_id', ChatId, 0, True, TStoreFormat.InFormData) //
    .AddParameter('title', Title, '', True, TStoreFormat.InFormData) //
    .AddParameter('description', Description, '', True, TStoreFormat.InFormData)
    //
    .AddParameter('payload', Payload, '', True, TStoreFormat.InFormData) //
    .AddParameter('provider_token', ProviderToken, '', True, TStoreFormat.InFormData) //
    .AddParameter('start_parameter', StartParameter, '', True, TStoreFormat.InFormData) //
    .AddParameter('currency', Currency, '', True, TStoreFormat.InFormData) //
    .AddParameter('prices', TJsonUtils.ArrayToJString<TtgLabeledPrice>(Prices), '', True, TStoreFormat.InFormData) //
    .AddParameter('provider_data', ProviderData, '', False, TStoreFormat.InFormData) //
    .AddParameter('photo_url', PhotoUrl, '', False, TStoreFormat.InFormData) //
    .AddParameter('photo_size', PhotoSize, 0, False, TStoreFormat.InFormData) //
    .AddParameter('photo_width', PhotoWidth, 0, False, TStoreFormat.InFormData)
    //
    .AddParameter('photo_height', PhotoHeight, 0, False, TStoreFormat.InFormData) //
    .AddParameter('need_name', NeedName, False, False, TStoreFormat.InFormData)
    //
    .AddParameter('need_phone_number', NeedPhoneNumber, False, False, TStoreFormat.InFormData) //
    .AddParameter('need_email', NeedEmail, False, False, TStoreFormat.InFormData) //
    .AddParameter('need_shipping_address', NeedShippingAddress, False, False, TStoreFormat.InFormData) //
    .AddParameter('is_flexible', IsFlexible, False, False, TStoreFormat.InFormData) //
    .AddParameter('disable_notification', DisableNotification, False, False, TStoreFormat.InFormData) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False, TStoreFormat.InFormData) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.AnswerPreCheckoutQueryBad(const PreCheckoutQueryId, ErrorMessage: string): Boolean;
begin
  Result := GetRequest.SetMethod('answerPreCheckoutQuery') //
    .AddParameter('pre_checkout_query_id', PreCheckoutQueryId, '', True, TStoreFormat.InFormData) //
    .AddParameter('ok', False, True, False, TStoreFormat.InFormData) //
    .AddParameter('error_message', ErrorMessage, '', True, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.AnswerPreCheckoutQueryGood(const PreCheckoutQueryId: string): Boolean;
begin
  Result := GetRequest.SetMethod('answerPreCheckoutQuery') //
    .AddParameter('pre_checkout_query_id', PreCheckoutQueryId, '', True, TStoreFormat.InFormData) //
    .AddParameter('ok', True, False, False, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.AnswerShippingQueryBad(const ShippingQueryId, ErrorMessage: string): Boolean;
begin
  Result := GetRequest.SetMethod('answerShippingQuery') //
    .AddParameter('Shipping_query_id', ShippingQueryId, '', True, TStoreFormat.InFormData) //
    .AddParameter('ok', False, False, False, TStoreFormat.InFormData) //
    .AddParameter('error_message', ErrorMessage, '', False, TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

function TTelegramBot.AnswerShippingQueryGood(const ShippingQueryId: string;
  const ShippingOptions: TArray<TtgShippingOption>): Boolean;
begin
  Result := GetRequest.SetMethod('answerShippingQuery') //
    .AddParameter('Shipping_query_id', ShippingQueryId, '', True, TStoreFormat.InFormData) //
    .AddParameter('ok', True, False, False, TStoreFormat.InFormData) //
    .AddParameter('Shipping_options', TJsonUtils.ArrayToJString<TtgShippingOption>(ShippingOptions), '[]', True,
    TStoreFormat.InFormData) //
    .ExecuteAsBool;
end;

{$ENDREGION}
{$REGION 'Games'}

function TTelegramBot.GetGameHighScores(const UserId: Int64; const InlineMessageId: string): TArray<ItgGameHighScore>;
begin
  Result := TBaseJson.AsArray<ItgGameHighScore>(TtgGameHighScore, GetRequest.SetMethod('getGameHighScores') //
    .AddParameter('user_id', UserId, 0, True, TStoreFormat.InFormData) //
    .AddParameter('inline_message_id', InlineMessageId, '', True, TStoreFormat.InFormData) //
    .ExecuteAsString);
end;

function TTelegramBot.GetGameHighScores(const UserId, ChatId, MessageId: Int64): TArray<ItgGameHighScore>;
begin
  Result := TBaseJson.AsArray<ItgGameHighScore>(TtgGameHighScore, GetRequest.SetMethod('getGameHighScores') //
    .AddParameter('chat_id', ChatId, 0, True, TStoreFormat.InFormData) //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('message_id', MessageId, 0, True) //
    .ExecuteAsString);
end;

function TTelegramBot.SendGame(const ChatId: Int64; const GameShortName: string; const DisableNotification: Boolean;
  const ReplyToMessageId: Int64; ReplyMarkup: IReplyMarkup): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('sendGame') //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('game_short_name', GameShortName, '', True) //
    .AddParameter('disable_notification', DisableNotification, False, False) //
    .AddParameter('reply_to_message_id', ReplyToMessageId, 0, False) //
    .AddParameter('reply_markup', TInterfacedObject(ReplyMarkup), nil, False) //
    .ExecuteAsString);
end;

function TTelegramBot.SetGameScore(const UserId, Score: Int64; const InlineMessageId: string;
  const Force, DisableEditMessage: Boolean): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('setGameScore') //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('score', Score, 0, True) //
    .AddParameter('force', Force, False, False) //
    .AddParameter('disable_edit_message', DisableEditMessage, False, False) //
    .AddParameter('inline_message_id', InlineMessageId, '', True) //
    .ExecuteAsString);
end;

function TTelegramBot.SetGameScore(const UserId, Score, ChatId, MessageId: Int64;
  const Force, DisableEditMessage: Boolean): ITgMessage;
begin
  Result := TTgMessage.Create(GetRequest.SetMethod('setGameScore') //
    .AddParameter('user_id', UserId, 0, True) //
    .AddParameter('score', Score, 0, True) //
    .AddParameter('force', Force, False, False) //
    .AddParameter('disable_edit_message', DisableEditMessage, False, False) //
    .AddParameter('chat_id', ChatId, 0, True) //
    .AddParameter('message_id', MessageId, 0, True) //
    .ExecuteAsString);
end;
{$ENDREGION}

end.
